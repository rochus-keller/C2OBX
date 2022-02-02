/*
* Copyright (c) 2019 Rui Ueyama, author of the original C version
*               2022 Rochus Keller, migrated the C version to C++/Qt
*
* This file is part of the C2OBX C to Oberon+ transpiler application.
*
* The following is the license that applies to this copy of the
* application. For a license to use the application under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "Transpiler.h"
#include "Type.h"
#include "Tokenizer.h"
#include "Parser.h"
#include "Ast.h"
#include "Preprocessor.h"
#include <QFileInfo>
#include <QTextStream>
#include <QDir>
#include <QDateTime>
#include <QCoreApplication>
#include <QtDebug>
using namespace C;

struct EnumItem
{
    QByteArray name;
    int value;
    Type* type;
    EnumItem(const QByteArray& n, int v, Type* t):name(n),value(v),type(t){}
};
struct EnumType
{
    QByteArray name;
    QList<EnumItem> items;
};
static QHash<Type*,EnumType> enumTypes;
static QList<EnumItem> anonymousEnums;

struct Decls
{
    QMap<quint32,Type*> types; // line->Type
    QMap<quint32,Obj*> functions; // line->Obj
    QMap<quint32,C::Macro*> consts; // line->Macro
};
QMap<QByteArray,Decls> declOrder; // file->Decls

static QSet<Type*> usedTypes;
static QHash<Type*,QByteArray> typeNames;
static QByteArray prefix;
static const QSet<QByteArray> obxKeywords = QSet<QByteArray>() << "ARRAY" << "THEN" << "BEGIN"<< "IN"<< "TO"
                                                                << "BY"<< "IS"<< "TRUE"<< "CASE"<< "MOD"<< "TYPE"
                                                                << "CONST"<< "MODULE"<< "UNTIL"<< "DIV"<< "NIL"
                                                                << "VAR"<< "DO"<< "OF"<< "WHILE"<< "ELSE"<< "OR"
                                                                << "ELSIF"<< "POINTER"<< "END"<< "PROCEDURE"
                                                                << "FALSE"<< "RECORD"<< "FOR"<< "REPEAT"<< "IF"
                                                                << "RETURN"<< "DEFINITION"<< "PROC"<< "WITH"
                                                                << "LOOP"<< "EXIT"<< "OUT"<< "CLOSE"<< "UNSAFE"
                                                                << "CSTRUCT"<< "CUNION"<< "CARRAY"<< "CPOINTER";

static QByteArray escape( const QByteArray& name )
{
    if( name.isEmpty() || name.contains('_') )
        return name;

    if( obxKeywords.contains(name) )
        return name + "_";
    for( int i = 0; i < name.size(); i++ )
    {
        if( !::islower(name[i]) )
            return name;
    }
    if( obxKeywords.contains(name.toUpper()) )
        return name + "_";
    return name;
}

static inline bool isBaseType(Type* t)
{
    switch( t->kind )
    {
    case Type::BOOL:
    case Type::CHAR:
    case Type::SHORT:
    case Type::INT:
    case Type::LONG:
    case Type::FLOAT:
    case Type::DOUBLE:
    case Type::LDOUBLE:
    case Type::VOID:
        return true;
    default:
        return false;
    }
}

static bool renderTypeDecl(QTextStream& out, Type* t, int level );

static QByteArray defix( const QByteArray& name )
{
    if( !prefix.isEmpty() && name.startsWith(prefix) )
    {
        return name.mid(prefix.size());
    }
    return name;
}

static void renderTypeName( QTextStream& out, Type* t, int level = 0 )
{
    if( isBaseType(t) )
    {
        renderTypeDecl(out,t,level);
        return;
    }
    usedTypes.insert(t);
    QByteArray typeName = typeNames.value(t);
    if( typeName.isEmpty() )
        typeName = enumTypes.value(t).name;
    if( typeName.isEmpty() && t->origin )
    {
        usedTypes.insert(t->origin);
        typeName = typeNames.value(t->origin);
    }
    if( !typeName.isEmpty() )
        out << escape(defix(typeName));
    else
        renderTypeDecl(out,t,level);
}

static inline QByteArray ws(int level) { return QByteArray(level*4,' '); }

static bool renderEnum(QTextStream& out, Type* t, int level)
{
    const EnumType& et = enumTypes[t];
    if( et.items.isEmpty() )
    {
        out << "()";
        return false;
    }

    QMultiMap<int,const EnumItem*> itemsByVal;
    QMap<QByteArray,const EnumItem*> itemsByName;
    for(int i = 0; i < et.items.size(); i++ )
    {
        itemsByVal.insert(et.items[i].value, &et.items[i] );
        itemsByName.insert(et.items[i].name, &et.items[i] );
    }

    int i = 0;
    bool trueEnum = true;
    QMultiMap<int,const EnumItem*>::const_iterator j = itemsByVal.begin();
    for( j = itemsByVal.begin(); j != itemsByVal.end(); ++j, i++ )
    {
        if( j.key() != i )
        {
            trueEnum = false;
            break;
        }
    }

    if( trueEnum )
    {
        out << "(";
        QMultiMap<int,const EnumItem*>::const_iterator i;
        int done = 0;
        for( i = itemsByVal.begin(); i != itemsByVal.end(); ++i )
        {
            if( done++ )
                out << ", ";
            out << escape(defix(i.value()->name));
        }
        out << ")";
        return false;
    }else
    {
        out << "integer" << endl;
        out << ws(level-2) << "const" << endl;
        QMap<QByteArray,const EnumItem*>::const_iterator i;
        for( i = itemsByName.begin(); i != itemsByName.end(); ++i )
            out << ws(level-1) << escape(defix(i.key())) << " = " << i.value()->value << endl;
        //out << ws(level-2) << "type";
        return true;
    }
}

static inline bool validName( const QByteArray& name )
{
    if( name.isEmpty() )
        return false;
    if( !( ::isalpha(name[0]) || name[0] == '_') )
        return false;
    for( int i = 1; i < name.size(); i++ )
    {
        if( !( ::isalnum(name[i]) || name[i] == '_') )
            return false;
    }
    return true;
}

static void renderParams(QTextStream& out, Type* func )
{
    Type* p = func->params;
    Type* r = (func->return_ty && func->return_ty->kind != Type::VOID) ? func->return_ty : 0;
    if( func->params || r )
        out << "(";
    int nr = 1;
    while(p)
    {
        if( p != func->params )
            out << "; ";
        QByteArray name;
        if( p->name_pos )
            name = QByteArray::fromRawData(p->name_pos->loc,p->name_pos->len);
        if( !validName(name) )
            name = "_" + QByteArray::number(nr);
        out << escape(name) << ": ";
        renderTypeName(out,p);
        p = p->next;
        nr++;
    }

    if( func->params || r )
        out << ")";
    if( r )
    {
        out << ": ";
        renderTypeName(out,r);
    }
}

static inline QString renderFilePos(Token* tok)
{
    if( tok )
        return QString("%1:%2" ).arg(QFileInfo(tok->filename).fileName()).arg(tok->line_no);
    else
        return "???";
}

static bool renderTypeDecl(QTextStream& out, Type* t, int level )
{
    switch( t->kind )
    {
    // TODO: unsigned
    case Type::BOOL:
        out << "boolean";
        break;
    case Type::CHAR:
        if( t->is_unsigned )
            out << "byte";
        else
            out << "char";
        break;
    case Type::SHORT:
        out << "shortint";
        break;
    case Type::INT:
        out << "integer";
        break;
    case Type::LONG:
        out << "integer";
        break;
    case Type::FLOAT:
        out << "real";
        break;
    case Type::DOUBLE:
        out << "longreal";
        break;
    case Type::LDOUBLE:
        out << "longreal"; // TODO
        break;
    case Type::VOID:
        out << "void";
        break;
    case Type::PTR:
        if( t->base->kind == Type::PTR ) // **type
        {
            out << "*[]";
            if( t->base->base->kind == Type::PTR )
                qCritical() << "pointers like ***type are not supported";
#if 0
            Type* ptr = t->base->base;
            while( ptr && ptr->kind == Type::PTR  ) // !isBaseType(ptr->base)
            {
                out << "[]";
                ptr = ptr->base;
            }
#else
            if( t->base->base->kind == Type::PTR )
                qCritical() << "pointers like ***type are not supported";
#endif
            if( isBaseType(t->base->base) && t->base->base->kind != Type::VOID )
            {
                out << "*[]";
                renderTypeDecl(out, t->base->base, level );
            }else
                renderTypeDecl(out, t->base, level );
        }else if( isBaseType(t->base) && t->base->kind != Type::VOID )
        {
            out << "*[]";
            renderTypeName(out,t->base,level);
        }else
        {
            if( t->base->kind != Type::FUNC )
                out << "*";
            renderTypeName(out,t->base,level);
        }
        break;
    case Type::ARRAY:
        out << "carray ";
        if( t->array_len )
            out << t->array_len << " ";
        out << "of ";
        renderTypeName(out,t->base,level);
        break;
    case Type::FUNC:
        out << "proc";
        renderParams(out,t);
        break;
    case Type::ENUM:
        return renderEnum(out,t,level);
        break;
    case Type::STRUCT:
    case Type::TUNION:
        {
            if( t->kind == Type::STRUCT )
                out << "cstruct";
            else
                out << "cunion";
            Member* member = t->members;
            while( member )
            {
                out << endl << ws(level+1);
                QByteArray name = QByteArray::fromRawData(member->name->loc, member->name->len);
                if( member->is_bitfield )
                    qWarning() << "bitfields not supported:" << name << renderFilePos(member->name);
                out << escape(name) << ": ";
                renderTypeName(out,member->ty );
                member = member->next;
            }
            out << " end";
        }
        break;
    case Type::VLA:
        qWarning() << "VLA not supported:" << renderFilePos(t->name_pos);
        break;
    }
    return false;
}

static inline void registerType(Type*t, const QByteArray& name)
{
    if( t->name_pos )
    {
        Type* tmp = declOrder[t->name_pos->filename].types[t->name_pos->line_no];
        if( tmp != 0 && tmp != t )
            qWarning() << "type already registered" << name <<
                        renderFilePos(t->name_pos) << renderFilePos(tmp->name_pos);
        else
            declOrder[t->name_pos->filename].types[t->name_pos->line_no] = t;
    }else
        qWarning() << "no namepos" << name << renderFilePos(t->name_pos);
}

static inline void registerMacro(Macro* m, const QByteArray& name)
{
    if( m->is_objlike )
    {
        Q_ASSERT( m->body );
        if( m->body->kind == Token::_EOF )
            return;
        Macro* tmp = declOrder[m->body->filename].consts[m->body->line_no];
        if( tmp != 0 && tmp != m )
            qWarning() << "macro already registered" << name << renderFilePos(tmp->body);
        else
            declOrder[m->body->filename].consts[m->body->line_no] = m;
    }
}

static void processTypes()
{
    Scope* myScope = Parser::scope;

    for( Tokenizer::Macros::const_iterator i = Tokenizer::macros.begin(); i != Tokenizer::macros.end(); ++i )
    {
        if( i.value()->body == 0 || i.value()->body->filename == "<built-in>")
            continue;
        registerMacro(i.value(),i.key());
    }

    for( Scope::Tags::const_iterator i = myScope->tags.begin(); i != myScope->tags.end(); ++i )
    {
        Type* t = i.value();
        Q_ASSERT( t->kind == Type::STRUCT || t->kind == Type::TUNION || t->kind == Type::ENUM ); // holds in SDL
        if( t->kind == Type::ENUM )
        {
            // never happens in SDL, check
            Q_ASSERT( enumTypes[t].name.isEmpty() );
            enumTypes[t].name = i.key();
        }
        Q_ASSERT( typeNames[t].isEmpty() ); // this one holds in SDL
        typeNames[t] = i.key();
        registerType(t, i.key());
    }
    for( Scope::Vars::const_iterator i = myScope->vars.begin(); i != myScope->vars.end(); ++i )
    {
        VarScope* vs = i.value();
        const QByteArray name = i.key();
        if( vs->enum_ty )
        {
            if( vs->enum_ty->name_pos == 0 )
            {
                // this happens for anonymous enums like the SDLK_* in SDL_keycode.h
                // unfortunately we don't have file or position information here
                anonymousEnums.append(EnumItem(name, vs->enum_val, vs->enum_ty));
            }else
                enumTypes[vs->enum_ty].items.append(EnumItem(name, vs->enum_val, vs->enum_ty));
        }else if( vs->type_def )
        {
            if( !typeNames[vs->type_def].isEmpty() )
            {
                const QByteArray name2 = typeNames[vs->type_def];
                if( name != name2 )
                    qWarning() << "type" << vs->type_def << "renamed from:" << name2 << "to:" << name << renderFilePos(vs->type_def->name_pos);
                // the same type get's even more than one name; typedefs don't seem to be sufficiently mapped for this application
            }else
                typeNames[vs->type_def] = name;
            registerType(vs->type_def, i.key());
            if( vs->type_def->kind == Type::ENUM )
            {
                //Q_ASSERT( enumTypes[vs->type_def].name.isEmpty() ); // holds in SDL
                enumTypes[vs->type_def].name = name;
            }else
            {
            }
        }
    }
}

static inline void registerFunction(Obj* o)
{
    if( o->ty->name_pos )
    {
        //Q_ASSERT( declOrder[o->ty->name_pos->filename].functions[o->ty->name_pos->line_no] == 0 );
        declOrder[o->ty->name_pos->filename].functions[o->ty->name_pos->line_no] = o;
    }else
        qWarning() << "no namepos" << o->name;
}

static void processFunctions()
{
    for( int i = 0; i < Parser::funcs.size(); i++ )
    {
        Obj* var = Parser::funcs[i];
        if( var->is_function && ( var->is_inline || var->is_definition ) )
            qWarning() << "not considering (inline) function definition" << var->name << renderFilePos(var->ty->name_pos);
        if( !( var->is_function && var->is_live && var->is_root && !var->is_inline && !var->is_definition ) )
            continue;
        registerFunction(var);
    }
}

static inline QByteArray getTypeName(Type* t)
{
    QByteArray typeName = typeNames.value(t);
    if( typeName.isEmpty() )
        typeName = enumTypes.value(t).name;
    if( typeName.isEmpty() && t->origin )
        typeName = typeNames.value(t->origin);
    return typeName;
}

static QByteArray formatNum( Token* t )
{
    QByteArray res(t->loc,t->len);
    if( res.endsWith('U') )
        res.chop(1);
    if( res.startsWith("0x") )
    {
        Q_ASSERT(res.length()>2);
        QByteArray num = res.mid(2);
        if( !::isdigit(num[0]) )
            num = "0"+num;
        res = num + "h";
    }
    return res;
}

static QByteArray renderNode(Node* n)
{
    if( n == 0 )
        throw "";
    switch(n->kind)
    {
    case Node::ADD:       // +
        return renderNode(n->lhs) + " + " + renderNode(n->rhs);
    case Node::SUB:       // -
        return renderNode(n->lhs) + " - " + renderNode(n->rhs);
    case Node::MUL:       // *
        return renderNode(n->lhs) + " * " + renderNode(n->rhs);
    case Node::DIV:       // /
        if( n->ty && ( n->ty->kind == Type::FLOAT || n->ty->kind == Type::DOUBLE || n->ty->kind == Type::LDOUBLE ))
            return renderNode(n->lhs) + " / " + renderNode(n->rhs);
        else
            return renderNode(n->lhs) + " div " + renderNode(n->rhs);
    case Node::NEG:       // unary -
        return "-" + renderNode(n->lhs);
    case Node::MOD:       // %
        return renderNode(n->lhs) + " mod " + renderNode(n->rhs);
    case Node::BITAND:    // &
        return "bitand(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case Node::BITOR:     // |
        return "bitor(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case Node::BITXOR:    // ^
        return "bitxor(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case Node::SHL:       // <<
        return "lsl(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case Node::SHR:       // >>
        return "ror(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case Node::EQ:        // ==
        return "(" + renderNode(n->lhs) + " = " + renderNode(n->rhs) + ")";
    case Node::NE:        // !=
        return "(" + renderNode(n->lhs) + " # " + renderNode(n->rhs) + ")";
    case Node::LT:        // <
        return "(" + renderNode(n->lhs) + " < " + renderNode(n->rhs) + ")";
    case Node::LE:        // <=
        return "(" + renderNode(n->lhs) + " <= " + renderNode(n->rhs) + ")";
    case Node::NOT:       // !
        return "not " + renderNode(n->lhs);
    case Node::BITNOT:    // ~
        return "bitnot(" + renderNode(n->lhs) + ")";
    case Node::LOGAND:    // &&
        return "(" + renderNode(n->lhs) + " & " + renderNode(n->rhs) + ")";
    case Node::LOGOR:     // ||
        return "(" + renderNode(n->lhs) + " or " + renderNode(n->rhs) + ")";
    case Node::VAR:       // Variable
        switch( n->tok->kind )
        {
        case Token::IDENT:
            return defix(QByteArray(n->tok->loc,n->tok->len));
        case Token::STR:
            return QByteArray(n->tok->loc,n->tok->len);
        default:
            throw "";
        }
        break;
    case Node::NUM:       // Integer
        return formatNum(n->tok);
    case Node::CAST:      // Type cast
        return renderNode(n->lhs);
    default:
        throw "";
    }

    return QByteArray();
}

static QByteArray renderMacro(Token* e)
{
    e = Preprocessor::preprocess(e);
    try
    {
        Tokenizer::tokenize_errors_cause_exception = true;
        Node* n = Parser::expr_checked(e);
        Tokenizer::tokenize_errors_cause_exception = false;
        return renderNode(n);
    }catch(...)
    {
        Tokenizer::tokenize_errors_cause_exception = false;
        QByteArray str;
        while( e->kind != Token::_EOF )
        {
            str += QByteArray(e->loc,e->len);
            e = e->next;
        }
        return "nil // " + str; // this is apparently not a constant, maybe a define to a type name
    }
    return QByteArray();
}

static void renderModule(const QByteArray& modName)
{
    QString name;
    if( !modName.isEmpty() )
        name = modName;
    else
        name = "Module";
    QFile f(QDir::current().absoluteFilePath(name+".obx"));
    if( !f.open(QIODevice::WriteOnly) )
    {
        qCritical() << "cannot open file for writing:" << f.fileName();
        return;
    }else
        qDebug() << "generating" << f.fileName();
    QTextStream out(&f);

    out << "// Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() << " on "
           << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

    out << "definition " << escape(modName) << " [";
    if( !prefix.isEmpty() )
        out << "prefix '" << prefix << "'"; // prefix, the string to be prefixed to proc names to find them in the library
    out << "]" << endl;

    QMap<QByteArray,Decls>::const_iterator i;

    for( i = declOrder.begin(); i != declOrder.end(); ++i )
    {
        if( i.key() == "<built-in>" )
            continue;
        out << endl << "    // from " << QFileInfo(i.key()).fileName() << endl;

        if( !i.value().consts.isEmpty() )
            out << "    const" << endl;
        QMap<quint32,Macro*>::const_iterator m;
        for( m = i.value().consts.begin(); m != i.value().consts.end(); ++m )
        {
            out << ws(2) << escape(defix(m.value()->name)) << " = ";
            out << renderMacro(m.value()->body);
            out << endl;
        }
        if( !i.value().consts.isEmpty() )
            out << endl;

        QMap<quint32,Type*>::const_iterator k;
        bool headerDone = false;
        for( k = i.value().types.begin(); k != i.value().types.end(); ++k )
        {
            Type* t = k.value();
            //if( !enumTypes.contains(t) && !usedTypes.contains(t) )
            //    continue;
            if( !headerDone )
            {
                out << "    type" << endl;
                headerDone = true;
            }
            out << ws(2) << escape(defix(getTypeName(t))) << " = ";
            headerDone = !renderTypeDecl(out,t,3);
            out << endl;
        }
        if( headerDone )
            out << endl;

        QMap<quint32,Obj*>::const_iterator j;
        for( j = i.value().functions.begin(); j != i.value().functions.end(); ++j )
        {
            Obj* func = j.value();
            out << "    proc " << escape(defix(func->name));
            renderParams(out, func->ty);
            if( func->ty->is_variadic )
                out << " [varargs]";
            out << endl;
        }
    }

    if( !anonymousEnums.isEmpty() )
    {
        out << "    // global enum constants" << endl;
        out << "    const" << endl;
        foreach( const EnumItem& e, anonymousEnums )
        {
            out << ws(2) << escape(defix(e.name)) << " = " << e.value << endl;
        }
    }

    out << "end " << escape(modName) << endl;
}

void Transpiler::render(const QByteArray& modName, const QByteArray& _prefix)
{
    prefix = _prefix;
    processTypes();
    processFunctions();
    renderModule(modName);
}

