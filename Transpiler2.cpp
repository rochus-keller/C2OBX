/*
* Copyright (c) 2022 Rochus Keller
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

#include "Transpiler2.h"
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

struct Decls2
{
    QMultiMap<quint32,Type*> types; // line->Type
    QMap<quint32,QPair<Token*,Token*> > alias; // line->Name
    QMap<quint32,Obj*> functions; // line->Obj
    QMap<quint32,C::Macro*> consts; // line->Macro
    QMap<quint32,Type*> consts2; // line->anonymous enum
    QMap<quint32,Obj*> consts3; // line->Obj
};
static QMap<QByteArray,Decls2> declOrder2; // file->Decls
static QByteArray prefix2;
static const QSet<QByteArray> obxKeywords2 = QSet<QByteArray>() << "ARRAY" << "THEN" << "BEGIN"<< "IN"<< "TO"
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

    if( obxKeywords2.contains(name) )
        return name + "_";
    for( int i = 0; i < name.size(); i++ )
    {
        if( !::islower(name[i]) )
            return name;
    }
    if( obxKeywords2.contains(name.toUpper()) )
        return name + "_";
    return name;
}

static inline QString renderFilePos(Token* tok)
{
    if( tok )
        return QString("%1:%2" ).arg(QFileInfo(tok->filename).fileName()).arg(tok->line_no);
    else
        return "???";
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

static inline bool isUnstructured(Type* t)
{
    if( isBaseType(t) && t->kind != Type::VOID )
        return true;
    switch( t->kind )
    {
    case Type::ENUM:
    case Type::PTR:
    //case Type::FUNC:
        return true;
    default:
        return false;
    }
}

static bool renderTypeDecl(QTextStream& out, Type* t, int level );

static QByteArray defix( const QByteArray& name )
{
    if( !prefix2.isEmpty() && name.startsWith(prefix2) )
    {
        return name.mid(prefix2.size());
    }
    return name;
}

static void renderTypeName( QTextStream& out, Type* t, int level )
{
    QByteArray typeName;
    if( t->typeName )
        typeName = t->typeName->txt;
    else if( t->origin && t->origin->typeName )
        typeName = t->origin->typeName->txt;
    if( !typeName.isEmpty() )
        out << escape(defix(typeName));
    else
        renderTypeDecl(out,t,level);
}

static inline QByteArray ws(int level) { return QByteArray(level*4,' '); }

static bool renderEnum(QTextStream& out, Type* t, int level)
{
    if( t->consts.isEmpty() )
    {
        out << "()";
        return false;
    }

    QMultiMap<int,const ObjScope*> itemsByVal;
    QMap<QByteArray,const ObjScope*> itemsByName;
    for(int i = 0; i < t->consts.size(); i++ )
    {
        itemsByVal.insert(t->consts[i]->enum_val, t->consts[i] );
        itemsByName.insert(t->consts[i]->name, t->consts[i] );
    }

    int i = 0;
    bool trueEnum = true;
    QMultiMap<int,const ObjScope*>::const_iterator j = itemsByVal.begin();
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
        QMultiMap<int,const ObjScope*>::const_iterator i;
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
        QMap<QByteArray,const ObjScope*>::const_iterator i;
        for( i = itemsByName.begin(); i != itemsByName.end(); ++i )
            out << ws(level-1) << escape(defix(i.key())) << " = " << i.value()->enum_val << endl;
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

static void renderParams(QTextStream& out, Type* func, int level)
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
            name = p->name_pos->txt;
        if( !validName(name) )
            name = "_" + QByteArray::number(nr);
        out << escape(name) << ": ";
        renderTypeName(out,p,level);
        p = p->next;
        nr++;
    }

    if( func->params || r )
        out << ")";
    if( r )
    {
        out << ": ";
        renderTypeName(out,r,level+1);
    }
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
            if( t->base->base->kind == Type::PTR )
                qCritical() << "pointers like ***type are not supported";
            if( isUnstructured(t->base->base) )
            {
                out << "*[]";
                renderTypeDecl(out, t->base->base, level );
            }else
                renderTypeDecl(out, t->base, level );
        }else if( isUnstructured(t->base) )
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
        renderParams(out,t,level);
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
            level += 3;
            while( member )
            {
                out << endl << ws(level);
                QByteArray name = member->name->txt;
                if( member->is_bitfield )
                    qWarning() << "bitfields not supported:" << name << renderFilePos(member->name);
                out << escape(name) << ": ";
                renderTypeName(out,member->ty,level );
                member = member->next;
            }
            level -= 3;
            out << " end";
        }
        break;
    case Type::VLA:
        qWarning() << "VLA not supported:" << renderFilePos(t->name_pos);
        break;
    }
    return false;
}

static inline Token* getNameOf(Type* t)
{
    Q_ASSERT( t->tag || !t->typedefs.isEmpty() );
    Token* name = 0;
    if( !t->typedefs.isEmpty() )
        name = t->typedefs.first();
    else
        name = t->tag;
    return name;
}

static void orderDecls()
{
    foreach( Type* t, Parser::typeDecls )
    {
        if( t->tag == 0 && t->typedefs.isEmpty() )
            continue;

        Token* name = getNameOf(t);

#if 0
        Type* tmp = declOrder2[name->filename].types[name->line_no];
        if( tmp != 0 )
        {
            Token* name2 = getNameOf(tmp);
            qWarning() << "type already registered" << name->txt << renderFilePos(name)
                       << "as" << name2->txt << renderFilePos(name2);
        }else
        {
            declOrder2[name->filename].types[name->line_no] = t;
            for( int i = 1; i < t->typedefs.size(); i++ )
                declOrder2[t->typedefs[i]->filename].alias[t->typedefs[i]->line_no] = qMakePair(t->typedefs[i],name);
        }
#else
        // types is now MultiMap; it happens that there is more than one declaration on the same line, e.g. like
        // "typedef struct _SDL_iconv_t *SDL_iconv_t;" which is "struct _SDL_iconv_t" and pointer to struct

        declOrder2[name->filename].types.insert(name->line_no,t);
        for( int i = 1; i < t->typedefs.size(); i++ )
            declOrder2[t->typedefs[i]->filename].alias[t->typedefs[i]->line_no] = qMakePair(t->typedefs[i],name);
#endif
    }

    foreach( Type* t, Parser::anonymousEnums )
    {
        Q_ASSERT( t->name_pos != 0 ); // is set even if there is no name
        Decls2& d = declOrder2[t->name_pos->filename];
        Type* tmp = d.consts2[t->name_pos->line_no];
        if( tmp != 0 && tmp != t )
            qWarning() << "anonymous enum already registered" <<
                        renderFilePos(t->name_pos) << renderFilePos(tmp->name_pos);
        else
            d.consts2[t->name_pos->line_no] = t;
    }

    for( Tokenizer::Macros::const_iterator i = Tokenizer::macros.begin(); i != Tokenizer::macros.end(); ++i )
    {
        Macro* m = i.value();
        if( m->body == 0 || m->body->filename == "<built-in>" || !m->is_objlike )
            continue;
        Q_ASSERT( m->body );
        if( m->body->kind == Token::_EOF )
            continue;
        Macro* tmp = declOrder2[m->body->filename].consts[m->body->line_no];
        if( tmp != 0 && tmp != m )
            qWarning() << "macro already registered" << m->name << renderFilePos(tmp->body);
        else
            declOrder2[m->body->filename].consts[m->body->line_no] = m;
    }

    Obj* vars = Parser::globalVars;
    while(vars)
    {
        if( vars->is_const )
            declOrder2[vars->tok->filename].consts3[vars->tok->line_no] = vars;
        vars = vars->next;
    }

    for( int i = 0; i < Parser::funcs.size(); i++ )
    {
        Obj* f = Parser::funcs[i];
        if( f->is_function && ( f->is_inline || f->is_definition ) )
            qWarning() << "not considering (inline) function definition" << f->name << renderFilePos(f->ty->name_pos);
        if( !( f->is_function && f->is_live && f->is_root && !f->is_inline && !f->is_definition ) )
            continue;
        if( f->ty->name_pos )
        {
            //Q_ASSERT( declOrder[o->ty->name_pos->filename].functions[o->ty->name_pos->line_no] == 0 );
            declOrder2[f->ty->name_pos->filename].functions[f->ty->name_pos->line_no] = f;
        }else
            qWarning() << "function has no namepos" << f->name;
    }
}

static inline QByteArray getTypeDeclName(Type* t)
{
    QByteArray typeName;
#if 0
    if( t->typeName )
        typeName = t->typeName->txt;
    else if( t->origin && t->origin->typeName )
        typeName = t->origin->typeName->txt;
    else if( t->name )
        typeName = t->name->txt;
#else
    if( !t->typedefs.isEmpty() )
        typeName = t->typedefs.first()->txt;
    else if( t->tag )
        typeName = t->tag->txt;
#endif
    if( !typeName.isEmpty() )
        typeName = escape(defix(typeName));
    return typeName;
}

static QByteArray formatNum( Token* t )
{
    QByteArray res = t->txt;
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
            return defix(n->tok->txt);
        case Token::STR:
            return n->tok->txt;
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
        if( n && n->kind == Node::VAR && n->var->ty && n->var->ty->kind >= Type::FUNC )
            return "nil // CHECK " + renderNode(n);
        else
            return renderNode(n);
    }catch(...)
    {
        Tokenizer::tokenize_errors_cause_exception = false;
        QByteArray str;
        while( e->kind != Token::_EOF )
        {
            str += e->txt;
            e = e->next;
        }
        return "nil // CHECK " + str; // this is apparently not a constant, maybe a define to a type name
    }
    return QByteArray();
}

static void renderModule(const QString& outFilePath, const QByteArray& modName)
{
    QString name;
    if( !modName.isEmpty() )
        name = modName;
    else
        name = "Module";
    QString path = outFilePath;
    if( path.isEmpty() )
        path = QDir::current().absoluteFilePath(name+".obx");
    QFile f(path);
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
    if( !prefix2.isEmpty() )
        out << "prefix '" << prefix2 << "'"; // prefix, the string to be prefixed to proc names to find them in the library
    out << "]" << endl;

    QMap<QByteArray,Decls2>::const_iterator i;

    for( i = declOrder2.begin(); i != declOrder2.end(); ++i )
    {
        if( i.key() == "<built-in>" )
            continue;
        out << endl << "    // from " << QFileInfo(i.key()).fileName() << endl;

        if( !i.value().consts.isEmpty() || !i.value().consts2.isEmpty() || !i.value().consts3.isEmpty() )
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
        for( k = i.value().consts2.begin(); k != i.value().consts2.end(); ++k )
        {
            for( int j = 0; j < k.value()->consts.size(); j++ )
            {
                out << ws(2) << escape(defix(k.value()->consts[j]->name)) << " = ";
                out << k.value()->consts[j]->enum_val;
                out << endl;
            }
        }
        if( !i.value().consts2.isEmpty() )
            out << endl;

        QMap<quint32,Obj*>::const_iterator l;
        for( l = i.value().consts3.begin(); l != i.value().consts3.end(); ++l )
        {
            out << ws(2) << escape(defix(l.value()->nameBuf)) << " = nil // CHECK" << endl;
        }
        if( !i.value().consts3.isEmpty() )
            out << endl;

        bool headerDone = false;
        for( k = i.value().types.begin(); k != i.value().types.end(); ++k )
        {
            Type* t = k.value();
            if( !headerDone )
            {
                out << "    type" << endl;
                headerDone = true;
            }
            const QByteArray name = escape(defix(getTypeDeclName(t)));
            out << ws(2) << name << " = ";
            headerDone = !renderTypeDecl(out,t,3);
            out << endl;
        }

        if( !i.value().alias.isEmpty() )
        {
            if( !headerDone )
            {
                out << "    type" << endl;
                headerDone = true;
            }
            QMap<quint32,QPair<Token*,Token*> >::const_iterator j;
            for( j = i.value().alias.begin(); j != i.value().alias.end(); ++j )
                out << ws(2) << escape(defix(j.value().first->txt)) << " = "
                    << escape(defix(j.value().second->txt)) << endl;
        }

        if( headerDone )
            out << endl;

        QMap<quint32,Obj*>::const_iterator j;
        for( j = i.value().functions.begin(); j != i.value().functions.end(); ++j )
        {
            Obj* func = j.value();
            out << "    proc " << escape(defix(func->name));
            renderParams(out, func->ty, 2);
            if( func->ty->is_variadic )
                out << " [varargs]";
            out << endl;
            // TEST qDebug() << i.key() << "\t" << func->name;
        }
    }

    out << "end " << escape(modName) << endl;
}

void Transpiler2::render(const QString& outFilePath, const QByteArray& modName, const QByteArray& _prefix)
{
    prefix2 = _prefix;
    declOrder2.clear();
    orderDecls();
    renderModule(outFilePath, modName);
}
