/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
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

#include <QCoreApplication>
#include <QFileInfo>
#include <QStringList>
#include <QtDebug>
#include <QDir>
#include <QDateTime>

extern "C" {
    #define noreturn
    #include <chibicc/chibicc.h>
    StringArray include_paths;
    bool opt_fcommon = true;
    bool opt_fpic;
    char *base_file;
    bool file_exists(char *path) {
      struct stat st;
      return !stat(path, &st);
    }
}

static const QSet<QByteArray> obxKeywords = QSet<QByteArray>() << "ARRAY" << "THEN" << "BEGIN"<< "IN"<< "TO"
                                                                << "BY"<< "IS"<< "TRUE"<< "CASE"<< "MOD"<< "TYPE"
                                                                << "CONST"<< "MODULE"<< "UNTIL"<< "DIV"<< "NIL"
                                                                << "VAR"<< "DO"<< "OF"<< "WHILE"<< "ELSE"<< "OR"
                                                                << "ELSIF"<< "POINTER"<< "END"<< "PROCEDURE"
                                                                << "FALSE"<< "RECORD"<< "FOR"<< "REPEAT"<< "IF"
                                                                << "RETURN"<< "DEFINITION"<< "PROC"<< "WITH"
                                                                << "LOOP"<< "EXIT"<< "OUT"<< "CLOSE"<< "UNSAFE"
                                                                << "CSTRUCT"<< "CUNION"<< "CARRAY"<< "CPOINTER";
static const char* s_nodeKind[] = {
    "ND_NULL_EXPR", // Do nothing
    "ND_ADD",       // +
    "ND_SUB",       // -
    "ND_MUL",       // *
    "ND_DIV",       // /
    "ND_NEG",       // unary -
    "ND_MOD",       // %
    "ND_BITAND",    // &
    "ND_BITOR",     // |
    "ND_BITXOR",    // ^
    "ND_SHL",       // <<
    "ND_SHR",       // >>
    "ND_EQ",        // ==
    "ND_NE",        // !=
    "ND_LT",        // <
    "ND_LE",        // <=
    "ND_ASSIGN",    // =
    "ND_COND",      // ?:
    "ND_COMMA",     // ",
    "ND_MEMBER",    // . (struct member access)
    "ND_ADDR",      // unary &
    "ND_DEREF",     // unary *
    "ND_NOT",       // !
    "ND_BITNOT",    // ~
    "ND_LOGAND",    // &&
    "ND_LOGOR",     // ||
    "ND_RETURN",    // "return"
    "ND_IF",        // "if"
    "ND_FOR",       // "for" or "while"
    "ND_DO",        // "do"
    "ND_SWITCH",    // "switch"
    "ND_CASE",      // "case"
    "ND_BLOCK",     // { ... }
    "ND_GOTO",      // "goto"
    "ND_GOTO_EXPR", // "goto" labels-as-values
    "ND_LABEL",     // Labeled statement
    "ND_LABEL_VAL", // [GNU] Labels-as-values
    "ND_FUNCALL",   // Function call
    "ND_EXPR_STMT", // Expression statement
    "ND_STMT_EXPR", // Statement expression
    "ND_VAR",       // Variable
    "ND_VLA_PTR",   // VLA designator
    "ND_NUM",       // Integer
    "ND_CAST",      // Type cast
    "ND_MEMZERO",   // Zero-clear a stack variable
    "ND_ASM",       // "asm"
    "ND_CAS",       // Atomic compare-and-swap
    "ND_EXCH",
};

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
QHash<Type*,EnumType> enumTypes;
QList<EnumItem> anonymousEnums;

struct Decls
{
    QMap<quint32,Type*> types; // line->Type
    QMap<quint32,Obj*> functions; // line->Obj
    QMap<quint32,Macro*> consts; // line->Macro
};
QMap<QByteArray,Decls> declOrder; // file->Decls

QHash<Type*,QByteArray> typeNames;
QSet<Type*> usedTypes;
static QByteArray modName, prefix;

static inline QString renderFilePos(Token* tok)
{
    if( tok )
        return QString("%1:%2" ).arg(QFileInfo(tok->filename).fileName()).arg(tok->line_no);
    else
        return "???";
}

static QByteArray defix( const QByteArray& name )
{
    if( !prefix.isEmpty() && name.startsWith(prefix) )
    {
        return name.mid(prefix.size());
    }
    return name;
}

static Token *must_tokenize_file(char *path) {
  Token *tok = tokenize_file(path);
  if (!tok)
    error("%s: %s", path, strerror(errno));
  return tok;
}

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

static void renderParams(QTextStream& out, Type* func );
static bool renderTypeDecl(QTextStream &out, Type *t, int level);

static inline bool isBaseType(Type* t)
{
    switch( t->kind )
    {
    case TY_BOOL:
    case TY_CHAR:
    case TY_SHORT:
    case TY_INT:
    case TY_LONG:
    case TY_FLOAT:
    case TY_DOUBLE:
    case TY_LDOUBLE:
    case TY_VOID:
        return true;
    default:
        return false;
    }
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

#if 0
    renderFilePos(t->name_pos);
#endif

}

static QVariant readValue( const char* data, Type* t )
{
    if( data == 0 )
        return QVariant();
    switch( t->kind )
    {
    case TY_SHORT:
    case TY_INT:
    case TY_LONG:
        switch( t->size )
        {
        case 1:
            if( t->is_unsigned )
                return *(quint8*)data;
            else
                return *(qint8*)data;
        case 2:
            if( t->is_unsigned )
                return *(quint16*)data;
            else
                return *(qint16*)data;
        case 4:
            if( t->is_unsigned )
                return *(quint32*)data;
            else
                return *(qint32*)data;
        case 8:
            if( t->is_unsigned )
                return *(quint64*)data;
            else
                return *(qint64*)data;
        }
        break;
    case TY_FLOAT:
        return *(float*)data;
    case TY_DOUBLE:
        return *(double*)data;
    case TY_PTR:
        if( t->base && t->base->kind == TY_CHAR )
            return data;
    default:
        qCritical() << "invalid type to read" << t->kind;
    }
    return QVariant();
}

static void printFunction( QTextStream& out, Obj* var )
{
    out << var->name << " ";
#if 0
    if( var->is_definition )
        out << "definition ";
    if( var->is_function )
        out << "function ";
    if( var->is_static )
        out << "static ";
    if( var->is_tentative )
        out << "tentative ";
    if( var->is_tls )
        out << "tls ";
    if( var->is_inline )
        out << "inline ";
    if( var->is_live )
        out << "live ";
    if( var->is_root )
        out << "root ";
#endif
    Q_ASSERT( var->ty );
    //renderType(out, var->ty);
    // out << readValue(var->init_data, var->ty ).toString() << " ";
    out << "(";
    Type* pt = var->ty->params;
    while(pt)
    {
        if( pt != var->ty->params )
            out << ", ";
        if( pt->name_pos )
            out << QByteArray::fromRawData(pt->name_pos->loc,pt->name_pos->len) << ": ";
        renderTypeName(out,pt);
        out << " " << pt << " " << pt->origin;
        pt = pt->next;
    }
    out << ")";
    Q_ASSERT( var->ty->return_ty );
    out << ": ";
    renderTypeName(out,var->ty->return_ty);
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

static void processFunctions( Obj *prog )
{
    QTextStream out(stdout);
    for (Obj *var = prog; var; var = var->next)
    {
        if( var->is_function && ( var->is_inline || var->is_definition ) )
            qWarning() << "not considering (inline) function definition" << var->name << renderFilePos(var->ty->name_pos);
        if( !( var->is_function && var->is_live && var->is_root && !var->is_inline && !var->is_definition ) )
            continue;
        registerFunction(var);
#if 0
        printFunction(out,var);
        if( var->ty->name_pos )
            out << "| " << renderFilePos(var->ty->name_pos);
        out << endl;
#endif

    }
}

static void renderTok( QTextStream& out, Token* cur, int level = 0, bool lean = false )
{
    out << QByteArray(level*2,' ');
    switch( cur->kind )
    {
    case TK_IDENT:
        out << "IDENT "; break;
    case TK_PUNCT:
        out << "PUNCT "; break;
    case TK_KEYWORD:
        out << "KEYWORD "; break;
    case TK_STR:
        out << "STR   "; break; //  << cur->str << " "; break;
    case TK_NUM:
        out << "NUM   "; break; // << cur->val << " " << double(cur->fval) << " "; break;
    case TK_PP_NUM:
        out << "PPNUM "; break;
    case TK_EOF:
        out << "EOF "; break;
    }

    if( cur->len )
        out << "\"" << QByteArray(cur->loc,cur->len) << "\" ";
    if( !lean && cur->ty )
        renderTypeName(out, cur->ty);
    out << endl;
}

struct Hideset {
  Hideset *next;
  char *name;
};

static void printTok(Token *tok, bool lean = false)
{
    QTextStream out(stdout);
    for (Token *cur = tok; cur; cur = cur->next)
    {
        if( lean )
            out << cur << " ";
        renderTok(out,cur,0,lean);
        if( !lean && cur->origin )
            renderTok(out,cur->origin, 1,lean);
        if( !lean && cur->hideset )
        {
            Hideset* hs = cur->hideset;
            out << QByteArray(3,' ');
            while( hs )
            {
                out << hs->name << " ";
                hs = hs->next;
            }
            out << endl;
        }
    }
}

static inline void registerType(Type*t,HashEntry* e)
{
    if( t->name_pos )
    {
        Type* tmp = declOrder[t->name_pos->filename].types[t->name_pos->line_no];
        if( tmp != 0 && tmp != t )
            qWarning() << "type already registered" << QByteArray::fromRawData(e->key, e->keylen) <<
                        renderFilePos(t->name_pos) << renderFilePos(tmp->name_pos);
        else
            declOrder[t->name_pos->filename].types[t->name_pos->line_no] = t;
    }else
        qWarning() << "no namepos" << QByteArray::fromRawData(e->key, e->keylen) << renderFilePos(t->name_pos);
}

static inline void registerMacro(Macro* m,HashEntry* e)
{
    if( m->is_objlike )
    {
        Q_ASSERT( m->body );
        if( m->body->kind == TK_EOF )
            return;
        Macro* tmp = declOrder[m->body->filename].consts[m->body->line_no];
        if( tmp != 0 && tmp != m )
            qWarning() << "macro already registered" << QByteArray::fromRawData(e->key, e->keylen) << renderFilePos(tmp->body);
        else
            declOrder[m->body->filename].consts[m->body->line_no] = m;
    }
}

static void printMacros()
{
    for( int i = 0; i < macros.capacity; i++ )
    {
        HashEntry* e = &macros.buckets[i];
        if( e->key == 0 )
            continue;
        Macro* m = (Macro*)e->val;
        qDebug() << "macro" << QByteArray(m->name) << m->is_objlike;
    }
}

static void processTypes()
{
    Scope* myScope = globalScope;
    QTextStream out(stdout);

    for( int i = 0; i < macros.capacity; i++ )
    {
        HashEntry* e = &macros.buckets[i];
        if( e->key == 0 )
            continue;
        registerMacro((Macro*)e->val,e);
    }

    for( int i = 0; i < myScope->tags.capacity; i++ )
    {
        HashEntry* e = &myScope->tags.buckets[i];
        if( e->key == 0 )
            continue;
        Type* t = (Type*)e->val;
        Q_ASSERT( t->kind == TY_STRUCT || t->kind == TY_UNION || t->kind == TY_ENUM ); // holds in SDL
#if 0
        out << "type " << QByteArray::fromRawData(e->key, e->keylen) << " " << t;
        renderTypeName(out,t);
        out << endl;
#endif
        if( t->kind == TY_ENUM )
        {
            // never happens in SDL, check
            Q_ASSERT( enumTypes[t].name.isEmpty() );
            enumTypes[t].name = QByteArray::fromRawData(e->key, e->keylen);
        }
        Q_ASSERT( typeNames[t].isEmpty() ); // this one holds in SDL
        typeNames[t] = QByteArray::fromRawData(e->key, e->keylen);
        registerType(t, e);
    }
    for( int i = 0; i < myScope->vars.capacity; i++ )
    {
        HashEntry* e = &myScope->vars.buckets[i];
        if( e->key == 0 )
            continue;
        VarScope* vs = (VarScope*)e->val;
        const QByteArray name = QByteArray::fromRawData(e->key, e->keylen);
        if( vs->enum_ty )
        {
            if( vs->enum_ty->name_pos == 0 )
            {
                // this happens for anonymous enums like the SDLK_* in SDL_keycode.h
                // unfortunately we don't have file or position information here
                anonymousEnums.append(EnumItem(name, vs->enum_val, vs->enum_ty));
            }else
                enumTypes[vs->enum_ty].items.append(EnumItem(name, vs->enum_val, vs->enum_ty));
#if 0
            out << "enum " << QByteArray::fromRawData(e->key, e->keylen) << " " << vs->enum_val << " (";
            renderType(out,vs->enum_ty);
            out << ")" << endl;
#endif
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
            registerType(vs->type_def,e);
            if( vs->type_def->kind == TY_ENUM )
            {
                //Q_ASSERT( enumTypes[vs->type_def].name.isEmpty() ); // holds in SDL
                enumTypes[vs->type_def].name = name;
            }else
            {
#if 0
                out << "typedef " << name << " " << vs->type_def << " ";
                renderTypeName(out,vs->type_def);
                out << endl;
#endif
            }
        }
    }

#if 0
    QHash<Type*,EnumType>::const_iterator i;
    for( i = enumTypes.begin(); i != enumTypes.end(); ++i )
    {
        qDebug() << "ENUM" << i.value().name;
        for( int j = 0; j < i.value().items.size(); j++ )
            qDebug() << "    " << i.value().items[j].name << i.value().items[j].value;
    }
#endif
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
    Type* r = (func->return_ty && func->return_ty->kind != TY_VOID) ? func->return_ty : 0;
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

static inline QByteArray getTypeName(Type* t)
{
    QByteArray typeName = typeNames.value(t);
    if( typeName.isEmpty() )
        typeName = enumTypes.value(t).name;
    if( typeName.isEmpty() && t->origin )
        typeName = typeNames.value(t->origin);
    return typeName;
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

static bool renderTypeDecl(QTextStream& out, Type* t, int level )
{
    switch( t->kind )
    {
    // TODO: unsigned
    case TY_BOOL:
        out << "boolean";
        break;
    case TY_CHAR:
        if( t->is_unsigned )
            out << "byte";
        else
            out << "char";
        break;
    case TY_SHORT:
        out << "shortint";
        break;
    case TY_INT:
        out << "integer";
        break;
    case TY_LONG:
        out << "integer";
        break;
    case TY_FLOAT:
        out << "real";
        break;
    case TY_DOUBLE:
        out << "longreal";
        break;
    case TY_LDOUBLE:
        out << "longreal"; // TODO
        break;
    case TY_VOID:
        out << "void";
        break;
    case TY_PTR:
        if( t->base->kind == TY_PTR ) // **type
        {
            out << "*[]";
            if( t->base->base->kind == TY_PTR )
            	qCritical() << "pointers like ***type are not supported";
#if 0
            Type* ptr = t->base->base;
            while( ptr && ptr->kind == TY_PTR  ) // !isBaseType(ptr->base)
            {
                out << "[]";
                ptr = ptr->base;
            }
#else
            if( t->base->base->kind == TY_PTR )
            	qCritical() << "pointers like ***type are not supported";
#endif
            if( isBaseType(t->base->base) && t->base->base->kind != TY_VOID )
            {
                out << "*[]";
                renderTypeDecl(out, t->base->base, level );
            }else
                renderTypeDecl(out, t->base, level );
        }else if( isBaseType(t->base) && t->base->kind != TY_VOID )
        {
            out << "*[]";
            renderTypeName(out,t->base,level);
        }else
        {
            if( t->base->kind != TY_FUNC )
                out << "*";
            renderTypeName(out,t->base,level);
        }
        break;
    case TY_ARRAY:
        out << "carray ";
        if( t->array_len )
            out << t->array_len << " ";
        out << "of ";
        renderTypeName(out,t->base,level);
        break;
    case TY_FUNC:
        out << "proc";
        renderParams(out,t);
        break;
    case TY_ENUM:
        return renderEnum(out,t,level);
        break;
    case TY_STRUCT:
    case TY_UNION:
        {
            if( t->kind == TY_STRUCT )
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
    case TY_VLA:
        qWarning() << "VLA not supported:" << renderFilePos(t->name_pos);
        break;
    }
    return false;
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

static void dumpNode(Node* n, int level)
{
    if( n == 0 )
        return;
    QTextStream out(stdout);
    QByteArray tab(level*2,' ');
    out << tab << s_nodeKind[n->kind] << " ";
    if( n->kind == ND_NUM )
        out << formatNum(n->tok);
    out << endl;
    if( n->lhs )
        dumpNode(n->lhs,level+1);
    if( n->rhs )
        dumpNode(n->rhs,level+1);
    if( n->next )
        dumpNode(n->next,level);
}

static QByteArray renderNode(Node* n)
{
    if( n == 0 )
        throw "";
    switch(n->kind)
    {
    case ND_ADD:       // +
        return renderNode(n->lhs) + " + " + renderNode(n->rhs);
    case ND_SUB:       // -
        return renderNode(n->lhs) + " - " + renderNode(n->rhs);
    case ND_MUL:       // *
        return renderNode(n->lhs) + " * " + renderNode(n->rhs);
    case ND_DIV:       // /
        if( n->ty && ( n->ty->kind == TY_FLOAT || n->ty->kind == TY_DOUBLE || n->ty->kind == TY_LDOUBLE ))
            return renderNode(n->lhs) + " / " + renderNode(n->rhs);
        else
            return renderNode(n->lhs) + " div " + renderNode(n->rhs);
    case ND_NEG:       // unary -
        return "-" + renderNode(n->lhs);
    case ND_MOD:       // %
        return renderNode(n->lhs) + " mod " + renderNode(n->rhs);
    case ND_BITAND:    // &
        return "bitand(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case ND_BITOR:     // |
        return "bitor(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case ND_BITXOR:    // ^
        return "bitxor(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case ND_SHL:       // <<
        return "lsl(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case ND_SHR:       // >>
        return "ror(" + renderNode(n->lhs) + ", " + renderNode(n->rhs) + ")";
    case ND_EQ:        // ==
        return "(" + renderNode(n->lhs) + " = " + renderNode(n->rhs) + ")";
    case ND_NE:        // !=
        return "(" + renderNode(n->lhs) + " # " + renderNode(n->rhs) + ")";
    case ND_LT:        // <
        return "(" + renderNode(n->lhs) + " < " + renderNode(n->rhs) + ")";
    case ND_LE:        // <=
        return "(" + renderNode(n->lhs) + " <= " + renderNode(n->rhs) + ")";
    case ND_NOT:       // !
        return "not " + renderNode(n->lhs);
    case ND_BITNOT:    // ~
        return "bitnot(" + renderNode(n->lhs) + ")";
    case ND_LOGAND:    // &&
        return "(" + renderNode(n->lhs) + " & " + renderNode(n->rhs) + ")";
    case ND_LOGOR:     // ||
        return "(" + renderNode(n->lhs) + " or " + renderNode(n->rhs) + ")";
    case ND_VAR:       // Variable
        switch( n->tok->kind )
        {
        case TK_IDENT:
            return defix(QByteArray(n->tok->loc,n->tok->len));
        case TK_STR:
            return QByteArray(n->tok->loc,n->tok->len);
        default:
            throw "";
        }
        break;
    case ND_NUM:       // Integer
        return formatNum(n->tok);
    case ND_CAST:      // Type cast
        return renderNode(n->lhs);
    default:
        throw "";
    }

    return QByteArray();
}

static QByteArray concatToken(Token* tok )
{
    QByteArray res;
    for (Token *cur = tok; cur && cur->kind != TK_EOF; cur = cur->next)
    {
        res += QByteArray(cur->loc,cur->len);
    }
    return res;
}

static QByteArray renderMacro(Token* e)
{
    //Token* start = e;
    QByteArray res;

    //qDebug() << "*** before";
    //printTok(start,true);
    e = preprocess(e);
    //qDebug() << "*** after";
    //printTok(e,true);
    //qDebug() << "*** tokens:" << concatToken(e);
    //qDebug() << "*** nodes";
    Node* n = expr_checked(e);
    //res = renderNode(res);
    //dumpNode(n,0);
    try
    {
        return renderNode(n);
    }catch(...)
    {
        return "nil // CHECK: " + concatToken(e);
    }

    //qDebug() << "*** end";
    return res;
    //return renderExpr(e);
}

static void renderModule()
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

static void define(char *str) {
  char *eq = strchr(str, '=');
  if (eq)
    define_macro(strndup(str, eq - str), eq + 1);
  else
    define_macro(str, "1");
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/C2OBX");
    a.setApplicationName("C2OBX");
    a.setApplicationVersion("2021-11-10");

    QTextStream out(stdout);

    out << "C2OBX version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;
    QStringList files;
    QByteArrayList includes;
    const QStringList args = QCoreApplication::arguments();
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "usage: C2OBX [options] <c header file>" << endl;
            out << "  reads a C header and translates it to an Oberon+ module." << endl;
            out << "options:" << endl;
            out << "  -m module     module name" << endl;
            out << "  -p prefix     the prefix to remove" << endl;
            out << "  -Ipath        include path" << endl;
            out << "  -h            display this information" << endl;
            out << "  -Ddefine      add define" << endl;
            return 0;
        }else if( args[i] == "-m" )
        {
            if( i+1 < args.size() )
            {
                modName = args[i+1].toUtf8();
                i++;
            }
        }else if( args[i] == "-p" )
        {
            if( i+1 < args.size() )
            {
                prefix = args[i+1].toUtf8();
                i++;
            }
        }else if( args[i].startsWith("-I") )
        {
            includes.append(args[i].mid(2).toUtf8());
            strarray_push(&include_paths, includes.back().data() );
        }
        else if( args[i].startsWith("-D") )
        {
            define(args[i].mid(2).toUtf8().data());
        }
        else if( !args[ i ].startsWith( '-' ) )
        {
            files += args[ i ];
        }else
        {
            qCritical() << "error: invalid command line option " << args[i];
            return -1;
        }
    }
    if( files.size() != 1 )
    {
        qCritical() << "error: expecting one header file";
    }
    QDir::setCurrent( QFileInfo(files.first()).absolutePath() );
    QByteArray base = files.first().toUtf8();
    base_file = base.data();
    Token *tok = must_tokenize_file(base_file);
    //printMacros();
    //printTok(tok);
    tok = preprocess(tok);
    //printMacros();
    //printTok(tok);
    Obj *prog = parse(tok);
    // never arrives here in case of C errors
    processTypes();
    processFunctions(prog);
    renderModule();
    //printTok(tok);

    return 0; // a.exec();
}
