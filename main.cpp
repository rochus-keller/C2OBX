#include <QCoreApplication>
#include <QFileInfo>
#include <QStringList>
#include <QtDebug>
#include <QDir>

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

struct Decls
{
    QMap<quint32,Type*> types; // line->Type
    QMap<quint32,Obj*> functions; // line->Obj
};
QMap<QByteArray,Decls> declOrder; // file->Decls

QHash<Type*,QByteArray> typeNames;
QSet<Type*> usedTypes;
static QByteArray modName;

static inline QString renderFilePos(Token* tok)
{
    if( tok )
        return QString("%1:%2" ).arg(QFileInfo(tok->filename).fileName()).arg(tok->line_no);
    else
        return "???";
}

static QByteArray defix( const QByteArray& name )
{
    if( !modName.isEmpty() && name.startsWith(modName) )
    {
        int pos = modName.size();
        if( pos + 1 < name.size() && name[pos] == '_' )
            pos++;
        return name.mid(pos);
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

static void renderType( QTextStream& out, Type* t )
{
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

    if( typeName.isEmpty() )
    {
        switch(t->kind)
        {
        case TY_BOOL: out << "boolean"; break;
        case TY_CHAR: out << "char"; break;
        case TY_SHORT: out << "shortint"; break;
        case TY_INT: out << "integer"; break;
        case TY_LONG: out << "longint"; break; // TODO
        case TY_FLOAT: out << "real "; break;
        case TY_DOUBLE: out << "longreal "; break;
        case TY_LDOUBLE: out << "longreal "; break; // TODO
        case TY_VOID: out << "void "; break;
#if 0
        case TY_PTR: out << "PTR "; break;
        case TY_ARRAY: out << "ARRAY "; break;
        case TY_FUNC: out << "FUNC "; break;
#else
        case TY_PTR:
            if( t->base->kind != TY_FUNC )
                out << "*";
            renderType(out,t->base);
            break;
        case TY_ARRAY:
            out << "[]";
            renderType(out,t->base);
            break;
        case TY_FUNC:
            out << "proc";
            renderParams(out,t);
            break;

#endif
#if 0
        case TY_ENUM: out << "ENUM "; break;
        case TY_VLA: out << "VLA "; break;
        case TY_STRUCT: out << "STRUCT "; break;
        case TY_UNION: out << "UNION "; break;
#endif
        default:
            out << "???";
        }
    }

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
    Type* p = var->ty->params;
    while(p)
    {
        if( p != var->ty->params )
            out << ", ";
        if( p->name_pos )
            out << QByteArray::fromRawData(p->name_pos->loc,p->name_pos->len) << ": ";
        renderType(out,p);
        p = p->next;
    }
    out << ")";
    Q_ASSERT( var->ty->return_ty );
    out << ": ";
    renderType(out,var->ty->return_ty);
}

static inline void registerFunction(Obj* o)
{
    if( o->ty->name_pos )
    {
        Q_ASSERT( declOrder[o->ty->name_pos->filename].functions[o->ty->name_pos->line_no] == 0 );
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

static void renderTok( QTextStream& out, Token* cur, int level = 0 )
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
        out << "STR " << cur->str << " "; break;
    case TK_NUM:
        out << "NUM " << cur->val << " " << double(cur->fval) << " "; break;
    case TK_PP_NUM:
        out << "PPNUM "; break;
    case TK_EOF:
        out << "EOF "; break;
    }
    out << QByteArray(cur->loc).mid(0,cur->len) << " ";
    if( cur->ty )
        renderType(out, cur->ty);
    out << endl;
}

struct Hideset {
  Hideset *next;
  char *name;
};

static void printTok(Token *tok)
{
    QTextStream out(stdout);
    for (Token *cur = tok; cur; cur = cur->next)
    {
        renderTok(out,cur);
        if( cur->origin )
            renderTok(out,cur->origin, 1);
        if( cur->hideset )
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

static void processTypes()
{
    Scope* myScope = globalScope;
    QTextStream out(stdout);
    for( int i = 0; i < myScope->tags.capacity; i++ )
    {
        HashEntry* e = &myScope->tags.buckets[i];
        if( e->key == 0 )
            continue;
        Type* t = (Type*)e->val;
        Q_ASSERT( t->kind == TY_STRUCT || t->kind == TY_UNION || t->kind == TY_ENUM ); // holds in SDL
#if 0
        out << "type " << QByteArray::fromRawData(e->key, e->keylen) << " ";
        renderType(out,t);
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
        if( vs->enum_ty )
        {
            const QByteArray name = QByteArray::fromRawData(e->key, e->keylen);
            enumTypes[vs->enum_ty].items.append(EnumItem(name, vs->enum_val, vs->enum_ty));
#if 0
            out << "enum " << QByteArray::fromRawData(e->key, e->keylen) << " " << vs->enum_val << " (";
            renderType(out,vs->enum_ty);
            out << ")" << endl;
#endif
        }/*else if( vs->var )
        {
            out << "global ";
            printObj(out,vs->var);
            out << endl;
        }*/else if( vs->type_def )
        {
            const QByteArray name = QByteArray::fromRawData(e->key, e->keylen);
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
                Q_ASSERT( enumTypes[vs->type_def].name.isEmpty() ); // holds in SDL
                enumTypes[vs->type_def].name = name;
            }else
            {
#if 0
                out << "typedef " << name << " ";
                renderType(out,vs->type_def);
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
        renderType(out,p);
        p = p->next;
        nr++;
    }

    if( func->params || r )
        out << ")";
    if( r )
    {
        out << ": ";
        renderType(out,r);
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

static void renderEnum(QTextStream& out, Type* t, int level)
{
    const EnumType& et = enumTypes[t];
    out << "(";
    for(int i = 0; i < et.items.size(); i++ )
    {
        if( i != 0 )
            out << ", ";
        out << escape(defix(et.items[i].name));
    }
    out << ")";
}

static inline QByteArray ws(int level) { return QByteArray(level*4,' '); }

static void renderTypeDecl(QTextStream& out, Type* t, int level = 3 )
{
    switch( t->kind )
    {
    case TY_ENUM:
        renderEnum(out,t,level);
        break;
    case TY_BOOL:
        out << "boolean";
        break;
    case TY_CHAR:
        out << "char";
        break;
    case TY_SHORT:
        out << "shortint";
        break;
    case TY_INT:
        out << "integer";
        break;
    case TY_LONG:
        out << "longint"; // TODO
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
    case TY_FUNC:
        out << "proc";
        renderParams(out,t);
        break;
    case TY_PTR:
        out << "*";
        renderTypeDecl(out,t->base,level);
        break;
    case TY_ARRAY:
        out << "carray of ";
        renderTypeDecl(out,t->base,level);
        break;
    case TY_STRUCT:
        out << "cstruct end"; // TODO
        break;
    case TY_UNION:
        out << "cunion end"; // TODO
        break;
    case TY_VOID:
        out << "void";
        break;
    case TY_VLA:
        Q_ASSERT( false );
    }
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
    out << "definition " << escape(modName) << endl;

    QMap<QByteArray,Decls>::const_iterator i;

    for( i = declOrder.begin(); i != declOrder.end(); ++i )
    {
        out << endl << "    // from " << QFileInfo(i.key()).fileName() << endl;

        QMap<quint32,Type*>::const_iterator k;
        bool headerDone = false;
        for( k = i.value().types.begin(); k != i.value().types.end(); ++k )
        {
            Type* t = k.value();
            if( !enumTypes.contains(t) && !usedTypes.contains(t) )
                continue;
            if( !headerDone )
            {
                out << "    type" << endl;
                headerDone = true;
            }
            out << ws(2) << escape(defix(getTypeName(t))) << " = ";
            renderTypeDecl(out,t);
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
            out << endl;
        }
    }

    out << "end " << escape(modName) << endl;
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    QTextStream out(stdout);

    QStringList files;
    QByteArrayList includes;
    const QStringList args = QCoreApplication::arguments();
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "options:" << endl;
            out << "  -m module     module name" << endl;
            out << "  -Ipath        include path" << endl;
            out << "  -h            display this information" << endl;
            return 0;
        }else if( args[i] == "-m" )
        {
            if( i+1 < args.size() )
            {
                modName = args[i+1].toUtf8();
                i++;
            }
        }else if( args[i].startsWith("-I") )
        {
            includes.append(args[i].mid(2).toUtf8());
            strarray_push(&include_paths, includes.back().data() );
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
    QByteArray base = files.first().toUtf8();
    base_file = base.data();
    Token *tok = must_tokenize_file(base_file);
    //printTok(tok);
    tok = preprocess(tok);
    Obj *prog = parse(tok);
    processTypes();
    processFunctions(prog);
    renderModule();
    //printTok(tok);

    return 0; // a.exec();
}
