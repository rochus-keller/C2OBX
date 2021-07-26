#include <QCoreApplication>
#include <QFileInfo>
#include <QStringList>
#include <QtDebug>

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

static Token *must_tokenize_file(char *path) {
  Token *tok = tokenize_file(path);
  if (!tok)
    error("%s: %s", path, strerror(errno));
  return tok;
}

static void renderType( QTextStream& out, Type* t )
{
    switch(t->kind)
    {
    case TY_VOID: out << "VOID "; break;
    case TY_BOOL: out << "BOOL "; break;
    case TY_CHAR: out << "CHAR "; break;
    case TY_SHORT: out << "SHORT "; break;
    case TY_INT: out << "INT "; break;
    case TY_LONG: out << "LONG "; break;
    case TY_FLOAT: out << "FLOAT "; break;
    case TY_DOUBLE: out << "DOUBLE "; break;
    case TY_LDOUBLE: out << "LDOUBLE "; break;
    case TY_ENUM: out << "ENUM "; break;
    case TY_PTR: out << "PTR "; break;
    case TY_FUNC: out << "FUNC "; break;
    case TY_ARRAY: out << "ARRAY "; break;
    case TY_VLA: out << "VLA "; break;
    case TY_STRUCT: out << "STRUCT "; break;
    case TY_UNION: out << "UNION "; break;
    }
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

static void printAst( Obj *prog )
{
    QTextStream out(stdout);
    for (Obj *var = prog; var; var = var->next)
    {
        out << var->name << " ";
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
        if( var->ty )
        {
            renderType(out, var->ty);
            out << readValue(var->init_data, var->ty ).toString() << " ";
        }
        if( var->ty && var->ty->name_pos )
            out << QFileInfo(var->ty->name_pos->filename).fileName() << " " << var->ty->name_pos->line_no << flush;
        out << endl;
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

static void selectGlobalFunctions(Obj *prog)
{
    QTextStream out(stdout);
    for (Obj *var = prog; var; var = var->next)
    {
        if( !( var->is_function && var->is_live && var->is_root && !var->is_inline ) )
            continue;

        out << "proc " << var->name << "(";

        out << ")" << endl;
    }
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    if( a.arguments().size() == 1 )
        return -1;

    strarray_push(&include_paths, "/home/me/Entwicklung/Modules/c2obx/chibicc/clib");
    strarray_push(&include_paths, "/home/me/Entwicklung/Modules/c2obx/Testdaten/sdl2");

    QByteArray base = a.arguments()[1].toUtf8();
    base_file = base.data();
    Token *tok = must_tokenize_file(base_file);
    //printTok(tok);
    tok = preprocess(tok);
    Obj *prog = parse(tok);
    printAst(prog);
    printTok(tok);
    //selectGlobalFunctions(prog);

    return 0; // a.exec();
}
