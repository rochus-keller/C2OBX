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

#include <QCoreApplication>
#include <QFileInfo>
#include <QStringList>
#include <QtDebug>
#include <QDir>
#include <QDateTime>

#include "Tokenizer.h"
#include "Preprocessor.h"
#include "Parser.h"
#include "Transpiler.h"
#include "Transpiler2.h"
#include "Type.h"

static void printMacros()
{
    QTextStream out(stdout);
    C::Tokenizer::Macros::const_iterator i;

    for( i = C::Tokenizer::macros.begin(); i != C::Tokenizer::macros.end(); ++i )
    {
        C::Macro* m = i.value();
        out << "macro " << m->name << " " << m->is_objlike << endl;
    }
}

static void renderTok( QTextStream& out, C::Token* cur, int level = 0, bool lean = false )
{
    out << QByteArray(level*2,' ');
    switch( cur->kind )
    {
    case C::Token::IDENT:
        out << "IDENT "; break;
    case C::Token::PUNCT:
        out << "PUNCT "; break;
    case C::Token::KEYWORD:
        out << "KEYWORD "; break;
    case C::Token::STR:
        out << "STR   "; break; //  << cur->str << " "; break;
    case C::Token::NUM:
        out << "NUM   "; break; // << cur->val << " " << double(cur->fval) << " "; break;
    case C::Token::PP_NUM:
        out << "PPNUM "; break;
    case C::Token::_EOF:
        out << "EOF "; break;
    }

    if( cur->len )
        out << "\"" << cur->txt << "\" ";
    if( cur->file )
        out << QFileInfo(cur->file->name).fileName() << " " << cur->line_no;
    //if( !lean && cur->ty )
    //    renderTypeName(out, cur->ty);
    out << endl;
}

static void printTok(C::Token *tok, bool lean = false)
{
    QTextStream out(stdout);
    for (C::Token *cur = tok; cur; cur = cur->next)
    {
        if( lean )
            out << cur << " ";
        renderTok(out,cur,0,lean);
#if 0
        if( !lean && cur->origin )
            renderTok(out,cur->origin, 1,lean);
#endif
    }
}

static void printAllTypeDecls()
{
    QTextStream out(stdout);
    QSet<C::Type*>::const_iterator i;
    for( i = C::Parser::typeDecls.begin(); i != C::Parser::typeDecls.end(); ++i )
    {
        C::Type* t = (*i);
        out << "type " << t->kind;
        if( t->name )
            out << " " << t->name->txt << " " << QFileInfo(t->name->filename).fileName() << " " << t->name->line_no;
        out << endl;
        if( t->tag )
            out << "    tag " << t->tag->txt << " " << QFileInfo(t->tag->filename).fileName() << " " << t->tag->line_no << endl;
        foreach( C::Token* tok, t->typedefs )
            out << "    def " << tok->txt << " " << QFileInfo(tok->filename).fileName() << " " << tok->line_no << endl;
    }
}

static inline QString toAbsolute(const QString& path)
{
    QFileInfo info(path);
    if( info.isRelative() )
    {
        const QString tmp = info.canonicalFilePath();
        return tmp;
    }else
        return path;
}

static int readArgs(const QStringList& args, QStringList& files, QByteArray& modName,
                    QByteArray& prefix, QString& optionFile, QString& outFile)
{
    QTextStream out(stdout);
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if( args[i].isEmpty() )
            continue;
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "usage: C2OBX [options] <c header file>" << endl;
            out << "  reads a C header and translates it to an Oberon+ module." << endl;
            out << "options:" << endl;
            out << "  -f file       load the command line options from file" << endl;
            out << "  -m module     module name" << endl;
            out << "  -p prefix     the prefix to remove" << endl;
            out << "  -o path       the path where the Oberon+ module is written" << endl;
            out << "  -Ipath        include path" << endl;
            out << "  -h            display this information" << endl;
            out << "  -Ddefine      add define" << endl;
            return 1;
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
        }else if( args[i] == "-f" )
        {
            if( i+1 < args.size() )
            {
                optionFile = toAbsolute(args[i+1]);
                i++;
            }
        }else if( args[i] == "-o" )
        {
            if( i+1 < args.size() )
            {
                outFile = toAbsolute(args[i+1]);
                i++;
            }
        }else if( args[i].startsWith("-I") )
        {
            C::Preprocessor::include_paths.append(toAbsolute(args[i].mid(2)).toUtf8());
        }
        else if( args[i].startsWith("-D") )
        {
            C::Tokenizer::define_macro(args[i].mid(2).toUtf8());
        }
        else if( !args[ i ].startsWith( '-' ) )
        {
            files += toAbsolute(args[ i ]);
        }else
        {
            qCritical() << "error: invalid command line option " << args[i];
            return -1;
        }
    }
    return 0;
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/C2OBX");
    a.setApplicationName("C2OBX");
    a.setApplicationVersion("2022-09-03");

    QTextStream out(stdout);

    C::Preprocessor::init_builtins();

    out << "C2OBX version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;
    QStringList files;
    QByteArray modName, prefix;
    QString optionFile, outFile;
    const QStringList args = QCoreApplication::arguments();
    const int res = readArgs(args, files, modName, prefix, optionFile, outFile);
    if( res > 0 )
        return 0;
    if( res < 0 )
        return res;
    if( !optionFile.isEmpty() )
    {
        QDir::setCurrent( QFileInfo(optionFile).absolutePath() );
        QString dummy;
        QFile in(optionFile);
        if( !in.open(QIODevice::ReadOnly) )
        {
            qCritical() << "error: cannot open options file" << optionFile;
            return -1;
        }
        QStringList options = QString::fromUtf8(in.readAll()).split('\n');
        for( int i = 0; i < options.size(); i++ )
            options[i] = options[i].trimmed();
        options.prepend(QString());

        const int res = readArgs(options, files, modName, prefix, dummy, outFile);
        if( res > 0 )
            return 0;
        if( res < 0 )
            return res;
    }
    if( files.size() != 1 )
    {
        qCritical() << "error: expecting one header file";
        return -1;
    }
    QDir::setCurrent( QFileInfo(files.first()).absolutePath() );
    QByteArray base = files.first().toUtf8();

    C::Token *tok = C::Tokenizer::tokenize(base);
    if(!tok)
        return -1;
    tok = C::Preprocessor::preprocess(tok);
    //printMacros();
    //printTok(tok);
    C::Parser::parse(tok);
    //printAllTypeDecls();

    C::Transpiler2::render(outFile, modName, prefix);

    return 0; // a.exec();
}
