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
        out << "\"" << QByteArray(cur->loc,cur->len) << "\" ";
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

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/C2OBX");
    a.setApplicationName("C2OBX");
    a.setApplicationVersion("2022-02-02");

    QTextStream out(stdout);

    C::Preprocessor::init_builtins();

    out << "C2OBX version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;
    QStringList files;
    QByteArray modName, prefix;
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
            C::Preprocessor::include_paths.append(args[i].mid(2).toUtf8());
        }
        else if( args[i].startsWith("-D") )
        {
            C::Tokenizer::define_macro(args[i].mid(2).toUtf8());
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

    C::Token *tok = C::Tokenizer::tokenize(base);
    if(!tok)
        return -1;
    tok = C::Preprocessor::preprocess(tok);
    //printMacros();
    //printTok(tok);
    C::Parser::parse(tok);

    C::Transpiler::render(modName, prefix);

    return 0; // a.exec();
}
