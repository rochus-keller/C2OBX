/*
* Copyright 2024 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the C2OBX parser library.
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

#include "DependencyAnalyzer.h"
#include <QCoreApplication>
#include <QFileInfo>
#include <QStringList>
#include <QtDebug>
#include <QDir>
#include <QDateTime>
#include "Tokenizer.h"
#include "Preprocessor.h"
#include "Parser.h"
#include "Ast.h"
#include "Type.h"

DependencyAnalyzer::DependencyAnalyzer()
{

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

static int readArgs(const QStringList& args, QStringList& files, QString& optionFile, QString& outFile,
                    bool& onlyMainDeps, bool& clustered, bool& clusterByStatics, bool& haveStructs)
{
    QTextStream out(stdout);
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if( args[i].isEmpty() )
            continue;
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "usage: DA [options] <c file>" << endl;
            out << "  reads a C header and translates it to an Oberon+ module." << endl;
            out << "options:" << endl;
            out << "  -f file       load the command line options from file" << endl;
            out << "  -Ipath        include path" << endl;
            out << "  -h            display this information" << endl;
            out << "  -Ddefine      add define" << endl;
            out << "  -main         only deps of main" << endl;
            out << "  -fg           group by file" << endl;
            out << "  -st           show structured types" << endl;
            return 1;
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
        }else if( args[i] == "-main" )
            onlyMainDeps = true;
        else if( args[i] == "-fg" )
            clustered = true;
        else if( args[i] == "-st" )
            haveStructs = true;
        else if( args[i].startsWith("-I") )
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

static void printAllFuncs(const QSet<QString>& paths)
{
    QTextStream out(stdout);
    for( int i = 0; i < C::Parser::funcs.size(); i++ )
    {
        C::Obj* func = C::Parser::funcs[i];
        if( func->is_definition )
        {
            // function body
            QFileInfo fi(func->definitionTok->filename);
            if( paths.contains(fi.absolutePath()) )
                out << func->name << " " << (func->is_static?"static ":"") << fi.fileName() << " "
                    << func->definitionTok->line_no << endl;
        }else
        {
            // function with only header
            QFileInfo fi(func->tok->filename);
            if( paths.contains(fi.absolutePath()) )
                out << "!!" << func->name << " " << (func->is_static?"static ":"") << fi.fileName() << " "
                    << func->tok->line_no << endl;
        }
    }
}

static QString toId(void* o)
{
    return QString("L%1").arg(quint64(o),0,16);
}

static void collectUses(const QSet<QString>& paths, QSet<C::Obj*>& objs, C::Obj* root)
{
    foreach( C::Obj* u, root->uses )
    {
        if( !objs.contains(u) )
        {
            if( u->is_definition && u->definitionTok )
            {
                // function body
                QFileInfo fi(u->definitionTok->filename);
                if( paths.contains(fi.absolutePath()) )
                {
                    objs << u;
                    collectUses(paths, objs, u );
                }
            }
        }
    }
}

static void printDependencyGraph(const QSet<QString>& paths, const QString& fileName,
                                 bool onlyMainDeps, bool clustered, bool clusterByStatics, bool haveStructs)
{
    QFile f(fileName);
    if( !f.open(QIODevice::WriteOnly) )
        return;
    QTextStream s(&f);

    // use with transitive reduction, tred in > out
    // use with dot -Tpdf in > out

    s << "digraph \"Function Dependency Graph\" {" << endl;
    s << "    graph [splines=ortho]" << endl;
    s << "    node [shape=box]" << endl;
    //s << "    concentrate=true" << endl;

    QSet<C::Obj*> objs;

    for( int i = 0; i < C::Parser::funcs.size(); i++ )
    {
        C::Obj* func = C::Parser::funcs[i];
        if( onlyMainDeps )
        {
            if( !func->is_static && strcmp(func->name,"main") == 0 )
            {
                objs << func;
                collectUses(paths, objs,func);
                break;
            }
        }else if( func->is_definition && func->definitionTok )
        {
            // function body
            QFileInfo fi(func->definitionTok->filename);
            if( paths.contains(fi.absolutePath()) )
                objs << func;
        }
    }

    QHash<C::File*,QSet<C::Obj*> > modules;
    QHash<C::Obj*,QSet<C::Obj*> > statics; // static -> set of all pointing to it
    QSet<C::Type*> allTypes;
    for( int i = 0; i < C::Parser::funcs.size(); i++ )
    {
        C::Obj* func = C::Parser::funcs[i];
        if( objs.contains(func))
        {
            // function body
            s << "    " << toId(func) << " [label=\"" << func->name << "\\n";
            if( func->body )
                s << "F ";
            else
                s << "G ";
            if( func->is_static )
                s << "S ";
            QFileInfo fi(func->definitionTok->filename);
            s << fi.fileName() << ":" << func->definitionTok->line_no;
            s << "\"];" << endl;
            modules[func->definitionTok->file].insert(func);
            if( clusterByStatics )
            {
                if( func->is_static || func->usedBy.isEmpty() )
                    statics[func].insert(func);
                foreach( C::Obj* u, func->uses )
                {
                    if( u->is_static && objs.contains(u) )
                    {
                        statics[u].insert(func);
                        statics[u].insert(u);
                    }
                }
            }
            if( haveStructs )
            {
                func->collectTypes();
                foreach( C::Type* t, func->usesT )
                {
                    if( t->typeName == 0 || allTypes.contains(t) )
                        continue;
                    QFileInfo fi(t->typeName->filename);
                    if( paths.contains(fi.absolutePath()) )
                        allTypes.insert(t);
                }
            }
        }
    }

    if( clustered && !clusterByStatics )
    {
        QHash<C::File*,QSet<C::Obj*> >::const_iterator i;
        for( i = modules.begin(); i != modules.end(); ++i )
        {
            s << "    subgraph cluster_" << toId(i.key()) << "{" << endl;
            s << "        style=filled; color=lightgrey;" << endl;
            s << "        label = \"" << QFileInfo(i.key()->name).fileName() << "\";" << endl;
            QSet<C::Obj*>::const_iterator j;
            for( j = i.value().begin(); j != i.value().end(); ++j)
                s << "        " << toId((*j)) << ";" << endl;
            s << "    }" << endl;
        }
    }else if( clustered && clusterByStatics )
    {
        // TODO: this doesn't find all relevant clusters yet
        QList< QSet<C::Obj*> > cluster = statics.values();
        bool changed;
        do
        {
            changed = false;
            for( int j = 0; j < cluster.size(); j++ )
            {
                for( int k = j+1; k < cluster.size(); k++ )
                {
                    const QSet<C::Obj*> cut = cluster[j] & cluster[k];
                    if( !cut.isEmpty() )
                    {
                        changed = true;
                        cluster[j].unite(cut);
                        cluster[k].clear();
                    }
                }
            }
        }while(changed);

        for( int i = 0; i < cluster.size(); i++ )
        {
            if( cluster[i].isEmpty() )
                continue;
            s << "    subgraph cluster_" << i << "{" << endl;
            s << "        style=filled; color=lightgrey;" << endl;
            QSet<C::Obj*>::const_iterator j;
            for( j = cluster[i].begin(); j != cluster[i].end(); ++j)
                s << "        " << toId((*j)) << ";" << endl;
            s << "    }" << endl;
        }
    }

    if( haveStructs )
    {
        foreach( C::Type* t, allTypes )
        {
            s << "    " << toId(t) << " [label=\"" << t->typeName->txt << "\"];" << endl;
        }
    }

    for( int i = 0; i < C::Parser::funcs.size(); i++ )
    {
        C::Obj* func = C::Parser::funcs[i];
        if( objs.contains(func))
        {
            // function body
            s << "    " << toId(func) << " -> {";
            bool first = true;
            foreach( C::Obj* u, func->uses )
            {
                if( !objs.contains(u) )
                    continue;
                if( !first )
                    s << ", ";
                first = false;
                s << toId(u);
            }
            if( haveStructs )
            {
                foreach( C::Type* t, func->usesT )
                {
                    if( !allTypes.contains(t) )
                        continue;
                    if( !first )
                        s << ", ";
                    first = false;
                    s << toId(t);
                }
            }

            s << "};" << endl;
        }
    }

    s << "}";
}

static int calcRanks( QSet<C::Obj*>& guard, C::Obj* obj)
{
    if( guard.contains(obj) )
        return 0;
    guard.insert(obj);
    int rank = obj->rank;
    foreach( C::Obj* o, obj->uses )
    {
        if( !o->is_function )
            continue;
        if( o->rank < obj->rank + 1 )
            o->rank = obj->rank + 1;
        const int res = calcRanks(guard,o);
        if( res > rank )
            rank = res;
    }
    foreach( C::Type* t, obj->usesT )
    {
        if( t->rank < obj->rank )
            t->rank = obj->rank;
    }
    return rank;
}

static int calcRanks(const QSet<QString>& paths, bool onlyMainDeps)
{
    QList<C::Obj*> roots;
    for( int i = 0; i < C::Parser::funcs.size(); i++ )
    {
        C::Obj* func = C::Parser::funcs[i];
        if( func->is_definition && func->definitionTok )
        {
            // function body
            QFileInfo fi(func->definitionTok->filename);
            if( paths.contains(fi.absolutePath()) )
            {
                func->collectTypes();
                if( (onlyMainDeps && func->nameBuf == "main") ||
                        (!onlyMainDeps && func->usedBy.isEmpty()) )
                    roots << func;
            }
        }
    }

    int rank = 0;
    for( int i = 0; i < roots.size(); i++ )
    {
        QSet<C::Obj*> guard;
        const int res = calcRanks(guard,roots[i]);
        if( res > rank )
            rank = res;
    }
    return rank;
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);
    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/C2OBX");
    a.setApplicationName("C2OBX");
    a.setApplicationVersion("2024-02-07");

    QTextStream out(stdout);

    C::Preprocessor::init_builtins();

    out << "DA version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;
    QStringList files;
    QString optionFile, outFile;
    bool onlyMainDeps = false, clustered = false, clusterByStatics = false, haveStructs = false;
    const QStringList args = QCoreApplication::arguments();
    const int res = readArgs(args, files, optionFile, outFile, onlyMainDeps, clustered, clusterByStatics, haveStructs);
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

        const int res = readArgs(options, files, dummy, outFile, onlyMainDeps, clustered, clusterByStatics, haveStructs);
        if( res > 0 )
            return 0;
        if( res < 0 )
            return res;
    }
    QSet<QString> paths;
    foreach( const QString& file, files )
    {
        QFileInfo fi(file);
        paths.insert(fi.absolutePath());
        QDir::setCurrent( fi.absolutePath() );
        QByteArray base = file.toUtf8();

        C::Preprocessor::setIgnoreMissingIncludes(true);
        C::Token *tok = C::Tokenizer::tokenize(base);
        if(!tok)
            return -1;
        tok = C::Preprocessor::preprocess(tok);
        C::Parser::parse(tok);
    }
    //printAllFuncs(paths);
    printDependencyGraph(paths,"graph.dot", onlyMainDeps, clustered, clusterByStatics, haveStructs);
    // TODO: evaluation const int rank = calcRanks(paths, onlyMainDeps);

    return 0; // a.exec();
}
