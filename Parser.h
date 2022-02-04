#ifndef PARSER_H
#define PARSER_H

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

#include <QString>
#include <QHash>

namespace C
{
struct Token;
struct Obj;
struct Type;
struct Node;

// Scope for local variables, global variables, typedefs
// or enum constants
struct ObjScope {
    QByteArray name;
    Obj *obj;
    Type *typedef_;
    Type *enum_ty;
    int enum_val;
    ObjScope():obj(0),typedef_(0),enum_ty(0),enum_val(0){}
};

// Represents a block scope.
struct Scope {
    Scope *next;

    // C has two block scopes; one is for variables/typedefs and
    // the other is for struct/union/enum tags.
    typedef QHash<QByteArray,ObjScope*> Objs;
    Objs objs; // typedef and object scope
    typedef QHash<QByteArray,Type*> Tags;
    Tags tags; // struct/union/enum scope

    Scope():next(0){}
};

class Parser
{
public:
    static Scope *scope; // global scope
    static Obj *globalVars;
    static QList<Obj*> funcs;
    static QSet<Type*> typeDecls; // all global types named by tag and/or typedef
    static QSet<Type*> anonymousEnums;
    static qint64 const_expr(Token **rest, Token *tok);
    static Node *expr_checked(Token *tok);
    static Obj *parse(Token *tok);
private:
    Parser();
};
}

#endif // PARSER_H
