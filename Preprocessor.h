#ifndef PREPROCESSOR_H
#define PREPROCESSOR_H

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

#include <QByteArray>
#include <QList>

namespace C
{
struct Token;

class Preprocessor
{
public:
    static void setIgnoreMissingIncludes(bool on);
    static QList<QByteArray> include_paths;
    static QByteArray base_file;
    static QByteArray search_include_paths(QByteArray filename);
    static long eval_const_expr(Token **rest, Token* tok);
    static void init_macros();
    static void init_builtins();
    static Token *preprocess(Token *tok);
private:
    Preprocessor();
};
}

#endif // PREPROCESSOR_H
