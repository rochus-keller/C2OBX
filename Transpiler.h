#ifndef TRANSPILER_H
#define TRANSPILER_H

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

namespace C
{
class Transpiler
{
public:
    static void render( const QByteArray& modName, const QByteArray& prefix );
private:
    Transpiler();
};

}

#endif // TRANSPILER_H
