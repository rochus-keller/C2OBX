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

#include "Type.h"
#include "Tokenizer.h"
#include <string.h>
#include <QtDebug>
using namespace C;

Type * Type::_void = new Type(Type::VOID, 1, 1);
Type * Type::_bool = new Type(Type::BOOL, 1, 1);

Type * Type::_char = new Type(Type::CHAR, 1, 1);
Type * Type::_short = new Type(Type::SHORT, 2, 2);
Type * Type::_int = new Type(Type::INT, 4, 4);
Type * Type::_long = new Type(Type::LONG, 8, 8);

Type * Type::_uchar = new Type(Type::CHAR, 1, 1, true);
Type * Type::_ushort = new Type(Type::SHORT, 2, 2, true);
Type * Type::_uint = new Type(Type::INT, 4, 4, true);
Type * Type::_ulong = new Type(Type::LONG, 8, 8, true);

Type * Type::_float = new Type(Type::FLOAT, 4, 4);
Type * Type::_double = new Type(Type::DOUBLE, 8, 8);
Type * Type::_ldouble = new Type(Type::LDOUBLE, 16, 16);


Type::Type(Type::Kind k, int s, int a, bool u):kind(k),size(s),align(a),is_unsigned(u),
    is_atomic(false),origin(0),base(0),name(0),name_pos(0),array_len(0),vla_len(0),vla_size(0),
    members(0),is_flexible(0),is_packed(0),return_ty(0),params(0),is_variadic(0),next(0),tag(0),typeName(0)
{
}

bool Type::is_integer() const
{
    Kind k = kind;
    return k == BOOL || k == CHAR || k == SHORT ||
            k == INT  || k == LONG || k == ENUM;
}

bool Type::is_flonum() const
{
    return kind == FLOAT || kind == DOUBLE || kind == LDOUBLE;
}

bool Type::is_numeric() const
{
    return is_integer() || is_flonum();
}

bool Type::is_compatible(const Type* t2) const
{
    const Type* t1 = this;
    if (t1 == t2)
        return true;

    if (t1->origin)
        return t1->origin->is_compatible(t2);

    if (t2->origin)
        return t1->is_compatible(t2->origin);

    if (t1->kind != t2->kind)
        return false;

    switch (t1->kind) {
    case CHAR:
    case SHORT:
    case INT:
    case LONG:
        return t1->is_unsigned == t2->is_unsigned;
    case FLOAT:
    case DOUBLE:
    case LDOUBLE:
        return true;
    case PTR:
        return t1->base->is_compatible(t2->base);
    case FUNC: {
            if (!t1->return_ty->is_compatible(t2->return_ty))
                return false;
            if (t1->is_variadic != t2->is_variadic)
                return false;

            Type *p1 = t1->params;
            Type *p2 = t2->params;
            for (; p1 && p2; p1 = p1->next, p2 = p2->next)
                if (!p1->is_compatible(p2))
                    return false;
            return p1 == 0 && p2 == 0;
        }
    case ARRAY:
        if (!t1->base->is_compatible(t2->base))
            return false;
        return t1->array_len < 0 && t2->array_len < 0 &&
                t1->array_len == t2->array_len;
    }
    return false;
}

Type*Type::new_type(Type::Kind kind, int size, int align)
{
    return new Type( kind, size, align);
}

Type*Type::enum_type()
{
    return new_type(ENUM, 4, 4);
}

Type*Type::struct_type()
{
    return new_type(STRUCT, 0, 1);
}

Type*Type::array_of(int len) const
{
    Type *ty = new_type(ARRAY, size * len, align);
    ty->base = const_cast<Type*>(this);
    ty->array_len = len;
    return ty;
}

Type*Type::copy_type() const
{
    Type* ret = new Type();
    *ret = *this;
    ret->origin = const_cast<Type*>(this);
    return ret;
}

Type*Type::pointer_to() const
{
    Type *ty = new_type(PTR, 8, 8); // TODO: 8 or 4 maybe sizeof(void*)
    ty->base = const_cast<Type*>(this);
    ty->is_unsigned = true;
    return ty;
}

Type*Type::func_type() const
{
    // The C spec disallows sizeof(<function type>), but
    // GCC allows that and the expression is evaluated to 1.
    Type *ty = new_type(FUNC, 1, 1);
    ty->return_ty = const_cast<Type*>(this);
    return ty;
}

Type*Type::vla_of(Node* len) const
{
    Type *ty = new_type(VLA, 8, 8);
    ty->base = const_cast<Type*>(this);
    ty->vla_len = len;
    return ty;
}

Type*Type::get_common_type(const Type* ty2) const
{
    const Type *ty1 = this;
    if (ty1->base)
        return ty1->base->pointer_to();

    if (ty1->kind == FUNC)
        return ty1->pointer_to();
    if (ty2->kind == FUNC)
        return ty2->pointer_to();

    if (ty1->kind == LDOUBLE || ty2->kind == LDOUBLE)
        return _ldouble;
    if (ty1->kind == DOUBLE || ty2->kind == DOUBLE)
        return _double;
    if (ty1->kind == FLOAT || ty2->kind == FLOAT)
        return _float;

    if (ty1->size < 4)
        ty1 = _int;
    if (ty2->size < 4)
        ty2 = _int;

    if (ty1->size != ty2->size)
        return (ty1->size < ty2->size) ? const_cast<Type*>(ty2) : const_cast<Type*>(ty1);

    if (ty2->is_unsigned)
        return const_cast<Type*>(ty2);
    else
        return const_cast<Type*>(ty1);
}

Member*Type::get_struct_member(Token* tok) const
{
    // Find a struct member by name.
    Type* ty = const_cast<Type*>(this);
    for (Member *mem = ty->members; mem; mem = mem->next) {
        // Anonymous struct member
        if ((mem->ty->kind == Type::STRUCT || mem->ty->kind == Type::TUNION) &&
                !mem->name) {
            if (mem->ty->get_struct_member(tok))
                return mem;
            continue;
        }

        // Regular struct member
        if( mem->name->txt == tok->txt )
            return mem;
    }
    return NULL;
}


Member::Member()
{
    ::memset(this,0,sizeof(Member));
}
