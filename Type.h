#ifndef TYPE_H
#define TYPE_H

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

#include <QList>

namespace C
{
struct Token;
struct Node;
struct Obj;
struct Member;
struct ObjScope;

struct Type
{
    enum Kind {
        VOID,
        BOOL,
        CHAR,
        SHORT,
        INT,
        LONG,
        FLOAT,
        DOUBLE,
        LDOUBLE,
        ENUM,
        PTR,
        FUNC,
        ARRAY,
        VLA, // variable-length array
        STRUCT,
        TUNION,
    };
    Kind kind;
    int size;           // sizeof() value
    int align;          // alignment
    bool is_unsigned;   // unsigned or signed
    bool is_atomic;     // true if _Atomic
    Type *origin;       // for type compatibility check

    // Pointer-to or array-of type. We intentionally use the same member
    // to represent pointer/array duality in C.
    //
    // In many contexts in which a pointer is expected, we examine this
    // member instead of "kind" member to determine whether a type is a
    // pointer or not. That means in many contexts "array of T" is
    // naturally handled as if it were "pointer to T", as required by
    // the C spec.
    Type *base;

    // Declaration
    Token *name; // this is used to transport param names
    Token *name_pos;
    Token* typeName; // this is used to transport the present type alias name

    // Array
    int array_len;

    // Variable-length array
    Node *vla_len; // # of elements
    Obj *vla_size; // sizeof() value

    // Struct
    Member *members;
    bool is_flexible;
    bool is_packed;

    // Function type
    Type *return_ty;
    Type *params;
    bool is_variadic;
    Type *next;

    // enum, struct, union
    Token* tag;
    QList<Token*> typedefs;

    // enum
    QList<ObjScope*> consts;

    Type(Kind = VOID, int size = 0, int aligned = 0, bool isUnsigned = false);
    bool is_integer() const;
    bool is_flonum() const;
    bool is_numeric() const;
    bool is_compatible(const Type *t2) const;
    static Type* new_type(Kind,int size, int align);
    static Type *enum_type();
    static Type *struct_type();
    Type* array_of(int len) const;
    Type *copy_type() const;
    Type* pointer_to() const; // this: base
    Type *func_type() const; // this: return type
    Type *vla_of(Node *len) const; // this: base
    Type *get_common_type(const Type *ty2) const;
    Member *get_struct_member(Token *tok) const;

    static Type *_void;
    static Type *_bool;
    static Type *_char;
    static Type *_short;
    static Type *_int;
    static Type *_long;
    static Type *_uchar;
    static Type *_ushort;
    static Type *_uint;
    static Type *_ulong;
    static Type *_float;
    static Type *_double;
    static Type *_ldouble;
};

// Struct member
struct Member {
    Member *next;
    Type *ty;
    Token *tok; // for error message
    Token *name;
    int idx;
    int align;
    int offset;

    // Bitfield
    bool is_bitfield;
    int bit_offset;
    int bit_width;

    Member();
};

}

#endif // TYPE_H
