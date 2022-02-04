#ifndef AST_H
#define AST_H

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
#include <QList>

namespace C
{
struct Type;
struct Token;
struct Member;
struct Obj;
struct Relocation;

// AST node type
struct Node {
    enum Kind {
        NULL_EXPR, // Do nothing
        ADD,       // +
        SUB,       // -
        MUL,       // *
        DIV,       // /
        NEG,       // unary -
        MOD,       // %
        BITAND,    // &
        BITOR,     // |
        BITXOR,    // ^
        SHL,       // <<
        SHR,       // >>
        EQ,        // ==
        NE,        // !=
        LT,        // <
        LE,        // <=
        ASSIGN,    // =
        COND,      // ?:
        COMMA,     // ,
        MEMBER,    // . (struct member access)
        ADDR,      // unary &
        DEREF,     // unary *
        NOT,       // !
        BITNOT,    // ~
        LOGAND,    // &&
        LOGOR,     // ||
        RETURN,    // "return"
        IF,        // "if"
        FOR,       // "for" or "while"
        DO,        // "do"
        SWITCH,    // "switch"
        CASE,      // "case"
        BLOCK,     // { ... }
        GOTO,      // "goto"
        GOTO_EXPR, // "goto" labels-as-values
        LABEL,     // Labeled statement
        LABEL_VAL, // [GNU] Labels-as-values
        FUNCALL,   // Function call
        EXPR_STMT, // Expression statement
        STMT_EXPR, // Statement expression
        VAR,       // Variable
        VLA_PTR,   // VLA designator
        NUM,       // Integer
        CAST,      // Type cast
        MEMZERO,   // Zero-clear a stack variable
        ASM,       // "asm"
        CAS,       // Atomic compare-and-swap
        EXCH,      // Atomic exchange
    };

    Kind kind; // Node kind
    Node *next;    // Next node
    Type *ty;      // Type, e.g. int or pointer to int
    Token *tok;    // Representative token

    Node *lhs;     // Left-hand side
    Node *rhs;     // Right-hand side

    // "if" or "for" statement
    Node *cond;
    Node *then;
    Node *els;
    Node *init;
    Node *inc;

    // "break" and "continue" labels
    const char *brk_label;
    const char *cont_label;

    // Block or statement expression
    Node *body;

    // Struct member access
    Member *member;

    // Function call
    Type *func_ty;
    Node *args;
    bool pass_by_stack;
    Obj *ret_buffer;

    // Goto or labeled statement, or labels-as-values
    const char *label;
    const char *unique_label;
    Node *goto_next;

    // Switch
    Node *case_next;
    Node *default_case;

    // Case
    long begin;
    long end;

    // "asm" string literal
    QByteArray asm_str;

    // Atomic compare-and-swap
    Node *cas_addr;
    Node *cas_old;
    Node *cas_new;

    // Atomic op= operators
    Obj *atomic_addr;
    Node *atomic_expr;

    // Variable
    Obj *var;

    // Numeric literal
    qint64 val;
    long double fval;

    Node();

    Node *new_cast(Type *ty); // this: expr
    void add_type();
    Node *struct_ref(Token *tok) const;

    static Node *new_node(Node::Kind kind, Token *tok);
    static Node *new_binary(Node::Kind kind, Node *lhs, Node *rhs, Token *tok);
    static Node *new_unary(Node::Kind kind, Node *expr, Token *tok);
    static Node *new_num(qint64 val, Token *tok);
    static Node *new_long(qint64 val, Token *tok);
    static Node *new_ulong(long val, Token *tok);
    static Node *new_var_node(Obj *var, Token *tok);
    static Node *new_vla_ptr(Obj *var, Token *tok);

};

// Variable or function
struct Obj {
    Obj *next;
    const char* name;    // Variable name (pointer to nameBuf, required for taking address of member)
    QByteArray nameBuf;
    Type *ty;      // Type
    Token *tok;    // representative token
    bool is_local; // local or global/function
    int align;     // alignment

    // Local variable
    int offset;

    // Global variable or function
    bool is_function;
    bool is_definition;
    bool is_static;

    // Global variable
    bool is_tentative;
    bool is_tls;
    QByteArray init_data;
    Relocation *rel;

    // Function
    bool is_inline;
    Obj *params;
    Node *body;
    Obj *locals;
    Obj *va_area;
    Obj *alloca_bottom;
    int stack_size;

    // Static inline function
    bool is_live;
    bool is_root;
    QList<QByteArray> refs;

    Obj();
};

struct Relocation {
  Relocation *next;
  int offset;
  const char **label;
  long addend;
  Relocation():next(0),offset(0),label(0),addend(0){}
};

}

#endif // AST_H
