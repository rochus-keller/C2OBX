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

#include "Parser.h"
#include "Ast.h"
#include "Type.h"
#include "Tokenizer.h"
#include <QVector>
#include <QSet>
#include <QtDebug>
using namespace C;


// Variable attributes such as typedef or extern.
struct VarAttr {
    bool is_typedef;
    bool is_static;
    bool is_extern;
    bool is_inline;
    bool is_tls;
    int align;
    VarAttr():is_typedef(false),is_static(false),is_extern(false),is_inline(false),is_tls(false),align(0){}
} ;

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
struct Initializer {
    Initializer *next;
    Type *ty;
    Token *tok;
    bool is_flexible;

    // If it's not an aggregate type and has an initializer,
    // `expr` has an initialization expression.
    Node *expr;

    // If it's an initializer for an aggregate type (e.g. array or struct),
    // `children` has initializers for its children.
    QVector<Initializer*> children;

    // Only one member can be initialized for a union.
    // `mem` is used to clarify which member is initialized.
    Member *mem;
    Initializer():next(0),ty(0),tok(0),is_flexible(false),expr(0),mem(0){}
};

// For local variable initializer.
struct InitDesg {
    InitDesg *next;
    int idx;
    Member *member;
    Obj *var;
    InitDesg(InitDesg* n = 0,int i = 0,Member* m = 0,Obj* v = 0):next(n),idx(i),member(m),var(v){}
};

// All local variable instances created during parsing are
// accumulated to this list.
static Obj *locals = 0;

// Likewise, global variables are accumulated to this list.
Obj *Parser::globalVars = 0;

Scope *Parser::scope = new Scope();
QList<Obj*> Parser::funcs;

// Points to the function object the parser is currently parsing.
static Obj *current_fn;

// Lists of all goto statements and labels in the curent function.
static Node *gotos;
static Node *labels;

// Current "goto" and "continue" jump targets.
static const char *brk_label;
static const char *cont_label;

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
static Node *current_switch;

static Obj *builtin_alloca;

static bool is_typename(Token *tok);
static Type *declspec(Token **rest, Token *tok, VarAttr *attr);
static Type *typename_(Token **rest, Token *tok);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *typeof_specifier(Token **rest, Token *tok);
static Type *type_suffix(Token **rest, Token *tok, Type *ty);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static void array_initializer2(Token **rest, Token *tok, Initializer *init, int i);
static void struct_initializer2(Token **rest, Token *tok, Initializer *init, Member *mem);
static void initializer2(Token **rest, Token *tok, Initializer *init);
static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty);
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var);
static void gvar_initializer(Token **rest, Token *tok, Obj *var);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static qint64 eval(Node *node);
static qint64 eval2(Node *node, const char ***label);
static qint64 eval_rval(Node *node, const char ***label);
static bool is_const_expr(Node *node);
static Node *assign(Token **rest, Token *tok);
static Node *logor(Token **rest, Token *tok);
static double eval_double(Node *node);
static Node *conditional(Token **rest, Token *tok);
static Node *logand(Token **rest, Token *tok);
static Node *bitor_(Token **rest, Token *tok);
static Node *bitxor_(Token **rest, Token *tok);
static Node *bitand_(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *shift(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *new_add(Node *lhs, Node *rhs, Token *tok);
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Member *get_struct_member(Type *ty, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *funcall(Token **rest, Token *tok, Node *node);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Token *parse_typedef(Token *tok, Type *basety);
static bool is_function(Token *tok);
static Token *function(Token *tok, Type *basety, VarAttr *attr);
static Token *global_variable(Token *tok, Type *basety, VarAttr *attr);

static int align_to(int n, int align) {
    if( align == 0 )
    {
        qCritical() << "align_to align=0";
        return 8;
    }
    return (n + align - 1) / align * align;
}

static int align_down(int n, int align) {
    return align_to(n - align + 1, align);
}

static void enter_scope(void) {
    Scope *sc = new Scope();
    sc->next = Parser::scope;
    Parser::scope = sc;
}

static void leave_scope(void) {
    Parser::scope = Parser::scope->next;
}

// Find a variable by name.
static VarScope *find_var(Token *tok) {
    for (Scope *sc = Parser::scope; sc; sc = sc->next) {
        VarScope *sc2 = sc->vars.value(QByteArray(tok->loc, tok->len));
        if (sc2)
            return sc2;
    }
    return NULL;
}

static Type *find_tag(Token *tok) {
    for (Scope *sc = Parser::scope; sc; sc = sc->next) {
        Type *ty = sc->tags.value(QByteArray(tok->loc, tok->len));
        if (ty)
            return ty;
    }
    return NULL;
}

static VarScope *push_scope(const char *name) {
    VarScope *sc = new VarScope();
    Parser::scope->vars.insert(name, sc);
    return sc;
}

static Initializer *new_initializer(Type *ty, bool is_flexible) {
    Initializer *init = new Initializer();
    init->ty = ty;

    if (ty->kind == Type::ARRAY) {
        if (is_flexible && ty->size < 0) {
            init->is_flexible = true;
            return init;
        }

        init->children.resize(ty->array_len);
        for (int i = 0; i < ty->array_len; i++)
            init->children[i] = new_initializer(ty->base, false);
        return init;
    }

    if (ty->kind == Type::STRUCT || ty->kind == Type::TUNION) {
        // Count the number of struct members.
        int len = 0;
        for (Member *mem = ty->members; mem; mem = mem->next)
            len++;

        init->children.resize(len);

        for (Member *mem = ty->members; mem; mem = mem->next) {
            if (is_flexible && ty->is_flexible && !mem->next) {
                Initializer *child = new Initializer();
                child->ty = mem->ty;
                child->is_flexible = true;
                init->children[mem->idx] = child;
            } else {
                init->children[mem->idx] = new_initializer(mem->ty, false);
            }
        }
        return init;
    }

    return init;
}

static Obj *new_var(const char *name, Type *ty) {
    Obj *var = new Obj();
    var->nameBuf = name;
    var->name = var->nameBuf.constData();
    var->ty = ty;
    var->align = ty->align;
    push_scope(name)->var = var;
    return var;
}

static Obj *new_lvar(const char *name, Type *ty) {
    Obj *var = new_var(name, ty);
    var->is_local = true;
    var->next = locals;
    locals = var;
    return var;
}

static Obj *new_gvar(const char *name, Type *ty) {
    Obj *var = new_var(name, ty);
    var->next = Parser::globalVars;
    var->is_static = true;
    var->is_definition = true;
    Parser::globalVars = var;
    return var;
}

static QByteArray new_unique_name(void) {
    static int id = 0;
    return Tokenizer::format(".L..%d", id++);
}

static Obj *new_anon_gvar(Type *ty) {
    return new_gvar(new_unique_name(), ty);
}

static Obj *new_string_literal(const char *p, Type *ty) {
    Obj *var = new_anon_gvar(ty);
    var->init_data = p;
    return var;
}

static QByteArray get_ident(Token *tok) {
    if (tok->kind != Token::IDENT)
        Tokenizer::error_tok(tok, "expected an identifier");
    return QByteArray(tok->loc, tok->len);
}

static Type *find_typedef(Token *tok) {
    if (tok->kind == Token::IDENT) {
        VarScope *sc = find_var(tok);
        if (sc)
            return sc->type_def;
    }
    return NULL;
}

static void push_tag_scope(Token *tok, Type *ty) {
    Parser::scope->tags.insert(QByteArray(tok->loc, tok->len), ty);
}

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//             | "typedef" | "static" | "extern" | "inline"
//             | "_Thread_local" | "__thread"
//             | "signed" | "unsigned"
//             | struct-decl | union-decl | typedef-name
//             | enum-specifier | typeof-specifier
//             | "const" | "volatile" | "auto" | "register" | "restrict"
//             | "__restrict" | "__restrict__" | "_Noreturn")+
//
// The order of typenames in a type-specifier doesn't matter. For
// example, `int long static` means the same as `static long int`.
// That can also be written as `static long` because you can omit
// `int` if `long` or `short` are specified. However, something like
// `char int` is not a valid type specifier. We have to accept only a
// limited combinations of the typenames.
//
// In this function, we count the number of occurrences of each typename
// while keeping the "current" type object that the typenames up
// until that point represent. When we reach a non-typename token,
// we returns the current type object.
static Type *declspec(Token **rest, Token *tok, VarAttr *attr) {
    // We use a single integer as counters for all typenames.
    // For example, bits 0 and 1 represents how many times we saw the
    // keyword "void" so far. With this, we can use a switch statement
    // as you can see below.
    enum {
        VOID     = 1 << 0,
        BOOL     = 1 << 2,
        CHAR     = 1 << 4,
        SHORT    = 1 << 6,
        INT      = 1 << 8,
        LONG     = 1 << 10,
        FLOAT    = 1 << 12,
        DOUBLE   = 1 << 14,
        OTHER    = 1 << 16,
        SIGNED   = 1 << 17,
        UNSIGNED = 1 << 18,
    };

    Type *ty = Type::_int;
    int counter = 0;
    bool is_atomic = false;

    while (is_typename(tok)) {
        // Handle storage class specifiers.
        if (tok->equal("typedef") || tok->equal("static") || tok->equal("extern") ||
                tok->equal("inline") || tok->equal("_Thread_local") || tok->equal("__thread")) {
            if (!attr)
                Tokenizer::error_tok(tok, "storage class specifier is not allowed in this context");

            if (tok->equal("typedef"))
                attr->is_typedef = true;
            else if (tok->equal("static"))
                attr->is_static = true;
            else if (tok->equal("extern"))
                attr->is_extern = true;
            else if (tok->equal("inline"))
                attr->is_inline = true;
            else
                attr->is_tls = true;

            if (attr->is_typedef &&
                    attr->is_static + attr->is_extern + attr->is_inline + attr->is_tls > 1)
                Tokenizer::error_tok(tok, "typedef may not be used together with static,"
                                          " extern, inline, __thread or _Thread_local");
            tok = tok->next;
            continue;
        }

        // These keywords are recognized but ignored.
        if (tok->consume(&tok, "const") || tok->consume(&tok,  "volatile") ||
                tok->consume(&tok,  "auto") || tok->consume(&tok,  "register") ||
                tok->consume(&tok,  "restrict") || tok->consume(&tok,  "__restrict") ||
                tok->consume(&tok,  "__restrict__") || tok->consume(&tok,  "_Noreturn"))
            continue;

        if (tok->equal("_Atomic")) {
            tok = tok->next;
            if (tok->equal("(")) {
                ty = typename_(&tok, tok->next);
                tok = tok->skip(")");
            }
            is_atomic = true;
            continue;
        }

        if (tok->equal("_Alignas")) {
            if (!attr)
                Tokenizer::error_tok(tok, "_Alignas is not allowed in this context");
            tok = tok->next->skip( "(");

            if (is_typename(tok))
                attr->align = typename_(&tok, tok)->align;
            else
                attr->align = Parser::const_expr(&tok, tok);
            tok = tok->skip(")");
            continue;
        }

        // Handle user-defined types.
        Type *ty2 = find_typedef(tok);
        if (tok->equal("struct") || tok->equal("union") || tok->equal("enum") ||
                tok->equal("typeof") || ty2) {
            if (counter)
                break;

            if (tok->equal("struct")) {
                ty = struct_decl(&tok, tok->next);
            } else if (tok->equal("union")) {
                ty = union_decl(&tok, tok->next);
            } else if (tok->equal("enum")) {
                ty = enum_specifier(&tok, tok->next);
            } else if (tok->equal("typeof")) {
                ty = typeof_specifier(&tok, tok->next);
            } else {
                ty = ty2;
                tok = tok->next;
            }

            counter += OTHER;
            continue;
        }

        // Handle built-in types.
        if (tok->equal("void"))
            counter += VOID;
        else if (tok->equal("_Bool"))
            counter += BOOL;
        else if (tok->equal("char"))
            counter += CHAR;
        else if (tok->equal("short"))
            counter += SHORT;
        else if (tok->equal("int"))
            counter += INT;
        else if (tok->equal("long"))
            counter += LONG;
        else if (tok->equal("float"))
            counter += FLOAT;
        else if (tok->equal("double"))
            counter += DOUBLE;
        else if (tok->equal("signed"))
            counter |= SIGNED;
        else if (tok->equal("unsigned"))
            counter |= UNSIGNED;
        else
            Q_ASSERT(false);

        switch (counter) {
        case VOID:
            ty = Type::_void;
            break;
        case BOOL:
            ty = Type::_bool;
            break;
        case CHAR:
        case SIGNED + CHAR:
            ty = Type::_char;
            break;
        case UNSIGNED + CHAR:
            ty = Type::_uchar;
            break;
        case SHORT:
        case SHORT + INT:
        case SIGNED + SHORT:
        case SIGNED + SHORT + INT:
            ty = Type::_short;
            break;
        case UNSIGNED + SHORT:
        case UNSIGNED + SHORT + INT:
            ty = Type::_ushort;
            break;
        case INT:
        case SIGNED:
        case SIGNED + INT:
            ty = Type::_int;
            break;
        case UNSIGNED:
        case UNSIGNED + INT:
            ty = Type::_uint;
            break;
        case LONG:
        case LONG + INT:
        case LONG + LONG:
        case LONG + LONG + INT:
        case SIGNED + LONG:
        case SIGNED + LONG + INT:
        case SIGNED + LONG + LONG:
        case SIGNED + LONG + LONG + INT:
            ty = Type::_long;
            break;
        case UNSIGNED + LONG:
        case UNSIGNED + LONG + INT:
        case UNSIGNED + LONG + LONG:
        case UNSIGNED + LONG + LONG + INT:
            ty = Type::_ulong;
            break;
        case FLOAT:
            ty = Type::_float;
            break;
        case DOUBLE:
            ty = Type::_double;
            break;
        case LONG + DOUBLE:
            ty = Type::_ldouble;
            break;
        default:
            Tokenizer::error_tok(tok, "invalid type");
        }

        tok = tok->next;
    }

    if (is_atomic) {
        ty = ty->copy_type();
        ty->is_atomic = true;
    }

    *rest = tok;
    return ty;
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
    if (tok->equal("void") && tok->next->equal(")")) {
        *rest = tok->next->next;
        return ty->func_type();
    }

    Type head;
    Type *cur = &head;
    bool is_variadic = false;

    while (!tok->equal(")")) {
        if (cur != &head)
            tok = tok->skip(",");

        if (tok->equal("...")) {
            is_variadic = true;
            tok = tok->next;
            tok->skip(")");
            break;
        }

        Type *ty2 = declspec(&tok, tok, NULL);
        ty2 = declarator(&tok, tok, ty2);

        Token *name = ty2->name;

        if (ty2->kind == Type::ARRAY) {
            // "array of T" is converted to "pointer to T" only in the parameter
            // context. For example, *argv[] is converted to **argv by this.
            ty2 = ty2->base->pointer_to();
            ty2->name = name;
        } else if (ty2->kind == Type::FUNC) {
            // Likewise, a function is converted to a pointer to a function
            // only in the parameter context.
            ty2 = ty2->pointer_to();
            ty2->name = name;
        }

        cur = cur->next = ty2->copy_type();
    }

    if (cur == &head)
        is_variadic = true;

    ty = ty->func_type();
    ty->params = head.next;
    ty->is_variadic = is_variadic;
    *rest = tok->next;
    return ty;
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
static Type *array_dimensions(Token **rest, Token *tok, Type *ty) {
    while (tok->equal("static") || tok->equal("restrict"))
        tok = tok->next;

    if (tok->equal("]")) {
        ty = type_suffix(rest, tok->next, ty);
        return ty->array_of(-1);
    }

    Node *expr = conditional(&tok, tok);
    tok = tok->skip("]");
    ty = type_suffix(rest, tok, ty);

    if (ty->kind == Type::VLA || !is_const_expr(expr))
        return ty->vla_of(expr);
    return ty->array_of(eval(expr));
}

// type-suffix = "(" func-params
//             | "[" array-dimensions
//             | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
    if (tok->equal("("))
        return func_params(rest, tok->next, ty);

    if (tok->equal("["))
        return array_dimensions(rest, tok->next, ty);

    *rest = tok;
    return ty;
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
static Type *pointers(Token **rest, Token *tok, Type *ty) {
    while (tok->consume(&tok,  "*")) {
        ty = ty->pointer_to();
        while (tok->equal("const") || tok->equal("volatile") || tok->equal("restrict") ||
               tok->equal("__restrict") || tok->equal("__restrict__"))
            tok = tok->next;
    }
    *rest = tok;
    return ty;
}

// declarator = pointers ("(" ident ")" | "(" declarator ")" | ident) type-suffix
static Type *declarator(Token **rest, Token *tok, Type *ty) {
    ty = pointers(&tok, tok, ty);

    if (tok->equal("(")) {
        Token *start = tok;
        Type dummy;
        declarator(&tok, start->next, &dummy);
        tok = tok->skip(")");
        ty = type_suffix(rest, tok, ty);
        return declarator(&tok, start->next, ty);
    }

    Token *name = NULL;
    Token *name_pos = tok;

    if (tok->kind == Token::IDENT) {
        name = tok;
        tok = tok->next;
    }

    ty = type_suffix(rest, tok, ty);
    ty->name = name;
    ty->name_pos = name_pos;
    return ty;
}

// abstract-declarator = pointers ("(" abstract-declarator ")")? type-suffix
static Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
    ty = pointers(&tok, tok, ty);

    if (tok->equal("(")) {
        Token *start = tok;
        Type dummy;
        abstract_declarator(&tok, start->next, &dummy);
        tok = tok->skip(")");
        ty = type_suffix(rest, tok, ty);
        return abstract_declarator(&tok, start->next, ty);
    }

    return type_suffix(rest, tok, ty);
}

// type-name = declspec abstract-declarator
static Type *typename_(Token **rest, Token *tok) {
    Type *ty = declspec(&tok, tok, NULL);
    return abstract_declarator(rest, tok, ty);
}

static bool is_end(Token *tok) {
    return tok->equal("}") || (tok->equal(",") && tok->next->equal("}"));
}

static bool consume_end(Token **rest, Token *tok) {
    if (tok->equal("}")) {
        *rest = tok->next;
        return true;
    }

    if (tok->equal(",") && tok->next->equal("}")) {
        *rest = tok->next->next;
        return true;
    }

    return false;
}

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
//
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
static Type *enum_specifier(Token **rest, Token *tok) {
    Type *ty = Type::enum_type();

    // Read a struct tag.
    Token *tag = NULL;
    if (tok->kind == Token::IDENT) {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !tok->equal("{")) {
        Type *ty = find_tag(tag);
        if (!ty)
            Tokenizer::error_tok(tag, "unknown enum type");
        if (ty->kind != Type::ENUM)
            Tokenizer::error_tok(tag, "not an enum tag");
        *rest = tok;
        return ty;
    }

    tok = tok->skip("{");

    // Read an enum-list.
    int i = 0;
    int val = 0;
    while (!consume_end(rest, tok)) {
        if (i++ > 0)
            tok = tok->skip(",");

        const QByteArray name = get_ident(tok);
        tok = tok->next;

        if (tok->equal("="))
            val = Parser::const_expr(&tok, tok->next);

        VarScope *sc = push_scope(name);
        sc->enum_ty = ty;
        sc->enum_val = val++;
    }

    if (tag)
        push_tag_scope(tag, ty);
    return ty;
}

// typeof-specifier = "(" (expr | typename) ")"
static Type *typeof_specifier(Token **rest, Token *tok) {
    tok = tok->skip("(");

    Type *ty;
    if (is_typename(tok)) {
        ty = typename_(&tok, tok);
    } else {
        Node *node = expr(&tok, tok);
        node->add_type();
        ty = node->ty;
    }
    *rest = tok->skip(")");
    return ty;
}

// Generate code for computing a VLA size.
static Node *compute_vla_size(Type *ty, Token *tok) {
    Node *node = Node::new_node(Node::NULL_EXPR, tok);
    if (ty->base)
        node = Node::new_binary(Node::COMMA, node, compute_vla_size(ty->base, tok), tok);

    if (ty->kind != Type::VLA)
        return node;

    Node *base_sz;
    if (ty->base->kind == Type::VLA)
        base_sz = Node::new_var_node(ty->base->vla_size, tok);
    else
        base_sz = Node::new_num(ty->base->size, tok);

    ty->vla_size = new_lvar("", Type::_ulong);
    Node *expr = Node::new_binary(Node::ASSIGN, Node::new_var_node(ty->vla_size, tok),
                            Node::new_binary(Node::MUL, ty->vla_len, base_sz, tok),
                            tok);
    return Node::new_binary(Node::COMMA, node, expr, tok);
}

static Node *new_alloca(Node *sz) {
    Node *node = Node::new_unary(Node::FUNCALL, Node::new_var_node(builtin_alloca, sz->tok), sz->tok);
    node->func_ty = builtin_alloca->ty;
    node->ty = builtin_alloca->ty->return_ty;
    node->args = sz;
    sz->add_type();
    return node;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
    Node head;
    Node *cur = &head;
    int i = 0;

    while (!tok->equal(";")) {
        if (i++ > 0)
            tok = tok->skip(",");

        Type *ty = declarator(&tok, tok, basety);
        if (ty->kind == Type::VOID)
            Tokenizer::error_tok(tok, "variable declared void");
        if (!ty->name)
            Tokenizer::error_tok(ty->name_pos, "variable name omitted");

        if (attr && attr->is_static) {
            // static local variable
            Obj *var = new_anon_gvar(ty);
            push_scope(get_ident(ty->name))->var = var;
            if (tok->equal("="))
                gvar_initializer(&tok, tok->next, var);
            continue;
        }

        // Generate code for computing a VLA size. We need to do this
        // even if ty is not VLA because ty may be a pointer to VLA
        // (e.g. int (*foo)[n][m] where n and m are variables.)
        cur = cur->next = Node::new_unary(Node::EXPR_STMT, compute_vla_size(ty, tok), tok);

        if (ty->kind == Type::VLA) {
            if (tok->equal("="))
                Tokenizer::error_tok(tok, "variable-sized object may not be initialized");

            // Variable length arrays (VLAs) are translated to alloca() calls.
            // For example, `int x[n+2]` is translated to `tmp = n + 2,
            // x = alloca(tmp)`.
            Obj *var = new_lvar(get_ident(ty->name), ty);
            Token *tok = ty->name;
            Node *expr = Node::new_binary(Node::ASSIGN, Node::new_vla_ptr(var, tok),
                                    new_alloca(Node::new_var_node(ty->vla_size, tok)),
                                    tok);

            cur = cur->next = Node::new_unary(Node::EXPR_STMT, expr, tok);
            continue;
        }

        Obj *var = new_lvar(get_ident(ty->name), ty);
        if (attr && attr->align)
            var->align = attr->align;

        if (tok->equal("=")) {
            Node *expr = lvar_initializer(&tok, tok->next, var);
            cur = cur->next = Node::new_unary(Node::EXPR_STMT, expr, tok);
        }

        if (var->ty->size < 0)
            Tokenizer::error_tok(ty->name, "variable has incomplete type");
        if (var->ty->kind == Type::VOID)
            Tokenizer::error_tok(ty->name, "variable declared void");
    }

    Node *node = Node::new_node(Node::BLOCK, tok);
    node->body = head.next;
    *rest = tok->next;
    return node;
}

static Token *skip_excess_element(Token *tok) {
    if (tok->equal("{")) {
        tok = skip_excess_element(tok->next);
        return tok->skip("}");
    }

    assign(&tok, tok);
    return tok;
}

// string-initializer = string-literal
static void string_initializer(Token **rest, Token *tok, Initializer *init) {
    if (init->is_flexible)
        *init = *new_initializer(init->ty->base->array_of(tok->ty->array_len), false);

    int len = qMin(init->ty->array_len, tok->ty->array_len);

    switch (init->ty->base->size) {
    case 1: {
            const char *str = tok->str;
            for (int i = 0; i < len; i++)
                init->children[i]->expr = Node::new_num(str[i], tok);
            break;
        }
    case 2: {
            const QString str = QString::fromUtf8(tok->str);
            const ushort * tmp = str.utf16();
            for (int i = 0; i < len; i++)
                init->children[i]->expr = Node::new_num(tmp[i], tok);
            break;
        }
    case 4: {
            const QString str = QString::fromUtf8(tok->str);
            for (int i = 0; i < len; i++)
                init->children[i]->expr = Node::new_num(str[i].unicode(), tok);
            break;
        }
    default:
        Q_ASSERT(false);
    }

    *rest = tok->next;
}

// array-designator = "[" const-expr "]"
//
// C99 added the designated initializer to the language, which allows
// programmers to move the "cursor" of an initializer to any element.
// The syntax looks like this:
//
//   int x[10] = { 1, 2, [5]=3, 4, 5, 6, 7 };
//
// `[5]` moves the cursor to the 5th element, so the 5th element of x
// is set to 3. Initialization then continues forward in order, so
// 6th, 7th, 8th and 9th elements are initialized with 4, 5, 6 and 7,
// respectively. Unspecified elements (in this case, 3rd and 4th
// elements) are initialized with zero.
//
// Nesting is allowed, so the following initializer is valid:
//
//   int x[5][10] = { [5][8]=1, 2, 3 };
//
// It sets x[5][8], x[5][9] and x[6][0] to 1, 2 and 3, respectively.
//
// Use `.fieldname` to move the cursor for a struct initializer. E.g.
//
//   struct { int a, b, c; } x = { .c=5 };
//
// The above initializer sets x.c to 5.
static void array_designator(Token **rest, Token *tok, Type *ty, int *begin, int *end) {
    *begin = Parser::const_expr(&tok, tok->next);
    if (*begin >= ty->array_len)
        Tokenizer::error_tok(tok, "array designator index exceeds array bounds");

    if (tok->equal("...")) {
        *end = Parser::const_expr(&tok, tok->next);
        if (*end >= ty->array_len)
            Tokenizer::error_tok(tok, "array designator index exceeds array bounds");
        if (*end < *begin)
            Tokenizer::error_tok(tok, "array designator range [%d, %d] is empty", *begin, *end);
    } else {
        *end = *begin;
    }

    *rest = tok->skip("]");
}

// struct-designator = "." ident
static Member *struct_designator(Token **rest, Token *tok, Type *ty) {
    Token *start = tok;
    tok = tok->skip(".");
    if (tok->kind != Token::IDENT)
        Tokenizer::error_tok(tok, "expected a field designator");

    for (Member *mem = ty->members; mem; mem = mem->next) {
        // Anonymous struct member
        if (mem->ty->kind == Type::STRUCT && !mem->name) {
            if (mem->ty->get_struct_member(tok)) {
                *rest = start;
                return mem;
            }
            continue;
        }

        // Regular struct member
        if (mem->name->len == tok->len && !strncmp(mem->name->loc, tok->loc, tok->len)) {
            *rest = tok->next;
            return mem;
        }
    }

    Tokenizer::error_tok(tok, "struct has no such member");
}

// designation = ("[" const-expr "]" | "." ident)* "="? initializer
static void designation(Token **rest, Token *tok, Initializer *init) {
    if (tok->equal("[")) {
        if (init->ty->kind != Type::ARRAY)
            Tokenizer::error_tok(tok, "array index in non-array initializer");

        int begin, end;
        array_designator(&tok, tok, init->ty, &begin, &end);

        Token *tok2;
        for (int i = begin; i <= end; i++)
            designation(&tok2, tok, init->children[i]);
        array_initializer2(rest, tok2, init, begin + 1);
        return;
    }

    if (tok->equal(".") && init->ty->kind == Type::STRUCT) {
        Member *mem = struct_designator(&tok, tok, init->ty);
        designation(&tok, tok, init->children[mem->idx]);
        init->expr = NULL;
        struct_initializer2(rest, tok, init, mem->next);
        return;
    }

    if (tok->equal(".") && init->ty->kind == Type::TUNION) {
        Member *mem = struct_designator(&tok, tok, init->ty);
        init->mem = mem;
        designation(rest, tok, init->children[mem->idx]);
        return;
    }

    if (tok->equal("."))
        Tokenizer::error_tok(tok, "field name not in struct or union initializer");

    if (tok->equal("="))
        tok = tok->next;
    initializer2(rest, tok, init);
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
static int count_array_init_elements(Token *tok, Type *ty) {
    bool first = true;
    Initializer *dummy = new_initializer(ty->base, true);

    int i = 0, max = 0;

    while (!consume_end(&tok, tok)) {
        if (!first)
            tok = tok->skip(",");
        first = false;

        if (tok->equal("[")) {
            i = Parser::const_expr(&tok, tok->next);
            if (tok->equal("..."))
                i = Parser::const_expr(&tok, tok->next);
            tok = tok->skip("]");
            designation(&tok, tok, dummy);
        } else {
            initializer2(&tok, tok, dummy);
        }

        i++;
        max = qMax(max, i);
    }
    return max;
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void array_initializer1(Token **rest, Token *tok, Initializer *init) {
    tok = tok->skip("{");

    if (init->is_flexible) {
        int len = count_array_init_elements(tok, init->ty);
        *init = *new_initializer(init->ty->base->array_of(len), false);
    }

    bool first = true;

    if (init->is_flexible) {
        int len = count_array_init_elements(tok, init->ty);
        *init = *new_initializer(init->ty->base->array_of(len), false);
    }

    for (int i = 0; !consume_end(rest, tok); i++) {
        if (!first)
            tok = tok->skip(",");
        first = false;

        if (tok->equal("[")) {
            int begin, end;
            array_designator(&tok, tok, init->ty, &begin, &end);

            Token *tok2;
            for (int j = begin; j <= end; j++)
                designation(&tok2, tok, init->children[j]);
            tok = tok2;
            i = end;
            continue;
        }

        if (i < init->ty->array_len)
            initializer2(&tok, tok, init->children[i]);
        else
            tok = skip_excess_element(tok);
    }
}

// array-initializer2 = initializer ("," initializer)*
static void array_initializer2(Token **rest, Token *tok, Initializer *init, int i) {
    if (init->is_flexible) {
        int len = count_array_init_elements(tok, init->ty);
        *init = *new_initializer(init->ty->base->array_of(len), false);
    }

    for (; i < init->ty->array_len && !is_end(tok); i++) {
        Token *start = tok;
        if (i > 0)
            tok = tok->skip(",");

        if (tok->equal("[") || tok->equal(".")) {
            *rest = start;
            return;
        }

        initializer2(&tok, tok, init->children[i]);
    }
    *rest = tok;
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void struct_initializer1(Token **rest, Token *tok, Initializer *init) {
    tok = tok->skip("{");

    Member *mem = init->ty->members;
    bool first = true;

    while (!consume_end(rest, tok)) {
        if (!first)
            tok = tok->skip(",");
        first = false;

        if (tok->equal(".")) {
            mem = struct_designator(&tok, tok, init->ty);
            designation(&tok, tok, init->children[mem->idx]);
            mem = mem->next;
            continue;
        }

        if (mem) {
            initializer2(&tok, tok, init->children[mem->idx]);
            mem = mem->next;
        } else {
            tok = skip_excess_element(tok);
        }
    }
}

// struct-initializer2 = initializer ("," initializer)*
static void struct_initializer2(Token **rest, Token *tok, Initializer *init, Member *mem) {
    bool first = true;

    for (; mem && !is_end(tok); mem = mem->next) {
        Token *start = tok;

        if (!first)
            tok = tok->skip(",");
        first = false;

        if (tok->equal("[") || tok->equal(".")) {
            *rest = start;
            return;
        }

        initializer2(&tok, tok, init->children[mem->idx]);
    }
    *rest = tok;
}

static void union_initializer(Token **rest, Token *tok, Initializer *init) {
    // Unlike structs, union initializers take only one initializer,
    // and that initializes the first union member by default.
    // You can initialize other member using a designated initializer.
    if (tok->equal("{") && tok->next->equal(".")) {
        Member *mem = struct_designator(&tok, tok->next, init->ty);
        init->mem = mem;
        designation(&tok, tok, init->children[mem->idx]);
        *rest = tok->skip("}");
        return;
    }

    init->mem = init->ty->members;

    if (tok->equal("{")) {
        initializer2(&tok, tok->next, init->children[0]);
        tok->consume(&tok,  ",");
        *rest = tok->skip("}");
    } else {
        initializer2(rest, tok, init->children[0]);
    }
}

// initializer = string-initializer | array-initializer
//             | struct-initializer | union-initializer
//             | assign
static void initializer2(Token **rest, Token *tok, Initializer *init) {
    if (init->ty->kind == Type::ARRAY && tok->kind == Token::STR) {
        string_initializer(rest, tok, init);
        return;
    }

    if (init->ty->kind == Type::ARRAY) {
        if (tok->equal("{"))
            array_initializer1(rest, tok, init);
        else
            array_initializer2(rest, tok, init, 0);
        return;
    }

    if (init->ty->kind == Type::STRUCT) {
        if (tok->equal("{")) {
            struct_initializer1(rest, tok, init);
            return;
        }

        // A struct can be initialized with another struct. E.g.
        // `struct T x = y;` where y is a variable of type `struct T`.
        // Handle that case first.
        Node *expr = assign(rest, tok);
        expr->add_type();
        if (expr->ty->kind == Type::STRUCT) {
            init->expr = expr;
            return;
        }

        struct_initializer2(rest, tok, init, init->ty->members);
        return;
    }

    if (init->ty->kind == Type::TUNION) {
        union_initializer(rest, tok, init);
        return;
    }

    if (tok->equal("{")) {
        // An initializer for a scalar variable can be surrounded by
        // braces. E.g. `int x = {3};`. Handle that case.
        initializer2(&tok, tok->next, init);
        *rest = tok->skip("}");
        return;
    }

    init->expr = assign(rest, tok);
}

static Type *copy_struct_type(Type *ty) {
    ty = ty->copy_type();

    Member head;
    Member *cur = &head;
    for (Member *mem = ty->members; mem; mem = mem->next) {
        Member *m = new Member();
        *m = *mem;
        cur = cur->next = m;
    }

    ty->members = head.next;
    return ty;
}

static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty) {
    Initializer *init = new_initializer(ty, true);
    initializer2(rest, tok, init);

    if ((ty->kind == Type::STRUCT || ty->kind == Type::TUNION) && ty->is_flexible) {
        ty = copy_struct_type(ty);

        Member *mem = ty->members;
        while (mem->next)
            mem = mem->next;
        mem->ty = init->children[mem->idx]->ty;
        ty->size += mem->ty->size;

        *new_ty = ty;
        return init;
    }

    *new_ty = init->ty;
    return init;
}

static Node *init_desg_expr(InitDesg *desg, Token *tok) {
    if (desg->var)
        return Node::new_var_node(desg->var, tok);

    if (desg->member) {
        Node *node = Node::new_unary(Node::MEMBER, init_desg_expr(desg->next, tok), tok);
        node->member = desg->member;
        return node;
    }

    Node *lhs = init_desg_expr(desg->next, tok);
    Node *rhs = Node::new_num(desg->idx, tok);
    return Node::new_unary(Node::DEREF, new_add(lhs, rhs, tok), tok);
}

static Node *create_lvar_init(Initializer *init, Type *ty, InitDesg *desg, Token *tok) {
    if (ty->kind == Type::ARRAY) {
        Node *node = Node::new_node(Node::NULL_EXPR, tok);
        for (int i = 0; i < ty->array_len; i++) {
            InitDesg desg2(desg, i);
            Node *rhs = create_lvar_init(init->children[i], ty->base, &desg2, tok);
            node = Node::new_binary(Node::COMMA, node, rhs, tok);
        }
        return node;
    }

    if (ty->kind == Type::STRUCT && !init->expr) {
        Node *node = Node::new_node(Node::NULL_EXPR, tok);

        for (Member *mem = ty->members; mem; mem = mem->next) {
            InitDesg desg2(desg, 0, mem);
            Node *rhs = create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
            node = Node::new_binary(Node::COMMA, node, rhs, tok);
        }
        return node;
    }

    if (ty->kind == Type::TUNION) {
        Member *mem = init->mem ? init->mem : ty->members;
        InitDesg desg2(desg, 0, mem);
        return create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok);
    }

    if (!init->expr)
        return Node::new_node(Node::NULL_EXPR, tok);

    Node *lhs = init_desg_expr(desg, tok);
    return Node::new_binary(Node::ASSIGN, lhs, init->expr, tok);
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//   x[0][0] = 6;
//   x[0][1] = 7;
//   x[1][0] = 8;
//   x[1][1] = 9;
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var) {
    Initializer *init = initializer(rest, tok, var->ty, &var->ty);
    InitDesg desg(NULL, 0, NULL, var);

    // If a partial initializer list is given, the standard requires
    // that unspecified elements are set to 0. Here, we simply
    // zero-initialize the entire memory region of a variable before
    // initializing it with user-supplied values.
    Node *lhs = Node::new_node(Node::MEMZERO, tok);
    lhs->var = var;

    Node *rhs = create_lvar_init(init, var->ty, &desg, tok);
    return Node::new_binary(Node::COMMA, lhs, rhs, tok);
}

static quint64 read_buf(char *buf, int sz) {
    if (sz == 1)
        return *buf;
    if (sz == 2)
        return *(quint16 *)buf;
    if (sz == 4)
        return *(quint32 *)buf;
    if (sz == 8)
        return *(quint64 *)buf;
    Q_ASSERT(false);
}

static void write_buf(char *buf, quint64 val, int sz) {
    if (sz == 1)
        *buf = val;
    else if (sz == 2)
        *(quint16 *)buf = val;
    else if (sz == 4)
        *(quint32 *)buf = val;
    else if (sz == 8)
        *(quint64 *)buf = val;
    else
        Q_ASSERT(false);
}

static Relocation *
write_gvar_data(Relocation *cur, Initializer *init, Type *ty, char *buf, int offset) {
    if (ty->kind == Type::ARRAY) {
        int sz = ty->base->size;
        for (int i = 0; i < ty->array_len; i++)
            cur = write_gvar_data(cur, init->children[i], ty->base, buf, offset + sz * i);
        return cur;
    }

    if (ty->kind == Type::STRUCT) {
        for (Member *mem = ty->members; mem; mem = mem->next) {
            if (mem->is_bitfield) {
                Node *expr = init->children[mem->idx]->expr;
                if (!expr)
                    break;

                char *loc = buf + offset + mem->offset;
                quint64 oldval = read_buf(loc, mem->ty->size);
                quint64 newval = eval(expr);
                quint64 mask = (1L << mem->bit_width) - 1;
                quint64 combined = oldval | ((newval & mask) << mem->bit_offset);
                write_buf(loc, combined, mem->ty->size);
            } else {
                cur = write_gvar_data(cur, init->children[mem->idx], mem->ty, buf,
                        offset + mem->offset);
            }
        }
        return cur;
    }

    if (ty->kind == Type::TUNION) {
        if (!init->mem)
            return cur;
        return write_gvar_data(cur, init->children[init->mem->idx],
                init->mem->ty, buf, offset);
    }

    if (!init->expr)
        return cur;

    if (ty->kind == Type::FLOAT) {
        *(float *)(buf + offset) = eval_double(init->expr);
        return cur;
    }

    if (ty->kind == Type::DOUBLE) {
        *(double *)(buf + offset) = eval_double(init->expr);
        return cur;
    }

    const char **label = NULL;
    quint64 val = eval2(init->expr, &label);

    if (!label) {
        write_buf(buf + offset, val, ty->size);
        return cur;
    }

    Relocation *rel = new Relocation();
    rel->offset = offset;
    rel->label = label;
    rel->addend = val;
    cur->next = rel;
    return cur->next;
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
static void gvar_initializer(Token **rest, Token *tok, Obj *var) {
    Initializer *init = initializer(rest, tok, var->ty, &var->ty);

    Relocation head;
    QByteArray buf;
    buf.resize(var->ty->size);
    write_gvar_data(&head, init, var->ty, buf.data(), 0);
    var->init_data = buf;
    var->rel = head.next;
}

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
    static QSet<QByteArray> map;

    if (map.isEmpty()) {
        static char *kw[] = {
            "void", "_Bool", "char", "short", "int", "long", "struct", "union",
            "typedef", "enum", "static", "extern", "_Alignas", "signed", "unsigned",
            "const", "volatile", "auto", "register", "restrict", "__restrict",
            "__restrict__", "_Noreturn", "float", "double", "typeof", "inline",
            "_Thread_local", "__thread", "_Atomic",
        };

        for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
            map.insert(kw[i]);
    }

    return map.contains(QByteArray(tok->loc, tok->len)) || find_typedef(tok);
}

// asm-stmt = "asm" ("volatile" | "inline")* "(" string-literal ")"
static Node *asm_stmt(Token **rest, Token *tok) {
    Node *node = Node::new_node(Node::ASM, tok);
    tok = tok->next;

    while (tok->equal("volatile") || tok->equal("inline"))
        tok = tok->next;

    tok = tok->skip("(");
    if (tok->kind != Token::STR || tok->ty->base->kind != Type::CHAR)
        Tokenizer::error_tok(tok, "expected string literal");
    node->asm_str = tok->str.constData();
    *rest = tok->next->skip( ")");
    return node;
}

// stmt = "return" expr? ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ("..." const-expr)? ":" stmt
//      | "default" ":" stmt
//      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "do" stmt "while" "(" expr ")" ";"
//      | "asm" asm-stmt
//      | "goto" (ident | "*" expr) ";"
//      | "break" ";"
//      | "continue" ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr-stmt
static Node *stmt(Token **rest, Token *tok) {
    if (tok->equal("return")) {
        Node *node = Node::new_node(Node::RETURN, tok);
        if (tok->next->consume(rest, ";"))
            return node;

        Node *exp = expr(&tok, tok->next);
        *rest = tok->skip(";");

        exp->add_type();
        Type *ty = current_fn->ty->return_ty;
        if (ty->kind != Type::STRUCT && ty->kind != Type::TUNION)
            exp = exp->new_cast(current_fn->ty->return_ty);

        node->lhs = exp;
        return node;
    }

    if (tok->equal("if")) {
        Node *node = Node::new_node(Node::IF, tok);
        tok = tok->next->skip( "(");
        node->cond = expr(&tok, tok);
        tok = tok->skip(")");
        node->then = stmt(&tok, tok);
        if (tok->equal("else"))
            node->els = stmt(&tok, tok->next);
        *rest = tok;
        return node;
    }

    if (tok->equal("switch")) {
        Node *node = Node::new_node(Node::SWITCH, tok);
        tok = tok->next->skip( "(");
        node->cond = expr(&tok, tok);
        tok = tok->skip(")");

        Node *sw = current_switch;
        current_switch = node;

        const char *brk = brk_label;
        brk_label = node->brk_label = new_unique_name();

        node->then = stmt(rest, tok);

        current_switch = sw;
        brk_label = brk;
        return node;
    }

    if (tok->equal("case")) {
        if (!current_switch)
            Tokenizer::error_tok(tok, "stray case");

        Node *node = Node::new_node(Node::CASE, tok);
        int begin = Parser::const_expr(&tok, tok->next);
        int end;

        if (tok->equal("...")) {
            // [GNU] Case ranges, e.g. "case 1 ... 5:"
            end = Parser::const_expr(&tok, tok->next);
            if (end < begin)
                Tokenizer::error_tok(tok, "empty case range specified");
        } else {
            end = begin;
        }

        tok = tok->skip(":");
        node->label = new_unique_name();
        node->lhs = stmt(rest, tok);
        node->begin = begin;
        node->end = end;
        node->case_next = current_switch->case_next;
        current_switch->case_next = node;
        return node;
    }

    if (tok->equal("default")) {
        if (!current_switch)
            Tokenizer::error_tok(tok, "stray default");

        Node *node = Node::new_node(Node::CASE, tok);
        tok = tok->next->skip( ":");
        node->label = new_unique_name();
        node->lhs = stmt(rest, tok);
        current_switch->default_case = node;
        return node;
    }

    if (tok->equal("for")) {
        Node *node = Node::new_node(Node::FOR, tok);
        tok = tok->next->skip( "(");

        enter_scope();

        const char *brk = brk_label;
        const char *cont = cont_label;
        brk_label = node->brk_label = new_unique_name();
        cont_label = node->cont_label = new_unique_name();

        if (is_typename(tok)) {
            Type *basety = declspec(&tok, tok, NULL);
            node->init = declaration(&tok, tok, basety, NULL);
        } else {
            node->init = expr_stmt(&tok, tok);
        }

        if (!tok->equal(";"))
            node->cond = expr(&tok, tok);
        tok = tok->skip(";");

        if (!tok->equal(")"))
            node->inc = expr(&tok, tok);
        tok = tok->skip(")");

        node->then = stmt(rest, tok);

        leave_scope();
        brk_label = brk;
        cont_label = cont;
        return node;
    }

    if (tok->equal("while")) {
        Node *node = Node::new_node(Node::FOR, tok);
        tok = tok->next->skip( "(");
        node->cond = expr(&tok, tok);
        tok = tok->skip(")");

        const char *brk = brk_label;
        const char *cont = cont_label;
        brk_label = node->brk_label = new_unique_name();
        cont_label = node->cont_label = new_unique_name();

        node->then = stmt(rest, tok);

        brk_label = brk;
        cont_label = cont;
        return node;
    }

    if (tok->equal("do")) {
        Node *node = Node::new_node(Node::DO, tok);

        const char *brk = brk_label;
        const char *cont = cont_label;
        brk_label = node->brk_label = new_unique_name();
        cont_label = node->cont_label = new_unique_name();

        node->then = stmt(&tok, tok->next);

        brk_label = brk;
        cont_label = cont;

        tok = tok->skip("while");
        tok = tok->skip("(");
        node->cond = expr(&tok, tok);
        tok = tok->skip(")");
        *rest = tok->skip(";");
        return node;
    }

    if (tok->equal("asm"))
        return asm_stmt(rest, tok);

    if (tok->equal("goto")) {
        if (tok->next->equal("*")) {
            // [GNU] `goto *ptr` jumps to the address specified by `ptr`.
            Node *node = Node::new_node(Node::GOTO_EXPR, tok);
            node->lhs = expr(&tok, tok->next->next);
            *rest = tok->skip(";");
            return node;
        }

        Node *node = Node::new_node(Node::GOTO, tok);
        node->label = get_ident(tok->next);
        node->goto_next = gotos;
        gotos = node;
        *rest = tok->next->next->skip(";");
        return node;
    }

    if (tok->equal("break")) {
        if (!brk_label)
            Tokenizer::error_tok(tok, "stray break");
        Node *node = Node::new_node(Node::GOTO, tok);
        node->unique_label = brk_label;
        *rest = tok->next->skip( ";");
        return node;
    }

    if (tok->equal("continue")) {
        if (!cont_label)
            Tokenizer::error_tok(tok, "stray continue");
        Node *node = Node::new_node(Node::GOTO, tok);
        node->unique_label = cont_label;
        *rest = tok->next->skip( ";");
        return node;
    }

    if (tok->kind == Token::IDENT && tok->next->equal(":")) {
        Node *node = Node::new_node(Node::LABEL, tok);
        node->label = strndup(tok->loc, tok->len);
        node->unique_label = new_unique_name();
        node->lhs = stmt(rest, tok->next->next);
        node->goto_next = labels;
        labels = node;
        return node;
    }

    if (tok->equal("{"))
        return compound_stmt(rest, tok->next);

    return expr_stmt(rest, tok);
}

// compound-stmt = (typedef | declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
    Node *node = Node::new_node(Node::BLOCK, tok);
    Node head;
    Node *cur = &head;

    enter_scope();

    while (!tok->equal("}")) {
        if (is_typename(tok) && !tok->next->equal(":")) {
            VarAttr attr;
            Type *basety = declspec(&tok, tok, &attr);

            if (attr.is_typedef) {
                tok = parse_typedef(tok, basety);
                continue;
            }

            if (is_function(tok)) {
                tok = function(tok, basety, &attr);
                continue;
            }

            if (attr.is_extern) {
                tok = global_variable(tok, basety, &attr);
                continue;
            }

            cur = cur->next = declaration(&tok, tok, basety, &attr);
        } else {
            cur = cur->next = stmt(&tok, tok);
        }
        cur->add_type();
    }

    leave_scope();

    node->body = head.next;
    *rest = tok->next;
    return node;
}

// expr-stmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok) {
    if (tok->equal(";")) {
        *rest = tok->next;
        return Node::new_node(Node::BLOCK, tok);
    }

    Node *node = Node::new_node(Node::EXPR_STMT, tok);
    node->lhs = expr(&tok, tok);
    *rest = tok->skip(";");
    return node;
}

// expr = assign ("," expr)?
static Node *expr(Token **rest, Token *tok) {
    Node *node = assign(&tok, tok);

    if (tok->equal(","))
        return Node::new_binary(Node::COMMA, node, expr(rest, tok->next), tok);

    *rest = tok;
    return node;
}

static qint64 eval(Node *node) {
    return eval2(node, NULL);
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a postiive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
static qint64 eval2(Node *node, const char ***label) {
    node->add_type();

    if (node->ty->is_flonum())
        return eval_double(node);

    switch (node->kind) {
    case Node::ADD:
        return eval2(node->lhs, label) + eval(node->rhs);
    case Node::SUB:
        return eval2(node->lhs, label) - eval(node->rhs);
    case Node::MUL:
        return eval(node->lhs) * eval(node->rhs);
    case Node::DIV:
        if (node->ty->is_unsigned)
            return (quint64)eval(node->lhs) / eval(node->rhs);
        return eval(node->lhs) / eval(node->rhs);
    case Node::NEG:
        return -eval(node->lhs);
    case Node::MOD:
        if (node->ty->is_unsigned)
            return (quint64)eval(node->lhs) % eval(node->rhs);
        return eval(node->lhs) % eval(node->rhs);
    case Node::BITAND:
        return eval(node->lhs) & eval(node->rhs);
    case Node::BITOR:
        return eval(node->lhs) | eval(node->rhs);
    case Node::BITXOR:
        return eval(node->lhs) ^ eval(node->rhs);
    case Node::SHL:
        return eval(node->lhs) << eval(node->rhs);
    case Node::SHR:
        if (node->ty->is_unsigned && node->ty->size == 8)
            return (quint64)eval(node->lhs) >> eval(node->rhs);
        return eval(node->lhs) >> eval(node->rhs);
    case Node::EQ:
        return eval(node->lhs) == eval(node->rhs);
    case Node::NE:
        return eval(node->lhs) != eval(node->rhs);
    case Node::LT:
        if (node->lhs->ty->is_unsigned)
            return (quint64)eval(node->lhs) < eval(node->rhs);
        return eval(node->lhs) < eval(node->rhs);
    case Node::LE:
        if (node->lhs->ty->is_unsigned)
            return (quint64)eval(node->lhs) <= eval(node->rhs);
        return eval(node->lhs) <= eval(node->rhs);
    case Node::COND:
        return eval(node->cond) ? eval2(node->then, label) : eval2(node->els, label);
    case Node::COMMA:
        return eval2(node->rhs, label);
    case Node::NOT:
        return !eval(node->lhs);
    case Node::BITNOT:
        return ~eval(node->lhs);
    case Node::LOGAND:
        return eval(node->lhs) && eval(node->rhs);
    case Node::LOGOR:
        return eval(node->lhs) || eval(node->rhs);
    case Node::CAST: {
            qint64 val = eval2(node->lhs, label);
            if (node->ty->is_integer()) {
                switch (node->ty->size) {
                case 1: return node->ty->is_unsigned ? (quint8)val : (qint8)val;
                case 2: return node->ty->is_unsigned ? (quint16)val : (int16_t)val;
                case 4: return node->ty->is_unsigned ? (quint32)val : (int32_t)val;
                }
            }
            return val;
        }
    case Node::ADDR:
        return eval_rval(node->lhs, label);
    case Node::LABEL_VAL:
        *label = &node->unique_label;
        return 0;
    case Node::MEMBER:
        if (!label)
            Tokenizer::error_tok(node->tok, "not a compile-time constant");
        if (node->ty->kind != Type::ARRAY)
            Tokenizer::error_tok(node->tok, "invalid initializer");
        return eval_rval(node->lhs, label) + node->member->offset;
    case Node::VAR:
        if (!label)
            Tokenizer::error_tok(node->tok, "not a compile-time constant");
        if (node->var->ty->kind != Type::ARRAY && node->var->ty->kind != Type::FUNC)
            Tokenizer::error_tok(node->tok, "invalid initializer");
        *label = &node->var->name;
        return 0;
    case Node::NUM:
        return node->val;
    }

    Tokenizer::error_tok(node->tok, "not a compile-time constant");
}

static qint64 eval_rval(Node *node, const char ***label) {
    switch (node->kind) {
    case Node::VAR:
        if (node->var->is_local)
            Tokenizer::error_tok(node->tok, "not a compile-time constant");
        *label = &node->var->name;
        return 0;
    case Node::DEREF:
        return eval2(node->lhs, label);
    case Node::MEMBER:
        return eval_rval(node->lhs, label) + node->member->offset;
    }

    Tokenizer::error_tok(node->tok, "invalid initializer");
}

static bool is_const_expr(Node *node) {
    node->add_type();

    switch (node->kind) {
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
    case Node::BITAND:
    case Node::BITOR:
    case Node::BITXOR:
    case Node::SHL:
    case Node::SHR:
    case Node::EQ:
    case Node::NE:
    case Node::LT:
    case Node::LE:
    case Node::LOGAND:
    case Node::LOGOR:
        return is_const_expr(node->lhs) && is_const_expr(node->rhs);
    case Node::COND:
        if (!is_const_expr(node->cond))
            return false;
        return is_const_expr(eval(node->cond) ? node->then : node->els);
    case Node::COMMA:
        return is_const_expr(node->rhs);
    case Node::NEG:
    case Node::NOT:
    case Node::BITNOT:
    case Node::CAST:
        return is_const_expr(node->lhs);
    case Node::NUM:
        return true;
    }

    return false;
}

qint64 Parser::const_expr(Token **rest, Token *tok) {
    Node *node = conditional(rest, tok);
    return eval(node);
}

static double eval_double(Node *node) {
    node->add_type();

    if (node->ty->is_integer()) {
        if (node->ty->is_unsigned)
            return (unsigned long)eval(node);
        return eval(node);
    }

    switch (node->kind) {
    case Node::ADD:
        return eval_double(node->lhs) + eval_double(node->rhs);
    case Node::SUB:
        return eval_double(node->lhs) - eval_double(node->rhs);
    case Node::MUL:
        return eval_double(node->lhs) * eval_double(node->rhs);
    case Node::DIV:
        return eval_double(node->lhs) / eval_double(node->rhs);
    case Node::NEG:
        return -eval_double(node->lhs);
    case Node::COND:
        return eval_double(node->cond) ? eval_double(node->then) : eval_double(node->els);
    case Node::COMMA:
        return eval_double(node->rhs);
    case Node::CAST:
        if (node->lhs->ty->is_flonum())
            return eval_double(node->lhs);
        return eval(node->lhs);
    case Node::NUM:
        return node->fval;
    }

    Tokenizer::error_tok(node->tok, "not a compile-time constant");
}

// Convert op= operators to expressions containing an assignment.
//
// In general, `A op= C` is converted to ``tmp = &A, *tmp = *tmp op B`.
// However, if a given expression is of form `A.x op= C`, the input is
// converted to `tmp = &A, (*tmp).x = (*tmp).x op C` to handle assignments
// to bitfields.
static Node *to_assign(Node *binary) {
    binary->lhs->add_type();
    binary->rhs->add_type();
    Token *tok = binary->tok;

    // Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`.
    if (binary->lhs->kind == Node::MEMBER) {
        Obj *var = new_lvar("", binary->lhs->lhs->ty->pointer_to());

        Node *expr1 = Node::new_binary(Node::ASSIGN, Node::new_var_node(var, tok),
                                 Node::new_unary(Node::ADDR, binary->lhs->lhs, tok), tok);

        Node *expr2 = Node::new_unary(Node::MEMBER,
                                Node::new_unary(Node::DEREF, Node::new_var_node(var, tok), tok),
                                tok);
        expr2->member = binary->lhs->member;

        Node *expr3 = Node::new_unary(Node::MEMBER,
                                Node::new_unary(Node::DEREF, Node::new_var_node(var, tok), tok),
                                tok);
        expr3->member = binary->lhs->member;

        Node *expr4 = Node::new_binary(Node::ASSIGN, expr2,
                                 Node::new_binary(binary->kind, expr3, binary->rhs, tok),
                                 tok);

        return Node::new_binary(Node::COMMA, expr1, expr4, tok);
    }

    // If A is an atomic type, Convert `A op= B` to
    //
    // ({
    //   T1 *addr = &A; T2 val = (B); T1 old = *addr; T1 new;
    //   do {
    //    new = old op val;
    //   } while (!atomic_compare_exchange_strong(addr, &old, new));
    //   new;
    // })
    if (binary->lhs->ty->is_atomic) {
        Node head;
        Node *cur = &head;

        Obj *addr = new_lvar("", binary->lhs->ty->pointer_to());
        Obj *val = new_lvar("", binary->rhs->ty);
        Obj *old = new_lvar("", binary->lhs->ty);
        Obj *new_ = new_lvar("", binary->lhs->ty);

        cur = cur->next =
                Node::new_unary(Node::EXPR_STMT,
                          Node::new_binary(Node::ASSIGN, Node::new_var_node(addr, tok),
                                     Node::new_unary(Node::ADDR, binary->lhs, tok), tok),
                          tok);

        cur = cur->next =
                Node::new_unary(Node::EXPR_STMT,
                          Node::new_binary(Node::ASSIGN, Node::new_var_node(val, tok), binary->rhs, tok),
                          tok);

        cur = cur->next =
                Node::new_unary(Node::EXPR_STMT,
                          Node::new_binary(Node::ASSIGN, Node::new_var_node(old, tok),
                                     Node::new_unary(Node::DEREF, Node::new_var_node(addr, tok), tok), tok),
                          tok);

        Node *loop = Node::new_node(Node::DO, tok);
        loop->brk_label = new_unique_name();
        loop->cont_label = new_unique_name();

        Node *body = Node::new_binary(Node::ASSIGN,
                                Node::new_var_node(new_, tok),
                                Node::new_binary(binary->kind, Node::new_var_node(old, tok),
                                           Node::new_var_node(val, tok), tok),
                                tok);

        loop->then = Node::new_node(Node::BLOCK, tok);
        loop->then->body = Node::new_unary(Node::EXPR_STMT, body, tok);

        Node *cas = Node::new_node(Node::CAS, tok);
        cas->cas_addr = Node::new_var_node(addr, tok);
        cas->cas_old = Node::new_unary(Node::ADDR, Node::new_var_node(old, tok), tok);
        cas->cas_new = Node::new_var_node(new_, tok);
        loop->cond = Node::new_unary(Node::NOT, cas, tok);

        cur = cur->next = loop;
        cur = cur->next = Node::new_unary(Node::EXPR_STMT, Node::new_var_node(new_, tok), tok);

        Node *node = Node::new_node(Node::STMT_EXPR, tok);
        node->body = head.next;
        return node;
    }

    // Convert `A op= B` to ``tmp = &A, *tmp = *tmp op B`.
    Obj *var = new_lvar("", binary->lhs->ty->pointer_to());

    Node *expr1 = Node::new_binary(Node::ASSIGN, Node::new_var_node(var, tok),
                             Node::new_unary(Node::ADDR, binary->lhs, tok), tok);

    Node *expr2 =
            Node::new_binary(Node::ASSIGN,
                       Node::new_unary(Node::DEREF, Node::new_var_node(var, tok), tok),
                       Node::new_binary(binary->kind,
                                  Node::new_unary(Node::DEREF, Node::new_var_node(var, tok), tok),
                                  binary->rhs,
                                  tok),
                       tok);

    return Node::new_binary(Node::COMMA, expr1, expr2, tok);
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
static Node *assign(Token **rest, Token *tok) {
    Node *node = conditional(&tok, tok);

    if (tok->equal("="))
        return Node::new_binary(Node::ASSIGN, node, assign(rest, tok->next), tok);

    if (tok->equal("+="))
        return to_assign(new_add(node, assign(rest, tok->next), tok));

    if (tok->equal("-="))
        return to_assign(new_sub(node, assign(rest, tok->next), tok));

    if (tok->equal("*="))
        return to_assign(Node::new_binary(Node::MUL, node, assign(rest, tok->next), tok));

    if (tok->equal("/="))
        return to_assign(Node::new_binary(Node::DIV, node, assign(rest, tok->next), tok));

    if (tok->equal("%="))
        return to_assign(Node::new_binary(Node::MOD, node, assign(rest, tok->next), tok));

    if (tok->equal("&="))
        return to_assign(Node::new_binary(Node::BITAND, node, assign(rest, tok->next), tok));

    if (tok->equal("|="))
        return to_assign(Node::new_binary(Node::BITOR, node, assign(rest, tok->next), tok));

    if (tok->equal("^="))
        return to_assign(Node::new_binary(Node::BITXOR, node, assign(rest, tok->next), tok));

    if (tok->equal("<<="))
        return to_assign(Node::new_binary(Node::SHL, node, assign(rest, tok->next), tok));

    if (tok->equal(">>="))
        return to_assign(Node::new_binary(Node::SHR, node, assign(rest, tok->next), tok));

    *rest = tok;
    return node;
}

// conditional = logor ("?" expr? ":" conditional)?
static Node *conditional(Token **rest, Token *tok) {
    Node *cond = logor(&tok, tok);

    if (!tok->equal("?")) {
        *rest = tok;
        return cond;
    }

    if (tok->next->equal(":")) {
        // [GNU] Compile `a ?: b` as `tmp = a, tmp ? tmp : b`.
        cond->add_type();
        Obj *var = new_lvar("", cond->ty);
        Node *lhs = Node::new_binary(Node::ASSIGN, Node::new_var_node(var, tok), cond, tok);
        Node *rhs = Node::new_node(Node::COND, tok);
        rhs->cond = Node::new_var_node(var, tok);
        rhs->then = Node::new_var_node(var, tok);
        rhs->els = conditional(rest, tok->next->next);
        return Node::new_binary(Node::COMMA, lhs, rhs, tok);
    }

    Node *node = Node::new_node(Node::COND, tok);
    node->cond = cond;
    node->then = expr(&tok, tok->next);
    tok = tok->skip(":");
    node->els = conditional(rest, tok);
    return node;
}

// logor = logand ("||" logand)*
static Node *logor(Token **rest, Token *tok) {
    Node *node = logand(&tok, tok);
    while (tok->equal("||")) {
        Token *start = tok;
        node = Node::new_binary(Node::LOGOR, node, logand(&tok, tok->next), start);
    }
    *rest = tok;
    return node;
}

// logand = bitor ("&&" bitor)*
static Node *logand(Token **rest, Token *tok) {
    Node *node = bitor_(&tok, tok);
    while (tok->equal("&&")) {
        Token *start = tok;
        node = Node::new_binary(Node::LOGAND, node, bitor_(&tok, tok->next), start);
    }
    *rest = tok;
    return node;
}

// bitor = bitxor ("|" bitxor)*
static Node *bitor_(Token **rest, Token *tok) {
    Node *node = bitxor_(&tok, tok);
    while (tok->equal("|")) {
        Token *start = tok;
        node = Node::new_binary(Node::BITOR, node, bitxor_(&tok, tok->next), start);
    }
    *rest = tok;
    return node;
}

// bitxor = bitand ("^" bitand)*
static Node *bitxor_(Token **rest, Token *tok) {
    Node *node = bitand_(&tok, tok);
    while (tok->equal("^")) {
        Token *start = tok;
        node = Node::new_binary(Node::BITXOR, node, bitand_(&tok, tok->next), start);
    }
    *rest = tok;
    return node;
}

// bitand = equality ("&" equality)*
static Node *bitand_(Token **rest, Token *tok) {
    Node *node = equality(&tok, tok);
    while (tok->equal("&")) {
        Token *start = tok;
        node = Node::new_binary(Node::BITAND, node, equality(&tok, tok->next), start);
    }
    *rest = tok;
    return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
    Node *node = relational(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (tok->equal("==")) {
            node = Node::new_binary(Node::EQ, node, relational(&tok, tok->next), start);
            continue;
        }

        if (tok->equal("!=")) {
            node = Node::new_binary(Node::NE, node, relational(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
static Node *relational(Token **rest, Token *tok) {
    Node *node = shift(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (tok->equal("<")) {
            node = Node::new_binary(Node::LT, node, shift(&tok, tok->next), start);
            continue;
        }

        if (tok->equal("<=")) {
            node = Node::new_binary(Node::LE, node, shift(&tok, tok->next), start);
            continue;
        }

        if (tok->equal(">")) {
            node = Node::new_binary(Node::LT, shift(&tok, tok->next), node, start);
            continue;
        }

        if (tok->equal(">=")) {
            node = Node::new_binary(Node::LE, shift(&tok, tok->next), node, start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// shift = add ("<<" add | ">>" add)*
static Node *shift(Token **rest, Token *tok) {
    Node *node = add(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (tok->equal("<<")) {
            node = Node::new_binary(Node::SHL, node, add(&tok, tok->next), start);
            continue;
        }

        if (tok->equal(">>")) {
            node = Node::new_binary(Node::SHR, node, add(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
    lhs->add_type();
    rhs->add_type();

    // num + num
    if (lhs->ty->is_numeric() && rhs->ty->is_numeric())
        return Node::new_binary(Node::ADD, lhs, rhs, tok);

    if (lhs->ty->base && rhs->ty->base)
        Tokenizer::error_tok(tok, "invalid operands");

    // Canonicalize `num + ptr` to `ptr + num`.
    if (!lhs->ty->base && rhs->ty->base) {
        Node *tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }

    // VLA + num
    if (lhs->ty->base->kind == Type::VLA) {
        rhs = Node::new_binary(Node::MUL, rhs, Node::new_var_node(lhs->ty->base->vla_size, tok), tok);
        return Node::new_binary(Node::ADD, lhs, rhs, tok);
    }

    // ptr + num
    rhs = Node::new_binary(Node::MUL, rhs, Node::new_long(lhs->ty->base->size, tok), tok);
    return Node::new_binary(Node::ADD, lhs, rhs, tok);
}

// Like `+`, `-` is overloaded for the pointer type.
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
    lhs->add_type();
    rhs->add_type();

    // num - num
    if (lhs->ty->is_numeric() && rhs->ty->is_numeric())
        return Node::new_binary(Node::SUB, lhs, rhs, tok);

    // VLA + num
    if (lhs->ty->base->kind == Type::VLA) {
        rhs = Node::new_binary(Node::MUL, rhs, Node::new_var_node(lhs->ty->base->vla_size, tok), tok);
        rhs->add_type();
        Node *node = Node::new_binary(Node::SUB, lhs, rhs, tok);
        node->ty = lhs->ty;
        return node;
    }

    // ptr - num
    if (lhs->ty->base && rhs->ty->is_integer()) {
        rhs = Node::new_binary(Node::MUL, rhs, Node::new_long(lhs->ty->base->size, tok), tok);
        rhs->add_type();
        Node *node = Node::new_binary(Node::SUB, lhs, rhs, tok);
        node->ty = lhs->ty;
        return node;
    }

    // ptr - ptr, which returns how many elements are between the two.
    if (lhs->ty->base && rhs->ty->base) {
        Node *node = Node::new_binary(Node::SUB, lhs, rhs, tok);
        node->ty = Type::_long;
        return Node::new_binary(Node::DIV, node, Node::new_num(lhs->ty->base->size, tok), tok);
    }

    Tokenizer::error_tok(tok, "invalid operands");
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
    Node *node = mul(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (tok->equal("+")) {
            node = new_add(node, mul(&tok, tok->next), start);
            continue;
        }

        if (tok->equal("-")) {
            node = new_sub(node, mul(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// mul = cast ("*" cast | "/" cast | "%" cast)*
static Node *mul(Token **rest, Token *tok) {
    Node *node = cast(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (tok->equal("*")) {
            node = Node::new_binary(Node::MUL, node, cast(&tok, tok->next), start);
            continue;
        }

        if (tok->equal("/")) {
            node = Node::new_binary(Node::DIV, node, cast(&tok, tok->next), start);
            continue;
        }

        if (tok->equal("%")) {
            node = Node::new_binary(Node::MOD, node, cast(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// cast = "(" type-name ")" cast | unary
static Node *cast(Token **rest, Token *tok) {
    if (tok->equal("(") && is_typename(tok->next)) {
        Token *start = tok;
        Type *ty = typename_(&tok, tok->next);
        tok = tok->skip(")");

        // compound literal
        if (tok->equal("{"))
            return unary(rest, start);

        // type cast
        Node *node = cast(rest, tok)->new_cast(ty);
        node->tok = start;
        return node;
    }

    return unary(rest, tok);
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | "&&" ident
//       | postfix
static Node *unary(Token **rest, Token *tok) {
    if (tok->equal("+"))
        return cast(rest, tok->next);

    if (tok->equal("-"))
        return Node::new_unary(Node::NEG, cast(rest, tok->next), tok);

    if (tok->equal("&")) {
        Node *lhs = cast(rest, tok->next);
        lhs->add_type();
        if (lhs->kind == Node::MEMBER && lhs->member->is_bitfield)
            Tokenizer::error_tok(tok, "cannot take address of bitfield");
        return Node::new_unary(Node::ADDR, lhs, tok);
    }

    if (tok->equal("*")) {
        // [https://www.sigbus.info/n1570#6.5.3.2p4] This is an oddity
        // in the C spec, but dereferencing a function shouldn't do
        // anything. If foo is a function, `*foo`, `**foo` or `*****foo`
        // are all equivalent to just `foo`.
        Node *node = cast(rest, tok->next);
        node->add_type();
        if (node->ty->kind == Type::FUNC)
            return node;
        return Node::new_unary(Node::DEREF, node, tok);
    }

    if (tok->equal("!"))
        return Node::new_unary(Node::NOT, cast(rest, tok->next), tok);

    if (tok->equal("~"))
        return Node::new_unary(Node::BITNOT, cast(rest, tok->next), tok);

    // Read ++i as i+=1
    if (tok->equal("++"))
        return to_assign(new_add(unary(rest, tok->next), Node::new_num(1, tok), tok));

    // Read --i as i-=1
    if (tok->equal("--"))
        return to_assign(new_sub(unary(rest, tok->next), Node::new_num(1, tok), tok));

    // [GNU] labels-as-values
    if (tok->equal("&&")) {
        Node *node = Node::new_node(Node::LABEL_VAL, tok);
        node->label = get_ident(tok->next);
        node->goto_next = gotos;
        gotos = node;
        *rest = tok->next->next;
        return node;
    }

    return postfix(rest, tok);
}

// struct-members = (declspec declarator (","  declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
    Member head;
    Member *cur = &head;
    int idx = 0;

    while (!tok->equal("}")) {
        VarAttr attr;
        Type *basety = declspec(&tok, tok, &attr);
        bool first = true;

        // Anonymous struct member
        if ((basety->kind == Type::STRUCT || basety->kind == Type::TUNION) &&
                tok->consume(&tok,  ";")) {
            Member *mem = new Member();
            mem->ty = basety;
            mem->idx = idx++;
            mem->align = attr.align ? attr.align : mem->ty->align;
            cur = cur->next = mem;
            continue;
        }

        // Regular struct members
        while (!tok->consume(&tok,  ";")) {
            if (!first)
                tok = tok->skip(",");
            first = false;

            Member *mem = new Member();
            mem->ty = declarator(&tok, tok, basety);
            mem->name = mem->ty->name;
            mem->idx = idx++;
            mem->align = attr.align ? attr.align : mem->ty->align;

            if (tok->consume(&tok,  ":")) {
                mem->is_bitfield = true;
                mem->bit_width = Parser::const_expr(&tok, tok);
            }

            cur = cur->next = mem;
        }
    }

    // If the last element is an array of incomplete type, it's
    // called a "flexible array member". It should behave as if
    // if were a zero-sized array.
    if (cur != &head && cur->ty->kind == Type::ARRAY && cur->ty->array_len < 0) {
        cur->ty = cur->ty->base->array_of(0);
        ty->is_flexible = true;
    }

    *rest = tok->next;
    ty->members = head.next;
}

// attribute = ("__attribute__" "(" "(" "packed" ")" ")")*
static Token *attribute_list(Token *tok, Type *ty) {
    while (tok->consume(&tok,  "__attribute__")) {
        tok = tok->skip("(");
        tok = tok->skip("(");

        bool first = true;

        while (!tok->consume(&tok,  ")")) {
            if (!first)
                tok = tok->skip(",");
            first = false;

            if (tok->consume(&tok,  "packed")) {
                ty->is_packed = true;
                continue;
            }

            if (tok->consume(&tok,  "aligned")) {
                tok = tok->skip("(");
                ty->align = Parser::const_expr(&tok, tok);
                tok = tok->skip(")");
                continue;
            }

            Tokenizer::error_tok(tok, "unknown attribute");
        }

        tok = tok->skip(")");
    }

    return tok;
}

// struct-union-decl = attribute? ident? ("{" struct-members)?
static Type *struct_union_decl(Token **rest, Token *tok) {
    Type *ty = Type::struct_type();
    tok = attribute_list(tok, ty);

    // Read a tag.
    Token *tag = NULL;
    if (tok->kind == Token::IDENT) {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !tok->equal("{")) {
        *rest = tok;

        Type *ty2 = find_tag(tag);
        if (ty2)
            return ty2;

        ty->size = -1;
        push_tag_scope(tag, ty);
        return ty;
    }

    tok = tok->skip("{");

    // Construct a struct object.
    struct_members(&tok, tok, ty);
    *rest = attribute_list(tok, ty);

    if (tag) {
        // If this is a redefinition, overwrite a previous type.
        // Otherwise, register the struct type.
        Type *ty2 = Parser::scope->tags.value(QByteArray(tag->loc, tag->len));
        if (ty2) {
            *ty2 = *ty;
            return ty2;
        }

        push_tag_scope(tag, ty);
    }

    return ty;
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
    Type *ty = struct_union_decl(rest, tok);
    ty->kind = Type::STRUCT;

    if (ty->size < 0)
        return ty;

    // Assign offsets within the struct to members.
    int bits = 0;

    for (Member *m = ty->members; m; m = m->next) {
        if (m->is_bitfield && m->bit_width == 0) {
            // Zero-width anonymous bitfield has a special meaning.
            // It affects only alignment.
            bits = align_to(bits, m->ty->size * 8);
        } else if (m->is_bitfield) {
            int sz = m->ty->size;
            if (bits / (sz * 8) != (bits + m->bit_width - 1) / (sz * 8))
                bits = align_to(bits, sz * 8);

            m->offset = align_down(bits / 8, sz);
            m->bit_offset = bits % (sz * 8);
            bits += m->bit_width;
        } else {
            if (!ty->is_packed)
                bits = align_to(bits, m->align * 8);
            m->offset = bits / 8;
            bits += m->ty->size * 8;
        }

        if (!ty->is_packed && ty->align < m->align)
            ty->align = m->align;
    }

    ty->size = align_to(bits, ty->align * 8) / 8;
    return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
    Type *ty = struct_union_decl(rest, tok);
    ty->kind = Type::TUNION;

    if (ty->size < 0)
        return ty;

    // If union, we don't have to assign offsets because they
    // are already initialized to zero. We need to compute the
    // alignment and the size though.
    for (Member *mem = ty->members; mem; mem = mem->next) {
        if (ty->align < mem->align)
            ty->align = mem->align;
        if (ty->size < mem->ty->size)
            ty->size = mem->ty->size;
    }
    ty->size = align_to(ty->size, ty->align);
    return ty;
}

// Convert A++ to `(typeof A)((A += 1) - 1)`
static Node *new_inc_dec(Node *node, Token *tok, int addend) {
    node->add_type();
    return new_add(to_assign(new_add(node, Node::new_num(addend, tok), tok)),
                   Node::new_num(-addend, tok), tok)->new_cast(node->ty);
}

// postfix = "(" type-name ")" "{" initializer-list "}"
//         = ident "(" func-args ")" postfix-tail*
//         | primary postfix-tail*
//
// postfix-tail = "[" expr "]"
//              | "(" func-args ")"
//              | "." ident
//              | "->" ident
//              | "++"
//              | "--"
static Node *postfix(Token **rest, Token *tok) {
    if (tok->equal("(") && is_typename(tok->next)) {
        // Compound literal
        Token *start = tok;
        Type *ty = typename_(&tok, tok->next);
        tok = tok->skip(")");

        if (Parser::scope->next == NULL) {
            Obj *var = new_anon_gvar(ty);
            gvar_initializer(rest, tok, var);
            return Node::new_var_node(var, start);
        }

        Obj *var = new_lvar("", ty);
        Node *lhs = lvar_initializer(rest, tok, var);
        Node *rhs = Node::new_var_node(var, tok);
        return Node::new_binary(Node::COMMA, lhs, rhs, start);
    }

    Node *node = primary(&tok, tok);

    for (;;) {
        if (tok->equal("(")) {
            node = funcall(&tok, tok->next, node);
            continue;
        }

        if (tok->equal("[")) {
            // x[y] is short for *(x+y)
            Token *start = tok;
            Node *idx = expr(&tok, tok->next);
            tok = tok->skip("]");
            node = Node::new_unary(Node::DEREF, new_add(node, idx, start), start);
            continue;
        }

        if (tok->equal(".")) {
            node = node->struct_ref(tok->next);
            tok = tok->next->next;
            continue;
        }

        if (tok->equal("->")) {
            // x->y is short for (*x).y
            node = Node::new_unary(Node::DEREF, node, tok);
            node = node->struct_ref(tok->next);
            tok = tok->next->next;
            continue;
        }

        if (tok->equal("++")) {
            node = new_inc_dec(node, tok, 1);
            tok = tok->next;
            continue;
        }

        if (tok->equal("--")) {
            node = new_inc_dec(node, tok, -1);
            tok = tok->next;
            continue;
        }

        *rest = tok;
        return node;
    }
}

// funcall = (assign ("," assign)*)? ")"
static Node *funcall(Token **rest, Token *tok, Node *fn) {
    fn->add_type();

    if (fn->ty->kind != Type::FUNC &&
            (fn->ty->kind != Type::PTR || fn->ty->base->kind != Type::FUNC))
        Tokenizer::error_tok(fn->tok, "not a function");

    Type *ty = (fn->ty->kind == Type::FUNC) ? fn->ty : fn->ty->base;
    Type *param_ty = ty->params;

    Node head;
    Node *cur = &head;

    while (!tok->equal(")")) {
        if (cur != &head)
            tok = tok->skip(",");

        Node *arg = assign(&tok, tok);
        arg->add_type();

        if (!param_ty && !ty->is_variadic)
            Tokenizer::error_tok(tok, "too many arguments");

        if (param_ty) {
            if (param_ty->kind != Type::STRUCT && param_ty->kind != Type::TUNION)
                arg = arg->new_cast(param_ty);
            param_ty = param_ty->next;
        } else if (arg->ty->kind == Type::FLOAT) {
            // If parameter type is omitted (e.g. in "..."), float
            // arguments are promoted to double.
            arg = arg->new_cast(Type::_double);
        }

        cur = cur->next = arg;
    }

    if (param_ty)
        Tokenizer::error_tok(tok, "too few arguments");

    *rest = tok->skip(")");

    Node *node = Node::new_unary(Node::FUNCALL, fn, tok);
    node->func_ty = ty;
    node->ty = ty->return_ty;
    node->args = head.next;

    // If a function returns a struct, it is caller's responsibility
    // to allocate a space for the return value.
    if (node->ty->kind == Type::STRUCT || node->ty->kind == Type::TUNION)
        node->ret_buffer = new_lvar("", node->ty);
    return node;
}

// generic-selection = "(" assign "," generic-assoc ("," generic-assoc)* ")"
//
// generic-assoc = type-name ":" assign
//               | "default" ":" assign
static Node *generic_selection(Token **rest, Token *tok) {
    Token *start = tok;
    tok = tok->skip("(");

    Node *ctrl = assign(&tok, tok);
    ctrl->add_type();

    Type *t1 = ctrl->ty;
    if (t1->kind == Type::FUNC)
        t1 = t1->pointer_to();
    else if (t1->kind == Type::ARRAY)
        t1 = t1->base->pointer_to();

    Node *ret = NULL;

    while (!tok->consume(rest, ")")) {
        tok = tok->skip(",");

        if (tok->equal("default")) {
            tok = tok->next->skip( ":");
            Node *node = assign(&tok, tok);
            if (!ret)
                ret = node;
            continue;
        }

        Type *t2 = typename_(&tok, tok);
        tok = tok->skip(":");
        Node *node = assign(&tok, tok);
        if (t1->is_compatible(t2))
            ret = node;
    }

    if (!ret)
        Tokenizer::error_tok(start, "controlling expression type not compatible with"
                                    " any generic association type");
    return ret;
}

// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | "_Alignof" "(" type-name ")"
//         | "_Alignof" unary
//         | "_Generic" generic-selection
//         | "__builtin_types_compatible_p" "(" type-name, type-name, ")"
//         | "__builtin_reg_class" "(" type-name ")"
//         | ident
//         | str
//         | num
static Node *primary(Token **rest, Token *tok) {
    Token *start = tok;

    if (tok->equal("(") && tok->next->equal("{")) {
        // This is a GNU statement expresssion.
        Node *node = Node::new_node(Node::STMT_EXPR, tok);
        node->body = compound_stmt(&tok, tok->next->next)->body;
        *rest = tok->skip(")");
        return node;
    }

    if (tok->equal("(")) {
        Node *node = expr(&tok, tok->next);
        *rest = tok->skip(")");
        return node;
    }

    if (tok->equal("sizeof") && tok->next->equal("(") && is_typename(tok->next->next)) {
        Type *ty = typename_(&tok, tok->next->next);
        *rest = tok->skip(")");

        if (ty->kind == Type::VLA) {
            if (ty->vla_size)
                return Node::new_var_node(ty->vla_size, tok);

            Node *lhs = compute_vla_size(ty, tok);
            Node *rhs = Node::new_var_node(ty->vla_size, tok);
            return Node::new_binary(Node::COMMA, lhs, rhs, tok);
        }

        return Node::new_ulong(ty->size, start);
    }

    if (tok->equal("sizeof")) {
        Node *node = unary(rest, tok->next);
        node->add_type();
        if (node->ty->kind == Type::VLA)
            return Node::new_var_node(node->ty->vla_size, tok);
        return Node::new_ulong(node->ty->size, tok);
    }

    if (tok->equal("_Alignof") && tok->next->equal("(") && is_typename(tok->next->next)) {
        Type *ty = typename_(&tok, tok->next->next);
        *rest = tok->skip(")");
        return Node::new_ulong(ty->align, tok);
    }

    if (tok->equal("_Alignof")) {
        Node *node = unary(rest, tok->next);
        node->add_type();
        return Node::new_ulong(node->ty->align, tok);
    }

    if (tok->equal("_Generic"))
        return generic_selection(rest, tok->next);

    if (tok->equal("__builtin_types_compatible_p")) {
        tok = tok->next->skip( "(");
        Type *t1 = typename_(&tok, tok);
        tok = tok->skip(",");
        Type *t2 = typename_(&tok, tok);
        *rest = tok->skip(")");
        return Node::new_num(t1->is_compatible(t2), start);
    }

    if (tok->equal("__builtin_reg_class")) {
        tok = tok->next->skip( "(");
        Type *ty = typename_(&tok, tok);
        *rest = tok->skip(")");

        if (ty->is_integer() || ty->kind == Type::PTR)
            return Node::new_num(0, start);
        if (ty->is_flonum())
            return Node::new_num(1, start);
        return Node::new_num(2, start);
    }

    if (tok->equal("__builtin_compare_and_swap")) {
        Node *node = Node::new_node(Node::CAS, tok);
        tok = tok->next->skip( "(");
        node->cas_addr = assign(&tok, tok);
        tok = tok->skip(",");
        node->cas_old = assign(&tok, tok);
        tok = tok->skip(",");
        node->cas_new = assign(&tok, tok);
        *rest = tok->skip(")");
        return node;
    }

    if (tok->equal("__builtin_atomic_exchange")) {
        Node *node = Node::new_node(Node::EXCH, tok);
        tok = tok->next->skip( "(");
        node->lhs = assign(&tok, tok);
        tok = tok->skip(",");
        node->rhs = assign(&tok, tok);
        *rest = tok->skip(")");
        return node;
    }

    if (tok->kind == Token::IDENT) {
        // Variable or enum constant
        VarScope *sc = find_var(tok);
        *rest = tok->next;

        // For "static inline" function
        if (sc && sc->var && sc->var->is_function) {
            if (current_fn)
                current_fn->refs.append(sc->var->name);
            else
                sc->var->is_root = true;
        }

        if (sc) {
            if (sc->var)
                return Node::new_var_node(sc->var, tok);
            if (sc->enum_ty)
                return Node::new_num(sc->enum_val, tok);
        }

        if (tok->next->equal("("))
            Tokenizer::error_tok(tok, "implicit declaration of a function");
        Tokenizer::error_tok(tok, "undefined variable");
    }

    if (tok->kind == Token::STR) {
        Obj *var = new_string_literal(tok->str, tok->ty);
        *rest = tok->next;
        return Node::new_var_node(var, tok);
    }

    if (tok->kind == Token::NUM) {
        Node *node;
        if (tok->ty->is_flonum()) {
            node = Node::new_node(Node::NUM, tok);
            node->fval = tok->fval;
        } else {
            node = Node::new_num(tok->val, tok);
        }

        node->ty = tok->ty;
        *rest = tok->next;
        return node;
    }

    Tokenizer::error_tok(tok, "expected an expression");
}

static Token *parse_typedef(Token *tok, Type *basety) {
    bool first = true;

    while (!tok->consume(&tok,  ";")) {
        if (!first)
            tok = tok->skip(",");
        first = false;

        Type *ty = declarator(&tok, tok, basety);
        if (!ty->name)
            Tokenizer::error_tok(ty->name_pos, "typedef name omitted");
        push_scope(get_ident(ty->name))->type_def = ty;
    }
    return tok;
}

static void create_param_lvars(Type *param) {
    if (param) {
        create_param_lvars(param->next);
        if (!param->name)
            Tokenizer::error_tok(param->name_pos, "parameter name omitted");
        new_lvar(get_ident(param->name), param);
    }
}

// This function matches gotos or labels-as-values with labels.
//
// We cannot resolve gotos as we parse a function because gotos
// can refer a label that appears later in the function.
// So, we need to do this after we parse the entire function.
static void resolve_goto_labels(void) {
    for (Node *x = gotos; x; x = x->goto_next) {
        for (Node *y = labels; y; y = y->goto_next) {
            if (!strcmp(x->label, y->label)) {
                x->unique_label = y->unique_label;
                break;
            }
        }

        if (x->unique_label == NULL)
            Tokenizer::error_tok(x->tok->next, "use of undeclared label");
    }

    gotos = labels = NULL;
}

static Obj *find_func(const char *name) {
    Scope *sc = Parser::scope;
    while (sc->next)
        sc = sc->next;

    VarScope *sc2 = sc->vars.value(name);
    if (sc2 && sc2->var && sc2->var->is_function)
        return sc2->var;
    return NULL;
}

static void mark_live(Obj *var) {
    if (!var->is_function || var->is_live)
        return;
    var->is_live = true;

    for (int i = 0; i < var->refs.size(); i++) {
        Obj *fn = find_func(var->refs[i]);
        if (fn)
            mark_live(fn);
    }
}

static Token *function(Token *tok, Type *basety, VarAttr *attr) {
    Type *ty = declarator(&tok, tok, basety);
    if (!ty->name)
        Tokenizer::error_tok(ty->name_pos, "function name omitted");
    QByteArray name_str = get_ident(ty->name);

    Obj *fn = find_func(name_str);
    if (fn) {
        // Redeclaration
        if (!fn->is_function)
            Tokenizer::error_tok(tok, "redeclared as a different kind of symbol");
        if (fn->is_definition && tok->equal("{"))
            Tokenizer::error_tok(tok, "redefinition of %s", name_str.constData());
        if (!fn->is_static && attr->is_static)
            Tokenizer::error_tok(tok, "static declaration follows a non-static declaration");
        fn->is_definition = fn->is_definition || tok->equal("{");
    } else {
        fn = new_gvar(name_str, ty);
        fn->is_function = true;
        fn->is_definition = tok->equal("{");
        fn->is_static = attr->is_static || (attr->is_inline && !attr->is_extern);
        fn->is_inline = attr->is_inline;
        Parser::funcs.append(fn);
    }

    fn->is_root = !(fn->is_static && fn->is_inline);

    if (tok->consume(&tok,  ";"))
        return tok;

    current_fn = fn;
    locals = NULL;
    enter_scope();
    create_param_lvars(ty->params);

    // A buffer for a struct/union return value is passed
    // as the hidden first parameter.
    Type *rty = ty->return_ty;
    if ((rty->kind == Type::STRUCT || rty->kind == Type::TUNION) && rty->size > 16)
        new_lvar("", rty->pointer_to());

    fn->params = locals;

    if (ty->is_variadic)
        fn->va_area = new_lvar("__va_area__", Type::_char->array_of(136));
    fn->alloca_bottom = new_lvar("__alloca_size__", Type::_char->pointer_to());

    tok = tok->skip("{");

    // [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
    // automatically defined as a local variable containing the
    // current function name.
    push_scope("__func__")->var =
            new_string_literal(fn->name, Type::_char->array_of(strlen(fn->name) + 1));

    // [GNU] __FUNCTION__ is yet another name of __func__.
    push_scope("__FUNCTION__")->var =
            new_string_literal(fn->name, Type::_char->array_of(strlen(fn->name) + 1));

    fn->body = compound_stmt(&tok, tok);
    fn->locals = locals;
    leave_scope();
    resolve_goto_labels();
    return tok;
}

static Token *global_variable(Token *tok, Type *basety, VarAttr *attr) {
    bool first = true;

    while (!tok->consume(&tok,  ";")) {
        if (!first)
            tok = tok->skip(",");
        first = false;

        Type *ty = declarator(&tok, tok, basety);
        if (!ty->name)
            Tokenizer::error_tok(ty->name_pos, "variable name omitted");

        Obj *var = new_gvar(get_ident(ty->name), ty);
        var->is_definition = !attr->is_extern;
        var->is_static = attr->is_static;
        var->is_tls = attr->is_tls;
        if (attr->align)
            var->align = attr->align;

        if (tok->equal("="))
            gvar_initializer(&tok, tok->next, var);
        else if (!attr->is_extern && !attr->is_tls)
            var->is_tentative = true;
    }
    return tok;
}

// Lookahead tokens and returns true if a given token is a start
// of a function definition or declaration.
static bool is_function(Token *tok) {
    if (tok->equal(";"))
        return false;

    Type dummy;
    Type *ty = declarator(&tok, tok, &dummy);
    return ty->kind == Type::FUNC;
}

// Remove redundant tentative definitions.
static void scan_globals(void) {
    Obj head;
    Obj *cur = &head;

    for (Obj *var = Parser::globalVars; var; var = var->next) {
        if (!var->is_tentative) {
            cur = cur->next = var;
            continue;
        }

        // Find another definition of the same identifier.
        Obj *var2 = Parser::globalVars;
        for (; var2; var2 = var2->next)
            if (var != var2 && var2->is_definition && !strcmp(var->name, var2->name))
                break;

        // If there's another definition, the tentative definition
        // is redundant
        if (!var2)
            cur = cur->next = var;
    }

    cur->next = NULL;
    Parser::globalVars = head.next;
}

static void declare_builtin_functions(void) {
    Type *ty = Type::_void->pointer_to()->func_type();
    ty->params = Type::_int->copy_type();
    builtin_alloca = new_gvar("alloca", ty);
    builtin_alloca->is_definition = false;
}

// program = (typedef | function-definition | global-variable)*
Obj *Parser::parse(Token *tok) {
    declare_builtin_functions();
    Parser::globalVars = NULL;

    while (tok->kind != Token::_EOF) {
        VarAttr attr;
        Type *basety = declspec(&tok, tok, &attr);

        // Typedef
        if (attr.is_typedef) {
            tok = parse_typedef(tok, basety);
            continue;
        }

        // Function
        if (is_function(tok)) {
            tok = function(tok, basety, &attr);
            continue;
        }

        // Global variable
        tok = global_variable(tok, basety, &attr);
    }

    for (Obj *var = Parser::globalVars; var; var = var->next)
        if (var->is_root)
            mark_live(var);

    // Remove redundant tentative definitions.
    scan_globals();
    return Parser::globalVars;
}

Node *Parser::expr_checked(Token *tok)
{
    try
    {

        Node* res = expr(&tok,tok);
        return res;
    }catch(...)
    {
        return 0;
    }
}

