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

#include "Ast.h"
#include "Type.h"
#include "Tokenizer.h"
#include <string.h>
using namespace C;


Node::Node():kind(NULL_EXPR),next(0),ty(0),tok(0),lhs(0),rhs(0),cond(0),then(0),els(0),init(0),inc(0),
    brk_label(0),cont_label(0),body(0),member(0),func_ty(0),args(0),pass_by_stack(0),ret_buffer(0),
    label(0),unique_label(0),goto_next(0),case_next(0),default_case(0),begin(0),end(0),cas_addr(0),
    cas_old(0),cas_new(0),atomic_addr(0),atomic_expr(0),var(0),val(0),fval(0)
{
}

Node*Node::new_cast(Type* ty)
{
    add_type();

    Node *node = new Node();
    node->kind = Node::CAST;
    node->tok = tok;
    node->lhs = this;
    node->ty = ty->copy_type();
    return node;
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
static void usual_arith_conv(Node **lhs, Node **rhs) {
    Type *ty = (*lhs)->ty->get_common_type((*rhs)->ty);
    *lhs = (*lhs)->new_cast(ty);
    *rhs = (*rhs)->new_cast(ty);
}

void Node::add_type()
{
    Node* node = this;
    if (!node || node->ty)
        return;

    node->lhs->add_type();
    node->rhs->add_type();
    node->cond->add_type();
    node->then->add_type();
    node->els->add_type();
    node->init->add_type();
    node->inc->add_type();

    for (Node *n = node->body; n; n = n->next)
        n->add_type();
    for (Node *n = node->args; n; n = n->next)
        n->add_type();

    Q_ASSERT( Type::_int );
    switch (node->kind) {
    case Node::NUM:
        node->ty = Type::_int;
        return;
    case Node::ADD:
    case Node::SUB:
    case Node::MUL:
    case Node::DIV:
    case Node::MOD:
    case Node::BITAND:
    case Node::BITOR:
    case Node::BITXOR:
        usual_arith_conv(&node->lhs, &node->rhs);
        node->ty = node->lhs->ty;
        return;
    case Node::NEG: {
            Type *ty = Type::_int->get_common_type(node->lhs->ty);
            node->lhs = node->lhs->new_cast(ty);
            node->ty = ty;
            return;
        }
    case Node::ASSIGN:
        if (node->lhs->ty->kind == Type::ARRAY)
            Tokenizer::error_tok(node->lhs->tok, "not an lvalue");
        if (node->lhs->ty->kind != Type::STRUCT)
            node->rhs = node->rhs->new_cast(node->lhs->ty);
        node->ty = node->lhs->ty;
        return;
    case Node::EQ:
    case Node::NE:
    case Node::LT:
    case Node::LE:
        usual_arith_conv(&node->lhs, &node->rhs);
        node->ty = Type::_int;
        return;
    case Node::FUNCALL:
        node->ty = node->func_ty->return_ty;
        return;
    case Node::NOT:
    case Node::LOGOR:
    case Node::LOGAND:
        node->ty = Type::_int;
        return;
    case Node::BITNOT:
    case Node::SHL:
    case Node::SHR:
        node->ty = node->lhs->ty;
        return;
    case Node::VAR:
    case Node::VLA_PTR:
        node->ty = node->var->ty;
        return;
    case Node::COND:
        if (node->then->ty->kind == Type::VOID || node->els->ty->kind == Type::VOID) {
            node->ty = Type::_void;
        } else {
            usual_arith_conv(&node->then, &node->els);
            node->ty = node->then->ty;
        }
        return;
    case Node::COMMA:
        node->ty = node->rhs->ty;
        return;
    case Node::MEMBER:
        node->ty = node->member->ty;
        return;
    case Node::ADDR: {
            Type *ty = node->lhs->ty;
            if (ty->kind == Type::ARRAY)
                node->ty = ty->base->pointer_to();
            else
                node->ty = ty->pointer_to();
            return;
        }
    case Node::DEREF:
        if (!node->lhs->ty->base)
            Tokenizer::error_tok(node->tok, "invalid pointer dereference");
        if (node->lhs->ty->base->kind == Type::VOID)
            Tokenizer::error_tok(node->tok, "dereferencing a void pointer");

        node->ty = node->lhs->ty->base;
        return;
    case Node::STMT_EXPR:
        if (node->body) {
            Node *stmt = node->body;
            while (stmt->next)
                stmt = stmt->next;
            if (stmt->kind == Node::EXPR_STMT) {
                node->ty = stmt->lhs->ty;
                return;
            }
        }
        Tokenizer::error_tok(node->tok, "statement expression returning void is not supported");
        return;
    case Node::LABEL_VAL:
        node->ty = Type::_void->pointer_to();
        return;
    case Node::CAS:
        node->cas_addr->add_type();
        node->cas_old->add_type();
        node->cas_new->add_type();
        node->ty = Type::_bool;

        if (node->cas_addr->ty->kind != Type::PTR)
            Tokenizer::error_tok(node->cas_addr->tok, "pointer expected");
        if (node->cas_old->ty->kind != Type::PTR)
            Tokenizer::error_tok(node->cas_old->tok, "pointer expected");
        return;
    case Node::EXCH:
        if (node->lhs->ty->kind != Type::PTR)
            Tokenizer::error_tok(node->cas_addr->tok, "pointer expected");
        node->ty = node->lhs->ty->base;
        return;
    }
}

// Create a node representing a struct member access, such as foo.bar
// where foo is a struct and bar is a member name.
//
// C has a feature called "anonymous struct" which allows a struct to
// have another unnamed struct as a member like this:
//
//   struct { struct { int a; }; int b; } x;
//
// The members of an anonymous struct belong to the outer struct's
// member namespace. Therefore, in the above example, you can access
// member "a" of the anonymous struct as "x.a".
//
// This function takes care of anonymous structs.
Node* Node::struct_ref(Token* tok) const
{
    Node* node = const_cast<Node*>(this);
    node->add_type();
    if (node->ty->kind != Type::STRUCT && node->ty->kind != Type::TUNION)
        Tokenizer::error_tok(node->tok, "not a struct nor a union");

    Type *ty = node->ty;

    for (;;) {
        Member *mem = ty->get_struct_member(tok);
        if (!mem)
            Tokenizer::error_tok(tok, "no such member");
        node = new_unary(Node::MEMBER, node, tok);
        node->member = mem;
        if (mem->name)
            break;
        ty = mem->ty;
    }
    return node;
}

Node *Node::new_node(Node::Kind kind, Token *tok) {
    Node *node = new Node();
    node->kind = kind;
    node->tok = tok;
    return node;
}

Node *Node::new_binary(Node::Kind kind, Node *lhs, Node *rhs, Token *tok) {
    Node *node = new_node(kind, tok);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

Node *Node::new_unary(Node::Kind kind, Node *expr, Token *tok) {
    Node *node = new_node(kind, tok);
    node->lhs = expr;
    return node;
}

Node *Node::new_num(qint64 val, Token *tok) {
    Node *node = new_node(Node::NUM, tok);
    node->val = val;
    return node;
}

Node *Node::new_long(qint64 val, Token *tok) {
    Node *node = new_node(Node::NUM, tok);
    node->val = val;
    node->ty = Type::_long;
    return node;
}

Node *Node::new_ulong(long val, Token *tok) {
    Node *node = new_node(Node::NUM, tok);
    node->val = val;
    node->ty = Type::_ulong;
    return node;
}

Node *Node::new_var_node(Obj *var, Token *tok) {
    Node *node = new_node(Node::VAR, tok);
    node->var = var;
    return node;
}

Node *Node::new_vla_ptr(Obj *var, Token *tok) {
    Node *node = new_node(Node::VLA_PTR, tok);
    node->var = var;
    return node;
}

Obj::Obj():next(0),ty(0),tok(0),is_local(false),align(0),offset(0),is_function(false),is_definition(false),
    is_static(false),is_tentative(false),is_tls(false),rel(0),is_inline(false),params(0),body(0),
    locals(0),va_area(0),alloca_bottom(0),stack_size(0),is_live(false),is_root(false),name(0),is_const(false)
{
}

