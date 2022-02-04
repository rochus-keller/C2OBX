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

#include "Preprocessor.h"
#include "Tokenizer.h"
#include "Type.h"
#include "Parser.h"
#include <QFile>
#include <QSet>
#include <QtDebug>
#include <QDateTime>
#include <QFileInfo>
using namespace C;

static QSet<QByteArray> pragma_once;

struct MacroArg {
    MacroArg *next;
    QByteArray name;
    bool is_va_args;
    Token *tok;
    MacroArg():next(0),is_va_args(false),tok(0){}
};

// `#if` can be nested, so we use a stack to manage nested `#if`s.
struct CondIncl {
    CondIncl *next;
    enum { IN_THEN, IN_ELIF, IN_ELSE } ctx;
    Token *tok;
    bool included;
    CondIncl():next(0),tok(0),included(false) {}
};
static CondIncl *cond_incl = 0;

static CondIncl *push_cond_incl(Token *tok, bool included) {
    CondIncl *ci = new CondIncl();
    ci->next = cond_incl;
    ci->ctx = CondIncl::IN_THEN;
    ci->tok = tok;
    ci->included = included;
    cond_incl = ci;
    return ci;
}

static int include_next_idx = 0;
QList<QByteArray> Preprocessor::include_paths;
QByteArray Preprocessor::base_file;

static Token *preprocess2(Token *tok);

long Preprocessor::eval_const_expr(Token** rest, Token* tok)
{
    // Read and evaluate a constant expression.
    Token *start = tok;
    Token *expr = tok->next->read_const_expr(rest);
    expr = preprocess2(expr);

    if (expr->kind == Token::_EOF)
        Tokenizer::error_tok(start, "no expression");

    // [https://www.sigbus.info/n1570#6.10.1p4] The standard requires
    // we replace remaining non-macro identifiers with "0" before
    // evaluating a constant expression. For example, `#if foo` is
    // equivalent to `#if 0` if foo is not defined.
    for (Token *t = expr; t->kind != Token::_EOF; t = t->next) {
        if (t->kind == Token::IDENT) {
            Token *next = t->next;
            *t = *t->new_num_token(0);
            t->next = next;
        }
    }

    // Convert pp-numbers to regular numbers
    expr->convert_pp_tokens();

    Token *rest2;
    long val = Parser::const_expr(&rest2, expr);
    if (rest2->kind != Token::_EOF)
        Tokenizer::error_tok(rest2, "extra token");
    return val;
}

static MacroArg *read_macro_arg_one(Token **rest, Token *tok, bool read_rest) {
    Token head;
    Token *cur = &head;
    int level = 0;

    for (;;) {
        if (level == 0 && tok->equal(")"))
            break;
        if (level == 0 && !read_rest && tok->equal(","))
            break;

        if (tok->kind == Token::_EOF)
            Tokenizer::error_tok(tok, "premature end of input");

        if (tok->equal("("))
            level++;
        else if (tok->equal(")"))
            level--;

        cur = cur->next = tok->copy_token();
        tok = tok->next;
    }

    cur->next = tok->new_eof();

    MacroArg *arg = new MacroArg();
    arg->tok = head.next;
    *rest = tok;
    return arg;
}

static MacroArg *
read_macro_args(Token **rest, Token *tok, Macro::Param *params, const QByteArray& va_args_name) {
    Token *start = tok;
    tok = tok->next->next;

    MacroArg head;
    MacroArg *cur = &head;

    Macro::Param *pp = params;
    for (; pp; pp = pp->next) {
        if (cur != &head)
            tok = tok->skip(",");
        cur = cur->next = read_macro_arg_one(&tok, tok, false);
        cur->name = pp->name;
    }

    if (!va_args_name.isEmpty()) {
        MacroArg *arg;
        if (tok->equal(")")) {
            arg = new MacroArg();
            arg->tok = tok->new_eof();
        } else {
            if (pp != params)
                tok = tok->skip(",");
            arg = read_macro_arg_one(&tok, tok, true);
        }
        arg->name = va_args_name;;
        arg->is_va_args = true;
        cur = cur->next = arg;
    } else if (pp) {
        Tokenizer::error_tok(start, "too many arguments");
    }

    tok->skip(")");
    *rest = tok;
    return head.next;
}

static MacroArg *find_arg(MacroArg *args, Token *tok) {
    const QByteArray name = tok->txt;
    for (MacroArg *ap = args; ap; ap = ap->next)
        if ( ap->name == name )
            return ap;
    return 0;
}

static bool has_varargs(MacroArg *args) {
    for (MacroArg *ap = args; ap; ap = ap->next)
        if (!strcmp(ap->name, "__VA_ARGS__"))
            return ap->tok->kind != Token::_EOF;
    return false;
}

// Replace func-like macro parameters with given arguments.
static Token *subst(Token *tok, MacroArg *args) {
    Token head;
    Token *cur = &head;

    while (tok->kind != Token::_EOF) {
        // "#" followed by a parameter is replaced with stringized actuals.
        if (tok->equal("#")) {
            MacroArg *arg = find_arg(args, tok->next);
            if (!arg)
                Tokenizer::error_tok(tok->next, "'#' is not followed by a macro parameter");
            cur = cur->next = arg->tok->stringize(tok);
            tok = tok->next->next;
            continue;
        }

        // [GNU] If __VA_ARG__ is empty, `,##__VA_ARGS__` is expanded
        // to the empty token list. Otherwise, its expaned to `,` and
        // __VA_ARGS__.
        if (tok->equal(",") && tok->next->equal("##")) {
            MacroArg *arg = find_arg(args, tok->next->next);
            if (arg && arg->is_va_args) {
                if (arg->tok->kind == Token::_EOF) {
                    tok = tok->next->next->next;
                } else {
                    cur = cur->next = tok->copy_token();
                    tok = tok->next->next;
                }
                continue;
            }
        }

        if (tok->equal("##")) {
            if (cur == &head)
                Tokenizer::error_tok(tok, "'##' cannot appear at start of macro expansion");

            if (tok->next->kind == Token::_EOF)
                Tokenizer::error_tok(tok, "'##' cannot appear at end of macro expansion");

            MacroArg *arg = find_arg(args, tok->next);
            if (arg) {
                if (arg->tok->kind != Token::_EOF) {
                    *cur = *cur->paste(arg->tok);
                    for (Token *t = arg->tok->next; t->kind != Token::_EOF; t = t->next)
                        cur = cur->next = t->copy_token();
                }
                tok = tok->next->next;
                continue;
            }

            *cur = *cur->paste(tok->next);
            tok = tok->next->next;
            continue;
        }

        MacroArg *arg = find_arg(args, tok);

        if (arg && tok->next->equal("##")) {
            Token *rhs = tok->next->next;

            if (arg->tok->kind == Token::_EOF) {
                MacroArg *arg2 = find_arg(args, rhs);
                if (arg2) {
                    for (Token *t = arg2->tok; t->kind != Token::_EOF; t = t->next)
                        cur = cur->next = t->copy_token();
                } else {
                    cur = cur->next = rhs->copy_token();
                }
                tok = rhs->next;
                continue;
            }

            for (Token *t = arg->tok; t->kind != Token::_EOF; t = t->next)
                cur = cur->next = t->copy_token();
            tok = tok->next;
            continue;
        }

        // If __VA_ARG__ is empty, __VA_OPT__(x) is expanded to the
        // empty token list. Otherwise, __VA_OPT__(x) is expanded to x.
        if (tok->equal("__VA_OPT__") && tok->next->equal("(")) {
            MacroArg *arg = read_macro_arg_one(&tok, tok->next->next, true);
            if (has_varargs(args))
                for (Token *t = arg->tok; t->kind != Token::_EOF; t = t->next)
                    cur = cur->next = t;
            tok = tok->skip(")");
            continue;
        }

        // Handle a macro token. Macro arguments are completely macro-expanded
        // before they are substituted into a macro body.
        if (arg) {
            Token *t = preprocess2(arg->tok);
            t->at_bol = tok->at_bol;
            t->has_space = tok->has_space;
            for (; t->kind != Token::_EOF; t = t->next)
                cur = cur->next = t->copy_token();
            tok = tok->next;
            continue;
        }

        // Handle a non-macro token.
        cur = cur->next = tok->copy_token();
        tok = tok->next;
        continue;
    }

    cur->next = tok;
    return head.next;
}

// If tok is a macro, expand it and return true.
// Otherwise, do nothing and return false.
static bool expand_macro(Token **rest, Token *tok) {
    if (tok->hideset->hideset_contains(tok->txt))
        return false;

    Macro *m = Tokenizer::find_macro(tok);
    if (!m)
        return false;

    // Built-in dynamic macro application such as __LINE__
    if (m->handler) {
        *rest = m->handler(tok);
        (*rest)->next = tok->next;
        return true;
    }

    // Object-like macro application
    if (m->is_objlike) {
        Hideset *hs = tok->hideset->hideset_union(new Hideset(m->name));
        Token *body = m->body->add_hideset(hs);
        for (Token *t = body; t->kind != Token::_EOF; t = t->next)
            t->origin = tok;
        *rest = body->append(tok->next);
        (*rest)->at_bol = tok->at_bol;
        (*rest)->has_space = tok->has_space;
        return true;
    }

    // If a funclike macro token is not followed by an argument list,
    // treat it as a normal identifier.
    if (!tok->next->equal("("))
        return false;

    // Function-like macro application
    Token *macro_token = tok;
    MacroArg *args = read_macro_args(&tok, tok, m->params, m->va_args_name);
    Token *rparen = tok;

    // Tokens that consist a func-like macro invocation may have different
    // hidesets, and if that's the case, it's not clear what the hideset
    // for the new tokens should be. We take the interesection of the
    // macro token and the closing parenthesis and use it as a new hideset
    // as explained in the Dave Prossor's algorithm.
    Hideset *hs = macro_token->hideset->hideset_intersection(rparen->hideset);
    hs = hs->hideset_union(new Hideset(m->name));

    Token *body = subst(m->body, args);
    body = body->add_hideset(hs);
    for (Token *t = body; t->kind != Token::_EOF; t = t->next)
        t->origin = macro_token;
    *rest = body->append(tok->next);
    (*rest)->at_bol = macro_token->at_bol;
    (*rest)->has_space = macro_token->has_space;
    return true;
}

QByteArray Preprocessor::search_include_paths(QByteArray filename) {
    if (!filename.isEmpty() && filename[0] == '/')
        return filename;

    static QHash<QByteArray,QByteArray> cache;
    QByteArray cached = cache.value(filename);
    if (!cached.isEmpty())
        return cached;

    // Search a file from the include paths.
    for (int i = 0; i < Preprocessor::include_paths.size(); i++) {
        QByteArray path = Preprocessor::include_paths[i] + "/" + filename;
        if (!QFile::exists(path))
            continue;
        cache.insert(filename, path);
        include_next_idx = i + 1;
        return path;
    }
    return QByteArray();
}

static QByteArray search_include_next(QByteArray filename) {
    for (; include_next_idx < Preprocessor::include_paths.size(); include_next_idx++) {
        QByteArray path = Preprocessor::include_paths[include_next_idx] + "/" + filename;
        if (QFile::exists(path))
            return path;
    }
    return QByteArray();
}

// Read an #include argument.
static QByteArray read_include_filename(Token **rest, Token *tok, bool *is_dquote) {
    // Pattern 1: #include "foo.h"
    if (tok->kind == Token::STR) {
        // A double-quoted filename for #include is a special kind of
        // token, and we don't want to interpret any escape sequences in it.
        // For example, "\f" in "C:\foo" is not a formfeed character but
        // just two non-control characters, backslash and f.
        // So we don't want to use token->str.
        *is_dquote = true;
        *rest = tok->next->skip_line();
        return QByteArray(tok->loc + 1, tok->len - 2);
    }

    // Pattern 2: #include <foo.h>
    if (tok->equal("<")) {
        // Reconstruct a filename from a sequence of tokens between
        // "<" and ">".
        Token *start = tok;

        // Find closing ">".
        for (; !tok->equal(">"); tok = tok->next)
            if (tok->at_bol || tok->kind == Token::_EOF)
                Tokenizer::error_tok(tok, "expected '>'");

        *is_dquote = false;
        *rest = tok->next->skip_line();
        return start->next->join_tokens(tok);
    }

    // Pattern 3: #include FOO
    // In this case FOO must be macro-expanded to either
    // a single string token or a sequence of "<" ... ">".
    if (tok->kind == Token::IDENT) {
        Token *tok2 = preprocess2(tok->copy_line(rest));
        return read_include_filename(&tok2, tok2, is_dquote);
    }

    Tokenizer::error_tok(tok, "expected a filename");
}

// Detect the following "include guard" pattern.
//
//   #ifndef FOO_H
//   #define FOO_H
//   ...
//   #endif
static QByteArray detect_include_guard(Token *tok) {
    // Detect the first two lines.
    if (!tok->is_hash() || !tok->next->equal("ifndef"))
        return 0;
    tok = tok->next->next;

    if (tok->kind != Token::IDENT)
        return 0;

    const QByteArray macro = tok->txt;
    tok = tok->next;

    if (!tok->is_hash() || !tok->next->equal("define") || !tok->next->next->equal(macro))
        return NULL;

    // Read until the end of the file.
    while (tok->kind != Token::_EOF) {
        if (!tok->is_hash()) {
            tok = tok->next;
            continue;
        }

        if (tok->next->equal("endif") && tok->next->next->kind == Token::_EOF)
            return macro;

        if (tok->equal("if") || tok->equal("ifdef") || tok->equal("ifndef"))
            tok = tok->next->skip_cond_incl();
        else
            tok = tok->next;
    }
    return QByteArray();
}

static Token *include_file(Token *tok, const char *path, Token *filename_tok) {
    // Check for "#pragma once"
    if (pragma_once.contains(path))
        return tok;

    // If we read the same file before, and if the file was guarded
    // by the usual #ifndef ... #endif pattern, we may be able to
    // skip the file without opening it.
    static QHash<QByteArray,QByteArray> include_guards;
    QByteArray guard_name = include_guards.value(path);
    if (!guard_name.isEmpty() && Tokenizer::macros.contains(guard_name))
        return tok;

    Token *tok2 = Tokenizer::tokenize(path);
    if (!tok2)
        Tokenizer::error_tok(filename_tok, "%s: cannot open file", path);

    guard_name = detect_include_guard(tok2);
    if (!guard_name.isEmpty())
        include_guards.insert(path, guard_name);

    return tok2->append(tok);
}

// Read #line arguments
static void read_line_marker(Token **rest, Token *tok) {
    Token *start = tok;
    tok = Preprocessor::preprocess(tok->copy_line(rest));

    if (tok->kind != Token::NUM || tok->ty->kind != Type::INT)
        Tokenizer::error_tok(tok, "invalid line marker");
    start->file->line_delta = tok->val - start->line_no;

    tok = tok->next;
    if (tok->kind == Token::_EOF)
        return;

    if (tok->kind != Token::STR)
        Tokenizer::error_tok(tok, "filename expected");
    start->file->display_name = tok->str;
}

// Visit all tokens in `tok` while evaluating preprocessing
// macros and directives.
static Token *preprocess2(Token *tok) {
    Token head;
    Token *cur = &head;

    while (tok && tok->kind != Token::_EOF) {
        // If it is a macro, expand it.
        if (expand_macro(&tok, tok))
            continue;

        // Pass through if it is not a "#".
        if (!tok->is_hash()) {
            tok->line_delta = tok->file->line_delta;
            tok->filename = tok->file->display_name;
            cur = cur->next = tok;
            tok = tok->next;
            continue;
        }

        Token *start = tok;
        tok = tok->next;

        if (tok->equal("include")) {
            bool is_dquote;
            QByteArray filename = read_include_filename(&tok, tok->next, &is_dquote);

            if (!filename.isEmpty() && filename[0] != '/' && is_dquote) {
                QByteArray path = start->file->name + "/" + filename;
                if (QFile::exists(path)) {
                    tok = include_file(tok, path, start->next->next);
                    continue;
                }
            }

            QByteArray path = Preprocessor::search_include_paths(filename);
            tok = include_file(tok, !path.isEmpty() ? path : filename, start->next->next);
            continue;
        }

        if (tok->equal("include_next")) {
            bool ignore;
            QByteArray filename = read_include_filename(&tok, tok->next, &ignore);
            QByteArray path = search_include_next(filename);
            tok = include_file(tok, !path.isEmpty() ? path : filename, start->next->next);
            continue;
        }

        if (tok->equal("define")) {
            tok->next->read_macro_definition(&tok);
            continue;
        }

        if (tok->equal("undef")) {
            tok = tok->next;
            if (tok->kind != Token::IDENT)
                Tokenizer::error_tok(tok, "macro name must be an identifier");
            Tokenizer::undef_macro(tok->txt);
            tok = tok->next->skip_line();
            continue;
        }

        if (tok->equal("if")) {
            long val = Preprocessor::eval_const_expr(&tok, tok);
            push_cond_incl(start, val);
            if (!val)
                tok = tok->skip_cond_incl();
            continue;
        }

        if (tok->equal("ifdef")) {
            bool defined = Tokenizer::find_macro(tok->next);
            push_cond_incl(tok, defined);
            tok = tok->next->next->skip_line();
            if (!defined)
                tok = tok->skip_cond_incl();
            continue;
        }

        if (tok->equal("ifndef")) {
            bool defined = Tokenizer::find_macro(tok->next);
            push_cond_incl(tok, !defined);
            tok = tok->next->next->skip_line();
            if (defined)
                tok = tok->skip_cond_incl();
            continue;
        }

        if (tok->equal("elif")) {
            if (!cond_incl || cond_incl->ctx == CondIncl::IN_ELSE)
                Tokenizer::error_tok(start, "stray #elif");
            cond_incl->ctx = CondIncl::IN_ELIF;

            if (!cond_incl->included && Preprocessor::eval_const_expr(&tok, tok))
                cond_incl->included = true;
            else
                tok = tok->skip_cond_incl();
            continue;
        }

        if (tok->equal("else")) {
            if (!cond_incl || cond_incl->ctx == CondIncl::IN_ELSE)
                Tokenizer::error_tok(start, "stray #else");
            cond_incl->ctx = CondIncl::IN_ELSE;
            tok = tok->next->skip_line();

            if (cond_incl->included)
                tok = tok->skip_cond_incl();
            continue;
        }

        if (tok->equal("endif")) {
            if (!cond_incl)
                Tokenizer::error_tok(start, "stray #endif");
            cond_incl = cond_incl->next;
            tok = tok->next->skip_line();
            continue;
        }

        if (tok->equal("line")) {
            read_line_marker(&tok, tok->next);
            continue;
        }

        if (tok->kind == Token::PP_NUM) {
            read_line_marker(&tok, tok);
            continue;
        }

        if (tok->equal("pragma") && tok->next->equal("once")) {
            pragma_once.insert(tok->file->name);
            tok = tok->next->next->skip_line();
            continue;
        }

        if (tok->equal("pragma")) {
            do {
                tok = tok->next;
            } while (!tok->at_bol);
            continue;
        }

        if (tok->equal("warning")) {
            tok = tok->next;
            if (tok->kind != Token::STR)
                Tokenizer::error_tok(tok, "expecting a string literal");
            //qWarning() << tok->str;
            fprintf(stderr, "%s\n", tok->str.constData());
            tok = tok->next->skip_line();
            continue;
        }



        if (tok->equal("error"))
            Tokenizer::error_tok(tok, "error");

        // `#`-only line is legal. It's called a null directive.
        if (tok->at_bol)
            continue;

        Tokenizer::error_tok(tok, "invalid preprocessor directive");
    }

    cur->next = tok;
    return head.next;
}

static Macro *add_builtin(const char *name, Token * (*fn)(Token *)) {
    Macro *m = Tokenizer::add_macro(name, true, 0);
    m->handler = fn;
    return m;
}

static Token *file_macro(Token *tmpl) {
    while (tmpl->origin)
        tmpl = tmpl->origin;
    return tmpl->new_str_token(tmpl->file->display_name);
}

static Token *line_macro(Token *tmpl) {
    while (tmpl->origin)
        tmpl = tmpl->origin;
    int i = tmpl->line_no + tmpl->file->line_delta;
    return tmpl->new_num_token(i);
}

// __COUNTER__ is expanded to serial values starting from 0.
static Token *counter_macro(Token *tmpl) {
    static int i = 0;
    return tmpl->new_num_token(i++);
}

// __TIMESTAMP__ is expanded to a string describing the last
// modification time of the current file. E.g.
// "Fri Jul 24 01:32:50 2020"
static Token *timestamp_macro(Token *tmpl) {

    QFileInfo info(tmpl->file->name);
    if ( !info.isFile() )
        return tmpl->new_str_token("??? ??? ?? ??:??:?? ????");
    else
        return tmpl->new_str_token(info.lastModified().toString().toUtf8().constData());
}

static Token *base_file_macro(Token *tmpl) {
  return tmpl->new_str_token(Preprocessor::base_file);
}

// __DATE__ is expanded to the current date, e.g. "May 17 2020".
static QByteArray format_date(struct tm *tm) {
    static char mon[][4] = {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
    };

    return Tokenizer::format("\"%s %2d %d\"", mon[tm->tm_mon], tm->tm_mday, tm->tm_year + 1900);
}

// __TIME__ is expanded to the current time, e.g. "13:34:03".
static QByteArray format_time(struct tm *tm) {
    return Tokenizer::format("\"%02d:%02d:%02d\"", tm->tm_hour, tm->tm_min, tm->tm_sec);
}

void Preprocessor::init_builtins()
{
    add_builtin("__FILE__", file_macro);
    add_builtin("__LINE__", line_macro);
    add_builtin("__COUNTER__", counter_macro);
    add_builtin("__TIMESTAMP__", timestamp_macro);
    add_builtin("__BASE_FILE__", base_file_macro);
}

void Preprocessor::init_macros()
{
    // Define predefined macros
    Tokenizer::define_macro("_LP64", "1");
    Tokenizer::define_macro("__C99_MACRO_WITH_VA_ARGS", "1");
    Tokenizer::define_macro("__ELF__", "1");
    Tokenizer::define_macro("__LP64__", "1");
    Tokenizer::define_macro("__SIZEOF_DOUBLE__", "8");
    Tokenizer::define_macro("__SIZEOF_FLOAT__", "4");
    Tokenizer::define_macro("__SIZEOF_INT__", "4");
    Tokenizer::define_macro("__SIZEOF_LONG_DOUBLE__", "8");
    Tokenizer::define_macro("__SIZEOF_LONG_LONG__", "8");
    Tokenizer::define_macro("__SIZEOF_LONG__", "8");
    Tokenizer::define_macro("__SIZEOF_POINTER__", "8");
    Tokenizer::define_macro("__SIZEOF_PTRDIFF_T__", "8");
    Tokenizer::define_macro("__SIZEOF_SHORT__", "2");
    Tokenizer::define_macro("__SIZEOF_SIZE_T__", "8");
    Tokenizer::define_macro("__SIZE_TYPE__", "unsigned long");
    Tokenizer::define_macro("__STDC_HOSTED__", "1");
    Tokenizer::define_macro("__STDC_NO_COMPLEX__", "1");
    Tokenizer::define_macro("__STDC_UTF_16__", "1");
    Tokenizer::define_macro("__STDC_UTF_32__", "1");
    Tokenizer::define_macro("__STDC_VERSION__", "201112L");
    Tokenizer::define_macro("__STDC__", "1");
    Tokenizer::define_macro("__USER_LABEL_PREFIX__", "");
    Tokenizer::define_macro("__alignof__", "_Alignof");
    Tokenizer::define_macro("__amd64", "1");
    Tokenizer::define_macro("__amd64__", "1");
    Tokenizer::define_macro("__chibicc__", "1");
    Tokenizer::define_macro("__const__", "const");
    Tokenizer::define_macro("__gnu_linux__", "1");
    Tokenizer::define_macro("__inline__", "inline");
    Tokenizer::define_macro("__linux", "1");
    Tokenizer::define_macro("__linux__", "1");
    Tokenizer::define_macro("__signed__", "signed");
    Tokenizer::define_macro("__typeof__", "typeof");
    Tokenizer::define_macro("__unix", "1");
    Tokenizer::define_macro("__unix__", "1");
    Tokenizer::define_macro("__volatile__", "volatile");
    Tokenizer::define_macro("__x86_64", "1");
    Tokenizer::define_macro("__x86_64__", "1");
    Tokenizer::define_macro("linux", "1");
    Tokenizer::define_macro("unix", "1");

    init_builtins();

    time_t now = time(NULL);
    struct tm *tm = localtime(&now);
    Tokenizer::define_macro("__DATE__", format_date(tm));
    Tokenizer::define_macro("__TIME__", format_time(tm));
}

enum StringKind {
    STR_NONE, STR_UTF8, STR_UTF16, STR_UTF32, STR_WIDE,
};

static StringKind getStringKind(Token *tok) {
    if (!strcmp(tok->loc, "u8"))
        return STR_UTF8;

    switch (tok->loc[0]) {
    case '"': return STR_NONE;
    case 'u': return STR_UTF16;
    case 'U': return STR_UTF32;
    case 'L': return STR_WIDE;
    }
    Q_ASSERT(false);
}

// Concatenate adjacent string literals into a single string literal
// as per the C spec.
static void join_adjacent_string_literals(Token *tok) {
    // First pass: If regular string literals are adjacent to wide
    // string literals, regular string literals are converted to a wide
    // type before concatenation. In this pass, we do the conversion.
    for (Token *tok1 = tok; tok1->kind != Token::_EOF;) {
        if (tok1->kind != Token::STR || tok1->next->kind != Token::STR) {
            tok1 = tok1->next;
            continue;
        }

        StringKind kind = getStringKind(tok1);
        Type *basety = tok1->ty->base;

        for (Token *t = tok1->next; t->kind == Token::STR; t = t->next) {
            StringKind k = getStringKind(t);
            if (kind == STR_NONE) {
                kind = k;
                basety = t->ty->base;
            } else if (k != STR_NONE && kind != k) {
                Tokenizer::error_tok(t, "unsupported non-standard concatenation of string literals");
            }
        }

        if (basety->size > 1)
            for (Token *t = tok1; t->kind == Token::STR; t = t->next)
                if (t->ty->base->size == 1)
                    *t = *t->tokenize_string_literal(basety);

        while (tok1->kind == Token::STR)
            tok1 = tok1->next;
    }

    // Second pass: concatenate adjacent string literals.
    for (Token *tok1 = tok; tok1->kind != Token::_EOF;) {
        if (tok1->kind != Token::STR || tok1->next->kind != Token::STR) {
            tok1 = tok1->next;
            continue;
        }

        Token *tok2 = tok1->next;
        while (tok2->kind == Token::STR)
            tok2 = tok2->next;

        int len = tok1->ty->array_len;
        for (Token *t = tok1->next; t != tok2; t = t->next)
            len = len + t->ty->array_len - 1;

        QByteArray buf;

        for (Token *t = tok1; t != tok2; t = t->next) {
            buf += t->str;
        }

        *tok1 = *tok1->copy_token();
        tok1->ty = tok1->ty->base->array_of(len);
        tok1->str = buf;
        tok1->next = tok2;
        tok1 = tok2;
    }
}

Token*Preprocessor::preprocess(Token* tok)
{
    tok = preprocess2(tok);
    if (cond_incl)
        Tokenizer::error_tok(cond_incl->tok, "unterminated conditional directive");
    tok->convert_pp_tokens();
    join_adjacent_string_literals(tok);

    for (Token *t = tok; t; t = t->next)
        t->line_no += t->line_delta;
    return tok;
}

