#ifndef TOKENIZER_H
#define TOKENIZER_H

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
#include <QHash>

namespace C
{

struct Token;
struct Hideset;
struct Type;

struct File
{
    QByteArray name;
    int file_no;
    QByteArray contents;

    // For #line directive
    QByteArray display_name;
    int line_delta;

    File();
    Token *tokenize();
    static File* new_file(const QByteArray& name, int file_no, const QByteArray& contents);

    static QList<File*> input_files;
};

struct Token
{
    enum Kind {
        IDENT,   // Identifiers
        PUNCT,   // Punctuators
        KEYWORD, // Keywords
        STR,     // String literals
        NUM,     // Numeric literals
        PP_NUM,  // Preprocessing numbers
        _EOF,     // End-of-file markers
    };

    Kind kind;   // Token kind
    Token *next;      // Next token
    qint64 val;      // If kind is TK_NUM, its value
    long double fval; // If kind is TK_NUM, its value
    const char *loc;        // Token location
    int len;          // Token length
    QByteArray txt;   // redundant copy of loc/len used for unique address when assigned to node->label
    Type *ty;         // Used if TK_NUM or TK_STR
    QByteArray str;        // String literal contents (utf8) including terminating '\0'

    File *file;       // Source location
    QByteArray filename;   // Filename
    int line_no;      // Line number
    int line_delta;   // Line number
    bool at_bol;      // True if this token is at beginning of line
    bool has_space;   // True if this token follows a space character
    Hideset *hideset; // For macro expansion
    Token *origin;    // If this is expanded from a macro, the original token

    Token();
    ~Token();

    bool equal(const char *op) const;
    Token* skip(const char *op) const; // Ensure that the current token is `op`.
    bool consume(Token **rest, const char *str);
    static Token *new_token(Kind kind, const char *start, const char *end);
    bool is_keyword() const;
    bool convert_pp_int();
    void convert_pp_number();
    void convert_pp_tokens();
    void add_line_numbers(); // Initialize line info for all tokens.
    Token* tokenize_string_literal(Type *basety) const;
    bool is_hash() const;
    Token *skip_line() const;
    Token* copy_token() const;
    Token* new_eof() const;
    Token *append(Token *tok2);
    Token *skip_cond_incl2() const;
    Token *skip_cond_incl() const;
    Token *new_str_token(const char *str ) const; // this: tmpl
    Token *copy_line(Token **rest) const;
    Token *new_num_token(int val) const; // this: tmpl
    Token *read_const_expr(Token **rest) const;
    void read_macro_definition(Token **rest);
    QByteArray join_tokens(Token *end) const; // Concatenates all tokens in this and returns a new string.
    Token *stringize(Token *hash) const; // this: arg
    Token *paste(Token *rhs) const; // this: lhs
    Token *add_hideset(Hideset *hs) const;
};

struct Macro {
    struct Param {
        Param *next;
        QByteArray name;
        Param():next(0){}
    };

    QByteArray name;
    bool is_objlike; // Object-like or function-like
    Param* params;
    QByteArray va_args_name;
    Token *body;
    Token * (*handler)(Token *);

    Macro();

    static Param* read_macro_params(Token **rest, Token *tok, QByteArray& va_args_name);
};

struct Hideset
{
    Hideset *next;
    QByteArray name;
    Hideset(const QByteArray& n = QByteArray()):next(0),name(n){}
    Hideset *hideset_union(Hideset *hs2) const; // this: hs1
    bool hideset_contains(const QByteArray& name) const;
    Hideset *hideset_intersection(Hideset *hs2) const; // this: hs1
};

class Tokenizer
{
public:
    static void error(const char *fmt, ...); // Reports an error and exit.
    static void error_at(const char *loc, const char *fmt, ...);
    static void error_tok(const Token *tok, const char *fmt, ...);
    static void warn_tok(const Token *tok, const char *fmt, ...);

    static Token* tokenize(const QString& filePath);
    static QByteArray format(const char *fmt, ...); // Takes a printf-style format string and creates/returns a formatted string.

    typedef QHash<QByteArray,Macro*> Macros;
    static Macros macros;
    static bool tokenize_errors_cause_exception;

    static Macro *add_macro(const char *name, bool is_objlike, Token *body);
    static Macro* find_macro(Token*);
    static void undef_macro(const char *name);
    static void define_macro(const char *name, const char *buf);
    static void define_macro(const QByteArray& buf);
private:
    Tokenizer();
};
}

#endif // TOKENIZER_H
