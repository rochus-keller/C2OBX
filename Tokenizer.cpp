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

#include "Tokenizer.h"
#include "Type.h"
#include <QBuffer>
#include <QSet>
#include <stdarg.h>
#include <stdio.h>
#include <QtDebug>
using namespace C;

bool Tokenizer::tokenize_errors_cause_exception = false;
static File* current_file = 0; // Input file
static bool at_bol = false; // True if the current position is at the beginning of a line
static bool has_space = false; // True if the current position follows a space character

QHash<QByteArray,Macro*> Tokenizer::macros;

// Reports an error message in the following format.
//
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(const char *filename, const char *input, int line_no,
                      const char *loc, const char *fmt, va_list ap) {
    // Find a line containing `loc`.
    const char *line = loc;
    while (input < line && line[-1] != '\n')
        line--;

    const char *end = loc;
    while (*end && *end != '\n')
        end++;

    // Print out the line.
    const int indent = fprintf(stderr, "%s:%d: ", filename, line_no);
    fprintf(stderr, "%.*s\n", (int)(end - line), line);

    // Show the error message.
    const int pos = QString::fromUtf8(line, loc - line).size() + indent;

    fprintf(stderr, "%*s", pos, ""); // print pos spaces.
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}

void Tokenizer::error(const char* fmt, ...)
{
    if( tokenize_errors_cause_exception )
        throw 0;
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

void Tokenizer::error_at(const char* loc, const char* fmt, ...)
{
    if( tokenize_errors_cause_exception )
        throw 0;
    Q_ASSERT(current_file);
    int line_no = 1;
    for (const char *p = current_file->contents; p < loc; p++)
        if (*p == '\n')
            line_no++;

    va_list ap;
    va_start(ap, fmt);
    verror_at(current_file->name, current_file->contents, line_no, loc, fmt, ap);
    exit(1);
}

void Tokenizer::error_tok(const Token* tok, const char* fmt, ...)
{
    if( tokenize_errors_cause_exception )
        throw 0;
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
    exit(1);
}

void Tokenizer::warn_tok(const Token* tok, const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
    va_end(ap);
}

Token::Token():kind(IDENT),next(0),val(0),fval(0),ty(0),file(0),line_no(0),line_delta(0),
    at_bol(false),has_space(false),hideset(0),origin(0),loc(0),len(0)
{
}

Token::~Token()
{
}

bool Token::equal(const char* op) const
{
    return memcmp(loc, op, len) == 0 && op[len] == '\0';
}

Token* Token::skip(const char* op) const
{
    if (!equal(op))
        Tokenizer::error_tok(this, "expected '%s'", op);
    return next;
}

bool Token::consume(Token** rest, const char* str)
{
    if (equal(str))
    {
        *rest = next;
        return true;
    }
    *rest = this;
    return false;
}

Token* Token::new_token(Token::Kind kind, const char* start, const char* end)
{
    Q_ASSERT( current_file );
    Token *tok = new Token();
    tok->kind = kind;
    tok->loc = start;
    tok->len = end - start;
    tok->txt = QByteArray(start, end - start);
    tok->file = current_file;
    tok->filename = current_file->display_name;
    tok->at_bol = ::at_bol;
    tok->has_space = ::has_space;

    ::at_bol = ::has_space = false;
    return tok;
}

static bool startswith(const char *p, const char *q) {
  return strncmp(p, q, strlen(q)) == 0;
}

static bool in_range(quint32 *range, quint32 c) {
    for (int i = 0; range[i] != -1; i += 2)
        if (range[i] <= c && c <= range[i + 1])
            return true;
    return false;
}

// [https://www.sigbus.info/n1570#D] C11 allows not only ASCII but
// some multibyte characters in certan Unicode ranges to be used in an
// identifier.
//
// This function returns true if a given character is acceptable as
// the first character of an identifier.
//
// For example, ¾ (U+00BE) is a valid identifier because characters in
// 0x00BE-0x00C0 are allowed, while neither ⟘ (U+27D8) nor '　'
// (U+3000, full-width space) are allowed because they are out of range.
bool is_ident1(quint32 c) {
    static quint32 range[] = {
        '_', '_', 'a', 'z', 'A', 'Z', '$', '$',
        0x00A8, 0x00A8, 0x00AA, 0x00AA, 0x00AD, 0x00AD, 0x00AF, 0x00AF,
        0x00B2, 0x00B5, 0x00B7, 0x00BA, 0x00BC, 0x00BE, 0x00C0, 0x00D6,
        0x00D8, 0x00F6, 0x00F8, 0x00FF, 0x0100, 0x02FF, 0x0370, 0x167F,
        0x1681, 0x180D, 0x180F, 0x1DBF, 0x1E00, 0x1FFF, 0x200B, 0x200D,
        0x202A, 0x202E, 0x203F, 0x2040, 0x2054, 0x2054, 0x2060, 0x206F,
        0x2070, 0x20CF, 0x2100, 0x218F, 0x2460, 0x24FF, 0x2776, 0x2793,
        0x2C00, 0x2DFF, 0x2E80, 0x2FFF, 0x3004, 0x3007, 0x3021, 0x302F,
        0x3031, 0x303F, 0x3040, 0xD7FF, 0xF900, 0xFD3D, 0xFD40, 0xFDCF,
        0xFDF0, 0xFE1F, 0xFE30, 0xFE44, 0xFE47, 0xFFFD,
        0x10000, 0x1FFFD, 0x20000, 0x2FFFD, 0x30000, 0x3FFFD, 0x40000, 0x4FFFD,
        0x50000, 0x5FFFD, 0x60000, 0x6FFFD, 0x70000, 0x7FFFD, 0x80000, 0x8FFFD,
        0x90000, 0x9FFFD, 0xA0000, 0xAFFFD, 0xB0000, 0xBFFFD, 0xC0000, 0xCFFFD,
        0xD0000, 0xDFFFD, 0xE0000, 0xEFFFD, -1,
    };

    return in_range(range, c);
}

// Returns true if a given character is acceptable as a non-first
// character of an identifier.
bool is_ident2(quint32 c) {
    static quint32 range[] = {
        '0', '9', '$', '$', 0x0300, 0x036F, 0x1DC0, 0x1DFF, 0x20D0, 0x20FF,
        0xFE20, 0xFE2F, -1,
    };

    return is_ident1(c) || in_range(range, c);
}

// Read a UTF-8-encoded Unicode code point from a source file.
// We assume that source files are always in UTF-8.
//
// UTF-8 is a variable-width encoding in which one code point is
// encoded in one to four bytes. One byte UTF-8 code points are
// identical to ASCII. Non-ASCII characters are encoded using more
// than one byte.
static quint32 decode_utf8(const char **new_pos, const char *p)
{
    if ((unsigned char)*p < 128) {
        *new_pos = p + 1;
        return *p;
    }

    const char *start = p;
    int len;
    quint32 c;

    if ((unsigned char)*p >= 0b11110000) {
        len = 4;
        c = *p & 0b111;
    } else if ((unsigned char)*p >= 0b11100000) {
        len = 3;
        c = *p & 0b1111;
    } else if ((unsigned char)*p >= 0b11000000) {
        len = 2;
        c = *p & 0b11111;
    } else {
        Tokenizer::error_at(start, "invalid UTF-8 sequence");
    }

    for (int i = 1; i < len; i++) {
        if ((unsigned char)p[i] >> 6 != 0b10)
            Tokenizer::error_at(start, "invalid UTF-8 sequence");
        c = (c << 6) | (p[i] & 0b111111);
    }

    *new_pos = p + len;
    return c;
}

// Read an identifier and returns the length of it.
// If p does not point to a valid identifier, 0 is returned.
static int read_ident(const char *start) {
    const char *p = start;
    quint32 c = decode_utf8(&p, p);
    if (!is_ident1(c))
        return 0;

    for (;;) {
        const char *q;
        c = decode_utf8(&q, p);
        if (!is_ident2(c))
            return p - start;
        p = q;
    }
}

static int from_hex(char c) {
    if ('0' <= c && c <= '9')
        return c - '0';
    if ('a' <= c && c <= 'f')
        return c - 'a' + 10;
    return c - 'A' + 10;
}

// Read a punctuator token from p and returns its length.
static int read_punct(const char *p) {
    static char *kw[] = {
        "<<=", ">>=", "...", "==", "!=", "<=", ">=", "->", "+=",
        "-=", "*=", "/=", "++", "--", "%=", "&=", "|=", "^=", "&&",
        "||", "<<", ">>", "##",
    };

    for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
        if (startswith(p, kw[i]))
            return strlen(kw[i]);

    return ispunct(*p) ? 1 : 0;
}

bool Token::is_keyword() const
{
    static QSet<QByteArray> map;

    if ( map.isEmpty() ) {
        static const char *kw[] = {
            "return", "if", "else", "for", "while", "int", "sizeof", "char",
            "struct", "union", "short", "long", "void", "typedef", "_Bool",
            "enum", "static", "goto", "break", "continue", "switch", "case",
            "default", "extern", "_Alignof", "_Alignas", "do", "signed",
            "unsigned", "const", "volatile", "auto", "register", "restrict",
            "__restrict", "__restrict__", "_Noreturn", "float", "double",
            "typeof", "asm", "_Thread_local", "__thread", "_Atomic",
            "__attribute__",
        };

        for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
            map.insert(kw[i]);
    }

    return map.contains( txt );
}

static int read_escaped_char(const char **new_pos, const char *p) {
    if ('0' <= *p && *p <= '7') {
        // Read an octal number.
        int c = *p++ - '0';
        if ('0' <= *p && *p <= '7') {
            c = (c << 3) + (*p++ - '0');
            if ('0' <= *p && *p <= '7')
                c = (c << 3) + (*p++ - '0');
        }
        *new_pos = p;
        return c;
    }

    if (*p == 'x') {
        // Read a hexadecimal number.
        p++;
        if (!isxdigit(*p))
            Tokenizer::error_at(p, "invalid hex escape sequence");

        int c = 0;
        for (; isxdigit(*p); p++)
            c = (c << 4) + from_hex(*p);
        *new_pos = p;
        return c;
    }

    *new_pos = p + 1;

    // Escape sequences are defined using themselves here. E.g.
    // '\n' is implemented using '\n'. This tautological definition
    // works because the compiler that compiles our compiler knows
    // what '\n' actually is. In other words, we "inherit" the ASCII
    // code of '\n' from the compiler that compiles our compiler,
    // so we don't have to teach the actual code here.
    //
    // This fact has huge implications not only for the correctness
    // of the compiler but also for the security of the generated code.
    // For more info, read "Reflections on Trusting Trust" by Ken Thompson.
    // https://github.com/rui314/chibicc/wiki/thompson1984.pdf
    switch (*p) {
    case 'a': return '\a';
    case 'b': return '\b';
    case 't': return '\t';
    case 'n': return '\n';
    case 'v': return '\v';
    case 'f': return '\f';
    case 'r': return '\r';
        // [GNU] \e for the ASCII escape character is a GNU C extension.
    case 'e': return 27;
    default: return *p;
    }
}

// Find a closing double-quote.
static const char *string_literal_end(const char *p) {
    const char *start = p;
    for (; *p != '"'; p++) {
        if (*p == '\n' || *p == '\0')
            Tokenizer::error_at(start, "unclosed string literal");
        if (*p == '\\')
            p++;
    }
    return p;
}

static Token* read_string_literal(const char *start, const char *quote) {
    const char *end = string_literal_end(quote + 1);

    QByteArray buf;
    buf.resize(end - quote);
    int len = 0;

    for (const char *p = quote + 1; p < end;) {
        if (*p == '\\')
            buf[len++] = read_escaped_char(&p, p + 1);
        else
            buf[len++] = *p++;
    }

    Token *tok = Token::new_token(Token::STR, start, end + 1);
    tok->ty = Type::_char->array_of(len + 1);
    tok->str = buf;
    return tok;
}

// Read a UTF-8-encoded string literal and transcode it in UTF-16.
//
// UTF-16 is yet another variable-width encoding for Unicode. Code
// points smaller than U+10000 are encoded in 2 bytes. Code points
// equal to or larger than that are encoded in 4 bytes. Each 2 bytes
// in the 4 byte sequence is called "surrogate", and a 4 byte sequence
// is called a "surrogate pair".
static Token *read_utf16_string_literal(const char *start, const char *quote) {
    const char *end = string_literal_end(quote + 1);
    QString buf;
    buf.resize(end - start);
    int len = 0;

    for (const char *p = quote + 1; p < end;) {
        if (*p == '\\') {
            buf[len++] = read_escaped_char(&p, p + 1);
            continue;
        }

        quint32 c = decode_utf8(&p, p);
        if (c < 0x10000) {
            // Encode a code point in 2 bytes.
            buf[len++] = c;
        } else {
            // Encode a code point in 4 bytes.
            c -= 0x10000;
            buf[len++] = 0xd800 + ((c >> 10) & 0x3ff);
            buf[len++] = 0xdc00 + (c & 0x3ff);
        }
    }

    Token *tok = Token::new_token(Token::STR, start, end + 1);
    tok->ty = Type::_ushort->array_of(len + 1);
    tok->str = buf.toUtf8();
    return tok;
}

// Read a UTF-8-encoded string literal and transcode it in UTF-32.
//
// UTF-32 is a fixed-width encoding for Unicode. Each code point is
// encoded in 4 bytes.
static Token *read_utf32_string_literal(const char *start, const char *quote, Type *ty) {
    const char *end = string_literal_end(quote + 1);
    quint32 *buf = (quint32*)calloc(4, end - quote); // acceptable
    int len = 0;

    for (const char *p = quote + 1; p < end;) {
        if (*p == '\\')
            buf[len++] = read_escaped_char(&p, p + 1);
        else
            buf[len++] = decode_utf8(&p, p);
    }

    Token *tok = Token::new_token(Token::STR, start, end + 1);
    tok->ty = ty->array_of(len + 1);
    tok->str = QString::fromUcs4(buf).toUtf8();
    free(buf);
    return tok;
}

static Token *read_char_literal(const char *start, const char *quote, Type *ty) {
    const char *p = quote + 1;
    if (*p == '\0')
        Tokenizer::error_at(start, "unclosed char literal");

    int c;
    if (*p == '\\')
        c = read_escaped_char(&p, p + 1);
    else
        c = decode_utf8(&p, p);

    const char *end = strchr(p, '\'');
    if (!end)
        Tokenizer::error_at(p, "unclosed char literal");

    Token *tok = Token::new_token(Token::NUM, start, end + 1);
    tok->val = c;
    tok->ty = ty;
    return tok;
}

bool Token::convert_pp_int()
{
    Token* tok = this;
    Q_ASSERT(tok->kind == Token::PP_NUM);

    const char *p = tok->loc;

    // Read a binary, octal, decimal or hexadecimal number.
    int base = 10;
    if (!strncasecmp(p, "0x", 2) && isxdigit(p[2])) {
        p += 2;
        base = 16;
    } else if (!strncasecmp(p, "0b", 2) && (p[2] == '0' || p[2] == '1')) {
        p += 2;
        base = 2;
    } else if (*p == '0') {
        base = 8;
    }

    qint64 val = strtoul(p, (char**)&p, base);

    // Read U, L or LL suffixes.
    bool l = false;
    bool u = false;

    if (startswith(p, "LLU") || startswith(p, "LLu") ||
            startswith(p, "llU") || startswith(p, "llu") ||
            startswith(p, "ULL") || startswith(p, "Ull") ||
            startswith(p, "uLL") || startswith(p, "ull")) {
        p += 3;
        l = u = true;
    } else if (!strncasecmp(p, "lu", 2) || !strncasecmp(p, "ul", 2)) {
        p += 2;
        l = u = true;
    } else if (startswith(p, "LL") || startswith(p, "ll")) {
        p += 2;
        l = true;
    } else if (*p == 'L' || *p == 'l') {
        p++;
        l = true;
    } else if (*p == 'U' || *p == 'u') {
        p++;
        u = true;
    }

    if (p != tok->loc + tok->len)
        return false;

    Q_ASSERT(Type::_ulong && Type::_long && Type::_uint && Type::_int);

    // Infer a type.
    Type *ty = 0;
    if (base == 10) {
        if (l && u)
            ty = Type::_ulong;
        else if (l)
            ty = Type::_long;
        else if (u)
            ty = (val >> 32) ? Type::_ulong : Type::_uint;
        else
            ty = (val >> 31) ? Type::_long : Type::_int;
    } else {
        if (l && u)
            ty = Type::_ulong;
        else if (l)
            ty = (val >> 63) ? Type::_ulong : Type::_long;
        else if (u)
            ty = (val >> 32) ? Type::_ulong : Type::_uint;
        else if (val >> 63)
            ty = Type::_ulong;
        else if (val >> 32)
            ty = Type::_long;
        else if (val >> 31)
            ty = Type::_uint;
        else
            ty = Type::_int;
    }
    Q_ASSERT( ty );

    tok->kind = Token::NUM;
    tok->val = val;
    tok->ty = ty;
    return true;
}

// The definition of the numeric literal at the preprocessing stage
// is more relaxed than the definition of that at the later stages.
// In order to handle that, a numeric literal is tokenized as a
// "pp-number" token first and then converted to a regular number
// token after preprocessing.
//
// This function converts a pp-number token to a regular number token.
void Token::convert_pp_number()
{
    Token* tok = this;
    Q_ASSERT(tok->kind == Token::PP_NUM);

    // Try to parse as an integer constant.
    if (convert_pp_int())
        return;

    // If it's not an integer, it must be a floating point constant.
    char *end;
    long double val = strtold(tok->loc, &end);

    Q_ASSERT( Type::_float && Type::_ldouble && Type::_double );

    Type *ty = 0;
    if (*end == 'f' || *end == 'F') {
        ty = Type::_float;
        end++;
    } else if (*end == 'l' || *end == 'L') {
        ty = Type::_ldouble;
        end++;
    } else {
        ty = Type::_double;
    }

    Q_ASSERT( ty );

    if (tok->loc + tok->len != end)
        Tokenizer::error_tok(tok, "invalid numeric constant");

    tok->kind = Token::NUM;
    tok->fval = val;
    tok->ty = ty;
}

void Token::convert_pp_tokens()
{
    for( Token *t = this; t->kind != Token::_EOF; t = t->next) {
        if (t->is_keyword())
            t->kind = Token::KEYWORD;
        else if (t->kind == Token::PP_NUM)
            t->convert_pp_number();
    }
}

void Token::add_line_numbers()
{
    Q_ASSERT( current_file );
    const char *p = current_file->contents;
    int n = 1;

    Token* tok = this;
    do {
        if (p == tok->loc) {
            tok->line_no = n;
            tok = tok->next;
        }
        if (*p == '\n')
            n++;
    } while (*p++);
}

Token* Token::tokenize_string_literal(Type* basety) const
{
    Token *t = 0;
    if (basety->size == 2)
      t = read_utf16_string_literal(loc, loc);
    else
      t = read_utf32_string_literal(loc, loc, basety);
    Q_ASSERT(t);
    t->next = next;
    return t;
}

bool Token::is_hash() const
{
    return at_bol && equal("#");
}

// Some preprocessor directives such as #include allow extraneous
// tokens before newline. This function skips such tokens.
Token*Token::skip_line() const
{
    Token* tok = const_cast<Token*>(this);
    if (tok->at_bol)
      return tok;
    Tokenizer::warn_tok(tok, "extra token");
    while (tok->at_bol)
      tok = tok->next;
    return tok;
}

Token*Token::copy_token() const
{
    Token* t = new Token();
    *t = *this;
    t->next = 0;
    return t;
}

Token*Token::new_eof() const
{
    Token *t = copy_token();
    t->kind = _EOF;
    t->len = 0;
    return t;
}

Token*Token::append(Token* tok2)
{
    // Append tok2 to the end of tok1.
    Token* tok1 = this;
    if (tok1->kind == _EOF)
        return tok2;

    Token head;
    Token *cur = &head;

    for (; tok1->kind != _EOF; tok1 = tok1->next)
        cur = cur->next = tok1->copy_token();
    cur->next = tok2;
    return head.next;
}

Token*Token::skip_cond_incl2() const
{
    Token* tok = const_cast<Token*>(this);
    while (tok->kind != _EOF) {
        if (tok->is_hash() &&
                (tok->next->equal("if") || tok->next->equal("ifdef") ||
                 tok->next->equal("ifndef"))) {
            tok = tok->next->next->skip_cond_incl2();
            continue;
        }
        if (tok->is_hash() && tok->next->equal("endif"))
            return tok->next->next;
        tok = tok->next;
    }
    return tok;
}

Token*Token::skip_cond_incl() const
{
    // Skip until next `#else`, `#elif` or `#endif`.
    // Nested `#if` and `#endif` are skipped.
    Token* tok = const_cast<Token*>(this);
    while (tok->kind != _EOF) {
        if (tok->is_hash() &&
                (tok->next->equal("if") || tok->next->equal("ifdef") ||
                 tok->next->equal("ifndef"))) {
            tok = tok->next->next->skip_cond_incl2();
            continue;
        }

        if (tok->is_hash() &&
                (tok->next->equal("elif") || tok->next->equal("else") ||
                 tok->next->equal("endif")))
            break;
        tok = tok->next;
    }
    return tok;
}

QList<File*> File::input_files;

File::File():file_no(0),display_name(0),line_delta(0)
{
}

Token* File::tokenize()
{
    current_file = this;

    const char *p = current_file->contents;
    Token head;
    Token *cur = &head;

    at_bol = true;
    has_space = false;

    while (*p) {
        // Skip line comments.
        if (startswith(p, "//")) {
            p += 2;
            while (*p != '\n')
                p++;
            has_space = true;
            continue;
        }

        // Skip block comments.
        if (startswith(p, "/*")) {
            const char *q = strstr(p + 2, "*/");
            if (!q)
                Tokenizer::error_at(p, "unclosed block comment");
            p = q + 2;
            has_space = true;
            continue;
        }

        // Skip newline.
        if (*p == '\n') {
            p++;
            at_bol = true;
            has_space = false;
            continue;
        }

        // Skip whitespace characters.
        if (isspace(*p)) {
            p++;
            has_space = true;
            continue;
        }

        // Numeric literal
        if (isdigit(*p) || (*p == '.' && isdigit(p[1]))) {
            const char *q = p++;
            for (;;) {
                if (p[0] && p[1] && strchr("eEpP", p[0]) && strchr("+-", p[1]))
                    p += 2;
                else if (isalnum(*p) || *p == '.')
                    p++;
                else
                    break;
            }
            cur = cur->next = Token::new_token(Token::PP_NUM, q, p);
            continue;
        }

        // String literal
        if (*p == '"') {
            cur = cur->next = read_string_literal(p, p);
            p += cur->len;
            continue;
        }

        // UTF-8 string literal
        if (startswith(p, "u8\"")) {
            cur = cur->next = read_string_literal(p, p + 2);
            p += cur->len;
            continue;
        }

        // UTF-16 string literal
        if (startswith(p, "u\"")) {
            cur = cur->next = read_utf16_string_literal(p, p + 1);
            p += cur->len;
            continue;
        }

        Q_ASSERT( Type::_int && Type::_uint && Type::_ushort);

        // Wide string literal
        if (startswith(p, "L\"")) {
            cur = cur->next = read_utf32_string_literal(p, p + 1, Type::_int);
            p += cur->len;
            continue;
        }

        // UTF-32 string literal
        if (startswith(p, "U\"")) {
            cur = cur->next = read_utf32_string_literal(p, p + 1, Type::_uint);
            p += cur->len;
            continue;
        }

        // Character literal
        if (*p == '\'') {
            cur = cur->next = read_char_literal(p, p, Type::_int);
            cur->val = (char)cur->val;
            p += cur->len;
            continue;
        }

        // UTF-16 character literal
        if (startswith(p, "u'")) {
            cur = cur->next = read_char_literal(p, p + 1, Type::_ushort);
            cur->val &= 0xffff;
            p += cur->len;
            continue;
        }

        // Wide character literal
        if (startswith(p, "L'")) {
            cur = cur->next = read_char_literal(p, p + 1, Type::_int);
            p += cur->len;
            continue;
        }

        // UTF-32 character literal
        if (startswith(p, "U'")) {
            cur = cur->next = read_char_literal(p, p + 1, Type::_uint);
            p += cur->len;
            continue;
        }

        // Identifier or keyword
        int ident_len = read_ident(p);
        if (ident_len) {
            cur = cur->next = Token::new_token(Token::IDENT, p, p + ident_len);
            p += cur->len;
            continue;
        }

        // Punctuators
        int punct_len = read_punct(p);
        if (punct_len) {
            cur = cur->next = Token::new_token(Token::PUNCT, p, p + punct_len);
            p += cur->len;
            continue;
        }

        Tokenizer::error_at(p, "invalid token");
    }

    cur->next = Token::new_token(Token::_EOF, p, p);
    cur = head.next;
    cur->add_line_numbers();
    current_file = 0;
    return cur;
}

// Returns the contents of a given file.
static QByteArray read_file(const char *path) {
    FILE *fp;

    if (strcmp(path, "-") == 0) {
        // By convention, read from stdin if a given filename is "-".
        fp = stdin;
    } else {
        fp = fopen(path, "r");
        if (!fp)
            return NULL;
    }

    QBuffer buf;
    buf.open(QIODevice::WriteOnly);

    // Read the entire file.
    for (;;) {
        char buf2[4096];
        int n = fread(buf2, 1, sizeof(buf2), fp);
        if (n == 0)
            break;
        buf.write(buf2, n);
    }

    if (fp != stdin)
        fclose(fp);

    // Make sure that the last line is properly terminated with '\n'.
    buf.putChar('\n');
    buf.close();
    return buf.data();
}

File*File::new_file(const QByteArray& name, int file_no, const QByteArray& contents)
{
    File *file = new File();
    file->name = name;
    file->display_name = name;
    file->file_no = file_no;
    file->contents = contents;
    return file;
}

// Replaces \r or \r\n with \n.
static void canonicalize_newline(char *p) {
    int i = 0, j = 0;

    while (p[i]) {
        if (p[i] == '\r' && p[i + 1] == '\n') {
            i += 2;
            p[j++] = '\n';
        } else if (p[i] == '\r') {
            i++;
            p[j++] = '\n';
        } else {
            p[j++] = p[i++];
        }
    }

    p[j] = '\0';
}

// Removes backslashes followed by a newline.
static void remove_backslash_newline(char *p) {
    int i = 0, j = 0;

    // We want to keep the number of newline characters so that
    // the logical line number matches the physical one.
    // This counter maintain the number of newlines we have removed.
    int n = 0;

    while (p[i]) {
        if (p[i] == '\\' && p[i + 1] == '\n') {
            i += 2;
            n++;
        } else if (p[i] == '\n') {
            p[j++] = p[i++];
            for (; n > 0; n--)
                p[j++] = '\n';
        } else {
            p[j++] = p[i++];
        }
    }

    for (; n > 0; n--)
        p[j++] = '\n';
    p[j] = '\0';
}

static quint32 read_universal_char(char *p, int len) {
    quint32 c = 0;
    for (int i = 0; i < len; i++) {
        if (!isxdigit(p[i]))
            return 0;
        c = (c << 4) | from_hex(p[i]);
    }
    return c;
}

// Encode a given character in UTF-8.
static int encode_utf8(char *buf, quint32 c) {
  if (c <= 0x7F) {
    buf[0] = c;
    return 1;
  }

  if (c <= 0x7FF) {
    buf[0] = 0b11000000 | (c >> 6);
    buf[1] = 0b10000000 | (c & 0b00111111);
    return 2;
  }

  if (c <= 0xFFFF) {
    buf[0] = 0b11100000 | (c >> 12);
    buf[1] = 0b10000000 | ((c >> 6) & 0b00111111);
    buf[2] = 0b10000000 | (c & 0b00111111);
    return 3;
  }

  buf[0] = 0b11110000 | (c >> 18);
  buf[1] = 0b10000000 | ((c >> 12) & 0b00111111);
  buf[2] = 0b10000000 | ((c >> 6) & 0b00111111);
  buf[3] = 0b10000000 | (c & 0b00111111);
  return 4;
}

// Replace \u or \U escape sequences with corresponding UTF-8 bytes.
static void convert_universal_chars(char *p) {
    char *q = p;

    while (*p) {
        if (startswith(p, "\\u")) {
            const quint32 c = read_universal_char(p + 2, 4);
            if (c) {
                p += 6;
                q += encode_utf8(q, c);
            } else {
                *q++ = *p++;
            }
        } else if (startswith(p, "\\U")) {
            const quint32 c = read_universal_char(p + 2, 8);
            if (c) {
                p += 10;
                q += encode_utf8(q, c);
            } else {
                *q++ = *p++;
            }
        } else if (p[0] == '\\') {
            *q++ = *p++;
            *q++ = *p++;
        } else {
            *q++ = *p++;
        }
    }

    *q = '\0';
}

Token* Tokenizer::tokenize(const QString& filePath)
{
    const QByteArray path = filePath.toUtf8();
    QByteArray p = read_file(path.constData());
    if (p.isEmpty())
        return 0;

    // UTF-8 texts may start with a 3-byte "BOM" marker sequence.
    // If exists, just skip them because they are useless bytes.
    // (It is actually not recommended to add BOM markers to UTF-8
    // texts, but it's not uncommon particularly on Windows.)
    if (!memcmp(p.constData(), "\xef\xbb\xbf", 3))
    {
        p[0] = ' ';
        p[1] = ' ';
        p[2] = ' ';
    }

    canonicalize_newline(p.data());
    remove_backslash_newline(p.data());
    convert_universal_chars(p.data());

    // Save the filename for assembler .file directive.
    File *file = File::new_file(path, File::input_files.size() + 1, p);

    // Save the filename for assembler .file directive.
    File::input_files.append(file);

    return file->tokenize();
}

QByteArray Tokenizer::format(const char* fmt,...)
{
    va_list ap;
    va_start(ap, fmt);
    QString buf;
    buf.vsprintf(fmt,ap);
    va_end(ap);
    return buf.toUtf8();
}

Macro*Tokenizer::add_macro(const char* name, bool is_objlike, Token* body)
{
    Macro *m = new Macro();
    m->name = name;
    m->is_objlike = is_objlike;
    m->body = body;
    macros.insert(name, m);
    return m;
}

Macro*Tokenizer::find_macro(Token* tok)
{
    if (tok->kind != Token::IDENT)
        return 0;
    return macros.value( QByteArray(tok->loc, tok->len));
}

void Tokenizer::undef_macro(const char* name)
{
    macros.remove(name);
}

void Tokenizer::define_macro(const char* name, const char* buf)
{
    Token *tok = File::new_file("<built-in>", 1, buf)->tokenize();
    add_macro(name, true, tok);
}

void Tokenizer::define_macro(const QByteArray& buf)
{
    QByteArrayList parts = buf.split('=');
    switch( parts.size() )
    {
    case 2:
        define_macro(parts.first().constData(), parts.last().constData());
        break;
    case 1:
        if( parts.first().contains('(') )
        {
            Token *tok = File::new_file("<built-in>", 1, parts.first())->tokenize();
            if( tok )
                tok->read_macro_definition(&tok);
       }else
            define_macro(parts.first(), "1");
        break;
    default:
        error("invalid define: %s\n",buf.constData());
        break;
    }
}

// Double-quote a given string and returns it.
static QByteArray quote_string(const char *str) {
    int bufsize = 3;
    for (int i = 0; str[i]; i++) {
        if (str[i] == '\\' || str[i] == '"')
            bufsize++;
        bufsize++;
    }

    QByteArray buf;
    buf.resize(bufsize);
    char *p = buf.data();
    *p++ = '"';
    for (int i = 0; str[i]; i++) {
        if (str[i] == '\\' || str[i] == '"')
            *p++ = '\\';
        *p++ = str[i];
    }
    *p++ = '"';
    *p++ = '\0';
    return buf;
}

Token* Token::new_str_token(const char* str) const
{
    return File::new_file(file->name, file->file_no, quote_string(str))->tokenize();
}

Token*Token::copy_line(Token** rest) const
{
    // Copy all tokens until the next newline, terminate them with
    // an EOF token and then returns them. This function is used to
    // create a new list of tokens for `#if` arguments.
    Token *tok = const_cast<Token*>(this);
    Token head;
    Token *cur = &head;

    for (; !tok->at_bol && tok->kind != Token::_EOF; tok = tok->next)
      cur = cur->next = tok->copy_token();

    cur->next = tok->new_eof();
    *rest = tok;
    return head.next;
}

Token*Token::new_num_token(int val) const
{
    const QByteArray buf = Tokenizer::format("%d\n", val);
    return File::new_file(file->name, file->file_no, buf)->tokenize();
}

Token*Token::read_const_expr(Token** rest) const
{
    Token *tok = const_cast<Token*>(this);
    tok = tok->copy_line(rest);

    Token head;
    Token *cur = &head;

    while (tok->kind != _EOF) {
        // "defined(foo)" or "defined foo" becomes "1" if macro "foo"
        // is defined. Otherwise "0".
        if (tok->equal("defined")) {
            Token *start = tok;
            bool has_paren = tok->next->consume(&tok, "(");

            if (tok->kind != IDENT)
                Tokenizer::error_tok(start, "macro name must be an identifier");
            Macro *m = Tokenizer::find_macro(tok);
            tok = tok->next;

            if (has_paren)
                tok = tok->skip(")");

            cur = cur->next = start->new_num_token(m ? 1 : 0 );
            continue;
        }

        cur = cur->next = tok;
        tok = tok->next;
    }

    cur->next = tok;
    return head.next;
}

void Token::read_macro_definition(Token** rest)
{
    Token* tok = this;
    if (tok->kind != IDENT)
        Tokenizer::error_tok(tok, "macro name must be an identifier");
    QByteArray name = tok->txt;
    tok = tok->next;

    if (!tok->has_space && tok->equal("(")) {
        // Function-like macro
        QByteArray va_args_name;
        Macro::Param *params = Macro::read_macro_params(&tok, tok->next,va_args_name);

        Macro *m = Tokenizer::add_macro(name, false, tok->copy_line(rest));
        m->params = params;
        m->va_args_name = va_args_name;
    } else {
        // Object-like macro
        Tokenizer::add_macro(name, true, tok->copy_line(rest));
    }
}

QByteArray Token::join_tokens(Token* end) const
{
    Token* tok = const_cast<Token*>(this);
    // Compute the length of the resulting token.
    int len = 1;
    for (Token *t = tok; t != end && t->kind != _EOF; t = t->next) {
        if (t != tok && t->has_space)
            len++;
        len += t->len;
    }

    QByteArray buf;
    buf.resize(len);

    // Copy token texts.
    int pos = 0;
    for (Token *t = tok; t != end && t->kind != _EOF; t = t->next) {
        if (t != tok && t->has_space)
            buf[pos++] = ' ';
        strncpy(buf.data() + pos, t->loc, t->len);
        pos += t->len;
    }
    buf[pos] = '\0';
    return buf;
}

// Concatenates all tokens in `arg` and returns a new string token.
// This function is used for the stringizing operator (#).
Token*Token::stringize(Token* hash) const
{
    // Create a new string token. We need to set some value to its
    // source location for error reporting function, so we use a macro
    // name token as a template.
    return hash->new_str_token(join_tokens(0));
}

Token*Token::paste(Token* rhs) const
{
    // Concatenate two tokens to create a new token.
    Token* lhs = const_cast<Token*>(this);
    // Paste the two tokens.
    const QByteArray buf = Tokenizer::format("%.*s%.*s", lhs->len, lhs->loc, rhs->len, rhs->loc);

    // Tokenize the resulting string.
    Token *tok = File::new_file(lhs->file->name, lhs->file->file_no, buf)->tokenize();
    if (tok->next->kind != _EOF)
        Tokenizer::error_tok(lhs, "pasting forms '%s', an invalid token", buf.constData());
    return tok;
}

Token*Token::add_hideset(Hideset* hs) const
{
    Token* tok = const_cast<Token*>(this);

    Token head;
    Token *cur = &head;

    for (; tok; tok = tok->next) {
        Token *t = tok->copy_token();
        t->hideset = t->hideset->hideset_union(hs);
        cur = cur->next = t;
    }
    return head.next;
}

Macro::Macro():is_objlike(false),body(0),handler(0),params(0)
{

}

Macro::Param * Macro::read_macro_params(Token** rest, Token* tok, QByteArray& va_args_name)
{
    Macro::Param head;
    Macro::Param *cur = &head;

    while (!tok->equal(")")) {
        if (cur != &head)
            tok = tok->skip(",");

        if (tok->equal("...")) {
            va_args_name = "__VA_ARGS__";
            *rest = tok->next->skip(")");
            return head.next;
        }

        if (tok->kind != Token::IDENT)
            Tokenizer::error_tok(tok, "expected an identifier");

        if (tok->next->equal("...")) {
            va_args_name = tok->txt;
            *rest = tok->next->next->skip(")");
            return head.next;
        }

        Macro::Param *m = new Macro::Param();
        m->name = tok->txt;
        cur = cur->next = m;
        tok = tok->next;
    }

    *rest = tok->next;
    return head.next;
}

Hideset*Hideset::hideset_union(Hideset* hs2) const
{
    Hideset* hs1 = const_cast<Hideset*>(this);
    Hideset head;
    Hideset *cur = &head;

    for (; hs1; hs1 = hs1->next)
        cur = cur->next = new Hideset(hs1->name);
    cur->next = hs2;
    return head.next;
}

bool Hideset::hideset_contains(const QByteArray& name) const
{
    Hideset* hs = const_cast<Hideset*>(this);
    for (; hs; hs = hs->next)
        if (hs->name == name)
            return true;
    return false;
}

Hideset*Hideset::hideset_intersection(Hideset* hs2) const
{
    Hideset* hs1 = const_cast<Hideset*>(this);
    Hideset head;
    Hideset *cur = &head;

    for (; hs1; hs1 = hs1->next)
      if (hs2->hideset_contains(hs1->name))
        cur = cur->next = new Hideset(hs1->name);
    return head.next;
}

