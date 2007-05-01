#include <CxxToken.hxx>

#include <CxxParser.hxx>

static CxxToken *yyToken;
static CxxToken *make_character(const char *someText, size_t aLength);
static CxxToken *make_string(const char *someText, size_t aLength);
static CxxToken *make_identifier(const char *someText, size_t aLength);
static void make_line(const char *yyText, size_t yyLeng);
static CxxToken *make_literal_character(const char *someText, size_t aLength);
static CxxToken *make_narrow_character(const char *someText, size_t aLength);
static CxxToken *make_narrow_string(const char *someText, size_t aLength);
static CxxToken *make_number(const char *someText, size_t aLength);
static CxxToken *make_token(size_t aCxxToken);
static CxxToken *make_wide_character(const char *someText, size_t aLength);
static CxxToken *make_wide_string(const char *someText, size_t aLength);

#define LEX_SAVE_LINE(yyText, yyLeng) make_line(yyText, yyLeng);
#define LEX_ASCII_TOKEN(a) do {yyToken = make_token(a); return true;} while (0)
#define LEX_STATIC_TOKEN(a) do {static CxxToken theToken(PARSE_TOKEN(a)); yyToken = &theToken; return true;} while (0)
#define LEX_C_STATIC_TOKEN(a)					 \
    if (c_keywords) { LEX_IDENTIFIER_TOKEN(yytext, yyleng); }	 \
    else { LEX_STATIC_TOKEN(a); } return true;
#define LEX_MS_STATIC_TOKEN(a) LEX_STATIC_TOKEN(a)
// LEX_STATIC_TOKEN(CharacterLiteral)
#define LEX_ESCAPED_TOKEN(yyText, yyLeng)	do {yyToken = make_literal_character(yytext, yyleng); return true; } while (0)
// LEX_STATIC_TOKEN(CharacterLiteral)
#define LEX_CHARACTER_TOKEN(yyText, yyLeng)	do {yyToken = make_character(yyText, yyLeng); return true; } while (0)
//  LEX_STATIC_TOKEN(StringLiteral)
#define LEX_STRING_TOKEN(yyText, yyLeng)	do {yyToken = make_string(yyText, yyLeng); return true; } while (0)
// #define LEX_IDENTIFIER_TOKEN(yyText, yyLeng) // LEX_STATIC_TOKEN(Identifier)
#define LEX_IDENTIFIER_TOKEN(yyText, yyLeng)	do {yyToken = make_identifier(yyText, yyLeng); return true; } while (0)
// LEX_STATIC_TOKEN(IntegerLiteral)
#define LEX_NUMBER_TOKEN(yyText, yyLeng)	do {yyToken = make_number(yyText, yyLeng); return true;	} while (0)
