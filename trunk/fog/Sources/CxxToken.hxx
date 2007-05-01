#ifndef CXXTOKEN_HXX
#define CXXTOKEN_HXX

#include <iostream>
#include <stdlib.h>

#define YYSTYPE CxxTokenType
#define YY_parse_STYPE CxxTokenType
#define YACC_BANG_TYPE size_t
#define YACC_MARK_TYPE size_t

#define YACC_BANG() push_bang()
#define YACC_UNBANG(bangValue, msg) pop_bang (bangValue); yyerrok; yyclearin; yyerror (msg);

#define ERRMSG(a) do { std::cout << "ERROR -- " << a << std::endl; increment_error_count(); } while (0)

#ifdef NEEDS_BOOL
enum bool { false, true };
#endif

#ifdef BISON_PP_CLASS
#define PARSE_TOKEN(a) BISON_PP_CLASS::a
#else
#define PARSE_TOKEN(a) a
#endif

extern size_t line_number;
extern bool c_keywords;
extern bool echo_line_numbers;
extern bool echo_line_text;
extern void increment_error_count();
extern int tokenMarkDepth;

#include <memory.h>
extern char * yytext;
extern int yyleng;

// This is just gross.  If you change the lexer, you have to reinsert this.

# define        ARROW   257
# define        ARROW_STAR      258
# define        DEC     259
# define        EQ      260
# define        GE      261
# define        INC     262
# define        LE      263
# define        LOG_AND 264
# define        LOG_OR  265
# define        NE      266
# define        SHL     267
# define        SHR     268
# define        ASS_ADD 269
# define        ASS_AND 270
# define        ASS_DIV 271
# define        ASS_MOD 272
# define        ASS_MUL 273
# define        ASS_OR  274
# define        ASS_SHL 275
# define        ASS_SHR 276
# define        ASS_SUB 277
# define        ASS_XOR 278
# define        DOT_STAR        279
# define        ELLIPSIS        280
# define        SCOPE   281
# define        PRIVATE 282
# define        PROTECTED       283
# define        PUBLIC  284
# define        BOOL    285
# define        CHAR    286
# define        DOUBLE  287
# define        FLOAT   288
# define        INT     289
# define        LONG    290
# define        SHORT   291
# define        SIGNED  292
# define        UNSIGNED        293
# define        VOID    294
# define        WCHAR_T 295
# define        MS_INT64        296
# define        MS_W64  297
# define        CLASS   298
# define        ENUM    299
# define        NAMESPACE       300
# define        STRUCT  301
# define        TYPENAME        302
# define        UNION   303
# define        CONST   304
# define        VOLATILE        305
# define        AUTO    306
# define        EXPLICIT        307
# define        EXPORT  308
# define        EXTERN  309
# define        FRIEND  310
# define        INLINE  311
# define        MUTABLE 312
# define        REGISTER        313
# define        STATIC  314
# define        TEMPLATE        315
# define        TYPEDEF 316
# define        USING   317
# define        VIRTUAL 318
# define        MS_INLINE1      319
# define        MS_INLINE       320
# define        MS_CDECL        321
# define        MS_STDCALL      322
# define        MS_FORCEINLINE  323
# define        MS_DECLSPEC     324
# define        MS_ASM  325
# define        ASM     326
# define        BREAK   327
# define        CASE    328
# define        CATCH   329
# define        CONST_CAST      330
# define        CONTINUE        331
# define        DEFAULT 332
# define        DELETE  333
# define        DO      334
# define        DYNAMIC_CAST    335
# define        ELSE    336
# define        FALSE   337
# define        FOR     338
# define        GOTO    339
# define        IF      340
# define        NEW     341
# define        OPERATOR        342
# define        REINTERPRET_CAST        343
# define        RETURN  344
# define        SIZEOF  345
# define        STATIC_CAST     346
# define        SWITCH  347
# define        THIS    348
# define        THROW   349
# define        TRUE    350
# define        TRY     351
# define        TYPEID  352
# define        WHILE   353
# define        CharacterLiteral        354
# define        FloatingLiteral 355
# define        Identifier      356
# define        IntegerLiteral  357
# define        NumberLiteral   358
# define        StringLiteral   359
# define        SHIFT_THERE     360
# define        REDUCE_HERE_MOSTLY      361

class CxxToken
{
  int _value;

 public:
  char const * token_type;

 private:
  CxxToken (const CxxToken&);
  CxxToken& operator= (const CxxToken&);

 public:
  CxxToken ()
      : _value (0), token_type ("Unknown")
  {
  }

  CxxToken (int tokenValue)
      : _value (tokenValue),
        token_type (
            tokenValue == ARROW          ? "token/->"
            : tokenValue == MS_INT64     ? "token/__int64"
            : tokenValue == MS_W64       ? "token/__w64"
            : tokenValue == MS_INLINE1   ? "token/_inline"
            : tokenValue == MS_INLINE    ? "token/__inline"
            : tokenValue == MS_FORCEINLINE ? "token/__forceinline"
            : tokenValue == MS_CDECL     ? "token/__cdecl"
            : tokenValue == MS_STDCALL   ? "token/__stdcall"
            : tokenValue == MS_DECLSPEC  ? "token/__declspec"
            : tokenValue == MS_ASM       ? "token/__asm"
            : tokenValue == ARROW_STAR   ? "token/->*"
            : tokenValue == DEC          ? "token/--"
            : tokenValue == EQ           ? "token/=="
            : tokenValue == GE           ? "token/>="
            : tokenValue == INC          ? "token/++"
            : tokenValue == LE           ? "token/<="
            : tokenValue == LOG_AND      ? "token/&"
            : tokenValue == LOG_OR       ? "token/\\|"
            : tokenValue == NE           ? "token/!="
            : tokenValue == SHL          ? "token/<<"
            : tokenValue == SHR          ? "token/>>"
            : tokenValue == ASS_ADD      ? "token/+="
            : tokenValue == ASS_AND      ? "token/&="
            : tokenValue == ASS_DIV      ? "token//="
            : tokenValue == ASS_MOD      ? "token/%="
            : tokenValue == ASS_MUL      ? "token/*="
            : tokenValue == ASS_OR       ? "token/\\|="
            : tokenValue == ASS_SHL      ? "token/<<="
            : tokenValue == ASS_SHR      ? "token/>>="
            : tokenValue == ASS_SUB      ? "token/-="
            : tokenValue == ASS_XOR      ? "token/^="
            : tokenValue == DOT_STAR     ? "token/.*"
            : tokenValue == ELLIPSIS     ? "token/..."
            : tokenValue == SCOPE        ? "token/::"
            : tokenValue == PRIVATE      ? "token/private"
            : tokenValue == PROTECTED    ? "token/protected"
            : tokenValue == PUBLIC       ? "token/public"
            : tokenValue == BOOL         ? "token/bool"
            : tokenValue == CHAR         ? "token/char"
            : tokenValue == DOUBLE       ? "token/double"
            : tokenValue == FLOAT        ? "token/float"
            : tokenValue == INT          ? "token/int"
            : tokenValue == LONG         ? "token/long"
            : tokenValue == SHORT        ? "token/short"
            : tokenValue == SIGNED       ? "token/signed"
            : tokenValue == UNSIGNED     ? "token/unsigned"
            : tokenValue == VOID         ? "token/void"
            : tokenValue == WCHAR_T      ? "token/wchar_t"
            : tokenValue == CLASS        ? "token/class"
            : tokenValue == ENUM         ? "token/enum"
            : tokenValue == NAMESPACE    ? "token/namespace"
            : tokenValue == STRUCT       ? "token/struct"
            : tokenValue == TYPENAME     ? "token/typename"
            : tokenValue == UNION        ? "token/union"
            : tokenValue == CONST        ? "token/const"
            : tokenValue == VOLATILE     ? "token/volatile"
            : tokenValue == AUTO         ? "token/auto"
            : tokenValue == EXPLICIT     ? "token/explicit"
            : tokenValue == EXPORT       ? "token/export"
            : tokenValue == EXTERN       ? "token/extern"
            : tokenValue == FRIEND       ? "token/friend"
            : tokenValue == INLINE       ? "token/inline"
            : tokenValue == MUTABLE      ? "token/mutable"
            : tokenValue == REGISTER     ? "token/register"
            : tokenValue == STATIC       ? "token/static"
            : tokenValue == TEMPLATE     ? "token/template"
            : tokenValue == TYPEDEF      ? "token/typedef"
            : tokenValue == USING        ? "token/using"
            : tokenValue == VIRTUAL      ? "token/virtual"
            : tokenValue == ASM          ? "token/asm"
            : tokenValue == BREAK        ? "token/break"
            : tokenValue == CASE         ? "token/case"
            : tokenValue == CATCH        ? "token/catch"
            : tokenValue == CONST_CAST   ? "token/const_cast"
            : tokenValue == CONTINUE     ? "token/continue"
            : tokenValue == DEFAULT      ? "token/default"
            : tokenValue == DELETE       ? "token/delete"
            : tokenValue == DO           ? "token/do"
            : tokenValue == DYNAMIC_CAST ? "token/dynamic_cast"
            : tokenValue == ELSE         ? "token/else"
            : tokenValue == FALSE        ? "token/false"
            : tokenValue == FOR          ? "token/for"
            : tokenValue == GOTO         ? "token/goto"
            : tokenValue == IF           ? "token/if"
            : tokenValue == NEW          ? "token/new"
            : tokenValue == OPERATOR     ? "token/operator"
            : tokenValue == REINTERPRET_CAST     ? "token/reinterpret_cast"
            : tokenValue == RETURN       ? "token/return"
            : tokenValue == SIZEOF       ? "token/sizeof"
            : tokenValue == STATIC_CAST  ? "token/static_cast"
            : tokenValue == SWITCH       ? "token/switch"
            : tokenValue == THIS         ? "token/this"
            : tokenValue == THROW        ? "token/throw"
            : tokenValue == TRUE         ? "token/true"
            : tokenValue == TRY          ? "token/try"
            : tokenValue == TYPEID       ? "token/typeid"
            : tokenValue == WHILE        ? "token/while"
            : tokenValue == CharacterLiteral      ? "token/character-literal"
            : tokenValue == FloatingLiteral       ? "token/floating-literal"
            : tokenValue == Identifier            ? "token/identifier"
            : tokenValue == IntegerLiteral        ? "token/integer-literal"
            : tokenValue == NumberLiteral         ? "token/number-literal"
            : tokenValue == StringLiteral         ? "token/string-literal"
            : 0)
  {
    if (token_type == 0) {
	char * tok = new char [yyleng+1];
        memcpy (tok, yytext, yyleng);
	tok [yyleng] = 0;
        token_type = tok;
        }
  }

  CxxToken (char const * token_type)
      : _value (0), token_type (token_type)
  {
  }

  CxxToken (int tokenValue, char const * token_type)
      : _value (tokenValue), token_type (token_type)
  {
  }

  virtual ~CxxToken() {}
  int value() const { return _value; }

  virtual void emit (void);
};

class CxxTokenList : public CxxToken
{
  CxxTokenList * more_tokens;
  CxxToken * this_token;

 public:
  CxxTokenList (char const * token_type, CxxTokenList * more_tokens, CxxToken * this_token)
      : CxxToken (token_type), more_tokens (more_tokens), this_token (this_token) {}

  virtual void emit (void);
  virtual void emit1 (void);
};

//
// LITERAL TOKENS
//
class CxxCharacterLiteral : public CxxToken
{
 public:
  CxxCharacterLiteral ()
      : CxxToken ("character-literal") {}
};

class CxxFloatingLiteral : public CxxToken
{
 public:
  CxxFloatingLiteral ()
      : CxxToken ("floating-literal") {}
};

class CxxIntegerLiteral : public CxxToken
{
 public:
  CxxIntegerLiteral ()
      : CxxToken ("integer-literal") {}
};

class CxxStringLiteral;

class CxxStringList : public CxxToken
{
  CxxStringLiteral * anElement;
  CxxStringList * aList;

 protected:
  CxxStringList (char const * token_type)
      : CxxToken (token_type), anElement(0), aList(0) {}

 public:
  CxxStringList (CxxStringLiteral *anElement, CxxStringList *aList)
      : CxxToken ("string-list"), anElement (anElement), aList (aList) {}

  virtual void emit();
};

class CxxStringLiteral : public CxxStringList
{
 public:
  CxxStringLiteral ()
      : CxxStringList ("string-literal") {}
};

/// Simple tokens
/// These probably need to be fleshed out.

class CxxClass : public CxxToken
{
 protected:
  CxxClass (char const * token_type)
      : CxxToken (token_type) {}
};

class CxxLine : public CxxToken
{
 public:
  CxxLine ()
      : CxxToken ("line") {}

  virtual void emit();
};

class CxxNamespace : public CxxToken
{
 public:
  CxxNamespace ()
      : CxxToken ("namespace") {}
};

class CxxTypeId : public CxxToken
{
 public:
  CxxTypeId ()
      : CxxToken ("type-id") {}
};

class CxxTypeIdList : public CxxTokenList
{
 public:
  CxxTypeIdList (CxxTypeIdList * aList, CxxTypeId * anElement)
      : CxxTokenList ("type-id-list", aList, anElement) {}
};

/// Slightly more complicated tokens

class CxxLinedToken : public CxxToken
{
  CxxToken * aToken;
  CxxLine * aLine;

 public:
  CxxLinedToken (CxxToken *aToken, CxxLine *aLine)
      : CxxToken ("lined-token"), aToken (aToken), aLine (aLine) {}

  virtual void emit();
};

// The BIGGIES
// declarator, statement, declaration, expression

class CxxDeclarator : public CxxToken
{
 protected:
  CxxDeclarator (char const * token_type)
      : CxxToken (token_type) {}
};

class CxxStatement : public CxxToken
{
 protected:
  CxxStatement (char const * token_type)
      : CxxToken (0, token_type) {}
};

class CxxStatementList : public CxxStatement
{
 public:
  CxxStatementList * aList;
  CxxStatement * anElement;

  CxxStatementList (char const * token_type, CxxStatementList * aList, CxxStatement * anElement)
      : CxxStatement (token_type), aList (aList), anElement (anElement) {}

 public:
  CxxStatementList (CxxStatementList * aList, CxxStatement * anElement)
      : CxxStatement ("statement-list"), aList (aList), anElement (anElement) {}

  virtual void emit (void);
  void emit1 (void);
};

class CxxAtomicStatement : public CxxStatement
{
 protected:
  CxxAtomicStatement (char const * token_type)
      : CxxStatement (token_type) {}
 public:
  virtual void emit();
};

class CxxDeclaration : public CxxStatement
{
 protected:
  CxxDeclaration (char const * token_type)
      : CxxStatement (token_type) {}
};

class CxxDeclarationList : public CxxStatementList
{
 public:
  CxxDeclarationList (CxxDeclarationList* aList, CxxDeclaration *anElement)
      : CxxStatementList ("declaration-list", aList, anElement)
  {
    if (anElement == NULL) {
        std::cout << "Appending null declaration." << std::endl;
        }
  }

  CxxDeclarationList (char const * token_type, CxxDeclarationList* aList, CxxDeclaration *anElement)
      : CxxStatementList (token_type, aList, anElement)
  {
    if (anElement == NULL) {
        std::cout << "Appending null " << token_type << " declaration." << std::endl;
        }
  }

  virtual void emit (void);
};

class CxxFunctionDeclarationList : public CxxDeclarationList
{
 public:
  CxxFunctionDeclarationList (CxxFunctionDeclarationList *aList, CxxDeclaration *anElement)
      : CxxDeclarationList ("function-declaration-list", aList, anElement) {}
};

class CxxMemberDeclarationList : public CxxDeclarationList
{
 public:
  CxxMemberDeclarationList (int alist, int adeclaration)
      : CxxDeclarationList ("CxxMemberDeclarations", 0, 0) {}
  CxxMemberDeclarationList (CxxDeclarationList *aList, CxxDeclaration *aDeclaration)
      : CxxDeclarationList ("CxxMemberDeclarations", aList, aDeclaration) {}
  CxxMemberDeclarationList (CxxMemberDeclarationList *aList, CxxDeclaration *aDeclaration)
      : CxxDeclarationList ("CxxMemberDeclarations", aList, aDeclaration) {}
};

class CxxExpression : public CxxStatement
{
 protected:
  CxxExpression (char const * token_type)
      : CxxStatement (token_type) {}
};

class CxxBinaryExpression : public CxxExpression
{
 public:
  const char * operator_name;
  CxxExpression * left_operand;
  CxxExpression * right_operand;

  CxxBinaryExpression (const char * token_type,
                       const char * operator_name,
                       CxxExpression * left_operand,
                       CxxExpression * right_operand)
      : CxxExpression (token_type), operator_name (operator_name),
        left_operand (left_operand), right_operand (right_operand) {}
  virtual void emit();
};

class CxxExpressionList : public CxxExpression
{
 public:
  CxxExpressionList *aList;
  CxxExpression *anElement;

  CxxExpressionList (CxxExpressionList *aList, CxxExpression *anElement)
      : CxxExpression ("expression-list"), aList (aList), anElement (anElement) {}

  virtual void emit();
  virtual void emit1();
};

class CxxExceptionSpecification : public CxxToken
{
  CxxExpressionList * typeIds;

 public:
  CxxExceptionSpecification (CxxExpressionList *typeIds)
      : CxxToken ("exception-specification"), typeIds (typeIds) {}

  virtual void emit ();
};

class CxxName : public CxxExpression
{
 protected:
  CxxName (char const * token_type)
      : CxxExpression (token_type) {}

 public:
  virtual void emit (void);
};

class CxxBuiltInId : public CxxName
{
  CxxBuiltInId *aName;

 protected:
  CxxBuiltInId (char const * token_type)
      : CxxName (token_type) {}

 public:
  CxxBuiltInId (CxxBuiltInId *aName)
      : CxxName ("built-in-id"), aName (aName) {}
  virtual void emit();
};

class CxxBuiltInIdList : public CxxBuiltInId
{
 public:
  CxxBuiltInIdList (CxxBuiltInId *anExpr, CxxBuiltInId *anElement)
      : CxxBuiltInId ("built-in-id-list") {}
};

class CxxBuiltInIdId : public CxxName
{
  CxxBuiltInId *aName;
 public:
  CxxBuiltInIdId (CxxBuiltInId *aName)
      : CxxName ("built-in-id-id"), aName (aName) {}
};

class CxxBuiltInName : public CxxName
{
  CxxName * aName;
  CxxBuiltInId * anElement;

 public:
  CxxBuiltInName (char const * token_type)
      : CxxName (token_type) {}
  CxxBuiltInName (CxxName *aName, CxxBuiltInId *anElement)
      : CxxName ("built-in-name"), aName (aName), anElement (anElement) {}
  virtual void emit();
};


class CxxBaseSpecifier : public CxxToken
{
  CxxName * aName;

 public:
  CxxBaseSpecifier (char const * token_type)
      : CxxToken (token_type), aName (0) {}

  CxxBaseSpecifier (char const * token_type, CxxName * aName)
      : CxxToken (token_type), aName (aName) {}

  CxxBaseSpecifier (CxxName *aName)
      : CxxToken ("base-specifier"), aName (aName) {}

  virtual void emit ();
};

class CxxBaseSpecifierList : public CxxTokenList
{
 public:
  CxxBaseSpecifierList (CxxBaseSpecifierList *aList, CxxBaseSpecifier *anElement)
      : CxxTokenList ("base-specifier-list", aList, anElement) {}
};

class CxxVirtualBaseSpecifier : public CxxBaseSpecifier
{
 public:
  CxxVirtualBaseSpecifier (CxxBaseSpecifier * baseSpecifier)
      : CxxBaseSpecifier ("VirtualBaseSpecifier") {}
};

class CxxEnumerator : public CxxToken
{
  CxxName *aName;
  CxxExpression *anExpr;

 public:
  CxxEnumerator (CxxName *aName, CxxExpression *anExpr)
      : CxxToken ("enumerator"), aName (aName), anExpr (anExpr) {}
  virtual void emit ();
};

class CxxEnumeratorList : public CxxTokenList
{
 public:
  CxxEnumeratorList (CxxEnumeratorList *aList, CxxEnumerator *anElement)
      : CxxTokenList ("enumerator-list", aList, anElement) {}
};

class CxxEnumSpecifierId : public CxxName
{
  CxxName *aName;
  CxxEnumeratorList *aList;

 public:
  CxxEnumSpecifierId (CxxName *aName, CxxEnumeratorList *aList)
      : CxxName ("enum-specifier-id"), aName (aName), aList (aList) {}

  virtual void emit();
};

class CxxKeyword : public CxxName
{
 protected:
  CxxKeyword (char const * token_type)
      : CxxName (token_type) {}
};

class CxxNameExpression : public CxxName
{
  CxxName * aName;

 public:
  CxxNameExpression (CxxName *aName)
      : CxxName ("expression/name"), aName (aName) {}

  virtual void emit();
};

class CxxParameter : public CxxExpression
{
 protected:
  CxxParameter (char const * token_type)
      : CxxExpression (token_type) {}
};

class CxxParameterList : public CxxExpression
{
  CxxParameterList *aList;
  CxxParameter *anElement;

 public:
  CxxParameterList (CxxParameterList *aList, CxxParameter *anElement)
      : CxxExpression ("parameter-list"), aList (aList), anElement (anElement) {}

  virtual void emit();
  virtual void emit1();
};

class CxxTemplateArgument : public CxxToken
{
  CxxParameter * aParameter;
 public:
  CxxTemplateArgument (CxxParameter *aParameter)
      : CxxToken ("template-argument"), aParameter (aParameter) {}
  virtual void emit ();
};

class CxxTemplateArgumentList : public CxxTokenList
{
 public:
  CxxTemplateArgumentList (CxxTemplateArgumentList *aList, CxxTemplateArgument *anElement)
      : CxxTokenList ("template-argument-list", aList, anElement) {}
};

class CxxTemplateParameter : public CxxToken
{
  CxxParameter * aParameter;

 protected:
  CxxTemplateParameter (char const * token_type)
      : CxxToken (token_type), aParameter(0) {}

 public:
  CxxTemplateParameter (CxxParameter * aParameter)
      : CxxToken ("template-parameter"), aParameter (aParameter) {}

  virtual void emit();
};

class CxxTemplateParameterList : public CxxTokenList
{
 public:
  CxxTemplateParameterList (CxxTemplateParameterList *templateParameterList, CxxTemplateParameter *aName)
      : CxxTokenList ("template-parameter-list", templateParameterList, aName) {}
};

class CxxType1ParameterList : public CxxTokenList
{
 public:
  CxxType1ParameterList (CxxType1ParameterList *aList, CxxParameterList *someParameterList)
      : CxxTokenList ("type1-parameter-list", aList, someParameterList) {}
};

class CxxExceptionDeclaration : public CxxToken
{
  CxxParameter * aParameter;
 public:
  CxxExceptionDeclaration (CxxParameter *aParameter)
      : CxxToken ("exception-declaration"), aParameter (aParameter) {}

  virtual void emit();
};

class CxxHandler : public CxxToken
{
  CxxExceptionDeclaration * exceptionDeclaration;
  CxxStatement * aStatement;

 public:
  CxxHandler (CxxExceptionDeclaration *exceptionDeclaration, CxxStatement *aStatement)
      : CxxToken ("handler"), exceptionDeclaration (exceptionDeclaration), aStatement (aStatement) {}

  virtual void emit();
};

class CxxHandlerList : public CxxTokenList
{
 public:
  CxxHandlerList (CxxHandlerList *aList, CxxHandler *anElement)
      : CxxTokenList ("handler-list", aList, anElement) {}
};

class CxxUnaryExpression : public CxxExpression
{
  const char * operator_name;
  CxxExpression * operand;

 protected:
  CxxUnaryExpression (const char * token_type, const char * operator_name, CxxExpression * operand)
      : CxxExpression (token_type), operator_name (operator_name), operand (operand) {}
  virtual void emit();
};

class CxxCondition : public CxxExpression
{
  CxxParameterList * aList;

 public:
  CxxCondition (CxxParameterList *aList)
      : CxxExpression ("condition"), aList (aList) {}

  virtual void emit();
};

class CxxFunctionBody : public CxxStatement
{
 protected:
  CxxFunctionBody (char const * token_type)
      : CxxStatement (token_type) {}
};

class CxxUtility : public CxxToken
{
 public:
  CxxUtility (char const * token_type)
      : CxxToken (token_type) {}
//   CxxUtility ()
//       : CxxToken ("utility") {}
//   CxxUtility (CxxUtility * aUtility)
//       : CxxToken ("utility") {}
};

//
// Keywords
//

class CxxAccessSpecifier : public CxxKeyword
{
 public:
  CxxAccessSpecifier ()
      : CxxKeyword ("access-specifier") {}
};

class CxxClassKey : public CxxKeyword
{
 public:
  CxxClassKey ()
      : CxxKeyword ("class-key") {}
};

class CxxElaboratedTypeSpecifier : public CxxName
{
  CxxClassKey *classKey;
  CxxName *aName;
 public:
  CxxElaboratedTypeSpecifier (char const * token_type)
      : CxxName (token_type) {}
  CxxElaboratedTypeSpecifier (CxxClassKey *classKey, CxxName *aName)
      : CxxName ("elaborated-type-specifier"), classKey (classKey), aName (aName) {}
  virtual void emit();
};

class CxxClassMemberList : public CxxKeyword
{
  CxxClass * aClass;
  CxxDeclarationList * DeclarationList;

 public:
  CxxClassMemberList (CxxClass *aClass, CxxDeclarationList *DeclarationList)
      : CxxKeyword ("class-member-list"), aClass (aClass), DeclarationList (DeclarationList) {}

  virtual void emit();
};

class CxxDeclSpecifierId : public CxxKeyword
{
 protected:
  CxxDeclSpecifierId (char const * token_type)
      : CxxKeyword (token_type) {}
};

class CxxDeclSpecifierName : public CxxName
{
  CxxName *aName;
  CxxDeclSpecifierId *declSpecifier;

 public:
  CxxDeclSpecifierName (CxxName *aName, CxxDeclSpecifierId *declSpecifier)
      : CxxName ("decl-specifier-name"), aName(aName), declSpecifier(declSpecifier) {}
  virtual void emit();
};

class CxxAccessSpecifierId : public CxxDeclSpecifierId
{
  CxxAccessSpecifier * aName;

 public:
  CxxAccessSpecifierId (CxxAccessSpecifier * aName)
      : CxxDeclSpecifierId ("access-specifier-id"), aName (aName) {}
  virtual void emit();
};

class CxxDeclSpecifier : public CxxDeclSpecifierId
{
 public:
  CxxDeclSpecifier ()
      : CxxDeclSpecifierId ("decl-specifier") {}
};

class CxxCvQualifier : public CxxDeclSpecifierId
{
 protected:
  CxxCvQualifier (const char * token_type)
      : CxxDeclSpecifierId (token_type) {}
};

class CxxCvQualifierList : public CxxCvQualifier
{
  CxxCvQualifierList * aList;
  CxxCvQualifier * anElement;

 public:
  CxxCvQualifierList (CxxCvQualifierList *aList, CxxCvQualifier *anElement)
      : CxxCvQualifier ("cv-qualifier-list"), aList (aList), anElement (anElement) {}

  virtual void emit ();
};

class CxxCvDeclSpecifier : public CxxDeclSpecifierId
{
  CxxCvQualifierList * cvQualifierList;
 public:
  CxxCvDeclSpecifier (CxxCvQualifierList *cvQualifierList)
      : CxxDeclSpecifierId ("cv-decl-specifier"), cvQualifierList(cvQualifierList) {}

  virtual void emit();
};

class CxxDeclSpecifierList : public CxxDeclSpecifierId
{
  CxxDeclSpecifierId *aList;
  CxxDeclSpecifierId *anElement;
 public:
  CxxDeclSpecifierList (CxxDeclSpecifierId *aList, CxxDeclSpecifierId *anElement)
      : CxxDeclSpecifierId ("decl-specifier-list"), aList(aList), anElement(anElement) {}

  virtual void emit();
};

class CxxSetTemplateDeclSpecifier : public CxxDeclSpecifierId
{
  CxxDeclSpecifierId *aName;

 public:
  CxxSetTemplateDeclSpecifier (CxxDeclSpecifierId *aName)
      : CxxDeclSpecifierId ("set-template-decl-specifier"), aName(aName) {}

  virtual void emit();
};

//
// NAMES
//

class CxxOperatorAddId : public CxxName
{
 public:
  CxxOperatorAddId ()
      : CxxName ("operator+") {};
};

class CxxOperatorArrowId : public CxxName
{
 public:
  CxxOperatorArrowId ()
      : CxxName ("operator->") {};
};

class CxxOperatorArrowStarId : public CxxName
{
 public:
  CxxOperatorArrowStarId ()
      : CxxName ("operator->*") {};
};

class CxxOperatorAssAddId : public CxxName
{
 public:
  CxxOperatorAssAddId ()
      : CxxName ("operator+=") {};
};

class CxxOperatorAssBitAndId : public CxxName
{
 public:
  CxxOperatorAssBitAndId ()
      : CxxName ("operator&=") {};
};

class CxxOperatorAssBitOrId : public CxxName
{
 public:
  CxxOperatorAssBitOrId ()
      : CxxName ("operator|=") {};
};

class CxxOperatorAssDivId : public CxxName
{
 public:
  CxxOperatorAssDivId ()
      : CxxName ("operator/=") {};
};

class CxxOperatorAssId : public CxxName
{
 public:
  CxxOperatorAssId ()
      : CxxName ("operator=") {};
};

class CxxOperatorAssModId : public CxxName
{
 public:
  CxxOperatorAssModId ()
      : CxxName ("operator%=") {};
};

class CxxOperatorAssMulId : public CxxName
{
 public:
  CxxOperatorAssMulId ()
      : CxxName ("operator*=") {};
};

class CxxOperatorAssShlId : public CxxName
{
 public:
  CxxOperatorAssShlId ()
      : CxxName ("operator<<=") {};
};

class CxxOperatorAssShrId : public CxxName
{
 public:
  CxxOperatorAssShrId ()
      : CxxName ("operator>>=") {};
};

class CxxOperatorAssSubId : public CxxName
{
 public:
  CxxOperatorAssSubId ()
      : CxxName ("operator-=") {};
};

class CxxOperatorAssXorId : public CxxName
{
 public:
  CxxOperatorAssXorId ()
      : CxxName ("operator^=") {};
};

class CxxOperatorBitAndId : public CxxName
{
 public:
  CxxOperatorBitAndId ()
      : CxxName ("operator-bit-and"){};
};

class CxxOperatorBitNotId : public CxxName
{
 public:
  CxxOperatorBitNotId ()
      : CxxName ("operator-bit-not") {};
};

class CxxOperatorBitOrId : public CxxName
{
 public:
  CxxOperatorBitOrId ()
      : CxxName ("operator-bit-or") {};
};

class CxxOperatorCallId : public CxxName
{
 public:
  CxxOperatorCallId ()
      : CxxName ("operator-call") {};
};

class CxxOperatorCommaId : public CxxName
{
 public:
  CxxOperatorCommaId ()
      : CxxName ("operator-comma") {};
};

class CxxOperatorDecId : public CxxName
{
 public:
  CxxOperatorDecId ()
      : CxxName ("operator-decrement") {};
};

class CxxOperatorDeleteId : public CxxName
{
 public:
  CxxOperatorDeleteId ()
      : CxxName ("operator-delete") {};
};

class CxxOperatorDivId : public CxxName
{
 public:
  CxxOperatorDivId ()
      : CxxName ("operator-divide") {};
};

class CxxOperatorEqId : public CxxName
{
 public:
  CxxOperatorEqId ()
      : CxxName ("operator=") {};
};

class CxxOperatorFunctionId : public CxxName
{
 public:
  CxxName * operatorId;

  CxxOperatorFunctionId (CxxName * operatorId)
      : CxxName ("operator-function"), operatorId (operatorId) {};

  virtual void emit ();
};

class CxxOperatorGeId : public CxxName
{
 public:
  CxxOperatorGeId ()
      : CxxName ("operator>=") {};
};

class CxxOperatorGtId : public CxxName
{
 public:
  CxxOperatorGtId ()
      : CxxName ("operator>") {};
};

class CxxOperatorIncId : public CxxName
{
 public:
  CxxOperatorIncId ()
      : CxxName ("operator-increment") {};
};

class CxxOperatorIndexId : public CxxName
{
 public:
  CxxOperatorIndexId ()
      : CxxName ("operator[]") {};
};

class CxxOperatorLeId : public CxxName
{
 public:
  CxxOperatorLeId ()
      : CxxName ("operator<=") {};
};

class CxxOperatorLogAndId : public CxxName
{
 public:
  CxxOperatorLogAndId ()
      : CxxName ("operator-logical-and") {};
};

class CxxOperatorLogNotId : public CxxName
{
 public:
  CxxOperatorLogNotId ()
      : CxxName ("operator-logical-not") {};
};

class CxxOperatorLogOrId : public CxxName
{
 public:
  CxxOperatorLogOrId ()
      : CxxName ("operator-logical-or") {};
};

class CxxOperatorLtId : public CxxName
{
 public:
  CxxOperatorLtId ()
      : CxxName ("operator<") {};
};

class CxxOperatorModId : public CxxName
{
 public:
  CxxOperatorModId ()
      : CxxName ("operator-mod") {};
};

class CxxOperatorMulId : public CxxName
{
 public:
  CxxOperatorMulId ()
      : CxxName ("operator*") {};
};

class CxxOperatorNeId : public CxxName
{
 public:
  CxxOperatorNeId ()
      : CxxName ("operator!=") {};
};

class CxxOperatorNewId : public CxxName
{
 public:
  CxxOperatorNewId ()
      : CxxName ("operator-new") {};
};

class CxxOperatorShlId : public CxxName
{
 public:
  CxxOperatorShlId ()
      : CxxName ("operator<<") {};
};

class CxxOperatorShrId : public CxxName
{
 public:
  CxxOperatorShrId ()
      : CxxName ("operator>>") {};
};

class CxxOperatorSubId : public CxxName
{
 public:
  CxxOperatorSubId ()
      : CxxName ("operator-") {};
};

class CxxOperatorXorId : public CxxName
{
 public:
  CxxOperatorXorId ()
      : CxxName ("operator-xor") {};
};

class CxxSetTemplateId : public CxxName
{
  CxxName * aName;

 public:
  CxxSetTemplateId (CxxName * aName)
      :CxxName ("set-template-id"), aName(aName) {}

  virtual void emit();
};

class CxxSetTemplateName : public CxxName
{
  CxxName * aName;
 public:
  CxxSetTemplateName (CxxName * aName)
      : CxxName ("set-template-name"), aName(aName) {}

  virtual void emit();
};


class CxxSetTemplateScope : public CxxName
{
  CxxName * aName;
 public:
  CxxSetTemplateScope (CxxName * aName)
      : CxxName ("set-template-scope"), aName(aName) {}
  virtual void emit();
};

class CxxConversionFunctionId : public CxxName
{
  CxxExpression *typeId;
 public:
  CxxConversionFunctionId (CxxExpression *typeId)
      : CxxName ("conversion-function-id"), typeId(typeId) {}

  virtual void emit();
};

class CxxDestructorId : public CxxName
{
  CxxName * aName;

 public:
  CxxDestructorId (CxxName *aName)
      : CxxName ("destructor-id"), aName(aName) {}
};

class CxxEpsilon : public CxxName
{
 public:
  CxxEpsilon ()
      : CxxName ("epsilon") {}
};

class CxxIdentifier : public CxxName
{
 public:
  CxxIdentifier ()
      : CxxName ("identifier") {}
};

class CxxPseudoDestructorId : public CxxName
{
  CxxBuiltInId *aScope;
  CxxBuiltInId *aName;
 public:
  CxxPseudoDestructorId (CxxBuiltInId *aScope, CxxBuiltInId *aName)
      : CxxName ("pseudo-destructor-id"), aScope(aScope), aName(aName) {}

  virtual void emit();
};

enum CxxIsTemplate { IS_DEFAULT, IS_TEMPLATE };

class CxxGlobalId : public CxxName
{
  CxxName *nestedId;

 public:
  CxxGlobalId (CxxIsTemplate isTemplate, CxxName *nestedId)
      : CxxName ("global-id"), nestedId (nestedId) {}
};

class CxxNestedId : public CxxName
{
  CxxName *nestingId;
  CxxName *nestedId;

 public:
  CxxNestedId (CxxName *nestingId, CxxName *nestedId)
      : CxxName ("nested-id"), nestingId (nestingId), nestedId (nestedId) {}

  virtual void emit ();
};

class CxxNestedScope : public CxxName
{
  CxxName *nestingId;

 public:
  CxxNestedScope (CxxName *nestingId)
      : CxxName ("nested-scope"), nestingId (nestingId) {}

  virtual void emit();
};

class CxxScopedId : public CxxName
{
  CxxName * globalId;
  CxxName * nestedId;

 public:
  CxxScopedId (CxxName *globalId, CxxName *nestedId)
      :CxxName ("scoped-id"), globalId (globalId), nestedId (nestedId) {}
  virtual void emit();
};

class CxxTypedName : public CxxName
{
  CxxName *frontName;
  CxxName *backName;

 public:
  CxxTypedName (CxxName *frontName, CxxName *backName)
      :CxxName ("typed-name"), frontName (frontName), backName (backName) {}

  virtual void emit();
};

class CxxTypedExpression : public CxxExpression
{
  CxxName *frontName;
  CxxExpression *backName;

 public:
  CxxTypedExpression (CxxName *frontName, CxxExpression *backName)
      : CxxExpression ("expression/typed"), frontName (frontName), backName (backName) {}
  virtual void emit ();

};

class CxxTypeidExpression : public CxxExpression
{
  CxxExpression * aList;
 public:
  CxxTypeidExpression (CxxExpression *aList)
      : CxxExpression ("expression/typeid"), aList(aList) {}
  virtual void emit();
};

//
// DECLARATORS
//

class CxxGlobalDeclarator : public CxxDeclarator
{
  CxxIsTemplate isTemplate;
  CxxDeclarator *aDeclarator;

 public:
  CxxGlobalDeclarator (CxxIsTemplate isTemplate, CxxDeclarator *aDeclarator)
      : CxxDeclarator ("global-declarator") {}
};

class CxxNestedDeclarator : public CxxDeclarator
{
  CxxName *aName;
  CxxDeclarator *aDeclarator;

 public:
  CxxNestedDeclarator (CxxName *aName, CxxDeclarator *aDeclarator)
      : CxxDeclarator ("nested-declarator"), aName (aName), aDeclarator (aDeclarator) {}

  virtual void emit ();
};

class CxxPointerDeclarator : public CxxDeclarator
{
 protected:
  CxxPointerDeclarator (char const * token_type)
      : CxxDeclarator (token_type) {}
 public:
  CxxPointerDeclarator ()
      : CxxDeclarator ("pointer-declarator") {}

  virtual void emit ();
};

class CxxCvDeclarator : public CxxPointerDeclarator
{
  CxxPointerDeclarator *aDeclarator;
  CxxCvQualifierList *cvQualifierList;

 public:
  CxxCvDeclarator (CxxPointerDeclarator *aDeclarator, CxxCvQualifierList *cvQualifierList)
      : CxxPointerDeclarator ("cv-declarator"), aDeclarator (aDeclarator), cvQualifierList (cvQualifierList) {}
  virtual void emit ();
};

class CxxReferenceDeclarator : public CxxDeclarator
{
 public:
  CxxReferenceDeclarator ()
      : CxxDeclarator ("reference-declarator") {};
  virtual void emit ();
};

//
// DECLARATIONS

class CxxAccessibilitySpecifier : public CxxDeclaration
{
  CxxAccessSpecifier *accessSpecifier;

 public:
  CxxAccessibilitySpecifier (CxxAccessSpecifier *accessSpecifier)
      : CxxDeclaration ("accessibility-specifier"), accessSpecifier (accessSpecifier) {}

  virtual void emit();
};

class CxxAsmDefinition : public CxxDeclaration
{
  CxxStringList * aString;
 public:
  CxxAsmDefinition (CxxStringList *aString)
      : CxxDeclaration ("asm-definition"), aString(aString) {}

  virtual void emit();
};

class CxxDeclSpecifierDeclaration : public CxxDeclaration
{
  CxxDeclaration *aDeclaration;
  CxxDeclSpecifierId *declSpecifier;

 public:
  CxxDeclSpecifierDeclaration (CxxDeclaration *aDeclaration, CxxDeclSpecifierId *declSpecifier)
      : CxxDeclaration ("decl-specifier-declaration"), aDeclaration (aDeclaration), declSpecifier (declSpecifier) {}

  virtual void emit ();
};

class CxxExplicitSpecialization : public CxxDeclaration
{
  CxxDeclaration * aDeclaration;
 public:
  CxxExplicitSpecialization (CxxDeclaration *aDeclaration)
      : CxxDeclaration ("explicit-specialization"), aDeclaration(aDeclaration) {}

  virtual void emit();
};

class CxxIncludeDeclaration : public CxxDeclaration
{
  CxxStringList *aString;
  CxxUtility *aUtility;

 public:
  CxxIncludeDeclaration (CxxStringList *aString, CxxUtility *aUtility)
      : CxxDeclaration ("include-declaration"), aString(aString), aUtility(aUtility) {}

  virtual void emit();
};

class CxxLinedDeclaration : public CxxDeclaration
{
  CxxDeclaration * declaration;
  CxxLine * line;
 public:
  CxxLinedDeclaration (CxxDeclaration *aDeclaration, CxxLine *aLine)
      : CxxDeclaration ("lined-declaration"), declaration (aDeclaration), line (aLine)  {}

  virtual void emit();
};

class CxxLinkageSpecification : public CxxDeclaration
{
  CxxName * aName;

 public:
  CxxLinkageSpecification (CxxName *aName)
      : CxxDeclaration ("linkage-specification"), aName (aName) {}

  virtual void emit();
};

class CxxNamespaceAliasDefinition : public CxxDeclaration
{
  CxxName *aName;
  CxxName *forId;
 public:
  CxxNamespaceAliasDefinition (CxxName *aName, CxxName *forId)
      : CxxDeclaration ("namespace-alias-definition"), aName(aName), forId(forId) {}

  virtual void emit();
};

class CxxNameSpaceDeclaration : public CxxDeclaration
{
  CxxName * aName;
 public:
  CxxNameSpaceDeclaration (CxxName *aName)
      : CxxDeclaration ("namespace-declaration"), aName(aName) {}

  virtual void emit();
};

class CxxSetTemplateDeclaration : public CxxDeclaration
{
  CxxDeclaration *aDeclaration;
 public:
  CxxSetTemplateDeclaration (CxxDeclaration *aDeclaration)
      : CxxDeclaration ("set-template-declaration"), aDeclaration (aDeclaration) {}

  virtual void emit();
};

class CxxSimpleDeclaration : public CxxDeclaration
{
  CxxExpression * anExpr;

 public:
  CxxSimpleDeclaration (CxxExpression *anExpr)
      : CxxDeclaration ("simple-declaration"), anExpr (anExpr) {}

  virtual void emit();
};

class CxxTemplateDeclaration : public CxxDeclaration
{
  CxxTemplateParameterList *aList;
  CxxDeclaration *aDeclaration;

 public:
  CxxTemplateDeclaration (CxxTemplateParameterList *aList, CxxDeclaration *aDeclaration)
      : CxxDeclaration ("template-declaration"), aList(aList), aDeclaration(aDeclaration) {}
};

class CxxUsingDeclaration : public CxxDeclaration
{
  bool isTypename;
  CxxName *aName;
 public:
  CxxUsingDeclaration (bool isTypename, CxxName *aName)
      : CxxDeclaration ("using-declaration"), isTypename(isTypename), aName(aName) {}
};

class CxxUsingDirective : public CxxDeclaration
{
  CxxName * aName;

 public:
  CxxUsingDirective (CxxName *aName)
      : CxxDeclaration ("using-directive"), aName(aName) {}
};


// Simple statements

class CxxBreakStatement : public CxxAtomicStatement
{
 public:
  CxxBreakStatement ()
      : CxxAtomicStatement ("statement/break") {}
};

class CxxCaseStatement : public CxxStatement
{
  CxxExpression *anExpr;
  CxxStatement *aStmt;

 public:
  CxxCaseStatement (CxxExpression *anExpr, CxxStatement *aStmt)
      : CxxStatement ("statement/case"), anExpr (anExpr), aStmt (aStmt) {}

  virtual void emit ();
};

class CxxContinueStatement : public CxxAtomicStatement
{
 public:
  CxxContinueStatement ()
      : CxxAtomicStatement ("statement/continue") {}
};

class CxxCompoundStatement : public CxxStatement
{
  CxxStatementList * statementList;

 public:
  CxxCompoundStatement (CxxStatementList *statementList)
      : CxxStatement ("compound-statement"), statementList (statementList) {}

  virtual void emit ();
};

class CxxDeclarationStatement : public CxxStatement
{
  CxxDeclaration *aDecl;

 public:
  CxxDeclarationStatement (CxxDeclaration *aDecl)
      : CxxStatement ("statement/declaration"), aDecl (aDecl) {}

  virtual void emit();
};

class CxxDefaultStatement : public CxxStatement
{
  CxxStatement * aStmt;

 public:
  CxxDefaultStatement (CxxStatement *aStmt)
      : CxxStatement ("statement/default"), aStmt (aStmt) {}

  virtual void emit();
};

class CxxDoWhileStatement : public CxxStatement
{
  CxxStatement *aStmt;
  CxxExpression *testExpr;

 public:
  CxxDoWhileStatement (CxxStatement *aStmt, CxxExpression *testExpr)
      : CxxStatement ("statement/do-while"), aStmt (aStmt), testExpr (testExpr) {}

  virtual void emit ();
};

class CxxLabelStatement : public CxxStatement
{
  CxxToken *aLabel;
  CxxStatement *aStmt;

 public:
  CxxLabelStatement (CxxToken *aLabel, CxxStatement *aStmt)
      : CxxStatement ("statement/label"), aLabel (aLabel), aStmt (aStmt) {}

  virtual void emit ();
};

class CxxLinedStatement : public CxxStatement
{
  CxxStatement *aStatement;
  CxxLine *aLine;

 public:
  CxxLinedStatement (CxxStatement *aStatement, CxxLine *aLine)
      : CxxStatement ("statement/lined"), aStatement (aStatement), aLine (aLine) {}

  virtual void emit();
};

class CxxForStatement : public CxxStatement
{
  CxxExpression *initExpr;
  CxxCondition *testExpr;
  CxxExpression *stepExpr;
  CxxStatement *aStmt;

 public:
  CxxForStatement (CxxExpression *initExpr, CxxCondition *testExpr, CxxExpression *stepExpr, CxxStatement *aStmt)
      : CxxStatement ("statement/for"), initExpr (initExpr), testExpr (testExpr), stepExpr (stepExpr), aStmt (aStmt) {}

  virtual void emit();
};

class CxxGotoStatement : public CxxStatement
{
  CxxToken * aLabel;

 public:
  CxxGotoStatement (CxxToken *aLabel)
      : CxxStatement ("statement/goto"), aLabel (aLabel) {}

  virtual void emit ();
};

class CxxIfStatement : public CxxStatement
{
  CxxCondition *testExpr;
  CxxStatement *trueStmt;
  CxxStatement *falseStmt;

 public:
  CxxIfStatement (CxxCondition *testExpr, CxxStatement *trueStmt, CxxStatement *falseStmt)
      : CxxStatement ("statement/if"), testExpr (testExpr), trueStmt (trueStmt), falseStmt (falseStmt) {}
  virtual void emit();
};

class CxxReturnStatement : public CxxStatement
{
  CxxExpression * anExpr;

 public:
  CxxReturnStatement (CxxExpression * anExpr)
      : CxxStatement ("statement/return"), anExpr (anExpr) {}

  virtual void emit();
};

class CxxSwitchStatement : public CxxStatement
{
  CxxCondition *testExpr;
  CxxStatement *aStmt;

 public:
  CxxSwitchStatement (CxxCondition *testExpr, CxxStatement *aStmt)
      : CxxStatement ("statement/switch"), testExpr (testExpr), aStmt (aStmt) {}

  virtual void emit();
};

class CxxTryBlockStatement : public CxxStatement
{
  CxxFunctionBody * tryBlock;
 public:
  CxxTryBlockStatement (CxxFunctionBody *tryBlock)
      : CxxStatement ("statement/try-block"), tryBlock(tryBlock) {}

  virtual void emit();
};

class CxxWhileStatement : public CxxStatement
{
  CxxCondition *testExpr;
  CxxStatement *aStmt;

 public:
  CxxWhileStatement (CxxCondition *testExpr, CxxStatement *aStmt)
      : CxxStatement ("statement/while"), testExpr (testExpr), aStmt (aStmt) {}

  virtual void emit ();
};

class CxxTokenStatementList : public CxxTokenList
{
 public:
  CxxTokenStatementList ()
      : CxxTokenList ("token-statement-list", 0, 0) {}
};

//
//
// ExpressionList
//
//

class CxxArrowExpression : public CxxExpression
{
  CxxExpression *anExpr;
  CxxName *aName;

 public:
  CxxArrowExpression (CxxExpression *anExpr, CxxName *aName)
      : CxxExpression ("expression/arrow"), anExpr (anExpr), aName (aName) {}
  virtual void emit();
};

class CxxAbstractArrayExpression : public CxxUnaryExpression
{
 public:
  CxxAbstractArrayExpression (CxxExpression *sizeExpr)
      : CxxUnaryExpression ("expression/abstract-array", "abstract-array", sizeExpr) {}
};

class CxxAddExpression : public CxxBinaryExpression
{
 public:
  CxxAddExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/add", "add", leftExpr, rightExpr) {}
};

class CxxAndExpression : public CxxBinaryExpression
{
 public:
  CxxAndExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/and", "and", leftExpr, rightExpr) {}
};

class CxxArrayExpression : public CxxBinaryExpression
{
 public:
  CxxArrayExpression (CxxExpression *anExpr, CxxExpression *indexExpr)
      : CxxBinaryExpression ("expression/array", "array", anExpr, indexExpr) {}
};

class CxxArrowStarExpression : public CxxBinaryExpression
{
 public:
  CxxArrowStarExpression (CxxExpression *anExpr, CxxExpression *memberExpr)
      : CxxBinaryExpression ("expression/arrow-star", "arrow-star", anExpr, memberExpr) {}
};

class CxxAssignmentExpression : public CxxExpression
{
  CxxToken *assOp;
  CxxExpression *leftExpr;
  CxxExpression *rightExpr;

 public:
  CxxAssignmentExpression (CxxToken *assOp, CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxExpression ("expression/assignment"), assOp (assOp), leftExpr (leftExpr), rightExpr (rightExpr) {}

  virtual void emit ();
};

class CxxBitFieldExpression : public CxxBinaryExpression
{
 public:
  CxxBitFieldExpression (CxxExpression *nameExpr, CxxExpression *sizeExpr)
      : CxxBinaryExpression ("expression/bit-field", "colon", nameExpr, sizeExpr) {}
};

class CxxCastExpression : public CxxBinaryExpression
{
 public:
  CxxCastExpression (CxxExpression *aCast, CxxExpression *anExpr)
      : CxxBinaryExpression ("expression/cast", "cast", aCast, anExpr) {}
};

class CxxCharacterLiteralExpression : public CxxExpression
{
  CxxCharacterLiteral * aLiteral;

 public:
  CxxCharacterLiteralExpression (CxxCharacterLiteral *aLiteral)
      : CxxExpression ("expression/character-literal"), aLiteral (aLiteral) {}

  virtual void emit ();
};

class CxxComplementExpression : public CxxUnaryExpression
{
 public:
  CxxComplementExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/complement", "tilde", anExpr) {}
};

class CxxConditionalExpression : public CxxExpression
{
  CxxExpression *testExpr;
  CxxExpression *trueExpr;
  CxxExpression *falseExpr;

 public:
  CxxConditionalExpression (CxxExpression *testExpr, CxxExpression *trueExpr, CxxExpression *falseExpr)
      : CxxExpression ("expression/conditional"), testExpr (testExpr), trueExpr (trueExpr), falseExpr (falseExpr) {}
  virtual void emit ();
};

class CxxConstCastExpression : public CxxBinaryExpression
{
 public:
  CxxConstCastExpression (CxxExpression *aType, CxxExpression *anExpr)
      : CxxBinaryExpression ("expression/const-cast", "const-cast", aType, anExpr) {}
};

class CxxDeleteExpression : public CxxUnaryExpression
{
 public:
  CxxDeleteExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/delete", "delete", anExpr) {}
};

class CxxDivideExpression : public CxxBinaryExpression
{
 public:
  CxxDivideExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/divide", "divide", leftExpr, rightExpr) {}
};

class CxxDotExpression : public CxxExpression
{
  CxxExpression *anExpr;
  CxxName *aName;
 public:
  CxxDotExpression (CxxExpression *anExpr, CxxName *aName)
      : CxxExpression ("expression/dot"), anExpr(anExpr), aName(aName) {}

  virtual void emit();
};

class CxxDotStarExpression : public CxxBinaryExpression
{
 public:
  CxxDotStarExpression (CxxExpression *anExpr, CxxExpression *memberExpr)
      : CxxBinaryExpression ("expression/dot-star", "dot-star", anExpr, memberExpr) {}
};

class CxxDynamicCastExpression : public CxxBinaryExpression
{
 public:
  CxxDynamicCastExpression (CxxExpression *aType, CxxExpression *anExpr)
      : CxxBinaryExpression ("expression/dynamic-cast", "dynamic-cast", aType, anExpr) {}
};

class CxxEqualExpression : public CxxBinaryExpression
{
 public:
  CxxEqualExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/equal", "equal", leftExpr, rightExpr) {}
};

class CxxExclusiveOrExpression : public CxxBinaryExpression
{
 public:
  CxxExclusiveOrExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/exclusive-or", "xor", leftExpr, rightExpr) {}
};

class CxxFalseExpression : public CxxExpression
{
 public:
  CxxFalseExpression ()
      : CxxExpression ("expression/false") {}

  virtual void emit();
};

class CxxFloatingLiteralExpression : public CxxExpression
{
  CxxFloatingLiteral *aLiteral;
 public:
  CxxFloatingLiteralExpression (CxxFloatingLiteral *aLiteral)
      : CxxExpression ("expression/floating-literal"), aLiteral(aLiteral) {}
  virtual void emit();
};

class CxxFunctionDefinitionExpression : public CxxExpression
{
  CxxExpression *anExpr;
  CxxFunctionBody *functionBody;

 public:
  CxxFunctionDefinitionExpression (CxxExpression *anExpr, CxxFunctionBody *functionBody)
      : CxxExpression ("expression/function-definition"), anExpr (anExpr), functionBody (functionBody) {}

  virtual void emit ();
};

class CxxNewExpression : public CxxExpression
{
 public:
  CxxParameterList * aPlace;
  CxxExpression * anInit;
  CxxParameterList * aTypeParams;

 protected:
  CxxNewExpression (char const * token_type)
      : CxxExpression (token_type), aPlace(0), anInit(0), aTypeParams(0) {}
  CxxNewExpression (char const * token_type, CxxParameterList *aPlace, CxxExpression *anInit)
      : CxxExpression (token_type), aPlace (aPlace), anInit (anInit), aTypeParams (0) {}
 public:
  CxxNewExpression (CxxParameterList *aPlace, CxxParameterList *aType, CxxExpression *anInit)
      : CxxExpression ("expression/new"), aPlace (aPlace), anInit (anInit), aTypeParams (aType) {}

  virtual void emit ();
};

class CxxNewTypeIdExpression : public CxxNewExpression
{
  CxxExpression * aTypeExpr;

 public:
  CxxNewTypeIdExpression (char const * token_type)
      : CxxNewExpression (token_type), aTypeExpr(0) {}
  CxxNewTypeIdExpression (CxxParameterList *aPlace, CxxExpression *aType, CxxExpression *anInit)
      : CxxNewExpression ("expression/new-type-id", aPlace, anInit), aTypeExpr (aType) {}

  virtual void emit();
};

class CxxGlobalExpression : public CxxExpression
{
  CxxIsTemplate isTemplate;
  CxxNewExpression * aNewExpr;
  CxxDeleteExpression *aDeleteExpr;
 public:
  CxxGlobalExpression (CxxIsTemplate isTemplate, CxxNewExpression *anExpr)
      : CxxExpression ("expression/global"), isTemplate(isTemplate), aNewExpr(anExpr), aDeleteExpr(0) {}

  CxxGlobalExpression (CxxIsTemplate isTemplate, CxxDeleteExpression *anExpr)
      : CxxExpression ("expression/global"), isTemplate(isTemplate), aNewExpr(0), aDeleteExpr(anExpr) {}
};

class CxxGreaterEqualExpression : public CxxBinaryExpression
{
 public:
  CxxGreaterEqualExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/greater-equal", "greater-equal", leftExpr, rightExpr) {}
};

class CxxGreaterThanExpression : public CxxBinaryExpression
{
 public:
  CxxGreaterThanExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/greater-than", "greater-than", leftExpr, rightExpr) {}
};

class CxxInclusiveOrExpression : public CxxBinaryExpression
{
 public:
  CxxInclusiveOrExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/inclusive-or", "inclusive-or", leftExpr, rightExpr) {}
};

class CxxIndexCastExpression : public CxxBinaryExpression
{
 public:
  CxxIndexCastExpression (CxxExpression *aCast, CxxExpression *anExpr)
      : CxxBinaryExpression ("expression/index-cast", "index-cast", aCast, anExpr) {}
};

class CxxIntegerLiteralExpression : public CxxExpression
{
  CxxIntegerLiteral *aLiteral;

 public:
  CxxIntegerLiteralExpression (CxxIntegerLiteral *aLiteral)
      : CxxExpression ("expression/integer-literal"), aLiteral (aLiteral) {}

  virtual void emit();
};

class CxxLessEqualExpression : public CxxBinaryExpression
{
 public:
  CxxLessEqualExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/less-equal", "less-equal", leftExpr, rightExpr) {}
};

class CxxLessThanExpression : public CxxBinaryExpression
{
 public:
  CxxLessThanExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/less-than", "less-than", leftExpr, rightExpr) {}
};

class CxxLogicalAndExpression : public CxxBinaryExpression
{
 public:
  CxxLogicalAndExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/logical-and", "logical-and", leftExpr, rightExpr) {}
};

class CxxLogicalOrExpression : public CxxBinaryExpression
{
 public:
  CxxLogicalOrExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/logical-or", "logical-or", leftExpr, rightExpr) {}
};

class CxxMinusExpression : public CxxUnaryExpression
{
 public:
  CxxMinusExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/minus", "unary-minus", anExpr) {}
};

class CxxModulusExpression : public CxxBinaryExpression
{
 public:
  CxxModulusExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/modulus", "mod", leftExpr, rightExpr) {}
};

class CxxMultiplyExpression : public CxxBinaryExpression
{
  CxxDeclarator * aDeclarator;

 public:
  CxxMultiplyExpression (CxxExpression *leftExpr, CxxDeclarator *aDeclarator, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/multiply", "asterisk", leftExpr, rightExpr), aDeclarator (aDeclarator) {}

  virtual void emit();
};

class CxxNotEqualExpression : public CxxBinaryExpression
{
 public:
  CxxNotEqualExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/not-equal", "not-equal", leftExpr, rightExpr) {}
};

class CxxNotExpression : public CxxUnaryExpression
{
 public:
  CxxNotExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/not", "exclamation", anExpr) {}
};

class CxxEllipsesExpression : public CxxParameter
{
 public:
  CxxEllipsesExpression ()
      : CxxParameter ("expression/ellipses") {}
  virtual void emit();
};

class CxxPlusExpression : public CxxUnaryExpression
{
 public:
  CxxPlusExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/plus", "plus", anExpr) {}
};

class CxxPostDecrementExpression : public CxxUnaryExpression
{
 public:
  CxxPostDecrementExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/post-decrement", "post-decrement", anExpr) {}
};

class CxxPostIncrementExpression : public CxxUnaryExpression
{
 public:
  CxxPostIncrementExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/post-increment", "post-increment", anExpr) {}
};

class CxxPreDecrementExpression : public CxxUnaryExpression
{
 public:
  CxxPreDecrementExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/pre-decrement", "pre-decrement", anExpr) {}
};

class CxxPreIncrementExpression : public CxxUnaryExpression
{
 public:
  CxxPreIncrementExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/pre-increment", "pre-increment", anExpr) {}
};

class CxxReinterpretCastExpression : public CxxBinaryExpression
{
 public:
  CxxReinterpretCastExpression (CxxExpression *aType, CxxExpression *anExpr)
      : CxxBinaryExpression ("expression/reinterpret-cast", "reinterpret-cast", aType, anExpr) {}
};

class CxxShiftLeftExpression : public CxxBinaryExpression
{
 public:
  CxxShiftLeftExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/shift-left", "shift-left", leftExpr, rightExpr) {}
};

class CxxShiftRightExpression : public CxxBinaryExpression
{
 public:
  CxxShiftRightExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/shift-right", "shift-right", leftExpr, rightExpr) {}
};

class CxxSizeofExpression : public CxxUnaryExpression
{
 public:
  CxxSizeofExpression (CxxExpression *anExpr)
      : CxxUnaryExpression ("expression/sizeof", "sizeof", anExpr) {}
};

class CxxStaticCastExpression : public CxxBinaryExpression
{
 public:
  CxxStaticCastExpression (CxxExpression *aType, CxxExpression *anExpr)
      : CxxBinaryExpression ("expression/static-cast", "static-cast", aType, anExpr) {}
};

class CxxStringLiteralExpression : public CxxExpression
{
  CxxStringList *aString;

 public:
  CxxStringLiteralExpression (CxxStringList *aString)
      : CxxExpression ("expression/string-literal"), aString (aString) {}

  virtual void emit();
};

class CxxSubtractExpression : public CxxBinaryExpression
{
 public:
  CxxSubtractExpression (CxxExpression *leftExpr, CxxExpression *rightExpr)
      : CxxBinaryExpression ("expression/subtract", "subtract", leftExpr, rightExpr) {}
};

class CxxThisExpression : public CxxExpression
{
 public:
  CxxThisExpression ()
      : CxxExpression ("expression/this") {}

  virtual void emit();
};

class CxxThrowExpression : public CxxExpression
{
  CxxExpression *anExpr;

 public:
  CxxThrowExpression (CxxExpression *anExpr)
      : CxxExpression ("expression/throw"), anExpr(anExpr) {}

  virtual void emit ();
};

class CxxTrueExpression : public CxxExpression
{
 public:
  CxxTrueExpression ()
      : CxxExpression ("expression/true") {}

  virtual void emit ();
};

class CxxPointerExpression : public CxxExpression
{
  CxxDeclarator *aDeclarator;
  CxxExpression *anExpr;

 public:
  CxxPointerExpression (CxxDeclarator *aDeclarator, CxxExpression *anExpr)
      : CxxExpression ("expression/pointer"), aDeclarator (aDeclarator), anExpr (anExpr) {}

  virtual void emit();
};

class CxxInitializerClause : public CxxExpression
{
 protected:
  CxxInitializerClause (char const * token_type)
      : CxxExpression (token_type) {}
};

class CxxInitializerExpressionClause : public CxxInitializerClause
{
  CxxExpression * anExpr;
 public:
  CxxInitializerExpressionClause (CxxExpression *anExpr)
      : CxxInitializerClause ("initializer-expression-clause"), anExpr(anExpr) {}

  virtual void emit ();
};

class CxxInitializerClauseList : public CxxInitializerClause
{
  CxxInitializerClauseList * aList;
  CxxInitializerClause *anElement;

 public:
  CxxInitializerClauseList (CxxInitializerClauseList *aList, CxxInitializerClause *anElement)
      : CxxInitializerClause ("initializer-clause-list"), aList(aList), anElement(anElement) {}

  virtual void emit ();
};

class CxxInitializerListClause : public CxxInitializerClause
{
  CxxInitializerClauseList *aList;

 public:
  CxxInitializerListClause (CxxInitializerClauseList *aList)
      : CxxInitializerClause ("initializer-list-clause"), aList(aList) {}

  virtual void emit ();
};


class CxxCtorDefinitionExpression : public CxxExpression
{
  CxxExpressionList *anExpr;
  CxxFunctionBody *functionBody;

 public:
  CxxCtorDefinitionExpression (CxxExpressionList *anExpr, CxxFunctionBody *functionBody)
      : CxxExpression ("expression/ctor-definition"), anExpr (anExpr), functionBody (functionBody) {}

  virtual void emit ();
};


class CxxDeclSpecifierExpression : public CxxExpression
{
  CxxExpression *anExpr;
  CxxDeclSpecifierId *declSpecifier;

 public:
  CxxDeclSpecifierExpression (CxxExpression *anExpr, CxxDeclSpecifierId *declSpecifier)
      : CxxExpression ("expression/decl-specifier"), anExpr (anExpr), declSpecifier (declSpecifier) {}

  virtual void emit ();
};

class CxxCompoundExpression : public CxxExpression
{
  CxxExpressionList * aList;

 public:
  CxxCompoundExpression (CxxExpressionList *aList)
      : CxxExpression ("expression-list"), aList (aList) {}

  virtual void emit();
};

class CxxScopeExpression : public CxxExpression
{
  CxxExpression *anExpr;
  CxxDeclaration *functionDeclarationList;
 public:
  CxxScopeExpression (CxxExpression *anExpr, CxxDeclaration *functionDeclarationList)
      : CxxExpression ("expression/scope"), anExpr(anExpr), functionDeclarationList(functionDeclarationList) {}

  virtual void emit();
};

class CxxScopedPointerExpression : public CxxExpression
{
  CxxExpression *aScope;
  CxxDeclarator *aDeclarator;
  CxxExpression *anExpr;
 public:
  CxxScopedPointerExpression (CxxExpression *aScope, CxxDeclarator *aDeclarator, CxxExpression *anExpr)
      : CxxExpression ("expression/scoped-pointer"), aScope(aScope), aDeclarator(aDeclarator), anExpr(anExpr) {}

  virtual void emit();
};

class CxxSetTemplateExpression : public CxxExpression
{
  CxxExpression * anExpr;
 public:
  CxxSetTemplateExpression (CxxExpression *anExpr)
      : CxxExpression ("expression/set-template"), anExpr(anExpr) {}

  virtual void emit();
};

class CxxTokenListExpression : public CxxExpression
{
  CxxTokenList * someTokenList;

 public:
  CxxTokenListExpression (CxxTokenList *someTokenList)
      : CxxExpression ("expression/tokens"), someTokenList(someTokenList) {}

  virtual void emit();
};

class CxxTemplateName : public CxxName
{
  CxxName * aName;
  CxxTemplateArgumentList *templateArguments;

 public:
  CxxTemplateName (CxxName * aName, CxxTemplateArgumentList *templateArguments)
      : CxxName ("template-name"), aName (aName), templateArguments (templateArguments) {}

  virtual void emit ();
};

class CxxUtilityMode : public CxxUtility
{
 public:
  CxxUtilityMode ()
      : CxxUtility ("utility-mode") {};
};

class CxxMemInitializer : public CxxToken
{
  CxxName * aName;
  CxxExpression * anExpr;

 public:
  CxxMemInitializer (CxxName *aName, CxxExpression *anExpr)
      : CxxToken ("mem-initializer"), aName(aName), anExpr(anExpr) {}

  virtual void emit();
};

class CxxMemInitializerList : public CxxTokenList
{
 public:
  CxxMemInitializerList (CxxMemInitializerList *aList, CxxMemInitializer *anElement)
      : CxxTokenList ("mem-initializer-list", aList, anElement) {}
};

class CxxLinkageSpecifier : public CxxName
{
  CxxStringList * aString;
  CxxDeclarationList * aDeclarationList;

 public:
  CxxLinkageSpecifier (CxxStringList *aString, CxxDeclaration *aDeclaration)
      : CxxName ("linkage-specifier"), aString (aString), aDeclarationList (new CxxDeclarationList (new CxxDeclarationList (0,0), aDeclaration)) {}
  CxxLinkageSpecifier (CxxStringList *aString, CxxDeclarationList *aDeclarationList)
      : CxxName ("linkage-specifier"), aString (aString), aDeclarationList (aDeclarationList) {}

  virtual void emit();
};

class CxxNameSpaceDefinition : public CxxName
{
  CxxName *aName;
  CxxDeclarationList *aDeclaration;
 public:
  CxxNameSpaceDefinition (CxxName *aName, CxxDeclarationList *aDeclaration)
      : CxxName ("name-space-definition"), aName(aName), aDeclaration(aDeclaration) {}

  virtual void emit ();
};

class CxxAccessBaseSpecifier : public CxxBaseSpecifier
{
  CxxBaseSpecifier *baseSpecifier;
  CxxAccessSpecifier *accessSpecifier;

 public:
  CxxAccessBaseSpecifier (CxxBaseSpecifier *baseSpecifier, CxxAccessSpecifier *accessSpecifier)
      : CxxBaseSpecifier ("access-base-specifier"), baseSpecifier (baseSpecifier), accessSpecifier (accessSpecifier)
  {
  }

  virtual void emit ();
};

class CxxClassSpecifierId : public CxxClass
{
  CxxClassKey *classKey;
  CxxName *aName;
  CxxBaseSpecifierList *baseSpecifiers;

 public:
  CxxClassSpecifierId  (CxxClassKey *classKey, CxxName *aName, CxxBaseSpecifierList *baseSpecifiers)
      : CxxClass ("class-specifier-id"),
        classKey (classKey), aName (aName), baseSpecifiers (baseSpecifiers) {}

  virtual void emit();
};

class CxxPosition : public CxxName
{
 public:
  CxxPosition ()
      : CxxName ("position") {}
};

class CxxFunctionBlock : public CxxFunctionBody
{
  CxxStatement * aStatement;

 protected:
  CxxFunctionBlock (char const * token_type)
      : CxxFunctionBody (token_type), aStatement(0) {}
 public:
  CxxFunctionBlock (CxxStatement *aStatement)
      : CxxFunctionBody ("function-block"), aStatement (aStatement) {}

  virtual void emit ();
};

class CxxTryBlock : public CxxFunctionBody
{
  CxxStatement *aStatement;
  CxxHandlerList *exceptionHandlerList;

 public:
  CxxTryBlock (CxxStatement *aStatement, CxxHandlerList *exceptionHandlerList)
      : CxxFunctionBody ("try-block"), aStatement(aStatement), exceptionHandlerList(exceptionHandlerList) {}

  virtual void emit ();
};

class CxxTryFunctionBlock : public CxxFunctionBody
{
  CxxFunctionBody *functionBody;
  CxxHandlerList *exceptionHandlerList;

 public:
  CxxTryFunctionBlock (CxxFunctionBody *functionBody, CxxHandlerList *exceptionHandlerList)
      : CxxFunctionBody ("try-function-block"), functionBody(functionBody), exceptionHandlerList(exceptionHandlerList) {}

  virtual void emit ();
};

class CxxCtorFunctionBlock : public CxxFunctionBody
{
  CxxFunctionBody *functionBody;
  CxxMemInitializerList *ctorList;

 public:
  CxxCtorFunctionBlock (CxxFunctionBody *functionBody, CxxMemInitializerList *ctorList)
      : CxxFunctionBody ("ctor-function-block"), functionBody(functionBody), ctorList(ctorList) {}

  virtual void emit ();
};

class CxxInitializedParameter : public CxxParameter
{
  CxxParameter *aParameter;
  CxxExpression *anExpr;
 public:
  CxxInitializedParameter (CxxParameter *aParameter, CxxExpression *anExpr)
      : CxxParameter ("initialized-parameter"), aParameter(aParameter), anExpr(anExpr) {}

  virtual void emit ();
};

class CxxExpressionParameter : public CxxParameter
{
  CxxExpression * anExpr;

 public:
  CxxExpressionParameter (CxxExpression *anExpr)
      : CxxParameter ("expression/parameter"), anExpr (anExpr) {}

  virtual void emit ();
};

class CxxDeclSpecifierParameter : public CxxParameter
{
  CxxParameter *aName;
  CxxDeclSpecifierId *declSpecifier;

 public:
  CxxDeclSpecifierParameter (CxxParameter *aName, CxxDeclSpecifierId *declSpecifier)
      : CxxParameter ("decl-specifier-parameter"), aName (aName), declSpecifier (declSpecifier) {}

  virtual void emit ();
};

class CxxParenthesised : public CxxToken
{
  CxxParameterList *aList;
  CxxCvQualifierList *cvQualifierList;
  CxxExceptionSpecification *exceptionSpecification;

 public:
  CxxParenthesised (CxxParameterList *aList, CxxCvQualifierList *cvQualifierList, CxxExceptionSpecification *exceptionSpecification)
      : CxxToken ("parenthesised"), aList (aList), cvQualifierList (cvQualifierList), exceptionSpecification (exceptionSpecification) {}
  virtual void emit ();
};

class CxxAbstractFunctionExpression : public CxxExpression
{
  CxxParenthesised * aParenthesis;

 public:
  CxxAbstractFunctionExpression (CxxParenthesised *aParenthesis)
      : CxxExpression ("expression/abstract-function"), aParenthesis (aParenthesis) {}

  virtual void emit();
};

class CxxCallExpression : public CxxExpression
{
  CxxExpression * anExpr;
  CxxParenthesised * aParenthesis;

 public:
  CxxCallExpression (CxxExpression *anExpr, CxxParenthesised *aParenthesis)
      : CxxExpression ("expression/call"), anExpr (anExpr), aParenthesis (aParenthesis)  {}

  virtual void emit ();
};

class CxxType1Expression : public CxxExpression
{
  CxxExpression *functionName;
  CxxParenthesised *aParenthesis;
  CxxType1ParameterList *type1ParameterList;

 public:
  CxxType1Expression (CxxExpression *functionName, CxxParenthesised *aParenthesis, CxxType1ParameterList *type1ParameterList)
      : CxxExpression ("expression/type1"), functionName(functionName), aParenthesis(aParenthesis), type1ParameterList(type1ParameterList) {}

  virtual void emit ();
};

class CxxSimpleTypeParameter : public CxxTemplateParameter
{
 public:
  CxxName * aName;
  CxxSimpleTypeParameter (char const * token_type)
      : CxxTemplateParameter (token_type), aName(0) {}
  CxxSimpleTypeParameter (CxxName *aName)
      : CxxTemplateParameter ("simple-type-parameter"), aName(aName) {}

  virtual void emit ();
};

class CxxTypeTemplateParameter : public CxxTemplateParameter
{
  CxxSimpleTypeParameter *typeParameter;
  CxxExpression *typeId;
 public:
  CxxTypeTemplateParameter (CxxSimpleTypeParameter *typeParameter, CxxExpression *typeId)
      : CxxTemplateParameter ("type-template-parameter"), typeParameter(typeParameter), typeId(typeId) {}

  virtual void emit ();
};

class CxxClassTemplateParameter : public CxxSimpleTypeParameter
{
  CxxName *aName;
 public:
  CxxClassTemplateParameter (CxxName *aName)
      : CxxSimpleTypeParameter ("class-template-parameter"), aName(aName) {}

  virtual void emit ();
};

class CxxInitSimpleTypeParameter : public CxxSimpleTypeParameter
{
  CxxSimpleTypeParameter *templateParameterList;
  CxxExpression *anExpr;
 public:
  CxxInitSimpleTypeParameter (CxxSimpleTypeParameter *templateParameterList, CxxExpression *anExpr)
      : CxxSimpleTypeParameter ("init-simple-type-parameter"), templateParameterList(templateParameterList), anExpr(anExpr) {}

  virtual void emit ();
};

class CxxTypenameTemplateParameter : public CxxSimpleTypeParameter
{
  CxxName * aName;
 public:
  CxxTypenameTemplateParameter (CxxName *aName)
      : CxxSimpleTypeParameter ("typename-template-parameter"), aName(aName) {}

  virtual void emit ();
};

class CxxTypenameTypeParameter : public CxxSimpleTypeParameter
{
  CxxName * aName;
 public:
  CxxTypenameTypeParameter (CxxName *aName)
      : CxxSimpleTypeParameter ("typename-type-parameter"), aName(aName) {}

  virtual void emit ();
};

class CxxTemplatedTemplateParameter : public CxxTemplateParameter
{
  CxxTemplateParameter *typeParameter;
  CxxName *aName;

 public:
  CxxTemplatedTemplateParameter (char const * token_type)
      : CxxTemplateParameter (token_type) {}
  CxxTemplatedTemplateParameter  (CxxTemplateParameter *typeParameter, CxxName *aName)
      : CxxTemplateParameter ("templated-template-parameter"), typeParameter(typeParameter), aName(aName) {}

  virtual void emit ();
};

class CxxClassTypeParameter : public CxxSimpleTypeParameter
{
  CxxName *aName;
 public:
  CxxClassTypeParameter (CxxName *aName)
      : CxxSimpleTypeParameter ("class-type-parameter"), aName(aName) {}

  virtual void emit ();
};

class CxxTemplatedTypeParameter : public CxxTemplateParameter
{
  CxxTemplateParameterList *templateParameterList;
  CxxName *aName;
 public:
  CxxTemplatedTypeParameter (char const * token_type)
      : CxxTemplateParameter (token_type) {}
  CxxTemplatedTypeParameter (CxxTemplateParameterList *templateParameterList, CxxName *aName)
      : CxxTemplateParameter ("templated-type-parameter"), templateParameterList(templateParameterList), aName(aName) {}

  virtual void emit ();
};

class CxxInitTemplatedParameter : public CxxTemplatedTypeParameter
{
  CxxTemplatedTypeParameter *typeParameter;
  CxxName *aName;
 public:
  CxxInitTemplatedParameter (CxxTemplatedTypeParameter *typeParameter, CxxName *aName)
      : CxxTemplatedTypeParameter ("init-templated-parameter"), typeParameter(typeParameter), aName(aName) {}

  virtual void emit ();
};

#define FOGPARSERVALUE_ENUM(T,N) \
        const T *name2 (u_, N); \
        const T& N() const { return *name2 (u_, N); } \
        const T* & N() { return name2 (u_, N); }
#define FOGPARSERVALUE_POINTER(T,N) T *N
#define FOGPARSERVALUE_VALUE(T,N) T N

union CxxTokenType
{
  CxxToken *_token;

  FOGPARSERVALUE_VALUE (bool, _bool);
  FOGPARSERVALUE_VALUE (long, _long);
  // FOGPARSERVALUE_POINTER (CxxBrace, brace)
  // FOGPARSERVALUE_POINTER (CxxSpacing, spacing);

  FOGPARSERVALUE_POINTER (CxxAccessSpecifier, access_specifier);
  FOGPARSERVALUE_POINTER (CxxBaseSpecifier, base_specifier);
  FOGPARSERVALUE_POINTER (CxxBaseSpecifierList, base_specifiers);
  FOGPARSERVALUE_POINTER (CxxBuiltInId, built_in_id);
  FOGPARSERVALUE_POINTER (CxxCharacterLiteral, character_literal);
  FOGPARSERVALUE_POINTER (CxxClass, _class);
  FOGPARSERVALUE_POINTER (CxxClassKey, class_key);
  FOGPARSERVALUE_POINTER (CxxCondition, condition);
  FOGPARSERVALUE_POINTER (CxxCvQualifierList, cv_qualifiers);
  FOGPARSERVALUE_POINTER (CxxDeclSpecifierId, decl_specifier_id);
  FOGPARSERVALUE_POINTER (CxxDeclaration, declaration);
  FOGPARSERVALUE_POINTER (CxxDeclarationList, declarations);
  FOGPARSERVALUE_POINTER (CxxDeclarator, declarator);
  FOGPARSERVALUE_POINTER (CxxDeleteExpression, delete_expression);
  FOGPARSERVALUE_POINTER (CxxEnumerator, enumerator);
  FOGPARSERVALUE_POINTER (CxxEnumeratorList, enumerators);
  FOGPARSERVALUE_POINTER (CxxExceptionDeclaration, exception_declaration);
  FOGPARSERVALUE_POINTER (CxxExceptionSpecification, exception_specification);
  FOGPARSERVALUE_POINTER (CxxExpression, expression);
  FOGPARSERVALUE_POINTER (CxxExpressionList, expressions);
  // FOGPARSERVALUE_POINTER (CxxFileId, file_id)
  // FOGPARSERVALUE_POINTER (CxxFileIds, file_ids)
  // FOGPARSERVALUE_POINTER (CxxFileName, file_name)
  FOGPARSERVALUE_POINTER (CxxFloatingLiteral, floating_literal);
  FOGPARSERVALUE_POINTER (CxxFunctionBody, function_body);
  FOGPARSERVALUE_POINTER (CxxHandler, handler);
  FOGPARSERVALUE_POINTER (CxxHandlerList, handlers);
  FOGPARSERVALUE_POINTER (CxxIdentifier, identifier);
  FOGPARSERVALUE_POINTER (CxxInitializerClause, initializer_clause);
  FOGPARSERVALUE_POINTER (CxxInitializerClauseList, initializer_clauses);
  FOGPARSERVALUE_POINTER (CxxIntegerLiteral, integer_literal);
  FOGPARSERVALUE_POINTER (CxxKeyword, keyword);
  FOGPARSERVALUE_POINTER (CxxLine, line);
  FOGPARSERVALUE_POINTER (CxxMemInitializer, mem_initializer);
  FOGPARSERVALUE_POINTER (CxxMemInitializerList, mem_initializers);
  FOGPARSERVALUE_POINTER (CxxMemberDeclarationList, member_declarations);
  // FOGPARSERVALUE_POINTER (CxxMetaClass, meta_class)
  // FOGPARSERVALUE_POINTER (CxxMetaFunction, meta_function)
  // FOGPARSERVALUE_POINTER (CxxMetaInitializer, meta_initializer)
  // FOGPARSERVALUE_POINTER (CxxMetaInitializerList, meta_initializers)
  // FOGPARSERVALUE_POINTER (CxxMetaObject, meta_object)
  // FOGPARSERVALUE_POINTER (CxxMetaStatement, meta_statement)
  // FOGPARSERVALUE_POINTER (CxxMetaType, meta_type)
  // FOGPARSERVALUE_POINTER (CxxMetaVariable, meta_variable)
  FOGPARSERVALUE_POINTER (CxxName, name);
  FOGPARSERVALUE_POINTER (CxxNewExpression, new_expression);
  // FOGPARSERVALUE_POINTER (CxxNumberLiteral, number_literal);
  FOGPARSERVALUE_POINTER (CxxParameter, parameter);
  FOGPARSERVALUE_POINTER (CxxParameterList, parameters);
  FOGPARSERVALUE_POINTER (CxxParenthesised, parenthesised);
  FOGPARSERVALUE_POINTER (CxxPointerDeclarator, pointer_declarator);
  FOGPARSERVALUE_POINTER (CxxPosition, position);
  // FOGPARSERVALUE_POINTER (CxxSegment, segment)
  FOGPARSERVALUE_POINTER (CxxSimpleTypeParameter, simple_type_parameter);
  FOGPARSERVALUE_POINTER (CxxStatement, statement);
  FOGPARSERVALUE_POINTER (CxxStatementList, statements);
  FOGPARSERVALUE_POINTER (CxxStringLiteral, string_literal);
  FOGPARSERVALUE_POINTER (CxxStringList, strings);
  // FOGPARSERVALUE_POINTER (CxxSubspace, subspace);
  // FOGPARSERVALUE_POINTER (CxxSyntaxMacroParameter, syntax_macro_parameter)
  // FOGPARSERVALUE_POINTER (CxxSyntaxMacroParameterList, syntax_macro_parameters)
  FOGPARSERVALUE_POINTER (CxxTemplateArgument, template_argument);
  FOGPARSERVALUE_POINTER (CxxTemplateArgumentList, template_arguments);
  FOGPARSERVALUE_POINTER (CxxTemplateParameter, template_parameter);
  FOGPARSERVALUE_POINTER (CxxTemplateParameterList, template_parameters);
  FOGPARSERVALUE_POINTER (CxxTemplatedTypeParameter, templated_type_parameter);
  FOGPARSERVALUE_POINTER (CxxToken, token);
  FOGPARSERVALUE_POINTER (CxxTokenStatementList, token_statements);
  FOGPARSERVALUE_POINTER (CxxTokenList, tokens);
  // FOGPARSERVALUE_POINTER (CxxTreeArgument, tree_argument)
  // FOGPARSERVALUE_POINTER (CxxTreeArguments, tree_arguments)
  // FOGPARSERVALUE_POINTER (CxxTreeExpression, tree_expression)
  FOGPARSERVALUE_POINTER (CxxType1ParameterList, type1_parameters);
  FOGPARSERVALUE_POINTER (CxxUtility, utility);

  FOGPARSERVALUE_VALUE (int, bang);
  FOGPARSERVALUE_VALUE (CxxIsTemplate, is_template);
  FOGPARSERVALUE_VALUE (YACC_MARK_TYPE, mark);
  FOGPARSERVALUE_VALUE (size_t, nest);
#if 0
  CxxAccessSpecifier *access_specifier;
  CxxBaseSpecifier *base_specifier;
  CxxBaseSpecifierList *base_specifiers;
  CxxBuiltInId *built_in_id;
  CxxCharacterLiteral *character_literal;
  CxxClass *_class;
  CxxClassKey *class_key;
  CxxCondition *condition;
  CxxCvQualifierList *cv_qualifiers;
  CxxDeclaration *declaration;
  CxxDeclarationList *declarations;
  CxxDeclarator *declarator;
  CxxDeclSpecifierId *decl_specifier_id;
//      CxxDerived *derived;
//      CxxEnum *_enum;
  CxxEnumerator *enumerator;
  CxxEnumeratorList *enumerators;
  CxxExceptionDeclaration *exception_declaration;
  CxxExceptionSpecification *exception_specification;
  CxxExpression *expression;
  CxxExpressionList *expressions;
  CxxFileId *file_id;
  CxxFileIds *file_ids;
  CxxFileName *file_name;
  CxxFloatingLiteral *floating_literal;
  CxxFunctionBody *function_body;
  CxxFunctionDeclarationList *function_declarations;
  CxxHandler *handler;
  CxxHandlerList *handlers;
  CxxIdentifier *identifier;
//      CxxIds *ids;
  CxxInitializerClause *initializer_clause;
  CxxInitializerClauseList *initializer_clauses;
  CxxIntegerLiteral *integer_literal;
  CxxKeyword *keyword;
  CxxLine *line;
//      CxxList *list;
  CxxMemInitializer *mem_initializer;
  CxxMemInitializerList *mem_initializers;
  CxxMemberDeclarationList *member_declarations;
  CxxMetaClass *meta_class;
//      CxxMetaFunction *meta_function;
//      CxxMetaInitializer *meta_initializer;
//      CxxMetaInitializerList *meta_initializers;
//      CxxMetaObject *meta_object;
  CxxMetaParameter *meta_parameter;
  CxxMetaParameterList *meta_parameters;
//      CxxMetaPrototype *meta_prototype;
//      CxxMetaPrototypes *meta_prototypes;
//      CxxMetaStatement *meta_statement;
  CxxMetaType *meta_type;
//      CxxMetaVariable *meta_variable;
  CxxName *name;
//      CxxNamespace *_namespace;
  CxxNumberLiteral *number_literal;
  CxxParameter *parameter;
  CxxParameterList *parameters;
  CxxParenthesised *parenthesised;
  CxxPointerDeclarator *pointer_declarator;
  CxxSegment *segment;
  CxxSimpleTypeParameter *simple_type_parameter;
  CxxStatement *statement;
  CxxStatementList *statements;
  CxxStringLiteral *string_literal;
  CxxStringList *strings;
  CxxSyntaxMacroParameter *syntax_macro_parameter;
  CxxSyntaxMacroParameterList *syntax_macro_parameters;
  CxxTemplateArgument *template_argument;
  CxxTemplateArgumentList *template_arguments;
  CxxTemplateParameter *template_parameter;
  CxxTemplateParameterList *template_parameters;
  CxxTemplatedTypeParameter *templated_type_parameter;
  CxxToken *token;
  CxxTokenList *tokens;
//      CxxTreeArgument *tree_argument;
//      CxxTreeArguments *tree_arguments;
//      CxxTreeExpression *tree_expression;
  CxxType1ParameterList *type1_parameters;
//      CxxTypeId *type_id;
//      CxxTypeIdList *type_ids;
  CxxUtility *utility;
  YACC_BANG_TYPE bang;
  YACC_MARK_TYPE mark;
  size_t nest;
#endif
};

class CxxNaffToken : public CxxToken
{
  typedef CxxToken Super;
  char *_text;
  int _leng;
 private:
  CxxNaffToken (const CxxNaffToken&);
  CxxNaffToken& operator= (const CxxNaffToken&);
 public:
  CxxNaffToken (int tokenValue, const char *yyText, int yyLeng);
  virtual ~CxxNaffToken();
  virtual void emit();
};

#endif
