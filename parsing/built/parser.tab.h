/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSING_BUILT_PARSER_TAB_H_INCLUDED
# define YY_YY_PARSING_BUILT_PARSER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 85 "./parsing/parser.y"
 
	#include "../../ast/ast.h" 
	#include "../../ast/optimization.h" 

#line 54 "./parsing/built/parser.tab.h"

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    TokenArrow = 258,              /* TokenArrow  */
    TokenDefine = 259,             /* TokenDefine  */
    TokenMod = 260,                /* TokenMod  */
    TokenAnd = 261,                /* TokenAnd  */
    TokenOr = 262,                 /* TokenOr  */
    TokenXor = 263,                /* TokenXor  */
    TokenNeg = 264,                /* TokenNeg  */
    TokenLShift = 265,             /* TokenLShift  */
    TokenRShift = 266,             /* TokenRShift  */
    TokenEq = 267,                 /* TokenEq  */
    TokenNe = 268,                 /* TokenNe  */
    TokenGt = 269,                 /* TokenGt  */
    TokenLt = 270,                 /* TokenLt  */
    TokenGe = 271,                 /* TokenGe  */
    TokenLe = 272,                 /* TokenLe  */
    TokenLogAnd = 273,             /* TokenLogAnd  */
    TokenLogOr = 274,              /* TokenLogOr  */
    TokenLogNot = 275,             /* TokenLogNot  */
    TokenLCompose = 276,           /* TokenLCompose  */
    TokenRCompose = 277,           /* TokenRCompose  */
    TokenLApply = 278,             /* TokenLApply  */
    TokenRApply = 279,             /* TokenRApply  */
    TokenRBrace = 280,             /* TokenRBrace  */
    TokenLBrace = 281,             /* TokenLBrace  */
    TokenRBrack = 282,             /* TokenRBrack  */
    TokenLBrack = 283,             /* TokenLBrack  */
    TokenRParen = 284,             /* TokenRParen  */
    TokenLParen = 285,             /* TokenLParen  */
    TokenDoubleQuote = 286,        /* TokenDoubleQuote  */
    TokenSingleQuote = 287,        /* TokenSingleQuote  */
    TokenComma = 288,              /* TokenComma  */
    TokenSemicolon = 289,          /* TokenSemicolon  */
    TokenColon = 290,              /* TokenColon  */
    TokenFn = 291,                 /* TokenFn  */
    TokenTrue = 292,               /* TokenTrue  */
    TokenFalse = 293,              /* TokenFalse  */
    TokenCase = 294,               /* TokenCase  */
    TokenIf = 295,                 /* TokenIf  */
    TokenElif = 296,               /* TokenElif  */
    TokenElse = 297,               /* TokenElse  */
    TokenReturn = 298,             /* TokenReturn  */
    TokenStruct = 299,             /* TokenStruct  */
    TokenEnum = 300,               /* TokenEnum  */
    TokenRec = 301,                /* TokenRec  */
    TokenStr = 302,                /* TokenStr  */
    TokenNum = 303,                /* TokenNum  */
    TokenChar = 304,               /* TokenChar  */
    TokenIdent = 305,              /* TokenIdent  */
    FUNC_END_PREC = 306,           /* FUNC_END_PREC  */
    TokenAdd = 307,                /* TokenAdd  */
    TokenSub = 308,                /* TokenSub  */
    TokenMul = 309,                /* TokenMul  */
    TokenDiv = 310,                /* TokenDiv  */
    UNARY = 311                    /* UNARY  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 13 "./parsing/parser.y"
 
	AstNode* node;
	string str;
	string ident;
	i64 num;
	char ch;

#line 135 "./parsing/built/parser.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (AstNode** parse_node);


#endif /* !YY_YY_PARSING_BUILT_PARSER_TAB_H_INCLUDED  */
