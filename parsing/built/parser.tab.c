/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "./parsing/parser.y"

	#include <stdio.h>
	#include "../../types.h"
	#include "../../ast/ast.h"

	void yyerror(AstNode** node, char* msg) {
		fprintf(stderr, "Parse Error: %s\n", msg);
	}
	
	extern int yylex(void);

#line 83 "./parsing/built/parser.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "parser.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_TokenArrow = 3,                 /* TokenArrow  */
  YYSYMBOL_TokenDefine = 4,                /* TokenDefine  */
  YYSYMBOL_TokenMod = 5,                   /* TokenMod  */
  YYSYMBOL_TokenAnd = 6,                   /* TokenAnd  */
  YYSYMBOL_TokenOr = 7,                    /* TokenOr  */
  YYSYMBOL_TokenXor = 8,                   /* TokenXor  */
  YYSYMBOL_TokenNeg = 9,                   /* TokenNeg  */
  YYSYMBOL_TokenLShift = 10,               /* TokenLShift  */
  YYSYMBOL_TokenRShift = 11,               /* TokenRShift  */
  YYSYMBOL_TokenEq = 12,                   /* TokenEq  */
  YYSYMBOL_TokenNe = 13,                   /* TokenNe  */
  YYSYMBOL_TokenGt = 14,                   /* TokenGt  */
  YYSYMBOL_TokenLt = 15,                   /* TokenLt  */
  YYSYMBOL_TokenGe = 16,                   /* TokenGe  */
  YYSYMBOL_TokenLe = 17,                   /* TokenLe  */
  YYSYMBOL_TokenLogAnd = 18,               /* TokenLogAnd  */
  YYSYMBOL_TokenLogOr = 19,                /* TokenLogOr  */
  YYSYMBOL_TokenLogNot = 20,               /* TokenLogNot  */
  YYSYMBOL_TokenLCompose = 21,             /* TokenLCompose  */
  YYSYMBOL_TokenRCompose = 22,             /* TokenRCompose  */
  YYSYMBOL_TokenLApply = 23,               /* TokenLApply  */
  YYSYMBOL_TokenRApply = 24,               /* TokenRApply  */
  YYSYMBOL_TokenRBrace = 25,               /* TokenRBrace  */
  YYSYMBOL_TokenLBrace = 26,               /* TokenLBrace  */
  YYSYMBOL_TokenRBrack = 27,               /* TokenRBrack  */
  YYSYMBOL_TokenLBrack = 28,               /* TokenLBrack  */
  YYSYMBOL_TokenRParen = 29,               /* TokenRParen  */
  YYSYMBOL_TokenLParen = 30,               /* TokenLParen  */
  YYSYMBOL_TokenDoubleQuote = 31,          /* TokenDoubleQuote  */
  YYSYMBOL_TokenSingleQuote = 32,          /* TokenSingleQuote  */
  YYSYMBOL_TokenComma = 33,                /* TokenComma  */
  YYSYMBOL_TokenSemicolon = 34,            /* TokenSemicolon  */
  YYSYMBOL_TokenColon = 35,                /* TokenColon  */
  YYSYMBOL_TokenFn = 36,                   /* TokenFn  */
  YYSYMBOL_TokenTrue = 37,                 /* TokenTrue  */
  YYSYMBOL_TokenFalse = 38,                /* TokenFalse  */
  YYSYMBOL_TokenCase = 39,                 /* TokenCase  */
  YYSYMBOL_TokenIf = 40,                   /* TokenIf  */
  YYSYMBOL_TokenElif = 41,                 /* TokenElif  */
  YYSYMBOL_TokenElse = 42,                 /* TokenElse  */
  YYSYMBOL_TokenReturn = 43,               /* TokenReturn  */
  YYSYMBOL_TokenStruct = 44,               /* TokenStruct  */
  YYSYMBOL_TokenEnum = 45,                 /* TokenEnum  */
  YYSYMBOL_TokenStr = 46,                  /* TokenStr  */
  YYSYMBOL_TokenNum = 47,                  /* TokenNum  */
  YYSYMBOL_TokenChar = 48,                 /* TokenChar  */
  YYSYMBOL_TokenIdent = 49,                /* TokenIdent  */
  YYSYMBOL_FUNC_END_PREC = 50,             /* FUNC_END_PREC  */
  YYSYMBOL_TokenAdd = 51,                  /* TokenAdd  */
  YYSYMBOL_TokenSub = 52,                  /* TokenSub  */
  YYSYMBOL_TokenMul = 53,                  /* TokenMul  */
  YYSYMBOL_TokenDiv = 54,                  /* TokenDiv  */
  YYSYMBOL_UNARY = 55,                     /* UNARY  */
  YYSYMBOL_YYACCEPT = 56,                  /* $accept  */
  YYSYMBOL_FILE = 57,                      /* FILE  */
  YYSYMBOL_STRUCT = 58,                    /* STRUCT  */
  YYSYMBOL_STRUCT_MEMS = 59,               /* STRUCT_MEMS  */
  YYSYMBOL_STRUCT_MEM = 60,                /* STRUCT_MEM  */
  YYSYMBOL_ENUM = 61,                      /* ENUM  */
  YYSYMBOL_ENUM_VARS = 62,                 /* ENUM_VARS  */
  YYSYMBOL_ENUM_VAR = 63,                  /* ENUM_VAR  */
  YYSYMBOL_STMTS = 64,                     /* STMTS  */
  YYSYMBOL_STMT = 65,                      /* STMT  */
  YYSYMBOL_ID = 66,                        /* ID  */
  YYSYMBOL_EXPR = 67,                      /* EXPR  */
  YYSYMBOL_BINARY_EXPR = 68,               /* BINARY_EXPR  */
  YYSYMBOL_UNARY_EXPR = 69,                /* UNARY_EXPR  */
  YYSYMBOL_FUNC_CALL = 70,                 /* FUNC_CALL  */
  YYSYMBOL_INDEX = 71,                     /* INDEX  */
  YYSYMBOL_IF_EXPR = 72,                   /* IF_EXPR  */
  YYSYMBOL_ELIFS = 73,                     /* ELIFS  */
  YYSYMBOL_ELSE = 74,                      /* ELSE  */
  YYSYMBOL_BLOCK = 75,                     /* BLOCK  */
  YYSYMBOL_CASE_EXPR = 76,                 /* CASE_EXPR  */
  YYSYMBOL_CASE_ARMS = 77,                 /* CASE_ARMS  */
  YYSYMBOL_PATTERN = 78,                   /* PATTERN  */
  YYSYMBOL_CASE_VAL = 79,                  /* CASE_VAL  */
  YYSYMBOL_FUNC_DEF = 80,                  /* FUNC_DEF  */
  YYSYMBOL_FUNC_END = 81,                  /* FUNC_END  */
  YYSYMBOL_FUNC_ARGS = 82,                 /* FUNC_ARGS  */
  YYSYMBOL_FUNC_ARGS_LIST = 83,            /* FUNC_ARGS_LIST  */
  YYSYMBOL_ATOM = 84,                      /* ATOM  */
  YYSYMBOL_LITERAL = 85,                   /* LITERAL  */
  YYSYMBOL_GROUPED = 86,                   /* GROUPED  */
  YYSYMBOL_ARRAY = 87,                     /* ARRAY  */
  YYSYMBOL_EXPR_LIST = 88                  /* EXPR_LIST  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  41
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   768

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  56
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  33
/* YYNRULES -- Number of rules.  */
#define YYNRULES  91
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  159

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   310


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    94,    94,    95,    96,    98,   100,   101,   102,   104,
     106,   108,   109,   110,   112,   113,   115,   116,   118,   119,
     121,   123,   124,   125,   126,   127,   128,   129,   130,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   155,   156,   157,   159,   160,   162,   164,   166,   167,
     169,   170,   172,   174,   176,   177,   179,   180,   182,   183,
     185,   187,   188,   190,   191,   193,   194,   195,   197,   198,
     199,   200,   202,   203,   204,   205,   206,   208,   210,   212,
     213,   214
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "TokenArrow",
  "TokenDefine", "TokenMod", "TokenAnd", "TokenOr", "TokenXor", "TokenNeg",
  "TokenLShift", "TokenRShift", "TokenEq", "TokenNe", "TokenGt", "TokenLt",
  "TokenGe", "TokenLe", "TokenLogAnd", "TokenLogOr", "TokenLogNot",
  "TokenLCompose", "TokenRCompose", "TokenLApply", "TokenRApply",
  "TokenRBrace", "TokenLBrace", "TokenRBrack", "TokenLBrack",
  "TokenRParen", "TokenLParen", "TokenDoubleQuote", "TokenSingleQuote",
  "TokenComma", "TokenSemicolon", "TokenColon", "TokenFn", "TokenTrue",
  "TokenFalse", "TokenCase", "TokenIf", "TokenElif", "TokenElse",
  "TokenReturn", "TokenStruct", "TokenEnum", "TokenStr", "TokenNum",
  "TokenChar", "TokenIdent", "FUNC_END_PREC", "TokenAdd", "TokenSub",
  "TokenMul", "TokenDiv", "UNARY", "$accept", "FILE", "STRUCT",
  "STRUCT_MEMS", "STRUCT_MEM", "ENUM", "ENUM_VARS", "ENUM_VAR", "STMTS",
  "STMT", "ID", "EXPR", "BINARY_EXPR", "UNARY_EXPR", "FUNC_CALL", "INDEX",
  "IF_EXPR", "ELIFS", "ELSE", "BLOCK", "CASE_EXPR", "CASE_ARMS", "PATTERN",
  "CASE_VAL", "FUNC_DEF", "FUNC_END", "FUNC_ARGS", "FUNC_ARGS_LIST",
  "ATOM", "LITERAL", "GROUPED", "ARRAY", "EXPR_LIST", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-113)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     716,   716,   716,   716,   716,   -22,  -113,  -113,   716,   716,
    -113,  -113,  -113,  -113,   716,    33,  -113,   716,    20,    79,
    -113,  -113,  -113,  -113,  -113,  -113,  -113,  -113,  -113,  -113,
    -113,  -113,    -5,    -5,   135,    -1,   185,   -15,   235,   285,
      -5,  -113,   -14,   -13,  -113,  -113,  -113,   716,   716,   716,
     716,   716,   716,   716,   716,   716,   716,   716,   716,   716,
     716,   716,   716,   716,   716,   716,   716,    -8,  -113,   716,
     716,   716,   716,   716,  -113,  -113,    -6,    13,  -113,    68,
     716,     2,    19,    21,   335,    -5,   658,   627,   654,   647,
     647,   620,   620,   620,   620,   620,   620,   566,   593,   539,
     539,   512,   512,   385,  -113,    17,   107,   107,    -5,    -5,
    -113,   -15,    72,  -113,    56,    80,  -113,    57,   716,    46,
      50,    55,  -113,  -113,  -113,  -113,   693,  -113,   693,  -113,
     285,    82,  -113,    75,    93,    87,    55,    96,    89,   485,
    -113,  -113,   435,  -113,    68,     2,  -113,    74,  -113,    50,
    -113,  -113,    55,  -113,  -113,  -113,  -113,  -113,  -113
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
      17,     0,     0,     0,     0,     0,    85,    86,     0,     0,
      83,    82,    84,    20,     0,     0,     4,    17,    79,     0,
      21,    22,    23,    24,    25,    26,    27,    28,    78,    80,
      81,    79,    52,    53,    91,     0,     0,    74,     0,     0,
      51,     1,     0,     0,     2,     3,    16,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    19,     0,
       0,     0,     0,    90,    88,    87,    77,     0,    73,    65,
      17,    59,     0,     0,     0,    50,    43,    41,    42,    45,
      44,    35,    36,    37,    39,    38,    40,    33,    34,    31,
      32,    29,    30,     0,    55,     0,    46,    47,    48,    49,
      89,    76,     0,    67,     0,     0,    66,     0,     0,    61,
       8,    13,    18,    56,    54,    75,     0,    63,     0,    62,
       0,     0,    57,     0,     0,     7,    15,     0,    12,    72,
      71,    70,     0,    69,    65,    59,    60,     0,     5,     8,
      14,    10,    13,    68,    64,    58,     9,     6,    11
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
    -113,  -113,  -113,   -25,  -113,  -113,   -27,   -10,   -11,  -113,
       0,     1,  -113,  -113,  -113,  -113,  -113,   -17,  -113,  -112,
    -113,    11,  -113,  -113,  -113,  -113,  -113,    23,  -113,   -68,
    -113,  -113,   -60
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    15,    44,   134,   135,    45,   137,   138,    16,    17,
      31,    19,    20,    21,    22,    23,    24,   119,   132,    81,
      25,   114,   115,   144,    26,   141,    77,    78,    27,    28,
      29,    30,    35
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      18,     1,    32,    33,    34,    36,    46,   105,    37,    38,
      39,   116,     2,   110,   140,    40,   143,    18,   145,   146,
       3,   104,     4,    66,    47,    67,    74,   111,     5,     6,
       7,     8,     9,    41,    13,    82,    83,    76,    10,    11,
      12,    13,   112,   118,    14,   120,   124,   121,    84,    85,
      86,    87,    88,    89,    90,    91,    92,    93,    94,    95,
      96,    97,    98,    99,   100,   101,   102,   103,    34,   117,
     106,   107,   108,   109,    34,   126,   116,    42,    43,   113,
      18,   127,   129,   128,    48,    49,    50,    51,   131,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,   133,
      62,    63,    64,    65,   136,     6,     7,    66,    80,    67,
     147,    76,    48,    68,    10,    11,    12,    13,   148,   130,
     149,   151,   152,   156,   157,   158,   150,   139,   155,   142,
      69,    70,    71,    72,   125,    66,     0,    67,     0,     0,
      48,    49,    50,    51,   113,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,   154,    62,    63,    64,    65,
      71,    72,     0,    66,     0,    67,     0,     0,    73,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,     0,     0,    66,    75,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,    79,     0,    66,     0,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,    80,     0,    66,     0,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,     0,     0,    66,     0,    67,     0,     0,     0,   122,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,     0,   123,    66,     0,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,     0,     0,    66,     0,    67,     0,     0,   153,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    69,    70,    71,    72,
      48,    49,    50,    51,     0,    52,    53,    54,    55,    56,
      57,    58,    59,    60,    61,     0,    62,    63,    64,    65,
       0,     0,     0,    66,     0,    67,     0,    48,    49,    50,
      51,     0,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    61,     0,    62,    63,     0,    69,    70,    71,    72,
      66,     0,    67,     0,    48,    49,    50,    51,     0,    52,
      53,    54,    55,    56,    57,    58,    59,    60,    61,     0,
       0,     0,     0,    69,    70,    71,    72,    66,     0,    67,
       0,    48,    49,    50,    51,     0,    52,    53,    54,    55,
      56,    57,    58,    59,     0,    61,     0,     0,     0,     0,
      69,    70,    71,    72,    66,     0,    67,     0,    48,    49,
      50,    51,     0,    52,    53,    54,    55,    56,    57,    58,
      59,     0,     0,     0,     0,     0,     0,    69,    70,    71,
      72,    66,     0,    67,     0,    48,    49,    50,    51,     0,
      52,    53,    48,    49,     0,    51,     0,    52,    53,     0,
       0,     0,     0,     0,    69,    70,    71,    72,    66,     0,
      67,     0,    48,     0,     0,    66,     0,    67,     0,    48,
      49,     0,     0,    48,    52,    53,     0,     0,    52,    53,
       0,    69,    70,    71,    72,    66,     0,    67,    69,    70,
      71,    72,    66,     0,    67,     0,    66,     0,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    69,    70,
      71,    72,     1,     0,     0,    69,    70,    71,    72,    69,
      70,    71,    72,     2,     0,     0,     0,     0,     0,    80,
       0,     3,     0,     4,     0,     1,     0,     0,     0,     5,
       6,     7,     8,     9,     0,     0,     2,     0,     0,    10,
      11,    12,    13,     0,     3,    14,     4,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,     0,     0,     0,
       0,     0,    10,    11,    12,    13,     0,     0,    14
};

static const yytype_int16 yycheck[] =
{
       0,     9,     1,     2,     3,     4,    17,    67,    30,     8,
       9,    79,    20,    73,   126,    14,   128,    17,   130,   131,
      28,    29,    30,    28,     4,    30,    27,    33,    36,    37,
      38,    39,    40,     0,    49,    49,    49,    37,    46,    47,
      48,    49,    29,    41,    52,    26,    29,    26,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    80,
      69,    70,    71,    72,    73,     3,   144,    44,    45,    79,
      80,    25,    25,     3,     5,     6,     7,     8,    42,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    49,
      21,    22,    23,    24,    49,    37,    38,    28,    26,    30,
      35,   111,     5,    34,    46,    47,    48,    49,    25,   118,
      33,    25,    33,    49,   149,   152,   136,   126,   145,   128,
      51,    52,    53,    54,   111,    28,    -1,    30,    -1,    -1,
       5,     6,     7,     8,   144,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,   144,    21,    22,    23,    24,
      53,    54,    -1,    28,    -1,    30,    -1,    -1,    33,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    28,    29,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    26,    -1,    28,    -1,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    26,    -1,    28,    -1,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    28,    -1,    30,    -1,    -1,    -1,    34,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    27,    28,    -1,    30,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    28,    -1,    30,    -1,    -1,    33,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,    54,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    28,    -1,    30,    -1,     5,     6,     7,
       8,    -1,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    -1,    21,    22,    -1,    51,    52,    53,    54,
      28,    -1,    30,    -1,     5,     6,     7,     8,    -1,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    -1,
      -1,    -1,    -1,    51,    52,    53,    54,    28,    -1,    30,
      -1,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    -1,    19,    -1,    -1,    -1,    -1,
      51,    52,    53,    54,    28,    -1,    30,    -1,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    14,    15,    16,
      17,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    53,
      54,    28,    -1,    30,    -1,     5,     6,     7,     8,    -1,
      10,    11,     5,     6,    -1,     8,    -1,    10,    11,    -1,
      -1,    -1,    -1,    -1,    51,    52,    53,    54,    28,    -1,
      30,    -1,     5,    -1,    -1,    28,    -1,    30,    -1,     5,
       6,    -1,    -1,     5,    10,    11,    -1,    -1,    10,    11,
      -1,    51,    52,    53,    54,    28,    -1,    30,    51,    52,
      53,    54,    28,    -1,    30,    -1,    28,    -1,    30,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      53,    54,     9,    -1,    -1,    51,    52,    53,    54,    51,
      52,    53,    54,    20,    -1,    -1,    -1,    -1,    -1,    26,
      -1,    28,    -1,    30,    -1,     9,    -1,    -1,    -1,    36,
      37,    38,    39,    40,    -1,    -1,    20,    -1,    -1,    46,
      47,    48,    49,    -1,    28,    52,    30,    -1,    -1,    -1,
      -1,    -1,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    46,    47,    48,    49,    -1,    -1,    52
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     9,    20,    28,    30,    36,    37,    38,    39,    40,
      46,    47,    48,    49,    52,    57,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    76,    80,    84,    85,    86,
      87,    66,    67,    67,    67,    88,    67,    30,    67,    67,
      67,     0,    44,    45,    58,    61,    64,     4,     5,     6,
       7,     8,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    21,    22,    23,    24,    28,    30,    34,    51,
      52,    53,    54,    33,    27,    29,    66,    82,    83,    26,
      26,    75,    49,    49,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    29,    88,    67,    67,    67,    67,
      88,    33,    29,    66,    77,    78,    85,    64,    41,    73,
      26,    26,    34,    27,    29,    83,     3,    25,     3,    25,
      67,    42,    74,    49,    59,    60,    49,    62,    63,    67,
      75,    81,    67,    75,    79,    75,    75,    35,    25,    33,
      63,    25,    33,    33,    77,    73,    49,    59,    62
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    56,    57,    57,    57,    58,    59,    59,    59,    60,
      61,    62,    62,    62,    63,    63,    64,    64,    65,    65,
      66,    67,    67,    67,    67,    67,    67,    67,    67,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    68,    68,
      68,    68,    68,    68,    68,    68,    68,    68,    68,    68,
      68,    69,    69,    69,    70,    70,    71,    72,    73,    73,
      74,    74,    75,    76,    77,    77,    78,    78,    79,    79,
      80,    81,    81,    82,    82,    83,    83,    83,    84,    84,
      84,    84,    85,    85,    85,    85,    85,    86,    87,    88,
      88,    88
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     2,     1,     5,     3,     1,     0,     3,
       5,     3,     1,     0,     2,     1,     2,     0,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     2,     2,     2,     4,     3,     4,     5,     4,     0,
       2,     0,     3,     5,     4,     0,     1,     1,     2,     1,
       6,     1,     1,     1,     0,     3,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     3,     3,
       2,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (parse_node, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, parse_node); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, AstNode** parse_node)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (parse_node);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, AstNode** parse_node)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep, parse_node);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule, AstNode** parse_node)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)], parse_node);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, parse_node); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, AstNode** parse_node)
{
  YY_USE (yyvaluep);
  YY_USE (parse_node);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (AstNode** parse_node)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 4: /* FILE: STMTS  */
#line 96 "./parsing/parser.y"
                                { *parse_node = (yyvsp[0].node); }
#line 1397 "./parsing/built/parser.tab.c"
    break;

  case 16: /* STMTS: STMT STMTS  */
#line 115 "./parsing/parser.y"
                   { (yyval.node) = new_astnode(AstntStmts); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = (yyvsp[0].node); }
#line 1403 "./parsing/built/parser.tab.c"
    break;

  case 17: /* STMTS: %empty  */
#line 116 "./parsing/parser.y"
                           { (yyval.node) = NULL; }
#line 1409 "./parsing/built/parser.tab.c"
    break;

  case 18: /* STMT: ID TokenDefine EXPR TokenSemicolon  */
#line 118 "./parsing/parser.y"
                                          { (yyval.node) = new_astnode(AstntAssignStmt); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); }
#line 1415 "./parsing/built/parser.tab.c"
    break;

  case 19: /* STMT: EXPR TokenSemicolon  */
#line 119 "./parsing/parser.y"
                                                          { (yyval.node) = new_astnode(AstntExprStmt);   (yyval.node)->kids[0] = (yyvsp[-1].node);  				   }
#line 1421 "./parsing/built/parser.tab.c"
    break;

  case 20: /* ID: TokenIdent  */
#line 121 "./parsing/parser.y"
               { (yyval.node) = new_astnode(AstntIdent); (yyval.node)->val.i = (yyvsp[0].ident); }
#line 1427 "./parsing/built/parser.tab.c"
    break;

  case 21: /* EXPR: BINARY_EXPR  */
#line 123 "./parsing/parser.y"
                  { (yyval.node) = (yyvsp[0].node); }
#line 1433 "./parsing/built/parser.tab.c"
    break;

  case 22: /* EXPR: UNARY_EXPR  */
#line 124 "./parsing/parser.y"
                      { (yyval.node) = (yyvsp[0].node); }
#line 1439 "./parsing/built/parser.tab.c"
    break;

  case 23: /* EXPR: FUNC_CALL  */
#line 125 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1445 "./parsing/built/parser.tab.c"
    break;

  case 24: /* EXPR: INDEX  */
#line 126 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1451 "./parsing/built/parser.tab.c"
    break;

  case 25: /* EXPR: IF_EXPR  */
#line 127 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1457 "./parsing/built/parser.tab.c"
    break;

  case 26: /* EXPR: CASE_EXPR  */
#line 128 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1463 "./parsing/built/parser.tab.c"
    break;

  case 27: /* EXPR: FUNC_DEF  */
#line 129 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1469 "./parsing/built/parser.tab.c"
    break;

  case 28: /* EXPR: ATOM  */
#line 130 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1475 "./parsing/built/parser.tab.c"
    break;

  case 29: /* BINARY_EXPR: EXPR TokenLApply EXPR  */
#line 132 "./parsing/parser.y"
                                     { (yyval.node) = new_binary_astnode(AstntBinaryLApply,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1481 "./parsing/built/parser.tab.c"
    break;

  case 30: /* BINARY_EXPR: EXPR TokenRApply EXPR  */
#line 133 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryRApply,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1487 "./parsing/built/parser.tab.c"
    break;

  case 31: /* BINARY_EXPR: EXPR TokenLCompose EXPR  */
#line 134 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLCompose, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1493 "./parsing/built/parser.tab.c"
    break;

  case 32: /* BINARY_EXPR: EXPR TokenRCompose EXPR  */
#line 135 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryRCompose, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1499 "./parsing/built/parser.tab.c"
    break;

  case 33: /* BINARY_EXPR: EXPR TokenLogAnd EXPR  */
#line 136 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLogAnd,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1505 "./parsing/built/parser.tab.c"
    break;

  case 34: /* BINARY_EXPR: EXPR TokenLogOr EXPR  */
#line 137 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLogOr,    (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1511 "./parsing/built/parser.tab.c"
    break;

  case 35: /* BINARY_EXPR: EXPR TokenEq EXPR  */
#line 138 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryEq,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1517 "./parsing/built/parser.tab.c"
    break;

  case 36: /* BINARY_EXPR: EXPR TokenNe EXPR  */
#line 139 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryNe,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1523 "./parsing/built/parser.tab.c"
    break;

  case 37: /* BINARY_EXPR: EXPR TokenGt EXPR  */
#line 140 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryGt,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1529 "./parsing/built/parser.tab.c"
    break;

  case 38: /* BINARY_EXPR: EXPR TokenGe EXPR  */
#line 141 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryGe,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1535 "./parsing/built/parser.tab.c"
    break;

  case 39: /* BINARY_EXPR: EXPR TokenLt EXPR  */
#line 142 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLt,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1541 "./parsing/built/parser.tab.c"
    break;

  case 40: /* BINARY_EXPR: EXPR TokenLe EXPR  */
#line 143 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLe,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1547 "./parsing/built/parser.tab.c"
    break;

  case 41: /* BINARY_EXPR: EXPR TokenOr EXPR  */
#line 144 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryOr,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1553 "./parsing/built/parser.tab.c"
    break;

  case 42: /* BINARY_EXPR: EXPR TokenXor EXPR  */
#line 145 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryXor,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1559 "./parsing/built/parser.tab.c"
    break;

  case 43: /* BINARY_EXPR: EXPR TokenAnd EXPR  */
#line 146 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryAnd,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1565 "./parsing/built/parser.tab.c"
    break;

  case 44: /* BINARY_EXPR: EXPR TokenRShift EXPR  */
#line 147 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryRShift,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1571 "./parsing/built/parser.tab.c"
    break;

  case 45: /* BINARY_EXPR: EXPR TokenLShift EXPR  */
#line 148 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLShift,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1577 "./parsing/built/parser.tab.c"
    break;

  case 46: /* BINARY_EXPR: EXPR TokenAdd EXPR  */
#line 149 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryAdd,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1583 "./parsing/built/parser.tab.c"
    break;

  case 47: /* BINARY_EXPR: EXPR TokenSub EXPR  */
#line 150 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinarySub,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1589 "./parsing/built/parser.tab.c"
    break;

  case 48: /* BINARY_EXPR: EXPR TokenMul EXPR  */
#line 151 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryMul,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1595 "./parsing/built/parser.tab.c"
    break;

  case 49: /* BINARY_EXPR: EXPR TokenDiv EXPR  */
#line 152 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryDiv,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1601 "./parsing/built/parser.tab.c"
    break;

  case 50: /* BINARY_EXPR: EXPR TokenMod EXPR  */
#line 153 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryMod,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1607 "./parsing/built/parser.tab.c"
    break;

  case 51: /* UNARY_EXPR: TokenSub EXPR  */
#line 155 "./parsing/parser.y"
                                         { (yyval.node) = new_unary_astnode(AstntUnaryNeg, (yyvsp[0].node));    }
#line 1613 "./parsing/built/parser.tab.c"
    break;

  case 52: /* UNARY_EXPR: TokenNeg EXPR  */
#line 156 "./parsing/parser.y"
                                                 { (yyval.node) = new_unary_astnode(AstntUnaryBitNot, (yyvsp[0].node)); }
#line 1619 "./parsing/built/parser.tab.c"
    break;

  case 53: /* UNARY_EXPR: TokenLogNot EXPR  */
#line 157 "./parsing/parser.y"
                                                 { (yyval.node) = new_unary_astnode(AstntUnaryLogNot, (yyvsp[0].node)); }
#line 1625 "./parsing/built/parser.tab.c"
    break;

  case 54: /* FUNC_CALL: EXPR TokenLParen EXPR_LIST TokenRParen  */
#line 159 "./parsing/parser.y"
                                                  { (yyval.node) = new_astnode(AstntFuncCall); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node);   }
#line 1631 "./parsing/built/parser.tab.c"
    break;

  case 55: /* FUNC_CALL: EXPR TokenLParen TokenRParen  */
#line 160 "./parsing/parser.y"
                                                                  { (yyval.node) = new_astnode(AstntFuncCall); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = NULL; }
#line 1637 "./parsing/built/parser.tab.c"
    break;

  case 56: /* INDEX: EXPR TokenLBrack EXPR TokenRBrack  */
#line 162 "./parsing/parser.y"
                                         { (yyval.node) = new_astnode(AstntArrayIndex); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); }
#line 1643 "./parsing/built/parser.tab.c"
    break;

  case 57: /* IF_EXPR: TokenIf EXPR BLOCK ELIFS ELSE  */
#line 164 "./parsing/parser.y"
                                       { (yyval.node) = new_astnode(AstntIf); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-2].node); (yyval.node)->kids[2] = (yyvsp[-1].node); (yyval.node)->kids[3] = (yyvsp[0].node); }
#line 1649 "./parsing/built/parser.tab.c"
    break;

  case 58: /* ELIFS: TokenElif EXPR BLOCK ELIFS  */
#line 166 "./parsing/parser.y"
                                  { (yyval.node) = new_astnode(AstntElif); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = (yyvsp[-1].node); (yyval.node)->kids[2] = (yyvsp[0].node); }
#line 1655 "./parsing/built/parser.tab.c"
    break;

  case 59: /* ELIFS: %empty  */
#line 167 "./parsing/parser.y"
                                                                  { (yyval.node) = NULL; }
#line 1661 "./parsing/built/parser.tab.c"
    break;

  case 60: /* ELSE: TokenElse BLOCK  */
#line 169 "./parsing/parser.y"
                                          { (yyval.node) = new_astnode(AstntElse); (yyval.node)->kids[0] = (yyvsp[0].node); }
#line 1667 "./parsing/built/parser.tab.c"
    break;

  case 61: /* ELSE: %empty  */
#line 170 "./parsing/parser.y"
                                                                  { (yyval.node) = NULL; }
#line 1673 "./parsing/built/parser.tab.c"
    break;

  case 62: /* BLOCK: TokenLBrace STMTS TokenRBrace  */
#line 172 "./parsing/parser.y"
                                     { (yyval.node) = (yyvsp[-1].node); }
#line 1679 "./parsing/built/parser.tab.c"
    break;

  case 63: /* CASE_EXPR: TokenCase EXPR TokenLBrace CASE_ARMS TokenRBrace  */
#line 174 "./parsing/parser.y"
                                                                { (yyval.node) = new_astnode(AstntCase); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); }
#line 1685 "./parsing/built/parser.tab.c"
    break;

  case 64: /* CASE_ARMS: PATTERN TokenArrow CASE_VAL CASE_ARMS  */
#line 176 "./parsing/parser.y"
                                                 { (yyval.node) = new_astnode(AstntCaseArm); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); (yyval.node)->kids[2] = (yyvsp[0].node); }
#line 1691 "./parsing/built/parser.tab.c"
    break;

  case 65: /* CASE_ARMS: %empty  */
#line 177 "./parsing/parser.y"
                                                                                                 { (yyval.node) = NULL; 		  																     }
#line 1697 "./parsing/built/parser.tab.c"
    break;

  case 66: /* PATTERN: LITERAL  */
#line 179 "./parsing/parser.y"
                 { (yyval.node) = (yyvsp[0].node); }
#line 1703 "./parsing/built/parser.tab.c"
    break;

  case 67: /* PATTERN: ID  */
#line 180 "./parsing/parser.y"
                         { (yyval.node) = (yyvsp[0].node); }
#line 1709 "./parsing/built/parser.tab.c"
    break;

  case 68: /* CASE_VAL: EXPR TokenComma  */
#line 182 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[-1].node); }
#line 1715 "./parsing/built/parser.tab.c"
    break;

  case 69: /* CASE_VAL: BLOCK  */
#line 183 "./parsing/parser.y"
                                          { (yyval.node) = (yyvsp[0].node); }
#line 1721 "./parsing/built/parser.tab.c"
    break;

  case 70: /* FUNC_DEF: TokenFn TokenLParen FUNC_ARGS TokenRParen TokenArrow FUNC_END  */
#line 185 "./parsing/parser.y"
                                                                        { (yyval.node) = new_astnode(AstntFuncDef); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[0].node); }
#line 1727 "./parsing/built/parser.tab.c"
    break;

  case 71: /* FUNC_END: BLOCK  */
#line 187 "./parsing/parser.y"
                                    { (yyval.node) = (yyvsp[0].node); }
#line 1733 "./parsing/built/parser.tab.c"
    break;

  case 72: /* FUNC_END: EXPR  */
#line 188 "./parsing/parser.y"
                                            { (yyval.node) = (yyvsp[0].node); }
#line 1739 "./parsing/built/parser.tab.c"
    break;

  case 73: /* FUNC_ARGS: FUNC_ARGS_LIST  */
#line 190 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node);   }
#line 1745 "./parsing/built/parser.tab.c"
    break;

  case 74: /* FUNC_ARGS: %empty  */
#line 191 "./parsing/parser.y"
                                                  { (yyval.node) = NULL; }
#line 1751 "./parsing/built/parser.tab.c"
    break;

  case 75: /* FUNC_ARGS_LIST: ID TokenComma FUNC_ARGS_LIST  */
#line 193 "./parsing/parser.y"
                                             { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = (yyvsp[0].node);   }
#line 1757 "./parsing/built/parser.tab.c"
    break;

  case 76: /* FUNC_ARGS_LIST: ID TokenComma  */
#line 194 "./parsing/parser.y"
                                                                         { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = NULL; }
#line 1763 "./parsing/built/parser.tab.c"
    break;

  case 77: /* FUNC_ARGS_LIST: ID  */
#line 195 "./parsing/parser.y"
                                                                                 { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[0].node); (yyval.node)->kids[1] = NULL; }
#line 1769 "./parsing/built/parser.tab.c"
    break;

  case 78: /* ATOM: LITERAL  */
#line 197 "./parsing/parser.y"
              { (yyval.node) = (yyvsp[0].node); }
#line 1775 "./parsing/built/parser.tab.c"
    break;

  case 79: /* ATOM: ID  */
#line 198 "./parsing/parser.y"
                  { (yyval.node) = (yyvsp[0].node); }
#line 1781 "./parsing/built/parser.tab.c"
    break;

  case 80: /* ATOM: GROUPED  */
#line 199 "./parsing/parser.y"
                  { (yyval.node) = (yyvsp[0].node); }
#line 1787 "./parsing/built/parser.tab.c"
    break;

  case 82: /* LITERAL: TokenNum  */
#line 202 "./parsing/parser.y"
                    { (yyval.node) = new_astnode(AstntNumLit);  (yyval.node)->val.n = (yyvsp[0].num);    }
#line 1793 "./parsing/built/parser.tab.c"
    break;

  case 83: /* LITERAL: TokenStr  */
#line 203 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntStrLit);  (yyval.node)->val.s = (yyvsp[0].str);    }
#line 1799 "./parsing/built/parser.tab.c"
    break;

  case 84: /* LITERAL: TokenChar  */
#line 204 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntCharLit); (yyval.node)->val.c = (yyvsp[0].ch);    }
#line 1805 "./parsing/built/parser.tab.c"
    break;

  case 85: /* LITERAL: TokenTrue  */
#line 205 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntBoolLit); (yyval.node)->val.b = true;  }
#line 1811 "./parsing/built/parser.tab.c"
    break;

  case 86: /* LITERAL: TokenFalse  */
#line 206 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntBoolLit); (yyval.node)->val.b = false; }
#line 1817 "./parsing/built/parser.tab.c"
    break;

  case 87: /* GROUPED: TokenLParen EXPR TokenRParen  */
#line 208 "./parsing/parser.y"
                                      { (yyval.node) = (yyvsp[-1].node); }
#line 1823 "./parsing/built/parser.tab.c"
    break;

  case 88: /* ARRAY: TokenLBrack EXPR_LIST TokenRBrack  */
#line 210 "./parsing/parser.y"
                                         { (yyval.node) = new_astnode(AstntArray); (yyval.node)->kids[0] = (yyvsp[-1].node); }
#line 1829 "./parsing/built/parser.tab.c"
    break;

  case 89: /* EXPR_LIST: EXPR TokenComma EXPR_LIST  */
#line 212 "./parsing/parser.y"
                                     { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = (yyvsp[0].node);   }
#line 1835 "./parsing/built/parser.tab.c"
    break;

  case 90: /* EXPR_LIST: EXPR TokenComma  */
#line 213 "./parsing/parser.y"
                                                         { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = NULL; }
#line 1841 "./parsing/built/parser.tab.c"
    break;

  case 91: /* EXPR_LIST: EXPR  */
#line 214 "./parsing/parser.y"
                                                                 { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[0].node); (yyval.node)->kids[1] = NULL; }
#line 1847 "./parsing/built/parser.tab.c"
    break;


#line 1851 "./parsing/built/parser.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (parse_node, YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, parse_node);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, parse_node);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (parse_node, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, parse_node);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, parse_node);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 216 "./parsing/parser.y"

