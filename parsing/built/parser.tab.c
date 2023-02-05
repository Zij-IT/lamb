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
  YYSYMBOL_TokenRec = 46,                  /* TokenRec  */
  YYSYMBOL_TokenNil = 47,                  /* TokenNil  */
  YYSYMBOL_TokenStr = 48,                  /* TokenStr  */
  YYSYMBOL_TokenNum = 49,                  /* TokenNum  */
  YYSYMBOL_TokenChar = 50,                 /* TokenChar  */
  YYSYMBOL_TokenIdent = 51,                /* TokenIdent  */
  YYSYMBOL_FUNC_END_PREC = 52,             /* FUNC_END_PREC  */
  YYSYMBOL_TokenAdd = 53,                  /* TokenAdd  */
  YYSYMBOL_TokenSub = 54,                  /* TokenSub  */
  YYSYMBOL_TokenMul = 55,                  /* TokenMul  */
  YYSYMBOL_TokenDiv = 56,                  /* TokenDiv  */
  YYSYMBOL_UNARY = 57,                     /* UNARY  */
  YYSYMBOL_YYACCEPT = 58,                  /* $accept  */
  YYSYMBOL_FILE = 59,                      /* FILE  */
  YYSYMBOL_STRUCT = 60,                    /* STRUCT  */
  YYSYMBOL_STRUCT_MEMS = 61,               /* STRUCT_MEMS  */
  YYSYMBOL_STRUCT_MEM = 62,                /* STRUCT_MEM  */
  YYSYMBOL_ENUM = 63,                      /* ENUM  */
  YYSYMBOL_ENUM_VARS = 64,                 /* ENUM_VARS  */
  YYSYMBOL_ENUM_VAR = 65,                  /* ENUM_VAR  */
  YYSYMBOL_STMTS = 66,                     /* STMTS  */
  YYSYMBOL_STMT = 67,                      /* STMT  */
  YYSYMBOL_ID = 68,                        /* ID  */
  YYSYMBOL_EXPR = 69,                      /* EXPR  */
  YYSYMBOL_BINARY_EXPR = 70,               /* BINARY_EXPR  */
  YYSYMBOL_UNARY_EXPR = 71,                /* UNARY_EXPR  */
  YYSYMBOL_FUNC_CALL = 72,                 /* FUNC_CALL  */
  YYSYMBOL_INDEX = 73,                     /* INDEX  */
  YYSYMBOL_IF_EXPR = 74,                   /* IF_EXPR  */
  YYSYMBOL_ELIFS = 75,                     /* ELIFS  */
  YYSYMBOL_ELSE = 76,                      /* ELSE  */
  YYSYMBOL_BLOCK_STMTS = 77,               /* BLOCK_STMTS  */
  YYSYMBOL_BLOCK = 78,                     /* BLOCK  */
  YYSYMBOL_CASE_EXPR = 79,                 /* CASE_EXPR  */
  YYSYMBOL_CASE_ARMS = 80,                 /* CASE_ARMS  */
  YYSYMBOL_PATTERN = 81,                   /* PATTERN  */
  YYSYMBOL_CASE_VAL = 82,                  /* CASE_VAL  */
  YYSYMBOL_FUNC_DEF = 83,                  /* FUNC_DEF  */
  YYSYMBOL_FUNC_END = 84,                  /* FUNC_END  */
  YYSYMBOL_FUNC_ARGS = 85,                 /* FUNC_ARGS  */
  YYSYMBOL_FUNC_ARGS_LIST = 86,            /* FUNC_ARGS_LIST  */
  YYSYMBOL_ATOM = 87,                      /* ATOM  */
  YYSYMBOL_LITERAL = 88,                   /* LITERAL  */
  YYSYMBOL_GROUPED = 89,                   /* GROUPED  */
  YYSYMBOL_ARRAY = 90,                     /* ARRAY  */
  YYSYMBOL_EXPR_LIST = 91                  /* EXPR_LIST  */
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
#define YYFINAL  52
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   880

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  58
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  34
/* YYNRULES -- Number of rules.  */
#define YYNRULES  96
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  171

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   312


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
      55,    56,    57
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    96,    96,    97,    98,   100,   102,   103,   104,   106,
     108,   110,   111,   112,   114,   115,   117,   118,   120,   121,
     122,   123,   125,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   160,   161,   162,   164,   165,   167,
     169,   171,   172,   174,   175,   177,   178,   179,   181,   183,
     185,   186,   188,   190,   192,   193,   195,   197,   198,   200,
     201,   202,   204,   205,   206,   207,   209,   210,   211,   212,
     213,   214,   216,   218,   220,   221,   222
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
  "TokenReturn", "TokenStruct", "TokenEnum", "TokenRec", "TokenNil",
  "TokenStr", "TokenNum", "TokenChar", "TokenIdent", "FUNC_END_PREC",
  "TokenAdd", "TokenSub", "TokenMul", "TokenDiv", "UNARY", "$accept",
  "FILE", "STRUCT", "STRUCT_MEMS", "STRUCT_MEM", "ENUM", "ENUM_VARS",
  "ENUM_VAR", "STMTS", "STMT", "ID", "EXPR", "BINARY_EXPR", "UNARY_EXPR",
  "FUNC_CALL", "INDEX", "IF_EXPR", "ELIFS", "ELSE", "BLOCK_STMTS", "BLOCK",
  "CASE_EXPR", "CASE_ARMS", "PATTERN", "CASE_VAL", "FUNC_DEF", "FUNC_END",
  "FUNC_ARGS", "FUNC_ARGS_LIST", "ATOM", "LITERAL", "GROUPED", "ARRAY",
  "EXPR_LIST", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-78)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     730,   826,   826,   730,   826,   826,   -17,   -78,   -78,   826,
     826,   762,   -16,   -78,   -78,   -78,   -78,   -78,   826,     8,
     -78,   730,    15,    98,   -78,   -78,   -78,   -78,   -78,   -78,
     -78,   -78,   -78,   -78,   -78,   -78,   -78,     2,     2,   730,
      98,    -2,   154,    -3,   206,   -20,   258,   310,   -78,   362,
       3,     2,   -78,   -15,   -14,   -78,   -78,   -78,   826,   826,
     826,   826,   826,   826,   826,   826,   826,   826,   826,   826,
     826,   826,   826,   826,   826,   826,   826,   826,   794,   -78,
     826,   826,   826,   826,   -78,   -78,   826,   -78,   -78,     1,
       6,   -78,   -21,    -1,   -78,   -20,    12,    16,   414,     2,
     692,   670,   699,   678,   678,   663,   663,   663,   663,   663,
     663,   605,   634,   576,   576,   547,   547,   466,   -78,    17,
     176,   176,     2,     2,   -78,   -20,    38,    19,    42,   -78,
     826,     5,    21,    28,    33,   -78,   -78,   -78,   -78,   826,
     -78,    25,   310,    25,   -78,    51,    20,    31,    24,    33,
      60,    53,   518,   -78,   -78,   -21,    -1,   -78,   826,    36,
     -78,    28,   -78,   -78,    33,   -78,   -78,   -78,   -78,   -78,
     -78
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
      17,     0,     0,    67,     0,     0,     0,    89,    90,     0,
       0,     0,     0,    91,    87,    86,    88,    22,     0,     0,
       4,    17,    83,     0,    23,    24,    25,    26,    27,    30,
      28,    29,    31,    82,    84,    85,    83,    55,    56,    67,
      66,     0,    96,     0,     0,    78,     0,     0,    20,     0,
       0,    54,     1,     0,     0,     2,     3,    16,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    19,
       0,     0,     0,     0,    65,    68,    95,    93,    92,    81,
       0,    77,    71,    62,    21,    78,     0,     0,     0,    53,
      46,    44,    45,    48,    47,    38,    39,    40,    42,    41,
      43,    36,    37,    34,    35,    32,    33,     0,    58,     0,
      49,    50,    51,    52,    94,    80,     0,     0,     0,    72,
       0,    64,     0,     8,    13,    18,    59,    57,    79,     0,
      69,     0,     0,     0,    60,     0,     0,     0,     7,    15,
       0,    12,    76,    74,    73,    71,    62,    63,     0,     0,
       5,     8,    14,    10,    13,    70,    61,    75,     9,     6,
      11
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -78,   -78,   -78,   -73,   -78,   -78,   -75,   -59,    70,     9,
       4,     0,   -78,   -78,   -78,   -78,   -78,   -63,   -78,    55,
     -41,   -78,   -60,   -78,   -78,   -78,   -66,    23,   -29,   -78,
     -77,   -78,   -78,   -64
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,    19,    55,   147,   148,    56,   150,   151,    20,    21,
      36,    42,    24,    25,    26,    27,    28,   131,   144,    41,
      29,    30,   127,   128,   155,    31,   153,    90,    91,    32,
      33,    34,    35,    43
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      23,    37,    38,    40,    22,    44,    93,    22,    52,    46,
      47,    49,    39,    45,   119,   129,     7,     8,    51,    58,
      50,    23,   124,    85,    87,    22,    13,    14,    15,    16,
      77,    17,    78,    95,   125,   126,    96,    97,   133,    40,
     130,   139,   134,    22,   140,   141,   137,   143,    39,    89,
     145,     3,    53,    54,   158,   159,   160,   161,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   107,   108,   109,
     110,   111,   112,   113,   114,   115,   116,   117,   129,   146,
     120,   121,   122,   123,   149,   163,   164,   168,   169,   170,
     162,    57,   167,   166,    84,   165,   138,     0,     0,    89,
     154,   156,   157,    59,    60,    61,    62,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,   132,    73,
      74,    75,    76,     0,     0,     0,    77,     0,    78,    89,
     142,     0,    79,     0,     0,     0,     0,     0,     0,   152,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,     0,     0,     0,   152,    59,
      60,    61,    62,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,    73,    74,    75,    76,     0,
       0,    59,    77,     0,    78,     0,     0,    86,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,     0,    78,    80,    81,    82,
      83,    59,    60,    61,    62,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,    73,    74,    75,
      76,    82,    83,     0,    77,    88,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,    59,    60,    61,    62,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,    73,
      74,    75,    76,     0,    92,     0,    77,     0,    78,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    80,    81,    82,    83,    59,    60,    61,    62,     0,
      63,    64,    65,    66,    67,    68,    69,    70,    71,    72,
       0,    73,    74,    75,    76,     0,     3,     0,    77,     0,
      78,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    80,    81,    82,    83,    59,    60,    61,
      62,     0,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,     0,    73,    74,    75,    76,     0,     0,     0,
      77,     0,    78,     0,     0,     0,    94,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    80,    81,    82,    83,    59,
      60,    61,    62,     0,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,     0,    73,    74,    75,    76,     0,
       0,     0,    77,     0,    78,     0,     0,     0,   135,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    80,    81,    82,
      83,    59,    60,    61,    62,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,    73,    74,    75,
      76,     0,     0,   136,    77,     0,    78,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    80,
      81,    82,    83,    59,    60,    61,    62,     0,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,    73,
      74,    75,    76,     0,     0,     0,    77,     0,    78,     0,
       0,     0,    59,    60,    61,    62,     0,    63,    64,    65,
      66,    67,    68,    69,    70,    71,    72,     0,    73,    74,
       0,    80,    81,    82,    83,    77,     0,    78,     0,     0,
       0,    59,    60,    61,    62,     0,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    72,     0,     0,     0,     0,
      80,    81,    82,    83,    77,     0,    78,     0,     0,     0,
      59,    60,    61,    62,     0,    63,    64,    65,    66,    67,
      68,    69,    70,     0,    72,     0,     0,     0,     0,    80,
      81,    82,    83,    77,     0,    78,     0,     0,     0,    59,
      60,    61,    62,     0,    63,    64,    65,    66,    67,    68,
      69,    70,     0,     0,     0,     0,     0,     0,    80,    81,
      82,    83,    77,     0,    78,     0,     0,     0,    59,    60,
      61,    62,     0,    63,    64,    59,    60,     0,    62,     0,
      63,    64,     0,    59,     0,     0,     0,    80,    81,    82,
      83,    77,     0,    78,     0,     0,     0,    59,    77,     0,
      78,     0,    63,    64,    59,    60,    77,     0,    78,    63,
      64,     0,     0,     0,     0,     0,    80,    81,    82,    83,
      77,     0,    78,    80,    81,    82,    83,    77,     0,    78,
       0,    80,    81,    82,    83,     0,     0,     0,     0,     1,
       0,     0,     0,     0,     0,    80,    81,    82,    83,     0,
       2,     0,    80,    81,    82,    83,     3,     0,     4,     0,
       5,     0,     0,     0,     0,     0,     6,     7,     8,     9,
      10,     1,     0,    11,     0,     0,    12,    13,    14,    15,
      16,    17,     2,     0,    18,     0,     0,     0,     3,     0,
       4,     0,     5,     0,     0,     0,    48,     0,     6,     7,
       8,     9,    10,     1,     0,     0,     0,     0,    12,    13,
      14,    15,    16,    17,     2,     0,    18,     0,     0,     0,
       3,     0,     4,   118,     5,     0,     0,     0,     0,     0,
       6,     7,     8,     9,    10,     1,     0,     0,     0,     0,
      12,    13,    14,    15,    16,    17,     2,     0,    18,     0,
       0,     0,     3,     0,     4,     0,     5,     0,     0,     0,
       0,     0,     6,     7,     8,     9,    10,     0,     0,     0,
       0,     0,    12,    13,    14,    15,    16,    17,     0,     0,
      18
};

static const yytype_int16 yycheck[] =
{
       0,     1,     2,     3,     0,     5,    47,     3,     0,     9,
      10,    11,     3,    30,    78,    92,    37,    38,    18,     4,
      36,    21,    86,    25,    27,    21,    47,    48,    49,    50,
      28,    51,    30,    30,    33,    29,    51,    51,    26,    39,
      41,     3,    26,    39,    25,     3,    29,    42,    39,    45,
      29,    26,    44,    45,     3,    35,    25,    33,    58,    59,
      60,    61,    62,    63,    64,    65,    66,    67,    68,    69,
      70,    71,    72,    73,    74,    75,    76,    77,   155,    51,
      80,    81,    82,    83,    51,    25,    33,    51,   161,   164,
     149,    21,   158,   156,    39,   155,   125,    -1,    -1,    95,
     141,   142,   143,     5,     6,     7,     8,    -1,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    95,    21,
      22,    23,    24,    -1,    -1,    -1,    28,    -1,    30,   125,
     130,    -1,    34,    -1,    -1,    -1,    -1,    -1,    -1,   139,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,   158,     5,
       6,     7,     8,    -1,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    -1,    21,    22,    23,    24,    -1,
      -1,     5,    28,    -1,    30,    -1,    -1,    33,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    28,    -1,    30,    53,    54,    55,
      56,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    -1,    21,    22,    23,
      24,    55,    56,    -1,    28,    29,    30,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,
      54,    55,    56,     5,     6,     7,     8,    -1,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    26,    -1,    28,    -1,    30,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    53,    54,    55,    56,     5,     6,     7,     8,    -1,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      -1,    21,    22,    23,    24,    -1,    26,    -1,    28,    -1,
      30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    53,    54,    55,    56,     5,     6,     7,
       8,    -1,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      28,    -1,    30,    -1,    -1,    -1,    34,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    53,    54,    55,    56,     5,
       6,     7,     8,    -1,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    28,    -1,    30,    -1,    -1,    -1,    34,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,
      56,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    27,    28,    -1,    30,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,
      54,    55,    56,     5,     6,     7,     8,    -1,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    28,    -1,    30,    -1,
      -1,    -1,     5,     6,     7,     8,    -1,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    -1,    21,    22,
      -1,    53,    54,    55,    56,    28,    -1,    30,    -1,    -1,
      -1,     5,     6,     7,     8,    -1,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    -1,    -1,    -1,    -1,
      53,    54,    55,    56,    28,    -1,    30,    -1,    -1,    -1,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,    16,    17,    -1,    19,    -1,    -1,    -1,    -1,    53,
      54,    55,    56,    28,    -1,    30,    -1,    -1,    -1,     5,
       6,     7,     8,    -1,    10,    11,    12,    13,    14,    15,
      16,    17,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,
      55,    56,    28,    -1,    30,    -1,    -1,    -1,     5,     6,
       7,     8,    -1,    10,    11,     5,     6,    -1,     8,    -1,
      10,    11,    -1,     5,    -1,    -1,    -1,    53,    54,    55,
      56,    28,    -1,    30,    -1,    -1,    -1,     5,    28,    -1,
      30,    -1,    10,    11,     5,     6,    28,    -1,    30,    10,
      11,    -1,    -1,    -1,    -1,    -1,    53,    54,    55,    56,
      28,    -1,    30,    53,    54,    55,    56,    28,    -1,    30,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,    -1,     9,
      -1,    -1,    -1,    -1,    -1,    53,    54,    55,    56,    -1,
      20,    -1,    53,    54,    55,    56,    26,    -1,    28,    -1,
      30,    -1,    -1,    -1,    -1,    -1,    36,    37,    38,    39,
      40,     9,    -1,    43,    -1,    -1,    46,    47,    48,    49,
      50,    51,    20,    -1,    54,    -1,    -1,    -1,    26,    -1,
      28,    -1,    30,    -1,    -1,    -1,    34,    -1,    36,    37,
      38,    39,    40,     9,    -1,    -1,    -1,    -1,    46,    47,
      48,    49,    50,    51,    20,    -1,    54,    -1,    -1,    -1,
      26,    -1,    28,    29,    30,    -1,    -1,    -1,    -1,    -1,
      36,    37,    38,    39,    40,     9,    -1,    -1,    -1,    -1,
      46,    47,    48,    49,    50,    51,    20,    -1,    54,    -1,
      -1,    -1,    26,    -1,    28,    -1,    30,    -1,    -1,    -1,
      -1,    -1,    36,    37,    38,    39,    40,    -1,    -1,    -1,
      -1,    -1,    46,    47,    48,    49,    50,    51,    -1,    -1,
      54
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     9,    20,    26,    28,    30,    36,    37,    38,    39,
      40,    43,    46,    47,    48,    49,    50,    51,    54,    59,
      66,    67,    68,    69,    70,    71,    72,    73,    74,    78,
      79,    83,    87,    88,    89,    90,    68,    69,    69,    67,
      69,    77,    69,    91,    69,    30,    69,    69,    34,    69,
      36,    69,     0,    44,    45,    60,    63,    66,     4,     5,
       6,     7,     8,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    21,    22,    23,    24,    28,    30,    34,
      53,    54,    55,    56,    77,    25,    33,    27,    29,    68,
      85,    86,    26,    78,    34,    30,    51,    51,    69,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    69,    69,
      69,    69,    69,    69,    69,    69,    69,    69,    29,    91,
      69,    69,    69,    69,    91,    33,    29,    80,    81,    88,
      41,    75,    85,    26,    26,    34,    27,    29,    86,     3,
      25,     3,    69,    42,    76,    29,    51,    61,    62,    51,
      64,    65,    69,    84,    78,    82,    78,    78,     3,    35,
      25,    33,    65,    25,    33,    80,    75,    84,    51,    61,
      64
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    58,    59,    59,    59,    60,    61,    61,    61,    62,
      63,    64,    64,    64,    65,    65,    66,    66,    67,    67,
      67,    67,    68,    69,    69,    69,    69,    69,    69,    69,
      69,    69,    70,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    70,    70,    70,    70,    70,    70,
      70,    70,    70,    70,    71,    71,    71,    72,    72,    73,
      74,    75,    75,    76,    76,    77,    77,    77,    78,    79,
      80,    80,    81,    82,    83,    83,    84,    85,    85,    86,
      86,    86,    87,    87,    87,    87,    88,    88,    88,    88,
      88,    88,    89,    90,    91,    91,    91
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     2,     1,     5,     3,     1,     0,     3,
       5,     3,     1,     0,     2,     1,     2,     0,     4,     2,
       2,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     2,     2,     2,     4,     3,     4,
       5,     4,     0,     2,     0,     2,     1,     0,     3,     5,
       4,     0,     1,     1,     6,     7,     1,     1,     0,     3,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     3,     2,     1
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
#line 98 "./parsing/parser.y"
                                { *parse_node = (yyvsp[0].node); }
#line 1431 "./parsing/built/parser.tab.c"
    break;

  case 16: /* STMTS: STMT STMTS  */
#line 117 "./parsing/parser.y"
                   { (yyval.node) = new_astnode(AstntStmts); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = (yyvsp[0].node); }
#line 1437 "./parsing/built/parser.tab.c"
    break;

  case 17: /* STMTS: %empty  */
#line 118 "./parsing/parser.y"
                           { (yyval.node) = NULL; }
#line 1443 "./parsing/built/parser.tab.c"
    break;

  case 18: /* STMT: ID TokenDefine EXPR TokenSemicolon  */
#line 120 "./parsing/parser.y"
                                          { (yyval.node) = new_astnode(AstntAssignStmt); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); }
#line 1449 "./parsing/built/parser.tab.c"
    break;

  case 19: /* STMT: EXPR TokenSemicolon  */
#line 121 "./parsing/parser.y"
                                                          { (yyval.node) = new_astnode(AstntExprStmt);   (yyval.node)->kids[0] = (yyvsp[-1].node);  				   }
#line 1455 "./parsing/built/parser.tab.c"
    break;

  case 20: /* STMT: TokenReturn TokenSemicolon  */
#line 122 "./parsing/parser.y"
                                                  { (yyval.node) = new_astnode(AstntReturn); (yyval.node)->kids[0] = NULL; 					   }
#line 1461 "./parsing/built/parser.tab.c"
    break;

  case 21: /* STMT: TokenReturn EXPR TokenSemicolon  */
#line 123 "./parsing/parser.y"
                                                  { (yyval.node) = new_astnode(AstntReturn); (yyval.node)->kids[0] = (yyvsp[-1].node); 					   }
#line 1467 "./parsing/built/parser.tab.c"
    break;

  case 22: /* ID: TokenIdent  */
#line 125 "./parsing/parser.y"
               { (yyval.node) = new_astnode(AstntIdent); (yyval.node)->val.i = (yyvsp[0].ident); }
#line 1473 "./parsing/built/parser.tab.c"
    break;

  case 23: /* EXPR: BINARY_EXPR  */
#line 127 "./parsing/parser.y"
                  { (yyval.node) = (yyvsp[0].node); }
#line 1479 "./parsing/built/parser.tab.c"
    break;

  case 24: /* EXPR: UNARY_EXPR  */
#line 128 "./parsing/parser.y"
                      { (yyval.node) = (yyvsp[0].node); }
#line 1485 "./parsing/built/parser.tab.c"
    break;

  case 25: /* EXPR: FUNC_CALL  */
#line 129 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1491 "./parsing/built/parser.tab.c"
    break;

  case 26: /* EXPR: INDEX  */
#line 130 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1497 "./parsing/built/parser.tab.c"
    break;

  case 27: /* EXPR: IF_EXPR  */
#line 131 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1503 "./parsing/built/parser.tab.c"
    break;

  case 28: /* EXPR: CASE_EXPR  */
#line 132 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1509 "./parsing/built/parser.tab.c"
    break;

  case 29: /* EXPR: FUNC_DEF  */
#line 133 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1515 "./parsing/built/parser.tab.c"
    break;

  case 30: /* EXPR: BLOCK  */
#line 134 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1521 "./parsing/built/parser.tab.c"
    break;

  case 31: /* EXPR: ATOM  */
#line 135 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node); }
#line 1527 "./parsing/built/parser.tab.c"
    break;

  case 32: /* BINARY_EXPR: EXPR TokenLApply EXPR  */
#line 137 "./parsing/parser.y"
                                     { (yyval.node) = new_binary_astnode(AstntBinaryLApply,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1533 "./parsing/built/parser.tab.c"
    break;

  case 33: /* BINARY_EXPR: EXPR TokenRApply EXPR  */
#line 138 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryRApply,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1539 "./parsing/built/parser.tab.c"
    break;

  case 34: /* BINARY_EXPR: EXPR TokenLCompose EXPR  */
#line 139 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLCompose, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1545 "./parsing/built/parser.tab.c"
    break;

  case 35: /* BINARY_EXPR: EXPR TokenRCompose EXPR  */
#line 140 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryRCompose, (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1551 "./parsing/built/parser.tab.c"
    break;

  case 36: /* BINARY_EXPR: EXPR TokenLogAnd EXPR  */
#line 141 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLogAnd,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1557 "./parsing/built/parser.tab.c"
    break;

  case 37: /* BINARY_EXPR: EXPR TokenLogOr EXPR  */
#line 142 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLogOr,    (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1563 "./parsing/built/parser.tab.c"
    break;

  case 38: /* BINARY_EXPR: EXPR TokenEq EXPR  */
#line 143 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryEq,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1569 "./parsing/built/parser.tab.c"
    break;

  case 39: /* BINARY_EXPR: EXPR TokenNe EXPR  */
#line 144 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryNe,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1575 "./parsing/built/parser.tab.c"
    break;

  case 40: /* BINARY_EXPR: EXPR TokenGt EXPR  */
#line 145 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryGt,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1581 "./parsing/built/parser.tab.c"
    break;

  case 41: /* BINARY_EXPR: EXPR TokenGe EXPR  */
#line 146 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryGe,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1587 "./parsing/built/parser.tab.c"
    break;

  case 42: /* BINARY_EXPR: EXPR TokenLt EXPR  */
#line 147 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLt,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1593 "./parsing/built/parser.tab.c"
    break;

  case 43: /* BINARY_EXPR: EXPR TokenLe EXPR  */
#line 148 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLe,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1599 "./parsing/built/parser.tab.c"
    break;

  case 44: /* BINARY_EXPR: EXPR TokenOr EXPR  */
#line 149 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryOr,       (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1605 "./parsing/built/parser.tab.c"
    break;

  case 45: /* BINARY_EXPR: EXPR TokenXor EXPR  */
#line 150 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryXor,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1611 "./parsing/built/parser.tab.c"
    break;

  case 46: /* BINARY_EXPR: EXPR TokenAnd EXPR  */
#line 151 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryAnd,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1617 "./parsing/built/parser.tab.c"
    break;

  case 47: /* BINARY_EXPR: EXPR TokenRShift EXPR  */
#line 152 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryRShift,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1623 "./parsing/built/parser.tab.c"
    break;

  case 48: /* BINARY_EXPR: EXPR TokenLShift EXPR  */
#line 153 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryLShift,   (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1629 "./parsing/built/parser.tab.c"
    break;

  case 49: /* BINARY_EXPR: EXPR TokenAdd EXPR  */
#line 154 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryAdd,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1635 "./parsing/built/parser.tab.c"
    break;

  case 50: /* BINARY_EXPR: EXPR TokenSub EXPR  */
#line 155 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinarySub,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1641 "./parsing/built/parser.tab.c"
    break;

  case 51: /* BINARY_EXPR: EXPR TokenMul EXPR  */
#line 156 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryMul,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1647 "./parsing/built/parser.tab.c"
    break;

  case 52: /* BINARY_EXPR: EXPR TokenDiv EXPR  */
#line 157 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryDiv,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1653 "./parsing/built/parser.tab.c"
    break;

  case 53: /* BINARY_EXPR: EXPR TokenMod EXPR  */
#line 158 "./parsing/parser.y"
                                             { (yyval.node) = new_binary_astnode(AstntBinaryMod,      (yyvsp[-2].node), (yyvsp[0].node)); }
#line 1659 "./parsing/built/parser.tab.c"
    break;

  case 54: /* UNARY_EXPR: TokenSub EXPR  */
#line 160 "./parsing/parser.y"
                                         { (yyval.node) = new_unary_astnode(AstntUnaryNeg, (yyvsp[0].node));    }
#line 1665 "./parsing/built/parser.tab.c"
    break;

  case 55: /* UNARY_EXPR: TokenNeg EXPR  */
#line 161 "./parsing/parser.y"
                                                 { (yyval.node) = new_unary_astnode(AstntUnaryBitNot, (yyvsp[0].node)); }
#line 1671 "./parsing/built/parser.tab.c"
    break;

  case 56: /* UNARY_EXPR: TokenLogNot EXPR  */
#line 162 "./parsing/parser.y"
                                                 { (yyval.node) = new_unary_astnode(AstntUnaryLogNot, (yyvsp[0].node)); }
#line 1677 "./parsing/built/parser.tab.c"
    break;

  case 57: /* FUNC_CALL: EXPR TokenLParen EXPR_LIST TokenRParen  */
#line 164 "./parsing/parser.y"
                                                  { (yyval.node) = new_astnode(AstntFuncCall); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node);   }
#line 1683 "./parsing/built/parser.tab.c"
    break;

  case 58: /* FUNC_CALL: EXPR TokenLParen TokenRParen  */
#line 165 "./parsing/parser.y"
                                                                  { (yyval.node) = new_astnode(AstntFuncCall); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = NULL; }
#line 1689 "./parsing/built/parser.tab.c"
    break;

  case 59: /* INDEX: EXPR TokenLBrack EXPR TokenRBrack  */
#line 167 "./parsing/parser.y"
                                         { (yyval.node) = new_astnode(AstntArrayIndex); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); }
#line 1695 "./parsing/built/parser.tab.c"
    break;

  case 60: /* IF_EXPR: TokenIf EXPR BLOCK ELIFS ELSE  */
#line 169 "./parsing/parser.y"
                                       { (yyval.node) = new_astnode(AstntIf); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-2].node); (yyval.node)->kids[2] = (yyvsp[-1].node); (yyval.node)->kids[3] = (yyvsp[0].node); }
#line 1701 "./parsing/built/parser.tab.c"
    break;

  case 61: /* ELIFS: TokenElif EXPR BLOCK ELIFS  */
#line 171 "./parsing/parser.y"
                                  { (yyval.node) = new_astnode(AstntElif); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = (yyvsp[-1].node); (yyval.node)->kids[2] = (yyvsp[0].node); }
#line 1707 "./parsing/built/parser.tab.c"
    break;

  case 62: /* ELIFS: %empty  */
#line 172 "./parsing/parser.y"
                                                                  { (yyval.node) = NULL; }
#line 1713 "./parsing/built/parser.tab.c"
    break;

  case 63: /* ELSE: TokenElse BLOCK  */
#line 174 "./parsing/parser.y"
                                          { (yyval.node) = new_astnode(AstntElse); (yyval.node)->kids[0] = (yyvsp[0].node); }
#line 1719 "./parsing/built/parser.tab.c"
    break;

  case 64: /* ELSE: %empty  */
#line 175 "./parsing/parser.y"
                                                                  { (yyval.node) = NULL; }
#line 1725 "./parsing/built/parser.tab.c"
    break;

  case 65: /* BLOCK_STMTS: STMT BLOCK_STMTS  */
#line 177 "./parsing/parser.y"
                                { (yyval.node) = new_astnode(AstntStmts); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = (yyvsp[0].node); }
#line 1731 "./parsing/built/parser.tab.c"
    break;

  case 66: /* BLOCK_STMTS: EXPR  */
#line 178 "./parsing/parser.y"
                                                        { (yyval.node) = (yyvsp[0].node);										 					}
#line 1737 "./parsing/built/parser.tab.c"
    break;

  case 67: /* BLOCK_STMTS: %empty  */
#line 179 "./parsing/parser.y"
                                                        { (yyval.node) = NULL; 														}
#line 1743 "./parsing/built/parser.tab.c"
    break;

  case 68: /* BLOCK: TokenLBrace BLOCK_STMTS TokenRBrace  */
#line 181 "./parsing/parser.y"
                                           { (yyval.node) = new_astnode(AstntBlockStmt); (yyval.node)->kids[0] = (yyvsp[-1].node); }
#line 1749 "./parsing/built/parser.tab.c"
    break;

  case 69: /* CASE_EXPR: TokenCase EXPR TokenLBrace CASE_ARMS TokenRBrace  */
#line 183 "./parsing/parser.y"
                                                                { (yyval.node) = new_astnode(AstntCase); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); }
#line 1755 "./parsing/built/parser.tab.c"
    break;

  case 70: /* CASE_ARMS: PATTERN TokenArrow CASE_VAL CASE_ARMS  */
#line 185 "./parsing/parser.y"
                                                 { (yyval.node) = new_astnode(AstntCaseArm); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[-1].node); (yyval.node)->kids[2] = (yyvsp[0].node); }
#line 1761 "./parsing/built/parser.tab.c"
    break;

  case 71: /* CASE_ARMS: %empty  */
#line 186 "./parsing/parser.y"
                                                                                                 { (yyval.node) = NULL; 		  																     }
#line 1767 "./parsing/built/parser.tab.c"
    break;

  case 72: /* PATTERN: LITERAL  */
#line 188 "./parsing/parser.y"
                 { (yyval.node) = (yyvsp[0].node); }
#line 1773 "./parsing/built/parser.tab.c"
    break;

  case 73: /* CASE_VAL: BLOCK  */
#line 190 "./parsing/parser.y"
                                  { (yyval.node) = (yyvsp[0].node); }
#line 1779 "./parsing/built/parser.tab.c"
    break;

  case 74: /* FUNC_DEF: TokenFn TokenLParen FUNC_ARGS TokenRParen TokenArrow FUNC_END  */
#line 192 "./parsing/parser.y"
                                                                                         { (yyval.node) = new_astnode(AstntFuncDef); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[0].node); (yyval.node)->kids[2] = new_astnode(AstntBoolLit); (yyval.node)->kids[2]->val.b = false; }
#line 1785 "./parsing/built/parser.tab.c"
    break;

  case 75: /* FUNC_DEF: TokenRec TokenFn TokenLParen FUNC_ARGS TokenRParen TokenArrow FUNC_END  */
#line 193 "./parsing/parser.y"
                                                                                     { (yyval.node) = new_astnode(AstntFuncDef); (yyval.node)->kids[0] = (yyvsp[-3].node); (yyval.node)->kids[1] = (yyvsp[0].node); (yyval.node)->kids[2] = new_astnode(AstntBoolLit); (yyval.node)->kids[2]->val.b = true;  }
#line 1791 "./parsing/built/parser.tab.c"
    break;

  case 76: /* FUNC_END: EXPR  */
#line 195 "./parsing/parser.y"
                                   { (yyval.node) = (yyvsp[0].node); }
#line 1797 "./parsing/built/parser.tab.c"
    break;

  case 77: /* FUNC_ARGS: FUNC_ARGS_LIST  */
#line 197 "./parsing/parser.y"
                          { (yyval.node) = (yyvsp[0].node);   }
#line 1803 "./parsing/built/parser.tab.c"
    break;

  case 78: /* FUNC_ARGS: %empty  */
#line 198 "./parsing/parser.y"
                                                  { (yyval.node) = NULL; }
#line 1809 "./parsing/built/parser.tab.c"
    break;

  case 79: /* FUNC_ARGS_LIST: ID TokenComma FUNC_ARGS_LIST  */
#line 200 "./parsing/parser.y"
                                             { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = (yyvsp[0].node);   }
#line 1815 "./parsing/built/parser.tab.c"
    break;

  case 80: /* FUNC_ARGS_LIST: ID TokenComma  */
#line 201 "./parsing/parser.y"
                                                                         { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = NULL; }
#line 1821 "./parsing/built/parser.tab.c"
    break;

  case 81: /* FUNC_ARGS_LIST: ID  */
#line 202 "./parsing/parser.y"
                                                                                 { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[0].node); (yyval.node)->kids[1] = NULL; }
#line 1827 "./parsing/built/parser.tab.c"
    break;

  case 82: /* ATOM: LITERAL  */
#line 204 "./parsing/parser.y"
              { (yyval.node) = (yyvsp[0].node); }
#line 1833 "./parsing/built/parser.tab.c"
    break;

  case 83: /* ATOM: ID  */
#line 205 "./parsing/parser.y"
                  { (yyval.node) = (yyvsp[0].node); }
#line 1839 "./parsing/built/parser.tab.c"
    break;

  case 84: /* ATOM: GROUPED  */
#line 206 "./parsing/parser.y"
                  { (yyval.node) = (yyvsp[0].node); }
#line 1845 "./parsing/built/parser.tab.c"
    break;

  case 86: /* LITERAL: TokenNum  */
#line 209 "./parsing/parser.y"
                    { (yyval.node) = new_astnode(AstntNumLit);  (yyval.node)->val.n = (yyvsp[0].num);    }
#line 1851 "./parsing/built/parser.tab.c"
    break;

  case 87: /* LITERAL: TokenStr  */
#line 210 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntStrLit);  (yyval.node)->val.s = (yyvsp[0].str);    }
#line 1857 "./parsing/built/parser.tab.c"
    break;

  case 88: /* LITERAL: TokenChar  */
#line 211 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntCharLit); (yyval.node)->val.c = (yyvsp[0].ch);    }
#line 1863 "./parsing/built/parser.tab.c"
    break;

  case 89: /* LITERAL: TokenTrue  */
#line 212 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntBoolLit); (yyval.node)->val.b = true;  }
#line 1869 "./parsing/built/parser.tab.c"
    break;

  case 90: /* LITERAL: TokenFalse  */
#line 213 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntBoolLit); (yyval.node)->val.b = false; }
#line 1875 "./parsing/built/parser.tab.c"
    break;

  case 91: /* LITERAL: TokenNil  */
#line 214 "./parsing/parser.y"
                        { (yyval.node) = new_astnode(AstntNilLit); 					 }
#line 1881 "./parsing/built/parser.tab.c"
    break;

  case 92: /* GROUPED: TokenLParen EXPR TokenRParen  */
#line 216 "./parsing/parser.y"
                                      { (yyval.node) = (yyvsp[-1].node); }
#line 1887 "./parsing/built/parser.tab.c"
    break;

  case 93: /* ARRAY: TokenLBrack EXPR_LIST TokenRBrack  */
#line 218 "./parsing/parser.y"
                                         { (yyval.node) = new_astnode(AstntArray); (yyval.node)->kids[0] = (yyvsp[-1].node); }
#line 1893 "./parsing/built/parser.tab.c"
    break;

  case 94: /* EXPR_LIST: EXPR TokenComma EXPR_LIST  */
#line 220 "./parsing/parser.y"
                                     { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-2].node); (yyval.node)->kids[1] = (yyvsp[0].node);   }
#line 1899 "./parsing/built/parser.tab.c"
    break;

  case 95: /* EXPR_LIST: EXPR TokenComma  */
#line 221 "./parsing/parser.y"
                                                         { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[-1].node); (yyval.node)->kids[1] = NULL; }
#line 1905 "./parsing/built/parser.tab.c"
    break;

  case 96: /* EXPR_LIST: EXPR  */
#line 222 "./parsing/parser.y"
                                                                 { (yyval.node) = new_astnode(AstntNodeList); (yyval.node)->kids[0] = (yyvsp[0].node); (yyval.node)->kids[1] = NULL; }
#line 1911 "./parsing/built/parser.tab.c"
    break;


#line 1915 "./parsing/built/parser.tab.c"

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

#line 224 "./parsing/parser.y"

