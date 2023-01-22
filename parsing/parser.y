%{
	#include <stdio.h>
	#include "../../types.h"
	#include "../../ast/ast.h"

	void yyerror(AstNode** node, char* msg) {
		fprintf(stderr, "Parse Error: %s\n", msg);
	}
	
	extern int yylex(void);
%}

%union { 
	AstNode* node;
	string str;
	string ident;
	i64 num;
	char ch;
}

%token TokenArrow
%token TokenDefine
%token TokenMod
%token TokenAnd
%token TokenOr
%token TokenXor
%token TokenNeg
%token TokenLShift
%token TokenRShift
%token TokenEq
%token TokenNe
%token TokenGt
%token TokenLt
%token TokenGe
%token TokenLe
%token TokenLogAnd
%token TokenLogOr
%token TokenLogNot
%token TokenLCompose
%token TokenRCompose
%token TokenLApply
%token TokenRApply
%token TokenRBrace
%token TokenLBrace
%token TokenRBrack
%token TokenLBrack
%token TokenRParen
%token TokenLParen
%token TokenDoubleQuote
%token TokenSingleQuote
%token TokenComma
%token TokenSemicolon
%token TokenColon
%token TokenFn
%token TokenTrue
%token TokenFalse
%token TokenCase
%token TokenIf
%token TokenElif
%token TokenElse
%token TokenReturn
%token TokenStruct
%token TokenEnum
%token <str>   TokenStr
%token <num>   TokenNum
%token <ch>    TokenChar
%token <ident> TokenIdent

%left FUNC_END_PREC
%left TokenRApply TokenLApply
%left TokenRCompose TokenLCompose
%left TokenLogAnd
%left TokenLogOr
%left TokenEq  TokenNe  TokenGt  TokenLt  TokenGe  TokenLe
%left TokenOr
%left TokenXor
%left TokenAnd
%left TokenRShift TokenLShift
%left TokenAdd TokenSub
%left TokenMul TokenDiv TokenMod
%right UNARY 
%right TokenLParen TokenLBrack

%code requires { 
	#include "../../ast/ast.h" 
	#include "../../ast/optimization.h" 
}

%parse-param { AstNode** parse_node }

%type <node> LITERAL ATOM EXPR STMT STMTS ID GROUPED BLOCK IF_EXPR ELIFS ELSE CASE_EXPR CASE_ARMS PATTERN CASE_VAL ARRAY EXPR_LIST FUNC_DEF FUNC_ARGS FUNC_ARGS_LIST FUNC_CALL UNARY_EXPR INDEX FUNC_END BINARY_EXPR FILE
%%

FILE: FILE STRUCT 
	| FILE ENUM
	| STMTS 		{ *parse_node = $1; }

STRUCT: TokenStruct TokenIdent TokenLBrace STRUCT_MEMS TokenRBrace

STRUCT_MEMS: STRUCT_MEM TokenComma STRUCT_MEMS
		   | STRUCT_MEM
		   | 

STRUCT_MEM: TokenIdent TokenColon TokenIdent
		
ENUM: TokenEnum TokenIdent TokenLBrace ENUM_VARS TokenRBrace 

ENUM_VARS: ENUM_VAR TokenComma ENUM_VARS
		 | ENUM_VAR
		 | 

ENUM_VAR: TokenIdent ENUM_VAR
		| TokenIdent

STMTS : STMT STMTS { $$ = new_astnode(AstntStmts); $$->kids[0] = $1; $$->kids[1] = $2; }
	  | 	   	   { $$ = NULL; }

STMT : ID TokenDefine EXPR TokenSemicolon { $$ = new_astnode(AstntAssignStmt); $$->kids[0] = $1; $$->kids[1] = $3; }
	 | EXPR TokenSemicolon				  { $$ = new_astnode(AstntExprStmt);   $$->kids[0] = $1;  				   }
	 | BLOCK TokenSemicolon				  { $$ = new_astnode(AstntBlockStmt);  $$->kids[0] = $1; 				   }

ID: TokenIdent { $$ = new_astnode(AstntIdent); $$->val.i = $1; }

EXPR: BINARY_EXPR { $$ = $1; }
	| UNARY_EXPR  { $$ = $1; }
	| FUNC_CALL	  { $$ = $1; }
	| INDEX		  { $$ = $1; }
	| IF_EXPR	  { $$ = $1; }
	| CASE_EXPR	  { $$ = $1; }
	| FUNC_DEF	  { $$ = $1; }
	| ATOM		  { $$ = $1; }
	
BINARY_EXPR: EXPR TokenLApply EXPR   { $$ = new_binary_astnode(AstntBinaryLApply,   $1, $3); }
		   | EXPR TokenRApply EXPR   { $$ = new_binary_astnode(AstntBinaryRApply,   $1, $3); }
		   | EXPR TokenLCompose EXPR { $$ = new_binary_astnode(AstntBinaryLCompose, $1, $3); }
		   | EXPR TokenRCompose EXPR { $$ = new_binary_astnode(AstntBinaryRCompose, $1, $3); }
		   | EXPR TokenLogAnd EXPR   { $$ = new_binary_astnode(AstntBinaryLogAnd,   $1, $3); }
		   | EXPR TokenLogOr EXPR    { $$ = new_binary_astnode(AstntBinaryLogOr,    $1, $3); }
		   | EXPR TokenEq EXPR       { $$ = new_binary_astnode(AstntBinaryEq,       $1, $3); }
		   | EXPR TokenNe EXPR       { $$ = new_binary_astnode(AstntBinaryNe,       $1, $3); }
		   | EXPR TokenGt EXPR       { $$ = new_binary_astnode(AstntBinaryGt,       $1, $3); }
		   | EXPR TokenGe EXPR       { $$ = new_binary_astnode(AstntBinaryGe,       $1, $3); }
		   | EXPR TokenLt EXPR       { $$ = new_binary_astnode(AstntBinaryLt,       $1, $3); }
		   | EXPR TokenLe EXPR       { $$ = new_binary_astnode(AstntBinaryLe,       $1, $3); }
		   | EXPR TokenOr EXPR       { $$ = new_binary_astnode(AstntBinaryOr,       $1, $3); }
		   | EXPR TokenXor EXPR      { $$ = new_binary_astnode(AstntBinaryXor,      $1, $3); }
		   | EXPR TokenAnd EXPR      { $$ = new_binary_astnode(AstntBinaryAnd,      $1, $3); }
		   | EXPR TokenRShift EXPR   { $$ = new_binary_astnode(AstntBinaryRShift,   $1, $3); }
		   | EXPR TokenLShift EXPR   { $$ = new_binary_astnode(AstntBinaryLShift,   $1, $3); }
		   | EXPR TokenAdd EXPR      { $$ = new_binary_astnode(AstntBinaryAdd,      $1, $3); }
		   | EXPR TokenSub EXPR      { $$ = new_binary_astnode(AstntBinarySub,      $1, $3); }
		   | EXPR TokenMul EXPR      { $$ = new_binary_astnode(AstntBinaryMul,      $1, $3); }
		   | EXPR TokenDiv EXPR      { $$ = new_binary_astnode(AstntBinaryDiv,      $1, $3); }
		   | EXPR TokenMod EXPR      { $$ = new_binary_astnode(AstntBinaryMod,      $1, $3); } 
	
UNARY_EXPR: TokenSub    %prec UNARY EXPR { $$ = new_unary_astnode(AstntUnaryNeg, $2);    }
		  | TokenNeg	%prec UNARY EXPR { $$ = new_unary_astnode(AstntUnaryBitNot, $2); }
		  | TokenLogNot %prec UNARY EXPR { $$ = new_unary_astnode(AstntUnaryLogNot, $2); }

FUNC_CALL: EXPR TokenLParen EXPR_LIST TokenRParen { $$ = new_astnode(AstntFuncCall); $$->kids[0] = $1; $$->kids[1] = $3;   }
		 | EXPR TokenLParen TokenRParen			  { $$ = new_astnode(AstntFuncCall); $$->kids[0] = $1; $$->kids[1] = NULL; }
		
INDEX: EXPR TokenLBrack EXPR TokenRBrack { $$ = new_astnode(AstntArrayIndex); $$->kids[0] = $1; $$->kids[1] = $3; }

IF_EXPR: TokenIf EXPR BLOCK ELIFS ELSE { $$ = new_astnode(AstntIf); $$->kids[0] = $2; $$->kids[1] = $3; $$->kids[2] = $4; $$->kids[3] = $5; }

ELIFS: TokenElif EXPR BLOCK ELIFS { $$ = new_astnode(AstntElif); $$->kids[0] = $2; $$->kids[1] = $3; $$->kids[2] = $4; }
	 |							  { $$ = NULL; }

ELSE: TokenElse BLOCK			  { $$ = new_astnode(AstntElse); $$->kids[0] = $2; }
	|							  { $$ = NULL; }

BLOCK: TokenLBrace STMTS TokenRBrace { $$ = new_astnode(AstntBlockStmt); $$->kids[0] = $2; }

CASE_EXPR: TokenCase EXPR TokenLBrace CASE_ARMS TokenRBrace	{ $$ = new_astnode(AstntCase); $$->kids[0] = $2; $$->kids[1] = $4; }

CASE_ARMS: PATTERN TokenArrow CASE_VAL CASE_ARMS { $$ = new_astnode(AstntCaseArm); $$->kids[0] = $1; $$->kids[1] = $3; $$->kids[2] = $4; }
		 |										 { $$ = NULL; 		  																     }
		
PATTERN: LITERAL { $$ = $1; }
	   | ID 	 { $$ = $1; }

CASE_VAL: EXPR TokenComma { $$ = $1; }
		| BLOCK			  { $$ = $1; }

FUNC_DEF: TokenFn TokenLParen FUNC_ARGS TokenRParen TokenArrow FUNC_END { $$ = new_astnode(AstntFuncDef); $$->kids[0] = $3; $$->kids[1] = $6; }

FUNC_END: BLOCK %prec FUNC_END_PREC { $$ = $1; }
		| EXPR  %prec FUNC_END_PREC { $$ = $1; }

FUNC_ARGS: FUNC_ARGS_LIST { $$ = $1;   }
		 | 				  { $$ = NULL; }

FUNC_ARGS_LIST: ID TokenComma FUNC_ARGS_LIST { $$ = new_astnode(AstntNodeList); $$->kids[0] = $1; $$->kids[1] = $3;   }
			  | ID TokenComma				 { $$ = new_astnode(AstntNodeList); $$->kids[0] = $1; $$->kids[1] = NULL; }
			  | ID 							 { $$ = new_astnode(AstntNodeList); $$->kids[0] = $1; $$->kids[1] = NULL; }

ATOM: LITERAL { $$ = $1; }
	| ID	  { $$ = $1; }
	| GROUPED { $$ = $1; }
	| ARRAY

LITERAL: TokenNum   { $$ = new_astnode(AstntNumLit);  $$->val.n = $1;    }
	   | TokenStr   { $$ = new_astnode(AstntStrLit);  $$->val.s = $1;    }
	   | TokenChar  { $$ = new_astnode(AstntCharLit); $$->val.c = $1;    }
	   | TokenTrue  { $$ = new_astnode(AstntBoolLit); $$->val.b = true;  }
	   | TokenFalse { $$ = new_astnode(AstntBoolLit); $$->val.b = false; }

GROUPED: TokenLParen EXPR TokenRParen { $$ = $2; }

ARRAY: TokenLBrack EXPR_LIST TokenRBrack { $$ = new_astnode(AstntArray); $$->kids[0] = $2; }

EXPR_LIST: EXPR TokenComma EXPR_LIST { $$ = new_astnode(AstntNodeList); $$->kids[0] = $1; $$->kids[1] = $3;   }
	  	 | EXPR	TokenComma			 { $$ = new_astnode(AstntNodeList); $$->kids[0] = $1; $$->kids[1] = NULL; }
		 | EXPR 					 { $$ = new_astnode(AstntNodeList); $$->kids[0] = $1; $$->kids[1] = NULL; }

%%
