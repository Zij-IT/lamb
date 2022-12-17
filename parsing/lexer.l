%{
	#include <stdint.h>
	#include <string.h>
	#include <stdio.h>
	#include "../lexer.h"

	static void string_push(char ch);
%}

%option outfile="./parsing/built/lexer.c"
%option nounput
%option noinput

%x STRING COMMENT

%%

"--[["			{ BEGIN(COMMENT); }
"--".*			{ }
<COMMENT>(.|\n)	{ }
<COMMENT>"]]--" { BEGIN(INITIAL); }

"\"" 	 	  { BEGIN(STRING); yylval.str = malloc(sizeof(char*)); yylval.str[0] = '\0'; }
<STRING>"::"  { string_push(':');  }
<STRING>":\"" { string_push('"');  }
<STRING>":n"  { string_push('\n'); }
<STRING>"\""  { BEGIN(INITIAL); return TokenStr; }
<STRING>.	  { string_push(*yytext);  }

\'.\' 	{ yylval.ch = yytext[1]; return TokenChar; }
"':n'"	{ yylval.ch = '\n'; 	 return TokenChar; }
"'::'"	{ yylval.ch = ':';  	 return TokenChar; }
"':''"	{ yylval.ch = '\''; 	 return TokenChar; }

"0b"[01]+ 			   { yylval.num = strtol(yytext + 2, NULL,  2); return TokenNum; }
"0o"[0-7]+ 			   { yylval.num = strtol(yytext + 2, NULL,  8); return TokenNum; }
"0d"[0-9]+ 			   { yylval.num = strtol(yytext + 2, NULL, 10); return TokenNum; }
"0h"[0-9abcdefABCDEF]+ { yylval.num = strtol(yytext + 2, NULL, 16); return TokenNum; }
([1-9][0-9]*|0) 	   { yylval.num = strtol(yytext + 0, NULL, 10); return TokenNum; }

".>"	 { return TokenRCompose;  }
"<."	 { return TokenLCompose;  }
"$>"	 { return TokenRApply;    }
"<$"	 { return TokenLApply;    }
"+"  	 { return TokenAdd;       }
"-"  	 { return TokenSub;     }
"*"  	 { return TokenMul;       }
"/"  	 { return TokenDiv;       }
"%"  	 { return TokenMod;       }
"&"  	 { return TokenAnd;       }
"|"  	 { return TokenOr;        }
"^"  	 { return TokenXor;       }
"~"  	 { return TokenNeg;       }
"<<"  	 { return TokenLShift;    }
">>"  	 { return TokenRShift;    }
"="  	 { return TokenEq;        }
"!="  	 { return TokenNe;        }
">"  	 { return TokenGt;        }
"<"  	 { return TokenLt;        }
">="  	 { return TokenGe;        }
"<="  	 { return TokenLe;        }
"&&"  	 { return TokenLogAnd;    }
"||"  	 { return TokenLogOr;     }
"!"  	 { return TokenLogNot;    }
":="  	 { return TokenDefine;    }
"->"  	 { return TokenArrow;     }
"}"  	 { return TokenRBrace;    }
"{"  	 { return TokenLBrace;    }
"]"  	 { return TokenRBrack;    }
"["  	 { return TokenLBrack;    }
")"  	 { return TokenRParen;    }
"("  	 { return TokenLParen;    }
","  	 { return TokenComma;     }
";"  	 { return TokenSemicolon; }
":"		 { return TokenColon;     }
"fn"  	 { return TokenFn;        }
"enum"   { return TokenEnum;      }
"struct" { return TokenStruct;    }
"return" { return TokenReturn;    }
"true"   { return TokenTrue;      }
"false"  { return TokenFalse;     }
"if"  	 { return TokenIf;        }
"elif"   { return TokenElif;      }
"else"   { return TokenElse;      }
"case"   { return TokenCase;      }

[_a-zA-Z][_a-zA-Z0-9]* { yylval.ident = strdup(yytext); return TokenIdent; }

(.|\n)	{ }

%%

static void string_push(char ch) {
	size_t len = strlen(yylval.str);
	yylval.str = realloc(yylval.str, len + 2);
	yylval.str[len] = ch;
	yylval.str[len + 1] = '\0';
}

token next_token(void) {
	return yylex();
}

str current_lexeme(void) {
	return yytext;
}

void set_lexer_file(FILE* file) {
	yyin = file;
}
