#ifndef LEXER_H
#define LEXER_H

#include "../types.h"
#include "./built/parser.tab.h"
#include <stdint.h>
#include <stdio.h>

typedef enum yytokentype token;

token next_token(void);

str current_lexeme(void);

void set_lexer_file(FILE *file);

#endif // LEXER_H