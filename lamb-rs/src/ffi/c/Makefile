parsing: ./parsing/built/lexer.c ./parsing/built/parser.tab.c

./parsing/built/lexer.c: ./parsing/lexer.l
	flex --noyywrap ./parsing/lexer.l
	
./parsing/built/parser.tab.c ./parser/built/parser.tab.h: ./parsing/parser.y
	bison -d --output-file="./parsing/built/parser.tab.c" ./parsing/parser.y
