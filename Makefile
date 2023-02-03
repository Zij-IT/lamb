CC=gcc

ALL_SRC:= ./main.c \
          ./memory.c \
          ./ast/ast.c \
          ./ast/optimization.c \
          ./parsing/built/lexer.c \
          ./parsing/built/parser.tab.c \
		  ./compile/vm.c \
		  ./compile/value.c \
		  ./compile/chunk.c \
		  ./compile/ast.c \
		  ./compile/debug.c \
		  ./compile/object.c \
		  ./compile/table.c \
		  ./compile/compiler.c \
		  ./compile/misc.c \
		  ./compile/native.c \

lamb: $(ALL_SRC)
	$(CC) $(ALL_SRC) -g -Wall -Wextra -lfl -o lamb

./parsing/built/lexer.c: ./parsing/lexer.l
	flex ./parsing/lexer.l
	
./parsing/built/parser.tab.c ./parser/built/parser.tab.h: ./parsing/parser.y
	bison -d --output-file="./parsing/built/parser.tab.c" ./parsing/parser.y

