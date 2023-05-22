CC=gcc

CFLAGS := $(CFLAGS) -Wall -Wextra -Wunreachable-code -lfl

ifeq ($(DEBUG_FULL), 1)
CFLAGS := $(CFLAGS) -DDEBUG_LOG_GC -g -Og
else
CFLAGS := $(CFLAGS) -O3
endif

ifeq ($(FEELING_SPICY), 1)
CFLAGS := $(CFLAGS) -Wswitch-enum -Wstrict-prototypes -Wconversion
endif

LAMB_RS_PATH :=./lamb-rs/target/release

ALL_SRC := ./main.c \
           ./ast/ast.c \
           ./ast/optimization.c \
           ./parsing/built/lexer.c \
           ./parsing/built/parser.tab.c \
		   ./compile/value.c \
		   ./compile/chunk.c \
		   ./compile/ast.c \
		   ./compile/object.c \
		   ./compile/table.c \
		   ./compile/compiler.c \
           ./compile/memory.c \
		   ./compile/misc.c \
		   ./debug/debug.c \
		   ./vm/vm.c \
		   ./vm/native.c \

lamb: $(ALL_SRC) ./liblamb_rs.a
	$(CC) -L$(LAMB_RS_PATH) $(ALL_SRC) $(CFLAGS) -o lamb -l:liblamb_rs.a

./parsing/built/lexer.c: ./parsing/lexer.l
	flex ./parsing/lexer.l
	
./parsing/built/parser.tab.c ./parser/built/parser.tab.h: ./parsing/parser.y
	bison -d --output-file="./parsing/built/parser.tab.c" ./parsing/parser.y

./liblamb_rs.a:
	cd lamb-rs && cargo build --release
