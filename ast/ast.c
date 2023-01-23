#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "../memory.h"
#include "optimization.h"

AstNode* new_astnode(AstNodeType type) {
  AstNode* node = calloc(1, sizeof(AstNode));
  node->type = type;
  
  return node;
}

AstNode* new_unary_astnode(AstNodeType type, AstNode* rhs) {
  AstNode* node = new_astnode(type);
  node->kids[0] = rhs;
  
  return node;
}

AstNode* new_binary_astnode(AstNodeType type, AstNode* lhs, AstNode* rhs) {
  AstNode* node = new_astnode(type);
  node->kids[0] = lhs;
  node->kids[1] = rhs;
  
  return node;
}
void free_ast(AstNode* root) {
  if (root == NULL) {
    return;
  }
  
  switch(root->type) {
    case AstntStrLit:
      free(root->val.s);
      break;
    case AstntIdent:
      free(root->val.i);
      break;
    case AstntUnaryNeg:
    case AstntUnaryLogNot:
    case AstntUnaryBitNot:
      free_ast(root->kids[0]);
      break;
    case AstntBinaryAdd:
    case AstntBinarySub:
    case AstntBinaryMul:
    case AstntBinaryDiv:
    case AstntBinaryMod:
    case AstntBinaryLCompose:
    case AstntBinaryRCompose:
    case AstntBinaryLApply:
    case AstntBinaryRApply:
    case AstntBinaryLogAnd:
    case AstntBinaryLogOr:
    case AstntBinaryEq:
    case AstntBinaryNe:
    case AstntBinaryGt:
    case AstntBinaryGe:
    case AstntBinaryLt:
    case AstntBinaryLe:
    case AstntBinaryOr:
    case AstntBinaryXor:
    case AstntBinaryAnd:
    case AstntBinaryRShift:
    case AstntBinaryLShift:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntIf:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      free_ast(root->kids[2]);
      free_ast(root->kids[3]);
      break;
    case AstntElif:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      free_ast(root->kids[2]);
      break;
    case AstntElse:
      free_ast(root->kids[0]);
      break;
    case AstntCase:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntCaseArm:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      free_ast(root->kids[2]);
      break;
    case AstntArray:
      free_ast(root->kids[0]);
      break;
    case AstntFuncDef:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      free_ast(root->kids[2]);
      break;
    case AstntFuncCall:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntArrayIndex:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntReturn:
      free_ast(root->kids[0]);
      break;
    case AstntExprStmt:
      free_ast(root->kids[0]);
      break;
    case AstntAssignStmt:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntBlockStmt:
      free_ast(root->kids[0]);
      break;
    case AstntStmts:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntNodeList:
      free_ast(root->kids[0]);
      free_ast(root->kids[1]);
      break;
    case AstntNumLit:
    case AstntCharLit:
    case AstntBoolLit:
      break;
    }

  free(root);
}

#define BASE_PADDING 4

// General formula for printing is:
// 
//   spaces + BASE_PADDING + LINE_LENGTH
//
// LINE_LENGTH is lhe length of the line segment above the current line.
// In the case of unary, "rhs: " adds a requirement of 5 spaces for proper
// alignment
static void pad(u16 spaces) {
  for (u16 n = 0; n < spaces; n++) {
    printf(" ");
  }
}

#define pre_pad(padding, fmt, ...) \
  do { pad((padding)); printf((fmt), ##__VA_ARGS__); } while(false)

static void print_binary(char* op, AstNode* root, u16 spaces) {
  printf("Binary: {\n");
  pre_pad(spaces + BASE_PADDING, "op: '%s',\n", op);
  pre_pad(spaces + BASE_PADDING, "lhs: ");
  print_ast(root->kids[0], spaces + BASE_PADDING + 5);
  printf(",\n");
  pre_pad(spaces + BASE_PADDING, "rhs: ");
  print_ast(root->kids[1], spaces + BASE_PADDING + 5);
  printf(",\n");
  pre_pad(spaces, "}");
}

static void print_unary(char* op, AstNode* root, u16 spaces) {
  printf("Unary: {\n");
  pre_pad(spaces + BASE_PADDING, "op: '%s',\n", op);
  pre_pad(spaces + BASE_PADDING, "rhs: ");
  print_ast(root->kids[0], spaces + BASE_PADDING + 5);
  printf(",\n");
  pre_pad(spaces, "}");
}

static void print_block(AstNode* root, u16 spaces) {
  if (root == NULL) {
    printf("Empty");
  } else {
    print_ast(root, spaces);
  }
}

void print_ast(AstNode* root, u16 spaces) {
  if (root == NULL) {
    return;
  }
  
  switch(root->type) {
    case AstntStrLit:
      printf("\"%s\"", root->val.s);
      break;
    case AstntNumLit:
      printf("%ld", root->val.n);
      break;
    case AstntCharLit:
      printf("'%c'", root->val.c);
      break;
    case AstntBoolLit:
      printf("%s", root->val.b ? "true" : "false");
      break;
    case AstntUnaryNeg:
      print_unary("-", root, spaces);
      break;
    case AstntUnaryLogNot:
      print_unary("!", root, spaces);
      break;
    case AstntUnaryBitNot:
      print_unary("~", root, spaces);
      break;
    case AstntIdent:
      printf("Ident(%s)", root->val.i);
      break;
    case AstntBinaryAdd:
      print_binary("+", root, spaces);
      break;
    case AstntBinarySub:
      print_binary("-", root, spaces);
      break;
    case AstntBinaryMul:
      print_binary("*", root, spaces);
      break;
    case AstntBinaryDiv:
      print_binary("/", root, spaces);
      break;
    case AstntBinaryMod:
      print_binary("%", root, spaces);
      break;
    case AstntBinaryLApply:
      print_binary("<$", root, spaces);
      break;
    case AstntBinaryRApply:
      print_binary("$>", root, spaces);
      break;
    case AstntBinaryLCompose:
      print_binary("<.", root, spaces);
      break;
    case AstntBinaryRCompose:
      print_binary(".>", root, spaces);
      break;
    case AstntBinaryLogAnd:
      print_binary("&&", root, spaces);
      break;
    case AstntBinaryLogOr:
      print_binary("||", root, spaces);
      break;
    case AstntBinaryEq:
      print_binary("=", root, spaces);
      break;
    case AstntBinaryNe:
      print_binary("!=", root, spaces);
      break;
    case AstntBinaryGt:
      print_binary(">", root, spaces);
      break;
    case AstntBinaryGe:
      print_binary(">=", root, spaces);
      break;
    case AstntBinaryLt:
      print_binary("<", root, spaces);
      break;
    case AstntBinaryLe:
      print_binary("<=", root, spaces);
      break;
    case AstntBinaryOr:
      print_binary("|", root, spaces);
      break;
    case AstntBinaryXor:
      print_binary("^", root, spaces);
      break;
    case AstntBinaryAnd:
      print_binary("&&", root, spaces);
      break;
    case AstntBinaryRShift:
      print_binary(">>", root, spaces);
      break;
    case AstntBinaryLShift:
      print_binary("<<", root, spaces);
      break;
    case AstntIf:
      printf("If: {\n");
      pre_pad(spaces + BASE_PADDING, "cond: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 6);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "block: ");
      print_block(root->kids[1], spaces + BASE_PADDING + 7);
      printf(",\n");

      if(root->kids[2] != NULL) {
        print_ast(root->kids[2], spaces + BASE_PADDING);
      }

      if(root->kids[3] != NULL) {
        print_ast(root->kids[3], spaces + BASE_PADDING);
        printf(",\n");
      }

      pre_pad(spaces, "}");
      break;
    case AstntElif:
      pre_pad(spaces, "elif: Elif {\n");
      pre_pad(spaces + BASE_PADDING, "cond: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 6);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "block: ");
      print_block(root->kids[1], spaces + BASE_PADDING + 7);
      printf(",\n");
      pre_pad(spaces, "},\n");
      print_ast(root->kids[2], spaces);
      break;
    case AstntElse:
      pre_pad(spaces, "else: Else {\n");
      pre_pad(spaces + BASE_PADDING, "block: ");
      print_block(root->kids[0], spaces + BASE_PADDING + 7);
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    case AstntCase:
      printf("Case: {\n");
      pre_pad(spaces + BASE_PADDING, "value: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 7);
      printf(",\n");
      print_block(root->kids[1], spaces + BASE_PADDING);
      pre_pad(spaces, "}");
      break;
    case AstntCaseArm:
      pre_pad(spaces, "caseArm: CaseArm {\n");
      pre_pad(spaces + BASE_PADDING, "pattern: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 9);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "block: ");
      print_block(root->kids[1], spaces + BASE_PADDING + 7);
      printf(",\n");
      pre_pad(spaces, "},\n");
      print_ast(root->kids[2], spaces);
      break;
    case AstntArray:
      printf("[\n");
      for(AstNode* node = root->kids[0]; node != NULL; node = node->kids[1]) {
        pad(spaces + BASE_PADDING);
        print_ast(node->kids[0], spaces + BASE_PADDING);
        printf(",\n");
      }
      pre_pad(spaces, "]");
      break;
    case AstntFuncDef:
      printf("FuncDef: {\n");
      pre_pad(spaces + BASE_PADDING, "formal_params: [\n");
      for(AstNode* node = root->kids[0]; node != NULL; node = node->kids[1]) {
        pad(spaces + BASE_PADDING + 15 + BASE_PADDING);
        print_ast(node->kids[0], spaces + BASE_PADDING + 15 + BASE_PADDING);
        printf(",\n");
      }
      pre_pad(spaces + BASE_PADDING + 15, "]");
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "block: ");
      print_block(root->kids[1], spaces + BASE_PADDING + 7);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "recursive: ");
      print_block(root->kids[2], spaces + BASE_PADDING + 11);
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    case AstntReturn: {
      printf("Return: {\n");
      pre_pad(spaces + BASE_PADDING, "value: ");
      if (root->kids[0] != NULL) {
        print_ast(root->kids[0], spaces + BASE_PADDING + 7);
      } else {
        printf("None");
      }
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    }
    case AstntFuncCall:
      printf("FuncCall: {\n");
      pre_pad(spaces + BASE_PADDING, "callee: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 8);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "args: [\n");
      for(AstNode* node = root->kids[1]; node != NULL; node = node->kids[1]) {
        pad(spaces + BASE_PADDING + 6 + BASE_PADDING);
        print_ast(node->kids[0], spaces + BASE_PADDING + 6 + BASE_PADDING);
        printf(",\n");
      }
      pre_pad(spaces + BASE_PADDING + 6, "]");
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    case AstntArrayIndex:
      printf("ArrayIndex: {\n");
      pre_pad(spaces + BASE_PADDING, "index: ");
      print_ast(root->kids[1], spaces + BASE_PADDING + 7);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "indexee: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 9);
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    case AstntExprStmt:
      printf("ExpressionStatement: {\n");
      pre_pad(spaces + BASE_PADDING, "expr: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 6);
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    case AstntAssignStmt:
      printf("AssignmentStatement: {\n");
      pre_pad(spaces + BASE_PADDING, "lhs: ");
      print_ast(root->kids[0], spaces + BASE_PADDING + 5);
      printf(",\n");
      pre_pad(spaces + BASE_PADDING, "value: ");
      print_ast(root->kids[1], spaces + BASE_PADDING + 7);
      printf(",\n");
      pre_pad(spaces, "}");
      break;
    case AstntBlockStmt:
      printf("BlockStatement: {\n");
      pad(spaces + BASE_PADDING);
      print_ast(root->kids[0], spaces + BASE_PADDING);
      printf("\n");
      pre_pad(spaces, "}");
      break;
    case AstntStmts:
      printf("Statements: {\n");
      for(AstNode* node = root; node != NULL; node = node->kids[1]) {
        if(node->kids[0] != NULL) {
          pad(spaces + BASE_PADDING);
          print_ast(node->kids[0], spaces + BASE_PADDING);
          printf(",\n");
        }
      }
      pre_pad(spaces, "}");
      break;
    case AstntNodeList:
      break;
  }
}

#undef BASE_PADDING
#undef pre_pad

