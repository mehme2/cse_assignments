%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gpp.h"

int yylex();
void yyerror(data *, node **, char* s) {printf("error: %s\n", s);}
int yywrap() {return 1;}
%}

%parse-param {struct data * finalData} {struct node ** tree}

%union {
    struct frac {
    int top, bottom;
    } f;
    struct node * nodePtr;
    struct data {
    int type;
    union {
        struct frac fraction;
        int integer;
        int boolean;
        void * ptr;
    };
    } d;
}

%token <nodePtr>
    VALUEI
    VALUEF
    STRING
    IDENTIFIER
    KW_AND
    KW_OR
    KW_NOT
    KW_EQUAL
    KW_LESS
    KW_NIL
    KW_LIST
    KW_APPEND
    KW_CONCAT
    KW_SET
    KW_DEFFUN
    KW_DEFVAR
    KW_FOR
    KW_WHILE
    KW_IF
    KW_EXIT
    KW_LOAD
    KW_DISP
    KW_TRUE
    KW_FALSE
    OP_PLUS
    OP_MINUS
    OP_DIV
    OP_MULT

%token
    OP_OP
    OP_CP
    OP_COMMA
    OP_QUOTE
    COMMENT

%start START

%type <nodePtr> STMTLIST EXPLIST EXP OPERATION IDLIST VALUES

%%
START: STMTLIST
     ;

STMTLIST: STMTLIST EXP {$$ = *tree = addChild($1, $2); destroyData(*finalData); *finalData = eval($2); if(gppstop) YYACCEPT;}
        | {$$ = *tree = newNode(STMTLIST, (data){});}
        ;

EXP: VALUEI {$$ = $1;}
   | VALUEF {$$ = $1;}
   | IDENTIFIER {$$ = $1;}
   | KW_TRUE {$$ = $1;}
   | KW_FALSE {$$ = $1;}
   | KW_NIL {$$ = $1;}
   | STRING {$$ = $1;}
   | OP_QUOTE OP_OP VALUES OP_CP {$$ = addChild(newNode(LIST, (data){}), $3);}
   | OP_OP OPERATION EXPLIST OP_CP {$$ = $2; addChild($$, $3);}
   ;

VALUES: {$$ = 0;}
      | EXP {$$ = $1;}
      | VALUES OP_COMMA EXP {$$ = addSibling($1, $3);}

EXPLIST: {$$ = newNode(EXPLIST, (data){});}
       | EXPLIST EXP {$$ = addChild($1, $2);}
       ;

OPERATION: KW_DEFFUN IDENTIFIER OP_OP IDLIST OP_CP {$$ = addChild($1, addSibling($2, $4));}
         | KW_DEFVAR IDENTIFIER {$$ = addChild($1, $2);}
         | KW_SET IDENTIFIER {$$ = addChild($1, $2);}
         | KW_FOR OP_OP IDENTIFIER EXP EXP OP_CP {$$ = addChild($1, addSibling(addSibling($3, $4), $5));}
         | KW_WHILE EXP {$$ = addChild($1, $2);}
         | KW_IF EXP {$$ = addChild($1, $2);}
         | OP_PLUS {$$ = $1;}
         | OP_MINUS {$$ = $1;}
         | OP_MULT {$$ = $1;}
         | OP_DIV {$$ = $1;}
    	 | KW_AND {$$ = $1;}
    	 | KW_OR {$$ = $1;}
    	 | KW_NOT {$$ = $1;}
    	 | KW_EQUAL {$$ = $1;}
    	 | KW_LESS {$$ = $1;}
    	 | KW_LIST {$$ = $1;}
    	 | KW_APPEND {$$ = $1;}
    	 | KW_CONCAT {$$ = $1;}
    	 | KW_EXIT {$$ = $1;}
    	 | KW_LOAD {$$ = $1;}
    	 | KW_DISP {$$ = $1;}
         | EXP {$$ = addChild(newNode(CALL, (data){}), $1);}
         ;

IDLIST: {$$ = newNode(IDLIST, (data){});}
      | IDLIST IDENTIFIER {$$ = addChild($1, $2);}
      ;
%%
