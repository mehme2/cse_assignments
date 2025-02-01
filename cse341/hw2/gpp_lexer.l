%option noyywrap

%{
#include <stdio.h>

void processToken(const char * token) {
    printf("%s\n", token);
}
%}

other .[^\n]*\n
comment ;;[^\n]+\n
alpha [a-zA-Z]
digit [0-9]
integer {digit}+
fraction {integer}(:|f){integer}
identrest ({digit}|\_|{alpha})+
ident ({alpha}{identrest})|{alpha}
ws [ \n\t]+

%%

{ws}

"and"      processToken("KW_AND");
"or"       processToken("KW_OR");
"not"      processToken("KW_NOT");
"equal"    processToken("KW_EQUAL");
"less"     processToken("KW_LESS");
"nil"      processToken("KW_NIL");
"list"     processToken("KW_LIST");
"append"   processToken("KW_APPEND");
"concat"   processToken("KW_CONCAT");
"deffun"   processToken("KW_DEFFUN");
"for"      processToken("KW_FOR");
"if"       processToken("KW_IF");
"exit"     processToken("KW_EXIT");
"load"     processToken("KW_LOAD");
"print"    processToken("KW_DISP");
"true"     processToken("KW_TRUE");
"false"    processToken("KW_FALSE");

"+"        processToken("OP_PLUS");
"-"        processToken("OP_MINUS");
"/"        processToken("OP_DIV");
"*"        processToken("OP_MULT");
"("        processToken("OP_OP");
")"        processToken("OP_CP");
","        processToken("OP_COMMA");

{comment}  processToken("COMMENT");
{fraction} processToken("VALUEF");
{integer}  processToken("VALUEI");
{ident}    processToken("IDENTIFIER");

(.|{digit}({digit}|{alpha}|":")+) printf("SYNTAX ERROR Invalid token: %s\n", yytext);

%%

int main() {
    yylex();
    return 0;
}
