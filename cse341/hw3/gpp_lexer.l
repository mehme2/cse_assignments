%{
#include <stdio.h>
#include "y.tab.h"
#include "gpp.h"
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
string \"((\\.)|[^"\\])*\"

%%

[ \n\t]+

"and"      {yylval.nodePtr = newNode(KW_AND, (data){}); return KW_AND;}
"or"       {yylval.nodePtr = newNode(KW_OR, (data){}); return KW_OR;}
"not"      {yylval.nodePtr = newNode(KW_NOT, (data){}); return KW_NOT;}
"equal"    {yylval.nodePtr = newNode(KW_EQUAL, (data){}); return KW_EQUAL;}
"less"     {yylval.nodePtr = newNode(KW_LESS, (data){}); return KW_LESS;}
"nil"      {yylval.nodePtr = newNode(KW_NIL, (data){.type = KW_NIL}); return KW_NIL;}
"list"     {yylval.nodePtr = newNode(KW_LIST, (data){}); return KW_LIST;}
"append"   {yylval.nodePtr = newNode(KW_APPEND, (data){}); return KW_APPEND;}
"concat"   {yylval.nodePtr = newNode(KW_CONCAT, (data){}); return KW_CONCAT;}
"deffun"   {yylval.nodePtr = newNode(KW_DEFFUN, (data){}); return KW_DEFFUN;}
"defvar"   {yylval.nodePtr = newNode(KW_DEFVAR, (data){}); return KW_DEFVAR;}
"set"      {yylval.nodePtr = newNode(KW_SET, (data){}); return KW_SET;}
"for"      {yylval.nodePtr = newNode(KW_FOR, (data){}); return KW_FOR;}
"while"    {yylval.nodePtr = newNode(KW_WHILE, (data){}); return KW_WHILE;}
"if"       {yylval.nodePtr = newNode(KW_IF, (data){}); return KW_IF;}
"exit"     {yylval.nodePtr = newNode(KW_EXIT, (data){}); return KW_EXIT;}
"load"     {yylval.nodePtr = newNode(KW_LOAD, (data){}); return KW_LOAD;}
"print"    {yylval.nodePtr = newNode(KW_DISP, (data){}); return KW_DISP;}
"true"     {yylval.nodePtr = newNode(BOOLEAN, (data){.type = BOOLEAN, .boolean = 1}); return KW_TRUE;}
"false"    {yylval.nodePtr = newNode(BOOLEAN, (data){.type = BOOLEAN, .boolean = 0}); return KW_FALSE;}

"+"        {yylval.nodePtr = newNode(OP_PLUS, (data){}); return OP_PLUS;}
"-"        {yylval.nodePtr = newNode(OP_MINUS, (data){}); return OP_MINUS;}
"/"        {yylval.nodePtr = newNode(OP_DIV, (data){}); return OP_DIV;}
"*"        {yylval.nodePtr = newNode(OP_MULT, (data){}); return OP_MULT;}
"("        {return OP_OP;}
")"        {return OP_CP;}
","        {return OP_COMMA;}
"'"        {return OP_QUOTE;}

{comment}  //{return COMMENT;}

{fraction} {
            int top = atoi(yytext);
            int idx;
            for(idx = 0; yytext[idx] >= '0' && yytext[idx] <= '9'; ++idx);
            int bottom = atoi(&yytext[idx + 1]);
            yylval.nodePtr = newNode(VALUEF, (data){.type = VALUEF, .fraction.top = top, .fraction.bottom = bottom});
            return VALUEF;
           }
{integer}  {
            yylval.nodePtr = newNode(VALUEI, (data){.type = VALUEI, .integer = atoi(yytext)});
            return VALUEI;
           }
{ident}    {
            int len = strlen(yytext);
            yylval.nodePtr = newNode(IDENTIFIER, (data){.type = STRING, .ptr = strcpy(malloc(len + 1), yytext)});
            ((char*)yylval.nodePtr->d.ptr)[len] = 0;
            return IDENTIFIER;
           }

{string}   {
            int len = 0;
            for(int i = 1; yytext[i] && yytext[i] != '\"'; ++i) {
                if(yytext[i] == '\\') ++i;
                ++len;
            }
            char * str = malloc(len + 1);
            int si = 0;
            for(int i = 1; yytext[i] && yytext[i] != '\"'; ++i) {
                if(yytext[i] == '\\') ++i;
                str[si++] = yytext[i];
            }
            str[len] = 0;
            yylval.nodePtr = newNode(STRING, (data){.type = STRING, .ptr = str});
            return STRING;
           }

(.|{digit}({digit}|{alpha}|":")+) printf("SYNTAX ERROR Invalid token: %s\n", yytext);

%%
