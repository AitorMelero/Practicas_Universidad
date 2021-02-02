/*Fichero: alfa.l*/

/* Seccion de definicion */
%{
#include <stdio.h>
#include <stdlib.h>
#include "alfa.h"
#include "y.tab.h"

long columna = 1, linea = 1;
int error = 0;
%}

%option noyywrap

/* Seccion de reglas */

LETRASDIGITO [a-zA-Z0-9_]
DIGITO [0-9]
LETRAS [a-zA-Z]
COMENTARIO "//".*
MAIN "main"
INT "int"
BOOLEAN "boolean"
ARRAY "array"
FUNCTION "function"
IF "if"
ELSE "else"
WHILE "while"
SCANF "scanf"
PRINTF "printf"
RETURN "return"
PUNTOYCOMA ";"
COMA ","
PARENTESISIZQUIERDO "("
PARENTESISDERECHO ")"
CORCHETEIZQUIERDO "["
CORCHETEDERECHO "]"
LLAVEIZQUIERDA "{"
LLAVEDERECHA "}"
IGUAL "=="
ASIGNACION "="
MAS "+"
MENOS "-"
ASTERISCO "*"
DIVISION "/"
AND "&&"
OR "||"
DISTINTO "!="
NOT "!"
MENORIGUAL "<="
MAYORIGUAL ">="
MAYOR ">"
MENOR "<"
TRUE "true"
FALSE "false"
TABULADOR "\t"
ESPACIO " "
SALTO "\n"
IDENTIFICADOR {LETRAS}({LETRASDIGITO})*
CONSTANTE_ENTERA ({DIGITO})+
SIMBOLO_NOPERMITIDO .

%%

{COMENTARIO} {columna+=yyleng;}
{MAIN} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAIN;}
{INT} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_INT;}
{BOOLEAN} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_BOOLEAN;}
{ARRAY} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_ARRAY;}
{FUNCTION} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_FUNCTION;}
{IF} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_IF;}
{ELSE} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_ELSE;}
{WHILE} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_WHILE;}
{SCANF} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_SCANF;}
{PRINTF} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_PRINTF;}
{RETURN} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_RETURN;}
{PUNTOYCOMA} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_PUNTOYCOMA;}
{COMA} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_COMA;}
{PARENTESISIZQUIERDO} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_PARENTESISIZQUIERDO;}
{PARENTESISDERECHO} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_PARENTESISDERECHO;}
{CORCHETEIZQUIERDO} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_CORCHETEIZQUIERDO;}
{CORCHETEDERECHO} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_CORCHETEDERECHO;}
{LLAVEIZQUIERDA} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_LLAVEIZQUIERDA;}
{LLAVEDERECHA} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_LLAVEDERECHA;}
{IGUAL} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_IGUAL;}
{ASIGNACION} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_ASIGNACION;}
{MAS} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAS;}
{MENOS} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MENOS;}
{ASTERISCO} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_ASTERISCO;}
{DIVISION} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_DIVISION;}
{AND} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_AND;}
{OR} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_OR;}
{DISTINTO} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_DISTINTO;}
{NOT} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_NOT;}
{MENORIGUAL} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MENORIGUAL;}
{MAYORIGUAL} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAYORIGUAL;}
{MAYOR} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAYOR;}
{MENOR} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_MENOR;}
{CONSTANTE_ENTERA} {yylval.atributos.valor_entero = atoi(yytext); strcpy(yylval.atributos.lexema, yytext); columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_CONSTANTE_ENTERA;}
{TRUE} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_TRUE;}
{FALSE} {columna+=yyleng; fprintf(yyout,";D:\t%s\n",yytext); return TOK_FALSE;}
{TABULADOR} {columna+=yyleng;}
{ESPACIO} {columna+=yyleng;}
{SALTO} {linea++;columna=1;}
{IDENTIFICADOR} {strcpy(yylval.atributos.lexema,yytext);columna+=yyleng;if(yyleng<=100){fprintf(yyout,";D:\t%s\n",yytext);}else{fprintf(stdout,"****Error en [lin %ld, col %ld]: identificador demasiado largo (%s)\n",linea,columna-yyleng,yytext);error=1;return TOK_ERROR;};return TOK_IDENTIFICADOR;}
{SIMBOLO_NOPERMITIDO} {columna+=yyleng; fprintf(stdout, "****Error en [lin %ld, col %ld]: simbolo no permitido (%s)\n", linea, columna-yyleng, yytext);error = 2;return  TOK_ERROR;}
%%
