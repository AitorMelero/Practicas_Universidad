/*Fichero: alfa.l
 *Autores: Miguel Manzano, Aitor Melero
 *Pareja: 14
 */

/* Seccion de definicion */
%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"
int columna = 1, linea = 1, error = 0;
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
ERROR .

%%

{COMENTARIO} {columna+=yyleng;}
{MAIN} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAIN;}
{INT} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_INT;}
{BOOLEAN} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_BOOLEAN;}
{ARRAY} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_ARRAY;}
{FUNCTION} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_FUNCTION;}
{IF} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_IF;}
{ELSE} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_ELSE;}
{WHILE} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_WHILE;}
{SCANF} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_SCANF;}
{PRINTF} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_PRINTF;}
{RETURN} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_RETURN;}
{PUNTOYCOMA} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_PUNTOYCOMA;}
{COMA} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_COMA;}
{PARENTESISIZQUIERDO} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_PARENTESISIZQUIERDO;}
{PARENTESISDERECHO} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_PARENTESISDERECHO;}
{CORCHETEIZQUIERDO} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_CORCHETEIZQUIERDO;}
{CORCHETEDERECHO} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_CORCHETEDERECHO;}
{LLAVEIZQUIERDA} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_LLAVEIZQUIERDA;}
{LLAVEDERECHA} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_LLAVEDERECHA;}
{IGUAL} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_IGUAL;}
{ASIGNACION} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_ASIGNACION;}
{MAS} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAS;}
{MENOS} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MENOS;}
{ASTERISCO} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_ASTERISCO;}
{DIVISION} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_DIVISION;}
{AND} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_AND;}
{OR} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_OR;}
{DISTINTO} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_DISTINTO;}
{NOT} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_NOT;}
{MENORIGUAL} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MENORIGUAL;}
{MAYORIGUAL} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAYORIGUAL;}
{MAYOR} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MAYOR;}
{MENOR} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_MENOR;}
{TRUE} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_TRUE;}
{FALSE} {columna+=yyleng; strcpy(yylval.cadena,yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_FALSE;}
{TABULADOR} {columna+=yyleng;}
{ESPACIO} {columna+=yyleng;}
{SALTO} {linea++;columna=1;}
{IDENTIFICADOR} {columna+=yyleng;strcpy(yylval.cadena,yytext);if(yyleng<=100){fprintf(yyout,";D:\t%s\n",yytext);}else{fprintf(stdout,"****Error en [lin %d, col %ld]: identificador demasiado largo (%s)\n",linea,columna-yyleng,yytext);error=1;return TOK_ERROR;};return TOK_IDENTIFICADOR;}
{CONSTANTE_ENTERA} {columna+=yyleng; yylval.numero = atoi(yytext); fprintf(yyout,";D:\t%s\n",yytext); return TOK_CONSTANTE_ENTERA;}
{ERROR} {columna+=yyleng; strcpy(yylval.cadena,yytext); return TOK_ERROR;}
<<EOF>> {return 0;}

%%
