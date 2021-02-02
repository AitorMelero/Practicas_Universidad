/*Fichero: alfa.l
 *Autores: Miguel Manzano, Aitor Melero
 *Pareja: 14
 */

/* Seccion de definicion */
%{
#include <stdio.h>
#include <stdlib.h>
#include "tokens.h"
int columna = 1, linea = 1;
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

{COMENTARIO} {columna+=yyleng; return 1;}
{MAIN} {columna+=yyleng; return TOK_MAIN;}
{INT} {columna+=yyleng; return TOK_INT;}
{BOOLEAN} {columna+=yyleng; return TOK_BOOLEAN;}
{ARRAY} {columna+=yyleng; return TOK_ARRAY;}
{FUNCTION} {columna+=yyleng; return TOK_FUNCTION;}
{IF} {columna+=yyleng; return TOK_IF;}
{ELSE} {columna+=yyleng; return TOK_ELSE;}
{WHILE} {columna+=yyleng; return TOK_WHILE;}
{SCANF} {columna+=yyleng; return TOK_SCANF;}
{PRINTF} {columna+=yyleng; return TOK_PRINTF;}
{RETURN} {columna+=yyleng; return TOK_RETURN;}
{PUNTOYCOMA} {columna+=yyleng; return TOK_PUNTOYCOMA;}
{COMA} {columna+=yyleng; return TOK_COMA;}
{PARENTESISIZQUIERDO} {columna+=yyleng; return TOK_PARENTESISIZQUIERDO;}
{PARENTESISDERECHO} {columna+=yyleng; return TOK_PARENTESISDERECHO;}
{CORCHETEIZQUIERDO} {columna+=yyleng; return TOK_CORCHETEIZQUIERDO;}
{CORCHETEDERECHO} {columna+=yyleng; return TOK_CORCHETEDERECHO;}
{LLAVEIZQUIERDA} {columna+=yyleng; return TOK_LLAVEIZQUIERDA;}
{LLAVEDERECHA} {columna+=yyleng; return TOK_LLAVEDERECHA;}
{IGUAL} {columna+=yyleng; return TOK_IGUAL;}
{ASIGNACION} {columna+=yyleng; return TOK_ASIGNACION;}
{MAS} {columna+=yyleng; return TOK_MAS;}
{MENOS} {columna+=yyleng; return TOK_MENOS;}
{ASTERISCO} {columna+=yyleng; return TOK_ASTERISCO;}
{DIVISION} {columna+=yyleng; return TOK_DIVISION;}
{AND} {columna+=yyleng; return TOK_AND;}
{OR} {columna+=yyleng; return TOK_OR;}
{DISTINTO} {columna+=yyleng; return TOK_DISTINTO;}
{NOT} {columna+=yyleng; return TOK_NOT;}
{MENORIGUAL} {columna+=yyleng; return TOK_MENORIGUAL;}
{MAYORIGUAL} {columna+=yyleng; return TOK_MAYORIGUAL;}
{MAYOR} {columna+=yyleng; return TOK_MAYOR;}
{MENOR} {columna+=yyleng; return TOK_MENOR;}
{TRUE} {columna+=yyleng; return TOK_TRUE;}
{FALSE} {columna+=yyleng; return TOK_FALSE;}
{TABULADOR} {columna+=yyleng; return 1;}
{ESPACIO} {columna+=yyleng; return 1;}
{SALTO} {columna=1; linea++; return 1;}
{IDENTIFICADOR} {columna+=yyleng; return TOK_IDENTIFICADOR;}
{CONSTANTE_ENTERA} {columna+=yyleng; return TOK_CONSTANTE_ENTERA;}
{ERROR} {columna+=yyleng; return TOK_ERROR;}
<<EOF>> {return 0;}


%%

