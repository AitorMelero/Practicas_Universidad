#include <stdio.h>
#include <stdlib.h>
#include "tokens.h"

int yylex();

/*Seccion de funciones de usuario*/
int main(int argc, char** argv) {
    int flag; /*Valor del token a analizar*/
    extern int linea;
    extern int columna;
    extern int yyleng;
    extern FILE* yyin;
    extern FILE* yyout;
    extern char* yytext;

    /*Ficheros de entrada y de salida*/
    yyin=fopen(argv[1],"r");
    yyout=fopen(argv[2],"w");

    /*Analizamos fichero hasta que termine o hasta que haya un error*/
    do {
      /*Leemos token a analizar*/
      flag = yylex();

      if(flag==TOK_MAIN) {
        fprintf(yyout,"TOK_MAIN\t%d\t%s\n", TOK_MAIN, yytext);
      } else if(flag==TOK_INT) {
        fprintf(yyout,"TOK_INT\t%d\t%s\n", TOK_INT, yytext);
      } else if(flag==TOK_BOOLEAN) {
        fprintf(yyout,"TOK_BOOLEAN\t%d\t%s\n", TOK_BOOLEAN, yytext);
      } else if(flag==TOK_ARRAY) {
        fprintf(yyout,"TOK_ARRAY\t%d\t%s\n", TOK_ARRAY, yytext);
      } else if(flag==TOK_FUNCTION) {
        fprintf(yyout,"TOK_FUNCTION\t%d\t%s\n", TOK_FUNCTION, yytext);
      } else if(flag==TOK_IF) {
        fprintf(yyout,"TOK_IF\t%d\t%s\n", TOK_IF, yytext);
      } else if(flag==TOK_ELSE) {
        fprintf(yyout,"TOK_ELSE\t%d\t%s\n", TOK_ELSE, yytext);
      } else if(flag==TOK_WHILE) {
        fprintf(yyout,"TOK_WHILE\t%d\t%s\n", TOK_WHILE, yytext);
      } else if(flag==TOK_SCANF) {
        fprintf(yyout,"TOK_SCANF\t%d\t%s\n", TOK_SCANF, yytext);
      } else if(flag==TOK_PRINTF) {
        fprintf(yyout,"TOK_PRINTF\t%d\t%s\n", TOK_PRINTF, yytext);
      } else if(flag==TOK_RETURN) {
        fprintf(yyout,"TOK_RETURN\t%d\t%s\n", TOK_RETURN, yytext);
      } else if(flag==TOK_PUNTOYCOMA) {
        fprintf(yyout,"TOK_PUNTOYCOMA\t%d\t%s\n", TOK_PUNTOYCOMA, yytext);
      } else if(flag==TOK_COMA) {
        fprintf(yyout,"TOK_COMA\t%d\t%s\n", TOK_COMA, yytext);
      } else if(flag==TOK_PARENTESISIZQUIERDO) {
        fprintf(yyout,"TOK_PARENTESISIZQUIERDO\t%d\t%s\n", TOK_PARENTESISIZQUIERDO, yytext);
      } else if(flag==TOK_PARENTESISDERECHO) {
        fprintf(yyout,"TOK_PARENTESISDERECHO\t%d\t%s\n", TOK_PARENTESISDERECHO, yytext);
      } else if(flag==TOK_CORCHETEIZQUIERDO) {
        fprintf(yyout,"TOK_CORCHETEIZQUIERDO\t%d\t%s\n", TOK_CORCHETEIZQUIERDO, yytext);
      } else if(flag==TOK_CORCHETEDERECHO) {
        fprintf(yyout,"TOK_CORCHETEDERECHO\t%d\t%s\n", TOK_CORCHETEDERECHO, yytext);
      } else if(flag==TOK_LLAVEIZQUIERDA) {
        fprintf(yyout,"TOK_LLAVEIZQUIERDA\t%d\t%s\n", TOK_LLAVEIZQUIERDA, yytext);
      } else if(flag==TOK_LLAVEDERECHA) {
        fprintf(yyout,"TOK_LLAVEDERECHA\t%d\t%s\n", TOK_LLAVEDERECHA, yytext);
      } else if(flag==TOK_ASIGNACION) {
        fprintf(yyout,"TOK_ASIGNACION\t%d\t%s\n", TOK_ASIGNACION, yytext);
      } else if(flag==TOK_MAS) {
        fprintf(yyout,"TOK_MAS\t%d\t%s\n", TOK_MAS, yytext);
      } else if(flag==TOK_MENOS) {
        fprintf(yyout,"TOK_MENOS\t%d\t%s\n", TOK_MENOS, yytext);
      } else if(flag==TOK_DIVISION) {
        fprintf(yyout,"TOK_DIVISION\t%d\t%s\n", TOK_DIVISION, yytext);
      } else if(flag==TOK_ASTERISCO) {
        fprintf(yyout,"TOK_ASTERISCO\t%d\t%s\n", TOK_ASTERISCO, yytext);
      } else if(flag==TOK_AND) {
        fprintf(yyout,"TOK_AND\t%d\t%s\n", TOK_AND, yytext);
      } else if(flag==TOK_OR) {
        fprintf(yyout,"TOK_OR\t%d\t%s\n", TOK_OR, yytext);
      } else if(flag==TOK_NOT) {
        fprintf(yyout,"TOK_NOT\t%d\t%s\n", TOK_NOT, yytext);
      } else if(flag==TOK_IGUAL) {
        fprintf(yyout,"TOK_IGUAL\t%d\t%s\n", TOK_IGUAL, yytext);
      } else if(flag==TOK_DISTINTO) {
        fprintf(yyout,"TOK_DISTINTO\t%d\t%s\n", TOK_DISTINTO, yytext);
      } else if(flag==TOK_MENORIGUAL) {
        fprintf(yyout,"TOK_MENORIGUAL\t%d\t%s\n", TOK_MENORIGUAL, yytext);
      } else if(flag==TOK_MAYORIGUAL) {
        fprintf(yyout,"TOK_MAYORIGUAL\t%d\t%s\n", TOK_MAYORIGUAL, yytext);
      } else if(flag==TOK_MENOR) {
        fprintf(yyout,"TOK_MENOR\t%d\t%s\n", TOK_MENOR, yytext);
      } else if(flag==TOK_MAYOR) {
        fprintf(yyout,"TOK_MAYOR\t%d\t%s\n",TOK_MAYOR,yytext);
      } else if(flag==TOK_TRUE) {
        fprintf(yyout,"TOK_TRUE\t%d\t%s\n", TOK_TRUE, yytext);
      } else if(flag==TOK_FALSE) {
        fprintf(yyout,"TOK_FALSE\t%d\t%s\n", TOK_FALSE, yytext);
      } else if(flag==TOK_IDENTIFICADOR) {
        if(yyleng > 100) {
          fprintf(stderr,"****Error en [lin %d, col %d]: identificador demasiado largo (%s)\n", linea, columna-yyleng, yytext);
          flag = -1;
        } else {
          fprintf(yyout,"TOK_IDENTIFICADOR\t%d\t%s\n", TOK_IDENTIFICADOR, yytext);
        }
      } else if(flag==TOK_CONSTANTE_ENTERA) {
        fprintf(yyout,"TOK_CONSTANTE_ENTERA\t%d\t%s\n", TOK_CONSTANTE_ENTERA, yytext);
      } else if(flag==TOK_ERROR) {
        fprintf(stderr,"****Error en [lin %d, col %d]: simbolo no permitido (%s)\n", linea, columna-yyleng, yytext);
      }

    } while((flag!=0) && (flag!=-1));

    /*Cerramos los ficheros*/
    fclose(yyin);
    fclose(yyout);

    return 0;
}
