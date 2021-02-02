#include "y.tab.h"
#include <stdio.h>
#include <stdlib.h>
extern FILE *yyin;
extern FILE *yyout;

int main (int argc, char **argv){

  if(argc!=3){
    printf("\n ERROR AL INTRODUCIR LOS PARAMETROS\n\t");
    return 0;
  }

  yyin=fopen(argv[1],"r");

  if(!yyin){
    printf("\n ERROR AL ABRIR EL FICHERO DE ENTRADA\n");
    return 0;
  }

  yyout=fopen(argv[2],"w");
  if(!yyout){
    printf("\n ERROR AL ABRIR EL FICHERO DE SALIDA\n");
    fclose(yyin);
    return 0;
  }

  if(yyparse()!=0){
    printf("\nERROR EN EL ANALISIS SINTACTICO\n");
  }else{
    printf("\nANALISIS SINTACTICO TERMINADO CON EXITO\n");
  }
  
  fclose(yyout);
  fclose(yyin);
  return 0;
}