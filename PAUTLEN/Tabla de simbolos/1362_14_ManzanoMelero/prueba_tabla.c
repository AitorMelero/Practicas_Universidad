/*
 * fichero: prueba_tabla.c
 * autores: Miguel Manzano, Aitor Melero
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tablaSimbolos.h"

/*Funcion que genera el fichero de salida en funcion del de entrada*/
void alfa_parsea(FILE* entrada, FILE* salida, char* cad);


/*Funcion principal que llama a alfa_parsea mientras se lea algo en el fichero de entrada*/
int main (int argc, char **argv){
  FILE *entrada = NULL;
  FILE *salida = NULL;
  char cadena[1024];

  if(argc!=3){
    printf("\n ERROR AL INTRODUCIR LOS PARAMETROS\n\t");
    return 1;
  }

  entrada=fopen(argv[1],"r");

  if(!entrada){
    printf("\n ERROR AL ABRIR EL FICHERO DE ENTRADA\n");
    return 1;
  }

  salida=fopen(argv[2],"w");
  if(!salida){
    printf("\n ERROR AL ABRIR EL FICHERO DE SALIDA\n");
    return 1;
  }

  /* vamos llamando al fichero de entrada en funcion de lo que leemos*/
  while(fgets(cadena, 1024, entrada)) {
    alfa_parsea(entrada, salida, cadena);
  }

  terminar();
  fclose(entrada);
  fclose(salida);
  return 0;
}

void alfa_parsea(FILE* entrada, FILE* salida, char* cad) {
  INFO_SIMBOLO* t_info = NULL;
  int scanea, res;
  char* id;

  /*Leemos el id y el valor correspondiente*/
  scanea = sscanf(cad, "%ms\t%i", &id, &res);

  /*En funcion de los datos leidos sabemos con que simbolos trabajamos*/
  if(scanea == 2) {
    if(res < -1) {
      if((res == -999) && (!strcmp(id, "cierre"))) {
        fprintf(salida, "cierre \n");
        free(id);
        cerrarFuncion();
      } else {
        t_info = crear_info_simbolo(id, FUNCION, ENTERO, ESCALAR, res, 0);
        if(!t_info) {
          free(id);
          return;
        }
        if(declararFuncion(id, t_info) == OK) {
          fprintf(salida, "%s\n", id);
        } else {
          fprintf(salida, "-1\t%s\n", id);
        }
        liberar_info_simbolo(t_info);
        free(id);
      }
    } else if(res > -1) {
      t_info = crear_info_simbolo(id, VARIABLE, ENTERO, ESCALAR, res, 0);
      if(!t_info) {
        free(id);
        return;
      }
      if(declarar(id, t_info) == ERR) {
        fprintf(salida, "-1\t%s\n", id);
      } else {
        fprintf(salida, "%s\n", id);
      }
      liberar_info_simbolo(t_info);
      free(id);
    }
  } else if(scanea == 1) {
    t_info = usoLocal(id);
    if(!t_info) {
      fprintf(salida, "%s\t-1\n", id);
    } else {
      fprintf(salida, "%s\t%d\n", id, t_info->adicional1);
    }
    free(id);
  }

}
