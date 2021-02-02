/*
 * fichero: tablaHash.c
 * autores: Miguel Manzano, Aitor Melero
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tablaHash.h"

/* posición inicial de parámetros de función */
#define POS_INI_PARAMS 0
/* posición de inicio de variables locales de función */
#define POS_INI_VARS_LOCALES 1

#define HASH_INI 5381
#define HASH_FACTOR 33


/******************************************************************************/
/* Crea un simbolo segun la informacion recibida ******************************/
/******************************************************************************/
INFO_SIMBOLO *crear_info_simbolo(const char *lexema, CATEGORIA categ, TIPO tipo,
CLASE clase, int adic1, int adic2){

  INFO_SIMBOLO *s = NULL;

  s = (INFO_SIMBOLO*)calloc(1, sizeof(INFO_SIMBOLO));
  if(!s){
    return NULL;
  }

  s->lexema = (char*)calloc((strlen(lexema)+1),sizeof(char));
  strcpy(s->lexema, lexema);

  s->categoria = categ;
  s->tipo = tipo;
  s->clase = clase;
  s->adicional1 = adic1;
  s->adicional2 = adic2;

  return s;
}

/* Libera simbolo*/
void liberar_info_simbolo(INFO_SIMBOLO *is){
  if(!is){
    return;
  }
  free(is->lexema);
  free(is);
}
/******************************************************************************/
/* Crea un nodo, cuya informacion es el simbolo que recibe ********************/
/******************************************************************************/
NODO_HASH *crear_nodo(INFO_SIMBOLO *is){

  NODO_HASH *n = NULL;

  n = (NODO_HASH*)malloc(sizeof(NODO_HASH));
  if(!n){
    liberar_nodo(n);
    return NULL;
  }

  n->info = is;
  n->siguiente = NULL;

  return n;
}

/******************************************************************************/
/* Libera nodo ****************************************************************/
/******************************************************************************/
void liberar_nodo(NODO_HASH *nh){
  if(!nh){
    return;
  }

  if (nh->info != NULL){
    liberar_info_simbolo(nh->info);
  }
  free(nh);
}

/******************************************************************************/
/* Crea una tabla hash segun el tamaño recibido *******************************/
/******************************************************************************/
TABLA_HASH *crear_tabla(int tam){

  TABLA_HASH *t = NULL;
  /*como se indica en las diapositivas valores entre 1-64*/
  if(tam < 0){
    fprintf(stdout, "accede aqui\n");
    return NULL;
  }

  t = (TABLA_HASH*)malloc(tam*sizeof(TABLA_HASH));
  if(!t){
    fprintf(stdout, "accede aqui2\n");
    return NULL;
  }

  t->tam = tam;
  t->tabla = (NODO_HASH**)calloc(tam, sizeof(NODO_HASH*));
  if(!t->tabla){
    fprintf(stdout, "accede aqui3\n");
    free(t);
    return NULL;
  }

  return t;
}

/******************************************************************************/
/* Libera tabla hash **********************************************************/
/******************************************************************************/
void liberar_tabla(TABLA_HASH *th){

  int i=0;
  NODO_HASH *n, *x;

      if (th) {
          if (th->tabla) {
              for (i = 0; i < th->tam; i++) {
                  n = th->tabla[i];
                  while (n) {
                      x = n->siguiente;
                      liberar_nodo(n);
                      n = x;
                  }
              }
              free(th->tabla);
          }
          free(th);
      }
}
/******************************************************************************/
/*Convierte la cadena recibida en un numero ***********************************/
/******************************************************************************/
unsigned long hash(const char *str){

  unsigned long hash_h = HASH_INI;
  unsigned char *c;

  c = (unsigned char*) str;
  while (*c){
    hash_h = (hash_h * HASH_FACTOR) + *c; /* hash * 33 + c */
    c++;
  }

  return hash_h;
}

/******************************************************************************/
/* Busca en la tabla hash el simbolo indicado y lo devuelve si lo encuentra ***/
/******************************************************************************/
INFO_SIMBOLO *buscar_simbolo(const TABLA_HASH *th, const char *lexema){

  unsigned int indice_hash;
  NODO_HASH *aux = NULL;

  if(!th){
    fprintf(stdout, "aqui\n");
    return NULL;
  }

  indice_hash = hash(lexema) % th->tam;
  /* recorre la tabla de nodos comprobando si ese nodo esta en la tabla*/
  for(aux = th->tabla[indice_hash]; aux && (!aux->info || strcmp(lexema, aux->info->lexema)); aux = aux->siguiente);

  if(aux) {
    return aux->info;
  }
  return NULL;
}

/******************************************************************************/
/* Inserta el simbolo en la tabla hash segun la informacion recibida **********/
/******************************************************************************/
STATUS insertar_simbolo(TABLA_HASH *th, const char *lexema, CATEGORIA categ, TIPO
tipo, CLASE clase, int adic1, int adic2){

  int i;
  INFO_SIMBOLO *s;
  NODO_HASH *n = NULL;

  if (buscar_simbolo(th, lexema)) {
      return ERR;
  }

  i = hash(lexema) % th->tam;
  if (!(s = crear_info_simbolo(lexema, categ, tipo, clase, adic1, adic2))) {
      return ERR;
  }
  if (!(n = crear_nodo(s))) {
      liberar_info_simbolo(s);
      return ERR;
  }

  n->siguiente = th->tabla[i];
  th->tabla[i] = n;
  return OK;
}

/******************************************************************************/
/* Borra el simbolo indicado de la tabla **************************************/
/******************************************************************************/
void borrar_simbolo(TABLA_HASH *th, const char *lexema){

  int indice_hash;
  unsigned long hash_h;
  NODO_HASH *aux = NULL;
  NODO_HASH *anterior = NULL;

  if(!th){
    return;
  }

  hash_h = hash(lexema);
  indice_hash = hash_h % th->tam;
  /* recorre la tabla de nodos comprobando si ese nodo esta en la tabla*/
  for(aux = th->tabla[indice_hash]; !aux; aux = aux->siguiente){
    anterior = aux;
    if(strcmp(lexema, aux->info->lexema)==0){
      anterior->siguiente = aux->siguiente;
      liberar_nodo(aux);
      return;
    }
  }

  return;
}
