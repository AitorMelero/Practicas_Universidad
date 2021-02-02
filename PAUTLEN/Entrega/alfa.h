#ifndef _ALFA_H_
#define _ALFA_H_

#include "tablaHash.h"
#include "tablaSimbolos.h"
#include "generacion.h"

#define TIPO_ENTERO 0
#define TIPO_LOGICO 1
#define MAX_LONG_ID 100
#define MAX_TAMANIO_VECTOR 64

typedef struct {
  char lexema[MAX_LONG_ID+1]; /*guarda el lexema de los identificadores*/
  int tipo; /*guarda el tipo de una expresión (TIPO_ENTERO, TIPO_LOGICO)*/
  int valor_entero; /*guarda el valor de una constante entera.*/
  int es_direccion; /*indica si un símbolo es dirección o un valor constante.*/
  int etiqueta;
} tipo_atributos;

#endif