/*
 * fichero: tablaSimbolos.h
 * autores: Miguel Manzano, Aitor Melero
 */

#ifndef TABLASIMBOLOS_H
#define TABLASIMBOLOS_H

#include "tablaHash.h"

#define TABLA_SIMBOLOS_GLOBAL_TAM       65536
#define TABLA_SIMBOLOS_LOCAL_TAM        65536

/*SOLO PARA EL MODO EXTENDIDO CON VARIAS TABLAS DE SIMBOLOS LOCALES (FUNCIONES ANIDADAS)*/
#define TABLA_SIMBOLOS_LOCAL_NIVELES    5

/*declara una tabla de simbolos*/
STATUS declarar(const char* id, INFO_SIMBOLO* desc_id);

/*declara una variable global y la inserta en la tabla global si no esta ya*/
STATUS declararGlobal(const char *id, INFO_SIMBOLO *desc_id);

/*declara una variable local y la inserta en la tabla local si no esta ya*/
STATUS declararLocal(const char *id, INFO_SIMBOLO *desc_id);

/*devuelve el simbolo indicado de la tabla global*/
INFO_SIMBOLO *usoGlobal(const char *id);

/*devuelve el simbolo indicado de la tabla local*/
INFO_SIMBOLO *usoLocal(const char *id);

/*crea un ambito local para declarar la funcion siempre que no este declarada*/
STATUS declararFuncion(const char *id, INFO_SIMBOLO *desc_id);

/*cierra el ambito local reservado para la funcion*/
STATUS cerrarFuncion();

/*libera la memoria reservada para la tabla de simbolos*/
void terminar();

/*indica si el simbolo pertence al ambito local*/
int EsLocal(const char *id);

#endif /* TABLASIMBOLOS_H */
