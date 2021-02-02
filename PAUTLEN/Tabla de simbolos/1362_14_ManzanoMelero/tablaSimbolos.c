/*
 * fichero: tablaSimbolos.c
 * autores: Miguel Manzano, Aitor Melero
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tablaSimbolos.h"

/* LAS DOS SIGUIENTES LÍNEAS SON PARA EL MODO EXTENDIDO CON VARIAS
   TABLAS DE SIMBOLOS LOCALES (FUNCIONES ANIDADAS)
   SI QUEREMOS LA IMPLEMENTACION QUE SIRVE PARA EL COMPILADOR,
   BASTARIA CON DECLARAR:*/

TABLA_HASH *TablaSimbolosLocal = NULL;
TABLA_HASH *TablaSimbolosGlobal = NULL;


/******************************************************************************/
/* Crea una tabla de simbolos con un ambito global y uno local ****************/
/******************************************************************************/
STATUS declarar(const char *id, INFO_SIMBOLO *desc_id) {

  if(TablaSimbolosLocal == NULL) {
    return declararGlobal(id, desc_id);
  } else {
    return declararLocal(id, desc_id);
  }
}


/******************************************************************************/
/* declara una variable global y la inserta en la tabla global si no esta ya **/
/******************************************************************************/
STATUS declararGlobal(const char *id, INFO_SIMBOLO *desc_id){

  if(TablaSimbolosGlobal == NULL) {
    TablaSimbolosGlobal = crear_tabla(TABLA_SIMBOLOS_GLOBAL_TAM);
    if(TablaSimbolosGlobal == NULL) {
        return ERR;
    }
  }
  if(buscar_simbolo(TablaSimbolosGlobal, id) == NULL){
    return insertar_simbolo(TablaSimbolosGlobal, id, desc_id->categoria, desc_id->tipo, desc_id->clase, desc_id->adicional1, desc_id->adicional2);
  }
  else return ERR;
}

/******************************************************************************/
/* declara una variable local y la inserta en la tabla local si no esta ya ****/
/******************************************************************************/
STATUS declararLocal(const char *id, INFO_SIMBOLO *desc_id){

  if(buscar_simbolo(TablaSimbolosLocal, id) == NULL){
    return insertar_simbolo(TablaSimbolosLocal, id, desc_id->categoria, desc_id->tipo, desc_id->clase, desc_id->adicional1, desc_id->adicional2);
  }
  else return ERR;
}

/******************************************************************************/
/* devuelve el simbolo indicado de la tabla global ****************************/
/******************************************************************************/
INFO_SIMBOLO *usoGlobal(const char *id){
  INFO_SIMBOLO *dato = NULL;

  if(!TablaSimbolosGlobal){
    return NULL;
  }

  dato = buscar_simbolo(TablaSimbolosGlobal, id);
  if(!dato){
    liberar_info_simbolo(dato);
    return NULL;
  }

  return dato;
}

/******************************************************************************/
/* devuelve el simbolo indicado de la tabla local *****************************/
/******************************************************************************/
INFO_SIMBOLO *usoLocal(const char *id){
  INFO_SIMBOLO *dato = NULL;

  if(!TablaSimbolosLocal){
    return usoGlobal(id);
  }

  dato = buscar_simbolo(TablaSimbolosLocal, id);
  if(!dato){
    return usoGlobal(id);
  }

  return dato;
}

/******************************************************************************/
/* crea un ambito local para declarar la funcion siempre que no este declarada*/
/******************************************************************************/
STATUS declararFuncion(const char *id, INFO_SIMBOLO *desc_id){

  if(buscar_simbolo(TablaSimbolosGlobal, id) == NULL) {

    if(insertar_simbolo(TablaSimbolosGlobal, id, desc_id->categoria, desc_id->tipo, desc_id->clase, desc_id->adicional1, desc_id->adicional2) == ERR) {
      return ERR;
    }

    liberar_tabla(TablaSimbolosLocal);
    TablaSimbolosLocal = crear_tabla(TABLA_SIMBOLOS_LOCAL_TAM);

    if(TablaSimbolosLocal == NULL) {
      borrar_simbolo(TablaSimbolosGlobal, id);
      liberar_tabla(TablaSimbolosLocal);
      TablaSimbolosLocal = NULL;
      return ERR;
    }

    if(insertar_simbolo(TablaSimbolosLocal, id, desc_id->categoria, desc_id->tipo, desc_id->clase, desc_id->adicional1, desc_id->adicional2) == ERR) {
      borrar_simbolo(TablaSimbolosGlobal, id);
      liberar_tabla(TablaSimbolosLocal);
      TablaSimbolosLocal = NULL;
      return ERR;
    }

    return OK;
  }

  return ERR;
}

/******************************************************************************/
/* cierra el ambito local reservado para la funcion ***************************/
/******************************************************************************/
STATUS cerrarFuncion(){

  /* si queremos cerrar una funcion el ambito local debe contener las variables
  relacionadas con esa funcion, por lo tanto no puede estar vacio y seria error*/
  if(!TablaSimbolosLocal){
    return ERR;
  }

  liberar_tabla(TablaSimbolosLocal);
  TablaSimbolosLocal = NULL;
  return OK;
}

/******************************************************************************/
/* libera la memoria reservada para la tabla de simbolos **********************/
/******************************************************************************/
void terminar(){

  if(TablaSimbolosGlobal != NULL){
    liberar_tabla(TablaSimbolosGlobal);
  }
  if(TablaSimbolosLocal != NULL){
    liberar_tabla(TablaSimbolosLocal);
  }

}
