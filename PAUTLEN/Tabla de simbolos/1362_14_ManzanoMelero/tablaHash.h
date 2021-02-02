/*
 * fichero: tablaHash.h
 * autores: Miguel Manzano, Aitor Melero
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef TABLA_H
#define TABLA_H
/* posición inicial de parámetros de función (empiezan a contar en 0) */
#define POS_INI_PARAMS 0
/* posición de inicio de variables locales de función (empiezan a contar en 1) */
#define POS_INI_VARS_LOCALES 1
#define HASH_INI 5381
#define HASH_FACTOR 33

/* Retorno de función */
typedef enum { ERR = 0, OK = 1 } STATUS;

/* Categoría de un símbolo*/
typedef enum { VARIABLE, PARAMETRO, FUNCION } CATEGORIA;

/* Tipo de dato*/
typedef enum { ENTERO, BOOLEANO } TIPO;

/* Clase de un símbolo*/
typedef enum { ESCALAR, VECTOR } CLASE;

/* Información de un símbolo */
typedef struct {
char *lexema;
CATEGORIA categoria;
TIPO tipo;
CLASE clase;
int adicional1;/* valor si escalar, longitud si vector, número de parámetros si función */
int adicional2; /* posición en llamada a función si parámetro, posición de declaración si
variable local de función, número de variables locales si función */
} INFO_SIMBOLO;

/* Nodo de la tabla hash */
typedef struct nodo_hash {
INFO_SIMBOLO *info;
struct nodo_hash *siguiente; /* puntero al siguiente nodo (encadenamiento si colisión en misma celda) */
} NODO_HASH;

/* Tabla hash */
typedef struct {
int tam;
NODO_HASH **tabla;
} TABLA_HASH;

/* Crea un simbolo segun la informacion recibida*/
INFO_SIMBOLO *crear_info_simbolo(const char *lexema, CATEGORIA categ, TIPO tipo,
CLASE clase, int adic1, int adic2);

/* Libera simbolo*/
void liberar_info_simbolo(INFO_SIMBOLO *is);

/* Crea un nodo, cuya informacion es el simbolo que recibe*/
NODO_HASH *crear_nodo(INFO_SIMBOLO *is);

/* Libera nodo*/
void liberar_nodo(NODO_HASH *nh);

/* Crea una tabla hash segun el tamaño recibido*/
TABLA_HASH *crear_tabla(int tam);

/* Libera tabla hash*/
void liberar_tabla(TABLA_HASH *th);

/*Convierte la cadena recibida en un numero*/
unsigned long hash(const char *str);

/* Busca en la tabla hash el simbolo indicado y devuelve el simbolo si lo encuentra*/
INFO_SIMBOLO *buscar_simbolo(const TABLA_HASH *th, const char *lexema);

/* Inserta el simbolo en la tabla hash segun la informacion recibida*/
STATUS insertar_simbolo(TABLA_HASH *th, const char *lexema, CATEGORIA categ, TIPO
tipo, CLASE clase, int adic1, int adic2);

/* Borra el simbolo indicado de la tabla*/
void borrar_simbolo(TABLA_HASH *th, const char *lexema);

#endif
