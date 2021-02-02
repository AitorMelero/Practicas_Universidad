/**************************************************************
 * File: permutacion.h
 * Author: Aitor Melero, Ana Roa
 * Date: 23/10/2020
 * Last_Date: 23/10/2020
 * Function: Definicion de funcionalidad para algoritmo de permutacion.
 * ***********************************************************/


#ifndef PERMUTACION_H
#define PERMUTACION_H

/* INCLUDES */
#include "type.h"


/* ESTRUCTURA PERMUTACION */
typedef struct {
    FILE* i;                /* fichero de entrada */
    FILE* o;                /* fichero salida */
    int* k1;                /* vector clave 1 */
    int* k2;                /* vector clave 2 */
    char* texto;            /* texto del fichero de entrada */
    int*** matriz_texto;    /* lista de matrices del texto */
    int num_matrices;       /* numero de matrices */
    int m;                  /* numero de filas de las matrices */
    int n;                  /* numero de columnas de las matrices */
} PERMUTACION;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_permutacion(FILE* i, FILE* o, char* k1, char* k2)
 * Function: Crear un elemento de tipo PERMUTACION.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 *              char* k1: Cadena con el vector clave 1.
 *              char* k2: Cadena con el vector clave 2.
 * Return:
 *              PERMUTACION: Elemento PERMUTACION creado.
 * ***********************************************************/
PERMUTACION* create_permutacion(FILE* i, FILE* o, char* k1, char* k2);


/*************************************************************/

/**************************************************************
 * Name: free_permutacion(PERMUTACION* permutacion)
 * Function: Liberar un elemento de tipo PERMUTACION.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generar_claves_permutacion(PERMUTACION* permutacion, char* k1, char* k2)
 * Function: Genera los vectores clave en formato de lista de enteros.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generar_claves_permutacion(PERMUTACION* permutacion, char* k1, char* k2);


/*************************************************************/

/**************************************************************
 * Name: generador_texto_permutacion(PERMUTACION* permutacion)
 * Function: Lee del fichero de entrada y escribe dicho texto sin espacios.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/**************************************************************
 * Name: generar_matriz_texto_permutacion(PERMUTACION* permutacion)
 * Function: Genera la lista de matrices del texto de entrada.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generar_matriz_texto_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/**************************************************************
 * Name: cifrar_permutacion(PERMUTACION* permutacion)
 * Function: Cifra el texto de entrada usando el cifrado de permutacion.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS cifrar_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/**************************************************************
 * Name: descifrar_permutacion(PERMUTACION* permutacion)
 * Function: Descifra el texto de entrada usando el cifrado de permutacion.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS descifrar_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/**************************************************************
 * Name: generador_matriz_fila_permutacion(PERMUTACION* permutacion)
 * Function: Genera la matriz cuadrada para la permutacion de filas.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              int**: Matriz cuadrada para la permutacion de filas.
 * ***********************************************************/
int** generador_matriz_fila_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/**************************************************************
 * Name: generador_matriz_columna_permutacion(PERMUTACION* permutacion)
 * Function: Genera la matriz cuadrada para la permutacion de columnas.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              int**: Matriz cuadrada para la permutacion de columnas.
 * ***********************************************************/
int** generador_matriz_columna_permutacion(PERMUTACION* permutacion);


/*************************************************************/

/**************************************************************
 * Name: multiplicar_matriz_permutacion(int** matriz, int** matriz_permutacion, int m, int n, BOOL fila)
 * Function: Multiplica matrices para la permutacion.
 * Parameters:
 *              int** matriz: Matriz del texto a permutar.
 *              int** matriz_permutacion: Matriz fila o columna.
 *              int m: Numero de filas de la matriz.
 *              int n: Numero de columnas de la matriz.
 *              BOOL fila: Booleano que indica si tenemos matriz fila o matriz columna.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS multiplicar_matriz_permutacion(int** matriz, int** matriz_permutacion, int m, int n, BOOL fila);


/*************************************************************/

/**************************************************************
 * Name: calcular_matriz_traspuesta_permutacion(int** matriz, int tam)
 * Function: Genera la matriz traspuesta de una matriz dada.
 * Parameters:
 *              int** matriz: Matriz a calcular su traspuesta.
 *              int tam: Tamanio de la matriz cuadrada.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS calcular_matriz_traspuesta_permutacion(int** matriz, int tam);


/*************************************************************/

/**************************************************************
 * Name: escribe_salida_permutacion(PERMUTACION* permutacion)
 * Function: Escribe el texto-solucion en el fichero de salida.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS escribe_salida_permutacion(PERMUTACION* permutacion);


#endif /* PERMUTACION_H */