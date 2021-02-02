/**************************************************************
 * File: ic.h
 * Author: Aitor Melero, Ana Roa
 * Date: 18/10/2020
 * Last_Date: 18/10/2020
 * Function: Definicion de funcionalidad del indice de coincidencia.
 * ***********************************************************/


#ifndef IC_H
#define IC_H

/* INCLUDES */
#include "type.h"


/* ESTRUCTURA IC */
typedef struct {
    FILE* i;                /* fichero con texto cifrado */
    char* texto_cifrado;    /* cadena con texto cifrado sin espacios */
    int l;                  /* longitud de n-gramas */
    char** n_gramas;        /* tabla con los n-gramas */
    double resultado;          /* indice de coincidencia */
    int long_clave;         /* posible longitud de claves */
} IC;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_ic(FILE* i, int l)
 * Function: Crear un elemento de tipo IC.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              int l: Longitud de n-gramas
 * Return:
 *              IC: Elemento IC creado.
 * ***********************************************************/
IC* create_ic(FILE* i, int l);


/*************************************************************/

/**************************************************************
 * Name: free_ic(IC* ic)
 * Function: Liberar un elemento de tipo IC.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_ic(IC* ic);


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generador_texto_cifrado_ic(IC* ic)
 * Function: Lee del fichero cifrado y escribe dicho texto sin espacios.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_cifrado_ic(IC* ic);


/*************************************************************/

/**************************************************************
 * Name: generador_ngramas_ic(IC* ic)
 * Function: Genera la tabla de n-gramas.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_ngramas_ic(IC* ic);


/*************************************************************/

/**************************************************************
 * Name: generador_long_clave_ic(IC* ic)
 * Function: Guarda la posible longitud de clave.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_long_clave_ic(IC* ic);


/*************************************************************/

/**************************************************************
 * Name: calcular_ic(char* cadena)
 * Function: Calcula el indice de coincidencia.
 * Parameters:
 *              char* cadena: Cadena a calcular el ic.
 * Return:
 *              double: ic de la cadena pasada por argumento.
 * ***********************************************************/
double calcular_ic(char* cadena);


/*************************************************************/

/**************************************************************
 * Name: calcula_fi(char letra, char* cadena)
 * Function: Calcula la frecuencia de una letra en una cadena.
 * Parameters:
 *              char letra: Letra a calcular frecuencia.
 *              char* cadena: Cadena donde calcular la frecuencia.
 * Return:
 *              int: Frecuencia de la letra en la cadena.
 * ***********************************************************/
int calcula_fi(char letra, char* cadena);


#endif /* IC_H */