/**************************************************************
 * File: flujo.h
 * Author: Aitor Melero, Ana Roa
 * Date: 21/10/2020
 * Last_Date: 21/10/2020
 * Function: Definicion de funcionalidad del algoritmo flujo.
 * ***********************************************************/


#ifndef FLUJO_H
#define FLUJO_H


/* INCLUDES */
#include "type.h"


/* ESTRUCTURA FLUJO */
typedef struct {
    mpz_t m;                         /* tamanio del espacio de texto cifrado */
    mpz_t a;                         /* coeficiente multiplicativo de la funcion afin */
    mpz_t b;                         /* termino constante de la funcion afin */
    FILE* i;                         /* fichero de entrada */
    FILE* o;                         /* fichero de salida */
} FLUJO;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_flujo(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o)
 * Function: Crear un elemento de tipo AFIN.
 * Parameters:
 *              mpz_t m: Tamanio del espacio de texto cifrado.
 *              mpz_t a: Coeficiente multiplicativo de la funcion afin.
 *              mpz_t b: Termino constante de la funcion afin.
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              FLUJO: Elemento FLUJO creado.
 * ***********************************************************/
FLUJO* create_flujo(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o);

/**************************************************************
 * Name: free_flujo(FLUJO* flujo)
 * Function: Liberar un elemento de tipo FLUJO.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_flujo(FLUJO* flujo);

/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(FLUJO* flujo)
 * Function: Devuelve el fichero de entrada usado en flujo.
 * Parameters:
 *              FLUJO* afin: Elemento de tipo flujo.
 * Return:
 *              FILE*: Fichero de entrada usado en flujo.
 * ***********************************************************/
FILE* get_input_file(FLUJO* flujo);

/**************************************************************
 * Name: get_output_file(FLUJO* flujo)
 * Function: Devuelve el fichero de salida usado en flujo.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              FILE*: Fichero de salida usado en flujo.
 * ***********************************************************/
FILE* get_output_file(FLUJO* flujo);

/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: cifrar(FLUJO* flujo)
 * Function: Cifra un fichero usando cifrado flujo.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS cifrar(FLUJO* flujo);

/*************************************************************/

/**************************************************************
 * Name: descifrar(FLUJO* flujo)
 * Function: Descifra un fichero usando cifrado flujo.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS descifrar(FLUJO* flujo);

#endif