/**************************************************************
 * File: afin.h
 * Author: Aitor Melero, Ana Roa
 * Date: 03/10/2020
 * Last_Date: 07/10/2020
 * Function: Definicion de funcionalidad del algoritmo afin.
 * ***********************************************************/


#ifndef AFIN_H
#define AFIN_H


/* INCLUDES */
#include "type.h"


/* ESTRUCTURA AFIN */
typedef struct {
    mpz_t m;                         /* tamanio del espacio de texto cifrado */
    mpz_t a;                         /* coeficiente multiplicativo de la funcion afin */
    mpz_t b;                         /* termino constante de la funcion afin */
    FILE* i;                         /* fichero de entrada */
    FILE* o;                         /* fichero de salida */
} AFIN;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_afin(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o)
 * Function: Crear un elemento de tipo AFIN.
 * Parameters:
 *              mpz_t m: Tamanio del espacio de texto cifrado.
 *              mpz_t a: Coeficiente multiplicativo de la funcion afin.
 *              mpz_t b: Termino constante de la funcion afin.
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              AFIN: Elemento AFIN creado.
 * ***********************************************************/
AFIN* create_afin(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o);


/*************************************************************/

/**************************************************************
 * Name: free_afin(AFIN* afin)
 * Function: Liberar un elemento de tipo AFIN.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_afin(AFIN* afin);


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(AFIN* afin)
 * Function: Devuelve el fichero de entrada usado en afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              FILE*: Fichero de entrada usado en afin.
 * ***********************************************************/
FILE* get_input_file(AFIN* afin);


/*************************************************************/

/**************************************************************
 * Name: get_output_file(AFIN* afin)
 * Function: Devuelve el fichero de salida usado en afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              FILE*: Fichero de salida usado en afin.
 * ***********************************************************/
FILE* get_output_file(AFIN* afin);


/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: cifrar(AFIN* afin)
 * Function: Cifra un fichero usando cifrado afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS cifrar(AFIN* afin);


/*************************************************************/

/**************************************************************
 * Name: descifrar(AFIN* afin)
 * Function: Descifra un fichero usando cifrado afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS descifrar(AFIN* afin);


#endif /* AFIN_H */