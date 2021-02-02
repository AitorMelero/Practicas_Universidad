/**************************************************************
 * File: vigenere.h
 * Author: Aitor Melero, Ana Roa
 * Date: 08/10/2020
 * Last_Date: 08/10/2020
 * Function: Definicion de funcionalidad del algoritmo vigenere.
 * ***********************************************************/


#ifndef VIGENERE_H
#define VIGENERE_H


/* INCLUDES */
#include "type.h"


/* ESTRUCTURA VIGENERE */
typedef struct {
    mpz_t m;                         /* tamanio del espacio de texto cifrado */
    char* k;                         /* clave */
    FILE* i;                         /* fichero de entrada */
    FILE* o;                         /* fichero de salida */
} VIGENERE;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_vigenere(mpz_t m, char* k, FILE* i, FILE* o)
 * Function: Crear un elemento de tipo VIGENERE.
 * Parameters:
 *              mpz_t m: Tamanio del espacio de texto cifrado.
 *              char* k: Clave para vigenere.
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              VIGENERE: Elemento VIGENERE creado.
 * ***********************************************************/
VIGENERE* create_vigenere(char* k, FILE* i, FILE* o);


/*************************************************************/

/**************************************************************
 * Name: free_vigenere(VIGENERE* vigenere)
 * Function: Liberar un elemento de tipo VIGENERE.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_vigenere(VIGENERE* vigenere);


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(VIGENERE* vigenere)
 * Function: Devuelve el fichero de entrada usado en vigenere.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              FILE*: Fichero de entrada usado en vigenere.
 * ***********************************************************/
FILE* get_input_file(VIGENERE* vigenere);


/*************************************************************/

/**************************************************************
 * Name: get_output_file(VIGENERE* vigenere)
 * Function: Devuelve el fichero de salida usado en vigenere.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              FILE*: Fichero de salida usado en vigenere.
 * ***********************************************************/
FILE* get_output_file(VIGENERE* vigenere);


/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: cifrar(VIGENERE* vigenere)
 * Function: Cifra un fichero usando cifrado viegenere.
 * Parameters:
 *              VIGENERE* viegenere: Elemento de tipo vigenere.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS cifrar(VIGENERE* vigenere);


/*************************************************************/

/**************************************************************
 * Name: descifrar(VIGENERE* vigenere)
 * Function: Descifra un fichero usando cifrado vigenere.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS descifrar(VIGENERE* vigenere);


#endif /* VIGENERE_H */