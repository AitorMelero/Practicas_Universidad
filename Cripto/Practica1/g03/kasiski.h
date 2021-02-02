/**************************************************************
 * File: kasiski.h
 * Author: Aitor Melero, Ana Roa
 * Date: 17/10/2020
 * Last_Date: 17/10/2020
 * Function: Definicion de funcionalidad de Kasiski.
 * ***********************************************************/


#ifndef KASISKI_H
#define KASISKI_H

/* INCLUDES */
#include "type.h"
#include "euclides.h"


/* ESTRUCTURA KASISKI */
typedef struct {
    FILE* i;                /* fichero con texto cifrado */
    char* texto_cifrado;    /* cadena con texto cifrado sin espacios */
    int l;                  /* longitud de las subcadenas repetidas */
    int* distancias;        /* lista con las distancias de subcadena a subcadena */
    int num_distancias;     /* numero de distancias */
    int mcd;                /* posible longitud de claves */
} KASISKI;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_kasiski(FILE* i, int l)
 * Function: Crear un elemento de tipo KASISKI.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              int l: Longitud de subcadenas repetidas
 * Return:
 *              KASISKI: Elemento KASISKI creado.
 * ***********************************************************/
KASISKI* create_kasiski(FILE* i, int l);


/*************************************************************/

/**************************************************************
 * Name: free_kasiski(KASISKI* kasiski)
 * Function: Liberar un elemento de tipo KASISKI.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_kasiski(KASISKI* kasiski);


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generador_texto_cifrado_kasiski(KASISKI* kasiski)
 * Function: Lee del fichero cifrado y escribe dicho texto sin espacios.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_cifrado_kasiski(KASISKI* kasiski);


/*************************************************************/

/**************************************************************
 * Name: generador_distancias_kasiski(KASISKI* kasiski)
 * Function: Genera las distancias de subcadenas repetidas.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_distancias_kasiski(KASISKI* kasiski);


/*************************************************************/

/**************************************************************
 * Name: generador_mcd_kasiski(KASISKI* kasiski)
 * Function: Guarda la posible longitud de clave.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_mcd_kasiski(KASISKI* kasiski);


#endif /* KASISKI_H */