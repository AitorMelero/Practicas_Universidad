/**************************************************************
 * File: euclides.h
 * Author: Aitor Melero, Ana Roa
 * Date: 05/10/2020
 * Last_Date: 06/10/2020
 * Function: Definicion de funcionalidad de Euclides con GMP.
 * ***********************************************************/


#ifndef EUCLIDES_H
#define EUCLIDES_H


/* INCLUDES */
#include "type.h"


/* ESTRUCTURA EUCLIDES */
typedef struct {
    mpz_t m;              /* modulo */
    mpz_t a;              /* numero */
    mpz_t *r;             /* restos */
    int num_r;            /* tamanio lista r */
    mpz_t *q;             /* sumandos */
    int num_q;            /* tamanio lista q */
    char **solucion;      /* solucion */
    int num_solucion;     /* tamanio solucion */
    mpz_t mcd;            /* max.comun.div. */
    mpz_t inv;            /* inverso */
} EUCLIDES;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_euclides(mpz_t m, mpz_t a)
 * Function: Crear un elemento de tipo EUCLIDES.
 * Parameters:
 *              mpz_t m: Modulo a usar para Euclides.
 *              mpz_t a: Numero a usar para Euclides.
 * Return:
 *              EUCLIDES: Elemento EUCLIDES creado.
 * ***********************************************************/
EUCLIDES* create_euclides(mpz_t m, mpz_t a);


/*************************************************************/

/**************************************************************
 * Name: free_euclides(EUCLIDES* euclides)
 * Function: Liberar un elemento de tipo EUCLIDES.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES a liberar.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_euclides(EUCLIDES* euclides);


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/**************************************************************
 * Name: get_solucion(EUCLIDES* euclides)
 * Function: Devuelve una cadena con la solucion de euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 * Return:
 *              char**: Cadena con la solucion de euclides.
 * ***********************************************************/
char** get_solucion(EUCLIDES* euclides);


/*************************************************************/

/**************************************************************
 * Name: print_euclides(EUCLIDES* euclides)
 * Function: Imprime la solucion de euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void print_euclides(EUCLIDES* euclides);


#endif /* EUCLIDES_H */