/**************************************************************
 * File: seg_perf.h
 * Author: Aitor Melero, Ana Roa
 * Date: 12/11/2020
 * Last_Date: 12/11/2020
 * Function: Demostración de Seguridad Perfecta del cifrado por
 *           desplazamiento
 * ***********************************************************/


#ifndef SEGPERF_H
#define SEGPERF_H

/* INCLUDES */
#include "type.h"

/* ESTRUCTURA SEGPERF */
typedef struct {
    FILE* i;                         /* fichero de entrada */
    FILE* o;                         /* fichero de salida */
} SEGPERF;

/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_segperf(FILE* i, FILE* o)
 * Function: Crear un elemento de tipo SEGPERF.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              SEGPERF: Elemento SEGPERF creado.
 * ***********************************************************/
SEGPERF* create_segperf(FILE* i, FILE* o);


/*************************************************************/

/**************************************************************
 * Name: free_segperf(SEGPERF* sg)
 * Function: Liberar un elemento de tipo SEGPERF.
 * Parameters:
 *              SEGPERF* SEGPERF: Elemento de tipo segperf.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_segperf(SEGPERF* sg);


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(SEGPERF* sg)
 * Function: Devuelve el fichero de entrada usado en sg.
 * Parameters:
 *              SEGPERF* SEGPERF: Elemento de tipo sg.
 * Return:
 *              FILE*: Fichero de entrada usado en sg.
 * ***********************************************************/
FILE* get_input_file(SEGPERF* sg);


/*************************************************************/

/**************************************************************
 * Name: get_output_file(SEGPERF* sg)
 * Function: Devuelve el fichero de salida usado en sg.
 * Parameters:
 *              SEGPERF* SEGPERF: Elemento de tipo sg.
 * Return:
 *              FILE*: Fichero de salida usado en sg.
 * ***********************************************************/
FILE* get_output_file(SEGPERF* sg);


/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: equiprobable(SEGPERF* sg)
 * Function: Cifra un fichero usando cifrado sg.
 * Parameters:
 *              SEGPERF* sg: Elemento de tipo sg.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS equiprobable(SEGPERF* sg);


/*************************************************************/

/**************************************************************
 * Name: no_equiprobable(SEGPERF* sg)
 * Function: Descifra un fichero usando cifrado sg.
 * Parameters:
 *              SEGPERF* sg: Elemento de tipo sg.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS no_equiprobable(SEGPERF* sg);


#endif 