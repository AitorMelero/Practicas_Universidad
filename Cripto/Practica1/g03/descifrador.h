/**************************************************************
 * File: descifrador.h
 * Author: Aitor Melero, Ana Roa
 * Date: 18/10/2020
 * Last_Date: 18/10/2020
 * Function: Definicion de funcionalidad para descifrar texto
 *           texto cifrado con Vigenere.
 * ***********************************************************/


#ifndef DESCIFRADOR_H
#define DESCIFRADOR_H

/* INCLUDES */
#include "type.h"
#include "vigenere.h"
#include "kasiski.h"
#include "ic.h"


/* ESTRUCTURA DESCIFRADOR */
typedef struct {
    FILE* i;                /* fichero con texto cifrado */
    FILE* o;                /* fichero donde escribir texto claro */
    BOOL kasiski_met;       /* indica si la longitud de clave se halla con Kasiski o Ic */
    char* texto_cifrado;    /* cadena con texto cifrado sin espacios */
    int l;                  /* longitud de n-gramas o de cadenas repetidas */
    int long_clave;         /* posible longitud de claves */
    char** n_gramas;        /* tabla con los n-gramas */
    double** valores_mg;    /* tabla con valores de mg */
    char* clave;            /* posible clave */
    
} DESCIFRADOR;


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_descifrador(FILE* i, FILE* o, BOOL metodo, int l)
 * Function: Crear un elemento de tipo DESCIFRADOR.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero donde escribir el texto descifrado.
 *              BOOL metodo: Indica si usar kasiski o indice de coincidencias.
 *              int l: Longitud de n-gramas.
 * Return:
 *              DESCIFRADOR: Elemento DESCIFRADOR creado.
 * ***********************************************************/
DESCIFRADOR* create_descifrador(FILE* i, FILE* o, BOOL metodo, int l);


/*************************************************************/

/**************************************************************
 * Name: free_descifrador(DESCIFRADOR* descifrador)
 * Function: Liberar un elemento de tipo DESCIFRADOR.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_descifrador(DESCIFRADOR* descifrador);


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generador_texto_cifrado_descifrador(DESCIFRADOR* descifrador)
 * Function: Lee del fichero cifrado y escribe dicho texto sin espacios.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_cifrado_descifrador(DESCIFRADOR* descifrador);


/*************************************************************/

/**************************************************************
 * Name: generador_ngramas_descifrador(DESCIFRADOR* descifrador)
 * Function: Genera la tabla de n-gramas.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_ngramas_descifrador(DESCIFRADOR* descifrador);


/*************************************************************/

/**************************************************************
 * Name: generador_long_clave_descifrador(DESCIFRADOR* descifrador)
 * Function: Guarda la posible longitud de clave.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_long_clave_descifrador(DESCIFRADOR* descifrador);


/*************************************************************/

/**************************************************************
 * Name: generador_clave(DESCIFRADOR* descifrador)
 * Function: Descifra la clave.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_clave(DESCIFRADOR* descifrador);


/*************************************************************/

/**************************************************************
 * Name: generador_valores_mg(DESCIFRADOR* descifrador)
 * Function: Genera los valores mg.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_valores_mg(DESCIFRADOR* descifrador);


/*************************************************************/

/**************************************************************
 * Name: calcula_fi_descifrador(char letra, char* cadena)
 * Function: Calcula la frecuencia de una letra en una cadena.
 * Parameters:
 *              char letra: Letra a calcular la frecuencia.
 *              char* cadena: Cadena donde calcular la frecuencia de la letra.
 * Return:
 *              int: Frecuencia de la letra en la cadena.
 * ***********************************************************/
int calcula_fi_descifrador(char letra, char* cadena);


/*************************************************************/

/**************************************************************
 * Name: descifra_descifrador(DESCIFRADOR* descifrador)
 * Function: Descifra el mensaje con la clave hallada.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS descifra_descifrador(DESCIFRADOR* descifrador);


#endif /* DESCIFRADOR_H */