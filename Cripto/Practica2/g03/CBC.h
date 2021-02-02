/**************************************************************
 * File: CBC.h
 * Author: Aitor Melero, Ana Roa
 * Date: 5/11/2020
 * Last_Date: 21/11/2020
 * Function: Definicion de funcionalidad del modo CBC. 
 * ***********************************************************/

#include "type.h"

/* FUNCIONES */

/**************************************************************
 * Name: cifrar_CBC(unsigned char* clave, unsigned char* iv, unsigned char* i, int tam_i)
 * Function: Cifra un texto usando el modo de operacion CBC en DES. 
 * Parameters:
 *             unsigned char* clave: Clave de 64 bits para cifrar. 
 *             unsigned char* iv: Vector de inicializacion.
 *             unsigned char* i: Texto plano a cifrar.
 *             int tam_i: Tamanio del texto plano.
 * Return:
 *             unsigned char*: Texto cifrado. 
 * ***********************************************************/
unsigned char* cifrar_CBC(unsigned char* clave, unsigned char* iv, unsigned char* i, int tam_i);

/**************************************************************
 * Name: descifrar_CBC(unsigned char* clave, unsigned char* iv, unsigned char* i, int tam_i)
 * Function: Descifra un texto usando el modo de operacion CBC en DES. 
 * Parameters:
 *             unsigned char* clave: Clave de 64 bits para cifrar. 
 *             unsigned char* iv: Vector de inicializacion.
 *             unsigned char* i: Texto cifrado de entrada. 
 *             int tam_i: Tamanio del texto cifrado.
 * Return:
 *             unsigned char*: Texto plano. 
 * ***********************************************************/
unsigned char* descifrar_CBC(unsigned char* clave, unsigned char* iv, unsigned char* i, int tam_i);
