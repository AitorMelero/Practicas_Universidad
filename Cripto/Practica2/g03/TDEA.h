/**************************************************************
 * File: TDEA.h
 * Author: Aitor Melero, Ana Roa
 * Date: 16/11/2020
 * Last_Date: 21/11/2020
 * Function: Definicion de funcionalidad del triple DES. 
 * ***********************************************************/

#include "type.h"

/* FUNCIONES */

/**************************************************************
 * Name: cifrar_TDEA_CBC(unsigned char* clave, unsigned char* iv, unsigned char* texto_plano, int tam_i);
 * Function: Cifra un texto usando el triple DES con CBC. 
 * Parameters:
 *             unsigned char* clave: Clave de 192 bits para cifrar. 
 *             unsigned char* iv: Vector de inicializacion.
 *             unsigned char* i: Texto plano a cifrar.
 *             int tam_i: Tamanio del texto plano.
 * Return:
 *             unsigned char*: Texto cifrado. 
 * ***********************************************************/
unsigned char* cifrar_TDEA_CBC(unsigned char* clave, unsigned char* iv, unsigned char* texto_plano, int tam_i);

/**************************************************************
 * Name: descifrar_TDEA_CBC(unsigned char* clave, unsigned char* iv, unsigned char* texto_cifrado, int tam_i);
 * Function: Descifra un texto usando el triple DES con CBC. 
 * Parameters:
 *             unsigned char* clave: Clave de 192 bits para cifrar. 
 *             unsigned char* iv: Vector de inicializacion.
 *             unsigned char* i: Texto cifrado de entrada. 
 *             int tam_i: Tamanio del texto cifrado.
 * Return:
 *             unsigned char*: Texto plano. 
 * ***********************************************************/
unsigned char* descifrar_TDEA_CBC(unsigned char* clave, unsigned char* iv, unsigned char* texto_cifrado, int tam_i);
