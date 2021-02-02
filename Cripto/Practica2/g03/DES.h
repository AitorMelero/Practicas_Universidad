/**************************************************************
 * File: DES.h 
 * Author: Aitor Melero, Ana Roa
 * Date: 05/11/2020
 * Last_Date: 16/11/2020 
 * Function: Definicion de funcionalidad del algoritmo de cifrado
 *           DES. 
 * ***********************************************************/

#include "type.h"


/* FUNCIONES */

/**************************************************************
 * Name: cifrar_DES(unsigned char* texto_plano, unsigned char* clave)
 * Function: Cifra un bloque de texto de 64 bits con una clave de 64 bits. 
 * Parameters:
 *             unsigned char* texto_plano: Bloque de 64 bits a cifrar.
 *             unsigned char* clave: Clave de 64 bits para cifrar. 
 * Return:
 *             unsigned char*: Texto cifrado con DES. 
 * ***********************************************************/
unsigned char* cifrar_DES(unsigned char* texto_plano, unsigned char* clave);

/**************************************************************
 * Name: descifrar_DES(unsigned char* texto_cifrado, unsigned char* clave)
 * Function: Descifra un bloque de texto de 64 bits con una clave de 64 bits. 
 * Parameters:
 *             unsigned char* texto_descifrado: Bloque de 64 bits a descifrar.
 *             unsigned char* clave: Clave de 64 bits para descifrar. 
 * Return:
 *             unsigned char*: Texto descifrado con DES. 
 * ***********************************************************/
unsigned char* descifrar_DES(unsigned char* texto_cifrado, unsigned char* clave);


/*************************************************************/
/* PUBLICA PARA ESTUDIO **************************************/
/*************************************************************/

/**************************************************************
 * Name: funcion_S(unsigned char* ek)
 * Function: Genera el bloque de 32 bits a partir de 48 bits con
 *           las cajas S. 
 * Parameters:
 *             unsigned char*: Bloque de 32 bits generado. 
 * ***********************************************************/
unsigned char* funcion_S(unsigned char* ek);

/**************************************************************
 * Name: sacar_fila(unsigned char bloque)
 * Function: Genera el numero de fila de la caja S. 
 * Parameters:
 *             unsigned char bloque: Bloque donde mirar el numero de fila. 
 * Return:
 *            int: Numero de fila a mirar. 
 * ***********************************************************/
int sacar_fila(unsigned char bloque);


/**************************************************************
 * Name: sacar_columna(unsigned char bloque)
 * Function: Genera el numero de columna de la caja S. 
 * Parameters:
 *             unsigned char bloque: Bloque donde mirar el numero de columna. 
 * Return:
 *            int: Numero de columna a mirar. 
 * ***********************************************************/
int sacar_columna(unsigned char bloque);


/**************************************************************
 * Name: genera_S(int num_caja, int fila, int columna)
 * Function: Guardamos el contenido de la caja S. 
 * Parameters:
 *             int num_caja: Numero de caja.
 *             int fila: La fila donde esta el contenido.
 *             int columna: La columna donde esta el contenido. 
 * Return:
 *             unsigned char: El contenido de la caja S.
 * ***********************************************************/
unsigned char genera_S(int num_caja, int fila, int columna);

/**************************************************************
 * Name: funcion_IP(unsigned char* lr)
 * Function: Realiza la permutacion inicial IP. 
 * Parameters:
 *             unsigned char* lr: Bloque inicial de 64 bits a permutar.
 * Return:
 *             unsigned char*: Bloque permutado. 
 * ***********************************************************/
unsigned char* funcion_IP(unsigned char* lr);

/**************************************************************
 * Name: funcion_PC1(unsigned char* k)
 * Function: Realiza la PC1 para la clave. 
 * Parameters:
 *             unsigned char* k: Clave.
 * Return:
 *             unsigned char*: Clave de 56 bits. 
 * ***********************************************************/
unsigned char* funcion_PC1(unsigned char* k);

/**************************************************************
 * Name: genera_LCS(unsigned char* cndn)
 * Function: Genera los desplazamientos LCS para las 16 rondas. 
 * Parameters:
 *             unsigned char* cndn: CnDn.
 * Return:
 *             unsigned char**: Lista con todos los desplazamientos LCS. 
 * ***********************************************************/
unsigned char** genera_LCS(unsigned char* cndn);

/**************************************************************
 * Name: funcion_PC2(unsigned char* cndn)
 * Function: Realiza la PC2 para la clave. 
 * Parameters:
 *             unsigned char* cndn: CnDn.
 * Return:
 *             unsigned char*: Kn. 
 * ***********************************************************/
unsigned char* funcion_PC2(unsigned char* cndn);

/**************************************************************
 * Name: funcion_F(unsigned char* rn, unsigned char* kn)
 * Function: Funcion F del algoritmo DES. 
 * Parameters:
 *             unsigned char* rn: Bloque de 32 bits de la derecha (LN).
 *             unsigned char* kn: Clave de 48 bits generada. 
 * Return:
 *             unsigned char*: Bloque de 32 bits generado de la funcion F. 
 * ***********************************************************/
unsigned char* funcion_F(unsigned char* rn, unsigned char* kn);
