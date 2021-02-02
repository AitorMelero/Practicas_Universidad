/**************************************************************
 * File: operaciones_bit.h 
 * Author: Aitor Melero, Ana Roa
 * Date: 05/11/2020
 * Last_Date: 16/11/2020 
 *           en c. 
 * ***********************************************************/
#ifndef OPERACIONES_H 
#define	OPERACIONES_H

#include "type.h"


/* FUNCIONES */

/**************************************************************
 * Name: print_binint(int num, unsigned char* bits)
 * Function: Genera una cadena de bits a partir de un bloque de 8 bits.
 * Parameters:
 *             int num: Bloque de bits. 
 *             unsigned char* bits: Cadena que representa los bits. 
 * Return:
 *            void: Nada. 
 * ***********************************************************/
void print_binint(int num, unsigned char* bits);

/**************************************************************
 * Name: copiar_cadena(unsigned char* cad1, unsigned char* cad2, int num_bloque)
 * Function: Copia dos cadenas sin '\0'. 
 * Parameters:
 *             unsigned char* cad1: Cadena destino.
 *             unsigned char* cad2: Cadena origen. 
 *             int num_bloque: Numero de bloques de 8 bits de la cadena.
 * Return:
 *            void: Nada. 
 * ***********************************************************/
void copiar_cadena(unsigned char* cad1, unsigned char* cad2, int num_bloque);

/**************************************************************
 * Name: genera_aleatorio(int min, int max) 
 * Function: Genera un numero aleatorio entre min y max. 
 * Parameters:
 *             int min: Minimo numero aleatorio posible.
 *             int max: Maximo numero aleatorio posible.
 * Return:
 *             unsigned char: Numero aleatorio generado.
 * ***********************************************************/
unsigned char genera_aleatorio(int min, int max);

/**************************************************************
 * Name: genera_clave() 
 * Function: Genera una clave de 64 bits con la paridad correcta. 
 * Parameters:
 *             Ninguno.
 * Return:
 *             unsigned char: Numero aleatorio generado.
 * ***********************************************************/
unsigned char* genera_clave();

/**************************************************************
 * Name: calcula_paridad(unsigned char bloque) 
 * Function: Calcula la paridad impar de un bloque de 7 bits. 
 * Parameters:
 *             unsigned char bloque: Bloque de 7 bits.
 * Return:
 *             unsigned char: FF o FE en funcion de la paridad. 
 * ***********************************************************/
unsigned char calcula_paridad(unsigned char bloque);

/**************************************************************
 * Name: desplazamiento_izq(unsigned char* bloque, int desp, int num_bloques)
 * Function: Desplaza hacia la izquierda uno o varios bloques de 8 bits. 
 * Parameters:
 *             unsigned char* bloque: Bloque de bits a desplazar.
 *             int desp: Numero de bits a desplazar.
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del desplazamiento. 
 * ***********************************************************/
unsigned char* desplazamiento_izq(unsigned char* bloque, int desp, int num_bloques);

/**************************************************************
 * Name: desplazamiento_der(unsigned char* bloque, int desp, int num_bloques)
 * Function: Desplaza hacia la derecha uno o varios bloques de 8 bits. 
 * Parameters:
 *             unsigned char* bloque: Bloque de bits a desplazar.
 *             int desp: Numero de bits a desplazar.
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del desplazamiento. 
 * ***********************************************************/
unsigned char* desplazamiento_der(unsigned char* bloque, int desp, int num_bloques);

/**************************************************************
 * Name: xor(unsigned char* bloque, unsigned char* bloque2, int num_bloques)
 * Function: Operacion xor para dos bloques de bits.
 * Parameters:
 *             unsigned char* bloque: Bloque de bits.
 *             unsigned char* bloque2: Bloque de bits. 
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del xor. 
 * ***********************************************************/
unsigned char* xor(unsigned char* bloque, unsigned char* bloque2, int num_bloques);

/**************************************************************
 * Name: and(unsigned char* bloque, unsigned char* bloque2, int num_bloques)
 * Function: Operacion and para dos bloques de bits.
 * Parameters:
 *             unsigned char* bloque: Bloque de bits.
 *             unsigned char* bloque2: Bloque de bits. 
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del and. 
 * ***********************************************************/
unsigned char* and(unsigned char* bloque, unsigned char* bloque2, int num_bloques);

/**************************************************************
 * Name: paridad(unsigned char* clave)
 * Function: Comprueba la paridad de la clave. 
 * Parameters:
 *             unsigned char* clave: Clave a analizar.
 * Return:
 *             int: 1 si se cumple la paridad, 0 en caso contrario.
 * ***********************************************************/
int paridad(unsigned char* clave);

/**************************************************************
 * Name: funcion_xtime(unsigned char num)
 * Function: Funcion xtime para un numero. 
 * Parameters:
 *            unsigned char num: Numero para xtime.
 * Return:
 *            unsigned char: Solucion de xtime.
 * ***********************************************************/
unsigned char funcion_xtime(unsigned char num);

/**************************************************************
 * Name: multiplica_polinomios(unsigned char num1, unsigned char num2)
 * Function: Funcion que multiplica dos bloques de 8 bits. 
 * Parameters:
 *            unsigned char num1: Bloque uno a multiplicar. 
 *            unsigned char num2: Bloque dos a multiplicar. 
 * Return:
 *            unsigned char: Resultado de la multiplicacion. 
 * ***********************************************************/
unsigned char multiplica_polinomios(unsigned char num1, unsigned char num2);

#endif /* OPERACIONES_H */
