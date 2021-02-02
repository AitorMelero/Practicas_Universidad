/**************************************************************
 * File: TDEA.c
 * Author: Aitor Melero, Ana Roa
 * Date: 16/11/2020
 * Last_Date: 21/11/2020
 * Function: Implementacion de funcionalidad del triple DES. 
 * ***********************************************************/

#include "TDEA.h"

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
unsigned char* cifrar_TDEA_CBC(unsigned char* clave, unsigned char* iv, unsigned char* texto_plano, int tam_i) {
    unsigned char** clave_aux = NULL;
    unsigned char** texto_cifrado = NULL;
    unsigned char* cifrado = NULL;
    int i;
    int paridad = 1;

    /* Control de errores */
    if (!clave || !iv || !texto_plano || tam_i<1) {
        printf("ERROR al cifrar con triple DES !!!\n");
        return NULL;
    }

    /* Reservamos recursos */
    clave_aux = (unsigned char**)calloc(3, sizeof(unsigned char*));
    texto_cifrado = (unsigned char**)calloc(2, sizeof(unsigned char*));
    for (i=0; i<3; i++) {
        clave_aux[i] = (unsigned char*)calloc(8, sizeof(unsigned char));
    }

    /* Ciframos con triple DES-CBC */
    for (i=0; i<3 && paridad==1; i++) {
        copiar_cadena(clave_aux[i], &clave[i*8], 8);

        /* Comprobamos la ronda */
        if (i==0) {
            texto_cifrado[i] = cifrar_CBC(clave_aux[i], iv, texto_plano, tam_i);
        } else if (i==1) {
            texto_cifrado[i] = descifrar_CBC(clave_aux[i], iv, texto_cifrado[i-1], tam_i);
        } else {
            cifrado = cifrar_CBC(clave_aux[i], iv, texto_cifrado[i-1], tam_i);
        }

        if (i != 2) {
            if (texto_cifrado[i] == NULL) {
                paridad = 0;
                free(texto_cifrado);
                for (i=0; i<3; i++) {
                    free(clave_aux[i]);
                }
                free(clave_aux);

                return NULL;
            }
        }
        
    }

    /* Liberamos recursos */
    for (i=0; i<3; i++) {
        free(clave_aux[i]);
        if (i!=2) {
            free(texto_cifrado[i]);
        }
    }
    free(clave_aux);
    free(texto_cifrado);


    return cifrado;
}

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
unsigned char* descifrar_TDEA_CBC(unsigned char* clave, unsigned char* iv, unsigned char* texto_cifrado, int tam_i) {
    unsigned char** clave_aux = NULL;
    unsigned char** texto_plano = NULL;
    unsigned char* plano = NULL;
    int i;
    int paridad = 1;

    /* Control de errores */
    if (!clave || !iv || !texto_cifrado || tam_i<1) {
        printf("ERROR al descifrar con triple DES !!!\n");
        return NULL;
    }

    /* Reservamos recursos */
    clave_aux = (unsigned char**)calloc(3, sizeof(unsigned char*));
    texto_plano = (unsigned char**)calloc(2, sizeof(unsigned char*));
    for (i=0; i<3; i++) {
        clave_aux[i] = (unsigned char*)calloc(8, sizeof(unsigned char));
    }

    /* Desciframos con triple DES-CBC */
    for (i=0; i<3 && paridad==1; i++) {
        copiar_cadena(clave_aux[i], &clave[(8*2)-(i*8)], 8);

        /* Comprobamos la ronda */
        if (i==0) {
            texto_plano[i] = descifrar_CBC(clave_aux[i], iv, texto_cifrado, tam_i);
        } else if (i==1) {
            texto_plano[i] = cifrar_CBC(clave_aux[i], iv, texto_plano[i-1], tam_i);
        } else {
            plano = descifrar_CBC(clave_aux[i], iv, texto_plano[i-1], tam_i);
        }

        if (i != 2) {
            if (texto_plano[i] == NULL) {
                paridad = 0;
                free(texto_plano);
                for (i=0; i<3; i++) {
                    free(clave_aux[i]);
                }
                free(clave_aux);

                return NULL;
            }
        }
 
    }

    /* Liberamos recursos */
    for (i=0; i<3; i++) {
        free(clave_aux[i]);
        if (i!=2) {
            free(texto_plano[i]);
        }
    }
    free(clave_aux);
    free(texto_plano);


    return plano;
}
