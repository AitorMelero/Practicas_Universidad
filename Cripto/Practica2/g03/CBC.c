/**************************************************************
 * File: CBC.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 5/11/2020
 * Last_Date: 21/11/2020 
 * Function: Implementacion de funcionalidad del modo CBC. 
 * ***********************************************************/

#include "CBC.h"

/**************************************************************
 * Name: generar_LR_CBC(unsigned char* texto_plano)
 * Function: Genera el bloque de texto plano de 64 bits justos. 
 * Parameters:
 *             unsigned char* texto_plano: Bloque a cifrar.
 * Return:
 *             unsigned char*: Texto plano de 64 bits. 
 * ***********************************************************/
unsigned char* generar_LR_CBC(unsigned char* texto_plano) {
    unsigned char* lr = NULL;
    int i, j;

    /* Control de errores */
    if (!texto_plano) {
        printf("ERROR al generar bloque LR de 64 bits!!!\n");
        return NULL;
    }

    /* Reservamos memoria para el bloque lr */
    lr = (unsigned char*)calloc(8, sizeof(unsigned char));

    /* Vamos asignando los bits a la nueva varible y si faltan añadimos 0 */
    for (i=0, j=0; i<8; i++) {
        /* Comprobamos si hemos llegado al final del texto plano */
        if (texto_plano[j] == 0) {
            lr[i] = 0;
        } else {
            lr[i] = texto_plano[j];
            j++;
        }
    }

    return lr;
}

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
unsigned char* cifrar_CBC(unsigned char* clave, unsigned char* iv, unsigned char* i, int tam_i) {
    BOOL leer = TRUE;
    unsigned char* bloque_texto = NULL;
    unsigned char* bloque_cifrado = NULL;
    unsigned char* bloque_cifrado_aux = NULL;
    unsigned char* aux = NULL;
    int j = 0;
    int k = 0;

    /* Control de errores */
    if (!clave || !iv || !i || tam_i<1) {
        printf("ERROR al cifrar con DES con el modo CBC!!!\n");
        return NULL;
    }

    /* Vamos cifrando bloque a bloque */
    for (j=0; j<tam_i; j++) {
        /* Reservamos memoria para bloque_texto */
        bloque_texto = (unsigned char*)calloc(8, sizeof(unsigned char));
        /* Si leemos menos de 8 caracteres es que estamos en el ultimo bloque */
        for (k=0; k<8 && leer == TRUE; k++) {
            bloque_texto[k] = i[(j*8)+k];
            if (bloque_texto[k] == 0) {
                leer = FALSE;
            }

        }

        /* Aniadimos '\0' al bloque leido en caso de que este no tenga 64 bits */
        aux = generar_LR_CBC(bloque_texto);

        /* En la primera iteracion hacemos xor de iv con el primer bloque de texto plano */
        free(bloque_texto);
        if (j==0) {
            bloque_texto = xor(aux, iv, 8);
        } else {
            bloque_texto = xor(aux, &bloque_cifrado[j*8-8], 8);
        }

       /* Ciframos con DES el resultado del xor anterior con la clave */
        if (bloque_cifrado == NULL){
            bloque_cifrado = (unsigned char*)calloc(8, sizeof(unsigned char));
        } else {
            bloque_cifrado = (unsigned char*)realloc(bloque_cifrado, (j+1)*8*sizeof(unsigned char));
        }

        bloque_cifrado_aux = cifrar_DES(bloque_texto, clave);
        if (bloque_cifrado_aux == NULL) {
            /* No se cumple la paridad */
            /* Liberamos los recursos que se creen mas tarde */
            free(bloque_texto);
            free(aux);
            free(bloque_cifrado_aux);

            return NULL;
        }

        copiar_cadena(&bloque_cifrado[j*8], bloque_cifrado_aux, 8);

        /* Liberamos los recursos que se creen mas tarde */
        free(bloque_texto);
        free(aux);
        free(bloque_cifrado_aux);
    }

    return bloque_cifrado;

}

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
unsigned char* descifrar_CBC(unsigned char* clave, unsigned char* iv, unsigned char* i, int tam_i) {
    unsigned char* bloque_texto = NULL;
    unsigned char* bloque_cifrado = NULL;
    unsigned char* bloque_texto_aux = NULL;
    unsigned char* aux = NULL;
    int j = 0;

    /* Control de errores */
    if (!clave || !iv || !i || tam_i<1) {
        printf("ERROR al descifrar con DES con el modo CBC!!!\n");
        return NULL;
    }

    /* Desciframos bloque a bloque */
    for (j=0; j<tam_i; j++) {
        /* Ciframos con DES el bloque de texto cifrado leido */
        if (bloque_cifrado == NULL){
            bloque_cifrado = (unsigned char*)calloc(8, sizeof(unsigned char));
            bloque_texto_aux = (unsigned char*)calloc(8, sizeof(unsigned char));
        } else {
            bloque_cifrado = (unsigned char*)realloc(bloque_cifrado, (j+1)*8*sizeof(unsigned char));
            bloque_texto_aux = (unsigned char*)realloc(bloque_texto_aux, (j+1)*8*sizeof(unsigned char));
        }

        copiar_cadena(&bloque_cifrado[j*8], &i[j*8], 8);

        /* Ciframos con DES */
        aux = descifrar_DES(&bloque_cifrado[j*8], clave);

        /* Comprobamos si se cumple la paridad */
        if (aux == NULL) {
            free(bloque_cifrado);
            free(bloque_texto_aux);
            return NULL;
        }

        /* En la primera iteracion hacemos xor de iv con el primer bloque de texto cifrado */
        if (j==0) {
            bloque_texto = xor(aux, iv, 8);
        } else {
            bloque_texto = xor(aux, &bloque_cifrado[(j-1)*8], 8);
        }

        /* Asignamos el bloque de texto plano */
        copiar_cadena(&bloque_texto_aux[j*8], bloque_texto, 8);

        /* Liberamos los recursos que se creen mas tarde */
        free(bloque_texto);
        free(aux);
    }

    /* Liberamos recursos */
    free(bloque_cifrado);

    return bloque_texto_aux;
    
}
