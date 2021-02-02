/**************************************************************
 * File: vigenere.c
 * Author: Aitor Melero, Ana Roa
 * Date: 08/10/2020
 * Last_Date: 08/10/2020
 * Function: Definicion de funcionalidad del algoritmo vigenere.
 * ***********************************************************/


/* INCLUDES */
#include "vigenere.h"

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_vigenere(mpz_t m, char* k, FILE* i, FILE* o)
 * Function: Crear un elemento de tipo VIGENERE.
 * Parameters:
 *              mpz_t m: Tamanio del espacio de texto cifrado.
 *              char* k: Clave para vigenere.
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              VIGENERE: Elemento VIGENERE creado.
 * ***********************************************************/
VIGENERE* create_vigenere(char* k, FILE* i, FILE* o) {
    VIGENERE* vigenere = NULL;

    vigenere = (VIGENERE*)calloc(1, sizeof(VIGENERE));
    if (!vigenere) {
        printf("Error creando objeto VIGENERE\n");
        return NULL;
    }

    /* Inicializamos los elementos de GMP */
    mpz_init(vigenere->m);

    /* Inicializamos los elementos de viegenere */
    mpz_set_si(vigenere->m, 26);
    vigenere->k = k;
    vigenere->i = i;
    vigenere->o = o;

    return vigenere;

}


/*************************************************************/

/**************************************************************
 * Name: free_vigenere(VIGENERE* vigenere)
 * Function: Liberar un elemento de tipo VIGENERE.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_vigenere(VIGENERE* vigenere) {
    if(vigenere) {
        mpz_clear(vigenere->m);
        if (vigenere->k) {
            free(vigenere->k);
            vigenere->k = NULL;
        }
        if(vigenere->i){
            fclose(vigenere->i);
            vigenere->i = NULL;
        }
        if(vigenere->o){
            fclose(vigenere->o);
            vigenere->o = NULL;
        }
        free(vigenere);
    }
}


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(VIGENERE* vigenere)
 * Function: Devuelve el fichero de entrada usado en vigenere.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              FILE*: Fichero de entrada usado en vigenere.
 * ***********************************************************/
FILE* get_input_file(VIGENERE* vigenere){
    if(!vigenere){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    if(!vigenere->i){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    return vigenere->i;
}


/*************************************************************/

/**************************************************************
 * Name: get_output_file(VIGENERE* vigenere)
 * Function: Devuelve el fichero de salida usado en vigenere.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              FILE*: Fichero de salida usado en vigenere.
 * ***********************************************************/
FILE* get_output_file(VIGENERE* vigenere){
        if(!vigenere){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    if(!vigenere->o){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    return vigenere->o;
}


/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: cifrar(VIGENERE* vigenere)
 * Function: Cifra un fichero usando cifrado viegenere.
 * Parameters:
 *              VIGENERE* viegenere: Elemento de tipo vigenere.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS cifrar(VIGENERE* vigenere){

    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    mpz_t valor_k;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    long int posicion_k = 0; 
    long int tamanio_k = strlen(vigenere->k);

    mpz_init(valor_aux);
    mpz_init(valor_k);

    if(!vigenere) {
        printf("Error en el argumento de entrada de cifrar\n");
        return ERROR;
    }

    while (fread(&caracter_cifrado, 1, 1, vigenere->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            mpz_set_si(valor_k, (long int) vigenere->k[posicion_k] - 65);
            /*x + k*/
            mpz_add(valor_aux, valor_aux, valor_k);
            /* x + k mod m*/
            mpz_mod(valor_aux, valor_aux, vigenere->m);
            if (posicion_k == (tamanio_k - 1)) {
                posicion_k = 0;
            } else {
                posicion_k++;
            }
            /* y  = x + k*/
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            caracter_claro = atoi(valor_aux2) + 65;
            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 1, vigenere->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, vigenere->o);
        }
        offset_escritura++;
        fseek(vigenere->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(vigenere->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    mpz_clear(valor_k);
    return OK;

}


/*************************************************************/

/**************************************************************
 * Name: descifrar(VIGENERE* vigenere)
 * Function: Descifra un fichero usando cifrado vigenere.
 * Parameters:
 *              VIGENERE* vigenere: Elemento de tipo vigenere.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS descifrar(VIGENERE* vigenere){
    
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    mpz_t valor_k;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    long int posicion_k = 0;
    long int tamanio_k = strlen(vigenere->k);

    mpz_init(valor_aux);
    mpz_init(valor_k);

    fseek(vigenere->i, offset_lectura, SEEK_SET);
     while (fread(&caracter_cifrado, 1, 1, vigenere->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            mpz_set_si(valor_k, (long int) vigenere->k[posicion_k] - 65);
            /*x - k*/
            mpz_sub(valor_aux, valor_aux, valor_k);
            if (posicion_k == (tamanio_k - 1)) {
                posicion_k = 0;
            } else {
                posicion_k++;
            }
            /* y  = x + k mod 26*/
            mpz_mod(valor_aux, valor_aux, vigenere->m);
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            caracter_claro = atoi(valor_aux2) + 65;
            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 1, vigenere->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, vigenere->o);
        }
        offset_escritura++;
        fseek(vigenere->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(vigenere->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    mpz_clear(valor_k);
    return OK;
}