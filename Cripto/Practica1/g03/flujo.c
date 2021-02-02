/**************************************************************
 * File: flujo.c
 * Author: Aitor Melero, Ana Roa
 * Date: 21/10/2020
 * Last_Date: 21/10/2020
 * Function: Definicion de funcionalidad del algoritmo flujo.
 * ***********************************************************/


/* INCLUDES */
#include "flujo.h"
#include "euclides.h"


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_flujo(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o)
 * Function: Crear un elemento de tipo AFIN.
 * Parameters:
 *              mpz_t m: Tamanio del espacio de texto cifrado.
 *              mpz_t a: Coeficiente multiplicativo de la funcion afin.
 *              mpz_t b: Termino constante de la funcion afin.
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              FLUJO: Elemento FLUJO creado.
 * ***********************************************************/
FLUJO* create_flujo(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o) {
    FLUJO* flujo = NULL;

    flujo = (FLUJO*)calloc(1, sizeof(FLUJO));
    if (!flujo) {
        printf("Error creando objeto FLUJO\n");
        return NULL;
    }

    /* Inicializamos los elementos de GMP */
    mpz_init(flujo->m);
    mpz_init(flujo->a);
    mpz_init(flujo->b);

    /* Inicializamos los elementos de flujo */
    mpz_set(flujo->m, m);
    mpz_set(flujo->a, a);
    mpz_set(flujo->b, b);
    flujo->i = i;
    flujo->o = o;

    
    return flujo;
}

/**************************************************************
 * Name: free_flujo(FLUJO* flujo)
 * Function: Liberar un elemento de tipo FLUJO.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_flujo(FLUJO* flujo) {
    if(flujo) {
        mpz_clear(flujo->m);
        mpz_clear(flujo->a);
        mpz_clear(flujo->b);
        if (flujo->i) {
            fclose(flujo->i);
        }
        if (flujo->o) {
            fclose(flujo->o);
        }
        free(flujo);
    }
}

/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(FLUJO* flujo)
 * Function: Devuelve el fichero de entrada usado en flujo.
 * Parameters:
 *              FLUJO* afin: Elemento de tipo flujo.
 * Return:
 *              FILE*: Fichero de entrada usado en flujo.
 * ***********************************************************/
FILE* get_input_file(FLUJO* flujo) {
    if(!flujo) {
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    if (!flujo->i) {
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    
    return flujo->i;
}

/**************************************************************
 * Name: get_output_file(FLUJO* flujo)
 * Function: Devuelve el fichero de salida usado en flujo.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              FILE*: Fichero de salida usado en flujo.
 * ***********************************************************/
FILE* get_output_file(FLUJO* flujo) {
    if(!flujo) {
        printf("Error al devolver el fichero de salida\n");
        return NULL;
    }
    if (!flujo->o) {
        printf("Error al devolver el fichero de salida\n");
        return NULL;
    }
    return flujo->o;
}

/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: cifrar(FLUJO* flujo)
 * Function: Cifra un fichero usando cifrado flujo.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS cifrar(FLUJO* flujo) {
    EUCLIDES* euclides = NULL;
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    int claves[1000];
    int i;
    int tam_fichero = 0;

    mpz_init(valor_aux);
    
    /* Crear elemento euclides con a y m */
    euclides = create_euclides(flujo->m, flujo->a);
    if(!euclides){
        printf("Error creando el objeto Euclides en cifrar\n");
        mpz_clear(valor_aux);
        return ERROR;
    }

    /* Comprobamos si la funcion tiene inversa */
    if (mpz_cmp_si(euclides->mcd, 1) != 0) {
        printf("No se puede calcular el inverso, el mcd es distinto de 1.\n");
        free_euclides(euclides);
        mpz_clear(valor_aux);
        return ERROR;
    }

    /* Crear conjunto de claves*/

    /*Calculamos el tamaño del fichero para saber la longuitud del conjunto de claves*/
    fseek(flujo->i, 0L, SEEK_END);
    tam_fichero = (ftell(flujo->i)-1);
    
    claves[0] = 3;
    for(i=1; i<=tam_fichero; i++){
        /*claves[i] = a*claves[i-1] +b mod m*/
        mpz_mul_si(valor_aux, flujo->a, (long int) claves[i-1]);
        mpz_add(valor_aux, valor_aux, flujo->b);
        mpz_mod(valor_aux, valor_aux, flujo->m);
        gmp_sprintf(valor_aux2, "%Zd", valor_aux);
        claves[i] = atoi(valor_aux2);
    }
    /*Devolvemos el puntero de lectura del fichero al inicio del fichero*/
    fseek(flujo->i, 0L, SEEK_SET);

    /*Leemos el fichero*/
    i=0;
    while (fread(&caracter_cifrado, 1, 1, flujo->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            /*x + k*/
            mpz_add_ui(valor_aux, valor_aux, claves[i]);
            /* x + k mod m*/
            mpz_mod(valor_aux, valor_aux, flujo->m);
            /* y  = x + k*/
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            caracter_claro = atoi(valor_aux2) + 65;
            i++;
            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 1, flujo->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, flujo->o);
        }
        offset_escritura++;
        fseek(flujo->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(flujo->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    free_euclides(euclides);

    return OK;

}

/*************************************************************/

/**************************************************************
 * Name: descifrar(FLUJO* flujo)
 * Function: Descifra un fichero usando cifrado flujo.
 * Parameters:
 *              FLUJO* flujo: Elemento de tipo flujo.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS descifrar(FLUJO* flujo){

    EUCLIDES* euclides = NULL;
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    int claves[1000];
    int i;
    int tam_fichero = 0;

    mpz_init(valor_aux);
    
    /* Crear elemento euclides con a y m */
    euclides = create_euclides(flujo->m, flujo->a);
    if(!euclides){
        printf("Error creando el objeto Euclides en cifrar\n");
        mpz_clear(valor_aux);
        return ERROR;
    }

    /* Comprobamos si la funcion tiene inversa */
    if (mpz_cmp_si(euclides->mcd, 1) != 0) {
        printf("No se puede calcular el inverso, el mcd es distinto de 1.\n");
        free_euclides(euclides);
        mpz_clear(valor_aux);
        return ERROR;
    }

    /* Crear conjunto de claves*/

    /*Calculamos el tamaño del fichero para saber la longuitud del conjunto de claves*/
    fseek(flujo->i, 0L, SEEK_END);
    tam_fichero = (ftell(flujo->i)-1);
    
    claves[0] = 3;
    for(i=1; i<=tam_fichero; i++){
        /*claves[i] = a*claves[i-1] +b mod m*/
        mpz_mul_si(valor_aux, flujo->a, (long int) claves[i-1]);
        mpz_add(valor_aux, valor_aux, flujo->b);
        mpz_mod(valor_aux, valor_aux, flujo->m);
        gmp_sprintf(valor_aux2, "%Zd", valor_aux);
        claves[i] = atoi(valor_aux2);
    }
    /*Devolvemos el puntero de lectura del fichero al inicio del fichero*/
    fseek(flujo->i, 0L, SEEK_SET);

    /*Leemos el fichero*/
    i=0;
    while (fread(&caracter_cifrado, 1, 1, flujo->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            /*x + k*/
            mpz_sub_ui(valor_aux, valor_aux, claves[i]);
            /* x + k mod m*/
            mpz_mod(valor_aux, valor_aux, flujo->m);
            /* y  = x + k*/
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            caracter_claro = atoi(valor_aux2) + 65;
            i++;
            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 1, flujo->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, flujo->o);
        }
        offset_escritura++;
        fseek(flujo->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(flujo->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    free_euclides(euclides);


    return OK;
}
