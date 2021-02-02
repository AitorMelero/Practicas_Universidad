/**************************************************************
 * File: afin.h
 * Author: Aitor Melero, Ana Roa
 * Date: 03/10/2020
 * Last_Date: 08/10/2020
 * Function: Definicion de funcionalidad del algoritmo afin.
 * ***********************************************************/


/* INCLUDES */
#include "afin.h"
#include "euclides.h"

/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_afin(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o)
 * Function: Crear un elemento de tipo AFIN.
 * Parameters:
 *              mpz_t m: Tamanio del espacio de texto cifrado.
 *              mpz_t a: Coeficiente multiplicativo de la funcion afin.
 *              mpz_t b: Termino constante de la funcion afin.
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              AFIN: Elemento AFIN creado.
 * ***********************************************************/
AFIN* create_afin(mpz_t m, mpz_t a, mpz_t b, FILE* i, FILE* o) {
    AFIN* afin = NULL;

    afin = (AFIN*)calloc(1, sizeof(AFIN));
    if (!afin) {
        printf("Error creando objeto AFIN\n");
        return NULL;
    }

    /* Inicializamos los elementos de GMP */
    mpz_init(afin->m);
    mpz_init(afin->a);
    mpz_init(afin->b);

    /* Inicializamos los elementos de afin */
    mpz_set(afin->m, m);
    mpz_set(afin->a, a);
    mpz_set(afin->b, b);
    afin->i = i;
    afin->o = o;
    /*strcpy(afin->i, i);
    strcpy(afin->o, o);*/

    
    return afin;
}


/*************************************************************/

/**************************************************************
 * Name: free_afin(AFIN* afin)
 * Function: Liberar un elemento de tipo AFIN.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_afin(AFIN* afin) {
    if(afin) {
        mpz_clear(afin->m);
        mpz_clear(afin->a);
        mpz_clear(afin->b);
        if (afin->i) {
            fclose(afin->i);
        }
        if (afin->o) {
            fclose(afin->o);
        }
        free(afin);
    }
}


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/**************************************************************
 * Name: get_input_file(AFIN* afin)
 * Function: Devuelve el fichero de entrada usado en afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              FILE*: Fichero de entrada usado en afin.
 * ***********************************************************/
FILE* get_input_file(AFIN* afin) {
    if(!afin) {
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    if (!afin->i) {
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    
    return afin->i;
}


/*************************************************************/

/**************************************************************
 * Name: get_output_file(AFIN* afin)
 * Function: Devuelve el fichero de salida usado en afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              FILE*: Fichero de salida usado en afin.
 * ***********************************************************/
FILE* get_output_file(AFIN* afin) {
    if(!afin) {
        printf("Error al devolver el fichero de salida\n");
        return NULL;
    }
    if (!afin->o) {
        printf("Error al devolver el fichero de salida\n");
        return NULL;
    }
    return afin->o;
}


/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: cifrar(AFIN* afin)
 * Function: Cifra un fichero usando cifrado afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS cifrar(AFIN* afin) {
    EUCLIDES* euclides = NULL;
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;

    mpz_init(valor_aux);
    
    /* Crear elemento euclides con a y m */
    euclides = create_euclides(afin->m, afin->a);
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
    /* Calcular el cifrado y = a*x + b mod m*/
    while (fread(&caracter_claro, 2, 1, afin->i) == 1) {
        if (caracter_claro >= 65 && caracter_claro <= 90) {
            /* a*x */
            mpz_mul_si(valor_aux, afin->a, (long int) caracter_claro - 65);
            /* (a*x) + b */
            mpz_add(valor_aux, valor_aux, afin->b);
            /* a*x + b mod m */
            mpz_mod(valor_aux, valor_aux, afin->m);
            /* y = a*x + b mod m */
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            caracter_cifrado = atoi(valor_aux2) + 65;
            /* vamos escribiendo caracter a caracter */
            /*fputs(&caracter_cifrado, afin->o);*/
            fwrite(&caracter_cifrado, 1, 1, afin->o);
        } else {
            fwrite(&caracter_claro, 1, 1, afin->o);
        }
        offset_escritura++;
        fseek(afin->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(afin->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    free_euclides(euclides);

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: descifrar(AFIN* afin)
 * Function: Descifra un fichero usando cifrado afin.
 * Parameters:
 *              AFIN* afin: Elemento de tipo afin.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS descifrar(AFIN* afin) {
    EUCLIDES* euclides = NULL;
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;

    mpz_init(valor_aux);
    
    /* Crear elemento euclides con a y m */
    euclides = create_euclides(afin->m, afin->a);
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

    /* Calcular el cifrado x = (y-b)*a⁻1 mod m */
    while (fread(&caracter_cifrado, 1, 1, afin->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            mpz_sub(valor_aux, valor_aux, afin->b);
            /* (y-b)*a⁻1 */
            mpz_mul(valor_aux, valor_aux, euclides->inv);
            /* (y-b)*a⁻1 mod m */
            mpz_mod(valor_aux, valor_aux, afin->m);
            /* x = (y-b)*a⁻1 mod m */
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            caracter_claro = atoi(valor_aux2) + 65;
            /* vamos escribiendo caracter a caracter */
            /*fputs(&caracter_cifrado, afin->o);*/
            fwrite(&caracter_claro, 1, 1, afin->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, afin->o);
        }
        offset_escritura++;
        fseek(afin->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(afin->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    free_euclides(euclides);

    return OK;
}