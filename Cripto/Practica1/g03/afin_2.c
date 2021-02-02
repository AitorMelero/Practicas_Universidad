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
    mpz_set_ui(afin->m, 676);
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
    char caracter_claro[2];
    char caracter_cifrado[2];
    mpz_t valor_aux;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    
    int alfabeto [27][27];
    int i,j,k=0;
    for (i=0; i<26;i++){
        for(j=0; j<26;j++){
            alfabeto[i][j] = k;
            k=k+1;
        }
    }
    alfabeto[i+1][j+1]='\0';

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
    while (fread(&caracter_claro, 1, 2, afin->i) == 2) {
        if ((caracter_claro[1] >= 65 && caracter_claro[1] <= 90) 
        && (caracter_claro[0] >= 65 && caracter_claro[0] <= 90)) {
            /*a*x*/
            mpz_mul_si(valor_aux, afin->a, (long int) alfabeto[caracter_claro[0] - 65 ][caracter_claro[1] - 65]);
            /* (a*x) + b */
            mpz_add(valor_aux, valor_aux, afin->b);
            /* a*x + b mod m */
            mpz_mod(valor_aux, valor_aux, afin->m);
            /* y = a*x + b mod m */
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            /*buscamos el numero en el alfabeto*/
            for (i=0; i<26;i++){
                for(j=0; j<26;j++){
                    if(alfabeto[i][j] == atoi(valor_aux2)){
                        caracter_cifrado[0] = i+65;
                        caracter_cifrado[1] = j+65;
                    }
                }
            }
            fwrite(&caracter_cifrado, 1, 2, afin->o);
        } else {
            fwrite(&caracter_claro, 1, 2, afin->o);
        }
        offset_escritura = offset_escritura + 2;
        fseek(afin->o, offset_escritura, SEEK_SET);
        offset_lectura = offset_lectura + 2;
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
    char caracter_claro[2];
    char caracter_cifrado[2];
    mpz_t valor_aux;
    char valor_aux2[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;

    int alfabeto [27][27];
    int i,j,k=0;
    for (i=0; i<26;i++){
        for(j=0; j<26;j++){
            alfabeto[i][j] = k;
            k=k+1;
        }
    }
    alfabeto[i+1][j+1]='\0';

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
    while (fread(&caracter_cifrado, 1, 2, afin->i) == 2) {

        if ((caracter_cifrado[1] >= 65 && caracter_cifrado[1] <= 90)
        && (caracter_cifrado[0] >= 65 && caracter_cifrado[0] <= 90)) {
            
            mpz_sub(valor_aux, valor_aux, valor_aux);
            mpz_add_ui(valor_aux, valor_aux, alfabeto[caracter_cifrado[0]-65][caracter_cifrado[1]-65]);
            /* y-b */
            mpz_sub(valor_aux, valor_aux, afin->b);
            /* (y-b)*a⁻1 */
            mpz_mul(valor_aux, valor_aux, euclides->inv);
            /* (y-b)*a⁻1 mod m */
            mpz_mod(valor_aux, valor_aux, afin->m);
            /* x = (y-b)*a⁻1 mod m */
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            for (i=0; i<26;i++){
                for(j=0; j<26;j++){
                    if(alfabeto[i][j] == atoi(valor_aux2)){
                        caracter_claro[0] = i+65;
                        caracter_claro[1] = j+65;
                    }
                }
            }
            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 2, afin->o);
        } else {
            fwrite(&caracter_cifrado, 1, 2, afin->o);
        }
        offset_escritura = offset_escritura + 2;
        fseek(afin->o, offset_escritura, SEEK_SET);
        offset_lectura = offset_lectura + 2;
        fseek(afin->i, offset_lectura, SEEK_SET);
    }

    mpz_clear(valor_aux);
    free_euclides(euclides);

    return OK;
}