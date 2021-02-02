/**************************************************************
 * File: kasiski.c
 * Author: Aitor Melero, Ana Roa
 * Date: 17/10/2020
 * Last_Date: 17/10/2020
 * Function: Implementacion de funcionalidad de Kasiski.
 * ***********************************************************/


/* INCLUDES */
#include "kasiski.h"


/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_kasiski(FILE* i, int l)
 * Function: Crear un elemento de tipo KASISKI.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              int l: Longitud de subcadenas repetidas
 * Return:
 *              KASISKI: Elemento KASISKI creado.
 * ***********************************************************/
KASISKI* create_kasiski(FILE* i, int l) {
    KASISKI* kasiski = NULL;
    STATUS estado = TRUE;

    /* Reservamos memoria para kasiski */
    kasiski = (KASISKI*)calloc(1, sizeof(KASISKI));
    if (!kasiski) {
        printf("ERROR al reservar kasiski!!!\n");
        return NULL;
    }

    kasiski->i = i;
    kasiski->texto_cifrado = NULL;
    /* Generamos el texto cifrado sin espacios */
    estado = generador_texto_cifrado_kasiski(kasiski);

    kasiski->distancias = NULL;
    /* Generamos la lista de distancias */
    if (estado == OK) {
        kasiski->l = l;
        kasiski->num_distancias = 0;
        estado = generador_distancias_kasiski(kasiski);
    }

    /* Hallamos la posible longitud de clave */
    if (estado == OK) {
        kasiski->mcd = -1;
        estado = generador_mcd_kasiski(kasiski);
    }

    /* Devolvemos a kasiski */
    if (estado == OK) {
        return kasiski;
    } else {
        printf("ERROR al inicializar kasiski!!!\n");
        free_kasiski(kasiski);
        return NULL;
    }
}


/*************************************************************/

/**************************************************************
 * Name: free_kasiski(KASISKI* kasiski)
 * Function: Liberar un elemento de tipo KASISKI.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_kasiski(KASISKI* kasiski) {
    if (kasiski) {
        if (kasiski->texto_cifrado) {
            free(kasiski->texto_cifrado);
            kasiski->texto_cifrado = NULL;
        }
        if (kasiski->distancias) {
            free(kasiski->distancias);
            kasiski->distancias = NULL;
        }
        free(kasiski);
        kasiski = NULL;
    }
}


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generador_texto_cifrado_kasiski(KASISKI* kasiski)
 * Function: Lee del fichero cifrado y escribe dicho texto sin espacios.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_cifrado_kasiski(KASISKI* kasiski) {
    char caracter_cifrado;
    int caracteres_leido = 0;
    int offset_lectura = 0;

    /* Control de errores */
    if (!kasiski || !kasiski->i) {
        printf("ERROR al generar texto cifrado con kasiski!!!\n");
        return ERROR;
    }

    /* Vamos leyendo caracter a caracter del fichero de la entrada y lo guardamos */
    fseek(kasiski->i, offset_lectura, SEEK_SET);
    while (fread(&caracter_cifrado, 1, 1, kasiski->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            if (kasiski->texto_cifrado) {
                caracteres_leido++;
                kasiski->texto_cifrado = (char*)realloc(kasiski->texto_cifrado, sizeof(char)*caracteres_leido);
                kasiski->texto_cifrado[caracteres_leido-1] = caracter_cifrado;
            } else {
                caracteres_leido++;
                kasiski->texto_cifrado = (char*)calloc(1, sizeof(char));
                kasiski->texto_cifrado[caracteres_leido-1] = caracter_cifrado;
            }
        }
        offset_lectura++;
        fseek(kasiski->i, offset_lectura, SEEK_SET);
    }

    /* Guardamos el final de cadena */
    kasiski->texto_cifrado = (char*)realloc(kasiski->texto_cifrado, sizeof(char)*caracteres_leido+1);
    kasiski->texto_cifrado[caracteres_leido] = '\0';

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_distancias_kasiski(KASISKI* kasiski)
 * Function: Genera las distancias de subcadenas repetidas.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_distancias_kasiski(KASISKI* kasiski) {
    int evaluacion = 0;
    int ini = 0;
    char** cadenas_comprobadas = NULL;
    int num_cadenas_comprobadas = 0;
    int num_cadenas_comprobadas_repetidas = 0;
    BOOL cadena_repetida = FALSE;
    BOOL repetida = FALSE;
    char* cadena1;
    char* cadena2;
    int i = 0;
    int j = 0;

    /* Control de errores */
    if (kasiski == NULL || kasiski->texto_cifrado == NULL) {
        printf("ERROR al generar distancias en kasiski!!!\n");
        return ERROR;
    } else {
        cadena1 = (char*)calloc(kasiski->l+1, sizeof(char));
        cadena2 = (char*)calloc(kasiski->l+1, sizeof(char));
    }

    /* Evaluamos las cadenas que se repiten y las distancias que hay entre ellas */
    while (((evaluacion+kasiski->l) < strlen(kasiski->texto_cifrado)) && (num_cadenas_comprobadas_repetidas < 3)) {
        repetida = FALSE;       /* varible de control */
        /* Asignamos la cadena a evaluar en una variable auxiliar */
        for (i=evaluacion, j=0; i<(evaluacion+kasiski->l); i++, j++) {
            cadena1[j] = kasiski->texto_cifrado[i];
        }
        cadena1[j] = '\0';
        ini = evaluacion + kasiski->l;
        /* Creamos si no se ha creado la variable de las cadenas evaluadas */
        if (!cadenas_comprobadas) {
            cadenas_comprobadas = (char**)calloc(1, sizeof(char*));
        }
        /* Comprobamos si esa cadena ya se ha evaluado */
        if (cadenas_comprobadas) {
            cadena_repetida = FALSE;
            for (i=0; (i<num_cadenas_comprobadas) && (cadena_repetida==FALSE); i++) {
                /* La subcadena ya se ha evaluado */
                if (strcmp(cadena1, cadenas_comprobadas[i]) == 0) {
                    cadena_repetida = TRUE;
                }
            }
            /* La cadena no esta repetida asi que la metemos en la lista para no repetirla mas tarde */
            if (cadena_repetida == FALSE) {
                if (num_cadenas_comprobadas == 0) {
                    num_cadenas_comprobadas++;
                    cadenas_comprobadas[num_cadenas_comprobadas-1] = (char*)calloc(kasiski->l+1, sizeof(char));
                    strcpy(cadenas_comprobadas[num_cadenas_comprobadas-1], cadena1);
                } else {
                    num_cadenas_comprobadas++;
                    cadenas_comprobadas = (char**)realloc(cadenas_comprobadas, sizeof(char*)*num_cadenas_comprobadas);
                    cadenas_comprobadas[num_cadenas_comprobadas-1] = (char*)calloc(kasiski->l+1, sizeof(char));
                    strcpy(cadenas_comprobadas[num_cadenas_comprobadas-1], cadena1);
                }
                /* Evaluamos la cadena */
                while ((ini+kasiski->l) < strlen(kasiski->texto_cifrado)) {
                    /* Asignamos la cadena 2 a evaluar en una variable auxiliar */
                    for (i=ini, j=0; i<(ini+kasiski->l); i++, j++) {
                        cadena2[j] = kasiski->texto_cifrado[i];
                    }
                    cadena2[j] = '\0';
                    /* Si las cadenas se repiten, se calculan las distancias */
                    if (strcmp(cadena1, cadena2) == 0) {
                        repetida = TRUE;    /* variable de control */
                        if (kasiski->num_distancias == 0) {
                            kasiski->distancias = (int*)calloc(1, sizeof(int));
                        } else {
                            kasiski->distancias = (int*)realloc(kasiski->distancias, sizeof(int)*(kasiski->num_distancias+1));
                        }
                        kasiski->distancias[kasiski->num_distancias] = ini - evaluacion;
                        kasiski->num_distancias++;
                    }
                    ini++;
                }
                /* Para evaluar el determinado numero de cadenas */
                if (repetida == TRUE) {
                    num_cadenas_comprobadas_repetidas++;
                }
            }
        }
        evaluacion++;
    }

    /* Liberamos recursos */
    if (cadenas_comprobadas) {
        for (i=0; i<num_cadenas_comprobadas; i++) {
            if (cadenas_comprobadas[i]) {
                free(cadenas_comprobadas[i]);
            }
        }
        free(cadenas_comprobadas);
    }
    if (cadena1) {
        free(cadena1);
    }
    if (cadena2) {
        free(cadena2);
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_mcd_kasiski(KASISKI* kasiski)
 * Function: Guarda la posible longitud de clave.
 * Parameters:
 *              KASISKI* kasiski: Elemento de tipo kasiski.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_mcd_kasiski(KASISKI* kasiski) {
    char cadena_gmp[1024];          /* cadena para sustituir de tipo gmp a tipo int el mcd */
    int cont_distancias = 0;
    EUCLIDES* euclides = NULL;
    mpz_t m, a;

    /* Control de errores */
    if (!kasiski || !kasiski->texto_cifrado) {
        printf("ERROR a la hora de generar la longitud de clave con kasiski!!!\n");
        return ERROR;
    }

    /* La longitud de clave no se ha encontrado */
    if (kasiski->num_distancias == 0) {
        printf("No se puede hallar la longitud de clave con Kasiski.\n");
        return ERROR;
    }

    /* Si solo hay una distancia o ninguna */
    if (kasiski->num_distancias == 1) {
        kasiski->mcd = kasiski->distancias[0];
    } else if (kasiski->num_distancias == 0) {
        kasiski->mcd = -1;
    } else {
        /* Usamos la estructura EUCLIDES para calcular el mcd */
        for (cont_distancias=0; (cont_distancias+1) < kasiski->num_distancias; cont_distancias++) {
            /* Inicializamos las variables auxiliares necesarias */
            mpz_init(m);
            mpz_init(a);
            /* Hallamos el mcd */
            if (cont_distancias == 0) {
                if (kasiski->distancias[cont_distancias] > kasiski->distancias[cont_distancias+1]) {
                    mpz_set_si(m, kasiski->distancias[cont_distancias]);
                    mpz_set_si(a, kasiski->distancias[cont_distancias+1]);
                    euclides = create_euclides(m, a);
                    gmp_sprintf(cadena_gmp, "%Zd", euclides->mcd);
                    kasiski->mcd = atoi(cadena_gmp);
                    free_euclides(euclides);
                } else {
                    mpz_set_si(m, kasiski->distancias[cont_distancias+1]);
                    mpz_set_si(a, kasiski->distancias[cont_distancias]);
                    euclides = create_euclides(m, a);
                    gmp_sprintf(cadena_gmp, "%Zd", euclides->mcd);
                    kasiski->mcd = atoi(cadena_gmp);
                    free_euclides(euclides);
                }
            } else {
                if (kasiski->distancias[cont_distancias] > kasiski->mcd) {
                    mpz_set_si(m, kasiski->distancias[cont_distancias]);
                    mpz_set_si(a, kasiski->mcd);
                    euclides = create_euclides(m, a);
                    gmp_sprintf(cadena_gmp, "%Zd", euclides->mcd);
                    kasiski->mcd = atoi(cadena_gmp);
                    free_euclides(euclides);
                } else {
                    mpz_set_si(m, kasiski->mcd);
                    mpz_set_si(a, kasiski->distancias[cont_distancias]);
                    euclides = create_euclides(m, a);
                    gmp_sprintf(cadena_gmp, "%Zd", euclides->mcd);
                    kasiski->mcd = atoi(cadena_gmp);
                    free_euclides(euclides);
                }
            }
            /* Liberamos los objetos gmp */
            mpz_clear(m);
            mpz_clear(a);
        }
    }

    return OK;
}