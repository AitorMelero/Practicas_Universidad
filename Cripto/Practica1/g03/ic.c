/**************************************************************
 * File: ic.c
 * Author: Aitor Melero, Ana Roa
 * Date: 18/10/2020
 * Last_Date: 18/10/2020
 * Function: Implementacion de funcionalidad del indice de coincidencia.
 * ***********************************************************/


/* INCLUDES */
#include "ic.h"


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_ic(FILE* i, int l)
 * Function: Crear un elemento de tipo IC.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              int l: Longitud de n-gramas
 * Return:
 *              IC: Elemento IC creado.
 * ***********************************************************/
IC* create_ic(FILE* i, int l) {
    IC* ic = NULL;
    STATUS estado = FALSE;

    /* Reservamos memoria para ic */
    ic = (IC*)calloc(1, sizeof(IC));
    if (!ic) {
        printf("ERROR al reservar memoria para ic!!!\n");
        return NULL;
    }

    /* Generamos el texto cifrado sin espacios */
    ic->i = i;
    ic->texto_cifrado = NULL;
    estado = generador_texto_cifrado_ic(ic);

    /* Generamos la tabla de los n-gramas */
    if (estado == OK) {
        ic->n_gramas = NULL;
        ic->l = l;
        estado = generador_ngramas_ic(ic);
    }

    /* Hallamos la posible longitud de clave */
    if (estado == OK) {
        ic->long_clave = -1;
        ic->resultado = -1;
        estado = generador_long_clave_ic(ic);
    }

    /* Devolvemos ic o NULL en caso de error */
    if (estado == ERROR) {
        printf("ERROR al inicializar ic!!!\n");
        return NULL;
    } else {
        return ic;
    }

}


/*************************************************************/

/**************************************************************
 * Name: free_ic(IC* ic)
 * Function: Liberar un elemento de tipo IC.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_ic(IC* ic) {
    int i;

    if (ic) {
        if (ic->texto_cifrado) {
            free(ic->texto_cifrado);
        }
        if (ic->n_gramas) {
            for (i=0; i<ic->l; i++) {
                if (ic->n_gramas[i]) {
                    free(ic->n_gramas[i]);
                }
            }
            free(ic->n_gramas);
        }
        free(ic);
    }
}


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generador_texto_cifrado_ic(IC* ic)
 * Function: Lee del fichero cifrado y escribe dicho texto sin espacios.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_cifrado_ic(IC* ic) {
    char caracter_cifrado;
    int caracteres_leido = 0;
    int offset_lectura = 0;

    /* Control de errores */
    if (!ic || !ic->i) {
        printf("ERROR al generar texto cifrado con ic!!!\n");
        return ERROR;
    }

    /* Vamos leyendo caracter a caracter del fichero de la entrada y lo guardamos */
    fseek(ic->i, offset_lectura, SEEK_SET);
    while (fread(&caracter_cifrado, 1, 1, ic->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            if (ic->texto_cifrado) {
                caracteres_leido++;
                ic->texto_cifrado = (char*)realloc(ic->texto_cifrado, sizeof(char)*caracteres_leido);
                ic->texto_cifrado[caracteres_leido-1] = caracter_cifrado;
            } else {
                caracteres_leido++;
                ic->texto_cifrado = (char*)calloc(1, sizeof(char));
                ic->texto_cifrado[caracteres_leido-1] = caracter_cifrado;
            }
        }
        offset_lectura++;
        fseek(ic->i, offset_lectura, SEEK_SET);
    }

    /* Guardamos el final de cadena */
    ic->texto_cifrado = (char*)realloc(ic->texto_cifrado, sizeof(char)*caracteres_leido+1);
    ic->texto_cifrado[caracteres_leido] = '\0';

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_ngramas_ic(IC* ic)
 * Function: Genera la tabla de n-gramas.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_ngramas_ic(IC* ic) {
    int i, j, cont_texto;

    /* Control de errores */
    if (!ic) {
        printf("ERROR al generar n-gramas de ic!!!\n");
        return ERROR;
    }

    /* Reservamos memoria para las filas de n-gramas */
    if (!ic->n_gramas) {
        ic->n_gramas = (char**)calloc(ic->l, sizeof(char*));
        for (i=0; i<(ic->l); i++) {
            if (!ic->n_gramas[i]) {
                ic->n_gramas[i] = (char*)calloc(1, sizeof(char));
                ic->n_gramas[i][0] = '\0';
            }
        }
    }

    /* Vamos guardando caracter a caracter en la tabla de n-gramas */
    cont_texto = 0;
    j = 0;
    while (cont_texto < strlen(ic->texto_cifrado)) {
        for (i=0; (i<ic->l) && (cont_texto < strlen(ic->texto_cifrado)); i++) {
            ic->n_gramas[i][j] = ic->texto_cifrado[cont_texto];
            ic->n_gramas[i] = (char*)realloc(ic->n_gramas[i], sizeof(char)*(j+2));
            ic->n_gramas[i][j+1] = '\0';
            cont_texto++;
        }
        j++;
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_long_clave_ic(IC* ic)
 * Function: Guarda la posible longitud de clave.
 * Parameters:
 *              IC* ic: Elemento de tipo ic.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_long_clave_ic(IC* ic) {
    int i;
    double cont_ic;

    /* Control de errores */
    if (!ic) {
        printf("ERROR al generar la longitud de clave en ic!!!\n");
        return ERROR;
    }

    /* Vamos calculando el sumatorio de la formula para hallar el ic */
    cont_ic = 0;
    for (i=0; i<ic->l; i++) {
        cont_ic += calcular_ic(ic->n_gramas[i]);
    }

    /* Hallamos la media de ICs */
    ic->resultado = cont_ic / ic->l;

    /* Suponemos que l es la longitud de clave */
    ic->long_clave = ic->l;

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: calcular_ic(char* cadena)
 * Function: Calcula el indice de coincidencia.
 * Parameters:
 *              char* cadena: Cadena a calcular el ic.
 * Return:
 *              double: ic de la cadena pasada por argumento.
 * ***********************************************************/
double calcular_ic(char* cadena) {
    int i, fi, long_texto;
    char letra;
    double sumatorio, ic;

    /* Inicializamos las variables */
    fi = 0;
    sumatorio = 0.0;
    long_texto = strlen(cadena);
    ic = 0.0;

    /* Parte del sumatorio de la formula del ic */
    for (i=0; i<26; i++) {
        letra = 65+i;
        fi = calcula_fi(letra, cadena);
        sumatorio += fi*(fi-1);
    }

    /* Parte de la division de la formula del ic */
    ic = sumatorio / (double) (long_texto*(long_texto-1));

    return ic;
}


/*************************************************************/

/**************************************************************
 * Name: calcula_fi(char letra, char* cadena)
 * Function: Calcula la frecuencia de una letra en una cadena.
 * Parameters:
 *              char letra: Letra a calcular frecuencia.
 *              char* cadena: Cadena donde calcular la frecuencia.
 * Return:
 *              int: Frecuencia de la letra en la cadena.
 * ***********************************************************/
int calcula_fi(char letra, char* cadena) {
    int i;
    int f=0;    /* frecuencia de la letra */

    /* Vamos sumando las veces que aparece la letra */
    for (i=0; i<strlen(cadena); i++) {
        if (cadena[i] == letra) {
            f++;
        }
    }

    return f;
}