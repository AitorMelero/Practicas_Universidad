/**************************************************************
 * File: descifrador.c
 * Author: Aitor Melero, Ana Roa
 * Date: 18/10/2020
 * Last_Date: 19/10/2020
 * Function: Implementacion de funcionalidad para descifrar texto
 *           texto cifrado con Vigenere.
 * ***********************************************************/


/* INCLUDES */
#include "descifrador.h"


/* Definimos las probabilidades de cada letra para el alfabeto espaniol e ingles */
const double SPAIN[26] = {11.96,0.92,2.92,6.87,16.78,0.52,0.73,0.89,4.15,0.30,0,8.37,2.12,7.01,8.69,2.77,1.53,4.94,7.88,3.31,4.80,0.39,0,0.06,1.54,0.15};
const double ENG[26] = {8.04,1.54,3.06,3.99,12.51,2.30,1.96,5.49,7.26,0.16,0.67,4.14,2.53,7.09,7.60,2.0,0.11,6.12,6.54,9.25,2.71,0.99,1.92,0.19,1.73,0.19};
const double CASTELLANO = 0.083235;
const double INGLES = 0.065;

/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_descifrador(FILE* i, FILE* o, BOOL metodo, int l)
 * Function: Crear un elemento de tipo DESCIFRADOR.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero donde escribir el texto descifrado.
 *              BOOL metodo: Indica si usar kasiski o indice de coincidencias.
 *              int l: Longitud de n-gramas.
 * Return:
 *              DESCIFRADOR: Elemento DESCIFRADOR creado.
 * ***********************************************************/
DESCIFRADOR* create_descifrador(FILE* i, FILE* o, BOOL metodo, int l) {
    DESCIFRADOR* descifrador = NULL;
    STATUS estado = ERROR;
    int j;

    /* Reservamos memoria para descifrador */
    descifrador = (DESCIFRADOR*)calloc(1, sizeof(DESCIFRADOR));
    if (!descifrador) {
        printf("ERROR al reservar memoria para descifrador!!!\n");
        return NULL;
    }

    /* Inicializamos valores */
    descifrador->i = i;
    descifrador->o = o;
    descifrador->kasiski_met = metodo;
    descifrador->l = l;
    descifrador->n_gramas = NULL;
    descifrador->long_clave = -1;

    /* Generamos el texto cifrado sin espacios */
    estado = generador_texto_cifrado_descifrador(descifrador);

    /* Hallamos la longitud de la clave */
    if (estado == OK) {
        estado = generador_long_clave_descifrador(descifrador);
    }

    /* Generamos la tabla de n-gramas */
    if (estado == OK) {
        estado = generador_ngramas_descifrador(descifrador);
    }

    /* Reservamos memoria para la tabla de valores de mg */
    if (estado == OK) {
        descifrador->valores_mg = (double**)calloc(descifrador->long_clave, sizeof(double*));
        for (j=0; j<descifrador->long_clave; j++) {
            descifrador->valores_mg[j] = (double*)calloc(26, sizeof(double));
        }
    }

    /* Hallamos la posible clave */
    if (estado == OK) {
        estado = generador_clave(descifrador);
    }

    /* Desciframos el mensaje con la posible clave encontrada */
    if (estado == OK) {
        estado = descifra_descifrador(descifrador);
    }

    /* Devolvemos el descifrador */
    if (estado == ERROR) {
        printf("ERROR al inicializar descifrador!!!\n");
        free_descifrador(descifrador);
        return NULL;
    }

    return descifrador;

}


/*************************************************************/

/**************************************************************
 * Name: free_descifrador(DESCIFRADOR* descifrador)
 * Function: Liberar un elemento de tipo DESCIFRADOR.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_descifrador(DESCIFRADOR* descifrador) {
    int i;

    if (descifrador) {
        if (descifrador->i) {
            fclose(descifrador->i);
            descifrador->i = NULL;
        }
        if (descifrador->o) {
            fclose(descifrador->o);
            descifrador->o = NULL;
        }
        if (descifrador->texto_cifrado) {
            free(descifrador->texto_cifrado);
            descifrador->texto_cifrado = NULL;
        }
        if (descifrador->n_gramas) {
            for (i=0; i<descifrador->long_clave; i++) {
                if (descifrador->n_gramas[i]) {
                    free(descifrador->n_gramas[i]);
                }
            }
            free(descifrador->n_gramas);
        }
        if (descifrador->valores_mg) {
            for (i=0; i<descifrador->long_clave; i++) {
                if (descifrador->valores_mg[i]) {
                    free(descifrador->valores_mg[i]);
                }
            }
            free(descifrador->valores_mg);
        }
        if (descifrador->clave) {
            free(descifrador->clave);
        }
        free(descifrador);
        descifrador = NULL;
    }
}


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generador_texto_cifrado_descifrador(DESCIFRADOR* descifrador)
 * Function: Lee del fichero cifrado y escribe dicho texto sin espacios.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_cifrado_descifrador(DESCIFRADOR* descifrador) {
    char caracter_cifrado;
    int caracteres_leido = 0;
    int offset_lectura = 0;

    /* Control de errores */
    if (!descifrador || !descifrador->i) {
        printf("ERROR al generar texto cifrado con descifrador!!!\n");
        return ERROR;
    }

    /* Vamos leyendo caracter a caracter del fichero de la entrada y lo guardamos */
    fseek(descifrador->i, offset_lectura, SEEK_SET);
    while (fread(&caracter_cifrado, 1, 1, descifrador->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            if (descifrador->texto_cifrado) {
                caracteres_leido++;
                descifrador->texto_cifrado = (char*)realloc(descifrador->texto_cifrado, sizeof(char)*caracteres_leido);
                descifrador->texto_cifrado[caracteres_leido-1] = caracter_cifrado;
            } else {
                caracteres_leido++;
                descifrador->texto_cifrado = (char*)calloc(1, sizeof(char));
                descifrador->texto_cifrado[caracteres_leido-1] = caracter_cifrado;
            }
        }
        offset_lectura++;
        fseek(descifrador->i, offset_lectura, SEEK_SET);
    }

    /* Guardamos el final de cadena */
    descifrador->texto_cifrado = (char*)realloc(descifrador->texto_cifrado, sizeof(char)*caracteres_leido+1);
    descifrador->texto_cifrado[caracteres_leido] = '\0';

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_ngramas_descifrador(DESCIFRADOR* descifrador)
 * Function: Genera la tabla de n-gramas.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_ngramas_descifrador(DESCIFRADOR* descifrador) {
    int i, j, cont_texto;

    /* Control de errores */
    if (!descifrador) {
        printf("ERROR al generar n-gramas de descifrador!!!\n");
        return ERROR;
    }

    /* Reservamos memoria para las filas de n-gramas */
    if (!descifrador->n_gramas) {
        descifrador->n_gramas = (char**)calloc(descifrador->long_clave, sizeof(char*));
        for (i=0; i<(descifrador->long_clave); i++) {
            if (!descifrador->n_gramas[i]) {
                descifrador->n_gramas[i] = (char*)calloc(1, sizeof(char));
                descifrador->n_gramas[i][0] = '\0';
            }
        }
    }

    /* Vamos guardando caracter a caracter en la tabla de n-gramas */
    cont_texto = 0;
    j = 0;
    while (cont_texto < strlen(descifrador->texto_cifrado)) {
        for (i=0; (i<descifrador->long_clave) && (cont_texto < strlen(descifrador->texto_cifrado)); i++) {
            descifrador->n_gramas[i][j] = descifrador->texto_cifrado[cont_texto];
            descifrador->n_gramas[i] = (char*)realloc(descifrador->n_gramas[i], sizeof(char)*(j+2));
            descifrador->n_gramas[i][j+1] = '\0';
            cont_texto++;
        }
        j++;
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_long_clave_descifrador(DESCIFRADOR* descifrador)
 * Function: Guarda la posible longitud de clave.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_long_clave_descifrador(DESCIFRADOR* descifrador) {
    KASISKI* kasiski = NULL;
    BOOL clave_hallada = FALSE;
    IC* ic = NULL;
    int i = 0;
    double ind_aux = 0;

    /* Control de errores */
    if (!descifrador) {
        printf("ERROR al hallar la longitud de clave en descifrador!!!\n");
        return ERROR;
    }

    /* Hallamos la longitud de clave con kasiski en caso de pedirlo el usuario */
    if (descifrador->kasiski_met == TRUE) {
        kasiski = create_kasiski(descifrador->i, descifrador->l);
        /* Caso de error en el que no se puede hallar la longitud de clave */
        if (!kasiski) {
            return ERROR;
        }
        descifrador->long_clave = kasiski->mcd;
        free_kasiski(kasiski);
    }

    /* Si el usuario no desea hallar la longitud de clave con kasiski, lo hacemos encontrando
       el mejor indice de coincidencia */
    if (descifrador->kasiski_met == FALSE) {
        while (clave_hallada == FALSE) {
            i++;
            ic = create_ic(descifrador->i, i);
            ind_aux = ic->resultado;
            /* Vamos comprobando con que longitud el indice de coincidencia es mas cercano al 
               del alfabeto */
            if (!(ind_aux <= CASTELLANO)) {
                /* Hemos encontrado la posible longitud de clave */
                descifrador->long_clave = i;
                clave_hallada = TRUE;
            }
            free_ic(ic);
        }
    }

    /* Una vez hallada la posible longitud de clave, reservamos memoria para la clave */
    descifrador->clave = (char*)calloc(descifrador->long_clave+1, sizeof(char));

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_clave(DESCIFRADOR* descifrador)
 * Function: Descifra la clave.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_clave(DESCIFRADOR* descifrador) {
    int i, j;                       /* indices auxiliares */
    double diferencia;              /* diferencia actual de ic */
    double diferencia_aux;          /* diferencia para hallar el ic mas cercano */
    int posicion_letra;             /* posicion de la letra de la clave */

    /* Control de errores */
    if (!descifrador) {
        printf("ERROR al generar clave en descifrador!!!\n");
        return ERROR;
    }

    /* Generamos la tabla con los valores de mg */
    if (generador_valores_mg(descifrador) == ERROR) {
        printf("ERROR 2 al generar clave en descifrador!!!\n");
        return ERROR;
    }

    /* Inicializamos valores */
    i = 0;
    j = 0;
    diferencia = 0.0;
    diferencia_aux = 1.0;
    posicion_letra = 0;

    /* Vamos comprobando el valor de cada caracter de la clave */
    for (i=0; i<descifrador->long_clave; i++) {
        posicion_letra = 0;
        diferencia_aux = 1.0;
        for (j=0; j<26; j++) {
            diferencia = fabs(CASTELLANO - descifrador->valores_mg[i][j]);
            if (diferencia < diferencia_aux) {
                diferencia_aux = diferencia;
                posicion_letra = j;
            }
        }
        descifrador->clave[i] = posicion_letra + 65;
    }

    /* Escribimos el caracter final en la clave */
    descifrador->clave[i] = '\0';

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_valores_mg(DESCIFRADOR* descifrador)
 * Function: Genera los valores mg.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_valores_mg(DESCIFRADOR* descifrador) {
    int i, j, k;            /* indices auxiliare */
    int n;                  /* longitud texto entre longitud n-grama */
    double sumatorio;       /* sumatorio en la ecuacion */
    int fi;                 /* frecuencia de una letra */

    /* Control de errores */
    if (!descifrador) {
        printf("ERROR al generar valores de mg en descifrador!!!\n");
        return ERROR;
    }

    /* Inicializamos variables */
    i = 0;
    j = 0;
    k = 0;
    n = strlen(descifrador->texto_cifrado) / descifrador->long_clave;
    sumatorio = 0.0;

    /* Vamos aplicando la formula vista en teoria para calcular la tabla */
    for (i=0; i<descifrador->long_clave; i++) {
        for (j=0; j<26; j++) {
            for (k=0, sumatorio=0; k<26; k++) {
                fi = calcula_fi_descifrador(65+((j+k)%26), descifrador->n_gramas[i]);
                sumatorio += (SPAIN[k]/100)*fi;
            }
            descifrador->valores_mg[i][j] = sumatorio / n;
        }
    }

    return OK;

}


/*************************************************************/

/**************************************************************
 * Name: calcula_fi_descifrador(char letra, char* cadena)
 * Function: Calcula la frecuencia de una letra en una cadena.
 * Parameters:
 *              char letra: Letra a calcular la frecuencia.
 *              char* cadena: Cadena donde calcular la frecuencia de la letra.
 * Return:
 *              int: Frecuencia de la letra en la cadena.
 * ***********************************************************/
int calcula_fi_descifrador(char letra, char* cadena) {
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


/*************************************************************/

/**************************************************************
 * Name: descifra_descifrador(DESCIFRADOR* descifrador)
 * Function: Descifra el mensaje con la clave hallada.
 * Parameters:
 *              DESCIFRADOR* descifrador: Elemento de tipo descifrador.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS descifra_descifrador(DESCIFRADOR* descifrador) {
    VIGENERE* vigenere = NULL;
    char* copia_clave = NULL;

    /* Control de errores */
    if (!descifrador) {
        printf("ERROR al descifrar en descifrador!!!\n");
        return ERROR;
    }

    /* Copiamos la clave para dejarsela a vigenere */
    copia_clave = (char*)calloc(descifrador->long_clave+1, sizeof(char));
    strcpy(copia_clave, descifrador->clave);
    /* Desciframos con el algoritmo de vigenere ya que se supone que ya tenemos la clave */
    vigenere = create_vigenere(copia_clave, descifrador->i, descifrador->o);
    descifrar(vigenere);

    /* Liberamos los recursos */
    free(vigenere->k);
    mpz_clear(vigenere->m);
    free(vigenere);

    return OK;
}