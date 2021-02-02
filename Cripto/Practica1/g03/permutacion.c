/**************************************************************
 * File: permutacion.c
 * Author: Aitor Melero, Ana Roa
 * Date: 23/10/2020
 * Last_Date: 23/10/2020
 * Function: Implementacion de funcionalidad para algoritmo de permutacion.
 * ***********************************************************/


/* INCLUDES */
#include "permutacion.h"


/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_permutacion(FILE* i, FILE* o, char* k1, char* k2)
 * Function: Crear un elemento de tipo PERMUTACION.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 *              char* k1: Cadena con el vector clave 1.
 *              char* k2: Cadena con el vector clave 2.
 * Return:
 *              PERMUTACION: Elemento PERMUTACION creado.
 * ***********************************************************/
PERMUTACION* create_permutacion(FILE* i, FILE* o, char* k1, char* k2) {
    PERMUTACION* permutacion = NULL;
    STATUS estado = OK;

    /* Reservamos memoria para tipo permutacion */
    permutacion = (PERMUTACION*)calloc(1, sizeof(PERMUTACION));
    if (!permutacion) {
        printf("ERROR al reservar permutacion!!!\n");
        return NULL;
    }

    /* Inicializamos valores */
    if (!i || !o) {
        printf("ERROR con el fichero de entrada/salida!!!\n");
        free_permutacion(permutacion);
        return NULL;
    }
    permutacion->i = i;
    permutacion->o = o;
    permutacion->m = strlen(k1);
    permutacion->n = strlen(k2);
    permutacion->num_matrices = 0;

    /* Pasamos la cadena de claves a una lista de enteros */
    estado = generar_claves_permutacion(permutacion, k1, k2);

    /* Pasamos el texto de entrada a una cadena sin caracteres especiales */
    if (estado == OK) {
        estado = generador_texto_permutacion(permutacion);
    }

    /* Generamos la lista de matrices del texto de entrada */
    if (estado == OK) {
        estado = generar_matriz_texto_permutacion(permutacion);
    }

    /* Devolvemos el tipo permutacion si no hay errores */
    if (estado == ERROR) {
        printf("ERROR al inicializar tipo permutacion!!!\n");
        free_permutacion(permutacion);
        return NULL;
    }

    return permutacion;
}


/*************************************************************/

/**************************************************************
 * Name: free_permutacion(PERMUTACION* permutacion)
 * Function: Liberar un elemento de tipo PERMUTACION.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_permutacion(PERMUTACION* permutacion) {
    int i, j;        /* indices */

    if (permutacion) {
        if (permutacion->i) {
            fclose(permutacion->i);
            permutacion->i = NULL;
        }
        if (permutacion->o) {
            fclose(permutacion->o);
            permutacion->o = NULL;
        }
        if (permutacion->k1) {
            free(permutacion->k1);
            permutacion->k1 = NULL;
        }
        if (permutacion->k2) {
            free(permutacion->k2);
            permutacion->k2 = NULL;
        }
        if (permutacion->texto) {
            free(permutacion->texto);
            permutacion->texto = NULL;
        }
        if (permutacion->matriz_texto) {
            for (i=0; i<permutacion->num_matrices; i++) {
                if (permutacion->matriz_texto[i]) {
                    for (j=0; j<permutacion->m; j++) {
                        if (permutacion->matriz_texto[i][j]) {
                            free(permutacion->matriz_texto[i][j]);
                            permutacion->matriz_texto[i][j] = NULL;
                        }
                    }
                    free(permutacion->matriz_texto[i]);
                    permutacion->matriz_texto[i] = NULL;
                }
            }
            free(permutacion->matriz_texto);
            permutacion->matriz_texto = NULL;
        }
        free(permutacion);
        permutacion = NULL;
    }
}


/*************************************************************/

/*************************************************************/
/********************RESTO DE FUNCIONES***********************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: generar_claves_permutacion(PERMUTACION* permutacion, char* k1, char* k2)
 * Function: Genera los vectores clave en formato de lista de enteros.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generar_claves_permutacion(PERMUTACION* permutacion, char* k1, char* k2) {
    int long_k1 = 0;        /* Longitud de k1 */
    int long_k2 = 0;        /* Longitud de k2 */
    int i;                  /* indice */

    /* Control de errores */
    if (!permutacion || !k1 || !k2) {
        printf("ERROR al generar las claves de permutacion!!!\n");
        return ERROR;
    }

    /* Inicializamos la longitud de las claves */
    long_k1 = strlen(k1);
    long_k2 = strlen(k2);

    /* Reservamos memoria para las claves */
    permutacion->k1 = (int*)calloc(long_k1, sizeof(int));
    permutacion->k2 = (int*)calloc(long_k2, sizeof(int));

    /* Asignamos la k1 */
    for (i=0; i<long_k1; i++) {
        permutacion->k1[i] = k1[i]-48;  /* 48 por el ascii de los numeros */
    }
    /* Asignamos la k2 */
    for (i=0; i<long_k2; i++) {
        permutacion->k2[i] = k2[i]-48;  /* 48 por el ascii de los numeros */
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_texto_permutacion(PERMUTACION* permutacion)
 * Function: Lee del fichero de entrada y escribe dicho texto sin espacios.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generador_texto_permutacion(PERMUTACION* permutacion) {
    char caracter;
    int caracteres_leido = 0;
    int offset_lectura = 0;

    /* Control de errores */
    if (!permutacion || !permutacion->i) {
        printf("ERROR al generar texto de entrada con permutacion!!!\n");
        return ERROR;
    }

    /* Vamos leyendo caracter a caracter del fichero de la entrada y lo guardamos */
    fseek(permutacion->i, offset_lectura, SEEK_SET);
    while (fread(&caracter, 1, 1, permutacion->i) == 1) {
        if (caracter >= 65 && caracter <= 90) {
            if (permutacion->texto) {
                caracteres_leido++;
                permutacion->texto = (char*)realloc(permutacion->texto, sizeof(char)*caracteres_leido);
                permutacion->texto[caracteres_leido-1] = caracter;
            } else {
                caracteres_leido++;
                permutacion->texto = (char*)calloc(1, sizeof(char));
                permutacion->texto[caracteres_leido-1] = caracter;
            }
        }
        offset_lectura++;
        fseek(permutacion->i, offset_lectura, SEEK_SET);
    }

    /* Guardamos el final de cadena */
    permutacion->texto = (char*)realloc(permutacion->texto, sizeof(char)*caracteres_leido+1);
    permutacion->texto[caracteres_leido] = '\0';

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generar_matriz_texto_permutacion(PERMUTACION* permutacion)
 * Function: Genera la lista de matrices del texto de entrada.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS generar_matriz_texto_permutacion(PERMUTACION* permutacion) {
    int i, j, k;        /* indices */
    int fin = 0;        /* indica si ya no hay mas texto que leer */

    /* Control de errores */
    if (!permutacion) {
        printf("ERROR al generar matriz de texto de permutacion!!!\n");
        return ERROR;
    }
    
    /* Hallamos el numero de matrices de la lista */
    permutacion->num_matrices = strlen(permutacion->texto) / (permutacion->m * permutacion->n);
    if (permutacion->num_matrices == 0) {
        permutacion->num_matrices = 1;
    } else if (strlen(permutacion->texto) % (permutacion->m * permutacion->n) != 0) {
        permutacion->num_matrices++;
    }

    /* Reservamos memoria para la lista de matrices */
    permutacion->matriz_texto = (int***)calloc(permutacion->num_matrices, sizeof(int**));

    /* Vamos asignando a la matriz cada letra del texto */
    for (i=0; i<permutacion->num_matrices; i++) {
        /* Reservamos para el numero de filas */
        if (!permutacion->matriz_texto[i]) {
            permutacion->matriz_texto[i] = (int**)calloc(permutacion->m, sizeof(int*));
        }
        for (j=0; j<permutacion->m; j++) {
            /* Reservamos para el numero de columnas */
            if (!permutacion->matriz_texto[i][j]) {
                permutacion->matriz_texto[i][j] = (int*)calloc(permutacion->n, sizeof(int));
            }
            /* Asignamos valores */
            for (k=0; k<permutacion->n; k++) {
                /* Si no hay mas texto que leer rellenamos la matriz con -1 */
                if (fin == strlen(permutacion->texto)) {
                    permutacion->matriz_texto[i][j][k] = -1;
                } else {
                    permutacion->matriz_texto[i][j][k] = permutacion->texto[fin];
                    fin++;
                }
            }
        }
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: cifrar_permutacion(PERMUTACION* permutacion)
 * Function: Cifra el texto de entrada usando el cifrado de permutacion.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS cifrar_permutacion(PERMUTACION* permutacion) {
    int limite_matrices = 0;            /* indica el numero de matrices exactas */
    int** matriz_fila = NULL;           /* matriz para permutacion de filas */
    int** matriz_columna = NULL;        /* matriz para permutacion de columnas */
    int i = 0;                          /* indice */

    /* Control de errores */
    if (!permutacion) {
        printf("ERROR al cifrar con permutacion!!!\n");
        return ERROR;
    }

    /* Asignamos el limite de numero de matrices */
    if ((strlen(permutacion->texto)/(permutacion->m * permutacion->n) != 0) && (strlen(permutacion->texto)%(permutacion->m * permutacion->n) != 0)) {
        limite_matrices = permutacion->num_matrices-1;
    } else if ((strlen(permutacion->texto)/(permutacion->m * permutacion->n) == 0) && (strlen(permutacion->texto)%(permutacion->m * permutacion->n) != 0)) { 
        limite_matrices = -1;
    } else {
        limite_matrices = permutacion->num_matrices;
    }
    
    /* Si tenemos al menos una matriz con el tamanio hacemos permutacion, si no lo dejamos como esta */
    if (limite_matrices != -1) {
        /* Generamos las matrices para las permutaciones */
        matriz_fila = generador_matriz_fila_permutacion(permutacion);
        matriz_columna = generador_matriz_columna_permutacion(permutacion);

        /* Por cada matriz vamos aplicando las permutaciones con la multiplicación */
        /* PRIMERO: FILAS */
        for (i=0; i<limite_matrices; i++) {
            multiplicar_matriz_permutacion(permutacion->matriz_texto[i], matriz_fila, permutacion->m, permutacion->n, TRUE);
        }
    
        /* SEGUNDO: COLUMNAS */
        for (i=0; i<limite_matrices; i++) {
            multiplicar_matriz_permutacion(permutacion->matriz_texto[i], matriz_columna, permutacion->m, permutacion->n, FALSE);
        }
    }

    /* Escribimos el texto cifrado en el fichero de salida */
    escribe_salida_permutacion(permutacion);

    /* Liberamos recursos */
    if (matriz_fila) {
        for (i=0; i<permutacion->m; i++) {
            if (matriz_fila[i]) {
                free(matriz_fila[i]);
                matriz_fila[i] = NULL;
            }
        }
        free(matriz_fila);
        matriz_fila = NULL;
    }
    if (matriz_columna) {
        for (i=0; i<permutacion->n; i++) {
            if (matriz_columna[i]) {
                free(matriz_columna[i]);
                matriz_columna[i] = NULL;
            }
        }
        free(matriz_columna);
        matriz_columna = NULL;
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: descifrar_permutacion(PERMUTACION* permutacion)
 * Function: Descifra el texto de entrada usando el cifrado de permutacion.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS descifrar_permutacion(PERMUTACION* permutacion) {
    int limite_matrices = 0;            /* indica el numero de matrices exactas */
    int** matriz_fila = NULL;           /* matriz para permutacion de filas */
    int** matriz_columna = NULL;        /* matriz para permutacion de columnas */
    int i = 0;                          /* indice */

    /* Control de errores */
    if (!permutacion) {
        printf("ERROR al cifrar con permutacion!!!\n");
        return ERROR;
    }

    /* Asignamos el limite de numero de matrices */
    if ((strlen(permutacion->texto)/(permutacion->m * permutacion->n) != 0) && (strlen(permutacion->texto)%(permutacion->m * permutacion->n) != 0)) {
        limite_matrices = permutacion->num_matrices-1;
    } else {
        limite_matrices = permutacion->num_matrices;
    }

    /* Generamos las matrices para las permutaciones */
    matriz_fila = generador_matriz_fila_permutacion(permutacion);
    matriz_columna = generador_matriz_columna_permutacion(permutacion);
    /* Sacamos la traspuesta para el descifrado */
    calcular_matriz_traspuesta_permutacion(matriz_columna, permutacion->n);
    calcular_matriz_traspuesta_permutacion(matriz_fila, permutacion->m);

    /* Por cada matriz vamos aplicando las permutaciones con la multiplicación */
    /* PRIMERO: COLUMNAS */
    for (i=0; i<limite_matrices; i++) {
        multiplicar_matriz_permutacion(permutacion->matriz_texto[i], matriz_columna, permutacion->m, permutacion->n, FALSE);
    }
    /* SEGUNDO: FILAS */
    for (i=0; i<limite_matrices; i++) {
        multiplicar_matriz_permutacion(permutacion->matriz_texto[i], matriz_fila, permutacion->m, permutacion->n, TRUE);
    }

    /* Escribimos el texto descifrado en el fichero de salida */
    escribe_salida_permutacion(permutacion);

    /* Liberamos recursos */
    if (matriz_fila) {
        for (i=0; i<permutacion->m; i++) {
            if (matriz_fila[i]) {
                free(matriz_fila[i]);
                matriz_fila[i] = NULL;
            }
        }
        free(matriz_fila);
        matriz_fila = NULL;
    }
    if (matriz_columna) {
        for (i=0; i<permutacion->n; i++) {
            if (matriz_columna[i]) {
                free(matriz_columna[i]);
                matriz_columna[i] = NULL;
            }
        }
        free(matriz_columna);
        matriz_columna = NULL;
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: generador_matriz_fila_permutacion(PERMUTACION* permutacion)
 * Function: Genera la matriz cuadrada para la permutacion de filas.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              int**: Matriz cuadrada para la permutacion de filas.
 * ***********************************************************/
int** generador_matriz_fila_permutacion(PERMUTACION* permutacion) {
    int** matriz_fila = NULL;       /* matriz fila m*m */
    int i, j;                       /* indices */
    int clave;                      /* valor de la clave */

    /* Control de errores */
    if (!permutacion) {
        printf("ERROR al generar matriz fila!!!\n");
        return NULL;
    }

    /* Reservamos memoria para la matriz fila */
    matriz_fila = (int**)calloc(permutacion->m, sizeof(int*));

    /* Asignamos la matriz */
    for (i=0; i<permutacion->m; i++) {
        clave = permutacion->k1[i]-1;
        /* Reservamos columnas */
        if (!matriz_fila[i]) {
            matriz_fila[i] = (int*)calloc(permutacion->m, sizeof(int));
        }
        for (j=0; j<permutacion->m; j++) {
            /* Si el valor de la clave coincide con la columna se guarda 1 */
            if (clave == j) {
                matriz_fila[i][j] = 1;
            } else {
                matriz_fila[i][j] = 0;
            }
        }
    } 

    return matriz_fila;
}


/*************************************************************/

/**************************************************************
 * Name: generador_matriz_columna_permutacion(PERMUTACION* permutacion)
 * Function: Genera la matriz cuadrada para la permutacion de columnas.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              int**: Matriz cuadrada para la permutacion de columnas.
 * ***********************************************************/
int** generador_matriz_columna_permutacion(PERMUTACION* permutacion) {
    int** matriz_columna = NULL;       /* matriz fila n*n */
    int i, j;                          /* indices */
    int clave;                         /* valor de la clave */

    /* Control de errores */
    if (!permutacion) {
        printf("ERROR al generar matriz fila!!!\n");
        return NULL;
    }

    /* Reservamos memoria para la matriz fila */
    matriz_columna = (int**)calloc(permutacion->n, sizeof(int*));
    for (i=0; i<permutacion->n; i++) {
        matriz_columna[i] = (int*)calloc(permutacion->n, sizeof(int));
    }

    /* Asignamos la matriz */
    for (i=0; i<permutacion->n; i++) {
        clave = permutacion->k2[i]-1;
        for (j=0; j<permutacion->n; j++) {
            /* Si el valor de la clave coincide con la fila se guarda 1 */
            if (clave == j) {
                matriz_columna[j][i] = 1;
            } else {
                matriz_columna[j][i] = 0;
            }
        }
    }

    return matriz_columna;
}


/*************************************************************/

/**************************************************************
 * Name: multiplicar_matriz_permutacion(int** matriz, int** matriz_permutacion, int m, int n, BOOL fila)
 * Function: Multiplica matrices para la permutacion.
 * Parameters:
 *              int** matriz: Matriz del texto a permutar.
 *              int** matriz_permutacion: Matriz fila o columna.
 *              int m: Numero de filas de la matriz.
 *              int n: Numero de columnas de la matriz.
 *              BOOL fila: Booleano que indica si tenemos matriz fila o matriz columna.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS multiplicar_matriz_permutacion(int** matriz, int** matriz_permutacion, int m, int n, BOOL fila) {
    int** matriz_aux = NULL;        /* matriz auxiliar */
    int valor_aux = 0;              /* valor auxiliar */
    int ind_1, ind_2, ind_3;        /* limites auxiliares para simplificar codigo */
    int i, j, k;                    /* indices */

    /* Control de errores */
    if (!matriz || !matriz_permutacion || m<1 || n<1) {
        printf("ERROR al multiplicar matriz permutacion!!!\n");
        return ERROR;
    }

    /* Reservamos memoria para la matriz auxiliar */
    matriz_aux = (int**)calloc(m, sizeof(int*));
    for (i=0; i<m; i++) {
        matriz_aux[i] = (int*)calloc(n, sizeof(int));
    }

    /* Comprobamos si la multiplicacion es para filo o columna */
    if (fila == TRUE) {
        ind_1 = m;
        ind_2 = n;
        ind_3 = m;
    } else {
        ind_1 = m;
        ind_2 = n;
        ind_3 = n;
    }

    /* Vamos multiplicando */
    for (i=0; i<ind_1; i++) {
        for (k=0; k<ind_2; k++) {
            for (j=0, valor_aux=0; j<ind_3; j++) {
                if (fila == TRUE) {
                    valor_aux += matriz_permutacion[i][j] * matriz[j][k];
                } else {
                    valor_aux += matriz[i][j] * matriz_permutacion[j][k];
                }
            }
            matriz_aux[i][k] = valor_aux;
        }
    }

    /* Copiamos la matriz */
    for (i=0; i<m; i++) {
        for (j=0; j<n; j++) {
            matriz[i][j] = matriz_aux[i][j];
        }
    }

    /* Liberamos la matriz auxiliar */
    if (matriz_aux) {
        for (i=0; i<m; i++) {
            if (matriz_aux[i]) {
                free(matriz_aux[i]);
                matriz_aux[i] = NULL;
            }
        }
        free(matriz_aux);
        matriz_aux = NULL;
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: calcular_matriz_traspuesta_permutacion(int** matriz, int tam)
 * Function: Genera la matriz traspuesta de una matriz dada.
 * Parameters:
 *              int** matriz: Matriz a calcular su traspuesta.
 *              int tam: Tamanio de la matriz cuadrada.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS calcular_matriz_traspuesta_permutacion(int** matriz, int tam) {
    int i, j;                   /* indices */
    int** matriz_aux = NULL;    /* matriz auxiliar para cambiar valores */

    /* Control de errores */
    if (!matriz || tam < 1) {
        printf("ERROR al generar matriz traspuesta en permutacion!!!\n");
        return ERROR;
    }

    /* Reservamos memoria para la matriz auxiliar */
    matriz_aux = (int**)calloc(tam, sizeof(int*));
    for (i=0; i<tam; i++) {
        matriz_aux[i] = (int*)calloc(tam, sizeof(int));
    }

    /* Asignamos la matriz traspuesta a la matriz auxiliar */
    for (i=0; i<tam; i++) {
        for (j=0; j<tam; j++) {
            matriz_aux[i][j] = matriz[j][i];
        }
    }

    /* Copiamos y liberamos la matriz */
    if (matriz_aux) {
        for (i=0; i<tam; i++) {
            if (matriz_aux[i]) {
                for (j=0; j<tam; j++) {
                    matriz[i][j] = matriz_aux[i][j];
                }
                free(matriz_aux[i]);
                matriz_aux[i] = NULL;
            }
        }
        free(matriz_aux);
    }

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: escribe_salida_permutacion(PERMUTACION* permutacion)
 * Function: Escribe el texto-solucion en el fichero de salida.
 * Parameters:
 *              PERMUTACION* permutacion: Elemento de tipo permutacion.
 * Return:
 *              STATUS: OK en caso correcto, ERROR en caso de error.
 * ***********************************************************/
STATUS escribe_salida_permutacion(PERMUTACION* permutacion) {
    char caracter;
    int offset_lectura = 0;
    int offset_escritura = 0;
    int i, j, k;                    /* indices */
    int num_caracter;               /* representa el numero de caracter */

    /* Control de errores */
    if (!permutacion || !permutacion->i || !permutacion->o) {
        printf("ERROR al generar texto de salida con permutacion!!!\n");
        return ERROR;
    }

    /* Inicializamos valores */
    fseek(permutacion->i, offset_lectura, SEEK_SET);
    fseek(permutacion->o, offset_escritura, SEEK_SET);
    i = 0;
    j = 0;
    k = 0;
    num_caracter = 0;

    /* Vamos leyendo caracter a caracter del fichero de la entrada y lo guardamos */
    while ((fread(&caracter, 1, 1, permutacion->i) == 1) && (num_caracter != -2)) {
        if (caracter >= 65 && caracter <= 90) {
            num_caracter = permutacion->matriz_texto[i][j][k];
            k++;
            /* Comprobamos si no hay final de matriz */
            if (num_caracter != -1) {
                /* Escribimos el caracter solucion y cambiamos indices */
                caracter = num_caracter;
                fwrite(&caracter, 1, 1, permutacion->o);
                offset_escritura++;
                fseek(permutacion->o, offset_escritura, SEEK_SET);
            }
            if (k == permutacion->n) {
                k = 0;
                j++;
                if (j == permutacion->m) {
                    j = 0;
                    i++;
                    if (i == permutacion->num_matrices) {
                        num_caracter = -2;
                    }
                }
            }
        } else {
            /* Escribimos el caracter especial tal cual */
            fwrite(&caracter, 1, 1, permutacion->o);
            offset_escritura++;
            fseek(permutacion->o, offset_escritura, SEEK_SET);
        }
        offset_lectura++;
        fseek(permutacion->i, offset_lectura, SEEK_SET);
    }

    return OK;
}