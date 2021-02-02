/**************************************************************
 * File: no_linealidad_SBoxes_AES.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 22/11/2020
 * Last_Date: 22/11/2020 
 * Function: Programa que estudia la no linealidad de las s-boxes de AES. 
 * ***********************************************************/

#include "type.h"

/**************************************************************
 * Name: genera_S_AES(char contenido)
 * Function: Pasa de cadena a numero el contenido de la s-box de AES. 
 * Parameters:
 *              char contenido: Contenido de la s-box de AES en cadena. 
 * Return:
 *              unsigned char: Contenido en formato numero. 
 * ***********************************************************/
unsigned char genera_S_AES(char contenido) {
    unsigned char res;

    /* Comprobamos la letra */
    if (contenido>=48 && contenido<=57) {
        res = (contenido - 48);
    } else {
        res = (contenido - 87);
    }

    return res;

}

/**************************************************************
 * Name: sacar_fila_AES(unsigned char bloque)
 * Function: Saca la fila para la caja s del AES a partir de un bloque. 
 * Parameters:
 *              unsigned char bloque: Bloque a partir del cual sacar la fila.
 * Return:
 *              int: Numero de fila.
 * ***********************************************************/
int sacar_fila_AES(unsigned char bloque) {
    return bloque >> 4;
}

/**************************************************************
 * Name: sacar_columna_AES(unsigned char bloque)
 * Function: Saca la columna para la caja s del AES a partir de un bloque. 
 * Parameters:
 *              unsigned char bloque: Bloque a partir del cual sacar la columna.
 * Return:
 *              int: Numero de columna.
 * ***********************************************************/
int sacar_columna_AES(unsigned char bloque) {
    return bloque & 0x0F;
}

/**************************************************************
 * Name: muestra_info_parametros()
 * Function: Muestra informacion sobre los parametros a introducir.
 * Parameters:
 *              Ninguno.
 * Return:
 *              Nada.
 * ***********************************************************/
void muestra_info_parametros() {
    printf("################################################\n\n");
    printf("Se debe introducir la siguiente secuencia: \n");
    printf("./no_linealidad_SBoxes_AES -D/-I\n");
    printf("################################################\n");
    return;
}


/* Funcion principal */
int main(int argc, char *argv[]) {
    int i, j;
    int iteraciones = 256;
    unsigned char x, y;
    unsigned char* res1_aux = NULL;
    unsigned char* res2_aux = NULL;
    unsigned char res1;
    unsigned char res2;
    int fila, columna;
    int no_lineal, casos; 

    /* Control de errores */
    if (argc != 2) {
        muestra_info_parametros();
        return -1;
    } else if ((strcmp(argv[1], "-I")!=0) && (strcmp(argv[1], "-D")!=0)) {
        muestra_info_parametros();
        return -1;
    }
    
    /* Bucle que va realizando las comprobaciones n veces */
    for (i=0, no_lineal=0, casos=0; i<iteraciones; i++) {
            x = (unsigned char) i;
        for (j=i; j<iteraciones; j++) {
            /* Asignamos variables */
            y = (unsigned char) j;

            /* f(x) + f(y) */
            if (strcmp(argv[1], "-D") == 0) {
                fila = sacar_fila_AES(x);
                columna = sacar_columna_AES(x);
                res1 = (genera_S_AES(DIRECT_SBOX[fila][columna][0])) << 4;
                res1 = (genera_S_AES(DIRECT_SBOX[fila][columna][1])) ^ res1;
                fila = sacar_fila_AES(y);
                columna = sacar_columna_AES(y);
                res2 = (genera_S_AES(DIRECT_SBOX[fila][columna][0])) << 4;
                res2 = (genera_S_AES(DIRECT_SBOX[fila][columna][1])) ^ res2;
            } else {
                fila = sacar_fila_AES(x);
                columna = sacar_columna_AES(x);
                res1 = (genera_S_AES(INVERSE_SBOX[fila][columna][0])) << 4;
                res1 = (genera_S_AES(INVERSE_SBOX[fila][columna][1])) ^ res1;
                fila = sacar_fila_AES(y);
                columna = sacar_columna_AES(y);
                res2 = (genera_S_AES(INVERSE_SBOX[fila][columna][0])) << 4;
                res2 = (genera_S_AES(INVERSE_SBOX[fila][columna][1])) ^ res2;
            }
            /* Guardamos el resultado */
            res1_aux = xor(&res1, &res2, 1);

            /* f(x + y) */
            res2_aux = xor(&x, &y, 1);
            fila = sacar_fila_AES(res2_aux[0]);
            columna = sacar_columna_AES(res2_aux[0]);
            free(res2_aux);
            res2_aux = (unsigned char*)calloc(1, sizeof(unsigned char*));
            if (strcmp(argv[1], "-D") == 0) {
                res2_aux[0] = (genera_S_AES(DIRECT_SBOX[fila][columna][0])) << 4;
                res2_aux[0] = (genera_S_AES(DIRECT_SBOX[fila][columna][1])) ^ res2_aux[0];
            } else {
                res2_aux[0] = (genera_S_AES(INVERSE_SBOX[fila][columna][0])) << 4;
                res2_aux[0] = (genera_S_AES(INVERSE_SBOX[fila][columna][1])) ^ res2_aux[0];
            }

            /* Comprobamos que los dos resultados no seran iguales casi al 100 % */
            if (res1_aux[0]!=res2_aux[0]){
                no_lineal++; 
            } else {
                /* Mostramos los resultados cuando son lineales */
                printf("#############################################\n");
                printf("CASO LINEAL: \n");
                printf("x = %d\n", x);
                printf("y = %d\n", y);
                printf("f(x) = %d\n", res1);
                printf("f(y) = %d\n", res2);
                printf("f(x + y)= %d\n", res2_aux[0]);
                printf("f(x) + f(y)= %d\n", res1_aux[0]);
                printf("#############################################\n");
            }

            casos++;

            /* Liberamos memoria */
            free(res2_aux);
            free(res1_aux);
        }
    }

    /* Mostramos la estadistica total de no linealidad */
    printf("\n\n#################################################\n");
    printf("CASOS NO LINEALES: %d\n", no_lineal);
    printf("CASOS LINEALES: %d\n", (casos-no_lineal));
    printf("CASOS TOTALES: %d\n", casos);
    printf("PORCENTAJE NO LINEALIDAD: %f\n", ((double)no_lineal/(double)casos)*100.0);
    printf("#################################################\n");

    return 0;
}
