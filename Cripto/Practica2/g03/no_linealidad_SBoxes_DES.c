/**************************************************************
 * File: no_linealidad_SBoxes_DES.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 22/11/2020
 * Last_Date: 22/11/2020 
 * Function: Programa que estudia la no linealidad de las s-boxes de DES. 
 * ***********************************************************/

#include "type.h"

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
    printf("./no_linealidad_SBoxes_DES \n");
    printf("################################################\n");
    return;
}


/* Funcion principal */
int main(int argc, char *argv[]) {
    int i, j, num_caja;
    int iteraciones = 64;
    unsigned char x, y, x_aux, y_aux, xor, res1, res2;
    int fila, columna;
    int no_lineal, casos; 

    /* Control de errores */
    if (argc != 1) {
        muestra_info_parametros();
        return -1;
    }

    /* Bucle que va realizando las comprobaciones n veces */
    for (i=0, no_lineal=0, casos=0; i<iteraciones; i++) {
            x = (unsigned char) i;
            x = x << 2;
        for (j=i; j<iteraciones; j++) {
            /* Asignamos variables */
            y = (unsigned char) j;
            y = y << 2;
            /* Recorremos el cada s-box */
            for (num_caja=0; num_caja<8; num_caja++) {
                /* Llamamos a la funcion de S-BOX con el xor de los dos numeros */
                xor = x ^ y; 
                fila = sacar_fila(xor);
                columna = sacar_columna(xor);
                res1 = genera_S(num_caja, fila, columna);

                /* Llamamos a la funcion de S-BOX para cada numero y hacemos su xor */
                fila = sacar_fila(x);
                columna = sacar_columna(x);
                x_aux = genera_S(num_caja, fila, columna);
                fila = sacar_fila(y);
                columna = sacar_columna(y);
                y_aux = genera_S(num_caja, fila, columna);
                res2 = x_aux ^ y_aux; 

                /* Comprobamos que los dos resultados no seran iguales casi al 100 % */
                if (res1 != res2) {
                    no_lineal++; 
                } else {
                    /* Mostramos los resultados cuando son lineales */
                    printf("#############################################\n");
                    printf("CASO LINEAL: \n");
                    printf("num_caja = %d\n", num_caja+1);
                    printf("x = %d\n", x>>2);
                    printf("y = %d\n", y>>2);
                    printf("f(x) = %d\n", x_aux);
                    printf("f(y) = %d\n", y_aux);
                    printf("f(x + y)= %d\n", res1);
                    printf("f(x) + f(y)= %d\n", res2);
                    printf("#############################################\n");
                }

                casos++;


            }
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
