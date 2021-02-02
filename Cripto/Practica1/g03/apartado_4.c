/**************************************************************
 * File: main.c
 * Author: Aitor Melero, Ana Roa
 * Date: 23/10/2020
 * Last_Date: 23/10/2020
 * Function: Funcion principal para programa permutacion.
 * ***********************************************************/


/* INCLUDES */
#include "permutacion.h"


/* DEFINES */
#define TEXTO_PLANO "Textos_Planos/"
#define TEXTO_CIFRADO "Textos_Cifrados/"


/**************************************************************
 * Name: muestra_info_parametros()
 * Function: Muestra informacion sobre los parametros a introducir.
 * Parameters:
 *              Ninguno.
 * Return:
 *              Nada.
 * ***********************************************************/
void muestra_info_parametros() {
    printf("################################################\n");
    printf("Se debe introducir la siguiente secuencia: \n");
    printf("./permutacion -C/-D -k1 <vector-clave1> -k2 <vector-clave2> -i <fichero entrada> -o <fichero salida>\n");
    printf("\nSiendo -i y -o opcionales.\n");
    printf("################################################\n");
    return;
}


/* MAIN */
int main(int argc, char *argv[]) {
    FILE* i = NULL;                         /* fichero de entrada */
    FILE* o = NULL;                         /* fichero de salida */
    char cad_plano[40] = TEXTO_PLANO;       /* cadena que representa un path */
    char cad_cifrado[40] = TEXTO_CIFRADO;   /* cadena que representa un path */
    PERMUTACION* permutacion = NULL;                      /* objeto afin */
    BOOL salida = FALSE;                    /* indica si se usa salida estandar o no */
    char texto[1024] = "";                  /* texto a usar en caso de entrada estandar */
    char salida_pantalla[1024];             /* texto a mostrar en la pantalla en caso de pedirlo */


    /* Control de errores */
    if (argc != 10 && argc != 6 && argc != 8) {
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[1], "-C") != 0 && strcmp(argv[1], "-D") != 0) {
        printf("Primer argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[2], "-k1") != 0) {
        printf("Segundo argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
     }else if (strcmp(argv[4], "-k2") != 0) {
        printf("Tercer argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (argc == 8) {
        if (strcmp(argv[6], "-i") != 0 && strcmp(argv[6], "-o") != 0) {
            printf("Cuarto argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        } 
    } else if (argc == 10) {
        if (strcmp(argv[6], "-i") != 0 && strcmp(argv[8], "-o") != 0) {
            printf("Argumentos incorrectos.\n");
            muestra_info_parametros();
            return 1;
        }
    }


    /* Abrimos los ficheros en funcion de los argumentos */
    if (argc == 10) {
        if (strcmp(argv[1], "-C") == 0) {
            strcat(cad_plano, argv[7]);
            strcat(cad_cifrado, argv[9]);
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-D") == 0) {
            strcat(cad_cifrado, argv[7]);
            strcat(cad_plano, argv[9]);
            i = fopen(cad_cifrado, "r");
            o = fopen(cad_plano, "w");
        }
    } else if (argc == 8) {
        if (strcmp(argv[1], "-C") == 0 && strcmp(argv[6], "-i") == 0) {
            salida = TRUE;
            strcat(cad_plano, argv[7]);
            strcat(cad_cifrado, "salida.txt");
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-C") == 0 && strcmp(argv[6], "-o") == 0) {
            strcat(cad_plano, "entrada.txt");
            strcat(cad_cifrado, argv[7]);
            o = fopen(cad_cifrado, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_plano, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_plano, "r");
        } else if (strcmp(argv[1], "-D") == 0 && strcmp(argv[6], "-i") == 0) {
            salida = TRUE;
            strcat(cad_plano, "salida.txt");
            strcat(cad_cifrado, argv[7]);
            o = fopen(cad_plano, "w");
            i = fopen(cad_cifrado, "r");
        } else if (strcmp(argv[1], "-D") == 0 && strcmp(argv[6], "-o") == 0) {
            strcat(cad_plano, argv[7]);
            strcat(cad_cifrado, "entrada.txt");
            o = fopen(cad_plano, "w");
            printf("Introduce el texto a descifrar:\n");
            i = fopen(cad_cifrado, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_cifrado, "r");
        }
    } else {
        if (strcmp(argv[1], "-C") == 0) {
            salida = TRUE;
            strcat(cad_plano, "entrada.txt");
            strcat(cad_cifrado, "salida.txt");
            o = fopen(cad_cifrado, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_plano, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_plano, "r");
        } else if (strcmp(argv[1], "-D") == 0) {
            salida = TRUE;
            strcat(cad_plano, "salida.txt");
            strcat(cad_cifrado, "entrada.txt");
            o = fopen(cad_plano, "w");
            printf("Introduce el texto a descifrar:\n");
            i = fopen(cad_cifrado, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_cifrado, "r");
        }
        
    }

    /* Si no existe el fichero a abrir, error */
    if (!i) {
        printf("No existe el fichero de entrada.\n");
    } else {
        /* Creamos el objeto afin */
        permutacion = create_permutacion(i, o, argv[3], argv[5]);
    }


    /* Ciframos o desciframos segun el parametro */
    if (permutacion) {
        if (strcmp(argv[1], "-C") == 0) {
            if (cifrar_permutacion(permutacion) == OK && salida == 1) {
                /* Mostrar mensaje cifrado por pantalla */
                fclose(permutacion->o);
                permutacion->o = fopen(cad_cifrado, "r");
                while(fgets(salida_pantalla, 1024, (FILE*) permutacion->o)) {
                    fputs(salida_pantalla, stdout);
                }
                printf("\n");
            }
        } else {
            if (descifrar_permutacion(permutacion) == OK && salida == 1) {
                /* Mostrar mensaje descifrado por pantalla */
                fclose(permutacion->o);
                permutacion->o = fopen(cad_plano, "r");
                while(fgets(salida_pantalla, 1024, (FILE*) permutacion->o)) {
                    fputs(salida_pantalla, stdout);
                }
                printf("\n");
            }
        }
    }


    /* Cerramos recursos */
    free_permutacion(permutacion);


    return 0;
}