/**************************************************************
 * File: main.c
 * Author: Aitor Melero, Ana Roa
 * Date: 03/10/2020
 * Last_Date: 08/10/2020
 * Function: Funcion principal para programa flujo.
 * ***********************************************************/


/* INCLUDES */
#include "flujo.h"


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
    printf("################################################\n\n");
    printf("Se debe introducir la siguiente secuencia: \n");
    printf("./flujo -C/-D -m <tamanio> -a <coeficiente> -b <constante> -i <fichero entrada> -o <fichero salida>\n");
    printf("\nSiendo -i y -o opcionales.\n");
    printf("################################################\n");
    return;
}

/* MAIN */
int main(int argc, char *argv[]) {
    mpz_t m;                                /* tamanio del espacio de texto cifrado */
    mpz_t a;                                /* coeficiente multiplicativo de la funcion flujo */
    mpz_t b;                                /* termino constante de la funcion flujo */
    FILE* i = NULL;                         /* fichero de entrada */
    FILE* o = NULL;                         /* fichero de salida */
    char cad_plano[40] = TEXTO_PLANO;       /* cadena que representa un path */
    char cad_cifrado[40] = TEXTO_CIFRADO;   /* cadena que representa un path */
    FLUJO* flujo = NULL;                      /* objeto flujo */
    BOOL salida = FALSE;                    /* indica si se usa salida estandar o no */
    char texto[1024] = "";                  /* texto a usar en caso de entrada estandar */
    char salida_pantalla[1024];             /* texto a mostrar en la pantalla en caso de pedirlo */


    /* Control de errores */
    if (argc != 8 && argc != 10 && argc != 12) {
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[1], "-C") != 0 && strcmp(argv[1], "-D") != 0) {
        printf("Primer argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[2], "-m") != 0) {
        printf("Segundo argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[4], "-a") != 0) {
        printf("Tercer argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[6], "-b") != 0) {
        printf("Cuarto argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (argc == 12) {
        if (strcmp(argv[8], "-i") != 0) {
            printf("Quinto argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        } else if (strcmp(argv[10], "-o") != 0) {
            printf("Sexto argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        }
    } else if (argc == 10) {
        if (strcmp(argv[8], "-i") != 0 && strcmp(argv[8], "-o") != 0) {
            printf("Quinto argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        }
    }

    /* Asignacion de variables de GMP*/
    mpz_init(m);
    mpz_set_str(m, argv[3], 10);
    mpz_init(a);
    mpz_set_str(a, argv[5], 10);
    mpz_init(b);
    mpz_set_str(b, argv[7], 10);

    /* Abrimos los ficheros en funcion de los argumentos */
    if (argc == 12) {
        if (strcmp(argv[1], "-C") == 0) {
            strcat(cad_plano, argv[9]);
            strcat(cad_cifrado, argv[11]);
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-D") == 0) {
            strcat(cad_cifrado, argv[9]);
            strcat(cad_plano, argv[11]);
            i = fopen(cad_cifrado, "r");
            o = fopen(cad_plano, "w");
        }
    } else if (argc == 10) {
        if (strcmp(argv[1], "-C") == 0 && strcmp(argv[8], "-i") == 0) {
            salida = TRUE;
            strcat(cad_plano, argv[9]);
            strcat(cad_cifrado, "salida.txt");
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-C") == 0 && strcmp(argv[8], "-o") == 0) {
            strcat(cad_plano, "entrada.txt");
            strcat(cad_cifrado, argv[9]);
            o = fopen(cad_cifrado, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_plano, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_plano, "r");
        } else if (strcmp(argv[1], "-D") == 0 && strcmp(argv[8], "-i") == 0) {
            salida = TRUE;
            strcat(cad_plano, "salida.txt");
            strcat(cad_cifrado, argv[9]);
            o = fopen(cad_plano, "w");
            i = fopen(cad_cifrado, "r");
        } else if (strcmp(argv[1], "-D") == 0 && strcmp(argv[8], "-o") == 0) {
            strcat(cad_plano, argv[9]);
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
    /* El parametro m debe tener el tamanio del alfabeto, 26 */
    if (!i) {
        printf("No existe el fichero de entrada.\n");
    } else if (mpz_cmp_si(m, 26) == 0 && mpz_cmp(m, a) > 0) {
        /* Creamos el objeto flujo */
        flujo = create_flujo(m, a, b, i, o);
    } else {
        printf("Valor m diferente que el tamanio del alfabeto o a > m.\n");
    }

    /* Ciframos o desciframos segun el parametro */
    if (flujo) {
        if (strcmp(argv[1], "-C") == 0) {
            if (cifrar(flujo) == OK && salida == 1) {
                /* Mostrar mensaje cifrado por pantalla */
                fclose(flujo->o);
                flujo->o = fopen(cad_cifrado, "r");
                while(fgets(salida_pantalla, 1024, (FILE*) flujo->o)) {
                    fputs(salida_pantalla, stdout);
                }
                printf("\n");
            }
        } else {
            if (descifrar(flujo) == OK && salida == 1) {
                /* Mostrar mensaje descifrado por pantalla */
                fclose(flujo->o);
                flujo->o = fopen(cad_plano, "r");
                while(fgets(salida_pantalla, 1024, (FILE*) flujo->o)) {
                    fputs(salida_pantalla, stdout);
                }
                printf("\n");
            }
        }
    }


    /* Cerramos recursos */
    free_flujo(flujo);
    mpz_clear(m);
    mpz_clear(a);
    mpz_clear(b);


    return 0;
}