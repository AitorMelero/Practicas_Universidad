/**************************************************************
 * File: main_descifrador.c
 * Author: Aitor Melero, Ana Roa
 * Date: 18/10/2020
 * Last_Date: 19/10/2020
 * Function: Funcion principal para programa descifrador.
 * ***********************************************************/


/* INCLUDES */
#include "descifrador.h"


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
    printf("./descifrador -met <i/k> -l <tam> -i <fichero entrada> -o <fichero salida>\n");
    printf("################################################\n");
    return;
}


/* MAIN */
int main(int argc, char *argv[]) {
    DESCIFRADOR* descifrador = NULL;        /* descifrador vigenere*/
    FILE* i = NULL;                         /* fichero con el texto cifrado */
    FILE* o = NULL;                         /* fichero donde escribir el texto claro */
    BOOL metodo = FALSE;                    /* indica si usar kasiski o ic para longitud de clave */
    char cad_plano[40] = TEXTO_PLANO;       /* cadena que representa un path */
    char cad_cifrado[40] = TEXTO_CIFRADO;   /* cadena que representa un path */
    int l = 0;                              /* tamanio l */

    /* Comprobamos el numero de argumentos */
    if (argc != 9) {
        muestra_info_parametros();
        return 1;
    }

    /* Comprobamos cada argumento */
    if (strcmp(argv[1], "-met")) {
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[2], "i") && strcmp(argv[2], "k")) {
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[3], "-l")) {
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[5], "-i")) {
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[7], "-o")) {
        muestra_info_parametros();
        return 1;
    }

    /* Abrimos ficheros con control de errores */
    strcat(cad_cifrado, argv[6]);
    strcat(cad_plano, argv[8]);

    i = fopen(cad_cifrado, "r");
    if (!i) {
        printf("ERROR al abrir fichero con texto cifrado!!!\n");
        return 1;
    }
    o = fopen(cad_plano, "w");
    if (!i) {
        printf("ERROR al abrir fichero donde escribir texto claro!!!\n");
        fclose(i);
        return 1;
    }

    /* Inicializamos el resto de variables */
    if (strcmp(argv[2], "k") == 0) {
        metodo = TRUE;      /* Usamos kasiski, por defecto no */
    }
    l = atoi(argv[4]);

    /* Creamos el tipo descifrador, automaticamente se descifra el mensaje */
    descifrador = create_descifrador(i, o, metodo, l);

    /* Mostramos por pantalla la longitud de la clave y la clave en caso de hallarse */
    if (descifrador) {
        printf("La longitud de clave es %d\n", descifrador->long_clave);
        printf("La clave es: %s\n", descifrador->clave);

        /* Liberamos el descifrador */
        free_descifrador(descifrador);  
    }
    

    return 0;
}