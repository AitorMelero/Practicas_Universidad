/**************************************************************
 * File: TDEA_CBC.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 16/11/2020
 * Last_Date: 16/11/2020 
 * Function: Programa que cifra o descifra un fichero con triple DES en CBC. 
 * ***********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "type.h"

/* DEFINES */
#define TEXTO_PLANO "Textos_Planos/"
#define TEXTO_CIFRADO "Textos_Cifrados/"



/**************************************************************
 * Name: leer_fichero(FILE* i, int* tam_i)
 * Function: Lee el fichero de entrada, cuenta el numero de bloques del texto 
 *           y devuelve el texto.
 * Parameters:
 *             FILE* i: Fichero de entrada.
 *             int* tam_i: Numero de bloques a leer y calcular. 
 * Return:
 *             unsigned char*: Texto leido del fichero.
 * ***********************************************************/
unsigned char* leer_fichero(FILE* i, int* tam_i) {
    BOOL leer = TRUE;
    unsigned char* texto_plano = NULL;
    int offset_lectura = 0;
    int j=0, k;

    /* Control de errores */
    if (!i) {
        printf("ERROR al leer del fichero!!!\n");
        return 0;
    }

    /* Inicializamos el indice del fichero de entrada */
    fseek(i, offset_lectura, SEEK_SET);


    /* Bucle hasta que leamos el final del fichero de entrada i */
    while (leer == TRUE && !feof(i)) {
        /* Reservamos memoria para texto_plano */
        if (j==0) {
            texto_plano = (unsigned char*)calloc(8, sizeof(unsigned char));
        } else {
            texto_plano = (unsigned char*)realloc(texto_plano, (j*8*2)*sizeof(unsigned char));
        }
        /* Si leemos menos de 8 caracteres es que estamos en el ultimo bloque */
        for (k=0; k<8 && leer == TRUE; k++) {
            if (fread(&texto_plano[j*8+k], sizeof(unsigned char), 1, i) != 1) {
                leer = FALSE;
            }

            /* Vamos modificando los indices */
            offset_lectura++;
            fseek(i, offset_lectura, SEEK_SET);

        }

        if (k != 8) {
            k--;
            while (k < 8) {
                texto_plano[j*8+k] = 0;
                k++;
            }
        }

        j++;
    }

    *(tam_i) = j;

    return texto_plano;

}

/**************************************************************
 * Name: escribir_fichero(FILE* o, unsigned char* bloque_texto, int num_i, BOOL descifra)
 * Function: Guardamos el texto cifrado o descifrado, segun lo indicado, y lo escribimos
 *           en el fichero.
 * Parameters:
 *             FILE* o: Fichero de salida.
 *             unsigned char* bloque_texto: Texto a escribir. 
 *             int num_i: Numero de bloques de 8 bits a escribir.
 *             BOOL descifra: Indica si se escribe un texto cifrado o descifrado.
 * Return:
 *             void: Nada.
 * ***********************************************************/
void escribir_fichero(FILE* o, unsigned char* bloque_texto, int num_i, BOOL descifra) {
    BOOL escribir = TRUE;
    int offset_escritura = 0;
    int k, j;

    /* Control de errores */
    if (!o || !bloque_texto) {
        printf("ERROR al escribir del fichero!!!\n");
        return;
    }

    /* Inicializamos el indice del fichero */
    fseek(o, offset_escritura, SEEK_SET);

    /* Vamos escribiendo el texto cifrado */
    for (j=0; j<num_i; j++) {
        /* Si leemos menos de 8 caracteres es que estamos en el ultimo bloque */
        for (k=0; k<8 && (escribir == TRUE); k++) {
            /*Buscamos el final de fichero para dejar de escribir */
            if (bloque_texto[(j*8)+k] == 0 && descifra == TRUE) {
                escribir = FALSE;
            } else {
                if (fwrite(&bloque_texto[(j*8)+k], sizeof(unsigned char), 1, o) != 1) {
                    escribir = FALSE;
                }
            }

            /* Vamos modificando los indices */
            offset_escritura++;
            fseek(o, offset_escritura, SEEK_SET);

        }
    }

}


/**************************************************************
 * Name: convierte_clave(unsigned char* clave_cadena)
 * Function: Convierte una cadena en la clave. 
 * Parameters:
 *              unsigned char* clave_cadena: Clave a generar. 
 * Return:
 *              unsigned char*: Clave generada.
 * ***********************************************************/
unsigned char* convierte_clave(unsigned char* clave_cadena) {
    int i, j;
    unsigned char clave_aux = 0;
    unsigned char* clave = (unsigned char*)calloc(24, sizeof(unsigned char));

    /* Vamos generando la clave */
    for (i=0, j=0; i<48; i=i+2, j++) {
        /* Primera letra del bloque */
        clave_aux = clave_cadena[i];
        if (clave_aux >=48 && clave_aux<=57) {
            clave_aux = clave_aux - 48;
            clave[j] = clave_aux << 4;
        } else {
            clave_aux = clave_aux - 55;
            clave[j] = clave_aux << 4;
        }
        /* Segunda letra del bloque */
        clave_aux = clave_cadena[i+1];
        if (clave_aux >=48 && clave_aux<=57) {
            clave_aux = clave_aux - 48;
            clave[j] = clave[j] ^ clave_aux;
        } else {
            clave_aux = clave_aux - 55;
            clave[j] = clave[j] ^ clave_aux;
        }
    }

    return clave;
}


/**************************************************************
 * Name: imprime_clave(unsigned char* clave)
 * Function: Imprime la clave en hexadecimal. 
 * Parameters:
 *              unsigned char* clave: Clave a imprimir en hexadecimal.
 * Return:
 *              Nada.
 * ***********************************************************/
void imprime_clave(unsigned char* clave) {
    int i;
    unsigned char letra = 0;
    int num = 0;

    printf("###########################################################\n");
    printf("CLAVE DE CIFRADO: ");
    /* Vamos imprimienndo uno a uno cada caracter */
    for (i=0; i<24; i++) {
        /* Primera letra del bloque */
        num = clave[i] >> 4;
        if (num < 10) {
            letra = num + 48;
        } else {
            letra = num + 55;
        }
        printf("%c", letra);
        /* Segunda letra del bloque */
        num = clave[i] & 0x0f;
        if (num < 10) {
            letra = num + 48;
        } else {
            letra = num + 55;
        }
        printf("%c", letra);
    }
    printf("\n###########################################################\n");

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
    printf("./TDEA-CBC -C/D -k <clave> -iv <iv> -i <fichero entrada> -o <fichero salida>\n");
    printf("################################################\n");
    return;
}


/* Funcion principal */
int main(int argc, char *argv[]) {
    FILE* i = NULL;
    FILE* o = NULL;
    int j;
    BOOL descifra = TRUE;
    unsigned char* clave = NULL;
    unsigned char* iv = NULL;
    unsigned char* clave_aux = NULL;
    unsigned char* iv_aux = NULL;
    int tam_i = 0;
    unsigned char* texto_cifrado = NULL;
    unsigned char* texto_plano = NULL;
    char cad_plano[40] = TEXTO_PLANO;
    char cad_cifrado[40] = TEXTO_CIFRADO;

    /* Control de errores */
    if (argc != 10 && argc != 8) {
       muestra_info_parametros();
       return -1;
    }

    if ((strcmp("-C", argv[1]) == 0 && argc != 8) || (strcmp("-D", argv[1]) == 0 && argc != 10)) {
        muestra_info_parametros();
        return -1;
    }

    if ((strcmp("-k", argv[2]) != 0 || strcmp(argv[1], "-D") != 0) && argc == 10 ) {
        muestra_info_parametros();
        return -1;
    }

    if (strcmp("-iv", argv[4]) != 0 && argc == 10) {
        muestra_info_parametros();
        return -1;
    }

    if (strcmp("-iv", argv[2]) != 0 && argc == 8) {
        muestra_info_parametros();
        return -1;
    }

    if (strcmp("-i", argv[4]) != 0 && argc == 8) {
        muestra_info_parametros();
        return -1;
    } else if (argc == 10) {
        if (strcmp("-i", argv[6]) != 0) {
            muestra_info_parametros();
            return -1;
        }
    }

    if (strcmp("-o", argv[6]) != 0 && argc == 8) {
        muestra_info_parametros();
        return -1;
    } else if (argc == 10){
        if (strcmp("-o", argv[8]) != 0) {
            muestra_info_parametros();
            return -1;
        }
    }

    /* Comprobamos la longitud de 64 bits de la clave e iv */
    if (argc == 10) {
        if (strlen(argv[3]) != 48 || strlen(argv[5]) != 8) {
            printf("La clave debe ser de 24 cifras (192 bits) y el iv debe ser de 8 cifras (64 bits)!!!\n");
            return -1;
        }
    } else {
        if (strlen(argv[3]) != 8) {
            printf("El iv debe ser de 8 cifras (64 bits)!!!\n");
            return -1;
        }
    }

    /* Asignamos variables */
    if (strcmp("-C", argv[1]) == 0) {
        descifra = FALSE;
        /* Damos el nombre de los ficheros */
        strcat(cad_cifrado, argv[7]);
        strcat(cad_plano, argv[5]);   
        clave = (unsigned char*)calloc(24, sizeof(unsigned char));
        for (j=0; j<3; j++) {
            clave_aux = genera_clave();
            copiar_cadena(&clave[j*8], clave_aux, 8);
            free(clave_aux);
        }
        iv = (unsigned char*)calloc(8, sizeof(unsigned char));
        iv_aux = (unsigned char*)calloc(9, sizeof(unsigned char));
        strcpy((char*)iv_aux, argv[3]);
        copiar_cadena(iv, iv_aux, 8);
    } else {
        descifra = TRUE;
        /* Damos el nombre de los ficheros */
        strcat(cad_cifrado, argv[7]);
        strcat(cad_plano, argv[9]);   
        clave_aux = (unsigned char*)calloc(49, sizeof(unsigned char));
        strcpy((char*)clave_aux, argv[3]);
        clave = convierte_clave(clave_aux);
        iv = (unsigned char*)calloc(8, sizeof(unsigned char));
        iv_aux = (unsigned char*)calloc(9, sizeof(unsigned char));
        strcpy((char*)iv_aux, argv[5]);
        copiar_cadena(iv, iv_aux, 8);
        free(clave_aux);
    }

    /* Ciframos o desciframos */
    if (descifra == FALSE) {
        i = fopen(cad_plano, "rb");
        o = fopen(cad_cifrado, "wb");
        if (!i || !o) {
            printf("ERROR al abrir fichero!!!");
            return -1;
        }
        texto_plano = leer_fichero(i, &tam_i);
        texto_cifrado = cifrar_TDEA_CBC(clave, iv, texto_plano, tam_i);
        imprime_clave(clave);
        if (texto_cifrado == NULL) {
            printf("ERROR al cifrar el fichero!!!\n");
            return -1;
        }
        escribir_fichero(o, texto_cifrado, tam_i, descifra);
    } else {
        i = fopen(cad_cifrado, "rb");
        o = fopen(cad_plano, "wb");
        if (!i || !o) {
            printf("ERROR al abrir fichero!!!");
            return -1;
        }
        texto_cifrado = leer_fichero(i, &tam_i);
        texto_plano = descifrar_TDEA_CBC(clave, iv, texto_cifrado, tam_i);
        if (texto_plano == NULL) {
            printf("ERROR al descifrar el fichero!!!\n");
            return -1;
        }
        escribir_fichero(o, texto_plano, tam_i, descifra);
    }

    /* Liberamos recursos */
    free(texto_plano);
    free(texto_cifrado);
    free(clave);
    free(iv);
    free(iv_aux);
    fclose(i);
    fclose(o);

    return 0;

}
