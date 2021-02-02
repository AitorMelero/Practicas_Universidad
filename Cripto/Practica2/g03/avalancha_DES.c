/**************************************************************
 * File: avalancha_DES.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 22/11/2020
 * Last_Date: 22/11/2020 
 * Function: Programa que estudia la avalancha en el cifrado DES. 
 * ***********************************************************/

#include "type.h"

#define BLOQUE 8

/**************************************************************
 * Name: muestra_info_parametros()
 * Function: Muestra informacion sobre los parametros a introducir.
 * Parameters:
 *              Ninguno.
 * Return:
 *              Nada.
 * ***********************************************************/
unsigned char* genera_texto() {
    unsigned char* texto = (unsigned char*)calloc(8, sizeof(unsigned char));
    int i;

    for (i=0; i<8; i++) {
        texto[i] = genera_aleatorio(0, 255);
    }

    return texto;

}

/**************************************************************
 * Name: cambia_texto(unsigned char* texto)
 * Function: Cambia un bit en un bloque de texto. 
 * Parameters:
 *              unsigned char* texto: Bloque de texto a cambiar.
 * Return:
 *              unsigned char*: Bloque de texto cambiado.
 * ***********************************************************/
unsigned char* cambia_texto(unsigned char* texto) {
    unsigned char* texto_cambio = NULL;
    int i;
    int bloque, pos_bloque;
    unsigned char num_aleatorio;
    unsigned char bloque_despl;
    
    /* Control de errores */
    if (!texto) {
        printf("ERROR al cambiar el bloque de texto.!!!\n");
        return NULL;
    }

    /* Copiamos el texto original */
    texto_cambio = (unsigned char*)calloc(8, sizeof(unsigned char));
    for (i=0; i<8; i++) {
        texto_cambio[i] = texto[i];
    }

    /* Generamos la posicion a cambiar */
    num_aleatorio = genera_aleatorio(0, 63);

    /* Cambiamos el bit */
    bloque = (int) num_aleatorio / 8;
    pos_bloque = (int) num_aleatorio % 8;
    bloque_despl = 0x80 >> pos_bloque;
    if ((texto[bloque] & bloque_despl) != 0) {
        texto_cambio[bloque] = texto[bloque] & (~bloque_despl);
    } else {
        texto_cambio[bloque] = texto[bloque] ^ bloque_despl;
    }

    return texto_cambio;

}

/**************************************************************
 * Name: cambia_clave(unsigned char* clave)
 * Function: Cambia un bit en un bloque de clave. 
 * Parameters:
 *              unsigned char* clave: Bloque de clave a cambiar.
 * Return:
 *              unsigned char*: Bloque de clave cambiado.
 * ***********************************************************/
unsigned char* cambia_clave(unsigned char* clave) {
    unsigned char* clave_cambio = NULL;
    int i;
    int bloque, pos_bloque;
    unsigned char num_aleatorio;
    unsigned char bloque_despl;
    
    /* Control de errores */
    if (!clave) {
        printf("ERROR al cambiar el bloque de clave.!!!\n");
        return NULL;
    }

    /* Copiamos la clave original */
    clave_cambio = (unsigned char*)calloc(8, sizeof(unsigned char));
    for (i=0; i<8; i++) {
        clave_cambio[i] = clave[i];
    }

    /* Generamos la posicion a cambiar */
    num_aleatorio = genera_aleatorio(0, 63);

    /* Cambiamos el bit */
    bloque = (int) num_aleatorio / 8;
    pos_bloque = (int) num_aleatorio % 8;
    /* Comprobamos si ha tocado el bit de paridad */
    if (pos_bloque == 7) {
        pos_bloque--;
    }
    bloque_despl = 0x80 >> pos_bloque;
    if ((clave[bloque] & bloque_despl) != 0) {
        clave_cambio[bloque] = clave[bloque] & (~bloque_despl);
    } else {
        clave_cambio[bloque] = clave[bloque] ^ bloque_despl;
    }

    /* Cambiamos el bit de paridad */
    clave_cambio[bloque] = clave_cambio[bloque] ^ 0x01;

    return clave_cambio;

}

/**************************************************************
 * Name: cambios_bits(unsigned char* bloque1, unsigned char* bloque2)
 * Function: Cuenta los bits de diferencia entre los dos bloques. 
 * Parameters:
 *              unsigned char* bloque1: Bloque uno.
 *              unsigned char* bloque2: Bloque dos.
 * Return:
 *              int: El numero de bits de diferencia entre los dos bloques.
 * ***********************************************************/
int cambios_bits(unsigned char* bloque1, unsigned char* bloque2) {
    int bits_diferencia;
    int i, j;
    unsigned char bloque1_aux;
    unsigned char bloque1_aux2;
    unsigned char bloque2_aux;
    unsigned char bloque2_aux2;

    /* Control de errores */
    if (!bloque1 || !bloque2) {
        printf("ERROR al comprobar bits de diferencia!!!\n");
        return -1;
    }

    for (i=0, bits_diferencia=0; i<8; i++) {
        bloque1_aux = bloque1[i];
        bloque2_aux = bloque2[i];
        
        for (j=0; j<8; j++) {
            bloque1_aux2 = bloque1_aux >> j;
            bloque1_aux2 = bloque1_aux2 & 0x01;
            bloque2_aux2 = bloque2_aux >> j;
            bloque2_aux2 = bloque2_aux2 & 0x01;
            if (bloque1_aux2 != bloque2_aux2) {
                bits_diferencia++;
            }
        }
    }

    return bits_diferencia;

}

/**************************************************************
 * Name: cifra_avalancha_DES_clave(unsigned char* texto, unsigned char* clave, unsigned char* clave_cambio)
 * Function: Estudia la avalancha producida por el cambio de clave. 
 * Parameters:
 *              unsigned char* texto: Bloque a cifrar.
 *              unsigned char* clave: Clave original del cifrado.
 *              unsigned char* clave_cambio: Clave original modificada por un bit.
 * Return:
 *              Nada.
 * ***********************************************************/
void cifra_avalancha_DES_clave(unsigned char* texto, unsigned char* clave, unsigned char* clave_cambio) {
    int i, j, k;
    unsigned char* ip = NULL;
    unsigned char* ip2 = NULL;
    unsigned char* pc1 = NULL;
    unsigned char* pc1_aux = NULL;
    unsigned char** lcs = NULL;
    unsigned char** lcs_aux = NULL;
    unsigned char* pc2 = NULL;
    unsigned char* pc2_aux = NULL;
    unsigned char* l = NULL;
    unsigned char* l2 = NULL;
    unsigned char* r = NULL;
    unsigned char* r2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    unsigned char* f_aux = NULL;
    unsigned char* f_aux2 = NULL;
    unsigned char* xor_aux = NULL;
    unsigned char* xor_aux2 = NULL;
    unsigned char* bits = NULL; 

    /* Control de errores */
    if (!texto || !clave || !clave_cambio) {
        printf("ERROR al cifrar con DES!!!\n");
        return;
    }

    /* Comprobamos la paridad de la clave */
    if (paridad(clave) == 0) { 
        printf("ERROR, la clave no cumple la paridad impar!!!\n");
        return;
    }

    /* Reservamos memoria */
    bits = (unsigned char*)calloc(8, sizeof(unsigned char));
    l = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    l2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    r = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    r2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    aux = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    aux2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Aplicamos IP */
    ip = funcion_IP(texto);
    ip2 = funcion_IP(texto);

    /* Generamos todas la claves */
    pc1 = funcion_PC1(clave);
    pc1_aux = funcion_PC1(clave_cambio);
    lcs = genera_LCS(pc1);
    lcs_aux = genera_LCS(pc1_aux);

    /* Asignamos las variables auxiliares */
    copiar_cadena(l, ip, BLOQUE/2);
    copiar_cadena(r, &ip[4], BLOQUE/2);
    copiar_cadena(l2, ip2, BLOQUE/2);
    copiar_cadena(r2, &ip2[4], BLOQUE/2);

    /* Vamos permutando lr con la funcion F */
    for (i=0; i<ROUNDS; i++) {
        copiar_cadena(aux, l, BLOQUE/2);
        copiar_cadena(l, r, BLOQUE/2);
        copiar_cadena(aux2, l2, BLOQUE/2);
        copiar_cadena(l2, r2, BLOQUE/2);
        pc2 = funcion_PC2(lcs[i]);
        pc2_aux = funcion_PC2(lcs_aux[i]);
        f_aux = funcion_F(r, pc2);
        f_aux2 = funcion_F(r2, pc2_aux);
        copiar_cadena(r, f_aux, BLOQUE/2);
        copiar_cadena(r2, f_aux2, BLOQUE/2);
        xor_aux = xor(aux, r, BLOQUE/2);
        xor_aux2 = xor(aux2, r2, BLOQUE/2);
        copiar_cadena(r, xor_aux, BLOQUE/2);
        copiar_cadena(r2, xor_aux2, BLOQUE/2);

        /* Imprimimos resultado */
        copiar_cadena(ip, l, BLOQUE/2);
        copiar_cadena(&ip[4], r, BLOQUE/2);
        copiar_cadena(ip2, l2, BLOQUE/2);
        copiar_cadena(&ip2[4], r2, BLOQUE/2);

        printf("\n##############################################\n");
        printf("################ RONDA %d #####################\n", i+1);
        printf("TEXTO ORIGINAL: ");
        for (j=0; j<8; j++) {
           print_binint(ip[j], bits);
           for (k=0; k<8; k++) {
               printf("%c", bits[k]);
           }
           printf(" ");
        }

        printf("\nTEXTO CAMBIADO: ");
        for (j=0; j<8; j++) {
           print_binint(ip2[j], bits);
           for (k=0; k<8; k++) {
               printf("%c", bits[k]);
           }
           printf(" ");
        }

        printf("\n CAMBIAN %d BITS.\n", cambios_bits(ip, ip2));
        printf("##############################################\n");

        /* Liberamos las variables auxiliares */
        free(f_aux);
        free(f_aux2);
        free(xor_aux);
        free(xor_aux2);
        free(pc2);
        free(pc2_aux);
    }

    /* Liberamos la memoria */
    free(ip);
    free(ip2);
    free(pc1);
    free(pc1_aux);
    for (i=0; i<ROUNDS; i++) {
        free(lcs[i]);
        free(lcs_aux[i]);
    }
    free(lcs);
    free(lcs_aux);
    free(l);
    free(l2);
    free(r);
    free(r2);
    free(aux);
    free(aux2);
    free(bits);

}

/**************************************************************
 * Name: cifra_avalancha_DES_texto(unsigned char* texto, unsigned char* texto_cambio, unsigned char* clave)
 * Function: Estudia la avalancha producida por el cambio de bloque a cifrar en el DES. 
 * Parameters:
 *              unsigned char* texto: Bloque original a cifrar.
 *              unsigned char* texto_cambio: Bloque original modificado por un bit.
 *              unsigned char* clave: Clave a usar en el cifrado.
 * Return:
 *              Nada.
 * ***********************************************************/
void cifra_avalancha_DES_texto(unsigned char* texto, unsigned char* texto_cambio, unsigned char* clave) {
    int i, j, k;
    unsigned char* ip = NULL;
    unsigned char* ip2 = NULL;
    unsigned char* pc1 = NULL;
    unsigned char** lcs = NULL;
    unsigned char* pc2 = NULL;
    unsigned char* l = NULL;
    unsigned char* l2 = NULL;
    unsigned char* r = NULL;
    unsigned char* r2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    unsigned char* f_aux = NULL;
    unsigned char* f_aux2 = NULL;
    unsigned char* xor_aux = NULL;
    unsigned char* xor_aux2 = NULL;
    unsigned char* bits = NULL; 

    /* Control de errores */
    if (!texto || !clave) {
        printf("ERROR al cifrar con DES!!!\n");
        return;
    }

    /* Comprobamos la paridad de la clave */
    if (paridad(clave) == 0) { 
        printf("ERROR, la clave no cumple la paridad impar!!!\n");
        return;
    }

    /* Reservamos memoria */
    bits = (unsigned char*)calloc(8, sizeof(unsigned char));
    l = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    l2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    r = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    r2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    aux = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    aux2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Aplicamos IP */
    ip = funcion_IP(texto);
    ip2 = funcion_IP(texto_cambio);

    /* Generamos todas la claves */
    pc1 = funcion_PC1(clave);
    lcs = genera_LCS(pc1);

    /* Asignamos las variables auxiliares */
    copiar_cadena(l, ip, BLOQUE/2);
    copiar_cadena(r, &ip[4], BLOQUE/2);
    copiar_cadena(l2, ip2, BLOQUE/2);
    copiar_cadena(r2, &ip2[4], BLOQUE/2);

    /* Vamos permutando lr con la funcion F */
    for (i=0; i<ROUNDS; i++) {
        copiar_cadena(aux, l, BLOQUE/2);
        copiar_cadena(l, r, BLOQUE/2);
        copiar_cadena(aux2, l2, BLOQUE/2);
        copiar_cadena(l2, r2, BLOQUE/2);
        pc2 = funcion_PC2(lcs[i]);
        f_aux = funcion_F(r, pc2);
        f_aux2 = funcion_F(r2, pc2);
        copiar_cadena(r, f_aux, BLOQUE/2);
        copiar_cadena(r2, f_aux2, BLOQUE/2);
        xor_aux = xor(aux, r, BLOQUE/2);
        xor_aux2 = xor(aux2, r2, BLOQUE/2);
        copiar_cadena(r, xor_aux, BLOQUE/2);
        copiar_cadena(r2, xor_aux2, BLOQUE/2);

        /* Imprimimos resultado */
        copiar_cadena(ip, l, BLOQUE/2);
        copiar_cadena(&ip[4], r, BLOQUE/2);
        copiar_cadena(ip2, l2, BLOQUE/2);
        copiar_cadena(&ip2[4], r2, BLOQUE/2);

        printf("\n##############################################\n");
        printf("################ RONDA %d #####################\n", i+1);
        printf("TEXTO ORIGINAL: ");
        for (j=0; j<8; j++) {
           print_binint(ip[j], bits);
           for (k=0; k<8; k++) {
               printf("%c", bits[k]);
           }
           printf(" ");
        }

        printf("\nTEXTO CAMBIADO: ");
        for (j=0; j<8; j++) {
           print_binint(ip2[j], bits);
           for (k=0; k<8; k++) {
               printf("%c", bits[k]);
           }
           printf(" ");
        }

        printf("\n CAMBIAN %d BITS.\n", cambios_bits(ip, ip2));
        printf("##############################################\n");

        /* Liberamos las variables auxiliares */
        free(f_aux);
        free(f_aux2);
        free(xor_aux);
        free(xor_aux2);
        free(pc2);
    }

    /* Liberamos la memoria */
    free(ip);
    free(ip2);
    free(pc1);
    for (i=0; i<ROUNDS; i++) {
        free(lcs[i]);
    }
    free(lcs);
    free(l);
    free(l2);
    free(r);
    free(r2);
    free(aux);
    free(aux2);
    free(bits);

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
    printf("./avalancha_DES\n");
    printf("################################################\n");
    return;
}


/* Funcion principal */
int main(int argc, char *argv[]) {
    unsigned char* texto = NULL;
    unsigned char* clave = NULL;
    unsigned char* texto_cambio = NULL;
    unsigned char* clave_cambio = NULL;
    unsigned char* bits = NULL;
    int i, j;

    /* Contro de errores */
    if (argc != 1) {
        muestra_info_parametros();
        return -1;
    }

    /* Reservamos memoria */
    bits = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Generamos bloque de texto y de clave aleatorios */
    texto = genera_texto();
    clave = genera_clave();

    /* Imprimimos el texto y la clave generada */
    printf("###################################################\n");
    printf("############ ESTUDIO AVALANCHA ####################\n");
    printf("###################################################\n");
    printf("TEXTO: ");
    for (i=0; i<8; i++) {
        print_binint(texto[i], bits);
        for (j=0; j<8; j++) {
            printf("%c", bits[j]);
        }
        printf(" ");
    }

    printf("\nCLAVE: ");
    for (i=0; i<8; i++) {
        print_binint(clave[i], bits);
        for (j=0; j<8; j++) {
            printf("%c", bits[j]);
        }
        printf(" ");
    }

    /* Cambiamos un bit aleatorio de los dos bloque generados */
    texto_cambio = cambia_texto(texto);
    clave_cambio = cambia_clave(clave);

    printf("\n\nTEXTO CAMBIADO: ");
    for (i=0; i<8; i++) {
        print_binint(texto_cambio[i], bits);
        for (j=0; j<8; j++) {
            printf("%c", bits[j]);
        }
        printf(" ");
    }

    printf("\nCLAVE CAMBIADA: ");
    for (i=0; i<8; i++) {
        print_binint(clave_cambio[i], bits);
        for (j=0; j<8; j++) {
            printf("%c", bits[j]);
        }
        printf(" ");
    }

    printf("\n\nAVALANCHA PRODUCIDA POR EL BLOQUE DE TEXTO\n");

    /* Ciframos con el texto normal y el modificado, mostramos resultados */
    cifra_avalancha_DES_texto(texto, texto_cambio, clave);

    printf("\n\nAVALANCHA PRODUCIDA POR LA CLAVE\n");

    /* Ciframos con la clave normal y la modificada, mostramos resultados */
    cifra_avalancha_DES_clave(texto, clave, clave_cambio);

    /* Liberamos recursos */
    free(texto);
    free(clave);
    free(texto_cambio);
    free(clave_cambio);
    free(bits);

    return 0;

}
