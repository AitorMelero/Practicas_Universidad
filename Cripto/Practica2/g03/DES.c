/**************************************************************
 * File: DES.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 05/11/2020
 * Last_Date: 16/11/2020 
 * Function: Implementacion de funcionalidad del algoritmo de cifrado
 *           DES. 
 * ***********************************************************/

#include "DES.h"

#define BLOQUE 8 /* Cada tipo char son 8 bits */

/**************************************************************
 * Name: funcion_PC1(unsigned char* k)
 * Function: Realiza la PC1 para la clave. 
 * Parameters:
 *             unsigned char* k: Clave.
 * Return:
 *             unsigned char*: Clave de 56 bits. 
 * ***********************************************************/
unsigned char* funcion_PC1(unsigned char* k) {
    unsigned char* c0d0 = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[8] = {128, 0, 0, 0, 0, 0, 0};

    /* Control de errores */
    if (!k) {
        printf("ERROR en la funcion PC1!!!\n");
        return NULL;
    }

    /* Reservamos memoria para c0d0 */
    c0d0 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    for (i=0; i<BLOQUE-1; i++) {
        c0d0[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_PC1; i++) {
        pos = PC1[i];
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, k, BLOQUE);
        copiar_cadena(copia2, ind_pos, BLOQUE);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
           for (j=0; j<pos_copia; j++) {
               aux = desplazamiento_izq(copia, 1, BLOQUE);
               copiar_cadena(copia, aux, BLOQUE);
               free(aux);
           }
           for (j=0; j<i; j++) {
               aux2 = desplazamiento_der(copia2, 1, BLOQUE-1);
               copiar_cadena(copia2, aux2, BLOQUE-1);
               free(aux2);
           }

           aux = and(copia, copia2, BLOQUE);

        } else {
            pos_copia = abs(pos_copia);

            for (j=0; j<pos_copia; j++) {
               aux = desplazamiento_der(copia, 1, BLOQUE);
               copiar_cadena(copia, aux, BLOQUE);
               free(aux);
            }
            for (j=0; j<i; j++) {
               aux2 = desplazamiento_der(copia2, 1, BLOQUE-1);
               copiar_cadena(copia2, aux2, BLOQUE-1);
               free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);
            
        }
        /* Libermos para modificar despues */
        free(copia);

        copia = xor(c0d0, aux, BLOQUE);
        copiar_cadena(c0d0, copia, BLOQUE-1);
        free(aux);

    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return c0d0;
}

/**************************************************************
 * Name: funcion_LCS(unsigned char* cd, int despl_ronda)
 * Function: Realiza el desplazamiento de las claves. 
 * Parameters:
 *             unsigned char* cd: CD generado en ronda anterior.
 *             int despl_ronda: Cantidad de desplazamiento segun la ronda.
 * Return:
 *             unsigned char*: Clave de 56 bits despues del desplazamiento. 
 * ***********************************************************/
unsigned char* funcion_LCS(unsigned char* cd, int despl_ronda) {
    unsigned char* cndn = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[8] = {128, 0, 0, 0, 0, 0, 0};

    /* Control de errores */
    if (!cd) {
        printf("ERROR en la funcion LCS!!!\n");
        return NULL;
    }

    /* Reservamos memoria para cndn */
    cndn = (unsigned char*)calloc(BLOQUE-1, sizeof(unsigned char));
    for (i=0; i<BLOQUE-1; i++) {
        cndn[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_LCS; i++) {
        /* El desplazamiento cambia segun la ronda */
        if (despl_ronda == 1) {
            pos = LCS1[i];
        } else {
            pos = LCS2[i];
        }
        
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, cd, BLOQUE-1);
        copiar_cadena(copia2, ind_pos, BLOQUE-1);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_izq(copia, 1, BLOQUE-1);
                copiar_cadena(copia, aux, BLOQUE-1);
                free(aux);
             }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE-1);
                copiar_cadena(copia2, aux2, BLOQUE-1);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE-1);

        } else {
            pos_copia = abs(pos_copia);

            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_der(copia, 1, BLOQUE-1);
                copiar_cadena(copia, aux, BLOQUE-1);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE-1);
                copiar_cadena(copia2, aux2, BLOQUE-1);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE-1);
        }

        /* Libermos para modificar despues */
        free(copia);

        copia = xor(cndn, aux, BLOQUE-1);
        copiar_cadena(cndn, copia, BLOQUE-1);
        free(aux);

    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return cndn;

}

/**************************************************************
 * Name: genera_LCS(unsigned char* cndn)
 * Function: Genera los desplazamientos LCS para las 16 rondas. 
 * Parameters:
 *             unsigned char* cndn: CnDn.
 * Return:
 *             unsigned char**: Lista con todos los desplazamientos LCS. 
 * ***********************************************************/
unsigned char** genera_LCS(unsigned char* cndn) {
    unsigned char** lista_cndn = NULL;
    unsigned char* aux = NULL;
    int despl;
    int i;

    /* Control de errores */
    if (!cndn) {
        printf("ERROR al generar la lista de LCS!!!\n");
        return NULL;
    }

    /* Reservamos memoria para la lista */
    lista_cndn = (unsigned char**)calloc(ROUNDS, sizeof(unsigned char*));
    for (i=0; i<ROUNDS; i++) {
        lista_cndn[i] = (unsigned char*)calloc(BLOQUE-1, sizeof(unsigned char));
    }

    for (i=0; i<ROUNDS; i++) {
        despl = ROUND_SHIFTS[i];
        if (i == 0) {
            aux = funcion_LCS(cndn, despl);
            copiar_cadena(lista_cndn[i], aux, BLOQUE-1);
            free(aux);
        } else {
            aux = funcion_LCS(lista_cndn[i-1], despl);
            copiar_cadena(lista_cndn[i], aux, BLOQUE-1);
            free(aux);
        }
    }

    return lista_cndn;
}

/**************************************************************
 * Name: funcion_PC2(unsigned char* cndn)
 * Function: Realiza la PC2 para la clave. 
 * Parameters:
 *             unsigned char* cndn: CnDn.
 * Return:
 *             unsigned char*: Kn. 
 * ***********************************************************/
unsigned char* funcion_PC2(unsigned char* cndn) {
    unsigned char* kn = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[8] = {128, 0, 0, 0, 0, 0};

    /* Control de errores */
    if (!cndn) {
        printf("ERROR en la funcion PC2!!!\n");
        return NULL;
    }

    /* Reservamos memoria para kn */
    kn = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    for (i=0; i<BLOQUE-2; i++) {
        kn[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_PC2; i++) {
        pos = PC2[i];
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, cndn, BLOQUE-1);
        copiar_cadena(copia2, ind_pos, BLOQUE-2);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_izq(copia, 1, BLOQUE-1);
                copiar_cadena(copia, aux, BLOQUE-1);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE-2);
                copiar_cadena(copia2, aux2, BLOQUE-2);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE-2);

        } else {
            pos_copia = abs(pos_copia);

            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_der(copia, 1, BLOQUE-1);
                copiar_cadena(copia, aux, BLOQUE-1);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE-2);
                copiar_cadena(copia2, aux2, BLOQUE-2);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE-2);

        }

        /* Libermos para modificar despues */
        free(copia);

        copia = xor(kn, aux, BLOQUE-2);
        copiar_cadena(kn, copia, BLOQUE-2);

        /* Libermos para modificar despues */
        free(aux);
        free(copia);
        copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return kn;
}

/**************************************************************
 * Name: generar_LR(unsigned char* texto_plano)
 * Function: Genera el bloque de texto plano de 64 bits justos. 
 * Parameters:
 *             unsigned char* texto_plano: Bloque a cifrar.
 * Return:
 *             unsigned char*: Texto plano de 64 bits. 
 * ***********************************************************/
unsigned char* generar_LR(unsigned char* texto_plano) {
    unsigned char* lr = NULL;
    int i, j;

    /* Control de errores */
    if (!texto_plano) {
        printf("ERROR al generar bloque LR de 64 bits!!!\n");
        return NULL;
    }

    /* Reservamos memoria para el bloque lr */
    lr = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Vamos asignando los bits a la nueva varible y si faltan añadimos 0 */
    for (i=0, j=0; i<BLOQUE; i++) {
        /* Comprobamos si hemos llegado al final del texto plano */
        if (texto_plano[j] == 0) {
            lr[i] = 0;
        } else {
            lr[i] = texto_plano[j];
            j++;
        }
    }

    return lr;
}

/**************************************************************
 * Name: funcion_IP(unsigned char* lr)
 * Function: Realiza la permutacion inicial IP. 
 * Parameters:
 *             unsigned char* lr: Bloque inicial de 64 bits a permutar.
 * Return:
 *             unsigned char*: Bloque permutado. 
 * ***********************************************************/
unsigned char* funcion_IP(unsigned char* lr) {
    unsigned char* l0r0 = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[8] = {128, 0, 0, 0, 0, 0, 0, 0};

    /* Control de errores */
    if (!lr) {
        printf("ERROR en la funcion IP!!!\n");
        return NULL;
    }

    /* Reservamos memoria para l0r0 */
    l0r0 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    for (i=0; i<BLOQUE; i++) {
        l0r0[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_IP; i++) {
        pos = IP[i];
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, lr, BLOQUE);
        copiar_cadena(copia2, ind_pos, BLOQUE);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_izq(copia, 1, BLOQUE);
                copiar_cadena(copia, aux, BLOQUE);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE);
                copiar_cadena(copia2, aux2, BLOQUE);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);

        } else {
            pos_copia = abs(pos_copia);

            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_der(copia, 1, BLOQUE);
                copiar_cadena(copia, aux, BLOQUE);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE);
                copiar_cadena(copia2, aux2, BLOQUE);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);
        }

        /* Libermos para modificar despues */
        free(copia);

        copia = xor(l0r0, aux, BLOQUE);
        copiar_cadena(l0r0, copia, BLOQUE);
        free(aux);

    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return l0r0;
}

/**************************************************************
 * Name: funcion_E(unsigned char* rn)
 * Function: Realiza la sustitucion de E en DES. 
 * Parameters:
 *             unsigned char* rn: Bloque de 32 bits a ampliar a 48.
 * Return:
 *             unsigned char*: Bloque de 48 bits generado. 
 * ***********************************************************/
unsigned char* funcion_E(unsigned char* rn) {
    unsigned char* e = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[6] = {128, 0, 0, 0, 0, 0};

    /* Control de errores */
    if (!rn) {
        printf("ERROR en la funcion E!!!\n");
        return NULL;
    }

    /* Reservamos memoria para e */
    e = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    for (i=0; i<BLOQUE; i++) {
        e[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_E; i++) {
        pos = E[i];
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, rn, BLOQUE/2);
        for (j=4; j<6; j++) {
            copia[j] = 0;
        }
        copiar_cadena(copia2, ind_pos, BLOQUE-2);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_izq(copia, 1, BLOQUE-2);
                copiar_cadena(copia, aux, BLOQUE-2);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE-2);
                copiar_cadena(copia2, aux2, BLOQUE-2);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE-2);

        } else {
            pos_copia = abs(pos_copia);
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_der(copia, 1, BLOQUE-2);
                copiar_cadena(copia, aux, BLOQUE-2);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE-2);
                copiar_cadena(copia2, aux2, BLOQUE-2);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);
            
        }

        /* Libermos para modificar despues */
        free(copia);

        copia = xor(e, aux, BLOQUE-2);
        copiar_cadena(e, copia, BLOQUE-2);
        
        /* Libermos para modificar despues */
        free(aux);
        free(copia);
        copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return e;
}

/**************************************************************
 * Name: sacar_fila(unsigned char bloque)
 * Function: Genera el numero de fila de la caja S. 
 * Parameters:
 *             unsigned char bloque: Bloque donde mirar el numero de fila. 
 * Return:
 *            int: Numero de fila a mirar. 
 * ***********************************************************/
int sacar_fila(unsigned char bloque) {
    unsigned char aux = 0x84;
    int fila;

    /* Sacamos la fila */
    aux = bloque & aux;
    if (aux == 0x84) {
        fila = 3;
    } else if (aux == 0x80) {
        fila = 2;
    } else if (aux == 0x04) {
        fila = 1;
    } else {
        fila = 0;
    }

    return fila;
}

/**************************************************************
 * Name: sacar_columna(unsigned char bloque)
 * Function: Genera el numero de columna de la caja S. 
 * Parameters:
 *             unsigned char bloque: Bloque donde mirar el numero de columna. 
 * Return:
 *            int: Numero de columna a mirar. 
 * ***********************************************************/
int sacar_columna(unsigned char bloque) {
    int columna;

    /* Sacamos el numero de la columan */
    columna = 0x0F & (bloque >> 3);

    return columna;
}

/**************************************************************
 * Name: genera_S(int num_caja, int fila, int columna)
 * Function: Guardamos el contenido de la caja S. 
 * Parameters:
 *             int num_caja: Numero de caja.
 *             int fila: La fila donde esta el contenido.
 *             int columna: La columna donde esta el contenido. 
 * Return:
 *             unsigned char: El contenido de la caja S.
 * ***********************************************************/
unsigned char genera_S(int num_caja, int fila, int columna) {
    unsigned char contenido;

    /* Control de errores */
    if (fila < 0 || fila > 3 || columna < 0 || columna > 15) {
        printf("ERROR al generar el contenido de S!!!\n");
        return 50;
    }

    contenido = S_BOXES[num_caja][fila][columna];

    return contenido;
}

/**************************************************************
 * Name: bloque_S(unsigned char* bloque_s, unsigned char s_aux, int num_caja)
 * Function: Vamos generando el bloque de las cajas S. 
 * Parameters:
 *             unsigned char* bloque_s: Cadena donde guardar el resultado.
 *             unsigned char s_aux: Contenido de la caja s numero num_caja.
 *             int num_caja: Numero de la caja s de donde es el contenido s_aux. 
 * Return:
 *            void: Nada 
 * ***********************************************************/
void bloque_S(unsigned char* bloque_s, unsigned char s_aux, int num_caja) {

    /* Comprobamos si hace falta hacer el desplazamiento */
    if (num_caja%2 == 0) {
        s_aux = s_aux << 4;
    }

    /* Vamos guardando el contenido de la caja S */
    if (num_caja == 0 || num_caja == 1) {
        bloque_s[0] = bloque_s[0] ^ s_aux;
    } else if (num_caja == 2 || num_caja == 3) {
        bloque_s[1] = bloque_s[1] ^ s_aux;
    } else if (num_caja == 4 || num_caja == 5) {
        bloque_s[2] = bloque_s[2] ^ s_aux;
    } else {
        bloque_s[3] = bloque_s[3] ^ s_aux;
    }
 
}

/**************************************************************
 * Name: funcion_S(unsigned char* ek)
 * Function: Genera el bloque de 32 bits a partir de 48 bits con
 *           las cajas S. 
 * Parameters:
 *             unsigned char* ek: Bloque de 48 bits a reducir. 
 * Return:
 *             unsigned char*: Bloque de 32 bits generado. 
 * ***********************************************************/
unsigned char* funcion_S(unsigned char* ek) {
    int i,j;
    unsigned char* copia = NULL;
    int fila;
    int columna;
    unsigned char* s_aux = NULL;
    unsigned char* s = NULL;
    unsigned char* aux = NULL;

    /* Control de errores */
    if (!ek) {
        printf("ERROR en la funcion S!!!\n");
        return NULL;
    }

    /* Reservamos memoria */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    s_aux = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    s = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Inicializamos el valor de s */
    for (i=0; i<(BLOQUE/2); i++) {
        s[i] = 0;
    }

    /* Nos desplazamos a la izquierda para trabajar con cada bloque de 6 bits */
    for (i=0; i<BLOQUE; i++) {
        /* Desplazamos hacia la izquierda en multiplos de 6 */
        copiar_cadena(copia, ek, BLOQUE-2);
        /*copiar_cadena(copia, desplazamiento_izq(copia, 6, BLOQUE-2), BLOQUE-2);*/
        for (j=0; j<(6*i); j++) {
            aux = desplazamiento_izq(copia, 1, BLOQUE-2);
            copiar_cadena(copia, aux, BLOQUE-2);
            free(aux);
        }
        /* Sacamos la fila */
        fila = sacar_fila(copia[0]);
        /* Sacamos la columna */
        columna = sacar_columna(copia[0]);
        /* Sacamos el valor de cada caja S */
        s_aux[i] = genera_S(i, fila, columna);
        /* Juntamos los valores de las cajas S */
        bloque_S(s, s_aux[i], i);
    }

    /* Liberamos recursos */
    free(copia);
    free(s_aux);

    return s;
}

/**************************************************************
 * Name: funcion_P(unsigned char* s)
 * Function: Realiza la permutacion de P en DES. 
 * Parameters:
 *             unsigned char* rn: Bloque de 32 bits a permutar.
 * Return:
 *             unsigned char*: Bloque de 32 bits generado. 
 * ***********************************************************/
unsigned char* funcion_P(unsigned char* s) {
    unsigned char* p = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[6] = {128, 0, 0, 0};

    /* Control de errores */
    if (!s) {
        printf("ERROR en la funcion E!!!\n");
        return NULL;
    }

    /* Reservamos memoria para p */
    p = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    for (i=0; i<BLOQUE; i++) {
        p[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_P; i++) {
        pos = P[i];
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, s, BLOQUE/2);
        for (j=4; j<6; j++) {
            copia[j] = 0;
        }
        copiar_cadena(copia2, ind_pos, BLOQUE/2);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_izq(copia, 1, BLOQUE/2);
                copiar_cadena(copia, aux, BLOQUE/2);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE/2);
                copiar_cadena(copia2, aux2, BLOQUE/2);
                free(aux2);
            }
            
            aux = and(copia, copia2, BLOQUE);

        } else {
            pos_copia = abs(pos_copia);
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_der(copia, 1, BLOQUE/2);
                copiar_cadena(copia, aux, BLOQUE/2);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE/2);
                copiar_cadena(copia2, aux2, BLOQUE/2);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);
        }

        /* Libermos para modificar despues */
        free(copia);

        copia = xor(p, aux, BLOQUE-2);
        copiar_cadena(p, copia, BLOQUE-2);
        
        /* Libermos para modificar despues */
        free(aux);
        free(copia);
        copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return p;
}

/**************************************************************
 * Name: funcion_swap(unsigned char* lr)
 * Function: Realiza el swap final de 32 y 32. 
 * Parameters:
 *             unsigned char* lr: Bloque sobre el que realizar el swap. 
 * Return:
 *             unsigned char*: Bloque con el swap. 
 * ***********************************************************/
unsigned char* funcion_swap(unsigned char* lr) {
    int i;
    unsigned char* sw = NULL;
    unsigned char* aux = NULL;

    /* Control de errores */
    if (!lr) {
        printf("ERROR al realizar el swap!!!\n");
        return NULL;
    }

    /* Reservamos memoria para el swap y copiamos lr */
    sw = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copiar_cadena(sw, lr, BLOQUE);

    /* Realizamos el swap con un desplazamiento */
    for (i=0; i<32; i++) {
        aux = desplazamiento_der(sw, 1, BLOQUE);
        copiar_cadena(sw, aux, BLOQUE);
        free(aux);
    }

    return sw;
}

/**************************************************************
 * Name: funcion_IP_inv(unsigned char* lr)
 * Function: Realiza la permutacion final IP inversa. 
 * Parameters:
 *             unsigned char* lr: Bloque inicial de 64 bits a permutar.
 * Return:
 *             unsigned char*: Bloque permutado. 
 * ***********************************************************/
unsigned char* funcion_IP_inv(unsigned char* lr) {
    unsigned char* ip_inv = NULL;
    int i, j;
    unsigned short pos;
    unsigned char* copia = NULL;
    unsigned char* copia2 = NULL;
    unsigned char* aux = NULL;
    unsigned char* aux2 = NULL;
    int pos_copia;
    unsigned char ind_pos[8] = {128, 0, 0, 0, 0, 0, 0, 0};

    /* Control de errores */
    if (!lr) {
        printf("ERROR en la funcion IP inversa!!!\n");
        return NULL;
    }

    /* Reservamos memoria para ip_inv */
    ip_inv = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    for (i=0; i<BLOQUE; i++) {
        ip_inv[i] = 0;
    }

    /* Reservamos y copiamos en copia */
    copia = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    copia2 = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Realizamos la permutacion */
    for (i=0; i<BITS_IN_IP; i++) {
        pos = IP_INV[i];
        pos_copia = pos - (i+1);
        
        /* Copiamos en las variables auxiliares */
        copiar_cadena(copia, lr, BLOQUE);
        copiar_cadena(copia2, ind_pos, BLOQUE);

        /* Vamos sustituyendo cada bit usando mascaras */
        if (pos_copia >= 0) {
            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_izq(copia, 1, BLOQUE);
                copiar_cadena(copia, aux, BLOQUE);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE);
                copiar_cadena(copia2, aux2, BLOQUE);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);

        } else {
            pos_copia = abs(pos_copia);

            for (j=0; j<pos_copia; j++) {
                aux = desplazamiento_der(copia, 1, BLOQUE);
                copiar_cadena(copia, aux, BLOQUE);
                free(aux);
            }
            for (j=0; j<i; j++) {
                aux2 = desplazamiento_der(copia2, 1, BLOQUE);
                copiar_cadena(copia2, aux2, BLOQUE);
                free(aux2);
            }

            aux = and(copia, copia2, BLOQUE);
        }

        /* Libermos para modificar despues */
        free(copia);

        copia = xor(ip_inv, aux, BLOQUE);
        copiar_cadena(ip_inv, copia, BLOQUE);
        free(aux);
        
    }

    /* Liberamos recursos */
    free(copia);
    free(copia2);

    return ip_inv;
}

/**************************************************************
 * Name: funcion_F(unsigned char* rn, unsigned char* kn)
 * Function: Funcion F del algoritmo DES. 
 * Parameters:
 *             unsigned char* rn: Bloque de 32 bits de la derecha (LN).
 *             unsigned char* kn: Clave de 48 bits generada. 
 * Return:
 *             unsigned char*: Bloque de 32 bits generado de la funcion F. 
 * ***********************************************************/
unsigned char* funcion_F(unsigned char* rn, unsigned char* kn) {
    unsigned char* rn_aux = NULL;
    unsigned char* aux = NULL;
    unsigned char* resultado_F = NULL;

    /* Control de errores */
    if (!rn || !kn) {
        printf("ERROR en la funcion F!!!\n");
        return NULL;
    }

    /* Reservamos memoria */
    rn_aux = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    resultado_F = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Llamamos a la funcion E */
    aux = funcion_E(rn);
    copiar_cadena(rn_aux, aux, BLOQUE-2);
    free(aux);
    /* Hacemos el xor de la salida de E con kn */
    aux = xor(rn_aux, kn, BLOQUE-2);
    copiar_cadena(rn_aux, aux, BLOQUE-2);
    free(aux);
    /* Llamamos a la funcion S */
    aux = funcion_S(rn_aux);
    copiar_cadena(rn_aux, aux, BLOQUE/2);
    free(aux);
    /* Llamamos a la funcion P */
    aux = funcion_P(rn_aux);
    copiar_cadena(resultado_F, aux, BLOQUE/2);
    free(aux);

    /* Liberamos recursos */
    free(rn_aux);

    return resultado_F;

}

/**************************************************************
 * Name: cifrar_DES(unsigned char* texto_plano, unsigned char* clave)
 * Function: Cifra un bloque de texto de 64 bits con una clave de 64 bits. 
 * Parameters:
 *             unsigned char* texto_plano: Bloque de 64 bits a cifrar.
 *             unsigned char* clave: Clave de 64 bits para cifrar. 
 * Return:
 *             unsigned char*: Texto cifrado con DES. 
 * ***********************************************************/
unsigned char* cifrar_DES(unsigned char* texto_plano, unsigned char* clave) {
    int i;
    unsigned char* lr = NULL;
    unsigned char* ip = NULL;
    unsigned char* pc1 = NULL;
    unsigned char** lcs = NULL;
    unsigned char* pc2 = NULL;
    unsigned char* l = NULL;
    unsigned char* r = NULL;
    unsigned char* swap = NULL;
    unsigned char* texto_cifrado = NULL;
    unsigned char* aux = NULL;
    unsigned char* f_aux = NULL;
    unsigned char* xor_aux = NULL;

    /* Control de errores */
    if (!texto_plano || !clave) {
        printf("ERROR al cifrar con DES!!!\n");
        return NULL;
    }

    /* Comprobamos la paridad de la clave */
    if (paridad(clave) == 0) { 
        printf("ERROR, la clave no cumple la paridad impar!!!\n");
        return NULL;
    }

    /* Reservamos memoria */
    l = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    r = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    aux = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Generamos lr */
    lr = generar_LR(texto_plano);

    /* Aplicamos IP */
    ip = funcion_IP(texto_plano);

    /* Generamos todas la claves */
    pc1 = funcion_PC1(clave);
    lcs = genera_LCS(pc1);

    /* Asignamos las variables auxiliares */
    copiar_cadena(l, ip, BLOQUE/2);
    copiar_cadena(r, &ip[4], BLOQUE/2);

    /* Vamos permutando lr con la funcion F */
    for (i=0; i<ROUNDS; i++) {
        copiar_cadena(aux, l, BLOQUE/2);
        copiar_cadena(l, r, BLOQUE/2);
        pc2 = funcion_PC2(lcs[i]);
        f_aux = funcion_F(r, pc2);
        copiar_cadena(r, f_aux, BLOQUE/2);
        xor_aux = xor(aux, r, BLOQUE/2);
        copiar_cadena(r, xor_aux, BLOQUE/2);

        /* Liberamos las variables auxiliares */
        free(f_aux);
        free(xor_aux);
        free(pc2);
    }

    /* Copiamos el lr */
    copiar_cadena(lr, l, BLOQUE/2);
    copiar_cadena(&lr[4], r, BLOQUE/2);

    /* Hacemos el SWAP y el IP inverso */
    swap = funcion_swap(lr);
    texto_cifrado = funcion_IP_inv(swap);

    /* Liberamos la memoria */
    free(lr);
    free(ip);
    free(pc1);
    for (i=0; i<ROUNDS; i++) {
        free(lcs[i]);
    }
    free(lcs);
    free(l);
    free(r);
    free(swap);
    free(aux);

    return texto_cifrado;
}

/**************************************************************
 * Name: descifrar_DES(unsigned char* texto_cifrado, unsigned char* clave)
 * Function: Descifra un bloque de texto de 64 bits con una clave de 64 bits. 
 * Parameters:
 *             unsigned char* texto_descifrado: Bloque de 64 bits a descifrar.
 *             unsigned char* clave: Clave de 64 bits para descifrar. 
 * Return:
 *             unsigned char*: Texto descifrado con DES. 
 * ***********************************************************/
unsigned char* descifrar_DES(unsigned char* texto_cifrado, unsigned char* clave) {
    int i;
    unsigned char* lr = NULL;
    unsigned char* ip = NULL;
    unsigned char* pc1 = NULL;
    unsigned char** lcs = NULL;
    unsigned char* pc2 = NULL;
    unsigned char* l = NULL;
    unsigned char* r = NULL;
    unsigned char* swap = NULL;
    unsigned char* texto_plano = NULL;
    unsigned char* aux = NULL;
    unsigned char* f_aux = NULL;
    unsigned char* xor_aux = NULL;

    /* Control de errores */
    if (!texto_cifrado || !clave) {
        printf("ERROR al cifrar con DES!!!\n");
        return NULL;
    }

    /* Comprobamos la paridad de la clave */
    if (paridad(clave) == 0) { 
        printf("ERROR, la clave no cumple la paridad impar!!!\n");
        return NULL;
    }

    /* Reservamos memoria */
    l = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    r = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));
    aux = (unsigned char*)calloc(BLOQUE, sizeof(unsigned char));

    /* Generamos lr para usarlo al final */
    lr = generar_LR(texto_cifrado);

    /* Aplicamos IP */
    ip = funcion_IP(texto_cifrado);

    /* Generamos todas la claves */
    pc1 = funcion_PC1(clave);
    lcs = genera_LCS(pc1);

    /* Asignamos las variables auxiliares */
    copiar_cadena(l, ip, BLOQUE/2);
    copiar_cadena(r, &ip[4], BLOQUE/2);

    /* Vamos permutando lr con la funcion F */
    for (i=0; i<ROUNDS; i++) {
        copiar_cadena(aux, l, BLOQUE/2);
        copiar_cadena(l, r, BLOQUE/2);
        /* Al descifrar usamos las clave en orden inverso */
        pc2 = funcion_PC2(lcs[ROUNDS - (i+1)]);
        f_aux = funcion_F(r, pc2);
        copiar_cadena(r, f_aux, BLOQUE/2);
        xor_aux = xor(aux, r, BLOQUE/2);
        copiar_cadena(r, xor_aux, BLOQUE/2);

        /* Liberamos las variables auxiliares */
        free(f_aux);
        free(xor_aux);
        free(pc2);
    }

    /* Copiamos el lr */
    copiar_cadena(lr, l, BLOQUE/2);
    copiar_cadena(&lr[4], r, BLOQUE/2);

    /* Hacemos el SWAP y el IP inverso */
    swap = funcion_swap(lr);
    texto_plano = funcion_IP_inv(swap);

    /* Liberamos la memoria */
    free(lr);
    free(ip);
    free(pc1);
    for (i=0; i<ROUNDS; i++) {
        free(lcs[i]);
    }
    free(lcs);
    free(l);
    free(r);
    free(swap);
    free(aux);

    return texto_plano;
}
