/**************************************************************
 * File: sbox_AES.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 23/11/2020
 * Last_Date: 23/11/2020 
 * Function: Implementacion para generar s-boxes del AES. 
 * ***********************************************************/
#include "type.h"
#include "operaciones_bit.h"
#define TABLAS_AES "Tablas_AES/"

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
    printf("./seg_perf -P/-I -i <fichero entrada> -o <fichero salida>\n");
    printf("\nSiendo -i y -o opcionales.\n");
    printf("################################################\n");
    return;
}

/**************************************************************
 * Name: numero_bits(unsigned char bloque)
 * Function: Devuelve el numero de bits de un bloque de bits. 
 * Parameters:
 *             unsigned char bloque: Bloque de bits.
 * Return:
 *             int: Numero de bits de bloque.
 * ***********************************************************/
int numero_bits(unsigned char bloque) {
    int i;
    unsigned char bloque_aux = 0;
    int numero_bits = 0;

    for (i=0; i<8; i++) {
        bloque_aux = bloque >> i;
        if ((bloque_aux & 0x01) == 1) {
            numero_bits = i + 1;
        }
    }
    
    return numero_bits;
}

/*************************************************************************************************
 * Name: resto_polinomios(unsigned char numerador, unsigned char denominador, BOOL gf8)
 * Function: Devuelve el resto de una division de polinomios. 
 * Parameters:
 *             unsigned char numerador: Polinomio numerador. 
 *             unsigned char denominador: Polinomio denominador.
 *             BOOL gf8: Indica si el polimonio numerador es el irreducible en AES.
 * Return:
 *             unsigned char: Bloque de 8 bits que representa el polinomio resto.
 * ***********************************************************************************************/
unsigned char resto_polinomios(unsigned char numerador, unsigned char denominador, BOOL gf8) {
    int num_bits = 0;
    int num_bits_numerador = 0;
    int num_bits_recorridos = 0;
    unsigned char bloque_numerador = 0;
    unsigned char comprueba_numerador = 0; 
    unsigned char* bloque_cociente = NULL;
    unsigned char* bloque_cociente_aux2 = NULL;
    unsigned char bloque_cociente_aux = 0;
    unsigned char bloque_resto = 0;
    
    /* Comprobamos el numero de bits que tiene el numerador y del denominador*/
    num_bits = numero_bits(denominador);
    num_bits_numerador = numero_bits(numerador);

    /* Cogemos el resto */
    bloque_numerador = numerador >> (num_bits_numerador-num_bits);
    bloque_resto = bloque_numerador;

    /* Vamos dividiendo */
    for (num_bits_recorridos=num_bits; num_bits_recorridos<=num_bits_numerador; num_bits_recorridos++) {
        /* Comprobamos si aniadimos un uno o un cero al cociente */
        comprueba_numerador = bloque_numerador >> (num_bits-1);

        /* Vamos sacando el cociente */
        if (comprueba_numerador == 1) {
            bloque_cociente_aux = (bloque_cociente_aux << 1) ^ 0x01;
            /* Vamos haciendo la division */
            bloque_resto = bloque_numerador ^ denominador;
        } else {
            bloque_cociente_aux = (bloque_cociente_aux << 1);
            bloque_resto = bloque_numerador;
            
        }

        /* Sacamos el nuevo numerador */
        if (num_bits_recorridos != num_bits_numerador) {
            bloque_numerador = (bloque_resto << 1) ^ ((numerador >> (num_bits_numerador-num_bits_recorridos-1)) & 0x01);
        } else {
            if (num_bits != num_bits_numerador) {
                bloque_numerador = bloque_resto;
            } else {
                bloque_resto = numerador ^ denominador;
            }
        }

    }

    /* Comprobamos si el denominador es GF8 */
    if (gf8 == TRUE) {
        /* Ronda extra por GF8 */
        bloque_numerador = (bloque_resto << 1) ^ 0x01;
        /* Comprobamos si aniadimos un uno o un cero al cociente */
        comprueba_numerador = bloque_numerador >> (num_bits-1);
        /* Vamos sacando el cociente */
        if (comprueba_numerador == 1) {
            bloque_cociente_aux2 = (unsigned char*)calloc(2, sizeof(unsigned char));
            bloque_cociente_aux2[0] = bloque_cociente_aux;
            bloque_cociente = desplazamiento_izq(bloque_cociente_aux2, 1, 2);
            bloque_cociente[0] = bloque_cociente[0] ^ 0x01;
            /* Vamos haciendo la division */
            bloque_resto = bloque_numerador ^ denominador;
            
        } else {
            bloque_cociente_aux2 = (unsigned char*)calloc(2, sizeof(unsigned char));
            bloque_cociente_aux2[0] = bloque_cociente_aux;
            bloque_cociente = desplazamiento_izq(bloque_cociente_aux2, 1, 2);
            bloque_resto = bloque_numerador;
            
        }
    } else {
        bloque_cociente = (unsigned char*)calloc(2, sizeof(unsigned char));
        bloque_cociente[0] = bloque_cociente_aux;
        bloque_cociente[1] = 0;
    }

    /* Liberamos recursos */
    free(bloque_cociente_aux2);
    free(bloque_cociente);

    return bloque_resto;
}

/*************************************************************************************************
 * Name: divide_polinomios(unsigned char numerador, unsigned char denominador, BOOL gf8)
 * Function: Devuelve el cociente de una division de polinomios. 
 * Parameters:
 *             unsigned char numerador: Polinomio numerador. 
 *             unsigned char denominador: Polinomio denominador.
 *             BOOL gf8: Indica si el polimonio numerador es el irreducible en AES.
 * Return:
 *             unsigned char: Bloque de 8 bits que representa el polinomio cociente.
 * ***********************************************************************************************/
unsigned char divide_polinomios(unsigned char numerador, unsigned char denominador, BOOL gf8) {
    int num_bits = 0;
    int num_bits_numerador = 0;
    int num_bits_recorridos = 0;
    unsigned char bloque_numerador = 0;
    unsigned char comprueba_numerador = 0; 
    unsigned char* bloque_cociente = NULL;
    unsigned char* bloque_cociente_aux2 = NULL;
    unsigned char bloque_cociente_aux = 0;
    unsigned char bloque_resto = 0;
    
    /* Comprobamos el numero de bits que tiene el numerador y del denominador*/
    num_bits = numero_bits(denominador);
    num_bits_numerador = numero_bits(numerador);

    /* Cogemos el resto */
    bloque_numerador = numerador >> (num_bits_numerador-num_bits);
    bloque_resto = bloque_numerador;

    /* Vamos dividiendo */
    for (num_bits_recorridos=num_bits; num_bits_recorridos<=num_bits_numerador; num_bits_recorridos++) {
        /* Comprobamos si aniadimos un uno o un cero al cociente */
        comprueba_numerador = bloque_numerador >> (num_bits-1);

        /* Vamos sacando el cociente */
        if (comprueba_numerador == 1) {
            bloque_cociente_aux = (bloque_cociente_aux << 1) ^ 0x01;
            /* Vamos haciendo la division */
            bloque_resto = bloque_numerador ^ denominador;
        } else {
            bloque_cociente_aux = (bloque_cociente_aux << 1);
            bloque_resto = bloque_numerador;
            
        }

        /* Sacamos el nuevo numerador */
        if (num_bits_recorridos != num_bits_numerador) {
            bloque_numerador = (bloque_resto << 1) ^ ((numerador >> (num_bits_numerador-num_bits_recorridos-1)) & 0x01);
        } else {
            if (num_bits != num_bits_numerador) {
                bloque_numerador = bloque_resto;
            } else {
                bloque_resto = numerador ^ denominador;
            }
        }

    }

    /* Comprobamos si el denominador es GF8 */
    if (gf8 == TRUE) {
        /* Ronda extra por GF8 */
        bloque_numerador = (bloque_resto << 1) ^ 0x01;
        /* Comprobamos si aniadimos un uno o un cero al cociente */
        comprueba_numerador = bloque_numerador >> (num_bits-1);
        /* Vamos sacando el cociente */
        if (comprueba_numerador == 1) {
            bloque_cociente_aux2 = (unsigned char*)calloc(2, sizeof(unsigned char));
            bloque_cociente_aux2[0] = bloque_cociente_aux;
            bloque_cociente = desplazamiento_izq(bloque_cociente_aux2, 1, 2);
            bloque_cociente[0] = bloque_cociente[0] ^ 0x01;
            /* Vamos haciendo la division */
            bloque_resto = bloque_numerador ^ denominador;
        } else {
            bloque_cociente_aux2 = (unsigned char*)calloc(2, sizeof(unsigned char));
            bloque_cociente_aux2[0] = bloque_cociente_aux;
            bloque_cociente = desplazamiento_izq(bloque_cociente_aux2, 1, 2);
            bloque_resto = bloque_numerador;
        }
    } else {
        bloque_cociente = (unsigned char*)calloc(2, sizeof(unsigned char));
        bloque_cociente[0] = bloque_cociente_aux;
        bloque_cociente[1] = 0;
    }

    bloque_cociente_aux = bloque_cociente[0];

    /* Liberamos recursos */
    free(bloque_cociente_aux2);
    free(bloque_cociente);

    return bloque_cociente_aux;
}

/*************************************************************************************************
 * Name: calculate_euclides_extendido(unsigned char r0, unsigned char r1, unsigned char mcd, unsigned char* resultado)
 * Function: Calcula euclides extendido a partir de un polimonio 
 * Parameters:
 *             unsigned char r0: Polimonio numerador.
 *             unsigned char r1: Polimonio denominador.
 *             unsigned char mcd: Mcd.
 *             unsigned char* resultado: Polimonio resultado.
 * Return:
 *             BOOL: TRUE si no ha habido error o FALSE si lo ha habido .
 * ***********************************************************************************************/
BOOL calculate_euclides_extendido(unsigned char m, unsigned char a, unsigned char* x, unsigned char* y, BOOL es_m){

    unsigned char x_aux=0;
    unsigned char y_aux=0;
    unsigned char a_aux=0;
    
    
    if (a == 0) {
        *x=1;
        *y=0;
    }
    else {
        a_aux = resto_polinomios(m, a, es_m);
        calculate_euclides_extendido (a, a_aux, x, y, FALSE);
        x_aux = *x;
        y_aux = *y;
        *x = y_aux;

        if (a == 1) {
            a_aux = m;
        } else {
            a_aux = divide_polinomios(m, a, es_m);
        }
        a_aux = multiplica_polinomios(a_aux, y_aux);
        y[0] = x_aux ^ a_aux;
    }
    return TRUE;
}

/*************************************************************************************************
 * Name: calculate_euclides(unsigned char polinomio)
 * Function: Calcula euclides a partir de un polimonio 
 * Parameters:
 *             unsigned char polinomio: Polinomio.
 * Return:
 *             unsigned char: devuelve el resultado del euclides extendido.
 * ***********************************************************************************************/
unsigned char* calculate_euclides(unsigned char polinomio){

    unsigned char m = 0x8D;
    unsigned char mcd;
    unsigned char r0;
    unsigned char r1;
    unsigned char resto;
    unsigned char* resultado;

    resultado = (unsigned char*) calloc (8, sizeof (unsigned char));

    divide_polinomios(m, polinomio,  TRUE);
    resto = resto_polinomios(m, polinomio, TRUE);
    r0=polinomio;
    r1=resto;
    while (resto!=0){
        divide_polinomios(r0, r1, FALSE);
        resto = resto_polinomios(r0, r1, FALSE);
        r0=r1;
        mcd=r1;
        r1=resto;
        
    }
    if(mcd==1){
        if(calculate_euclides_extendido(m, polinomio, &mcd, resultado, TRUE) == TRUE){
            return resultado;
        }
        else
            return NULL;
    }
    return NULL;

}

/**************************************************************
 * Name: imprimir_tabla(FILE* f, unsigned char*** tabla)
 * Function: Imprime en fichero la tabla. 
 * Parameters:
 *             FILE* f: Fichero donde se escribe.
 *             unsigned char*** tabla: Tabla a imprimir.
 * Return:
 *             void.
 * ************************************************************/
void imprimir_tabla(FILE* f, unsigned char*** tabla){
    int i, j;
    fseek(f, 0L, SEEK_SET);
    for(i=0; i<16; i++){
        for (j=0; j<16; j++){
            fwrite(tabla[i][j], sizeof(unsigned char), 2, f);
            fwrite(" ", sizeof(unsigned char), 1, f);
        }
        fwrite("\n", sizeof(unsigned char), 1, f);
    }
}

/**************************************************************
 * Name: imprime_clave(unsigned char* clave)
 * Function: Convierte un caracter de decimal a hexadecimal. 
 * Parameters:
 *             unsigned char* clave: Caracter a convertir.
 * Return:
 *             unsigned char*: Caracter en hezadecimal.
 * ************************************************************/
unsigned char* imprime_clave(unsigned char* clave) {
    int i;
    unsigned char* letra = (unsigned char*)calloc(2, sizeof(unsigned char));
    int num = 0;

    /* Vamos imprimienndo uno a uno cada caracter */
    for (i=0; i<1; i++) {
        /* Primera letra del bloque */
        num = clave[i] >> 4;
        if (num < 10) {
            letra[0] = num + 48;
        } else {
            letra[0] = num + 55;
        }
        /* Segunda letra del bloque */
        num = clave[i] & 0x0f;
        if (num < 10) {
            letra[1] = num + 48;
        } else {
            letra[1] = num + 55;
        } 
    }

    return letra;

}

/**************************************************************
 * Name: tabla_AES_directa()
 * Function: Calcula la tabla directa de SBox. 
 * Parameters:
 *             
 * Return:
 *             unsigned char***: tabla sbox completa
 * ************************************************************/
unsigned char*** tabla_AES_directa(){
    
    unsigned char*** s;
    unsigned char* b = NULL;
    unsigned char* shift_1;
    unsigned char* shift_2;
    unsigned char* shift_3;
    unsigned char* shift_4;
    unsigned char valor = 0x63; 

    unsigned char inverso_1=1;

    unsigned char* xor_1;
    unsigned char* xor_2;
    unsigned char* xor_3;
    unsigned char* xor_4;
    unsigned char* xor_5;

    int i, j;
    
    /*Inicializar tabla resultado*/
    s  = (unsigned char*** )calloc (16, sizeof(unsigned char**));
    
    for(i=0; i<16;i++){
        s[i]= (unsigned char**)calloc (16, sizeof(unsigned char*));
        for (j=0; j<16; j++){
            s[i][j] = (unsigned char*)calloc (3, sizeof(unsigned char));
        }
    }

    /*El inverso del 0 no se calcula y se mete el valor directamente*/
    for (i=0; i<16; i++){
        for (j=0; j<16; j++) {
           if (j==0 && i==0){
                s[0][0][0] = 54;
                s[0][0][1] = 51;
            } else{
                if(((i*16)+j) == 1){
                    b = (unsigned char*)calloc(1, sizeof(unsigned char));
                    b[0]=inverso_1;
                } else{
                    b=calculate_euclides((i*16)+j);
                    if(b==NULL){
                        return NULL;
                    }
                }
                /*Desplazamientos a la izquierda*/
                shift_1 = desplazamiento_izq(b, 1, 1);
                shift_2 = desplazamiento_izq(b, 2, 1);
                shift_3 = desplazamiento_izq(b, 3, 1);
                shift_4 = desplazamiento_izq(b, 4, 1);

                /*XOR de los desplazamientos*/
                xor_1 = xor(b, shift_1, 1);
                xor_2 = xor(xor_1, shift_2, 1);
                xor_3 = xor(xor_2, shift_3, 1);
                xor_4 = xor(xor_3, shift_4, 1);
                xor_5 = xor(xor_4, &valor, 1);
                
                free(s[i][j]);
                s[i][j]=imprime_clave(xor_5);

                free(shift_1);
                free(shift_2);
                free(shift_3);
                free(shift_4);

                free(xor_1);
                free(xor_2);
                free(xor_3);
                free(xor_4);
                free(xor_5);
                free(b);
            }
        }
    }


    return s;
}

/**************************************************************
 * Name: tabla_AES_inversa()
 * Function: Calcula la tabla inversa de SBox. 
 * Parameters:
 *             
 * Return:
 *             unsigned char***: tabla sbox completa
 * ************************************************************/
unsigned char*** tabla_AES_inversa(){
    
    unsigned char*** s;
    unsigned char* b=NULL;
    unsigned char a;
    unsigned char* shift_1;
    unsigned char* shift_2;
    unsigned char* shift_3;
    unsigned char valor = 0x05;

    unsigned char* xor_1;
    unsigned char* xor_2;
    unsigned char* xor_3;
    
    int i, j;
    
    /*Inicializar tabla resultado*/
    s  = (unsigned char*** )calloc (16, sizeof(unsigned char**));
    
    for(i=0; i<16;i++){
        s[i]= (unsigned char**)calloc (16, sizeof(unsigned char*));
        for (j=0; j<16; j++){
            s[i][j] = (unsigned char*)calloc (3, sizeof(unsigned char));
        }
    }

    for(i=0; i<16; i++){
        for(j=0; j<16; j++){

            a= (i*16)+j;
            shift_1 = desplazamiento_izq(&a, 1, 1);
            shift_2 = desplazamiento_izq(&a, 3, 1);
            shift_3 = desplazamiento_izq(&a, 6, 1);

            xor_1 = xor(shift_1, shift_2, 1);
            xor_2 = xor(xor_1, shift_3, 1);
            xor_3 = xor (xor_2, &valor, 1);

            if(*xor_3 == 0){
                s[i][j][0] = 48;
                s[i][j][1] = 48;
            }
            else if(*xor_3 == 1){
                s[i][j][0] = 48;
                s[i][j][1] = 49;
            }else{
                b = calculate_euclides(*xor_3);
                if(b==NULL){
                    return NULL;
                }
                free(s[i][j]);
                s[i][j] = imprime_clave(b);
                free(b);
            }
            free(shift_1);
            free(shift_2);
            free(shift_3);
            
            free(xor_1);
            free(xor_2);
            free(xor_3);
        }
        
    }
    

    return s;
}

/* Funcion principal */
int main(int argc, char *argv[]) {

    FILE* o = NULL;
    char cad_plano[40] = TABLAS_AES;
    unsigned char*** tabla;
    int i, j;

    /* Control de errores */
    if (argc != 2 && argc != 4) {
       muestra_info_parametros();
       return -1;
    } else if (strcmp(argv[1], "-C") != 0 && strcmp(argv[1], "-D") != 0) {
        printf("Primer argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (argc == 4) {
        if (strcmp(argv[2], "-o") != 0) {
            printf("Segundo argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        }
    }
    if (argc == 4) {
        if (strcmp(argv[1], "-C") == 0 && strcmp(argv[2], "-o") == 0) {
            strcat(cad_plano, argv[3]);
            o = fopen(cad_plano, "w");
            tabla = tabla_AES_directa();
            imprimir_tabla(o,tabla);
        } else if(strcmp(argv[1], "-D") == 0 && strcmp(argv[2], "-o") == 0) {
            strcat(cad_plano, argv[3]);
            o = fopen(cad_plano, "w");
            tabla = tabla_AES_inversa();
            imprimir_tabla(o,tabla);
        }
    } else if(argc ==2){
        if (strcmp(argv[1], "-C") == 0)  {
            strcat(cad_plano, "sbox_aes_directa.txt");
            printf("La tabla se ha escrito en %s\n", cad_plano);
            o = fopen(cad_plano, "w");
            tabla = tabla_AES_directa();
            imprimir_tabla(o,tabla);

        } else if(strcmp(argv[1], "-D") == 0){
            strcat(cad_plano, "sbox_aes_inversa.txt");
            printf("La tabla se ha escrito en %s\n", cad_plano);
            o = fopen(cad_plano, "w");
            tabla = tabla_AES_inversa();
            imprimir_tabla(o,tabla);
        }
    }


    /* Liberamos recursos */
    if (o) {
        fclose(o);
    }

    if (tabla) {
        for(i=0;i<16; i++){
            if(tabla[i]!=NULL){
                for(j=0;j<16;j++){
                    if(tabla[i][j]!=NULL){
                        free(tabla[i][j]);
                    }
                }
                free(tabla[i]);
            }
        }
        free(tabla);
    }
    

    


    return 0;

}
