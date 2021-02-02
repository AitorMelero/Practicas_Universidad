/**************************************************************
 * File: operaciones_bit.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 05/11/2020
 * Last_Date: 16/11/2020 
 * Function: Implementacion de operaciones a nivel de bit para bloques
 *           en c. 
 * ***********************************************************/

#include "operaciones_bit.h"

int semilla = 27;  /* Semilla para aleatoriedad */ 

/**************************************************************
 * Name: print_binint(int num, unsigned char* bits)
 * Function: Genera una cadena de bits a partir de un bloque de 8 bits.
 * Parameters:
 *             int num: Bloque de bits. 
 *             unsigned char* bits: Cadena que representa los bits. 
 * Return:
 *            void: Nada. 
 * ***********************************************************/
void print_binint(int num, unsigned char* bits){
	int sup = 8 * sizeof(int);
	int i = 0;

    /* Vamos generando la caracter a caracter la respresentacion */
	while(sup >= 0) {
		if (num & (((long int)1) << sup)) {
			if (sup <= 7) {
				bits[i] = '1';
				i++;
			}	
		} else {
			if (sup <= 7) {
				bits[i] = '0';
				i++;
			}	
		}
		sup--;
	}

}

/**************************************************************
 * Name: copiar_cadena(unsigned char* cad1, unsigned char* cad2, int num_bloque)
 * Function: Copia dos cadenas sin '\0'. 
 * Parameters:
 *             unsigned char* cad1: Cadena destino.
 *             unsigned char* cad2: Cadena origen. 
 *             int num_bloque: Numero de bloques de 8 bits de la cadena.
 * Return:
 *            void: Nada. 
 * ***********************************************************/
void copiar_cadena(unsigned char* cad1, unsigned char* cad2, int num_bloque) {
    int i;

    for (i=0; i<num_bloque; i++) {
        cad1[i] = cad2[i];
    }
}

/**************************************************************
 * Name: genera_aleatorio(int min, int max) 
 * Function: Genera un numero aleatorio entre min y max. 
 * Parameters:
 *             int min: Minimo numero aleatorio posible.
 *             int max: Maximo numero aleatorio posible.
 * Return:
 *             unsigned char: Numero aleatorio generado.
 * ***********************************************************/
unsigned char genera_aleatorio(int min, int max) {
    unsigned char num_aleatorio = 0;

    /* Control de errores */
    if ((min < 0) || (max > 255)) {
        printf("ERROR al generar numero aleatorio!!!\n");
        return 0;
    }

    /* Generamos el numero aleatorio */
    if (semilla == 27) {
        srand(time(NULL));
    } else {
        srand(rand());
    }
    num_aleatorio = (unsigned char) (rand() % (max-min+1) + min);
    semilla = num_aleatorio;

    return num_aleatorio;

}

/**************************************************************
 * Name: genera_clave() 
 * Function: Genera una clave de 64 bits con la paridad correcta. 
 * Parameters:
 *             Ninguno.
 * Return:
 *             unsigned char: Numero aleatorio generado.
 * ***********************************************************/
unsigned char* genera_clave() {
    unsigned char* clave = (unsigned char*)calloc(8, sizeof(unsigned char));
    unsigned char clave_aux = 0;
    int i;

    for (i=0; i<8; i++) {
        clave_aux = genera_aleatorio(0, 127);
        clave_aux = clave_aux << 1;
        clave[i] = clave_aux ^ calcula_paridad(clave_aux);
    }

    return clave;
}

/**************************************************************
 * Name: calcula_paridad(unsigned char bloque) 
 * Function: Calcula la paridad impar de un bloque de 7 bits. 
 * Parameters:
 *             unsigned char bloque: Bloque de 7 bits.
 * Return:
 *             unsigned char: FF o FE en funcion de la paridad. 
 * ***********************************************************/
unsigned char calcula_paridad(unsigned char bloque) {
    int i;
    int num_unos;
    unsigned char bloque_aux = 0;

    for (i=0, num_unos=0; i<8; i++) {
       bloque_aux = bloque >> i;
       bloque_aux = bloque_aux & 0x01;
       if (bloque_aux == 1) {
           num_unos++;
       }
    }

    /* Comprobamos paridad para aniadir uno o cero */
    if ((num_unos%2) == 0) {
        return 0x01;
    } else {
        return 0x00;
    }
}

/**************************************************************
 * Name: deplazamiento_izq(unsigned char* bloque, int desp, int num_bloques)
 * Function: Desplaza hacia la izquierda uno o varios bloques de 8 bits. 
 * Parameters:
 *             unsigned char* bloque: Bloque de bits a desplazar.
 *             int desp: Numero de bits a desplazar.
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del desplazamiento. 
 * ***********************************************************/
unsigned char* desplazamiento_izq(unsigned char* bloque, int desp, int num_bloques) {
    unsigned char* resultado = NULL;
    unsigned char* desp_aux = NULL;
    unsigned char bloque_despl; 
    int i;

    /* Control de errores */
    if (!bloque || (desp<0) || (num_bloques<1)) {
        printf("ERROR al realizar el desplazamiento hacia la izquierda!!!\n");
        return NULL;
    }

    /* Reservamos memoria para resultado y desp_aux */
    resultado = (unsigned char*)calloc(num_bloques, sizeof(unsigned char));
    desp_aux = (unsigned char*)calloc(num_bloques, sizeof(unsigned char));

    /* Averiguamos la cantidad de bits con menos peso para
       calculos posteriores */
    bloque_despl = pow(2, desp) - 1;

    /* Realizamos el desplazamiento bloque a bloque
       y guardamos los bits de menor peso */
    for (i=0; i<num_bloques; i++) {
        desp_aux[i] = bloque[i] >> (8-desp);
        resultado[i] = bloque[i] << desp;
        desp_aux[i] = bloque_despl & desp_aux[i];
        resultado[i] = (~bloque_despl) & resultado[i];
    }

    /* Obtenemos el resultado */
    for (i=0; i<num_bloques; i++) {
        resultado[i] = resultado[i] ^ desp_aux[(i+1)%num_bloques];
    }

    /* Liberamos recursos */
    free(desp_aux);

    return resultado;

}

/**************************************************************
 * Name: desplazamiento_der(unsigned char* bloque, int desp, int num_bloques)
 * Function: Desplaza hacia la derecha uno o varios bloques de 8 bits. 
 * Parameters:
 *             unsigned char* bloque: Bloque de bits a desplazar.
 *             int desp: Numero de bits a desplazar.
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del desplazamiento. 
 * ***********************************************************/
unsigned char* desplazamiento_der(unsigned char* bloque, int desp, int num_bloques) {
    unsigned char* resultado = NULL;
    unsigned char* desp_aux = NULL;
    unsigned char bloque_despl; 
    int i;

    /* Control de errores */
    if (!bloque || (desp<0) || (num_bloques<1)) {
        printf("ERROR al realizar el desplazamiento hacia la izquierda!!!\n");
        return NULL;
    }

    /* Reservamos memoria para resultado y desp_aux */
    resultado = (unsigned char*)calloc(num_bloques, sizeof(unsigned char));
    desp_aux = (unsigned char*)calloc(num_bloques, sizeof(unsigned char));

    /* Averiguamos la cantidad de bits con menos peso para
       calculos posteriores */
    bloque_despl = ((int)pow(2, desp) - 1) << (8 - desp);

    /* Realizamos el desplazamiento bloque a bloque
       y guardamos los bits de menor peso */
    for (i=0; i<num_bloques; i++) {
        desp_aux[i] = (bloque[i] & ((int)pow(2, desp)-1)) << (8 - desp);
        resultado[i] = bloque[i] >> desp;
        resultado[i] = (~bloque_despl) & resultado[i];
    }

    /* Obtenemos el resultado */
    for (i=0; i<num_bloques; i++) {
        resultado[i] = resultado[i] ^ desp_aux[((num_bloques-1)+i)%num_bloques];
    }

    /* Liberamos recursos */
    free(desp_aux);

    return resultado;
}

/**************************************************************
 * Name: xor(unsigned char* bloque, unsigned char* bloque2, int num_bloques)
 * Function: Operacion xor para dos bloques de bits.
 * Parameters:
 *             unsigned char* bloque: Bloque de bits.
 *             unsigned char* bloque2: Bloque de bits. 
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del xor. 
 * ***********************************************************/
unsigned char* xor(unsigned char* bloque, unsigned char* bloque2, int num_bloques) {
    int i;
    unsigned char* resultado = NULL;

    /* Control de errores */
    if (!bloque || !bloque2 || num_bloques<=0) {
        printf("ERROR en la operacion xor!!!");
        return NULL;
    }

    /* Reservamos memoria */
    resultado = (unsigned char*)calloc(num_bloques, sizeof(unsigned char));

    for (i=0; i<num_bloques; i++) {
        resultado[i] = bloque[i] ^ bloque2[i];
    }

    return resultado;
}

/**************************************************************
 * Name: and(unsigned char* bloque, unsigned char* bloque2, int num_bloques)
 * Function: Operacion and para dos bloques de bits.
 * Parameters:
 *             unsigned char* bloque: Bloque de bits.
 *             unsigned char* bloque2: Bloque de bits. 
 *             int num_bloques: Numero de bloques de bloque.
 *             
 * Return:
 *             unsigned char*: Resultado del and. 
 * ***********************************************************/
unsigned char* and(unsigned char* bloque, unsigned char* bloque2, int num_bloques) {
   int i;
   unsigned char* resultado = NULL;

   /* Control de errores */
   if (!bloque || !bloque2 || num_bloques<=0) {
       printf("ERROR en la operacion xor!!!");
       return NULL;
   }

   /* Reservamos memoria */
   resultado = (unsigned char*)calloc(num_bloques, sizeof(unsigned char));

   for (i=0; i<num_bloques; i++) {
       resultado[i] = bloque[i] & bloque2[i];
   }

   return resultado; 
}

/**************************************************************
 * Name: paridad(unsigned char* clave)
 * Function: Comprueba la paridad de la clave. 
 * Parameters:
 *             unsigned char* clave: Clave a analizar.
 * Return:
 *             int: 1 si se cumple la paridad, 0 en caso contrario.
 * ***********************************************************/
int paridad(unsigned char* clave) {
    unsigned char paridad;
    int i, j;
    int suma = 0;
    unsigned char aux;
    unsigned char uno = 0x01;
    int si_paridad = 1;

    for (i=0; i<8 && si_paridad==1; i++, suma=0) {
        /* Vemos si hay un numero par o impar de unos */
        paridad = clave[i] & uno;
        for (j=1; j<9; j++) {
            aux = (clave[i] >> j) & uno;
            if (aux == 1) {
                suma++;
            }
        }

        /* Comprobamos si se cumple la paridad impar */
        if ((paridad == 1) && (suma%2==0)) {
            si_paridad = 1;
        } else if ((paridad == 0) && (suma%2!=0)) {
            si_paridad = 1;
        } else {
            si_paridad = 0;
        }


    }

    return si_paridad;

}

/**************************************************************
 * Name: funcion_xtime(unsigned char num)
 * Function: Funcion xtime para un numero. 
 * Parameters:
 *            unsigned char num: Numero para xtime.
 * Return:
 *            unsigned char: Solucion de xtime.
 * ***********************************************************/
unsigned char funcion_xtime(unsigned char num) {
    unsigned char aux = num >> 7;
    unsigned char despl = 0;

    if (aux == 1) {
        despl = num << 1;
        despl = despl ^ 0x1b;
    } else {
        despl = num << 1;
    }

    return despl;
}

/**************************************************************
 * Name: multiplica_polinomios(unsigned char num1, unsigned char num2)
 * Function: Funcion que multiplica dos bloques de 8 bits. 
 * Parameters:
 *            unsigned char num1: Bloque uno a multiplicar. 
 *            unsigned char num2: Bloque dos a multiplicar. 
 * Return:
 *            unsigned char: Resultado de la multiplicacion. 
 * ***********************************************************/
unsigned char multiplica_polinomios(unsigned char num1, unsigned char num2) {
    int i, j;
    unsigned char bloque_aux = 0;
    unsigned char* descom = NULL;
    int num_descom = 0;
    BOOL operamos = TRUE;
    unsigned char* solucion = NULL;
    unsigned char resultado = 0;
    unsigned char xtime = num1;

    /* Descomponemos el bloque 2 */
    for (i=0; i<8; i++) {
        bloque_aux = num2 & (0X01 << i);

        if (bloque_aux != 0) {
            if (descom == NULL) {
                descom = (unsigned char*)calloc(1, sizeof(unsigned char));
                descom[0] = bloque_aux;
                num_descom++;
            } else {
                descom = (unsigned char*)realloc(descom, (num_descom+1)*sizeof(unsigned char));
                descom[num_descom] = bloque_aux;
                num_descom++;
            }
        }
    }

    /* Reservamos memoria para la solucion */
    solucion = (unsigned char*)calloc(num_descom, sizeof(unsigned char));

    /* Inicializamos variables de indice */
    i = 0;
    j = 0;
    while (operamos==TRUE && i<num_descom) {
        /* Hacemos el xtime */
        if (j!=0) {
            xtime = funcion_xtime(xtime);
        }

        if (i==0 && descom[i]==1) {
            solucion[i] = num1;
            i++;
            if (i==num_descom) {
                operamos = FALSE;
            }
        } else {
            bloque_aux = 0x01 << j;
            if (bloque_aux == descom[i]) {
                solucion[i] = xtime;
                i++;
                if (i==num_descom) {
                    operamos = FALSE;
                }
            }
        }

        j++;
    }

    /* Calculamos el resultado */
    for (i=0; i<num_descom; i++) {
        resultado = resultado ^ solucion[i];
    }

    /* Liberamos memoria */ 
    free(solucion);
    free(descom);

    return resultado;

}
