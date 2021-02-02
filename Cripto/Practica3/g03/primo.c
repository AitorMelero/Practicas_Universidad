#include "type.h"
#include <gmp.h>
#include <time.h>
#include <math.h>
#include <unistd.h>
#define TEXTO_SALIDA "Textos_Salida/"

/**************************************************************
 * Name: genera_a(mpz_t n, mpz_t a) 
 * Function: Genera un numero aleatoria a gracias a n. 
 * Parameters:
 *              mpz_t n: n.
 *              mpz_t a: a a calcular.
 * Return:
 *              void: Nada. 
 * ***********************************************************/
void genera_a(mpz_t n, mpz_t a) {
    gmp_randstate_t state;
    
    /* Inicializamos las variables a usar */
    gmp_randinit_mt(state);

    /* Modificamos el valor de la semilla */
    gmp_randseed_ui(state, time(NULL)*getpid());

    /* Calculamos el numero aleatorio */
    mpz_urandomm(a, state, n);

    /* En el caso poco probable de que el resultado no sea mayor que 1 se vuelve a llamar
       a la funcion */
    if (mpz_cmp_ui(a, 1) <= 0){
        genera_a(n, a);
    }

    /* Liberamos recursos */
    gmp_randclear(state);

}

/********************************************************************************
 * Name: primo(int nbits)
 * Function: Generacion de numeros primos, mediante el algoritmo Miller-Rabin
 * Parameters:
 *              int nbits: numero maximo bits significativo del numero generado.
 * Return:
 *              Valor 0 si el numero es COMPUESTO o 1 si el numero es POSIBLE PRIMO
 * *****************************************************************************/

int primo(mpz_t p){
    
    mpz_t q;
    mpz_t x;
    mpz_t aux;
    mpz_t r;
    mpz_t a;
    mpz_t dos;
    mpz_t m, k;
    long k_aux;
    int i;

    /*Inicializamos variables*/
    mpz_init(x);
    mpz_init(aux);
    mpz_init(r);
    mpz_init(a);
    mpz_init(dos);
    mpz_init(m);
    mpz_set_ui(m, 0);
    mpz_init(k);
    mpz_set_ui(k, 0);

    /*q = p -1*/
    mpz_sub_ui(aux, p, 1);
    mpz_init_set(q, aux);

    /* Calcular m y k */
    for (i=0; mpz_cmp_si(q, 0)>0; i++){
        mpz_mod_ui(r, q, 2);
        if(mpz_cmp_si(r, 0)==0) 
            mpz_add_ui(k, k, 1);
        
        if(mpz_cmp_si(r, 0)!=0) {
            mpz_set(m, q);
            break;
        }
        mpz_div_ui(q, q, 2);

    }

    /*Genera numero aleatorio A entre 1 y (p-1)*/
    genera_a(aux, a);
    
    /*x=a^m mod p*/
    mpz_powm(x, a, m, p);
    if(mpz_cmp_ui(x, 1)==0) {
        mpz_clear(a);
        mpz_clear(x);
        mpz_clear(m);
        mpz_clear(k);
        mpz_clear(r);
        mpz_clear(aux);
        mpz_clear(q);
        mpz_clear(dos);
        return 1;
    }
    if(mpz_cmp(x, aux)==0) {
        mpz_clear(a);
        mpz_clear(x);
        mpz_clear(m);
        mpz_clear(k);
        mpz_clear(r);
        mpz_clear(aux);
        mpz_clear(q);
        mpz_clear(dos);
        return 1;
    }

    k_aux = mpz_get_si(k);
    mpz_set_si(dos, 2);
    for(i=1; i<(k_aux-1); i++){
        /*x=x^2 mod p*/
        mpz_powm(x, x, dos, p);
        if(mpz_cmp_ui(x, 1)==0) {
            mpz_clear(x);
            mpz_clear(m);
            mpz_clear(k);
            mpz_clear(r);
            mpz_clear(aux);
            mpz_clear(q);
            mpz_clear(dos);
            mpz_clear(a);
            return 0;
        }
        if(mpz_cmp(x, aux)==0) {
            mpz_clear(x);
            mpz_clear(m);
            mpz_clear(k);
            mpz_clear(r);
            mpz_clear(aux);
            mpz_clear(q);
            mpz_clear(dos);
            mpz_clear(a);
            return 1;
        }
    }

    mpz_clear(x);
    mpz_clear(m);
    mpz_clear(k);
    mpz_clear(r);
    mpz_clear(aux);
    mpz_clear(q);
    mpz_clear(dos);
    mpz_clear(a);

    return 0;
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
    printf("./primo -b <bits> -p <prob> -o <fichero_salida>\n");
    printf("Siendo -o optativo\n");
    printf("################################################\n");
    return;
}

/* MAIN */
int main(int argc, char *argv[]) {
    
    int nbits;
    int prob;
    int salida_mpz;
    int salida_primo;
    mpz_t p;
    mpz_t nbits_aux;
    gmp_randstate_t state1;
    int nreps;
    float fprob;
    int i, j;
    int primos[303] = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009, 1013, 1019, 1021, 1031, 1033, 1039, 1049, 1051, 1061, 1063, 1069, 1087, 1091, 1093, 1097, 1103, 1109, 1117, 1123, 1129, 1151, 1153, 1163, 1171, 1181, 1187, 1193, 1201, 1213, 1217, 1223, 1229, 1231, 1237, 1249, 1259, 1277, 1279, 1283, 1289, 1291, 1297, 1301, 1303, 1307, 1319, 1321, 1327, 1361, 1367, 1373, 1381, 1399, 1409, 1423, 1427, 1429, 1433, 1439, 1447, 1451, 1453, 1459, 1471, 1481, 1483, 1487, 1489, 1493, 1499, 1511, 1523, 1531, 1543, 1549, 1553, 1559, 1567, 1571, 1579, 1583, 1597, 1601, 1607, 1609, 1613, 1619, 1621, 1627, 1637, 1657, 1663, 1667, 1669, 1693, 1697, 1699, 1709, 1721, 1723, 1733, 1741, 1747, 1753, 1759, 1777, 1783, 1787, 1789, 1801, 1811, 1823, 1831, 1847, 1861, 1867, 1871, 1873, 1877, 1879, 1889, 1901, 1907, 1913, 1931, 1933, 1949, 1951, 1973, 1979, 1987, 1993, 1997, 1999};
    BOOL esPrimo=FALSE;

    BOOL fichero = FALSE;
    char cad_plano[40]=TEXTO_SALIDA;
    FILE* o;
    char char_mpz[40];
    char char_primo[40];

    /* Control de errores */
    if ( argc != 5 && argc != 7) {
        printf("ERROR 1\n");
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    }
    if( argc == 7 && strcmp(argv[5], "-o") != 0) {
        printf("ERROR 2\n");
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    }
    if(argc == 5 && strcmp(argv[1], "-b") != 0 && strcmp(argv[3], "-p") != 0){
        printf("ERROR 3\n");
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    }
    if(argc == 7 && strcmp(argv[5], "-o") == 0){
        fichero = TRUE;
        strcat(cad_plano, argv[6]);
        o = fopen(cad_plano, "w");
    }

    nbits = atoi(argv[2]);
    prob = atoi(argv[4]);

    mpz_init(p);
    mpz_init(nbits_aux);

    /*Calcula el numero de veces que tiene que ejecutar el test*/
    /* nreps = log4((1-prob)*nbits*ln2/prob) */
    fprob = (float)prob;
    nreps = log(((1-(fprob/100))*nbits*log(2))/(fprob/100))/log(4);
    
    /*Pasamos el test nreps veces*/
    for(i=0; i<nreps;i++){
        while(esPrimo==FALSE){
            esPrimo = TRUE;
            /* Creamos numero primo aleatorio a partir de nbits */
            mpz_set_ui(nbits_aux, nbits);
            gmp_randinit_mt(state1);
            gmp_randseed_ui(state1, time(NULL)*getpid());
            mpz_urandomb(p, state1, nbits);
            gmp_randclear(state1);

            /*Iteramos los primos menores de 2000*/
            for(j=0;j<303;j++){
                if(mpz_cmp_ui(p, primos[j]) == 0){
                    esPrimo = TRUE;
                    break;
                }
                if(mpz_fdiv_ui(p, primos[j]) == 0) {
                    esPrimo = FALSE;
                    break;
                }
            }
            if(esPrimo == FALSE)
                continue;
            salida_primo = primo(p);
            if(salida_primo == 0) {
                esPrimo = FALSE;
                continue;
            }
        }
    }
        
    salida_mpz = mpz_probab_prime_p(p, nreps);

    gmp_printf("PRIMO: %Zd\n", p);
    printf("Seguridad del primo: probabilidad de equivocacion %0.2f y numero de veces que pasa el test %d\n", fprob/100, nreps);
    
    if(fichero==TRUE){
        if(salida_mpz == 1 || salida_mpz == 2)
            strcpy(char_mpz, "GMP: POSIBLE PRIMO\n");
        else if (salida_mpz == 0)
            strcpy(char_mpz, "GMP: COMPUESTO\n");
        
        if(salida_primo == 1)
            strcpy(char_primo, "TEST: POSIBLE PRIMO\n");
        else
            strcpy(char_primo, "TEST: COMPUESTO\n");
        
        printf("SALIDA POR FICHERO\n");
        fputs(char_mpz, o);
        fputs(char_primo, o);
        fclose(o);
    }
    else{
        if(salida_mpz==1 || salida_mpz == 2)
            printf("GMP: POSIBLE PRIMO\n");
        else if (salida_mpz == 0)
            printf("GMP: COMPUESTO\n");

        if(salida_primo==1)
            printf("TEST: POSIBLE PRIMO\n");
        else
            printf("TEST: COMPUESTO\n");
    }

    mpz_clear(p);
    mpz_clear(nbits_aux);
    
    
    return 0;
}