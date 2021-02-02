/**************************************************************
 * File: vegas.c 
 * Author: Aitor Melero, Ana Roa
 * Date: 10/12/2020
 * Last_Date: 13/12/2020
 * Function: Implementacion del A. las Vegas, apartado 2c de la P3. 
 * ***********************************************************/

#include "type.h"

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


/**************************************************************
 * Name: calcula_mk(mpz_t e, mpz_t d, mpz_t m, mpz_t k) 
 * Function: Calcula la m y la k para usarlas posteriormente. 
 * Parameters:
 *              mpz_t e: e.
 *              mpz_t d: d.
 *              mpz_t m: m a calcular.
 *              mpz_t k: k a calcular.
 * Return:
 *              void: Nada. 
 * ***********************************************************/
void calcula_mk(mpz_t e, mpz_t d, mpz_t m, mpz_t k) {
    mpz_t aux, aux2, modulo;
    BOOL encontrado = FALSE;
    long int i = 1;

    /* Inicializamos variables auxiliares */
    mpz_init(aux);
    mpz_init(aux2);
    mpz_set_ui(aux2, 1);
    mpz_init(modulo);

    /* ed - 1 */
    mpz_mul(aux, e, d);
    mpz_sub_ui(aux, aux, 1);

    /* Hacemos un bucle que vaya comprobando los valores m y k hasta encontrarlos */
    while (!encontrado)
    {
        mpz_mul_ui(aux2, aux2, 2);
        mpz_div(m, aux, aux2);
        mpz_mod_ui(modulo, m, 2);
        /* Comprobamos si se cumple "ed - 1 = 2^k m", tenemos solucion */
        if (mpz_cmp_ui(modulo, 0) != 0) {
            mpz_set_ui(k, i);
            encontrado = TRUE;
        } else {
            i++;
        }
    }
    
    /* Liberamos recursos */
    mpz_clear(aux);
    mpz_clear(aux2);
    mpz_clear(modulo);

}


/**************************************************************
 * Name: calcula_vegas(mpz_t n, mpz_t m, mpz_t a, mpz_t k, mpz_t p, mpz_t q) 
 * Function: Realiza los pasos 3, 4, 5 y 6 del algoritmo de las Vegas. 
 * Parameters:
 *              mpz_t n: n.
 *              mpz_t m: m.
 *              mpz_t a: a.
 *              mpz_t k: k.
 *              mpz_t p: p a calcular.
 *              mpz_t q: q a calcular.
 * Return:
 *              void: Nada. 
 * ***********************************************************/
void calcula_vegas(mpz_t n, mpz_t m, mpz_t a, mpz_t k, mpz_t p, mpz_t q) {
    mpz_t x, aux, y, i, aux2, aux3, mcd;
    BOOL encontrado = FALSE;

    /* Inicializamos variables */
    mpz_init(x);
    mpz_init(aux);
    mpz_init(aux2);
    mpz_init(aux3);
    mpz_init(y);
    mpz_init(i);
    mpz_init(mcd);

    mpz_gcd(mcd, a, n);

    /* Asignamos el valor de n - 1 para usarlo mas tarde */
    mpz_sub_ui(aux, n, 1);

    /* PASO 3 */
    if (mpz_cmp_ui(mcd, 1) > 0){
        mpz_set(p, mcd);
        mpz_div(q, n, p);
        encontrado = TRUE;
    } else {
        /* PASO 4 */
        mpz_powm(x, a, m, n);
        
        if (mpz_cmp_ui(x, 1) == 0){
            mpz_set_ui(p, 0);
            mpz_set_ui(q, 0);
            encontrado = TRUE;
        } else if (mpz_cmp(x, aux) == 0){
            mpz_set_ui(p, 0);
            mpz_set_ui(q, 0);
            encontrado = TRUE;
        }
    }

    /* PASO 5 */
    mpz_set_ui(i, 1);
    mpz_sub_ui(aux3, k, 1);
    mpz_set_ui(aux2, 2);

    while(mpz_cmp(i, aux3) <= 0 && encontrado == FALSE) {
        mpz_set(y, x);
        mpz_powm(x, x, aux2, n);

        if(mpz_cmp_ui(x, 1) == 0 && encontrado == FALSE){
            mpz_add_ui(y, y, 1);
            mpz_gcd(mcd, y, n);
            mpz_set(p, mcd);
            mpz_div(q, n, p);
            encontrado = TRUE;
        }

        if(mpz_cmp(x, aux) == 0 && encontrado == FALSE) {
            mpz_set_ui(p, 0);
            mpz_set_ui(q, 0);
            encontrado = TRUE;
        }

        mpz_add_ui(i, i, 1);
    }

    /* PASO 6 */
    if (encontrado == FALSE){
        mpz_add_ui(x, x, 1);
        mpz_gcd(mcd, x, n);
        if (mpz_cmp_ui(mcd, 1) == 0 || mpz_cmp(mcd, n) == 0) {
            mpz_set_ui(p, 0);
            mpz_set_ui(q, 0);
            encontrado = TRUE;
        } else {
            mpz_set(p, mcd);
            mpz_div(q, n, p);
            encontrado = TRUE;
        }
    }

    /* Liberamos recursos */
    mpz_clear(mcd);
    mpz_clear(x);
    mpz_clear(aux);
    mpz_clear(aux2);
    mpz_clear(aux3);
    mpz_clear(y);
    mpz_clear(i);

}


/* Funcion principal */
int main(int argc, char *argv[]) {
    mpz_t e, d, n, k, m, a, p, q, fn, mcd, p_aux, q_aux;
    gmp_randstate_t state;
    BOOL esPrimo = FALSE;
    BOOL tieneInverso = FALSE;
    BOOL tenemosFN = FALSE;
    int nbits;

    /* Control de errores para los argumentos */
    if (argc != 3){
        printf("ERROR al introducir parametros.\n./vegas -b <numero_bits>\n");
        return 1;
    }

    if (strcmp(argv[1], "-b") != 0) {
        printf("ERROR al introducir parametros.\n./vegas -b <numero_bits>\n");
        return 1;
    }

    /* Guardamos el numero de bits para hacer la prueba */
    nbits = atoi(argv[2])/2;

    mpz_init(e);
    mpz_init(d);
    mpz_init(n);
    mpz_init(k);
    mpz_init(m);
    mpz_init(a);
    mpz_init(p);
    mpz_init(q);
    mpz_init(fn);
    mpz_init(mcd);
    mpz_init(p_aux);
    mpz_init(q_aux);

    /* Inicializamos las variables a usar */
    gmp_randinit_mt(state);

    /* Modificamos el valor de la semilla */
    gmp_randseed_ui(state, time(NULL)*getpid());

    /* 1. Generar el primo p y el primo q */
    while (tenemosFN == FALSE){
        while(esPrimo == FALSE) {
            mpz_urandomb(p, state, nbits);
            if (mpz_probab_prime_p(p, 15) >= 1) {
                esPrimo = TRUE;
            }
        }

        esPrimo = FALSE;

        while(esPrimo == FALSE) {
            mpz_urandomb(q, state, nbits);
            if (mpz_probab_prime_p(q, 15) >= 1) {
                esPrimo = TRUE;
            }
        }

        esPrimo = FALSE;

        /* 2. Calculamos n = p*q */
        mpz_mul(n, p, q);

        /* 3. Calculamos fi(n) = (p-1)*(q-1) para hallar e y d */
        mpz_sub_ui(p_aux, p, 1);
        mpz_sub_ui(q_aux, q, 1);
        mpz_mul(fn, p_aux, q_aux);

        if (mpz_cmp_ui(fn, 2) > 0) {
            tenemosFN = TRUE;
        }
    }

    /* 4. Calculamos e (publica) y d (privada)*/
    while(tieneInverso == FALSE) {
        mpz_urandomm(e, state, fn);
        if (mpz_cmp_ui(e, 1) > 0 && mpz_cmp(e, fn) < 0) {
            mpz_invert(mcd, e, fn);
            if (mpz_cmp_ui(mcd, 0) > 0) {
                mpz_set(d, mcd);
                tieneInverso = TRUE;
            }
        }
    }

    gmp_printf("\np: %Zd\n", p);
    gmp_printf("\nq: %Zd\n", q);
    gmp_printf("\nn: %Zd\n", n);
    gmp_printf("\nfn: %Zd\n", fn);
    gmp_printf("\ne: %Zd\n", e);
    gmp_printf("\nd: %Zd\n", mcd);

    /* 5. A. las Vegas */
    calcula_mk(e,d,m,k);
    genera_a(n, a);
    calcula_vegas(n,m,a,k,p,q);

    printf("\n########################### SOLUCION ###############################\n");
    if (mpz_cmp_ui(p, 0) == 0 && mpz_cmp_ui(q, 0) == 0){
        printf("No se ha encontrado solucion. Intentalo de nuevo.\n");
    } else {
        gmp_printf("\np: %Zd\n", p);
        gmp_printf("\nq: %Zd\n", q);
    }

    mpz_clear(e);
    mpz_clear(d);
    mpz_clear(n);
    mpz_clear(k);
    mpz_clear(m);
    mpz_clear(a);
    mpz_clear(p);
    mpz_clear(q);
    mpz_clear(fn);
    mpz_clear(mcd);
    mpz_clear(p_aux);
    mpz_clear(q_aux);
    gmp_randclear(state);


    return 0;
}