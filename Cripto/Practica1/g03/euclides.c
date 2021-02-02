/**************************************************************
 * File: euclides.c
 * Author: Aitor Melero, Ana Roa
 * Date: 05/10/2020
 * Last_Date: 06/10/2020
 * Function: Implementacion de funcionalidad de Euclides con GMP.
 * ***********************************************************/


/* INCLUDES */
#include "euclides.h"


/*************************************************************/

/* FUNCIONES PRIVADAS*/

/**************************************************************
 * Name: add_r(EUCLIDES* euclides, mpz_t new_r)
 * Function: Aniade un resto a euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 *              mpz_t new_r: Resto a aniadir.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void add_r(EUCLIDES* euclides, mpz_t new_r) {
    /* Comprobamos si r ya tiene memoria asignada */
    if (!euclides) {
        printf("Error al aniadir r, Euclides no inicializado!!!\n");
        return;
    } else if (!euclides->r) {
        euclides->num_r++;
        euclides->r = (mpz_t *)calloc(euclides->num_r, sizeof(mpz_t));
        mpz_set(euclides->r[0], new_r);
    } else {
        euclides->num_r++;
        euclides->r = (mpz_t *)realloc(euclides->r, euclides->num_r*sizeof(mpz_t));
        mpz_init_set(euclides->r[euclides->num_r-1], new_r);
    }
}


/*************************************************************/

/**************************************************************
 * Name: add_q(EUCLIDES* euclides, mpz_t new_q)
 * Function: Aniade un factor a euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 *              mpz_t new_q: Factor a aniadir.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void add_q(EUCLIDES* euclides, mpz_t new_q) {
    /* Comprobamos si q ya tiene memoria asignada */
    if (!euclides) {
        printf("Error al aniadir q, Euclides no inicializado!!!\n");
        return;
    } else if (!euclides->q) {
        euclides->num_q++;
        euclides->q = (mpz_t *)calloc(euclides->num_q, sizeof(mpz_t));
        mpz_set(euclides->q[0], new_q);
    } else {
        euclides->num_q++;
        euclides->q = (mpz_t *)realloc(euclides->q, euclides->num_q*sizeof(mpz_t));
        mpz_init_set(euclides->q[euclides->num_q-1], new_q);
    }
}


/*************************************************************/

/**************************************************************
 * Name: add_solucion(EUCLIDES* euclides, char* new_solucion)
 * Function: Aniade una ecuacion nueva a la solucion a euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 *              char* new_solucion: Ecuacion a aniadir.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void add_solucion(EUCLIDES* euclides, char* new_solucion) {
    int tamanio = strlen(new_solucion)+1;

    /* Comprobamos si q ya tiene memoria asignada */
    if (!euclides) {
        printf("Error al aniadir solucion, Euclides no inicializado!!!\n");
        return;
    } else if (!euclides->solucion) {
        euclides->num_solucion++;
        euclides->solucion = (char**)calloc(euclides->num_solucion, sizeof(char*));
        euclides->solucion[0] = (char*)calloc(tamanio, sizeof(char));
        strcpy(euclides->solucion[0], new_solucion);
    } else {
        euclides->num_solucion++;
        euclides->solucion = (char**)realloc(euclides->solucion, euclides->num_solucion*sizeof(char*));
        euclides->solucion[euclides->num_solucion-1] = (char *)calloc(tamanio, sizeof(char));
        strcpy(euclides->solucion[euclides->num_solucion-1], new_solucion);
    }
}


/*************************************************************/

/**************************************************************
 * Name: generate_equation(mpz_t r0, mpz_t r1, mpz_t r2, mpz_t q)
 * Function: Genera ecuacion a ecuacion en formato cadena.
 * Parameters:
 *              mpz_t r0: R0.
 *              mpz_t r1: R1.
 *              mpz_t r2: R2.
 *              mpz_t q: Q.
 * Return:
 *              char*: Ecuacion en formato cadena.
 * ***********************************************************/
char* generate_equation(mpz_t r0, mpz_t r1, mpz_t r2, mpz_t q) {
    char* equation = (char*)calloc(1000, sizeof(char));
    
    gmp_sprintf(equation, "%Zd = %Zd * %Zd + %Zd", r0, q, r1, r2);

    return equation;
}


/*************************************************************/

/**************************************************************
 * Name: calculate_euclides_extendido(mpz_t m, mpz_t a, mpz_t* x, mpz_t* y)
 * Function: Calcula el inverso usando euclides extendido. Partimos de
 *           la ecuacion d = m*x + a*y.
 * Parameters:
 *              mpz_t m: Elemento m en ec. Euclides.
 *              mpz_t a: Elemento a en ec. Euclides.
 *              mpz_t* x: Elemento x en ec. Euclides (inverso).
 *              mpz_t* y: Elemento y en ec. Euclides.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void calculate_euclides_extendido(mpz_t m, mpz_t a, mpz_t* x, mpz_t* y) {
    mpz_t x_aux;
    mpz_t y_aux;
    mpz_t a_aux;

    mpz_init_set_si(x_aux, 0);
    mpz_init_set_si(y_aux, 0);
    mpz_init_set_si(a_aux, 0);

    if (mpz_cmp_si(a, 0) == 0) {
        mpz_set_si(*x, 1);
        mpz_set_si(*y, 0);
    } else {
        mpz_mod(a_aux, m, a);
        calculate_euclides_extendido(a, a_aux, x, y);
        mpz_set(x_aux, *x);
        mpz_set(y_aux, *y);
        mpz_set(*x, y_aux);
        /* y = x_aux - (m/a) * y_aux */
        mpz_tdiv_q(a_aux, m, a);
        mpz_mul(a_aux, a_aux, y_aux);
        mpz_sub(*y, x_aux, a_aux);
    }

    /* Liberamos recursos */
    mpz_clear(x_aux);
    mpz_clear(y_aux);
    mpz_clear(a_aux);
}


/*************************************************************/

/**************************************************************
 * Name: calculate_euclides(EUCLIDES* euclides)
 * Function: Calcula el mcd usando euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void calculate_euclides(EUCLIDES* euclides) {
    /* Declaracion de variables */
    mpz_t r0;
    mpz_t r1;
    mpz_t q;
    mpz_t r_aux;
    mpz_t x_aux;
    char* equation = NULL;

    /* Asignacion de variables */
    mpz_init(r0);
    mpz_set(r0, euclides->m);
    mpz_init(r1);
    mpz_set(r1, euclides->a);
    mpz_init(q);
    mpz_set_str(q, "1", 10);
    mpz_init(r_aux);
    mpz_set_str(r_aux, "1", 10);
    mpz_init(x_aux);
    mpz_set_str(x_aux, "0", 10);

    /* Algoritmo de Euclides */
    while (mpz_cmp_si(r_aux, 0) != 0) {
        mpz_tdiv_q(q, r0, r1);    /* q = r0 / r1 */
        add_q(euclides, q);
        mpz_set(euclides->mcd, r_aux);    /* euclides->mcd = r_aux */
        mpz_mod(r_aux, r0, r1);    /* r_aux = r0 % r1 */
        if (euclides->num_solucion == 0 && mpz_cmp_si(r_aux, 0) == 0) {
            mpz_set(euclides->mcd, r1);    /* euclides->mcd = r1 */
        }
        add_r(euclides, r_aux);
        equation = generate_equation(r0, r1, r_aux, q);
        add_solucion(euclides, equation);
        free(equation);
        mpz_set(r0, r1);    /* r0 = r1 */
        mpz_set(r1, r_aux);    /* r1 = r_aux */
    }

    /* Si el mcd es 1, calculamos el inverso ya que existe */
    if (mpz_cmp_si(euclides->mcd, 1) == 0) {
        calculate_euclides_extendido(euclides->m, euclides->a, &x_aux, &euclides->inv);
    }

    /* Aniadimos la solucion final */
    equation = (char*)calloc(500, sizeof(char));
    gmp_sprintf(equation, "\nmcd(%Zd, %Zd)=%Zd\ninverso=%Zd\n", euclides->m, euclides->a, euclides->mcd, euclides->inv);
    add_solucion(euclides, equation);

    free(equation);
    mpz_clear(r0);
    mpz_clear(r1);
    mpz_clear(q);
    mpz_clear(r_aux);
    mpz_clear(x_aux);
}


/* FUNCIONES PUBLICAS*/

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_euclides(mpz_t m, mpz_t a)
 * Function: Crear un elemento de tipo EUCLIDES.
 * Parameters:
 *              mpz_t m: Modulo a usar para Euclides.
 *              mpz_t a: Numero a usar para Euclides.
 * Return:
 *              EUCLIDES: Elemento EUCLIDES creado.
 * ***********************************************************/
EUCLIDES* create_euclides(mpz_t m, mpz_t a) {
    EUCLIDES* euclides = NULL;

    /* Creamos el objeto EUCLIDES */
    euclides = (EUCLIDES *)calloc(1, sizeof(EUCLIDES));
    if (!euclides) {
        printf("Error creando objeto EUCLIDES.\n");
        return NULL;
    }

    /* Inicializamos los elementos de GMP */
    mpz_init(euclides->m);
    mpz_init(euclides->a);
    mpz_init(euclides->mcd);
    mpz_init(euclides->inv);

    /* Inicializamos los elementos de euclides */
    mpz_set(euclides->m, m);
    mpz_set(euclides->a, a);
    euclides->r = NULL;
    euclides->num_r = 0;
    add_r(euclides, m);
    add_r(euclides, a);
    euclides->q = NULL;
    euclides->num_q = 0;
    euclides->solucion = NULL;
    euclides->num_solucion = 0;
    mpz_set_str(euclides->mcd, "0", 10);
    mpz_set_str(euclides->inv, "0", 10);

    /* Calculamos el mcd con el algoritmo de Euclides */
    calculate_euclides(euclides);

    return euclides;
}


/*************************************************************/

/**************************************************************
 * Name: free_euclides(EUCLIDES* euclides)
 * Function: Liberar un elemento de tipo EUCLIDES.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES a liberar.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_euclides(EUCLIDES* euclides) {
    int i;

    /* Liberamos todos los elementos de Euclides */
    if (euclides) {
        mpz_clear(euclides->m);
        mpz_clear(euclides->a);
        mpz_clear(euclides->mcd);
        mpz_clear(euclides->inv);
        if (euclides->r) {
            for (i=0; i<euclides->num_r; i++) {
                mpz_clear(euclides->r[i]);
            }
            free(euclides->r);
        }
        if (euclides->q) {
            for (i=0; i<euclides->num_q; i++) {
                mpz_clear(euclides->q[i]);
            }
            free(euclides->q);
        }
        if (euclides->solucion) {
            for(i=0; i<euclides->num_solucion; i++) {
                if (euclides->solucion[i]) {
                    free(euclides->solucion[i]);
                }
            }
            free(euclides->solucion);
        }
        free(euclides);
    }
}


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/**************************************************************
 * Name: get_solucion(EUCLIDES* euclides)
 * Function: Devuelve una cadena con la solucion de euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 * Return:
 *              char**: Cadena con la solucion de euclides.
 * ***********************************************************/
char** get_solucion(EUCLIDES* euclides) {
    if (!euclides) {
        printf("Error al devolver la solucion!!!\n");
        return NULL;
    }
    if (!euclides->solucion) {
        printf("Error al devolver la solucion!!!\n");
        return NULL;
    }

    return euclides->solucion;
}


/*************************************************************/

/**************************************************************
 * Name: print_euclides(EUCLIDES* euclides)
 * Function: Imprime la solucion de euclides.
 * Parameters:
 *              EUCLIDES* euclides: Elemento EUCLIDES.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void print_euclides(EUCLIDES* euclides) {
    int i;

    for(i=0; i<euclides->num_solucion; i++) {
        printf("%s\n", euclides->solucion[i]);
    }
}