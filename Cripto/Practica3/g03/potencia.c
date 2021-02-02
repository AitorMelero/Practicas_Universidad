#include "type.h"
#include <gmp.h>

/**************************************************************
 * Name: potencia(mpz_t base, mpz_t exponente, mpz_t modulo)
 * Function: Potenciación de grandes números.
 * Parameters:
 *              mpz_t base: Base.
 *              mpz_t exponente: Exponente.
 *              mpz_t modulo: Modulo.
 * Return:
 *              mpz_t valor calculado
 * ***********************************************************/
void potencia(mpz_t base, mpz_t exponente, mpz_t modulo){
    
    mpz_t x;
    mpz_t exponente_aux;
    mpz_t resultado;
    int l=0;
    int i;
    int* exponente_binario;

    /* l=numero digitos que tiene el numero en binario*/
    l=mpz_sizeinbase(exponente, 2);
    /*Reserva de memoria para el array que guarda el exponente en decimal*/
    exponente_binario = (int*)calloc(l, sizeof(int));

    /* x=1;*/
    mpz_init(x);
    mpz_set_ui(x, 1);
    /*Conversion del exponente de decimal a binario*/
    mpz_init(exponente_aux);
    mpz_init(resultado);
    mpz_set(exponente_aux, exponente);

    for(i=0; mpz_cmp_si(exponente_aux, 0)>0; i++){
        mpz_mod_ui(resultado, exponente_aux, 2);
        exponente_binario[i] = mpz_get_ui(resultado);
        mpz_div_ui(exponente_aux, exponente_aux, 2);
    }
    
    /*Calculo de x*/
    for (i=(l-1); i>=0; i--){
        /*x = x^2*/
        mpz_mul(x, x, x);
        /*x= x^2 mod modulo*/
        mpz_mod(x, x, modulo);
        if(exponente_binario[i]==1){
            /*x = x* base*/
            mpz_mul(x, x, base);
            /*x = x*base mod modulo*/
            mpz_mod(x, x, modulo);
        }
    }
    free(exponente_binario);
    
    gmp_printf("X: %Zd\n", x);
    mpz_clear(exponente_aux);
    mpz_clear(resultado);
    mpz_clear(x);
    mpz_clear(base);
    mpz_clear(exponente);
    mpz_clear(modulo);

    /*gmp_printf("(%Zd elevada a %Zd) mod %Zd\n", base, exponente, modulo);*/
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
    printf("./potenica <base> <exponente> <modulo>\n");
    printf("################################################\n");
    return;
}

/* MAIN */
int main(int argc, char *argv[]) {
    
    mpz_t base;
    mpz_t exponente;
    mpz_t modulo;
    
    mpz_t base_gmp;
    mpz_t exponente_gmp;
    mpz_t modulo_gmp;
    mpz_t x;

    /* Control de errores */
    if ( argc != 4 ) {
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    }

    mpz_init(base);
    mpz_init(base_gmp);
    mpz_set_str(base, argv[1], 10);
    mpz_set_str(base_gmp, argv[1], 10);

    mpz_init(exponente);
    mpz_init(exponente_gmp);
    mpz_set_str(exponente, argv[2], 10);
    mpz_set_str(exponente_gmp, argv[2], 10);

    mpz_init(modulo);
    mpz_init(modulo_gmp);
    mpz_set_str(modulo, argv[3], 10);
    mpz_set_str(modulo_gmp, argv[3], 10);
    
    mpz_init(x);
    mpz_set_ui(x, 1);
    
    potencia(base, exponente, modulo);
    mpz_powm(x, base_gmp, exponente_gmp, modulo_gmp);
    gmp_printf("mpz_powm: %Zd\n", x);
    
    
    mpz_clear(base_gmp);
    mpz_clear(exponente_gmp);
    mpz_clear(modulo_gmp);
    mpz_clear(x);
    
    return 0;
}