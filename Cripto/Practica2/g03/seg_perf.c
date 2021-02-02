/**************************************************************
 * File: seg_perf.c
 * Author: Aitor Melero, Ana Roa
 * Date: 12/11/2020
 * Last_Date: 12/11/2020
 * Function: Demostración de Seguridad Perfecta del cifrado por
 *           desplazamiento
 * ***********************************************************/


/* INCLUDES */
#include "seg_perf.h"
#include <time.h>


/* DEFINES */
#define TEXTO_PLANO "Textos_Planos/"
#define TEXTO_CIFRADO "Textos_Cifrados/"

/* FUNCIONES */

/*************************************************************/
/**************CONSTRUCTOR Y LIBERADOR************************/
/*************************************************************/

/**************************************************************
 * Name: create_segperf(FILE* i, FILE* o)
 * Function: Crear un elemento de tipo SEGPERF.
 * Parameters:
 *              FILE* i: Fichero de entrada.
 *              FILE* o: Fichero de salida.
 * Return:
 *              SEGPERF: Elemento SEGPERF creado.
 * ***********************************************************/
SEGPERF* create_segperf(FILE* i, FILE* o){
    
    SEGPERF* sg = NULL;
    sg = (SEGPERF*) calloc (1, sizeof(SEGPERF));
    if (!sg) {
        printf("Error creando objeto SEGPERF\n");
        return NULL;
    }
    sg->i = i;
    sg->o = o;
    
    return sg;
}


/*************************************************************/

/**************************************************************
 * Name: free_segperf(SEGPERF* sg)
 * Function: Liberar un elemento de tipo SEGPERF.
 * Parameters:
 *              SEGPERF* SEGPERF: Elemento de tipo segperf.
 * Return:
 *              void: Nada.
 * ***********************************************************/
void free_segperf(SEGPERF* sg){
    if(sg) {
        if (sg->i){
            fclose(sg->i);
        }  
        if (sg->o){
            fclose(sg->o);
        }
        free(sg);
    }
}


/*************************************************************/

/*************************************************************/
/***************************GETTERS***************************/
/*************************************************************/

/*************************************************************/

/**************************************************************
 * Name: get_input_file(SEGPERF* sg)
 * Function: Devuelve el fichero de entrada usado en sg.
 * Parameters:
 *              SEGPERF* SEGPERF: Elemento de tipo sg.
 * Return:
 *              FILE*: Fichero de entrada usado en sg.
 * ***********************************************************/
FILE* get_input_file(SEGPERF* sg){
    if(!sg){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    if(!sg->i){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    return sg->i;
}


/*************************************************************/

/**************************************************************
 * Name: get_output_file(SEGPERF* sg)
 * Function: Devuelve el fichero de salida usado en sg.
 * Parameters:
 *              SEGPERF* SEGPERF: Elemento de tipo sg.
 * Return:
 *              FILE*: Fichero de salida usado en sg.
 * ***********************************************************/
FILE* get_output_file(SEGPERF* sg){
    if(!sg){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    if(!sg->o){
        printf("Error al devolver el fichero de entrada\n");
        return NULL;
    }
    return sg->o;
}


/*************************************************************/

/*************************************************************/
/*****************CIFRADO Y DESCIFRADO************************/
/*************************************************************/

/**************************************************************
 * Name: equiprobable(SEGPERF* sg)
 * Function: Cifra un fichero usando cifrado sg equiprobable.
 * Parameters:
 *              SEGPERF* sg: Elemento de tipo sg.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS equiprobable(SEGPERF* sg){

    int clave =0;
    int min = 0;
    int max = 25;
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    char valor_aux2[100];
    char prob_aux[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    double pp[26]; /*Probabilidad texto plano*/
    double pc[26]; /*Probabilidad texto cifrado*/
    double pk[26]; /*Probabilidad claves*/
    double pcond[26][26]; /*Probabilidad Y|X*/
    double presultado[26][26];
    int i, j, contador=0;
    double total_caracteres=0.0;

    for(i=0; i<26;i++){
        pp[i]=0.0;
        pc[i]=0.0;
        pk[i]=0.0;
        for (j=0;j<26;j++){
            pcond[i][j]=0.0;
        }
    }

    mpz_init(valor_aux);

    srand(time(NULL));

    while (fread(&caracter_cifrado, 1, 1, sg->i) == 1) {
    if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
        total_caracteres+=1.0;
        }
    }
    fseek(sg->i, 0L, SEEK_SET);

    /*Leemos el fichero y le sumamos la clave, caracter a caracter*/
    while (fread(&caracter_cifrado, 1, 1, sg->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            /*Calculamos la probabilidad del texto plano*/
            gmp_sprintf(prob_aux, "%Zd", valor_aux);
            /*Pp(X)*/
            pp[atoi(prob_aux)]+=1.0;
            /*Hallamos la clave de forma aleatoria*/
            clave = (rand() % (max - min + 1)) + min;
            /*Probabilidad clave*/
            /*pk[clave]=pk[clave] + (probabilidad/total_caracteres);*/
            pk[clave]+=1.0;
            /*x + k*/
            mpz_add_ui(valor_aux, valor_aux, clave);
            /* x + k mod m*/
            mpz_mod_ui(valor_aux, valor_aux, 26);
            /* y  = x + k*/
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            /*Pc(Y)*/
            pc[atoi(valor_aux2)]+=1.0;
            caracter_claro = atoi(valor_aux2) + 65;
            /*p(Y|X)*/
            if(pcond[atoi(valor_aux2)][atoi(prob_aux)]==0.0){
                pcond[atoi(valor_aux2)][atoi(prob_aux)]+=pk[clave];
            }

            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 1, sg->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, sg->o);
        }
        offset_escritura++;
        fseek(sg->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(sg->i, offset_lectura, SEEK_SET);
    }


    printf("Probabiliad Pp(X): \n");
    for(i=0; i<26;i++){
        pp[i]=pp[i]/total_caracteres;
        if(pp[i]!=0)
            printf("Pp(%c)=%0.3f \n", (i+65), pp[i]);
        
    }
    printf("\n");
    printf("Probabiliad Pc(Y): \n");
    for(i=0; i<26;i++){
        pc[i]=pc[i]/total_caracteres;
        if(pc[i]!=0)
            printf("Pc(%c)=%0.3f \n", (i+65), pc[i]);
    }
    printf("\n");
    printf("Probabiliad Pk(K): \n");
    for(i=0; i<26;i++){
        pk[i]=pk[i]/total_caracteres;
        if(pk[i]!=0)
            printf("Pk(%c)=%0.3f \n", (i+65), pk[i]);
    }
    
    for(i=0;i<26;i++){
        for(j=0;j<26;j++){
            pcond[j][i] = pcond[j][i]/total_caracteres;
        }
    }

    printf("Probabilidades Pp(X|Y):\n");
    for(i=0; i<26;i++){
        printf("Pp(%c)=%0.3f \n", (i+65), pp[i]);
        for(j=0;j<26;j++){
            if (pc[j]!=0){
                presultado[i][j] = (pcond[j][i]*pp[i]) / pc[j];
            }
            else{
                presultado[i][j]=0.0;
            }
            printf("Pp(%c|%c)=%0.5f \n", (i+65), (j+65), presultado[i][j]);
        }
        printf("\n");
    }

    printf("\n");

    for(i=0; i<26;i++){
        for(j=0;j<26;j++){
            if (presultado[i][j] <= pp[i] + 0.05 || presultado[i][j]  >= pp[i] - 0.05) {
                contador++;
            }
        }
    }
    if (contador==676){
        printf("Se cumple seguridad perfecta \n");
    }
    else{
        printf("NO\n");
    }

    mpz_clear(valor_aux);

    return OK;
}


/*************************************************************/

/**************************************************************
 * Name: no_equiprobable(SEGPERF* sg)
 * Function: Cifra un fichero usando cifrado sg no equiprobable.
 * Parameters:
 *              SEGPERF* sg: Elemento de tipo sg.
 * Return:
 *              STATUS: OK, en caso correcto. ERROR, en otro caso.
 * ***********************************************************/
STATUS no_equiprobable(SEGPERF* sg){

    int clave =0;
    int min = 0;
    int max = 41;
    char caracter_claro;
    char caracter_cifrado;
    mpz_t valor_aux;
    char valor_aux2[100];
    char prob_aux[100];
    long int offset_lectura = 0;
    long int offset_escritura = 0;
    int claves[41] = {0, 11, 12, 3, 4, 5, 16, 7, 8, 9, 0, 11, 22, 3, 14, 1, 16, 7, 1, 19, 0, 21, 15, 3, 24, 5, 15, 1, 25, 3, 17, 8, 10, 14, 6, 0, 25, 8, 5, 3};
    double pp[26]; /*Probabilidad texto plano*/
    double pc[26]; /*Probabilidad texto cifrado*/
    double pk[26]; /*Probabilidad claves*/
    double pcond[26][26]; /*Probabilidad Y|X*/
    double presultado[26][26];
    double total_caracteres=1.0;
    int i, j;
    int contador=0;

    for(i=0; i<26;i++){
        pp[i]=0.0;
        pc[i]=0.0;
        pk[i]=0.0;
        for (j=0;j<26;j++){
            pcond[i][j]=0.0;
        }
    }
    mpz_init(valor_aux);

    while (fread(&caracter_cifrado, 1, 1, sg->i) == 1) {
    if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
        total_caracteres+=1.0;
        }
    }
    fseek(sg->i, 0L, SEEK_SET);

    srand(time(NULL));

    /*Leemos el fichero y le sumamos la clave, caracter a caracter*/
    while (fread(&caracter_cifrado, 1, 1, sg->i) == 1) {
        if (caracter_cifrado >= 65 && caracter_cifrado <= 90) {
            /* y-b */
            mpz_set_si(valor_aux, (long int) caracter_cifrado - 65);
            /*Calculamos la probabilidad del texto plano*/
            gmp_sprintf(prob_aux, "%Zd", valor_aux);
            /*Pp(X)*/
            pp[atoi(prob_aux)]+=1.0;
            /*Hallamos la clave de forma aleatoria*/
            clave = (rand() % (max - min + 1)) + min;
            
            pk[claves[clave]]+=1.0;
            /*x + claves[clave]*/
            mpz_add_ui(valor_aux, valor_aux, claves[clave]);
            /* x + k mod m*/
            mpz_mod_ui(valor_aux, valor_aux, 26);
            /* y  = x + k*/
            gmp_sprintf(valor_aux2, "%Zd", valor_aux);
            /*Pc(Y)*/
            pc[atoi(valor_aux2)]+=1.0;
            caracter_claro = atoi(valor_aux2) + 65;
            /*p(Y|X)*/
            if(pcond[atoi(valor_aux2)][atoi(prob_aux)]==0.0)
                pcond[atoi(valor_aux2)][atoi(prob_aux)]+=pk[claves[clave]];
            /* vamos escribiendo caracter a caracter */
            fwrite(&caracter_claro, 1, 1, sg->o);
        } else {
            fwrite(&caracter_cifrado, 1, 1, sg->o);
        }
        offset_escritura++;
        fseek(sg->o, offset_escritura, SEEK_SET);
        offset_lectura++;
        fseek(sg->i, offset_lectura, SEEK_SET);
    }

    printf("Probabiliad Pp(X): \n");
    for(i=0; i<26;i++){
        pp[i]=pp[i]/total_caracteres;
        printf("Pp(%c)=%0.3f \n", (i+65), pp[i]);
        
    }
    printf("\n");
    printf("Probabiliad Pc(Y): \n");
    for(i=0; i<26;i++){
        pc[i]=pc[i]/total_caracteres;
        printf("Pc(%c)=%0.3f \n", (i+65), pc[i]);
    }
    printf("\n");
    printf("Probabiliad Pk(K): \n");
    for(i=0; i<26;i++){
        pk[i] = pk[i] / total_caracteres;
        printf("Pk(%c)=%0.3f \n", (i+65), pk[i]);
    }
    for(i=0;i<26;i++){
        for(j=0;j<26;j++){
            pcond[j][i] = pcond[j][i]/total_caracteres;
        }
    }

    printf("\n");
    printf("Probabilidades Pp(X|Y)\n");
    for(i=0; i<26;i++){
        for(j=0;j<26;j++){
            if (pc[j]!=0){
                presultado[i][j] = (pcond[j][i]*pp[i]) / pc[j];
            }
            else{
                presultado[i][j]=0.0;
            }
            printf("Pp(%c|%c)=%0.5f \n", (i+65), (j+65), presultado[i][j]);
        }
        printf("\n");
    }

    printf("\n");

    for(i=0; i<26;i++){
        for(j=0;j<26;j++){
            if (presultado[i][j] >= pp[i] + 0.05 || presultado[i][j]  <= pp[i] - 0.05) {
                contador++;
            }
        }
    }
    if (contador==676){
        printf("Se cumple seguridad perfecta \n");
    }
    else{
        printf("NO se cumple seguridad perfecta\n");
    }

    mpz_clear(valor_aux);

    return OK;
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
    printf("./seg_perf -P/-I -i <fichero entrada> -o <fichero salida>\n");
    printf("\nSiendo -i y -o opcionales.\n");
    printf("################################################\n");
    return;
}

/* MAIN */
int main(int argc, char *argv[]) {
    FILE* i = NULL;                         /* fichero de entrada */
    FILE* o = NULL;                         /* fichero de salida */
    char cad_plano[40] = TEXTO_PLANO;       /* cadena que representa un path */
    char cad_cifrado[40] = TEXTO_CIFRADO;   /* cadena que representa un path */
    SEGPERF* sg = NULL;                      /* objeto seguridad perfecta */
    BOOL salida = FALSE;                    /* indica si se usa salida estandar o no */
    char texto[1024] = "";                  /* texto a usar en caso de entrada estandar */
    char salida_pantalla[1024];             /* texto a mostrar en la pantalla en caso de pedirlo */


    /* Control de errores */
    if (argc != 2 && argc != 4 && argc != 6) {
        printf("Numero de comandos introducido incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (strcmp(argv[1], "-P") != 0 && strcmp(argv[1], "-I") != 0) {
        printf("Primer argumento incorrecto.\n");
        muestra_info_parametros();
        return 1;
    } else if (argc == 4) {
        if (strcmp(argv[2], "-i") != 0 && strcmp(argv[2], "-o") != 0) {
            printf("Segundo argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        }
    } else if (argc == 6) {
        if (strcmp(argv[2], "-i") != 0) {
            printf("Segundo argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        } else if (strcmp(argv[4], "-o") != 0) {
            printf("Cuarto argumento incorrecto.\n");
            muestra_info_parametros();
            return 1;
        }
    }

    /* Abrimos los ficheros en funcion de los argumentos */
    if (argc == 6) {
        if (strcmp(argv[1], "-P") == 0) {
            strcat(cad_plano, argv[3]);
            strcat(cad_cifrado, argv[5]);
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-I") == 0) {
            strcat(cad_plano, argv[3]);
            strcat(cad_cifrado, argv[5]);
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        }
    } else if (argc == 4) {
        if (strcmp(argv[1], "-P") == 0 && strcmp(argv[2], "-i") == 0) {
            salida = TRUE;
            strcat(cad_plano, argv[3]);
            strcat(cad_cifrado, "salida.txt");
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-P") == 0 && strcmp(argv[2], "-o") == 0) {
            strcat(cad_plano, "entrada.txt");
            strcat(cad_cifrado, argv[3]);
            o = fopen(cad_cifrado, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_plano, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_plano, "r");
        } else if (strcmp(argv[1], "-I") == 0 && strcmp(argv[2], "-i") == 0) {
            salida = TRUE;
            strcat(cad_plano, argv[3]);
            strcat(cad_cifrado, "salida.txt");
            i = fopen(cad_plano, "r");
            o = fopen(cad_cifrado, "w");
        } else if (strcmp(argv[1], "-I") == 0 && strcmp(argv[2], "-o") == 0) {
            strcat(cad_plano, "entrada.txt");
            strcat(cad_cifrado, argv[3]);
            o = fopen(cad_cifrado, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_plano, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_plano, "r");
        }
    } else {
        if (strcmp(argv[1], "-P") == 0) {
            salida = TRUE;
            strcat(cad_plano, "entrada.txt");
            strcat(cad_cifrado, "salida.txt");
            o = fopen(cad_cifrado, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_plano, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_plano, "r");
        } else if (strcmp(argv[1], "-I") == 0) {
            salida = TRUE;
            strcat(cad_plano, "salida.txt");
            strcat(cad_cifrado, "entrada.txt");
            o = fopen(cad_plano, "w");
            printf("Introduce el texto a cifrar:\n");
            i = fopen(cad_cifrado, "w");
            fgets(texto, 1024, stdin);
            fputs(texto, i);
            fclose(i);
            i = fopen(cad_cifrado, "r");
        }
        
    }
    
    /* Si no existe el fichero a abrir, error */
    if (!i) {
        printf("No existe el fichero de entrada.\n");
    }
    sg = create_segperf(i, o);

    /* Llamamos a la funcion equiprobable o no equiprobable segun el parametro */
    if (sg) {
        if (strcmp(argv[1], "-P") == 0) {
            if (equiprobable(sg) == OK && salida == 1) {
                /* Mostrar mensaje cifrado por pantalla */
                fclose(sg->o);
                sg->o = fopen(cad_cifrado, "r");
                while(fgets(salida_pantalla, 1024, (FILE*) sg->o)) {
                    fputs(salida_pantalla, stdout);
                }
                printf("\n");
            }
        } else {
            if (no_equiprobable(sg) == OK && salida == 1) {
                /* Mostrar mensaje descifrado por pantalla */
                fclose(sg->o);
                sg->o = fopen(cad_cifrado, "r");
                while(fgets(salida_pantalla, 1024, (FILE*) sg->o)) {
                    fputs(salida_pantalla, stdout);
                }
                printf("\n");
            }
        }
    }

    /* Cerramos recursos */
    free_segperf(sg);


    return 0;
}