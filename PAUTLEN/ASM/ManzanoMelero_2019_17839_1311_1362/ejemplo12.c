#include<stdio.h>
#include <stdlib.h>
#include <string.h>
#include "generacion.h"

int main (int argc, char ** argv)
{
	/*
	 *

main {
	int m;
	int [4] v;
	bool [4] vb;
	
	m = 0;
	while ( m < 4 )
	{
		printf m;
		v[m] = m*10;
		vb[m] =  ! ( m > 2 );
		m = m + 1;
	}

	m = 0;
	while ( m < 4 )
	{
		printf m;
		if ( m < 2 ) 
			printf v[m];
		else 
			printf vb[m];
		m = m + 1;
	}
}

SALIDA ESPERADA 
0
1
2
3
0
0
1
10
2
true
3
false

	 *
	 *
	 */

    	if (argc != 2) {fprintf (stdout, "ERROR POCOS ARGUMENTOS\n"); return -1;}

	int etiqueta_logicas = 0;
    	int etiqueta = 0;
    	int getiqueta = -1;
    	int etiquetas[3];
    	int cima_etiquetas=-1;
    	FILE * fd_asm;

    	fd_asm = fopen(argv[1],"w");
    	escribir_subseccion_data(fd_asm);
    	escribir_cabecera_bss(fd_asm);

    	/*int m;*/
    	declarar_variable(fd_asm,"m", 1, 1);

    	/*int [4] v;*/
    	declarar_variable(fd_asm, "v",  1, 4);

    	/*int [4] vb;*/
    	declarar_variable(fd_asm, "vb",  1, 4);

    	escribir_segmento_codigo(fd_asm);
    	escribir_inicio_main(fd_asm);

    	/*m=0;*/
    	escribir_operando(fd_asm,"0",0);
    	asignar(fd_asm,"m",0);


       /*While. Gestion inicial de las etiquetas, guardado de etiqueta.*/
    	getiqueta++;
    	cima_etiquetas++;
    	etiquetas[cima_etiquetas]=getiqueta;

    	/*Inicio del while. Impresion de la etiqueta.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	while_inicio(fd_asm, etiqueta);

    	/*Condicion del bucle while.
	// ... ( m < 4 )*/
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"4",0);
    	menor(fd_asm,1,0,etiqueta_logicas++);

    	/*Recuperamos la etiqueta para imprimir la comparacion del while.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	while_exp_pila(fd_asm, 0, etiqueta);

    	/*printf m*/
    	escribir_operando(fd_asm,"m",1);
    	escribir(fd_asm,1,ENTERO);

    	/*v[m] = m*10;*/
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"10",0);
    	multiplicar(fd_asm,1,0);
    	escribir_operando(fd_asm,"m",1);
    	escribir_elemento_vector(fd_asm,"v", 4, 1);
    	asignarDestinoEnPila(fd_asm,0);

	/*vb[m] =  ! ( m > 2 );*/
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"2",0);
    	mayor(fd_asm,1,0,etiqueta_logicas++);
	no(fd_asm, 0, etiqueta_logicas++);
    	escribir_operando(fd_asm,"m",1);
    	escribir_elemento_vector(fd_asm,"vb", 4, 1);
    	asignarDestinoEnPila(fd_asm,0);


	
	
	
	
        /*m = m + 1*/
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"1",0);
    	sumar(fd_asm,1,0);
    	asignar(fd_asm,"m",0);

    	/*Recuperamos la etiqueta para imprimir el fin de etiqueta del while.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	while_fin(fd_asm, etiqueta);

    	/*Al cerrar el ámbito, decrementamos el contador de cima de etiquetas.*/
    	cima_etiquetas--;

	/*
	m = 0;
	while ( m < 4 )
	{
		printf m;
		if ( m < 2 ) 
			printf v[m];
		else 
			printf vb[m];
		m = m + 1;
	}
	*/

	/* m = 0;*/
    	escribir_operando(fd_asm,"0",0);
    	asignar(fd_asm,"m",0);

        /*While. Gestion inicial de las etiquetas, guardado de etiqueta.*/
    	getiqueta++;
    	cima_etiquetas++;
    	etiquetas[cima_etiquetas]=getiqueta;

    	/*Inicio del while. Impresion de la etiqueta.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	while_inicio(fd_asm, etiqueta);

    	/*Condicion del bucle while.
	// ... ( m < 4 )*/
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"4",0);
    	menor(fd_asm,1,0,etiqueta_logicas++);

    	/*Recuperamos la etiqueta para imprimir la comparacion del while.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	while_exp_pila(fd_asm, 0, etiqueta);


	/* printf m;*/
	escribir_operando(fd_asm,"m",1);
	escribir(fd_asm,1,0);


	/* if ( m < 2 ) */
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"2",0);
	menor(fd_asm,1,0,etiqueta_logicas++);


    	/*Gestion de etiquetas para abrir el ambito del if. Esta sera la etiqueta que tenga.*/
    	getiqueta++;
    	cima_etiquetas++;
    	etiquetas[cima_etiquetas]=getiqueta;
    	etiqueta = getiqueta;
    	ifthen_inicio(fd_asm, 0, etiqueta);


	/*	printf v[m];*/
	escribir_operando(fd_asm,"m",1);
    	escribir_elemento_vector(fd_asm,"v", 4, 1);
	escribir(fd_asm,1,0);


	/* else*/

    	/*Salto a fin sino al terminar el if, impresion de la etiqueta fin_si. Recogemos la etiqueta.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	ifthenelse_fin_then(fd_asm, etiqueta);



	/* 	printf vb[m];*/
	escribir_operando(fd_asm,"m",1);
    	escribir_elemento_vector(fd_asm,"vb", 4, 1);
	escribir(fd_asm,1,1);
	
    	/*Fin del condicional if. Imprimimos la etiqueta de ambito del fin del condicional y restamos el contador.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	ifthenelse_fin(fd_asm, etiqueta);
    	cima_etiquetas--;
	
        /*m = m + 1*/
    	escribir_operando(fd_asm,"m",1);
    	escribir_operando(fd_asm,"1",0);
    	sumar(fd_asm,1,0);
    	asignar(fd_asm,"m",0);
	

    	/*Recuperamos la etiqueta para imprimir el fin de etiqueta del while.*/
    	etiqueta = etiquetas[cima_etiquetas];
    	while_fin(fd_asm, etiqueta);

    	/*Al cerrar el ámbito, decrementamos el contador de cima de etiquetas.*/
    	cima_etiquetas--;


    	escribir_fin(fd_asm);

    	fclose(fd_asm);
}

