#include "generacion.h"

int main (int argc, char ** argv)
{
        int etiqueta = 0;
        int getiqueta = 0;
        int etiquetas[3];
        int cima_etiquetas=-1;
        FILE * fd_asm;

        fd_asm = fopen(argv[1],"w");
        escribir_subseccion_data(fd_asm);
        escribir_cabecera_bss(fd_asm);

        /*int m; int n;*/
        declarar_variable(fd_asm,"m", 1, 1);
        declarar_variable(fd_asm,"n", 1, 1);

        escribir_segmento_codigo(fd_asm);
        /*Declaramos la funcion. Vamos a imprimir su etiqueta y decir que tiene dos variables locales.
        //function int multiplicar( int a, int b )
        //{
        //        int c;
	//	  int d;*/
        declararFuncion(fd_asm,"multiplicar",2);

	/*c = a * 2;*/
	escribir_operando(fd_asm,"2",0);
        escribirParametro(fd_asm,0,2);
        multiplicar(fd_asm,0,1);
        escribirVariableLocal(fd_asm,1);
        asignarDestinoEnPila(fd_asm,0);

	/*d = b * 3;*/
	escribir_operando(fd_asm,"3",0);
        escribirParametro(fd_asm,1,2);
        multiplicar(fd_asm,0,1);
        escribirVariableLocal(fd_asm,2);
        asignarDestinoEnPila(fd_asm,0);

	/*return c * d;*/
	escribirVariableLocal(fd_asm,1);
	escribirVariableLocal(fd_asm,2);
	multiplicar(fd_asm,1,1);
	
        /*Retornamos de la funcion con lo que esta encima de la pila.*/
        retornarFuncion(fd_asm, 0);

        escribir_inicio_main(fd_asm);

        /*m=4*/
        escribir_operando(fd_asm,"4",0);
        asignar(fd_asm,"m",0);

	/*n=5*/
	escribir_operando(fd_asm,"5",0);
        asignar(fd_asm,"n",0);

	/*printf multiplicar( m, n );*/
        escribir_operando(fd_asm,"m",1);
        operandoEnPilaAArgumento(fd_asm,1);
        escribir_operando(fd_asm,"n",1);
        operandoEnPilaAArgumento(fd_asm,1);
        llamarFuncion(fd_asm,"multiplicar",2);


        /*Imprimimos el resultado de la funcion.*/
        escribir(fd_asm,0,ENTERO);


        escribir_fin(fd_asm);
        fclose(fd_asm);

}

