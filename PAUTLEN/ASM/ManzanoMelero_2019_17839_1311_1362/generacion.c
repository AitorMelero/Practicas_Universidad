/**
 * File: generacion.c
 * Autores: Miguel Manzano, Aitor Melero
 * Fecha: 7/10/2019
 */

#include <stdio.h>
#include "generacion.h"

void escribir_cabecera_bss(FILE* fpasm) {
  fprintf(fpasm, "segment .bss\n");
  fprintf(fpasm, "\t__esp resd 1\n");
}

void escribir_subseccion_data(FILE* fpasm) {
  fprintf(fpasm, "segment .data\n");
  fprintf(fpasm, "\t_error_division0 db \"ERROR, division por 0.\", 0\n");
  fprintf(fpasm, "\t_error_indice db \"Indice fuera de rango.\", 0\n");
}

void declarar_variable(FILE* fpasm, char* nombre, int tipo, int tamano) {
  if (tipo==ENTERO) {
    fprintf(fpasm, "\t_%s resd %d\n", nombre, tamano);
  }
  else if(tipo == BOOLEANO){
    fprintf(fpasm, "\t_%s resd %d\n", nombre, tamano);
  }
}

void escribir_segmento_codigo(FILE* fpasm){
  fprintf(fpasm, "segment .text\n");
  fprintf(fpasm, "\tglobal main\n");
  fprintf(fpasm, "\textern scan_int, print_int, scan_boolean, print_boolean, ");
  fprintf(fpasm, "print_endofline, print_blank, print_string\n");
}

void escribir_inicio_main(FILE* fpasm){
  fprintf(fpasm, "main:\n");
  fprintf(fpasm, "\tmov dword [__esp] , esp\n");
}

void escribir_fin(FILE* fpasm){

  /*SI NO SE HA PRODUCIDO NINGUN ERROR EN LA DIVISION O EN LOS INDICES*/
  fprintf(fpasm, "\tjmp fin\n");

  /********** ERROR DIVISION ******************/
  fprintf(fpasm, "fin_error_division:\n");
  fprintf(fpasm, "\tpush dword _error_division0\n");
  fprintf(fpasm, "\tcall print_string\n");
  fprintf(fpasm, "\tadd esp, 4\n");
  fprintf(fpasm, "\tcall print_endofline\n");
  fprintf(fpasm, "\tjmp fin\n");

  /*********** INDICE FUERA DE RANGO **********/
  fprintf(fpasm, "fin_indice_fuera_rango:\n");
  fprintf(fpasm, "\tpush dword _error_indice\n");
  fprintf(fpasm, "\tcall print_string\n");
  fprintf(fpasm, "\tadd esp, 4\n");
  fprintf(fpasm, "\tcall print_endofline\n");

  /************* FIN **************************/
  fprintf(fpasm, "fin:\n");
  fprintf(fpasm, "\tmov dword esp, [__esp]\n");
  fprintf(fpasm, "\tret\n");
}

void escribir_operando(FILE* fpasm, char* nombre, int es_variable){
  if(es_variable == 1){
    fprintf(fpasm, "\tpush dword _%s\n", nombre);
  }
  else if(es_variable == 0){
    fprintf(fpasm, "\tpush %s\n", nombre);
  }
}

void asignar(FILE* fpasm, char* nombre, int es_variable){
  if(es_variable == 1){
    fprintf(fpasm, "\tpop dword eax\n");
    fprintf(fpasm, "\tmov dword ebx, [eax]\n");
    fprintf(fpasm, "\tmov dword [_%s], ebx\n", nombre);
  }
  else if(es_variable == 0){
    fprintf(fpasm, "\tpop dword [_%s]\n", nombre);
  }
}

void sumar(FILE* fpasm, int es_variable_1, int es_variable_2){

  if(es_variable_2 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword edx, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword edx\n");
  }
  if(es_variable_1 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword eax, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword eax\n");
  }

  /* REALIZAMOS LA OPERACION FINAL Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\tadd eax, edx\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void restar(FILE* fpasm, int es_variable_1, int es_variable_2){
  if(es_variable_2 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword edx, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword edx\n");
  }
  if(es_variable_1 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword eax, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword eax\n");
  }

  /* REALIZAMOS LA OPERACION FINAL Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\tsub eax, edx\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void multiplicar(FILE* fpasm, int es_variable_1, int es_variable_2){
  if(es_variable_2 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword edx, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword edx\n");
  }
  if(es_variable_1 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword eax, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword eax\n");
  }

  /* REALIZAMOS LA OPERACION FINAL Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\timul eax, edx\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void dividir(FILE* fpasm, int es_variable_1, int es_variable_2){
  if(es_variable_2 == 1){
    fprintf(fpasm, "\tpop dword edx\n");
    fprintf(fpasm, "\tmov dword ecx, [edx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword ecx\n");
  }
  if(es_variable_1 == 1){
    fprintf(fpasm, "\tpop dword edx\n");
    fprintf(fpasm, "\tmov dword eax, [edx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword eax\n");
  }

  /******** COMPROBACION ERROR DE LA DIVISION *********/
  fprintf(fpasm, "\tcmp ecx, 0\n");
  fprintf(fpasm, "\tje fin_error_division\n");

  /******** DIVISION CORRECTA ***********/
  /* DUPLICAMOS EL TAMAÑO DEL OPERANDO MEDIANTE LA EXTENSION DE SIGNO CON
  LA INSTRUCCION CDQ PARA PODER REALIZAR LA DIVISION DE ESTE*/
  fprintf(fpasm, "\tcdq\n");
  /* REALIZAMOS LA OPERACION FINAL Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\tidiv ecx\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void o(FILE* fpasm, int es_variable_1, int es_variable_2){
  if(es_variable_2 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword edx, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword edx\n");
  }
  if(es_variable_1 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword eax, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword eax\n");
  }

  /* REALIZAMOS LA OPERACION FINAL Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\tor eax, edx\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void y(FILE* fpasm, int es_variable_1, int es_variable_2){
  if(es_variable_2 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword edx, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword edx\n");
  }
  if(es_variable_1 == 1){
    fprintf(fpasm, "\tpop dword ecx\n");
    fprintf(fpasm, "\tmov dword eax, [ecx]\n");
  }
  else{
    fprintf(fpasm, "\tpop dword eax\n");
  }

  /* REALIZAMOS LA OPERACION FINAL Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\tand eax, edx\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void cambiar_signo(FILE* fpasm, int es_variable){

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  /* REALIZAMOS EL CAMBIO DE SIGNO MEDIANTE LA INSTRUCCION NEG
  Y LO INSERTAMOS EN LA PILA */
  fprintf(fpasm, "\tneg eax\n");
  fprintf(fpasm, "\tpush dword eax\n");
}

void no(FILE* fpasm, int es_variable, int cuantos_no){

  fprintf(fpasm, "\tpop dword eax\n");
  if (es_variable == 1) {
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tor eax, eax\n");
  fprintf(fpasm, "\tjz negar%d\n", cuantos_no);
  fprintf(fpasm, "\tmov dword eax, 0\n");
  fprintf(fpasm, "\tjmp fin_no%d\n", cuantos_no);
  fprintf(fpasm, "negar%d:\n", cuantos_no);
  fprintf(fpasm, "\tmov dword eax, 1\n");
  fprintf(fpasm, "fin_no%d:\n", cuantos_no);

  fprintf(fpasm, "\tpush dword eax\n");

}

/********************************************************************/
/****** IMPLEMENTACION DE FUNCIONES DE COMPARACIONES LOGICAS ********/
void mayor(FILE* fpasm, int es_variable1, int es_variable2, int etiqueta){

  fprintf(fpasm, "\tpop dword edx\n");
  if(es_variable2 == 1){
    fprintf(fpasm, "\tmov dword edx, [edx]\n");
  }

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable1 == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tcmp eax, edx\n");
  fprintf(fpasm, "\tjg near mayor%d\n", etiqueta);
  fprintf(fpasm, "\tpush dword 0\n");
  fprintf(fpasm, "\tjmp fin_mayor%d\n", etiqueta);
  fprintf(fpasm, "mayor%d:\n", etiqueta);
  fprintf(fpasm, "\tpush dword 1\n");
  fprintf(fpasm, "fin_mayor%d: \n", etiqueta);
}

void mayor_igual(FILE* fpasm, int es_variable1, int es_variable2, int etiqueta){
  fprintf(fpasm, "\tpop dword edx\n");
  if(es_variable2 == 1){
    fprintf(fpasm, "\tmov dword edx, [edx]\n");
  }

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable1 == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tcmp eax, edx\n");
  fprintf(fpasm, "\tjge near mayor_igual%d\n", etiqueta);
  fprintf(fpasm, "\tpush dword 0\n");
  fprintf(fpasm, "\tjmp fin_mayor_igual%d\n", etiqueta);
  fprintf(fpasm, "mayor_igual%d:\n", etiqueta);
  fprintf(fpasm, "\tpush dword 1\n");
  fprintf(fpasm, "fin_mayor_igual%d: \n", etiqueta);
}

void menor(FILE* fpasm, int es_variable1, int es_variable2, int etiqueta){

  fprintf(fpasm, "\tpop dword edx\n");
  if(es_variable2 == 1){
    fprintf(fpasm, "\tmov dword edx, [edx]\n");
  }

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable1 == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tcmp eax, edx\n");
  fprintf(fpasm, "\tjl near menor%d\n", etiqueta);
  fprintf(fpasm, "\tpush dword 0\n");
  fprintf(fpasm, "\tjmp fin_menor%d\n", etiqueta);
  fprintf(fpasm, "menor%d:\n", etiqueta);
  fprintf(fpasm, "\tpush dword 1\n");
  fprintf(fpasm, "fin_menor%d: \n", etiqueta);
}

void menor_igual(FILE* fpasm, int es_variable1, int es_variable2, int etiqueta){

  fprintf(fpasm, "\tpop dword edx\n");
  if(es_variable2 == 1){
    fprintf(fpasm, "\tmov dword edx, [edx]\n");
  }

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable1 == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tcmp eax, edx\n");
  fprintf(fpasm, "\tjle near menor_igual%d\n", etiqueta);
  fprintf(fpasm, "\tpush dword 0\n");
  fprintf(fpasm, "\tjmp fin_menor_igual%d\n", etiqueta);
  fprintf(fpasm, "menor_igual%d:\n", etiqueta);
  fprintf(fpasm, "\tpush dword 1\n");
  fprintf(fpasm, "fin_menor_igual%d: \n", etiqueta);
}

void distinto(FILE* fpasm, int es_variable1, int es_variable2, int etiqueta){

  fprintf(fpasm, "\tpop dword edx\n");
  if(es_variable2 == 1){
    fprintf(fpasm, "\tmov dword edx, [edx]\n");
  }

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable1 == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tcmp eax, edx\n");
  fprintf(fpasm, "\tjne near distinto%d\n", etiqueta);
  fprintf(fpasm, "\tpush dword 0\n");
  fprintf(fpasm, "\tjmp fin_distinto%d\n", etiqueta);
  fprintf(fpasm, "distinto%d:\n", etiqueta);
  fprintf(fpasm, "\tpush dword 1\n");
  fprintf(fpasm, "fin_distinto%d: \n", etiqueta);
}

void igual(FILE* fpasm, int es_variable1, int es_variable2, int etiqueta){

  fprintf(fpasm, "\tpop dword edx\n");
  if(es_variable2 == 1){
    fprintf(fpasm, "\tmov dword edx, [edx]\n");
  }

  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable1 == 1){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm, "\tcmp eax, edx\n");
  fprintf(fpasm, "\tje near igual%d\n", etiqueta);
  fprintf(fpasm, "\tpush dword 0\n");
  fprintf(fpasm, "\tjmp fin_igual%d\n", etiqueta);
  fprintf(fpasm, "igual%d:\n", etiqueta);
  fprintf(fpasm, "\tpush dword 1\n");
  fprintf(fpasm, "fin_igual%d: \n", etiqueta);
}

/****************************************************************/

/**** IMPLEMENTACION DE LAS FUNCIONES DE LECTURA Y ESCRITURA ****/
void leer(FILE* fpasm, char* nombre, int tipo) {
  if (tipo == ENTERO) {
    fprintf(fpasm, "\tpush dword _%s\n", nombre);
    fprintf(fpasm, "\tcall scan_int\n");
  } else if (tipo == BOOLEANO) {
    fprintf(fpasm, "\tpush dword _%s\n", nombre);
    fprintf(fpasm, "\tcall scan_boolean\n");
  }
  fprintf(fpasm, "\tadd esp, 4\n");
}

void escribir(FILE* fpasm, int es_variable, int tipo) {
  if(es_variable == 1){
    fprintf(fpasm, "\tpop dword eax\n");
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
    fprintf(fpasm, "\tpush dword eax\n");
  }

  if (tipo == ENTERO) {
    fprintf(fpasm, "\tcall print_int\n");
  } else if (tipo == BOOLEANO) {
    fprintf(fpasm, "\tcall print_boolean\n");
  }
  fprintf(fpasm, "\tadd esp, 4\n");
  fprintf(fpasm, "\tcall print_endofline\n");
}

/***************************************************************/

/***** IMPLEMENTACION DE LAS FUNCIONES DE BUCLES ***************/
void while_inicio(FILE * fpasm, int etiqueta) {
  fprintf(fpasm, "inicio_while%d:\n", etiqueta);
}

void while_exp_pila (FILE * fpasm, int exp_es_variable, int etiqueta) {
  fprintf(fpasm,"\tpop eax\n");

  if (exp_es_variable > 0) {
    fprintf(fpasm,"\tmov eax, [eax]\n");
  }

  fprintf(fpasm,"\tcmp eax, 0\n");
  fprintf(fpasm,"\tje fin_while%d\n", etiqueta);
}

void while_fin(FILE * fpasm, int etiqueta) {
  fprintf(fpasm,"\tjmp inicio_while%d\n", etiqueta);
  fprintf(fpasm, "fin_while%d:\n", etiqueta);
}

void escribir_elemento_vector(FILE * fpasm,char * nombre_vector, int tam_max, int exp_es_direccion) {
  fprintf(fpasm,"\tpop dword eax\n");

  if (exp_es_direccion == 1) {
    fprintf(fpasm,"\tmov dword eax, [eax]\n");
  }

  fprintf(fpasm,"\tcmp eax, 0\n");
  fprintf(fpasm,"\tjl fin_indice_fuera_rango\n");
  fprintf(fpasm, "\tcmp eax, %d\n", tam_max-1);
  fprintf(fpasm, "\tjg fin_indice_fuera_rango\n");
  fprintf(fpasm, "\tmov dword edx, _%s\n", nombre_vector);
  fprintf(fpasm, "\tlea eax, [edx + eax*4]\n");
  fprintf(fpasm, "\tpush dword eax\n");

}

void asignarDestinoEnPila(FILE *fpasm, int es_variable){
  fprintf(fpasm, "\tpop dword ebx\n");
  fprintf(fpasm, "\tpop dword eax\n");
  if(es_variable){
    fprintf(fpasm, "\tmov dword eax, [eax]\n");
  }
  fprintf(fpasm, "\tmov dword [ebx], eax\n");
}

void ifthenelse_inicio(FILE * fpasm, int exp_es_variable, int etiqueta){
  fprintf(fpasm, "\tpop eax\n");
  if(exp_es_variable == 1){
    fprintf(fpasm, "\tmov eax, [eax]\n");
  }
    fprintf(fpasm, "\tcmp eax, 0\n");
    fprintf(fpasm, "\tje near fin_then%d\n", etiqueta);
}

void ifthen_inicio(FILE *fpasm, int exp_es_variable, int etiqueta){
  fprintf(fpasm, "\tpop dword eax\n");
  if(exp_es_variable == 1){
    fprintf(fpasm, "\tmov eax, [eax]\n");
  }
  fprintf(fpasm, "\tcmp eax, 0\n");
  fprintf(fpasm, "\tje near fin_then%d\n", etiqueta);
}

void ifthen_fin(FILE *fpasm, int etiqueta){
  fprintf(fpasm, "fin_then%d:\n", etiqueta);
}

void ifthenelse_fin_then(FILE * fpasm, int etiqueta){
  fprintf(fpasm, "\tjmp near fin_ifelse%d\n", etiqueta);
  fprintf(fpasm, "\tfin_then%d:\n", etiqueta);
}

void ifthenelse_fin( FILE * fpasm, int etiqueta){
  fprintf(fpasm, "\tfin_ifelse%d:\n", etiqueta);
}

/*************************************************************/

void escribirParametro(FILE* fpasm, int pos_parametro, int num_total_parametros) {
  int d_ebp;
  d_ebp = 4*(1 + (num_total_parametros - pos_parametro));

  fprintf(fpasm, "\tlea eax, [ebp + %d]\n", d_ebp);
  fprintf(fpasm, "\tpush dword eax\n");
}

void escribirVariableLocal(FILE* fpasm, int posicion_variable_local) {

  int d_ebp;

  d_ebp = 4*posicion_variable_local;
  fprintf(fpasm, "\tlea eax, [ebp - %d]\n", d_ebp);
  fprintf(fpasm, "\tpush dword eax\n");
}

void retornarFuncion(FILE * fd_asm, int es_variable) {

  fprintf(fd_asm, "\tpop eax\n");

  if (es_variable == 1) {
    fprintf(fd_asm, "\tmov dword eax, [eax]\n");
  }

  fprintf(fd_asm, "\tmov esp,ebp\n");
  fprintf(fd_asm, "\tpop ebp\n");
  fprintf(fd_asm, "\tret\n");
}

void declararFuncion(FILE * fd_asm, char * nombre_funcion, int num_var_loc) {

  fprintf(fd_asm, "%s:\n", nombre_funcion);
  fprintf(fd_asm, "\tpush dword ebp\n");
  fprintf(fd_asm, "\tmov ebp, esp\n");
  fprintf(fd_asm, "\tsub esp, %d\n", 4*num_var_loc);
}

void operandoEnPilaAArgumento(FILE * fd_asm, int es_variable) {

  if (es_variable == 1) {
    fprintf(fd_asm, "\tpop dword eax\n");
    fprintf(fd_asm, "\tmov eax, [eax]\n");
    fprintf(fd_asm, "\tpush dword eax\n");
  }
}

void llamarFuncion(FILE * fd_asm, char * nombre_funcion, int num_argumentos) {

  fprintf(fd_asm, "\tcall %s\n", nombre_funcion);
  limpiarPila(fd_asm, num_argumentos);
  fprintf(fd_asm, "\tpush dword eax\n");
}

void limpiarPila(FILE * fd_asm, int num_argumentos) {

  fprintf(fd_asm, "\tadd esp, %d\n", num_argumentos*4);
}
