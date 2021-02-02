/*Fichero: alfa.y
 *Autores: Miguel Manzano, Aitor Melero
 *Pareja: 14
 */

 /* Seccion de definicion */
%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "alfa.h"
  #include "y.tab.h"


  extern int yylex();
  extern int yyparse();
	extern FILE *yyout;
  extern long linea, columna;
  extern int error;

	extern int yyleng;

  void yyerror(char const *cadena);

	TIPO tipo;
	CLASE clase;
	int tamVector;

	INFO_SIMBOLO* simbolo_leido;

	INFO_SIMBOLO simbolo;

	int etiquetas = 0;
	int compara = 0;
	int condicional = 0;
	int bucles = 0;
	int indiceLocal = 0;
	int numVariables = 0;
	int numParametros = 0;
	int indiceParametro = 0;
	int funcion_true = 0;
	int llamada_funcion = 0;
	int parametros_aux = 0;
	int return_true = 0;
%}

%union
 {
 	tipo_atributos atributos;
 }

%token TOK_MAIN
%token TOK_INT
%token TOK_BOOLEAN
%token TOK_ARRAY
%token TOK_FUNCTION
%token TOK_IF
%token TOK_ELSE
%token TOK_WHILE
%token TOK_SCANF
%token TOK_PRINTF
%token TOK_RETURN
%token TOK_PUNTOYCOMA
%token TOK_COMA
%token TOK_PARENTESISIZQUIERDO
%token TOK_PARENTESISDERECHO
%token TOK_CORCHETEIZQUIERDO
%token TOK_CORCHETEDERECHO
%token TOK_LLAVEIZQUIERDA
%token TOK_LLAVEDERECHA
%token TOK_ASIGNACION
%token TOK_MAS
%token TOK_MENOS
%token TOK_DIVISION
%token TOK_ASTERISCO
%token TOK_AND
%token TOK_OR
%token TOK_NOT
%token TOK_IGUAL
%token TOK_DISTINTO
%token TOK_MENORIGUAL
%token TOK_MAYORIGUAL
%token TOK_MENOR
%token TOK_MAYOR
%token <atributos> TOK_IDENTIFICADOR
%token <atributos> TOK_CONSTANTE_ENTERA
%token TOK_TRUE
%token TOK_FALSE
%token TOK_ERROR
%type <atributos> constante_entera
%type <atributos> constante_logica
%type <atributos> constante
%type <atributos> exp
%type <atributos> if_exp
%type <atributos> if_exp_sent
%type <atributos> while
%type <atributos> while_exp
%type <atributos> elem_vect
%type <atributos> func_nombre
%type <atributos> func_declaracion
%type <atributos> call_func
%type <atributos> identificador_funcion
%left TOK_MAS TOK_MENOS TOK_OR
%left TOK_DIVISION TOK_ASTERISCO TOK_AND
%right MENOSU TOK_NOT

%start programa
%%

programa: TOK_MAIN TOK_LLAVEIZQUIERDA declaraciones escribeTabla funciones escribeMain sentencias TOK_LLAVEDERECHA {fprintf(yyout,";R1:\t<programa> ::= main { <declaraciones> <funciones> <sentencias> }\n");}
;
declaraciones: declaracion {fprintf(yyout,";R2:\t<declaraciones> ::= <declaracion>\n");}
		| declaracion declaraciones {fprintf(yyout,";R3:\t<declaraciones> ::= <declaracion> <declaraciones>\n");}
;
declaracion: clase identificadores TOK_PUNTOYCOMA {fprintf(yyout,";R4:\t<declaracion> ::= <clase> <identificadores> ;\n");}
;
clase: clase_escalar {fprintf(yyout,";R5:\t<clase> ::= <clase_escalar>\n");}
	| clase_vector {fprintf(yyout,";R7:\t<clase> ::= <clase_vector>\n");}
;
clase_escalar:tipo {fprintf(yyout,";R9:\t<clase_escalar> ::= <tipo>\n");}
;
tipo: TOK_INT {fprintf(yyout,";R10:\t<tipo> ::= int\n"); tipo = ENTERO;}
	|TOK_BOOLEAN {fprintf(yyout,";R11:\t<tipo> ::= boolean\n"); tipo = BOOLEANO;}
;
clase_vector: TOK_ARRAY tipo TOK_CORCHETEIZQUIERDO TOK_CONSTANTE_ENTERA TOK_CORCHETEDERECHO {
  tamVector = $4.valor_entero;
  if($4.valor_entero > MAX_TAMANIO_VECTOR || $4.valor_entero<1) {
    fprintf(yyout, "****Error semantico en lin %ld: El tamanyo del vector excede los limites permitidos (1,64).\n", linea);
    return -1;
  }
  fprintf(yyout, ";R15:\t<clase_vector> ::= array <tipo> [ <constante_entera> ]\n");
}
identificadores: identificador {fprintf(yyout,";R18:\t<identificadores> ::= <identificador>\n");}
		| identificador TOK_COMA identificadores {fprintf(yyout,";R19:\t<identificadores> ::= <identificador> , <identificadores>\n");}
;
funciones: funcion funciones {fprintf(yyout,";R20:\t<funciones> ::= <funcion> <funciones>\n");}
	| {fprintf(yyout,";R21:\t<funciones> ::=\n");}
;

func_nombre: TOK_FUNCTION tipo TOK_IDENTIFICADOR {
    return_true = 0;
    funcion_true = 1;
    simbolo_leido = usoLocal($3.lexema);
    if(simbolo_leido != NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Declaracion duplicada.\n", linea);
      return -1;
    }

    simbolo.categoria = FUNCION;
    simbolo.lexema = $3.lexema;
    simbolo.tipo = tipo;
    simbolo.clase = ESCALAR;

    strcpy($$.lexema, $3.lexema);
    $$.tipo = tipo;

    declararFuncion($3.lexema, &simbolo);
    indiceLocal=0;
    numVariables=0;
    indiceParametro = 0;
    numParametros = 0;
}

func_declaracion: func_nombre TOK_PARENTESISIZQUIERDO parametros_funcion TOK_PARENTESISDERECHO TOK_LLAVEIZQUIERDA declaracionfuncion_true {
    simbolo_leido = usoLocal($1.lexema);
    if(simbolo_leido == NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Declaracion duplicada.\n", linea);
      return -1;
    }
    simbolo_leido->adicional1 = numParametros;
    strcpy($$.lexema, $1.lexema);
    $$.tipo = $1.tipo;
    declarar_Funcion(yyout, $1.lexema, numVariables);
}

funcion: func_declaracion sentencias TOK_LLAVEDERECHA {
	if(!return_true) {
		fprintf(yyout, "****Error semantico en lin %ld: Funcion %s sin sentencia de retorno.\n", linea, $1.lexema);
		return -1;
	}
	cerrarFuncion();
	fin_funcion(yyout);
	simbolo_leido = usoLocal($1.lexema);
	if(simbolo_leido == NULL) {
		fprintf(yyout, "****Error semantico en lin %ld: Declaracion duplicada.\n", linea);
		return -1;
	}
	simbolo_leido->adicional1 = numParametros;
	funcion_true = 0;
	fprintf(yyout, ";R22:\t<funcion> ::=function <tipo> <identificador> ( <parametros_funcion> ) { <declaraciones_funcion> <sentencias> }\n");
}
;
parametros_funcion: parametro_funcion resto_parametros_funcion {fprintf(yyout,";R23:\t<parametros_funcion> ::= <parametro_funcion> <resto_parametros_funcion>\n");}
			| {fprintf(yyout,";R24:\t<parametros_funcion> :=\n");}
;
resto_parametros_funcion: TOK_PUNTOYCOMA parametro_funcion resto_parametros_funcion {fprintf(yyout,";R25:\t<resto_parametros_funcion> ::= ; <parametro_funcion> <resto_parametros_funcion>\n");}
			| {fprintf(yyout,";R26:\t<resto_parametros_funcion> ::=\n");}
;
parametro_funcion: tipo identificador_funcion {
	numParametros++;
	indiceParametro++;
	fprintf(yyout,";R27:\t<parametro_funcion> ::= <tipo> <identificador>\n");}
;
identificador_funcion: TOK_IDENTIFICADOR {	  simbolo_leido = usoLocal($1.lexema);
    if(simbolo_leido != NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Declaracion duplicada.\n", linea);
      return -1;
    }
    simbolo.lexema = $1.lexema;
    simbolo.categoria = PARAMETRO;
	  simbolo.tipo = tipo;
    simbolo.clase = ESCALAR;
    simbolo.adicional1 = numParametros;

    declarar($1.lexema, &simbolo);
}
;
declaracionfuncion_true: declaraciones {fprintf(yyout,";R28:\t<declaraciones_funcion> ::= <declaraciones>\n");}
			| {fprintf(yyout,";R29:\t<declaracionfuncion_true> ::=\n");}
;
sentencias: sentencia {fprintf(yyout,";R30:\t<sentencias> ::= <sentencia>\n");}
	| sentencia sentencias {fprintf(yyout,";R31:\t<sentencias> ::= <sentencia> <sentencias>\n");}
;
sentencia: sentencia_simple TOK_PUNTOYCOMA {fprintf(yyout,";R32:\t<sentencia> ::= <sentencia_simple> ;\n");}
	| bloque {fprintf(yyout,";R33:\t<sentencia> ::= <bloque>\n");}
;
sentencia_simple: asignacion {fprintf(yyout,";R34:\t<sentencia_simple> ::= <asignacion>\n");}
		| lectura {fprintf(yyout,";R35:\t<sentencia_simple> ::= <lectura>\n");}
		| escritura {fprintf(yyout,";R36:\t<sentencia_simple> ::= <escritura>\n");}
		| retorno_funcion {fprintf(yyout,";R38:\t<sentencia_simple> ::= <retorno_funcion>\n");}
;
bloque: condicional {fprintf(yyout,";R40:\t<bloque> ::= <condicional>\n");}
	| bucle {fprintf(yyout,";R41:\t<bloque> ::= <bucle>\n");}
;
asignacion: TOK_IDENTIFICADOR TOK_ASIGNACION exp {
    simbolo_leido = usoLocal($1.lexema);
    if(simbolo_leido==NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Acceso a variable no declarada (%s).\n", linea, $1.lexema);
      return -1;
    } else {
      if(simbolo_leido->categoria == FUNCION) {
        fprintf(yyout, "****Error semantico en lin %ld: Asignacion incompatible.\n", linea);
        return -1;
      }
      if(simbolo_leido->clase == VECTOR) {
        fprintf(yyout, "****Error semantico en lin %ld: Asignacion incompatible.\n", linea);
        return -1;
      }
      if(simbolo_leido->tipo != $3.tipo) {
        fprintf(yyout, "****Error semantico en lin %ld: Asignacion incompatible.\n", linea);
        return -1;

        }
      if (usoGlobal($1.lexema) == NULL) {
        if(simbolo_leido->categoria == PARAMETRO) {
          escribirParametro(yyout, simbolo_leido->adicional1, numParametros);
        } else {
          escribirVariableLocal(yyout, (simbolo_leido->adicional1+1));
        }
        asignarDestinoEnPila(yyout,$3.es_direccion);
      } else {
        asignar(yyout, $1.lexema, $3.es_direccion);
        fprintf(yyout, ";R43:\t<asignacion> ::= <identificador> = <exp>\n");
    }
  }
}
		| elem_vect TOK_ASIGNACION exp {
      if($1.tipo != $3.tipo) {
        fprintf(yyout, "****Error semantico en lin %ld: Asignacion incompatible.\n", linea);
        return -1;
      }
      asignar_vector(yyout, $3.es_direccion);
      fprintf(yyout, ";R44:\t<asignacion> ::= <elem_vect> = <exp>\n");}
    ;

elem_vect: TOK_IDENTIFICADOR TOK_CORCHETEIZQUIERDO exp TOK_CORCHETEDERECHO {
  simbolo_leido = usoLocal($1.lexema);
  if(simbolo_leido == NULL) {
    fprintf(yyout, "****Error semantico en lin %ld: Acceso a variable no declarada (%s).\n", linea, $1.lexema);
    return -1;
  }
  if(simbolo_leido->categoria == FUNCION) {
    fprintf(yyout,"****Error semantico en lin %ld: Identificador no valido\n", linea);
    return -1;
  }
  if(simbolo_leido->clase == ESCALAR) {
    fprintf(yyout, "****Error semantico en lin %ld: Intento de indexacion de una variable que no es de tipo vector.\n", linea);
    return -1;
  }
  $$.tipo = simbolo_leido->tipo;
  $$.es_direccion = 1;
  if($3.tipo != ENTERO) {
    fprintf(yyout, "****Error semantico en lin %ld: El indice en una operacion de indexacion tiene que ser de tipo entero.\n", linea);
    return -1;
  }
  escribir_operando_array(yyout, $1.lexema, $3.es_direccion, simbolo_leido->adicional1);

  fprintf(yyout, ";R48:\t<elem_vect> ::= <identificador> [ <exp> ]\n");}

;

condicional: if_exp_sent TOK_LLAVEDERECHA {
  ifthenelse_fin(yyout, $1.etiqueta);
  fprintf(yyout, ";R50:\t<condicional> ::= if ( <exp> ) { <sentencias> }\n");
}
           | if_exp_sent TOK_LLAVEDERECHA TOK_ELSE TOK_LLAVEIZQUIERDA sentencias TOK_LLAVEDERECHA {
            ifthenelse_fin(yyout, $1.etiqueta);
            fprintf(yyout, ";R51:\t<condicional> ::= if ( <exp> ) { <sentencias> } else { <sentencias> }\n");}
;

if_exp: TOK_IF TOK_PARENTESISIZQUIERDO exp TOK_PARENTESISDERECHO TOK_LLAVEIZQUIERDA {
    if($3.tipo != BOOLEANO) {
      fprintf(yyout, "****Error semantico en lin %ld: Condicional con condicion de tipo int.\n", linea);
      return -1;
    }
    $$.etiqueta = condicional++;
    ifthenelse_inicio(yyout, $3.es_direccion, $$.etiqueta);
  }
;

if_exp_sent:  if_exp sentencias {
  $$.etiqueta = $1.etiqueta;
  ifthenelse_fin_then(yyout, $$.etiqueta);

}
;

bucle: while_exp sentencias TOK_LLAVEDERECHA {
	while_fin(yyout, $1.etiqueta);
	fprintf(yyout, ";R52:\t<bucle> ::= while ( <exp> ) { <sentencias> }\n");}
;

while: TOK_WHILE TOK_PARENTESISIZQUIERDO {
	$$.etiqueta = bucles++;
	while_inicio(yyout, $$.etiqueta);
}

while_exp: while exp TOK_PARENTESISDERECHO TOK_LLAVEIZQUIERDA {
	if($2.tipo != BOOLEANO) {
		fprintf(yyout, "****Error semantico en lin %ld: Bucle con condicion de tipo int.\n", linea);
		return -1;
	}

	$$.etiqueta = $1.etiqueta;
	while_exp_pila(yyout, $2.es_direccion, $$.etiqueta);
}
;

lectura: TOK_SCANF TOK_IDENTIFICADOR {
    simbolo_leido = usoLocal($2.lexema);
    if(simbolo_leido == NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Acceso a variable no declarada (%s).\n", linea, $2.lexema);
      return -1;
    }
    leer(yyout, $2.lexema, simbolo_leido->tipo);
    fprintf(yyout, ";R54:\t<lectura> ::= scanf <identificador>\n");
}
;

escritura: TOK_PRINTF exp {
    escribir(yyout, ($2.es_direccion), ($2.tipo));

    fprintf(yyout, ";R56:\t<escritura> ::= printf <exp>\n");}
         ;

retorno_funcion: TOK_RETURN exp {
  if(!funcion_true) {
    fprintf(yyout, "****Error semantico en lin %ld: Sentencia de retorno fuera del cuerpo de una función.\n", linea);
    return -1;
  }

  return_true = 1;
  retornarFuncion(yyout, $2.es_direccion);
  fprintf(yyout, ";R61:\t<retorno_funcion> ::= return <exp>\n");}
;

exp: exp TOK_MAS exp {
  if($1.tipo!=ENTERO || $3.tipo != ENTERO) {
    fprintf(yyout, "****Error semantico en lin %ld: Operacion aritmetica con operandos boolean.\n", linea);
    return -1;
  }
  sumar(yyout, $1.es_direccion, $3.es_direccion);
  $$.es_direccion = 0;
  $$.tipo = ENTERO;

  fprintf(yyout, ";R72:\t<exp> ::= <exp> + <exp>\n");
}

   | exp TOK_MENOS exp {
  if($1.tipo!=ENTERO || $3.tipo != ENTERO) {
    fprintf(yyout, "****Error semantico en lin %ld: Operacion aritmetica con operandos boolean.\n", linea);
    return -1;
  }
  $$.tipo = ENTERO;
  restar(yyout, $1.es_direccion, $3.es_direccion);
  $$.es_direccion = 0;
    fprintf(yyout, ";R73:\t<exp> ::= <exp> - <exp>\n");
}

   | exp TOK_DIVISION exp {
  if($1.tipo!=ENTERO || $3.tipo != ENTERO) {
    fprintf(yyout, "****Error semantico en lin %ld: Operacion aritmetica con operandos boolean.\n", linea);
    return -1;
  }
  $$.tipo = ENTERO;
  dividir(yyout, $1.es_direccion, $3.es_direccion);
  $$.es_direccion = 0;
    fprintf(yyout, ";R74:\t<exp> ::= <exp> / <exp>\n");
}

   | exp TOK_ASTERISCO exp {
  if($1.tipo!=ENTERO || $3.tipo != ENTERO) {
    fprintf(yyout, "****Error semantico en lin %ld: Operacion aritmetica con operandos boolean.\n", linea);
    return -1;
  }
  $$.tipo = ENTERO;
  multiplicar(yyout, $1.es_direccion, $3.es_direccion);
  $$.es_direccion = 0;
    fprintf(yyout, ";R75:\t<exp> ::= <exp> * <exp>\n");
}

   | TOK_MENOS exp %prec MENOSU {
    if($2.tipo!=ENTERO) {
      fprintf(yyout, "****Error semantico en lin %ld: Operacion aritmetica con operandos boolean.\n", linea);
      return -1;
    }
    $$.tipo = ENTERO;
    cambiar_signo(yyout, $2.es_direccion);
    $$.es_direccion = 0;
    fprintf(yyout, ";R76:\t<exp> ::= - <exp>\n");
}
   | exp TOK_AND exp {
    if($1.tipo!=BOOLEANO || $3.tipo != BOOLEANO) {
      fprintf(yyout, "****Error semantico en lin %ld: Operacion logica con operandos int.\n", linea);
      return -1;
    }
    $$.tipo = BOOLEANO;
    y(yyout, $1.es_direccion, $3.es_direccion);
    $$.es_direccion = 0;
    fprintf(yyout, ";R77:\t<exp> ::= <exp> && <exp>\n");
}
   | exp TOK_OR exp {
    if($1.tipo!=BOOLEANO || $3.tipo != BOOLEANO) {
      fprintf(yyout, "****Error semantico en lin %ld: Operacion logica con operandos int.\n", linea);
      return -1;
    }
    $$.tipo = BOOLEANO;
    o(yyout, $1.es_direccion, $3.es_direccion);
    $$.es_direccion = 0;
    fprintf(yyout, ";R77:\t<exp> ::= <exp> && <exp>\n");
    fprintf(yyout, ";R78:\t<exp> ::= <exp> || <exp>\n");
  }
   | TOK_NOT exp {
    if($2.tipo!=BOOLEANO) {
      fprintf(yyout, "****Error semantico en lin %ld: Operacion logica con operandos int.\n", linea);
      return -1;
    }
    $$.tipo = BOOLEANO;
    no(yyout, $2.es_direccion, etiquetas++);
    $$.es_direccion = 0;
    fprintf(yyout, ";R79:\t<exp> ::= ! <exp>\n");
}
   | TOK_IDENTIFICADOR {
    strcpy($$.lexema, $1.lexema);
    simbolo_leido = usoLocal($1.lexema);
    if(simbolo_leido == NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Acceso a variable no declarada (%s).\n", linea, $1.lexema);
      return -1;
    }
    if (usoGlobal($1.lexema) == NULL) {
      if(simbolo_leido->categoria == PARAMETRO) {
        escribir_operando_funcion(yyout, (numParametros-simbolo_leido->adicional1)+1);
      } else {
        escribir_operando_funcion(yyout, -(simbolo_leido->adicional1+1));
      }

    } else {
      if(simbolo_leido->categoria==FUNCION) {
        fprintf(yyout,"Identificador no valido\n");
        return -1;
    }

    escribir_operando(yyout, $1.lexema, 1);
    }
    $$.es_direccion = 1;
    $$.tipo = simbolo_leido->tipo;

    fprintf(yyout, ";R80:\t<exp> ::= <identificador>\n");

  }
   | constante {
    $$.tipo =$1.tipo;
    $$.es_direccion = $1.es_direccion;
    escribir_operando(yyout, $1.lexema, 0);
    fprintf(yyout, ";R81:\t<exp> ::= <constante>\n");
  }
   | TOK_PARENTESISIZQUIERDO exp TOK_PARENTESISDERECHO {
    $$.tipo =$2.tipo;
    $$.es_direccion = $2.es_direccion;
    fprintf(yyout, ";R82:\t<exp> ::= ( <exp> )\n");
  }
   | TOK_PARENTESISIZQUIERDO comparacion TOK_PARENTESISDERECHO {
    $$.tipo =BOOLEANO;
    $$.es_direccion = 0;
    fprintf(yyout, ";R82:\t<exp> ::= ( <exp> )\n");
    fprintf(yyout, ";R83:\t<exp> ::= ( <comparacion> )\n");
  }
   | elem_vect {
    fprintf(yyout, ";R85:\t<exp> ::= <elem_vect>\n");

  }
   |  call_func lista_expresiones TOK_PARENTESISDERECHO {
    simbolo_leido = usoLocal($1.lexema);
    if(simbolo_leido == NULL) {
      fprintf(yyout, "****Error semantico en lin %ld: Funcion no declarada (%s).\n", linea, $1.lexema);
      return -1;
    }
    if(simbolo_leido->categoria != FUNCION){
      fprintf(yyout, "****Error semantico en lin %ld: El identificador no es una funcion (%s).\n", linea, $1.lexema);
      return -1;
    }
    if(simbolo_leido->adicional1 != parametros_aux) {
      fprintf(yyout, "****Error semantico en lin %ld: Numero incorrecto de parametros en llamada a funcion.\n", linea);
      return -1;
    }
    llamada_funcion = 0;
    $$.tipo = simbolo_leido->tipo;
    llamarFuncion(yyout, $1.lexema, simbolo_leido->adicional1);

    fprintf(yyout, ";R88:\t<exp> ::= <identificador> ( <lista_expresiones> )\n");}
;

call_func: TOK_IDENTIFICADOR TOK_PARENTESISIZQUIERDO {
  if(llamada_funcion) {
    fprintf(yyout, "****Error semantico en lin %ld: No esta permitido el uso de llamadas a funciones como parametros de otras funciones.\n", linea);
    return -1;
  }
  llamada_funcion = 1;
  parametros_aux = 0;
  strcpy($$.lexema, $1.lexema);
}
;
lista_expresiones: expf resto_lista_expresiones {
  llamada_funcion = 0;
  parametros_aux++;

  fprintf(yyout, ";R89:\t<lista_expresiones> ::= <exp> <resto_lista_expresiones>\n");}
                 |   {
                  llamada_funcion = 0;
                  fprintf(yyout, ";R90:\t<lista_expresiones> ::=\n");}
;

resto_lista_expresiones: TOK_COMA expf resto_lista_expresiones {
  parametros_aux++;

  fprintf(yyout, ";R91:\t<resto_lista_expresiones> ::= , <exp> <resto_lista_expresiones>\n");}
                       |   {fprintf(yyout, ";R92:\t<resto_lista_expresiones> ::=\n");}
                       ;
expf: exp {
  if($1.es_direccion) {
    cambiar_a_valor(yyout);
  }
}
;

comparacion: exp TOK_IGUAL exp {
  if($1.tipo != ENTERO || $3.tipo != ENTERO) {
    fprintf(yyout, "****Error semantico en lin %ld: Comparacion con operandos boolean.\n", linea);
    return -1;
  }
  igual(yyout, $1.es_direccion, $3.es_direccion, compara++);
  /*$$.tipo = BOOLEANO;
  $$.es_direccion = 0;*/
  fprintf(yyout, ";R93:\t<comparacion> ::= <exp> == <exp>\n");
}
           | exp TOK_DISTINTO exp {
            if($1.tipo != ENTERO || $3.tipo != ENTERO) {
              fprintf(yyout, "****Error semantico en lin %ld: Comparacion con operandos boolean.\n", linea);
              return -1;
            }
            distinto(yyout, $1.es_direccion, $3.es_direccion, compara++);
            fprintf(yyout, ";R94:\t<comparacion> ::= <exp> != <exp>\n");
            }
           | exp TOK_MENORIGUAL exp {
            if($1.tipo != ENTERO || $3.tipo != ENTERO) {
              fprintf(yyout, "****Error semantico en lin %ld: Comparacion con operandos boolean.\n", linea);
              return -1;
            }
            menor_igual(yyout, $1.es_direccion, $3.es_direccion, compara++);
            fprintf(yyout, ";R95:\t<comparacion> ::= <exp> <= <exp>\n");}
           | exp TOK_MAYORIGUAL exp {
            if($1.tipo != ENTERO || $3.tipo != ENTERO) {
              fprintf(yyout, "****Error semantico en lin %ld: Comparacion con operandos boolean.\n", linea);
              return -1;
            }
            mayor_igual(yyout, $1.es_direccion, $3.es_direccion, compara++);
            fprintf(yyout, ";R96:\t<comparacion> ::= <exp> >= <exp>\n");}
           | exp TOK_MENOR exp {
            if($1.tipo != ENTERO || $3.tipo != ENTERO) {
              fprintf(yyout, "****Error semantico en lin %ld: Comparacion con operandos boolean.\n", linea);
              return -1;
            }
            menor(yyout, $1.es_direccion, $3.es_direccion, compara++);
            fprintf(yyout, ";R97:\t<comparacion> ::= <exp> < <exp>\n");}
           | exp TOK_MAYOR exp {
            if($1.tipo != ENTERO || $3.tipo != ENTERO) {
              fprintf(yyout, "****Error semantico en lin %ld: Comparacion con operandos boolean.\n", linea);
              return -1;
            }
            mayor(yyout, $1.es_direccion, $3.es_direccion, compara++);
            fprintf(yyout, ";R98:\t<comparacion> ::= <exp> > <exp>\n");}
;

constante: constante_entera {$$.tipo = $1.tipo; $$.es_direccion = $1.es_direccion; strcpy($$.lexema, $1.lexema); fprintf(yyout, ";R100:\t<constante> ::= <constante_entera>\n");}
         | constante_logica {$$.tipo = $1.tipo; $$.es_direccion = $1.es_direccion; strcpy($$.lexema, $1.lexema); fprintf(yyout, ";R99:\t<constante> ::= <constante_logica>\n");}
;

constante_logica: TOK_TRUE {$$.tipo = BOOLEANO; $$.es_direccion = 0; strcpy($$.lexema,"1"); fprintf(yyout, ";R102:\t<constante_logica> ::= true\n");}
                | TOK_FALSE {$$.tipo = BOOLEANO; $$.es_direccion = 0; strcpy($$.lexema,"0"); fprintf(yyout, ";R103:\t<constante_logica> ::= false\n");}
;

constante_entera: TOK_CONSTANTE_ENTERA { $$.tipo = ENTERO; $$.es_direccion = 0; fprintf(yyout, ";R104:\t<constante_entera> ::= <numero>\n");}
;

identificador: TOK_IDENTIFICADOR {
    simbolo_leido = usoLocal($1.lexema);
    if((simbolo_leido != NULL && !funcion_true) || (simbolo_leido != NULL && EsLocal($1.lexema)) ) {
      fprintf(yyout, "****Error semantico en lin %ld: Declaracion duplicada.\n", linea);
      return -1;
    }

    simbolo.lexema = $1.lexema;
    simbolo.categoria = VARIABLE;
    simbolo.clase = clase;
    simbolo.tipo = tipo;
    if(clase == VECTOR) {
      simbolo.adicional1 = tamVector;

    } else {
      simbolo.adicional1 = 1;
    }
    if(funcion_true) {
      if(clase == VECTOR) {
        fprintf(yyout, "****Error semantico en lin %ld: Variable local de tipo no escalar.\n", linea);
        return -1;
      }
      simbolo.adicional1 = numVariables;
      numVariables++;
      indiceLocal++;
    } else {
      declarar_variable(yyout, $1.lexema, tipo,  simbolo.adicional1);

    }
    declarar($1.lexema, &simbolo);


    fprintf(yyout, ";R108:\t<identificador> ::= TOK_IDENTIFICADOR\n");}
;


escribeTabla: { escribir_segmento_codigo(yyout); }
;

escribeMain: { escribir_inicio_main(yyout);}
;

%%

void yyerror(char const * s) {
    printf("****Error sintactico en [lin %ld, col %ld]\n", linea, columna);
}
