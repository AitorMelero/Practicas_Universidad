segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
;D:	main
;D:	{
;D:	int
;R10:	<tipo> ::= int
;R9:	<clase_escalar> ::= <tipo>
;R5:	<clase> ::= <clase_escalar>
;D:	sum
	_sum resd 1
;R108:	<identificador> ::= TOK_IDENTIFICADOR
;D:	,
;D:	global
	_global resd 1
;R108:	<identificador> ::= TOK_IDENTIFICADOR
;D:	;
;R18:	<identificadores> ::= <identificador>
;R19:	<identificadores> ::= <identificador> , <identificadores>
;R4:	<declaracion> ::= <clase> <identificadores> ;
;D:	array
;D:	int
;R10:	<tipo> ::= int
;D:	[
;D:	2
;D:	]
;R15:	<clase_vector> ::= array <tipo> [ <constante_entera> ]
;R7:	<clase> ::= <clase_vector>
;D:	ab
	_ab resd 1
;R108:	<identificador> ::= TOK_IDENTIFICADOR
;D:	;
;R18:	<identificadores> ::= <identificador>
;R4:	<declaracion> ::= <clase> <identificadores> ;
;D:	function
;R2:	<declaraciones> ::= <declaracion>
;R3:	<declaraciones> ::= <declaracion> <declaraciones>
segment .text
global main
extern scan_int, print_int, scan_float, print_float, scan_boolean, print_boolean
extern print_endofline, print_blank, print_string
extern alfa_malloc, alfa_free, ld_float
;D:	int
;R10:	<tipo> ::= int
;D:	suma
;D:	(
;D:	int
;R10:	<tipo> ::= int
;D:	a
;R27:	<parametro_funcion> ::= <tipo> <identificador>
;D:	;
;D:	int
;R10:	<tipo> ::= int
;D:	b
;R27:	<parametro_funcion> ::= <tipo> <identificador>
;D:	)
;R26:	<resto_parametros_funcion> ::=
;R25:	<resto_parametros_funcion> ::= ; <parametro_funcion> <resto_parametros_funcion>
;R23:	<parametros_funcion> ::= <parametro_funcion> <resto_parametros_funcion>
;D:	{
;D:	int
;R10:	<tipo> ::= int
;R9:	<clase_escalar> ::= <tipo>
;R5:	<clase> ::= <clase_escalar>
;D:	local
;R108:	<identificador> ::= TOK_IDENTIFICADOR
;D:	;
;R18:	<identificadores> ::= <identificador>
;R4:	<declaracion> ::= <clase> <identificadores> ;
;D:	local
;R2:	<declaraciones> ::= <declaracion>
;R28:	<declaraciones_funcion> ::= <declaraciones>
suma:
	push dword ebp
	mov ebp, esp
	sub esp, 4
;D:	=
;D:	6
;R104:	<constante_entera> ::= <numero>
;R100:	<constante> ::= <constante_entera>
	push dword 6
;R81:	<exp> ::= <constante>
;D:	;
	lea eax, [ebp - 4]
	push dword eax
	pop dword ebx
	pop dword eax
	mov dword [ebx], eax
;R34:	<sentencia_simple> ::= <asignacion>
;R32:	<sentencia> ::= <sentencia_simple> ;
;D:	return
;D:	a
;D:	+
mov dword eax, ebp
add eax, 12
push dword eax
;R80:	<exp> ::= <identificador>
;D:	local
;D:	+
mov dword eax, ebp
add eax, -4
push dword eax
;R80:	<exp> ::= <identificador>
	pop dword ecx
	mov dword edx, [ecx]
	pop dword ecx
	mov dword eax, [ecx]
	add eax, edx
	push dword eax
;R72:	<exp> ::= <exp> + <exp>
;D:	global
;D:	;
	push dword _global
;R80:	<exp> ::= <identificador>
	pop dword ecx
	mov dword edx, [ecx]
	pop dword eax
	add eax, edx
	push dword eax
;R72:	<exp> ::= <exp> + <exp>
	pop eax
	mov esp,ebp
	pop ebp
	ret
;R61:	<retorno_funcion> ::= return <exp>
;R38:	<sentencia_simple> ::= <retorno_funcion>
;R32:	<sentencia> ::= <sentencia_simple> ;
;D:	}
;R30:	<sentencias> ::= <sentencia>
;R31:	<sentencias> ::= <sentencia> <sentencias>
mov eax, 0
mov esp, ebp
pop ebp
ret
;R22:	<funcion> ::=function <tipo> <identificador> ( <parametros_funcion> ) { <declaraciones_funcion> <sentencias> }
;D:	ab
;R21:	<funciones> ::=
;R20:	<funciones> ::= <funcion> <funciones>
main:
	mov dword [__esp] , esp
;D:	[
;D:	0
;R104:	<constante_entera> ::= <numero>
;R100:	<constante> ::= <constante_entera>
	push dword 0
;R81:	<exp> ::= <constante>
;D:	]
****Error semantico en lin 13: Intento de indexacion de una variable que no es de tipo vector.
	jmp fin
fin_error_division:
	push dword _error_division0
	call print_string
	add esp, 4
	call print_endofline
	jmp fin
fin_indice_fuera_rango:
	push dword _error_indice
	call print_string
	add esp, 4
	call print_endofline
fin:
	mov dword esp, [__esp]
	ret
