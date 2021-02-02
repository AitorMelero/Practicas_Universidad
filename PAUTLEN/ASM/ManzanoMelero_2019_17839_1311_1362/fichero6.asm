segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_m resd 1
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
main:
	mov dword [__esp] , esp
	push 0
	pop dword [_m]
	push dword _m
	push 5
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jg near mayor1
	push dword 0
	jmp fin_mayor1
mayor1:
	push dword 1
fin_mayor1: 
	pop eax
	cmp eax, 0
	je near fin_then1
	push 2
	call print_int
	add esp, 4
	call print_endofline
	jmp near fin_ifelse1
	fin_then1:
	push 3
	call print_int
	add esp, 4
	call print_endofline
	fin_ifelse1:
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
