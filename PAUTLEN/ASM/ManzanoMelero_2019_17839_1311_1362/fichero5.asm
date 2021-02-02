segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_m resd 1
	_v resd 4
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
main:
	mov dword [__esp] , esp
	push 0
	pop dword [_m]
inicio_while1:
	push dword _m
	push 4
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jle near menor_igual1
	push dword 0
	jmp fin_menor_igual1
menor_igual1:
	push dword 1
fin_menor_igual1: 
	pop eax
	cmp eax, 0
	je fin_while1
	push dword _m
	pop dword eax
	mov dword eax, [eax]
	push dword eax
	call print_int
	add esp, 4
	call print_endofline
	push dword _m
	push 10
	pop dword edx
	pop dword ecx
	mov dword eax, [ecx]
	imul eax, edx
	push dword eax
	push dword _m
	pop dword eax
	mov dword eax, [eax]
	cmp eax, 0
	jl fin_indice_fuera_rango
	cmp eax, 3
	jg fin_indice_fuera_rango
	mov dword edx, _v
	lea eax, [edx + eax*4]
	push dword eax
	pop dword ebx
	pop dword eax
	mov dword [ebx], eax
	push dword _m
	push 1
	pop dword edx
	pop dword ecx
	mov dword eax, [ecx]
	add eax, edx
	push dword eax
	pop dword [_m]
	jmp inicio_while1
fin_while1:
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
