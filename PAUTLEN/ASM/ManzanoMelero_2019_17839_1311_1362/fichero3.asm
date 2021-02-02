segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_x resd 1
	_y resd 1
	_z resd 1
	_j resd 1
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
main:
	mov dword [__esp] , esp
	push dword _x
	call scan_int
	add esp, 4
	push dword _z
	call scan_int
	add esp, 4
	push dword _x
	pop dword eax
	mov dword eax, [eax]
	neg eax
	push dword eax
	pop dword [_j]
	push dword _j
	pop dword eax
	mov dword eax, [eax]
	push dword eax
	call print_int
	add esp, 4
	call print_endofline
	push dword _x
	push dword _z
	pop dword ecx
	mov dword edx, [ecx]
	pop dword ecx
	mov dword eax, [ecx]
	sub eax, edx
	push dword eax
	call print_int
	add esp, 4
	call print_endofline
	push dword _x
	push 2
	pop dword ecx
	pop dword edx
	mov dword eax, [edx]
	cmp ecx, 0
	je fin_error_division
	cdq
	idiv ecx
	push dword eax
	pop dword [_y]
	push dword _y
	pop dword eax
	mov dword eax, [eax]
	push dword eax
	call print_int
	add esp, 4
	call print_endofline
	push dword _x
	push dword _y
	pop dword ecx
	mov dword edx, [ecx]
	pop dword ecx
	mov dword eax, [ecx]
	imul eax, edx
	push dword eax
	call print_int
	add esp, 4
	call print_endofline
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
