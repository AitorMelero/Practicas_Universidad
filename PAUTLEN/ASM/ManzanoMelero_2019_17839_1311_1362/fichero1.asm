segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_x resd 1
	_y resd 1
	_z resd 1
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
main:
	mov dword [__esp] , esp
	push 8
	pop dword [_x]
	push dword _y
	call scan_int
	add esp, 4
	push dword _x
	push dword _y
	pop dword ecx
	mov dword edx, [ecx]
	pop dword ecx
	mov dword eax, [ecx]
	add eax, edx
	push dword eax
	pop dword [_z]
	push dword _z
	pop dword eax
	mov dword eax, [eax]
	push dword eax
	call print_int
	add esp, 4
	call print_endofline
	push 7
	push dword _y
	pop dword ecx
	mov dword edx, [ecx]
	pop dword eax
	add eax, edx
	push dword eax
	pop dword [_z]
	push dword _z
	pop dword eax
	mov dword eax, [eax]
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
