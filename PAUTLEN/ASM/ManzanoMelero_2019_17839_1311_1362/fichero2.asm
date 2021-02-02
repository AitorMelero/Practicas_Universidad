segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_b1 resd 1
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
main:
	mov dword [__esp] , esp
	push dword _b1
	call scan_boolean
	add esp, 4
	push dword _b1
	pop dword eax
	mov dword eax, [eax]
	or eax, eax
	jz negar0
	mov dword eax, 0
	jmp fin_no0
negar0:
	mov dword eax, 1
fin_no0:
	push dword eax
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _b1
	pop dword eax
	mov dword eax, [eax]
	or eax, eax
	jz negar1
	mov dword eax, 0
	jmp fin_no1
negar1:
	mov dword eax, 1
fin_no1:
	push dword eax
	pop dword eax
	or eax, eax
	jz negar2
	mov dword eax, 0
	jmp fin_no2
negar2:
	mov dword eax, 1
fin_no2:
	push dword eax
	call print_boolean
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
