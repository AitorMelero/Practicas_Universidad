segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_b1 resd 1
	_x resd 1
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
main:
	mov dword [__esp] , esp
	push dword _b1
	call scan_boolean
	add esp, 4
	push dword _x
	call scan_int
	add esp, 4
	push dword _x
	push 3
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jg near mayor0
	push dword 0
	jmp fin_mayor0
mayor0:
	push dword 1
fin_mayor0: 
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _x
	push 3
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jge near mayor_igual1
	push dword 0
	jmp fin_mayor_igual1
mayor_igual1:
	push dword 1
fin_mayor_igual1: 
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _x
	push 3
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jl near menor2
	push dword 0
	jmp fin_menor2
menor2:
	push dword 1
fin_menor2: 
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _x
	push 3
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jle near menor_igual3
	push dword 0
	jmp fin_menor_igual3
menor_igual3:
	push dword 1
fin_menor_igual3: 
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _x
	push 3
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	je near igual4
	push dword 0
	jmp fin_igual4
igual4:
	push dword 1
fin_igual4: 
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _x
	push 3
	pop dword edx
	pop dword eax
	mov dword eax, [eax]
	cmp eax, edx
	jne near distinto5
	push dword 0
	jmp fin_distinto5
distinto5:
	push dword 1
fin_distinto5: 
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _b1
	push 0
	pop dword edx
	pop dword ecx
	mov dword eax, [ecx]
	and eax, edx
	push dword eax
	call print_boolean
	add esp, 4
	call print_endofline
	push dword _b1
	push 1
	pop dword edx
	pop dword ecx
	mov dword eax, [ecx]
	or eax, edx
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
