segment .data
	_error_division0 db "ERROR, division por 0.", 0
	_error_indice db "Indice fuera de rango.", 0
segment .bss
	__esp resd 1
	_z resd 1
segment .text
	global main
	extern scan_int, print_int, scan_boolean, print_boolean, print_endofline, print_blank, print_string
doble:
	push dword ebp
	mov ebp, esp
	sub esp, 4
	lea eax, [ebp + 8]
	push dword eax
	lea eax, [ebp - 4]
	push dword eax
	pop dword ebx
	pop dword eax
	mov dword eax, [eax]
	mov dword [ebx], eax
	push 2
	lea eax, [ebp + 8]
	push dword eax
	pop dword ecx
	mov dword edx, [ecx]
	pop dword eax
	imul eax, edx
	push dword eax
	pop eax
	mov esp,ebp
	pop ebp
	ret
main:
	mov dword [__esp] , esp
	push 2
	pop dword [_z]
	push dword _z
	pop dword eax
	mov eax, [eax]
	push dword eax
	call doble
	add esp, 4
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
