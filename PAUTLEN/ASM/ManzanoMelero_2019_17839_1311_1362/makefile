#-----------------------
# File: makefile
# Autor: Aitor Melero
#-----------------------

CC = gcc -ansi -pedantic
CFLAGS = -Wall
EXE = ejemplo1 ejemplo2 ejemplo3 ejemplo4 ejemplo5 ejemplo6 ejemplo7 ejemplo9 ejemplo12

all : $(EXE)

.PHONY : clean
clean :
	rm -f *.o core $(EXE)

$(EXE) : % : generacion.o ejemplo1.o ejemplo2.o ejemplo3.o ejemplo4.o ejemplo5.o ejemplo6.o ejemplo7.o ejemplo9.o ejemplo12.o
	@echo "#---------------------------"
	@echo "# Generando $@ "
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -o $@ $@.o generacion.o

generacion.o : generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo1.o : ejemplo1.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo2.o : ejemplo2.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo3.o : ejemplo3.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo4.o : ejemplo4.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo5.o : ejemplo5.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo6.o : ejemplo6.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo7.o : ejemplo7.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo9.o : ejemplo9.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo12.o : ejemplo12.c generacion.c generacion.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

ejemplo_1:
	@echo Ejecutando ejemplo 1
	@./ejemplo1 fichero1.asm

compila_1_asm:
	@echo Compilando ejemplo 1 con asm
	@nasm -g -o ej1.o -f elf32 fichero1.asm
	@gcc -m32 -o ej1 ej1.o alfalib.o

ejemplo1_valgrind:
	@echo Ejecutando ejemplo 1 con valgrind
	@valgrind --leak-check=yes --tool=memcheck ./ejemplo1 fichero1.asm

ejemplo_2:
	@echo Ejecutando ejemplo 2
	@./ejemplo2 fichero2.asm

compila_2_asm:
	@echo Compilando ejemplo 2 con asm
	@nasm -g -o ej2.o -f elf32 fichero2.asm
	@gcc -m32 -o ej2 ej2.o alfalib.o

ejemplo2_valgrind:
	@echo Ejecutando ejemplo 2 con valgrind
	@valgrind --leak-check=yes --tool=memcheck ./ejemplo2

ejemplo_3:
	@echo Ejecutando ejemplo 3
	@./ejemplo3 fichero3.asm

compila_3_asm:
	@echo Compilando ejemplo 3 con asm
	@nasm -g -o ej3.o -f elf32 fichero3.asm
	@gcc -m32 -o ej3 ej3.o alfalib.o

ejemplo3_valgrind:
	@echo Ejecutando ejemplo 3 con valgrind
	@valgrind --leak-check=yes --tool=memcheck ./ejemplo3

ejemplo_4:
	@echo Ejecutando ejemplo 4
	@./ejemplo4 fichero4.asm

compila_4_asm:
	@echo Compilando ejemplo 4 con asm
	@nasm -g -o ej4.o -f elf32 fichero4.asm
	@gcc -m32 -o ej4 ej4.o alfalib.o

ejemplo4_valgrind:
	@echo Ejecutando ejemplo 4 con valgrind
	@valgrind --leak-check=yes --tool=memcheck ./ejemplo4

ejemplo_5:
	@echo Ejecutando ejemplo 5
	@./ejemplo5 fichero5.asm

compila_5_asm:
	@echo Compilando ejemplo 5 con asm
	@nasm -g -o ej5.o -f elf32 fichero5.asm
	@gcc -m32 -o ej5 ej5.o alfalib.o

ejemplo5_valgrind:
	@echo Ejecutando ejemplo 5 con valgrind
	@valgrind --leak-check=yes --tool=memcheck ./ejemplo5

ejemplo_6:
		@echo Ejecutando ejemplo 6
		@./ejemplo6 fichero6.asm

compila_6_asm:
		@echo Compilando ejemplo 6 con asm
		@nasm -g -o ej6.o -f elf32 fichero6.asm
		@gcc -m32 -o ej6 ej6.o alfalib.o

ejemplo6_valgrind:
		@echo Ejecutando ejemplo 6 con valgrind
		@valgrind --leak-check=yes --tool=memcheck ./ejemplo6

ejemplo_7:
		@echo Ejecutando ejemplo 7
		@./ejemplo7 fichero7.asm

compila_7_asm:
		@echo Compilando ejemplo 7 con asm
		@nasm -g -o ej7.o -f elf32 fichero7.asm
		@gcc -m32 -o ej7 ej7.o alfalib.o

ejemplo7_valgrind:
		@echo Ejecutando ejemplo 7 con valgrind
		@valgrind --leak-check=yes --tool=memcheck ./ejemplo7

ejemplo_9:
		@echo Ejecutando ejemplo 9
		@./ejemplo9 fichero9.asm

compila_9_asm:
		@echo Compilando ejemplo 9 con asm
		@nasm -g -o ej9.o -f elf32 fichero9.asm
		@gcc -m32 -o ej9 ej9.o alfalib.o

ejemplo9_valgrind:
		@echo Ejecutando ejemplo 9 con valgrind
		@valgrind --leak-check=yes --tool=memcheck ./ejemplo9

ejemplo_12:
		@echo Ejecutando ejemplo 12
		@./ejemplo12 fichero12.asm

compila_12_asm:
		@echo Compilando ejemplo 12 con asm
		@nasm -g -o ej12.o -f elf32 fichero12.asm
		@gcc -m32 -o ej12 ej12.o alfalib.o

ejemplo12_valgrind:
		@echo Ejecutando ejemplo 12 con valgrind
		@valgrind --leak-check=yes --tool=memcheck ./ejemplo12
