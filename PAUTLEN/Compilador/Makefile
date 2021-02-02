
CC = gcc
BS = bison
FLAGS = -Wall -ansi
BFLAGS = -d -y -v
FX = flex

OBS = y.tab.o lex.yy.o tablaSimbolos.o tablaHash.o generacion.o

all: $(OBS)
	$(CC) $(CFLAGS) -o alfa alfa.c $(OBS)

lex.yy.o: lex.yy.c
	$(CC) $(CFLAGS) -c lex.yy.c

y.tab.o: y.tab.h y.tab.c
	$(CC) $(CFLAGS) -c y.tab.c

generacion.o: generacion.c generacion.h
	$(CC) $(CFLAGS) -c generacion.c

tablaHash.o: tablaHash.c tablaHash.h
	$(CC) $(CFLAGS) -c tablaHash.c

tablaSimbolos.o: tablaSimbolos.c tablaSimbolos.h
	$(CC) $(CFLAGS) -c tablaSimbolos.c

y.tab.c: alfa.y
	$(BS) $(BFLAGS) alfa.y

y.tab.h: alfa.y
	$(BS) $(BFLAGS) alfa.y

lex.yy.c: alfa.l y.tab.h
	$(FX) alfa.l

compila_1_asm:
	@echo Compilando ejemplo 1 con asm
	@nasm -g -o ej1.o -f elf32 salida.asm
	@gcc -m32 -o ej1 ej1.o alfalib.o

clear:
	rm -rf $(EXE) $(OBS)


borrar:
	rm lex.yy.c y.tab.h
