#################################################
# File: Makefile
# Author: Aitor Melero, Ana Roa
# Date:	08/10/2020
# Last_Date: 26/10/2020
# Function: Makefile para P1.
#################################################

#################################################
CC = gcc
CFLAGS = -g -Wall -pedantic -ansi
EJS = afin afin_2 vigenere descifrador flujo permutacion clean
#################################################
OBJECTS_1A = apartado_1a.o afin.o euclides.o
OBJECTS_1B = apartado_1b.o afin_2.o euclides.o
OBJECTS_2A = apartado_2a.o vigenere.o euclides.o
OBJECTS_2B = apartado_2b.o descifrador.o ic.o kasiski.o vigenere.o euclides.o
OBJECTS_3 = apartado_3.o flujo.o euclides.o
OBJECTS_4 = apartado_4.o permutacion.o
#################################################

all: $(EJS)

afin: $(OBJECTS_1A)
	$(CC) $(CFLAGS) -o afin $(OBJECTS_1A) -lgmp -L.

afin_2: $(OBJECTS_1B)
	$(CC) $(CFLAGS) -o afin_2 $(OBJECTS_1B) -lgmp -L.

vigenere: $(OBJECTS_2A)
	$(CC) $(CFLAGS) -o vigenere $(OBJECTS_2A) -lgmp -L.

descifrador: $(OBJECTS_2B)
	$(CC) $(CFLAGS) -o descifrador $(OBJECTS_2B) -lgmp -L.

flujo: $(OBJECTS_3)
	$(CC) $(CFLAGS) -o flujo $(OBJECTS_3) -lgmp -L.

permutacion: $(OBJECTS_4)
	$(CC) $(CFLAGS) -o permutacion $(OBJECTS_4)

apartado_1a.o: apartado_1a.c afin.o euclides.o
	$(CC) $(CFLAGS) -c apartado_1a.c

apartado_1b.o: apartado_1b.c afin_2.o euclides.o
	$(CC) $(CFLAGS) -c apartado_1b.c

apartado_2a.o: apartado_2a.c vigenere.o euclides.o
	$(CC) $(CFLAGS) -c apartado_2a.c

apartado_2b.o: apartado_2b.c descifrador.o ic.o kasiski.o euclides.o
	$(CC) $(CFLAGS) -c apartado_2b.c

apartado_3.o: apartado_3.c flujo.o euclides.o
	$(CC) $(CFLAGS) -c apartado_3.c

apartado_4.o: apartado_4.c permutacion.o
	$(CC) $(CFLAGS) -c apartado_4.c

afin.o: afin.h afin.c type.h euclides.o
	$(CC) $(CFLAGS) -c afin.c

afin_2.o: afin_2.h afin_2.c type.h euclides.o
	$(CC) $(CFLAGS) -c afin_2.c

vigenere.o: vigenere.h vigenere.c type.h euclides.o
	$(CC) $(CFLAGS) -c vigenere.c

descifrador.o: descifrador.h descifrador.c type.h ic.o kasiski.o vigenere.o euclides.o
	$(CC) $(CFLAGS) -c descifrador.c

ic.o: ic.h ic.c type.h euclides.o
	$(CC) $(CFLAGS) -c ic.c

kasiski.o: kasiski.h kasiski.c type.h  euclides.o
	$(CC) $(CFLAGS) -c kasiski.c

flujo.o: flujo.h flujo.c type.h euclides.o
	$(CC) $(CFLAGS) -c flujo.c

permutacion.o: permutacion.h permutacion.c type.h
	$(CC) $(CFLAGS) -c permutacion.c

euclides.o: euclides.h euclides.c type.h
	$(CC) $(CFLAGS) -c euclides.c -lgmp -L.

#################################################
# Limpiamos los *.o
#################################################
clean:
	rm -rf *.o

#################################################
# Ejecuciones con valgrind para apartado 1A
#################################################
valgrind_1a_1:
	valgrind --leak-check=yes ./afin -C -m 26 -a 5 -b 15 -i prueba.txt

valgrind_1a_2:
	valgrind --leak-check=yes ./afin -D -m 26 -a 5 -b 15 -i salida.txt

valgrind_1a_3:
	valgrind --leak-check=yes ./afin -C -m 26 -a 5 -b 15

#################################################
# Ejecuciones con valgrind para apartado 1B
#################################################
valgrind_1b_1:
	valgrind --leak-check=yes ./afin_2 -C -m 26 -a 5 -b 15 -i prueba.txt

valgrind_1b_2:
	valgrind --leak-check=yes ./afin_2 -D -m 26 -a 5 -b 15 -i salida.txt

valgrind_1b_3:
	valgrind --leak-check=yes ./afin_2 -C -m 26 -a 5 -b 15

#################################################
# Ejecuciones con valgrind para apartado 2A
#################################################
valgrind_2a_1:
	valgrind --leak-check=yes ./vigenere -C -k CLAVE

valgrind_2a_2:
	valgrind --leak-check=yes ./vigenere -C -k CLAVE -i prueba.txt

valgrind_2a_3:
	valgrind --leak-check=yes ./vigenere -D -k CLAVE -i salida.txt

#################################################
# Ejecuciones con valgrind para apartado 2B
#################################################
valgrind_2b_1:
	valgrind --leak-check=yes ./descifrador -met i -l 4 -i clave.txt -o SOLUCION.txt

valgrind_2b_2:
	valgrind --leak-check=yes ./descifrador -met k -l 3 -i clave.txt -o SOLUCION.txt

#################################################
# Ejecuciones con valgrind para apartado 3
#################################################
valgrind_3_1:
	valgrind --leak-check=yes ./flujo -C -m 26 -a 5 -b 15 -i prueba.txt

valgrind_3_2:
	valgrind --leak-check=yes ./flujo -D -m 26 -a 5 -b 15 -i salida.txt

valgrind_3_3:
	valgrind --leak-check=yes ./flujo -D -m 26 -a 5 -b 15

#################################################
# Ejecuciones con valgrind para apartado 4
#################################################
valgrind_4_1:
	valgrind --leak-check=yes ./permutacion -C -k1 321 -k2 231 -i alicia.txt -o solucion_permutacion.txt

valgrind_4_2:
	valgrind --leak-check=yes ./permutacion -C -k1 321 -k2 231

valgrind_4_3:
	valgrind --leak-check=yes ./permutacion -D -k1 321 -k2 231 -i solucion_permutacion.txt -o solucion_permutacion_plano.txt

valgrind_4_4:
	valgrind --leak-check=yes ./permutacion -D -k1 321 -k2 231 -i solucion_permutacion.txt

valgrind_4_5:
	valgrind --leak-check=yes ./permutacion -D -k1 321 -k2 231