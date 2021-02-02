#################################################
# File: Makefile
# Author: Aitor Melero, Ana Roa
# Date:	05/11/2020
# Last_Date: 17/11/2020
# Function: Makefile para P2 de cripto.
#################################################

#################################################
CC = gcc
CFLAGS = -g -Wall -pedantic -ansi
EJS = seg_perf sbox_AES desCBC TDEA-CBC  no_linealidad_SBoxes_DES avalancha_DES no_linealidad_SBoxes_AES clean
#################################################
OBJECTS_1 = seg_perf.o
OBJECTS_2A = desCBC.o operaciones_bit.o DES.o CBC.o
OBJECTS_2B = TDEA_CBC.o TDEA.o operaciones_bit.o DES.o CBC.o
OBJECTS_3A = no_linealidad_SBoxes_DES.o operaciones_bit.o DES.o
OBJECTS_3B = avalancha_DES.o operaciones_bit.o DES.o
OBJECTS_4A = no_linealidad_SBoxes_AES.o operaciones_bit.o
OBJECTS_4B = sbox_AES.o operaciones_bit.o
#################################################

all: $(EJS)

seg_perf: $(OBJECTS_1)
	$(CC) $(CFLAGS) -o seg_perf $(OBJECTS_1) -lgmp -lm -L.

sbox_AES: $(OBJECTS_4B)
	$(CC) $(CFLAGS) -o sbox_AES $(OBJECTS_4B) -lgmp -lm -L.

desCBC: $(OBJECTS_2A)
	$(CC) $(CFLAGS) -o desCBC $(OBJECTS_2A) -lgmp -lm -L.

TDEA-CBC: $(OBJECTS_2B)
	$(CC) $(CFLAGS) -o TDEA-CBC $(OBJECTS_2B) -lgmp -lm -L.

no_linealidad_SBoxes_DES: $(OBJECTS_3A)
	$(CC) $(CFLAGS) -o no_linealidad_SBoxes_DES $(OBJECTS_3A) -lgmp -lm -L.

avalancha_DES: $(OBJECTS_3B)
	$(CC) $(CFLAGS) -o avalancha_DES $(OBJECTS_3B) -lgmp -lm -L.

no_linealidad_SBoxes_AES: $(OBJECTS_4A)
	$(CC) $(CFLAGS) -o no_linealidad_SBoxes_AES $(OBJECTS_4A) -lgmp -lm -L.

seg_perf.o: seg_perf.h seg_perf.c type.h
	$(CC) $(CFLAGS) -c seg_perf.c

sbox_AES.o: sbox_AES.c operaciones_bit.o
	$(CC) $(CFLAGS) -c sbox_AES.c

desCBC.o: desCBC.c type.h operaciones_bit.o DES.o
	$(CC) $(CFLAGS) -c desCBC.c

TDEA_CBC.o: TDEA_CBC.c type.h operaciones_bit.o DES.o TDEA.o
	$(CC) $(CFLAGS) -c TDEA_CBC.c

no_linealidad_SBoxes_DES.o: no_linealidad_SBoxes_DES.c type.h operaciones_bit.o DES.o
	$(CC) $(CFLAGS) -c no_linealidad_SBoxes_DES.c

no_linealidad_SBoxes_AES.o: no_linealidad_SBoxes_AES.c type.h operaciones_bit.o
	$(CC) $(CFLAGS) -c no_linealidad_SBoxes_AES.c

avalancha_DES.o: avalancha_DES.c type.h operaciones_bit.o DES.o
	$(CC) $(CFLAGS) -c avalancha_DES.c

DES.o: type.h DES.h DES.c operaciones_bit.c
	$(CC) $(CFLAGS) -c DES.c

operaciones_bit.o: type.h operaciones_bit.h operaciones_bit.c DES.c
	$(CC) $(CFLAGS) -c operaciones_bit.c

CBC.o: type.h CBC.h CBC.c DES.c operaciones_bit.c 
	$(CC) $(CFLAGS) -c CBC.c

TDEA.o: type.h TDEA.h TDEA.c DES.c operaciones_bit.c 
	$(CC) $(CFLAGS) -c TDEA.c

clean:
	rm -rf *.o

apartado_1:
	./seg_perf -P -i don_quixote.txt -o apartado1.txt

apartado_2a_1:
	./desCBC -C -iv 12345678 -i prueba.txt -o CBC_cifrado.dec

apartado_2a_2:
	./desCBC -D -k RCEFIJLO -iv 12345678 -i CBC_cifrado.dec -o SOLUCION_CBC.txt

apartado_2b_1:
	./TDEA-CBC -C -iv 12345678 -i prueba.txt -o CBC_cifrado.dec

apartado_2b_2:
	./TDEA-CBC -D -k RCEFIJLORCEFIJLORCEFIJLO -iv 12345678 -i TDEA_cifrado.dec -o SOLUCION_TDEA.txt

apartado_3a:
	./no_linealidad_SBoxes_DES

apartado_3b:
	./avalancha_DES

apartado_4a_1:
	./no_linealidad_SBoxes_AES -D

apartado_4a_2:
	./no_linealidad_SBoxes_AES -I

apartado_4b:
	./sbox_AES -C

valgrind_1:
	valgrind --leak-check=yes ./seg_perf -P -i don_quixote.txt -o apartado1.txt

valgrind_2a_1:
	valgrind --leak-check=yes ./desCBC -C -iv 12345678 -i prueba.txt -o CBC_cifrado.dec

valgrind_2a_2:
	valgrind --leak-check=yes ./desCBC -D -k RCEFIJLO -iv 12345678 -i CBC_cifrado.dec -o SOLUCION_CBC.txt

valgrind_2b_1:
	valgrind --leak-check=yes ./TDEA-CBC -C -iv 12345678 -i prueba.txt -o TDEA_cifrado.dec

valgrind_2b_2:
	valgrind --leak-check=yes ./TDEA-CBC -D -k RCEFIJLORCEFIJLORCEFIJLO -iv 12345678 -i TDEA_cifrado.dec -o SOLUCION_TDEA.txt

valgrind_3a:
	valgrind --leak-check=yes ./no_linealidad_SBoxes_DES

valgrind_3b:
	valgrind --leak-check=yes ./avalancha_DES

valgrind_4a_1:
	valgrind --leak-check=yes ./no_linealidad_SBoxes_AES -D

valgrind_4a_2:
	valgrind --leak-check=yes ./no_linealidad_SBoxes_AES -I

valgrind_4b:
	valgrind --leak-check=yes ./sbox_AES -C
