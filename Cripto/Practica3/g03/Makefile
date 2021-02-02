#################################################
# File: Makefile
# Author: Aitor Melero, Ana Roa
# Date:	05/11/2020
# Last_Date: 13/11/2020
# Function: Makefile para P3 de cripto.
#################################################

#################################################
CC = gcc
CFLAGS = -g -Wall -pedantic -ansi
EJS = potencia primo vegas clean
#################################################
OBJECTS_2A = potencia.o
OBJECTS_2B = primo.o
OBJECTS_2C = vegas.o
#################################################

all: $(EJS)

potencia: $(OBJECTS_2A)
	$(CC) $(CFLAGS) -o potencia $(OBJECTS_2A) -lgmp -lm -L.

primo: $(OBJECTS_2B)
	$(CC) $(CFLAGS) -o primo $(OBJECTS_2B) -lgmp -lm -L.

vegas: $(OBJECTS_2C)
	$(CC) $(CFLAGS) -o vegas $(OBJECTS_2C) -lgmp -lm -L.

vegas.o: type.h vegas.c
	$(CC) $(CFLAGS) -c vegas.c

clean:
	rm -rf *.o

valgrind_2a:
	valgrind --leak-check=yes ./potencia 859663333789656222223 8877778888888888888884 45774555

valgrind_2b:
	valgrind --leak-check=yes ./primo -b 8096 -p 1.07 -o salida.txt

apartado2c:
	./vegas -b 8192
