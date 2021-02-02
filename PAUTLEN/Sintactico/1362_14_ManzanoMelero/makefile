
CC = gcc
CFLAGS = -Wall

all: lex.yy.o y.tab.o pruebaSintactico.o
	$(CC) $(CFLAGS) -o pruebaSintactico lex.yy.o y.tab.o pruebaSintactico.o
pruebaSintactico.o: pruebaSintactico.c y.tab.h
	$(CC) $(CFLAGS) -c pruebaSintactico.c
lex.yy.o: lex.yy.c y.tab.h
	$(CC) -c lex.yy.c
y.tab.o: y.tab.c
	$(CC) $(CFLAGS) -c y.tab.c
lex.yy.c: alfa.l
	$ flex alfa.l
y.tab.h: alfa.y
	$ bison -d -y -v alfa.y
clean_txt:
	rm salida1.txt salida2.txt salida3.txt
clean:
	rm pruebaSintactico  *.o lex.yy.c y.tab.c *.h y.output

ejemplo1:
	@echo ejecutando ejemplo1
	@./pruebaSintactico entrada_sin_1.txt salida1.txt
comprueba_1:
	@echo comprobando ejemplo 1
	@diff -Bb salida1.txt salida_sin_1.txt

ejemplo2:
	@echo ejecutando ejemplo2
	@./pruebaSintactico entrada_sin_2.txt salida2.txt
comprueba_2:
	@echo comprobando ejemplo 2
	@diff -Bb salida2.txt salida_sin_2.txt

ejemplo3:
	@echo ejecutando ejemplo3
	@./pruebaSintactico entrada_sin_3.txt salida3.txt
comprueba_3:
	@echo comprobando ejemplo 3
	@diff -Bb salida3.txt salida_sin_3.txt
