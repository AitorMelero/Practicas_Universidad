#-----------------------
# File: makefile
#-----------------------

CC = gcc -g -ansi
CCC = gcc -g -ansi
CFLAGS = -Wall


all : prueba_tabla

.PHONY : clean
clean :
	rm -f *.o prueba_tabla

prueba_tabla : % : tablaHash.o tablaSimbolos.o prueba_tabla.o
	@echo "#---------------------------"
	@echo "# Generando $@ "
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -o $@ $@.o tablaHash.o tablaSimbolos.o

tablaHash.o : tablaHash.c tablaHash.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CCC) $(CFLAGS) -c $<

tablaSimbolos.o : tablaSimbolos.c tablaSimbolos.h tablaHash.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CCC) $(CFLAGS) -c $<

prueba_tabla.o : prueba_tabla.c tablaHash.c tablaHash.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	$(CC) $(CFLAGS) -c $<

prueba:
	@echo Ejecutando
	@./prueba_tabla entrada.txt salida2.txt

prueba_valgrind:
	@echo Ejecutando valgrind
	@valgrind --leak-check=full ./prueba_tabla entrada.txt salida2.txt
