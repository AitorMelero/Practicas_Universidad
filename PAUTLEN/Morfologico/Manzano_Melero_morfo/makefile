
CC = gcc
FLEX = flex

all: pruebaMorfo

pruebaMorfo: main.c lex.yy.c
	$(CC) -o pruebaMorfo main.c lex.yy.c
lex.yy.c: alfa.l
	$(FLEX) alfa.l
clean:
	rm pruebaMorfo lex.yy.c *.txt
