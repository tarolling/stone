all: reference.s

reference.s: reference.c
	gcc -O0 -S reference.c -masm=att