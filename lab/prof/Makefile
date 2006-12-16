
main: main.c
	gcc -m64 -O3 -Wall main.c -o main

main.s: main.c
	gcc -m64 -O3 -Wall -fomit-frame-pointer -fno-PIC -S main.c
