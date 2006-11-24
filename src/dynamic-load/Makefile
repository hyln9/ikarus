
all: library1.so library2.so client

library1.so: library1.c
	gcc -Wall -shared -o library1.so library1.c

library2.so: library2.c
	gcc -Wall -shared -o library2.so library2.c

client: client.c
	gcc -Wall -ldl -o client client.c

