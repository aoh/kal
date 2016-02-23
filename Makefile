kal: kal.c
	$(CC) -O -o kal kal.c

kal.c: kal.scm
	ol -O1 -o kal.c kal.scm
