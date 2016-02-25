DESTDIR?=
PREFIX?=/usr
BINDIR?=/bin
MANDIR?=/share/man
INSTALL?=install
CFLAGS?=-Wall -O2
OFLAGS?=-O1
CC?=gcc

bin/kal: kal.c
	-mkdir -p bin
	$(CC) $(CFLAGS) -o bin/kal kal.c
	make test

kal.c: kal.scm kal/parse.scm kal/main.scm
	ol $(OFLAGS) -o kal.c kal.scm

test: bin/kal
	tests/check.sh bin/kal

install: bin/kal
	-mkdir -p $(DESTDIR)$(PREFIX)$(BINDIR)
	$(INSTALL) -m 755 bin/kal $(DESTDIR)$(PREFIX)$(BINDIR)/kal

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)$(BINDIR)/kal

clean:
	-rm -f kal.c

.phony: test
