DESTDIR?=
PREFIX?=/usr
BINDIR?=/bin
MANDIR?=/share/man
INSTALL?=install
CFLAGS?=-Wall -O2
OFLAGS?=-O1
CC?=gcc
OWLVER=0.1.15
OWLURL=https://github.com/owl-lisp/owl/releases/download/v$(OWLVER)

bin/kal: kal.c
	-mkdir -p bin
	$(CC) $(CFLAGS) -o bin/kal kal.c
	make test

kal.c: kal.scm kal/parse.scm kal/main.scm bin/ol
	bin/ol $(OFLAGS) -o kal.c kal.scm

kal.fasl: kal.scm kal/parse.scm kal/main.scm
	$(OWL) --version || make get-owl
	$(OWL) -o kal.fasl kal.scm

test: bin/kal
	tests/check.sh bin/kal

install: bin/kal
	-mkdir -p $(DESTDIR)$(PREFIX)$(BINDIR)
	$(INSTALL) -m 755 bin/kal $(DESTDIR)$(PREFIX)$(BINDIR)/kal

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)$(BINDIR)/kal

bin/ol:
	mkdir -p bin tmp
	test -f ol-$(OWLVER).c.gz || wget $(OWLURL)/ol-$(OWLVER).c.gz
	gzip -d < ol-$(OWLVER).c.gz > tmp/ol-$(OWLVER).c
	rm ol-$(OWLVER).c.gz
	cc -O2 -o bin/ol tmp/ol-$(OWLVER).c

clean:
	-rm -f kal.c

mrproper:
	make clean
	-rm -rf owl-lisp-*

.PHONY: test clean mrproper uninstall install get-owl
