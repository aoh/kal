DESTDIR?=
PREFIX?=/usr
BINDIR?=/bin
MANDIR?=/share/man
INSTALL?=install
CFLAGS?=-Wall -O2
OFLAGS?=-O1
CC?=gcc
OWLVERSION=0.1.11
OWL=owl-lisp-$(OWLVERSION)/bin/vm owl-lisp-$(OWLVERSION)/fasl/init.fasl

# If you already have owl, you can use it to build with 
#    $ make OWL=/usr/bin/ol

bin/kal: kal.c
	-mkdir -p bin
	$(CC) $(CFLAGS) -o bin/kal kal.c
	make test

kal.c: kal.scm kal/parse.scm kal/main.scm
	$(OWL) --version || make get-owl
	$(OWL) $(OFLAGS) -o kal.c kal.scm

test: bin/kal
	tests/check.sh bin/kal

install: bin/kal
	-mkdir -p $(DESTDIR)$(PREFIX)$(BINDIR)
	$(INSTALL) -m 755 bin/kal $(DESTDIR)$(PREFIX)$(BINDIR)/kal

uninstall:
	-rm -f $(DESTDIR)$(PREFIX)$(BINDIR)/kal

get-owl:
	test -d owl-lisp-$(OWLVERSION) || curl -L https://github.com/aoh/owl-lisp/archive/v$(OWLVERSION).tar.gz | tar -zxvf -
	cd owl-lisp-$(OWLVERSION) && make bin/vm

clean:
	-rm -f kal.c

mrproper:
	make clean
	-rm -rf owl-lisp-*

.PHONY: test clean mrproper uninstall install get-owl
