
# make file for "gema":
#
#	use "make gcc" to build with the Gnu C compiler
#	or "make acc" to build with the Solaris ANSI C compiler
#	or "make lcc" to build with the Lucid C compiler.
#	or "make cc" to use a compiler named "cc".
#
#  (An ANSI-C compiler is required.)

# $Id: Makefile,v 1.6 2003/11/02 00:37:06 gray Exp $

INSTALLDIR = /usr/local

# The default build target runs a script to determine the appropriate options.
default: 
	gemabuild

gcc:
	${MAKE} CC=gcc CCFLAGS="-ansi -Wall -O2 -DNDEBUG" gema

lcc:
	${MAKE} CC=lcc CCFLAGS="-XG -O -Xa -DNDEBUG" gema

acc:
	${MAKE} CC=acc CCFLAGS="-O -DNDEBUG" gema

cc:
	${MAKE} CC=cc CCFLAGS="-O -DNDEBUG" gema

debug:
	${MAKE} CCFLAGS="-g" gema

OBJS =  version.o gema.o reg-expr.o var.o util.o read.o match.o \
	cstream.o action.o

CC = cc
CCFLAGS = -O -DNDEBUG

clean:
	rm -f ${OBJS} gema 

.c.o:
	${CC} -c -DTRACE ${CCFLAGS} $<

gema: ${OBJS}
	${CC} ${CCFLAGS} -o gema ${OBJS}

install: ${INSTALLDIR}/bin/gema ${INSTALLDIR}/man/man1/gema.1

${INSTALLDIR}/bin/gema: gema
	cp gema $@
	strip $@

${INSTALLDIR}/man/man1/gema.1: ../doc/gema.1
	cp ../doc/gema.1 $@

reg-expr.o: reg-expr.c reg-expr.h

var.o: var.c main.h cstream.h util.h bool.h var.h

util.o: util.c bool.h main.h util.h

read.o: read.c reg-expr.h patimp.h util.h main.h bool.h cstream.h pattern.h

match.o: match.c var.h patimp.h util.h main.h bool.h cstream.h pattern.h

gema.o: gema.c util.h pattern.h main.h bool.h cstream.h

cstream.o: cstream.c util.h main.h bool.h cstream.h

action.o: action.c patimp.h var.h util.h main.h bool.h cstream.h pattern.h

version.o: version.c
