# Build runtime library for Dylan under PPC Linux


LIBDEST          = $(OPEN_DYLAN_USER_INSTALL)/lib
BINDEST          = $(OPEN_DYLAN_USER_INSTALL)/bin

EXPIRATION = -1

AS = as -L -o
AR = ar -q
CC = gcc -O2 -g -DEXPIRATION=$(EXPIRATION)

OBJS	 = ppc-linux-collector.o runtime.o ppc-linux-threads-primitives.o linux-spy-interfaces.o linux-support.o
USEROBJ	 = dylan-support.o

RUNTIMELIBDEST	 = $(LIBDEST)/runtime/

# Install shared runtime sources

collector.c: ../collector.c
	cp -fp ../*.c .
	cp -fp ../*.h .

ppc-linux-collector.o:       collector.c linux-win32.c

ppc-linux-threads-primitives.o:  linux-threads-primitives.c linux-win32.c

linux-spy-interfaces.o: linux-spy-interfaces.c

linux-support.o: linux-support.c

runtime.o:	runtime.s
	$(AS) runtime.o runtime.s

dylan-support.o:	dylan-support.s
	$(AS) dylan-support.o dylan-support.s


$(BINDEST)/libgc.a: gc.a
	cp -fp gc.a $(BINDEST)/libgc.a

$(BINDEST)/ppc-elf-linker.script: ppc-elf-linker.script
	cp -fp ppc-elf-linker.script $(BINDEST)

install-build: $(BINDEST)/libgc.a $(BINDEST)/ppc-elf-linker.script collector.c

install-runtime: $(USEROBJ) $(OBJS)
	cp -fp $(USEROBJ)  $(LIBDEST)
	cp -fp $(OBJS) $(RUNTIMELIBDEST)
   



install: install-build install-runtime


clean:
	rm -f *.o
	rm -f collector.c
