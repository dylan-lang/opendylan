# Build runtime library for Dylan under X86 Linux

LIBDEST          = $(FUNCTIONAL_DEVELOPER_USER_INSTALL)/lib
BINDEST          = $(FUNCTIONAL_DEVELOPER_USER_INSTALL)/bin

EXPIRATION = -1

AS = as -L -o
AR = ar -q
CFLAGS = -DGC_LINUX_THREADS=1 -D_REENTRANT=1 -O -g -DEXPIRATION=$(EXPIRATION) -I/home/housel/src/mps-kit/code

OBJS	 = x86-linux-collector.o runtime.o x86-linux-threads-primitives.o linux-spy-interfaces.o linux-support.o

RUNTIMELIBDEST	 = $(LIBDEST)/runtime/

# Install shared runtime sources

collector.c: ../collector.c
	cp -fp ../*.c . ; \
	cp -fp ../*.h .

x86-linux-collector.o:       collector.c

x86-linux-threads-primitives.o:  linux-threads-primitives.c

linux-spy-interfaces.o: linux-spy-interfaces.c

linux-support.o: linux-support.c

runtime.o:	runtime.s
	$(AS) runtime.o runtime.s

#$(LIBDEST)/libgc.a: gc.a
#	cp -fp gc.a $(LIBDEST)/libgc.a
#
$(LIBDEST)/libmpsplan.a: mpsplan.a
	cp -fp mpsplan.a $(LIBDEST)/libmpsplan.a

$(LIBDEST)/libmmdw.a: mmdw.a
	cp -fp mmdw.a $(LIBDEST)/libmmdw.a

$(BINDEST)/elf-linker.script: elf-linker.script
	cp -fp elf-linker.script $(BINDEST)

$(LIBDEST)/dylan-elf-dll.script: dylan-elf-dll.script
	cp -fp dylan-elf-dll.script $(LIBDEST)

$(LIBDEST)/dylan-elf-exe.script: dylan-elf-exe.script
	cp -fp dylan-elf-exe.script $(LIBDEST)

install-build: $(LIBDEST)/libmmdw.a $(LIBDEST)/libmpsplan.a $(BINDEST)/elf-linker.script \
               $(LIBDEST)/dylan-elf-dll.script $(LIBDEST)/dylan-elf-exe.script \
               collector.c

install-runtime: $(OBJS)
	cp -fp $(OBJS) $(RUNTIMELIBDEST)

install: install-runtime install-build

clean:
	rm -f *.o ; \
	rm -f collector.c
