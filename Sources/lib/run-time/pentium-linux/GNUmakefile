# Build runtime library for Dylan under X86 Linux

LIBDEST          = $(FUNCTIONAL_DEVELOPER_USER_INSTALL)/lib
BINDEST          = $(FUNCTIONAL_DEVELOPER_USER_INSTALL)/bin

MPS              = /home/andreas/mps-kit-1.100.1
MPS_PLATFORM     = lii4gc
MPS_VARIANT      = hi
MPS_LIB          = $(MPS)/code/$(MPS_PLATFORM)/$(MPS_VARIANT)

EXPIRATION = -1

AS = as -L -o
AR = ar -q
CFLAGS = -DGC_LINUX_THREADS=1 -D_REENTRANT=1 -O -g -DEXPIRATION=$(EXPIRATION) -I$(MPS)/code

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
$(LIBDEST)/libmpsplan.a: $(MPS_LIB)/mpsplan.a
	cp -fp $(MPS_LIB)/mpsplan.a $(LIBDEST)/libmpsplan.a

$(LIBDEST)/libmmdw.a: $(MPS_LIB)/mmdw.a
	cp -fp $(MPS_LIB)/mmdw.a $(LIBDEST)/libmmdw.a

$(LIBDEST)/x86-linux-build.jam: x86-linux-build.jam
	cp -fp x86-linux-build.jam $(LIBDEST)

$(LIBDEST)/mini-jambase.jam: ../mini-jambase.jam
	cp -fp ../mini-jambase.jam $(LIBDEST)

$(LIBDEST)/dylan-elf-dll.script: dylan-elf-dll.script
	cp -fp dylan-elf-dll.script $(LIBDEST)

$(LIBDEST)/dylan-elf-exe.script: dylan-elf-exe.script
	cp -fp dylan-elf-exe.script $(LIBDEST)

install-build: $(LIBDEST)/libmmdw.a $(LIBDEST)/libmpsplan.a \
	       $(LIBDEST)/x86-linux-build.jam $(LIBDEST)/mini-jambase.jam \
               $(LIBDEST)/dylan-elf-dll.script $(LIBDEST)/dylan-elf-exe.script \
               collector.c

install-runtime: $(OBJS)
	cp -fp $(OBJS) $(RUNTIMELIBDEST)

install: install-runtime install-build

clean:
	rm -f *.o ; \
	rm -f collector.c
