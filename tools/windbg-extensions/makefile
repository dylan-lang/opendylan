# Nmake macros for building Windows 32-Bit apps

!include <ntwin32.mak>

PROJ = wdbgdylan

all: $(PROJ).dll

OBJS = mangler.obj debugger.obj accessors.obj print.obj

# Update the object files if necessary

$(PROJ).obj: $(PROJ).c
    $(cc) $(cflags) $(cvarsdll) $(cdebug) $(scall) -I. -I..\..\sdktools\image\include $*.c

mangler.obj: mangler.c
    $(cc) $(cflags) $(cvarsdll) $(cdebug) $(scall) -I. -I..\..\sdktools\image\include $*.c

debugger.obj: debugger.c
    $(cc) $(cflags) $(cvarsdll) $(cdebug) $(scall) -I. -I..\..\sdktools\image\include $*.c

accessors.obj: accessors.c
    $(cc) $(cflags) $(cvarsdll) $(cdebug) $(scall) -I. -I..\..\sdktools\image\include $*.c

print.obj: print.c
    $(cc) $(cflags) $(cvarsdll) $(cdebug) $(scall) -I. -I..\..\sdktools\image\include $*.c

# Update the import library

$(PROJ).lib: $(PROJ).obj $(OBJS) $(PROJ).def
    $(implib) -machine:$(CPU) \
    -def:$*.def \
    $*.obj $(OBJS) \
    -out:$*.lib

# Update the dynamic link library

$(PROJ).dll: $(PROJ).obj $(PROJ).def $(PROJ).lib
    $(link) $(linkdebug) $(dlllflags) \
    -base:0x1C000000 \
    -out:$*.dll \
    $*.exp $*.obj $(OBJS) $(conlibsdll)

install: $(PROJ).dll
    copy $(PROJ).dll C:\mstools\bin
