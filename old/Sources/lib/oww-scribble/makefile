!include <ntwin32.mak>

LINKER            = link
BINDIR            = $(DW_BIN)
LIBDIR            = $(DW_LIB)
INCDIR            = $(DW_INCLUDE)
PBINDIR           = $(DW_PERSONAL_BIN)
PLIBDIR           = $(DW_PERSONAL_LIB)
PINCDIR           = $(DW_PERSONAL_INCLUDE)
LIB               = $(PLIBDIR);$(LIBDIR);$(LIB)

oww-scribble.exe:
        call $(INCDIR)\set-link-options
	$(link) -out:$@ @options.lnk @libs.lnk

oww-scribble.lib:
	$(implib) -out:$@ @objs.lnk

install-library: oww-scribble.lib
	(pushd $(PLIBDIR) & del /f /q oww-scribble.lib & popd)
	for %%F in (oww-scribble.lib) do move %%F $(PLIBDIR)

install-application: oww-scribble.exe
	(pushd $(PBINDIR) & del /f /q oww-scribble.exe & del /f /q oww-scribble.pdb & popd)
	for %%F in (oww-scribble.exe oww-scribble.pdb) do move %%F $(PBINDIR)

install: install-library

clean:
	del /f /q oww-scribble.lib oww-scribble.exe oww-scribble.pdb

clean-all: clean
	del /f /q *.obj *.asm *.harp *.lnk
