!include <ntwin32.mak>

LINKER            = link
BINDIR            = $(DW_BIN)
LIBDIR            = $(DW_LIB)
INCDIR            = $(DW_INCLUDE)
PBINDIR           = $(DW_PERSONAL_BIN)
PLIBDIR           = $(DW_PERSONAL_LIB)
PINCDIR           = $(DW_PERSONAL_INCLUDE)
LIB               = $(PLIBDIR);$(LIBDIR);$(LIB)

oww-scribble-names.exe:
        call $(INCDIR)\set-link-options
	$(link) -out:$@ @options.lnk @libs.lnk

oww-scribble-names.lib:
	$(implib) -out:$@ @objs.lnk

install-library: oww-scribble-names.lib
	(pushd $(PLIBDIR) & del /f /q oww-scribble-names.lib & popd)
	for %%F in (oww-scribble-names.lib) do move %%F $(PLIBDIR)

install-application: oww-scribble-names.exe
	(pushd $(PBINDIR) & del /f /q oww-scribble-names.exe & del /f /q oww-scribble-names.pdb & popd)
	for %%F in (oww-scribble-names.exe oww-scribble-names.pdb) do move %%F $(PBINDIR)

install: install-library

clean:
	del /f /q oww-scribble-names.lib oww-scribble-names.exe oww-scribble-names.pdb

clean-all: clean
	del /f /q *.obj *.asm *.harp *.lnk
