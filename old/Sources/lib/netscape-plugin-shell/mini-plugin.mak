#
#
#

OBJS    = \
        .\objs\nptest.obj 

RES     = \
        .\objs\test-plugin.res

################################################################################
# Libraries
#
LIBS    = \
    WINMM.LIB \
    kernel32.lib \
    user32.lib \
    gdi32.lib \
    winspool.lib \
    comdlg32.lib \
    advapi32.lib \
    shell32.lib \
    ole32.lib \
    oleaut32.lib \
    uuid.lib \
    wsock32.lib




CPP_INC     = /I .\include /I_gen
CPP_OPTS    = /nologo /W3 /GX /Od /c
CPP_DEFS    = /D "WIN32" /D "_WINDOWS" /D "_USRDLL" /D "_WINDLL" /D "_MBCS" /D "XP_PC"

LINK_FLAGS   = \
        /nologo\
        /entry:"DllEntryPoint"\
        /subsystem:windows\
        /dll\
         /DEBUG:FULL\
        /DEBUGTYPE:COFF\
        /incremental:no\
        /machine:I386\
	/base:0x18000000 \
        /force \
        /def:plugin.def




CPP_OPTS    = /MDd /Zi $(CPP_OPTS)
CPP_DEFS    = /D "_DEBUG" /D "STRICT" $(CPP_DEFS)

LINK_FLAGS   = $(LINK_FLAGS) /DEBUG


CPP_FLAGS   = $(CPP_OPTS) $(CPP_DEFS) $(CPP_INC)
RC_OPTS     = /l 0x409

################################################################################
# Tools
#
RSC     = rc.exe
CPP     = cl.exe
LINK    = link.exe

TARGET = mini-plugin
!include <ntwin32.mak>

LINKER            = link
BINDIR            = $(DW_BIN)
LIBDIR            = $(DW_LIB)
INCDIR            = $(DW_INCLUDE)
PBINDIR           = $(DW_PERSONAL_BIN)
PLIBDIR           = $(DW_PERSONAL_LIB)
PINCDIR           = $(DW_PERSONAL_INCLUDE)
LIB               = $(PLIBDIR);$(LIBDIR);$(LIB)


$(TARGET).dll: $(OBJS) $(RES)
	$(link) -out:$@ @<<
	    $(OBJS) $(LIBS) $(RES) $(LINK_FLAGS) 
<<

clean:
	del /f /q $(TARGET).lib $(TARGET).dll $(TARGET).pdb

clean-all: clean
	del /f /q *.obj *.asm *.harp *.lnk .\objs\*.obj .\objs\*.res



################################################################################
# Implicit Rules
#
{.\_stubs}.c{.\objs}.obj:
    $(CPP) $(CPP_FLAGS) $< /Fo$*

.c{.\objs}.obj:
    $(CPP) $(CPP_FLAGS) $< /Fo$*

.cpp{.\objs}.obj:
    $(CPP) $(CPP_FLAGS) $< /Fo$*

.cpp.obj:
    $(CPP) $(CPP_FLAGS) $< /Fo$*

.rc{.\objs}.res:
    $(RC) $(RC_FLAGS) /fo $*.res $< 


