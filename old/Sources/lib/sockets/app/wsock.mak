# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101
# TARGTYPE "Win32 (MIPS) Application" 0x0501

!IF "$(CFG)" == ""
CFG=Win32 (80x86) Debug
!MESSAGE No configuration specified.  Defaulting to Win32 (80x86) Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 (80x86) Release" && "$(CFG)" != "Win32 (80x86) Debug" &&\
 "$(CFG)" != "Win32 (MIPS) Debug" && "$(CFG)" != "Win32 (MIPS) Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "wsock.mak" CFG="Win32 (80x86) Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 (80x86) Release" (based on "Win32 (x86) Application")
!MESSAGE "Win32 (80x86) Debug" (based on "Win32 (x86) Application")
!MESSAGE "Win32 (MIPS) Debug" (based on "Win32 (MIPS) Application")
!MESSAGE "Win32 (MIPS) Release" (based on "Win32 (MIPS) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 (MIPS) Debug"

!IF  "$(CFG)" == "Win32 (80x86) Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(OUTDIR)/idvm-engine.exe $(OUTDIR)/wsock.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

MTL=MkTypLib.exe
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
CPP=cl.exe
# ADD BASE CPP /nologo /MD /W3 /GX /YX /O2 /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /YX /O2 /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "WIN32" /FR /c
CPP_PROJ=/nologo /MT /W3 /GX /YX /O2 /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D\
 "WIN32" /FR$(INTDIR)/ /Fp$(OUTDIR)/"wsock.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"wsock.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# SUBTRACT BASE BSC32 /Iu
# ADD BSC32 /nologo
# SUBTRACT BSC32 /Iu
BSC32_FLAGS=/nologo /o$(OUTDIR)/"wsock.bsc" 
BSC32_SBRS= \
	$(INTDIR)/wsock.sbr \
	$(INTDIR)/dialogs.sbr

$(OUTDIR)/wsock.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib mfc30.lib mfco30.lib mfcd30.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386 /FORCE
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO\
 /SUBSYSTEM:windows /INCREMENTAL:no /PDB:$(OUTDIR)/"wsock.pdb" /MACHINE:I386 /FORCE\
 /OUT:$(OUTDIR)/"idvm-engine.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/wsock.res \
	$(INTDIR)/wsock.obj \
	$(INTDIR)/dialogs.obj \
	"\DYLAN\build\mmdw.lib" \
	"\DYLAN\build\runtime.lib" \
	"\DYLAN\build\idvm\idvm.lib" \
	"\DYLAN\build\dylan-print\dylan-print.lib" \
	"\DYLAN\build\simple-streams\simple-streams.lib" \
	"\DYLAN\build\dylan\dylan.lib" \
	"\DYLAN\build\win32-interface\win32-interface.lib" \
	"\DYLAN\build\functional-extensions\functional-extensions.lib" \
	"\DYLAN\build\functional-dylan\functional-dylan.lib" \
	"\DYLAN\build\set\set.lib" \
	"\DYLAN\build\types\types.lib" \
	"\DYLAN\build\byte-vector\byte-vector.lib" \
	"\DYLAN\build\equal-table\equal-table.lib" \
	"\DYLAN\build\tcp-streams\streams.lib" \
	"\DYLAN\build\variable-search\variable-search.lib" \
	"\DYLAN\build\namespace\namespace.lib" \
	"\DYLAN\build\appl-names\appl-names.lib" \
	"\DYLAN\build\dylan-names\dylan-names.lib" \
	"\DYLAN\build\simple-streams-names\simple-streams-names.lib" \
	"\DYLAN\build\idvm-names\idvm-names.lib" \
	"\DYLAN\build\doss\doss.lib" \
	"\DYLAN\build\c-ffi\c-ffi.lib"

$(OUTDIR)/idvm-engine.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 (80x86) Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : $(OUTDIR)/idvm-engine.exe $(OUTDIR)/wsock.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

MTL=MkTypLib.exe
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
CPP=cl.exe
# ADD BASE CPP /nologo /MD /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "WIN32" /FR /c
CPP_PROJ=/nologo /MT /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "_WINDOWS" /D "_MBCS"\
 /D "WIN32" /FR$(INTDIR)/ /Fp$(OUTDIR)/"wsock.pch" /Fo$(INTDIR)/\
 /Fd$(OUTDIR)/"wsock.pdb" /c 
CPP_OBJS=.\WinDebug/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"wsock.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# SUBTRACT BASE BSC32 /Iu
# ADD BSC32 /nologo
# SUBTRACT BSC32 /Iu
BSC32_FLAGS=/nologo /o$(OUTDIR)/"wsock.bsc" 
BSC32_SBRS= \
	$(INTDIR)/wsock.sbr \
	$(INTDIR)/dialogs.sbr

$(OUTDIR)/wsock.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib mfc30d.lib mfco30d.lib mfcd30d.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386 /FORCE
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO\
 /SUBSYSTEM:windows /INCREMENTAL:yes /PDB:$(OUTDIR)/"wsock.pdb" /DEBUG /FORCE\
 /MACHINE:I386 /OUT:$(OUTDIR)/"idvm-engine.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/wsock.res \
	$(INTDIR)/wsock.obj \
	$(INTDIR)/dialogs.obj \
	"\DYLAN\build\mmdw.lib" \
	"\DYLAN\build\runtime.lib" \
	"\DYLAN\build\idvm\idvm.lib" \
	"\DYLAN\build\dylan-print\dylan-print.lib" \
	"\DYLAN\build\simple-streams\simple-streams.lib" \
	"\DYLAN\build\dylan\dylan.lib" \
	"\DYLAN\build\win32-interface\win32-interface.lib" \
	"\DYLAN\build\functional-extensions\functional-extensions.lib" \
	"\DYLAN\build\functional-dylan\functional-dylan.lib" \
	"\DYLAN\build\set\set.lib" \
	"\DYLAN\build\types\types.lib" \
	"\DYLAN\build\byte-vector\byte-vector.lib" \
	"\DYLAN\build\equal-table\equal-table.lib" \
	"\DYLAN\build\tcp-streams\streams.lib" \
	"\DYLAN\build\variable-search\variable-search.lib" \
	"\DYLAN\build\namespace\namespace.lib" \
	"\DYLAN\build\appl-names\appl-names.lib" \
	"\DYLAN\build\dylan-names\dylan-names.lib" \
	"\DYLAN\build\simple-streams-names\simple-streams-names.lib" \
	"\DYLAN\build\idvm-names\idvm-names.lib" \
	"\DYLAN\build\doss\doss.lib" \
	"\DYLAN\build\c-ffi\c-ffi.lib"

$(OUTDIR)/idvm-engine.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Debug"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 1
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : $(OUTDIR)/idvm-engine.exe $(OUTDIR)/wsock.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

MTL=MkTypLib.exe
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
CPP=cl.exe
# ADD BASE CPP /nologo /MD /Gt0 /QMOb2000 /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /c
# ADD CPP /nologo /MT /Gt0 /QMOb2000 /W3 /GX /Zi /YX /Od /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "WIN32" /FR /c
CPP_PROJ=/nologo /MT /Gt0 /QMOb2000 /W3 /GX /Zi /YX /Od /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /D "WIN32" /FR$(INTDIR)/ /Fp$(OUTDIR)/"wsock.pch"\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"wsock.pdb" /c 
CPP_OBJS=.\WinDebug/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"wsock.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# SUBTRACT BASE BSC32 /Iu
# ADD BSC32 /nologo
# SUBTRACT BSC32 /Iu
BSC32_FLAGS=/nologo /o$(OUTDIR)/"wsock.bsc" 
BSC32_SBRS= \
	$(INTDIR)/wsock.sbr \
	$(INTDIR)/dialogs.sbr

$(OUTDIR)/wsock.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib odbc32.lib mfc30d.lib mfco30d.lib mfcd30d.lib oleaut32.lib uuid.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:MIPS
# SUBTRACT BASE LINK32 /PDB:none
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:MIPS /FORCE
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO\
 /SUBSYSTEM:windows /PDB:$(OUTDIR)/"wsock.pdb" /DEBUG /MACHINE:MIPS /FORCE\
 /OUT:$(OUTDIR)/"idvm-engine.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/wsock.res \
	$(INTDIR)/wsock.obj \
	$(INTDIR)/dialogs.obj \
	"\DYLAN\build\mmdw.lib" \
	"\DYLAN\build\runtime.lib" \
	"\DYLAN\build\idvm\idvm.lib" \
	"\DYLAN\build\dylan-print\dylan-print.lib" \
	"\DYLAN\build\simple-streams\simple-streams.lib" \
	"\DYLAN\build\dylan\dylan.lib" \
	"\DYLAN\build\win32-interface\win32-interface.lib" \
	"\DYLAN\build\functional-extensions\functional-extensions.lib" \
	"\DYLAN\build\functional-dylan\functional-dylan.lib" \
	"\DYLAN\build\set\set.lib" \
	"\DYLAN\build\types\types.lib" \
	"\DYLAN\build\byte-vector\byte-vector.lib" \
	"\DYLAN\build\equal-table\equal-table.lib" \
	"\DYLAN\build\tcp-streams\streams.lib" \
	"\DYLAN\build\variable-search\variable-search.lib" \
	"\DYLAN\build\namespace\namespace.lib" \
	"\DYLAN\build\appl-names\appl-names.lib" \
	"\DYLAN\build\dylan-names\dylan-names.lib" \
	"\DYLAN\build\simple-streams-names\simple-streams-names.lib" \
	"\DYLAN\build\idvm-names\idvm-names.lib" \
	"\DYLAN\build\doss\doss.lib" \
	"\DYLAN\build\c-ffi\c-ffi.lib"

$(OUTDIR)/idvm-engine.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Release"

# PROP BASE Use_MFC 2
# PROP BASE Use_Debug_Libraries 0
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRel"
# PROP Intermediate_Dir "WinRel"
OUTDIR=.\WinRel
INTDIR=.\WinRel

ALL : $(OUTDIR)/idvm-engine.exe $(OUTDIR)/wsock.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

MTL=MkTypLib.exe
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
CPP=cl.exe
# ADD BASE CPP /nologo /MD /Gt0 /QMOb2000 /W3 /GX /YX /O2 /D "NDEBUG" /D "_WINDOWS" /D "_AFXDLL" /D "_MBCS" /FR /c
# ADD CPP /nologo /MT /Gt0 /QMOb2000 /W3 /GX /YX /O2 /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "WIN32" /FR /c
CPP_PROJ=/nologo /MT /Gt0 /QMOb2000 /W3 /GX /YX /O2 /D "NDEBUG" /D "_WINDOWS"\
 /D "_MBCS" /D "WIN32" /FR$(INTDIR)/ /Fp$(OUTDIR)/"wsock.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=.\WinRel/

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

RSC=rc.exe
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"wsock.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# SUBTRACT BASE BSC32 /Iu
# ADD BSC32 /nologo
# SUBTRACT BSC32 /Iu
BSC32_FLAGS=/nologo /o$(OUTDIR)/"wsock.bsc" 
BSC32_SBRS= \
	$(INTDIR)/wsock.sbr \
	$(INTDIR)/dialogs.sbr

$(OUTDIR)/wsock.bsc : $(OUTDIR)  $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib odbc32.lib mfc30.lib mfco30.lib mfcd30.lib oleaut32.lib uuid.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:MIPS
# SUBTRACT BASE LINK32 /PDB:none
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:MIPS /FORCE
# SUBTRACT LINK32 /PDB:none
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /NOLOGO\
 /SUBSYSTEM:windows /PDB:$(OUTDIR)/"wsock.pdb" /MACHINE:MIPS /FORCE\
 /OUT:$(OUTDIR)/"idvm-engine.exe" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/wsock.res \
	$(INTDIR)/wsock.obj \
	$(INTDIR)/dialogs.obj \
	"\DYLAN\build\mmdw.lib" \
	"\DYLAN\build\runtime.lib" \
	"\DYLAN\build\idvm\idvm.lib" \
	"\DYLAN\build\dylan-print\dylan-print.lib" \
	"\DYLAN\build\simple-streams\simple-streams.lib" \
	"\DYLAN\build\dylan\dylan.lib" \
	"\DYLAN\build\win32-interface\win32-interface.lib" \
	"\DYLAN\build\functional-extensions\functional-extensions.lib" \
	"\DYLAN\build\functional-dylan\functional-dylan.lib" \
	"\DYLAN\build\set\set.lib" \
	"\DYLAN\build\types\types.lib" \
	"\DYLAN\build\byte-vector\byte-vector.lib" \
	"\DYLAN\build\equal-table\equal-table.lib" \
	"\DYLAN\build\tcp-streams\streams.lib" \
	"\DYLAN\build\variable-search\variable-search.lib" \
	"\DYLAN\build\namespace\namespace.lib" \
	"\DYLAN\build\appl-names\appl-names.lib" \
	"\DYLAN\build\dylan-names\dylan-names.lib" \
	"\DYLAN\build\simple-streams-names\simple-streams-names.lib" \
	"\DYLAN\build\idvm-names\idvm-names.lib" \
	"\DYLAN\build\doss\doss.lib" \
	"\DYLAN\build\c-ffi\c-ffi.lib"

$(OUTDIR)/idvm-engine.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=.\wsock.rc
DEP_WSOCK=\
	.\wsock.ico\
	.\wsock.h

!IF  "$(CFG)" == "Win32 (80x86) Release"

$(INTDIR)/wsock.res :  $(SOURCE)  $(DEP_WSOCK) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 (80x86) Debug"

$(INTDIR)/wsock.res :  $(SOURCE)  $(DEP_WSOCK) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Debug"

$(INTDIR)/wsock.res :  $(SOURCE)  $(DEP_WSOCK) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Release"

$(INTDIR)/wsock.res :  $(SOURCE)  $(DEP_WSOCK) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\wsock.c
DEP_WSOCK_=\
	.\wsock.h

!IF  "$(CFG)" == "Win32 (80x86) Release"

$(INTDIR)/wsock.obj :  $(SOURCE)  $(DEP_WSOCK_) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 (80x86) Debug"

$(INTDIR)/wsock.obj :  $(SOURCE)  $(DEP_WSOCK_) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Debug"

$(INTDIR)/wsock.obj :  $(SOURCE)  $(DEP_WSOCK_) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Release"

$(INTDIR)/wsock.obj :  $(SOURCE)  $(DEP_WSOCK_) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\dialogs.c
DEP_DIALO=\
	.\wsock.h

!IF  "$(CFG)" == "Win32 (80x86) Release"

$(INTDIR)/dialogs.obj :  $(SOURCE)  $(DEP_DIALO) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 (80x86) Debug"

$(INTDIR)/dialogs.obj :  $(SOURCE)  $(DEP_DIALO) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Debug"

$(INTDIR)/dialogs.obj :  $(SOURCE)  $(DEP_DIALO) $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 (MIPS) Release"

$(INTDIR)/dialogs.obj :  $(SOURCE)  $(DEP_DIALO) $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\runtime.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\dylan-print\dylan-print.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\simple-streams\simple-streams.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\dylan\dylan.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\win32-interface\win32-interface.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\functional-extensions\functional-extensions.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\functional-dylan\functional-dylan.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\set\set.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\types\types.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\byte-vector\byte-vector.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\equal-table\equal-table.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\tcp-streams\streams.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\variable-search\variable-search.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\namespace\namespace.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\appl-names\appl-names.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\dylan-names\dylan-names.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\simple-streams-names\simple-streams-names.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\idvm-names\idvm-names.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\doss\doss.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\c-ffi\c-ffi.lib"
# End Source File
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\mmdw.lib"
# End Source File
################################################################################
################################################################################
# Begin Source File

SOURCE="\DYLAN\build\idvm\idvm.lib"
# End Source File
# End Group
# End Project
################################################################################