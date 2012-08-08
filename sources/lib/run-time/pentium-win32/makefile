# Build runtime library for Dylan under Win32

APPVER	 = 4.0
TARGETOS = BOTH

OPTIMIZEDDEBUG = 1

!if defined(BuildArea)
INCLUDEDEST      = $(BuildArea)\include
LIBDEST          = $(BuildArea)\lib
BINDEST          = $(BuildArea)\bin
!else
INCLUDEDEST      = $(OPEN_DYLAN_USER_INSTALL)\include
LIBDEST          = $(OPEN_DYLAN_USER_INSTALL)\lib
BINDEST          = $(OPEN_DYLAN_USER_INSTALL)\bin
!endif

!include <win32.mak>

# !if !defined(NODEBUG) && !defined(PROFILE) && !defined(TUNE)
# cdebug = $(cdebug) /Zi /Fd$(INCLUDEDEST)\system.pdb
# !endif

!if defined(OPTIMIZEDDEBUG) && ($(OPTIMIZEDDEBUG) > 0) && !defined(NODEBUG) && !defined(PROFILE) && !defined(TUNE)
cdebug = /G5 /O2 /Ob2 /Oy- /Oi /Z7 /DNDEBUG
!endif

# For newer versions of Win32.Mak, turn off extraneous definition of _DEBUG
!if defined(hcvars)
cvarsmt = $(noansi) -D_MT -MT
!endif

!if defined(gen0freq)
mmtuneflags = /DTUNE_GEN0_FREQ=$(gen0freq)
!else
mmtuneflags = 
!endif

CC	 = $(cc)
LINKLIB	 = $(implib) /nologo /out:
CFLAGS	 = $(cflags) $(cvarsmt) $(cdebug) /I$(INCLUDEDEST) /I. /I.. /I$(SDK4MEMORY_POOL_SYSTEM)\code $(OPEN_DYLAN_C_FLAGS) $(mmtuneflags)
HEAPOBJS = heap-display.obj heap-utils.obj heap-trail.obj heap-order1.obj heap-order2.obj heap-table.obj
OBJS	 = collector.obj $(HEAPOBJS) runtime.obj win32-threads-primitives.obj spy-interfaces.obj
LIBFILE	 = pentium-run-time.lib
USEROBJ	 = dylan-support.obj
USERLIB	 = dylan-support.lib
MINCRT   = minvcrt.lib

RUNTIMELIBDEST	 = $(LIBDEST)\pentium-run-time.lib
MMDWDEST	 = $(LIBDEST)\mmdw.lib
PLINTHDEST	 = $(LIBDEST)\mpsplinth.lib
MINCRTDEST	 = $(LIBDEST)\minvcrt.lib
BOEHMGCDEST	 = $(BINDEST)\gc.dll

NOWEAKOBJS	 = no-weakness-collector.obj $(HEAPOBJS) runtime.obj win32-threads-primitives.obj spy-interfaces.obj
NOWEAKLIBFILE	 = no-weakness-pentium-run-time.lib

BOEHMOBJS	 = boehm-collector.obj $(HEAPOBJS) runtime.obj win32-threads-primitives.obj spy-interfaces.obj
BOEHMLIBFILE	 = boehm-pentium-run-time.lib

RELEASEMMDW     = $(SDK4MEMORY_POOL_SYSTEM)\code\w3i3mv\hi\mmdw.lib
RELEASEPLINTH	  = $(SDK4MEMORY_POOL_SYSTEM)\code\w3i3mv\hi\mpsplan.lib

DEBUGMMDW        = debug-mmdw.lib
DEBUGPLINTH	 = debug-mpsplan.lib

SYMBOLICMMDW     = symbolic-mmdw.lib
SYMBOLICPLINTH	 = mpsplan.lib

PLINTHOBJS       = plinth.obj mpsiowin32.obj
DYLANPLINTH      = dylan-plinth.lib

collector.obj: ..\collector.c
	$(CC) $(CFLAGS) /c ..\collector.c

no-weakness-collector.obj: ..\no-weakness-collector.c ..\collector.c
	$(CC) $(CFLAGS) /c ..\no-weakness-collector.c

boehm-collector.obj:	..\boehm-collector.c ..\collector.c
	$(CC) $(CFLAGS) /c ..\boehm-collector.c

!if defined(fullcrt)
$(MINCRT):
!else
$(MINCRT): minvcrt.def rtsyms.obj
	$(LINKLIB)$(MINCRT) /def:minvcrt.def /machine:ix86
	$(LINKLIB)$(MINCRT) $(MINCRT) rtsyms.obj
!endif

rtsyms.obj: rtsyms.s
	as -n -o rtsyms.obj rtsyms.s

$(DYLANPLINTH): $(PLINTHOBJS)
	$(LINKLIB)$(DYLANPLINTH) $(PLINTHOBJS)

$(LIBFILE): $(OBJS)
	$(LINKLIB)$(LIBFILE) $(OBJS)

$(NOWEAKLIBFILE): $(NOWEAKOBJS)
	$(LINKLIB)$(NOWEAKLIBFILE) $(NOWEAKOBJS)

$(BOEHMLIBFILE): $(BOEHMOBJS)
	$(LINKLIB)$(BOEHMLIBFILE) $(BOEHMOBJS) gc.lib

$(USERLIB): $(USEROBJ)
	$(LINKLIB)$(USERLIB) $(USEROBJ)

ensure-dirs:
	if not exist $(LIBDEST) mkdir $(LIBDEST)
	if not exist $(BINDEST) mkdir $(BINDEST)

install-build:
	echo Building pentium run time: install-build
	xcopy /i/q ..\..\..\jamfiles\*.jam $(LIBDEST)
	if not exist $(LIBDEST)\x86-win32-build.jam copy $(LIBDEST)\x86-win32-vc6-build.jam $(LIBDEST)\x86-win32-build.jam
	if not exist $(LIBDEST)\dylan-support.obj copy dylan-support.obj $(LIBDEST)
	if not exist $(LIBDEST)\dylanpe.script copy dylanpe.script $(LIBDEST)
	if not exist $(BINDEST)\make-reloc.exe copy make-reloc.exe $(BINDEST)

install-userlib: $(USERLIB)
  echo Building pentium run time: install-userlib
	pushd $(LIBDEST) & (del /q /f $(USERLIB)) & popd
	copy $(USERLIB) $(LIBDEST)


install-runtime: $(LIBFILE)
  echo Building pentium run time: install-runtime
	pushd $(LIBDEST) & (del /q /f $(LIBFILE)) & popd
	copy $(LIBFILE) $(LIBDEST)

install-noweak-runtime: $(NOWEAKLIBFILE)
  echo Building pentium run time: install-noweak-runtime
	if exist $(RUNTIMELIBDEST) del /q /f $(RUNTIMELIBDEST)
	copy $(NOWEAKLIBFILE) $(RUNTIMELIBDEST)

install-boehm-runtime: $(BOEHMLIBFILE)
  echo Building pentium run time: install-boehm-runtime
	if exist $(RUNTIMELIBDEST) del /q /f $(RUNTIMELIBDEST)
	copy $(BOEHMLIBFILE) $(RUNTIMELIBDEST)
	copy gc.dll $(BOEHMGCDEST)

install-dylan-plinth: $(DYLANPLINTH)
  echo Building pentium run time: install-dylan-plinth
	if exist $(PLINTHDEST) del /q /f $(PLINTHDEST)
	copy $(DYLANPLINTH) $(PLINTHDEST)

install-ansi-plinth: $(SYMBOLICPLINTH)
	echo Building pentium run time: install-ansi-plinth
	if exist $(PLINTHDEST) del /q /f $(PLINTHDEST)
	copy $(SYMBOLICPLINTH) $(PLINTHDEST)

install-debug-ansi-plinth: $(DEBUGPLINTH)
  echo Building pentium run time: install-debug-ansi-plinth
	if exist $(PLINTHDEST) del /q /f $(PLINTHDEST)
	copy $(DEBUGPLINTH) $(PLINTHDEST)

install-plinth: install-dylan-plinth
  echo Building pentium run time: install-plinth


!if defined(fullcrt)
install-mincrt:
   echo Building pentium run time: install-mincrt (fullcrt)
!else
install-mincrt: $(MINCRT)
  echo Building pentium run time: install-mincrt
	if exist $(MINCRTDEST) del /q /f $(MINCRTDEST)
	copy $(MINCRT) $(MINCRTDEST)
!endif

install-mmdw:
  echo Building pentium run time: install-mmdw
	if exist $(MMDWDEST) del /q /f $(MMDWDEST)
	if not exist $(RELEASEMMDW) pushd $(SDK4MEMORY_POOL_SYSTEM)\code \
	& $(MAKE) /k /f w3i3mv.nmk mmdw.lib > $(BUILD_LOGS)\make-mmdw.log \
	& popd
  copy $(RELEASEMMDW) $(MMDWDEST)

install-symbolic-mmdw: $(SYMBOLICMMDW)
  echo Building pentium run time: install-symbolic-mmdw
	if exist $(MMDWDEST) del /q /f $(MMDWDEST)
	copy $(SYMBOLICMMDW) $(MMDWDEST)

install-debug-mmdw: $(DEBUGMMDW)
  echo Building pentium run time: install-debug-mmdw
	if exist $(MMDWDEST) del /q /f $(MMDWDEST)
	copy $(DEBUGMMDW) $(MMDWDEST)


install-common-libs: ensure-dirs install-plinth install-userlib install-mincrt


install-stripped: ensure-dirs install-mmdw install-runtime install-common-libs

install-debug: ensure-dirs install-debug-mmdw install-runtime install-common-libs

install-symbolic: ensure-dirs install-symbolic-mmdw install-runtime install-common-libs

install-noweak: ensure-dirs install-mmdw install-noweak-runtime install-common-libs

install-noweak-debug: ensure-dirs install-debug-mmdw install-noweak-runtime install-common-libs

install-noweak-symbolic: ensure-dirs install-symbolic-mmdw install-noweak-runtime install-common-libs

install-boehm: ensure-dirs install-mmdw install-boehm-runtime install-common-libs

install-boehm-debug: ensure-dirs install-debug-mmdw install-boehm-runtime install-common-libs

install-boehm-symbolic: ensure-dirs install-symbolic-mmdw install-boehm-runtime install-common-libs


install: ensure-dirs install-stripped install-build

# Only delete the products that should be built by this makefile.
# (The files runtime.obj & dylan-support.obj are checked out from HOPE)
clean:
	pushd . & (del /f /q *collector.obj $(HEAPOBJS) win32-threads-primitives.obj spy-interfaces.obj) & popd
        pushd . & (del /f /q *pentium-run-time.lib $(USERLIB)) & popd
        pushd . & (del /f /q $(MINCRT) rtsyms.obj) & popd
        pushd . & (del /f /q $(DYLANPLINTH) $(PLINTHOBJS)) & popd

