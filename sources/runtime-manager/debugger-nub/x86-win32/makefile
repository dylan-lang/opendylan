#

APPVER = 4.0
TARGETOS = BOTH

!include <win32.mak>

MAIN        = basic-test.obj
CMAIN       = test.obj

# For newer versions of Win32.Mak, turn off generation of PDB files ...
!if defined(hcvars) && !defined(NODEBUG) && !defined(PROFILE) && !defined(TUNE)
cdebug      = -Z7 -Od -DDEBUG
!endif

!ifdef DEBUGGING_DEBUGGER
CFLAGS      =  /DDEBUGGING $(cflags) $(cdebug) $(PLATFORM_CFLAGS)
!else
CFLAGS      = $(cflags) $(cdebug) $(PLATFORM_CFLAGS)
!endif

NUB_INSTALL_AREA = $(OPEN_DYLAN_USER_INSTALL)\lib
NUB_EXE_INSTALL_AREA = $(OPEN_DYLAN_USER_INSTALL)\bin

# Common object files.
# These make up builds of both the local and remote debugger nubs.

COBJS       = app_control.obj debug_map.obj coff_map.obj debug_points.obj memory_access.obj register_access.obj misc_utils.obj page_protection.obj remote_function_call.obj remote_object_registration.obj remote_objects.obj source_location_info.obj stack_backtrace2.obj stop_reasons.obj symbol_lookup.obj dylan-extensions.obj profile.obj quick_and_dirty.obj address-print.obj coff_relocations.obj page_fault_count.obj process_walker.obj

# Local object files
# Specific to builds of the local debugger nub.

LOBJS       = $(COBJS) server.obj server_locals.obj proxy.obj lproxy.obj

CLIBS        = DBGHELP.LIB
LLIBS        = $(CLIBS) devnub.lib

nub-core.h:       coff-extract.h cv-extract.h cv-types.h  nub_interface.h debug_map.h utils.h nub-core-types.h

app_control.c:                nub-core.h
debug_map.c:                  nub-core.h
coff_map.c:                   nub-core.h
debug_points.c:               nub-core.h
describe.c:                   nub-core.h
memory_access.c:              nub-core.h
register_access.c:            nub-core.h
misc_utils.c:                 nub-core.h
page_protection.c:            nub-core.h
remote_function_call.c:       nub-core.h
remote_object_registration.c: nub-core.h
remote_objects.c:             nub-core.h
server.c:                     nub-core.h
source_location_info.c:       nub-core.h
stack_backtrace2.c:           nub-core.h
stop_reasons.c:               nub-core.h
symbol_lookup.c:              nub-core.h
proxy.c:                      nub-core.h
lproxy.c:                     nub-core.h
dylan-extensions.c:           nub-core.h dylan-extensions.h
profile.c:                    nub-core.h
quick_and_dirty.c:            nub-core.h
address-print.c:              nub-core.h
basic-test.c:                 nub-core.h
coff_relocations.c:           nub-core.h
page_fault_count.c:           nub-core.h
dummy_page_fault_count.c:     nub-core.h
process_walker.c:             nub-core.h

devnub.lib:  $(LOBJS)
  $(implib) /nologo /out:$@ $(LOBJS)

test.exe: $(CMAIN) devnub.lib clienttest.lib
  $(link) $(lflags) /debug:full $(FORCE) /subsystem:console $(guilibsmt) /out:$@ $(CMAIN) $(LLIBS) clienttest.lib

basic-test.exe: $(MAIN) devnub.lib
  $(link) $(lflags) /debug:full $(FORCE) /subsystem:console $(guilibsmt) /out:$@ $(MAIN) $(LLIBS)

install:  devnub.lib
  if exist $(NUB_INSTALL_AREA)\devnub.lib del /q /f $(NUB_INSTALL_AREA)\devnub.lib
  copy devnub.lib $(NUB_INSTALL_AREA)\devnub.lib

clean:
  pushd . & (del *.obj *.res *.exe) & popd

clean-all:
  pushd . & (del *.obj *.res *.exe) & popd
