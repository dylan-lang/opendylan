#

APPVER = 4.0
TARGETOS = BOTH

!include <win32.mak>

MAIN        = basic-test.obj
CMAIN       = test.obj
RMAIN       = remote_nub_main.obj

# For newer versions of Win32.Mak, turn off generation of PDB files ...
!if defined(hcvars) && !defined(NODEBUG) && !defined(PROFILE) && !defined(TUNE)
cdebug      = -Z7 -Od -DDEBUG
!endif

!ifdef DEBUGGING_DEBUGGER
CFLAGS      =  /DDEBUGGING $(cflags) $(cdebug) $(PLATFORM_CFLAGS)
!else
CFLAGS      = $(cflags) $(cdebug) $(PLATFORM_CFLAGS)
!endif

NUB_INSTALL_AREA = $(FUNCTIONAL_DEVELOPER_USER_INSTALL)\lib
NUB_EXE_INSTALL_AREA = $(FUNCTIONAL_DEVELOPER_USER_INSTALL)\bin

# Common object files.
# These make up builds of both the local and remote debugger nubs.

COBJS       = app_control.obj debug_map.obj coff_map.obj debug_points.obj memory_access.obj register_access.obj misc_utils.obj page_protection.obj remote_function_call.obj remote_object_registration.obj remote_objects.obj source_location_info.obj stack_backtrace2.obj stop_reasons.obj symbol_lookup.obj dylan-extensions.obj profile.obj quick_and_dirty.obj address-print.obj coff_relocations.obj page_fault_count.obj process_walker.obj

# Platform RPC runtime libraries for Win32.

RPCLIBS     = rpcndr.lib rpcns4.lib rpcrt4.lib

# Remote object files
# Specific to builds of the remote debugger nub.

ROBJS       = $(COBJS) transport_protocols.obj server.obj synch_glue_remote.obj rap_stubs.obj rnub_s.obj

# Local object files
# Specific to builds of the local debugger nub.

ELOBJS      = $(COBJS) transport_protocols.obj server.obj server_locals.obj proxy.obj eproxy.obj nubserve_c.obj rnub_c.obj

LOBJS       = $(COBJS) server.obj server_locals.obj proxy.obj lproxy.obj

# Server object files
# Specific to builds of the standalone debugger connection server.

SOBJS        = server_locals.obj process_walker.obj server_remote.obj server_main.obj transport_protocols.obj nubserve_s.obj

CLIBS        = DBGHELP.LIB

RLIBS        = $(CLIBS) rdevnub.lib
LLIBS        = $(CLIBS) devnub.lib

rnub.h:           rnub.idl nub-core-types.h
  midl /nologo rnub.idl
nubserve.h:       nubserve.idl nub-core-types.h
  midl /nologo nubserve.idl

debug-server.res: debug-server.rc
accept-password.res: accept-password.rc

nub-core.h:       coff-extract.h cv-extract.h cv-types.h  nub_interface.h debug_map.h utils.h nub-core-types.h

rnub_c.c:                     rnub.h
rnub_s.c:                     rnub.h
nubserve_c.c:                 nubserve.h
nubserve_s.c:                 nubserve.h
server_starter.c:             transport_protocols.h nubserve.h accept-password.res
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
rserver.c:                    nub-core.h
source_location_info.c:       nub-core.h
stack_backtrace2.c:           nub-core.h
stop_reasons.c:               nub-core.h
symbol_lookup.c:              nub-core.h
proxy.c:                      nub-core.h
lproxy.c:                     nub-core.h
eproxy.c:                     nub-core.h transport_protocols.h rnub.h nubserve.h
dylan-extensions.c:           nub-core.h dylan-extensions.h
profile.c:                    nub-core.h
quick_and_dirty.c:            nub-core.h
address-print.c:              nub-core.h
basic-test.c:                 nub-core.h
coff_relocations.c:           nub-core.h
page_fault_count.c:           nub-core.h
dummy_page_fault_count.c:     nub-core.h
transport_protocols.c:        nub-core.h transport_protocols.h
server_locals.c:              nub-core.h
server_remote.c:              nub-core.h transport_protocols.h nubserve.h
server_main.c:                nub-core.h transport_protocols.h nubserve.h
remote_nub_main.c:            nub-core.h transport_protocols.h rnub.h
synch_glue_remote.c:          nub-core.h transport_protocols.h rnub.h
rap_stubs.c:                  nub-core.h
process_walker.c:             nub-core.h
server_stopper.c:             transport_protocols.h nubserve.h

devnub.lib:  $(LOBJS)
  $(implib) /nologo /out:$@ $(LOBJS)

edevnub.lib: $(ELOBJS)
  $(implib) /nologo /out:$@ $(ELOBJS) $(NUB_INSTALL_AREA)\mincrt.lib

rdevnub.lib: $(ROBJS)
  $(implib) /nologo /out:$@ $(ROBJS)

nubserve.exe: $(SOBJS) debug-server.res
  $(link) $(lflags) /debug:full $(FORCE) /subsystem:console $(guilibsmt) /out:$@ $(SOBJS) $(RPCLIBS) debug-server.res

rnub.exe: $(RMAIN) rdevnub.lib debug-server.res
  $(link) $(lflags) /debug:full $(FORCE) /subsystem:console $(guilibsmt) /out:$@ $(RMAIN) $(RLIBS) $(CLIBS) $(RPCLIBS) debug-server.res

start-dbg-server.exe: server_starter.obj nubserve_c.obj transport_protocols.obj debug-server.res accept-password.res
  $(link) $(lflags) /debug:full $(FORCE) /subsystem:windows $(guilibsmt) /out:$@ server_starter.obj nubserve_c.obj transport_protocols.obj $(RPCLIBS) accept-password.res

stop-dbg-server.exe: server_stopper.obj nubserve_c.obj transport_protocols.obj debug-server.res
  $(link) $(lflags) /debug:full $(FORCE) /subsystem:windows $(guilibsmt) /out:$@ server_stopper.obj nubserve_c.obj transport_protocols.obj $(RPCLIBS) debug-server.res

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
  pushd . & (del rnub.h nubserve.h) & popd
