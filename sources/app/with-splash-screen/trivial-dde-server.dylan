Module:    with-splash-screen
Synopsis:  Launch another app, providing a splash screen until it's ready.
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Name for DDE Service and Topic.
define constant $service-and-topic-name :: <byte-string> = "FunctionalDeveloper";

/// Default flags for DdeInitialize.
define constant $allow-executes :: <integer> =
  logior
    ($CBF-FAIL-ADVISES,
     $CBF-FAIL-POKES,
     $CBF-FAIL-REQUESTS,
     $CBF-SKIP-ALLNOTIFICATIONS);

/// This value gets used a lot.
define constant $machine-word-zero :: <machine-word>
  = as(<machine-word>, 0);

define variable *dde-server-id* :: <machine-word> = $machine-word-zero;
define variable *dde-server-service-handle* :: <HSZ> = $NULL-HSZ;

define function initialize-dde-server ()
  let result :: <integer> = 0;
  with-stack-structure (pidInst :: <LPDWORD>)
    // If the server's already been initialized, this will reinitialize it.
    pidInst.pointer-value := *dde-server-id*;
    result := DdeInitialize(pidInst, transaction-callback, $allow-executes, 0);
    *dde-server-id* := as(<machine-word>, pidInst.pointer-value)
  end with-stack-structure;
  when (result = $DMLERR-NO-ERROR)
    *dde-server-service-handle*
      := DdeCreateStringHandle(*dde-server-id*, $service-and-topic-name, $CP-WINANSI);
    unless (null-handle?(*dde-server-service-handle*))
      DdeNameService(*dde-server-id*, *dde-server-service-handle*, $NULL-HSZ, $DNS-REGISTER)
    end
  end
end function initialize-dde-server;

define function destroy-dde-server () => ()
  if (*dde-server-id* ~= $machine-word-zero)
    DdeNameService(*dde-server-id*, $NULL-HSZ, $NULL-HSZ, $DNS-UNREGISTER);
    DdeFreeStringHandle(*dde-server-id*, *dde-server-service-handle*);
    DdeUninitialize(*dde-server-id*);
  end;
  *dde-server-service-handle* := $NULL-HSZ;
  *dde-server-id* := $machine-word-zero;
end function destroy-dde-server;

define callback transaction-callback :: <PFNCALLBACK> = broadcast-dde-transaction;

define function broadcast-dde-transaction
    (uType :: <integer>, uFmt :: <integer>,
     hconv :: <HCONV>, hsz1 :: <HSZ>, hsz2 :: <HSZ>, hdata :: <HDDEDATA>,
     dwData1 :: <ffi-integer>, dwData2 :: <ffi-integer>)
 => (hdde-result :: <HDDEDATA>)
  // Not safe to signal across C-FFI boundary, so catch any condition.
  // (We could store it in a module variable for later inspection, if needed.)
  let dwData1 :: <machine-word> = as(<machine-word>, dwData1);
  let dwData2 :: <machine-word> = as(<machine-word>, dwData2);
  ignore(dwData2);
  let result :: <integer> = 0;
  block ()
    let service-or-topic-handle :: <HSZ> = *dde-server-service-handle*;
    result
      := select (uType)
	   $XTYP-CONNECT =>
	     // Allow connection, provided the topic (hsz1) and 
	     // service (hsz2) names are what we expect ...
	     if (DdeCmpStringHandles(hsz1, service-or-topic-handle) = 0
		   & DdeCmpStringHandles(hsz2, service-or-topic-handle) = 0)
	       1		// TRUE
	     else
	       0		// FALSE
	     end;
	   $XTYP-WILDCONNECT =>
	     // Fail wild connections, since we don't have multiple topics.
	     0;			// FALSE
	   $XTYP-EXECUTE =>
	     // "Execute" provided the topic name (hsz1) is as expected.
	     if (DdeCmpStringHandles(hsz1, service-or-topic-handle) = 0)
	       $DDE-FACK
	     else
	       $DDE-FNOTPROCESSED
	     end;
	   $XTYP-ERROR =>
	     0;
	   otherwise =>
	     0;
	 end;
    unless (null-pointer?(hdata))
      DdeUnaccessData(hdata);
      DdeFreeDataHandle(hdata);
    end;
  exception (<condition>)
    // Do nothing.  We must currently ensure no conditions are signalled
    // across the C-FFI boundary, or things will go badly wrong.
  end;
  make(<HDDEDATA>, address: result)
end function broadcast-dde-transaction;
