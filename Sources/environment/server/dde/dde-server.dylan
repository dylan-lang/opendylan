Module:    environment-dde-server
Author:    Jason Trenouth, Hugh Greene
Synopsis:  Controlling the Environment from external sources, via DDE.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// -=- ISSUES -=-
//
// See dde-server.txt.
//
// One IMPORTANT issue: the thread that starts this server (by calling
// "server-start" in the environment-server library-&-module) MUST be
// one which handles a Windows event loop (Get-, Translate- and
// DispatchMessage), or the DDE calls will fail.



/// -=- DDE SERVER CLASS -=-

// The following string represents the service and topic name which
// clients should use to connect to the environment's DDE server
// and when sending $XTYP-EXECUTE transactions to it.
//
// Note that the strings used in DDE transactions may be at most 255
// characters long (plus 1 for the NUL terminator).
//
// This is exported to the environment, so it may change at any time,
// even within one DDE conversation (though it should only be changed
// at the user's request).  So if the client is "out of date", it may
// find that its $XTYP-EXECUTE transactions fail at some point.

// Name for DDE Service and Topic.
define atomic variable *service-and-topic-name* :: <byte-string>
  = "FunctionalDeveloper";

// Default flags for DdeInitialize.
define constant $allow-executes :: <integer> =
  logior
    ($CBF-FAIL-ADVISES,
     $CBF-FAIL-POKES,
     $CBF-FAIL-REQUESTS,
     $CBF-SKIP-ALLNOTIFICATIONS);

// This value gets used a lot.
define constant $machine-word-zero :: <machine-word>
  = as(<machine-word>, 0);


// The generics for the dde-server-id slot have to be defined before the
// class, or the compiler will create incompatible implicit generics,
// because <machine-word>s aren't "proper" <object>s in some weird way.

define generic dde-server-id
    (server :: <environment-dde-server>)
 => (id :: <machine-word>);

define generic dde-server-id-setter
    (id :: <machine-word>, server :: <environment-dde-server>)
 => (id :: <machine-word>);


// The class definition proper.
define class <environment-dde-server> (<simple-lock>)
  atomic slot dde-server-id :: <machine-word> = $machine-word-zero;
  constant slot dde-server-channel :: <channel> = make(<channel>);
  constant slot dde-server-filter-flags :: <integer> = $allow-executes,
    init-keyword: filter-flags:;
  /* virtual atomic slot dde-server-service :: <byte-string>; */
  slot dde-server-service-handle :: <HSZ> = $NULL-HSZ;
end class;

// A virtual slot, so that the environment can change our service name
// from outside, at any time.
define generic dde-server-service
    (server :: <environment-dde-server>) => (service :: <byte-string>);

define method dde-server-service
    (server :: <environment-dde-server>) => (service :: <byte-string>)
  *service-and-topic-name*
end method;

/*---*** andrewa: this isn't currently being used
//---*** But what about the DDE-SERVER-SERVICE-HANDLE?  Maybe we have
//---*** to shutdown the server and restart it?  Or at least unregister
//---*** and then reregister the service.
define method dde-server-service-setter
    (service :: <byte-string>, server :: <environment-dde-server>)
 => (service :: <byte-string>)
  *service-and-topic-name* := service
end method;
*/


// -=- Make and Initialization -=-

// Force there to be only one server object, since we can currently only
// have one C callback (a C-FFI limitation), and each server needs a
// separate callback.  Not that we expect to make more than one anyway.

define constant $no-server :: <lock> = make(<lock>);

define atomic variable *environment-dde-server*
    :: type-union(<environment-dde-server>, singleton($no-server))
  = $no-server;

define method make
    (server-class == <environment-dde-server>,
     #rest args, #key)
 => (server :: <environment-dde-server>)
  with-lock (*environment-dde-server*)
    if (*environment-dde-server* == $no-server)
      *environment-dde-server* := next-method();
    end if;
  end with-lock;
end method;

define method initialize
    (server :: <environment-dde-server>,
     #rest args, #key)
  next-method();
  initialize-dde-server(server);
end method;

define function initialize-dde-server (server :: <environment-dde-server>)
  with-lock (server)
    let result :: <integer> = 0;
    with-stack-structure(pidInst :: <LPDWORD>)
      // If the server's already been initialized, this will
      // reinitialize it.
      pidInst.pointer-value := server.dde-server-id;
      result
        := DdeInitialize
             (pidInst, transaction-callback,
              server.dde-server-filter-flags, 0);
      server.dde-server-id := as(<machine-word>, pidInst.pointer-value);
      debug-message("  ... DDE server ID is %=", server.dde-server-id);
    end with-stack-structure;
    // Don't chance signalling errors until C memory is deallocated!
    dde-report-error("DdeIniitialize", result);
    debug-message("  ... DDE server initialised.");
    server.dde-server-service-handle
      := DdeCreateStringHandle
           (server.dde-server-id, server.dde-server-service, $CP-WINANSI);
    dde-check-result("DdeCreateStringHandle",
		     server, server.dde-server-service-handle);
    let result :: <HDDEDATA>
      = DdeNameService
          (server.dde-server-id, server.dde-server-service-handle,
           $NULL-HSZ, $DNS-REGISTER);
    dde-check-result("DdeNameService (register)", server, result);
    debug-message("  ... DDE service registered.");
  end with-lock;
end function;


// -=- "Desctructor" -=-

define function destroy-dde-server () => ()
  with-lock (*environment-dde-server*)
    if ( *environment-dde-server* ~= $no-server )
      do-destroy-dde-server(*environment-dde-server*);
      *environment-dde-server* := $no-server;
    else
      // This is one of those "should never happen" errors.
      dde-warning
        (format-string: "Attempt to destroy DDE server when none exists.");
      // This may return.
    end if;
  end with-lock;
end function;

define function do-destroy-dde-server
    (server :: <environment-dde-server>)
 => ()
  // Don't need to lock the server here, as it's been locked within
  // destroy-dde-server.
  if (server.dde-server-id ~= $machine-word-zero)
    let result :: <HDDEDATA>
      = DdeNameService(server.dde-server-id, $NULL-HSZ, $NULL-HSZ, $DNS-UNREGISTER);
    dde-check-result("DdeNameService (unregister)", server, result);
    let result :: <boolean>
      = DdeFreeStringHandle
          (server.dde-server-id, server.dde-server-service-handle);
    dde-check-result("DdeFreeStringHandle", server, result);
    server.dde-server-service-handle := $NULL-HSZ;
    let result :: <boolean>
      = DdeUninitialize(server.dde-server-id);
    dde-check-result("DdeUninitialize", server, result);
  end if;
  server.dde-server-id := $machine-word-zero;
end function;



/// -=- CALLBACK HANDLING -=-

// NOTE:
//
// There must be a Windows Get-/Translate-/DispatchMessage event loop
// in applications using this server, or this callback will never be
// called and the server will do nothing.

define callback transaction-callback :: <PFNCALLBACK>
  = broadcast-dde-transaction;

// ---*** BIG QUESTION: is it safe to "signal" inside a callback?
// ---*** ANSWER: No, so catch all conditions and ignore them.
// ---*** (We could store them in a module variable and check later.)
define function broadcast-dde-transaction
    (uType :: <integer>, uFmt :: <integer>,
     hconv :: <HCONV>, hsz1 :: <HSZ>, hsz2 :: <HSZ>, hdata :: <HDDEDATA>,
     dwData1 :: <ffi-integer>, // i.e., type-union(<integer>, <machine-word>)
     dwData2 :: <ffi-integer>)
 => (hdde-result :: <HDDEDATA>)
  // Not safe to signal across C-FFI boundary, so catch any condition.
  // (We could store it in a module variable for later inspection, if needed.)
  let dwData1 :: <machine-word> = as(<machine-word>, dwData1);
  let dwData2 :: <machine-word> = as(<machine-word>, dwData2);
  ignore(dwData2);
  let result :: <integer> = 0;
  block ()
    // Lock the server, so that it can't be destroyed under our feet.
    debug-message("DDE callback (uType = %=) {", uType);
    with-lock (*environment-dde-server*)
      let service-or-topic-handle :: <HSZ>
	= *environment-dde-server*.dde-server-service-handle;
      result
	:= select (uType) // by compose(\~, zero?, logand))
	     $XTYP-CONNECT => // = #x1062 = 4194
	       debug-message("  XTYP-CONNECT");
	       // Allow connection, provided the topic (hsz1) and service (hsz2)
	       // names are what we expect.
	       if ( DdeCmpStringHandles(hsz1, service-or-topic-handle) = 0
		  & DdeCmpStringHandles(hsz2, service-or-topic-handle) = 0 )
		 1; // TRUE
	       else
		 0; // FALSE
	       end if;
	     $XTYP-WILDCONNECT => // = #x20e2 = 8418
	       debug-message("  XTYP-WILDCONNECT");
	       // Fail wild connections, since we don't have multiple topics.
	       0; // i.e., NULL
	     $XTYP-EXECUTE => // = #x4050 = 16464
	       debug-message("  XTYP-EXECUTE");
	       // Execute, provided the topic name (hsz1) is as expected.
	       if ( DdeCmpStringHandles(hsz1, service-or-topic-handle) = 0 )
		 broadcast-dde-execute-transaction
		   (*environment-dde-server*, hdata); // Returns flags.
	       else
		 $DDE-FNOTPROCESSED;
	       end if;
	     $XTYP-ERROR => // = #x8002 = 32770
	       debug-message("  XTYP-ERROR");
	       dde-report-xtyp-error(dwData1);
	       // The above may return, if some "Ignore" handler is established.
	       0;
	     otherwise =>
	       dde-warning
		 (format-string: "Unexpected DDE transaction type.  (%d)",
		  format-arguments: uType);
	       // This may return.
	       0;
	   end select;
      debug-message("}, result = %=", result);
      unless (null-pointer?(hdata))
	let result :: <boolean> = DdeFreeDataHandle(hdata);
	dde-check-result("DdeFreeDataHandle",
			 *environment-dde-server*, result);
      end unless;
    end with-lock;
  exception (<condition>)
    // Do nothing.  We must currently ensure no conditions are signalled
    // across the C-FFI boundary, or things will go badly wrong.
  end;
  make(<HDDEDATA>, address: result)
end function;


define function broadcast-dde-execute-transaction
    (server :: <environment-dde-server>, hdata :: <HDDEDATA>)
 => (flags :: <integer>)
  // Don't need to lock the server here, as it's been locked within
  // broadcast-dde-transaction.
  if (server.dde-server-id ~= $machine-word-zero)
    // Access the DDE data object.
    // ---*** Strictly, command-size should be a <machine-word>, but
    // nobody is likely to send us a string so big that it matters!
    let (command-buffer :: <LPBYTE>, command-size :: <integer>)
      = DdeAccessData(hdata);
    dde-check-result("DdeAccessData", server, command-buffer);
    unless (command-size <= 0) // It could happen ...
      // Copy the contents to a <string>.
      let command-buffer :: <C-char*>
	= pointer-cast(<C-char*>, command-buffer);
      // The command-buffer contains the terminating ASCII NUL (if
      // there is one -- this is not guaranteed), which we don't want;
      // so we make a shorter <string> by copying up to the first NUL
      // or to command-size, whichever comes first.
      // NOTE: We can't just use "as(<byte-string>, command-buffer)",
      // because command-buffer is not guaranteed to be NUL-terminated.
      // We can't force command-buffer to be NUL-terminated and then use
      // "as", because it must be treated as read-only.
      //---*** What about Unicode? 
      let string-size =
	for (i from 0 below command-size,
	     until: as(<byte-character>,
		       pointer-value(command-buffer, index: i)) = '\0')
	  // Move along till we hit a NUL or the end of the buffer.
	finally i
        end for;
      let command-string :: <byte-string>
	= make(<byte-string>, size: string-size);
      for (i from 0 below string-size)
	command-string[i]
	  := as(<byte-character>,
		pointer-value(command-buffer, index: i));
      end for;
      // Send the command string.
      broadcast(server.dde-server-channel, command-string);
    end unless;

    // Release the DDE data object.
    let result :: <boolean> = DdeUnaccessData(hdata);
    dde-check-result("DdeUnaccessData", server, result);

    // Return "Acknowledge" flag.
    $DDE-FACK
  else
    $DDE-FNOTPROCESSED
  end if;
end function;



/// -=- ERROR HANDLING -=-

// NOTE:
//
// Most DDE errors indicate only that a single transaction has failed,
// (and other generally indicate catastrophic system resource
// failures) so modules using this one may provide handlers allowing
// any <dde-serious-condition> to be ignored.

// WARNING:
//
// Because some of the DDE message handling is done within a Windows
// callback, a <dde-serious-condition> may be signalled while in a Windows
// Get-/Translate-/DispatchMessage event loop, as well as when
// functions in this library are called explicitly.  A
// <parse-condition> or <command-call-condition> may be signalled
// similarly. 

define class <dde-condition> (<condition>)
end class;

define class <dde-warning> (<simple-warning>, <dde-condition>)
end class;

define class <dde-serious-condition>
    (<simple-condition>, <serious-condition>, <dde-condition>)
end class;

define function dde-warning (#rest args)
// NOTE: Unspecified return value.
  signal(apply(make, <dde-warning>, args));
end function;


define sealed generic dde-check-result
    (context :: <string>, server :: <environment-dde-server>,
     result :: <object>) => ();

define method dde-check-result
    (context :: <string>, server :: <environment-dde-server>,
     result :: <object>) => ()
  ignore(context, server, result);
  // debug-message("[checking result %=]", result);
end method;

define method dde-check-result
    (context :: <string>, server :: <environment-dde-server>,
     result :: <C-pointer>) => ()
  next-method();
  dde-check-result(context, server, pointer-address(result));
end method;

define method dde-check-result
    (context :: <string>, server :: <environment-dde-server>, result :: <integer>) => ()
  next-method();
  if (result = 0) dde-report-error(context, server) end;
end method;

define method dde-check-result
    (context :: <string>, server :: <environment-dde-server>, result :: <machine-word>) => ()
  next-method();
  if (result = $machine-word-zero) dde-report-error(context, server) end;
end method;

define method dde-check-result
    (context :: <string>, server :: <environment-dde-server>, result :: <boolean>) => ()
  next-method();
  if (result = #f) dde-report-error(context, server) end;
end method;


define method dde-report-error
    (context :: <string>, server :: <environment-dde-server>) => ()
  dde-report-error(context, DdeGetLastError(server.dde-server-id));
end method;

define method dde-report-error
    (context :: <string>, error-code :: <integer>) => ()
  let message :: false-or(<string>) = dde-error-message(error-code);
  if (message)
    error(make(<dde-serious-condition>,
               format-string: concatenate(context, ": ", message, ".  (%d)"),
               format-arguments: vector(error-code)));
    // This must not return, or DDE could get confused.
  end if;
end method;


// ---*** Can we get FormatMessage to do this for us?
// Produce a <string> description of a DDE error code (or #f for
// $DMLERR-NO-ERROR), as obtained from DdeGetLastError when some DDE
// command fails.  (See below for error reporting in another context.)
define function dde-error-message
    (error-code :: <integer>)
 => (message :: false-or(<string>))
  select (error-code)
    $DMLERR-NO-ERROR => #f;
    $DMLERR-ADVACKTIMEOUT => "A request for a synchronous advise transaction has timed out";
    $DMLERR-BUSY => "The response to the transaction caused the DDE-FBUSY flag to be set";
    $DMLERR-DATAACKTIMEOUT => "A request for a synchronous data transaction has timed out";
    $DMLERR-DLL-NOT-INITIALIZED => "A DDEML function was called without first calling the DdeInitialize function, or an invalid instance identifier was passed to a DDEML function";
    $DMLERR-DLL-USAGE => "An application initialized as APPCLASS_MONITOR has attempted to perform a dynamic data exchange (DDE) transaction, or an application initialized as APPCMD_CLIENTONLY has attempted to perform server transactions";
    $DMLERR-EXECACKTIMEOUT => "A request for a synchronous execute transaction has timed out";
    $DMLERR-INVALIDPARAMETER => "A parameter failed to be validated by the DDEML.  See DDEML reference pages for details";
    $DMLERR-LOW-MEMORY => "A DDEML application has created a prolonged race condition (in which the server application outruns the client), causing large amounts of memory to be consumed";
    $DMLERR-MEMORY-ERROR => "A memory allocation has failed";
    $DMLERR-NO-CONV-ESTABLISHED => "A client's attempt to establish a conversation has failed";
    $DMLERR-NOTPROCESSED => "A transaction has failed";
    $DMLERR-POKEACKTIMEOUT => "A request for a synchronous poke transaction has timed out";
    $DMLERR-POSTMSG-FAILED => "An internal call to the PostMessage function has failed";
    $DMLERR-REENTRANCY => "An application instance with a synchronous transaction already in progress attempted to initiate another synchronous transaction, or the DdeEnableCallback function was called from within a DDEML callback function";
    $DMLERR-SERVER-DIED => "A server-side transaction was attempted on a conversation terminated by the client, or the server terminated before completing a transaction";
    $DMLERR-SYS-ERROR => "An internal error has occurred in the DDEML";
    $DMLERR-UNADVACKTIMEOUT => "A request to end an advise transaction has timed out";
    $DMLERR-UNFOUND-QUEUE-ID => "An invalid transaction identifier was passed to a DDEML function.  Once the application has returned from an XTYP_XACT_COMPLETE callback, the transaction identifier for that callback function  is no longer valid";
    otherwise => "Unknown DDE error code";
  end select;
end function;


// Signal an <error> with an appropriate descriptive <string>, from a DDE
// error code, as extracted from the dwData1 parameter of an $XTYP-ERROR
// message.  Codes here have different meanings to when they are retrieved
// from DdeGetLastError.
define function dde-report-xtyp-error (error-code :: <machine-word>) => ()
  let message :: <string>
    = select (error-code by \=)
        $DMLERR-LOW-MEMORY => "Memory is low; advise, poke, or execute data may be lost, or the system may fail";
        otherwise => "Unknown DDE error code";
      end select;
  dde-warning
    (format-string: concatenate(message, ".  (%d)"),
     format-arguments: vector(error-code));
  // May return.
end function;



/// -=- DDE SERVER START & STOP -=-

// This is used by the environment to identify the server (which
// is a client of the environment, hence the name).
define constant $client-id = #"DDE Server";

define function server-start () => ()
  debug-message("  Starting DDE server {");
  *environment-dde-server* := make(<environment-dde-server>);
  debug-message("  initialising command-parser callback ...");
  tune-in(*environment-dde-server*.dde-server-channel,
          rcurry(parse-then-call, $client-id));
  debug-message("  }");
end function;

define function server-stop () => ()
  destroy-dde-server();
end function;
