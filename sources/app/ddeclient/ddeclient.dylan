Module:    ddeclient
Synopsis:  A simple program to send a single synchronous DDE client request.
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// This program is loosely based on the PMDDEML sample application from
// Microsoft.


// FOR THE FUTURE:
// Configurable timeout.
// Return values from the call (e.g., a string to dump to *standard-output*).
// Asynchronous calls?

/****************************************************************************

  Program: ddeclient.dylan

  Functions:

    initialize-dde
    finalize-dde
    check-dde-result
    report-dde-error
    do-dde-callback
    send-dde-command
    main

  Description:

    Sample Windows application that sends XTYP_EXECUTE messages under
    any Topic in any DDE Service, using DDEML.

****************************************************************************/

define constant $machine-word-zero :: <machine-word> = as(<machine-word>, 0);

// -=- Initialization and cleanup -=-

define method initialize-dde
    ()
 => (idInst :: <machine-word>)
  let idInst :: <machine-word> = $machine-word-zero;

  // Register this client with the DDEML.
  with-stack-structure( pidInst :: <LPDWORD> )
    pidInst.pointer-value := 0;
    report-dde-error
      (DdeInitialize
        (pidInst,             // receives instance ID
         dde-callback,        // address of callback function
         $APPCMD-CLIENTONLY,  // this is a client app
         0));                 // reserved
    idInst := as(<machine-word>, pointer-value(pidInst));
  end with-stack-structure;
  idInst
end method;

define method finalize-dde (idInst :: <machine-word>) => ()
  // Free all DDEML resources associated with this application.
  // ---*** Should we really call check-dde-result here?
  check-dde-result(idInst, DdeUninitialize(idInst));
end method;


/****************************************************************************

check-dde-result						[Function]

  Summary: Checks the return code from DDEML codes, displaying an error
           message and exiting the application if appropriate.

  Signature: CHECK-DDE-RESULT _idInst_ _result_ => no return value

  Arguments:
    idInst  :: <machine-word>
    result  :: <object>

    Methods are defined for result as a <C-pointer>, <integer>,
    <machine-word> or <boolean>.

  Values:
    None.

****************************************************************************/

define sealed generic check-dde-result
    (idInst :: <machine-word>, result :: <object>) => ();

define method check-dde-result
    (idInst :: <machine-word>, result :: <C-pointer>) => ()
  check-dde-result(idInst, pointer-address(result));
end method;

define method check-dde-result
    (idInst :: <machine-word>, result :: <integer>) => ()
  if (result = 0) report-dde-error(idInst) end;
end method;

define method check-dde-result
    (idInst :: <machine-word>, result :: <machine-word>) => ()
  if (result = $machine-word-zero) report-dde-error(idInst) end;
end method;

define method check-dde-result
    (idInst :: <machine-word>, result :: <boolean>) => ()
  if (result = #f) report-dde-error(idInst) end;
end method;


define sealed generic report-dde-error
    (source-or-code :: type-union(<machine-word>, <integer>)) => ();

define method report-dde-error (idInst :: <machine-word>) => ()
  report-dde-error(DdeGetLastError(idInst));
end method;

define method report-dde-error (error-code :: <integer>) => ()
  unless (error-code = $DMLERR-NO-ERROR)
    format-out("DDEML error, code %d\n", error-code);
    exit-application(1); // "$EXIT-FAILURE"
  end if;
end method;


/****************************************************************************

do-dde-callback							[Function]

  Summary: Processes messages for DDEML conversation.

  Arguments:
    wType   :: <integer>
    wFmt    :: <integer>
    hConv   :: <HCONV>
    hsz1    :: <HSZ>
    hsz2    :: <HSZ>
    hData   :: <HDDEDATA>
    dwData1 :: <integer>
    dwData2 :: <integer>

  Values:
    result  :: <HDDEDATA>

  Note:

  This callback will never actually be called, since this program has
  no Windows event loop.  However, the only message we'd expect to
  receive is one which indicates a critical error, in which case the
  user will have to take some drastic action anyway.

****************************************************************************/

define callback dde-callback :: <PFNCALLBACK> = do-dde-callback;

define method do-dde-callback
    (uType   :: <integer>,  // transaction type
     uFmt    :: <integer>,  // clipboard format
     hConv   :: <HCONV>,    // handle of the conversation
     hsz1    :: <HSZ>,      // handle of a string
     hsz2    :: <HSZ>,      // handle of a string
     hData   :: <HDDEDATA>, // handle of a global memory object
     dwData1 :: <integer>,  // transaction-specific data
     dwData2 :: <integer>)  // transaction-specific data
 => (result  :: <HDDEDATA>)
/*
  // We have to respond to XTYP_ERROR.
  if ( uType = $XTYP-ERROR )
    report-dde-error(dwData1);
  end if;

  // Otherwise, don't do anything here, since we never expect to be
  // called.
*/
  // We'll never even be called, since we don't have a Windows event
  // loop!

  $NULL-HDDEDATA
end method;


/****************************************************************************

send-dde-command						[Function]

  Summary: Sends an (Execute) command string to a given DDE Service & Topic.

  Arguments:
    idInst     :: <machine-word>
      Instance indentifier.
    lpService  :: <byte-string>
      Name of the DDE Service to be used.
    lpTopic    :: <byte-string>
      Name of the Topic within the Service.
    lpCommand  :: <byte-string>
      Command string to be sent in an "Execute" transaction.

  Values:
    bResult :: <boolean>
      #t iff this function succeeds.

****************************************************************************/

define method send-dde-command
    (idInst :: <machine-word>,
     lpService :: <byte-string>, lpTopic :: <byte-string>,
     lpCommand :: <byte-string>)
 => (bResult :: <boolean>)
  // True iff this function is successful.
  let bResult :: <boolean> = #f;

  // Create DDE string handles for Service and Topic names.
  let hszService :: <HSZ>
    = DdeCreateStringHandle(idInst, lpService, $CP-WINANSI);
  check-dde-result(idInst, hszService);
  let hszTopic :: <HSZ>
    = DdeCreateStringHandle(idInst, lpTopic, $CP-WINANSI);
  check-dde-result(idInst, hszTopic);

  // Attempt to start a conversation with the DDE Service.
  let hconv :: <HCONV>
    = DdeConnect
        (idInst, hszService, hszTopic, null-pointer(<LPCONVCONTEXT>));
  check-dde-result(idInst, hConv);

  // Find the length of the command string.
  let nLen :: <integer> = size(lpCommand);

  // Send the Execute command to the Server.
  with-c-string (c-string = lpCommand)
    let c-byte-array = pointer-cast(<LPBYTE>, c-string);
    let (hData :: <HDDEDATA>, dwResult :: <integer>)
      = DdeClientTransaction
	  (c-byte-array,    // data to pass
	   nLen + 1,        // length of data
	   hconv,           // handle of conversation
	   $NULL-HSZ,       // handle of name-string
	   $CF-TEXT,        // clipboard format
	   $XTYP-EXECUTE,   // transaction type
	   1000);           // timeout duration (in milliseconds)
    check-dde-result(idInst, hData);

    // Note:
    // This is a synchronous DDE call, so we don't have to hang about
    // waiting for replies.  We could act on the DdeGetLastError()
    // value, in case of failure, or on the contents of dwResult, in
    // case of success, but we choose not to, to keep things simple
    // for now.

    unless (null-pointer?(hData))
      check-dde-result(idInst, DdeFreeDataHandle(hData));
      bResult := #t;
    end unless;
  end with-c-string;

  // End the DDE conversation.
  check-dde-result(idInst, DdeDisconnect(hconv));

  // Free the Service and Topic string handles.
  check-dde-result(idInst, DdeFreeStringHandle(idInst, hszService));
  check-dde-result(idInst, DdeFreeStringHandle(idInst, hszTopic));

  bResult
end method;


/***************************************************************************/
//
// Main entry point.
//
/***************************************************************************/

define method format-out-error-message (message :: <string>) => ()
  format-out(as(<byte-string>, concatenate("Error: ", message, "\n")));
end method;

define method usage (exit-function :: <function>, message :: <string>) => ()
  format-out-error-message(message);
  format-out(as(<byte-string>,
                concatenate("Usage:\n  ", application-name(),
                ": <server> <topic> <execute-string> ...\n")));
  exit-function(#f);
end method;

define method main () => ()
  // idInst is the DDE instance handle.
  let idInst :: <machine-word> = $machine-word-zero;
  let success? :: <boolean>
    = block(return)
        let app-args = application-arguments();
        (size(app-args) >= 3)
          | usage(return, "Too few arguments (minimum 3)");

        // Extract the Service name.
        let service-name :: <byte-string>
          = as(<byte-string>, element(app-args, 0, default: ""));
        // Extract the Topic name.
        let topic-name :: <byte-string>
          = as(<byte-string>, element(app-args, 1, default: ""));
        let rest-args = copy-sequence(app-args, start: 2);
        // Extract the Execute command string.
        let exec-string :: <byte-string>
          = as(<byte-string>, reduce1(concatenate, rest-args));

        idInst := initialize-dde();
        send-dde-command(idInst, service-name, topic-name, exec-string)
      cleanup
        idInst ~= $machine-word-zero
          & finalize-dde(idInst);
      end block;

  exit-application
    (if (success?)
       0 // "$EXIT-SUCCESS"
     else
       1 // "$EXIT-FAILURE"
     end);
end method;


/// -=- And here's the function call which starts us off ... -=-

begin
  main();
end;
