Module:    dfmc-application
Synopsis:  Protocol implementations for callback registration
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// REGISTER-STOP-REASON-CALLBACK (internal)
//    Registers a function with this particular application. The
//    function will be invoked when stop-reasons occur in the
//    running application.
//    The callback will be invoked with the application known to be
//    stopped, but WITHOUT a debugger transaction being put into
//    effect.
//    The callback will accept two arguments, a <project-object>
//    and a <stop-reason>, and must return a boolean value indicating
//    interest in the stop reason.

define method register-stop-reason-callback
    (application :: <dfmc-application>, callback :: <function>)
 => ()
  with-lock(application.callback-registration-lock)
    if (application.registered-stop-reason-callback)
      error("An attempt was made to overwrite a registered stop-reason "
            "callback");
    else
      application.registered-stop-reason-callback := callback;
    end if;
  end with-lock
end method;


///// REGISTER-DEBUGGER-TRANSACTION-PROLOG (internal)
//    Registers a function with this particular application. The function
//    will be invoked _after_ a stop reason callback has been called
//    and returned #t.

define method register-debugger-transaction-prolog
    (application :: <dfmc-application>, callback :: <function>)
 => ()
  with-lock(application.callback-registration-lock)
    if (application.registered-debugger-transaction-prolog)
      error("An attempt was made to overwrite a registered "
            "debugger transaction prolog function");
    else
      application.registered-debugger-transaction-prolog := callback;
    end if
  end with-lock
end method;


///// REGISTER-DEBUGGER-TRANSACTION-EPILOG (internal)
//    Registers a function with this particular application. The function
//    will be invoked at the end of any debugger transaction, just before
//    the application is allowed to continue. (The debugger transaction
//    will still be in effect).

define method register-debugger-transaction-epilog
    (application :: <dfmc-application>, callback :: <function>)
 => ()
  with-lock(application.callback-registration-lock)
    if (application.registered-debugger-transaction-epilog)
      error("An attempt was made to overwrite a registered "
            "debugger transaction epilog function");
    else
      application.registered-debugger-transaction-epilog := callback;
    end if
  end with-lock
end method;

/*
///// REGISTER-DEBUG-POINT-CALLBACK (internal)
//    Registers a function with this particular application. The function
//    will be invoked whenever a debug point of some kind is encountered
//    by the application.

define method register-debug-point-callback
    (application :: <dfmc-application>, callback :: <function>)
 => ()
  with-lock(application.callback-registration-lock)
    if (application.registered-debug-point-callback)
      error("An attempt was made to overwrite a registered "
            "debug point callback");
    else
      application.registered-debug-point-callback := callback;
    end if
  end with-lock
end method;
*/

///// REGISTER-INTERACTOR-RETURN-CALLBACK (internal)
//    Registers the callback via which the environment handles the
//    returns of interactive executions.

define method register-interactor-return-callback
    (application :: <dfmc-application>, callback :: <function>)
 => ()
  with-lock(application.callback-registration-lock)
    if (application.registered-interactor-handler)
      error("An attempt was made to overwrite a registered "
            "interactor return handler");
    else
      application.registered-interactor-handler := callback;
    end if
  end with-lock
end method;


///// REGISTER-LIBRARY-INITIALIZATION-CALLBACK (internal)
//    Registers the callback via which the environment handles the
//    initialization phases of libraries.

define method register-library-initialization-callback
    (application :: <dfmc-application>, callback :: <function>)
 => ()
  with-lock(application.callback-registration-lock)
    if (application.registered-library-init-handler)
      error("An attempt was made to overwrite a registered "
            "library initialization handler");
    else
      application.registered-library-init-handler := callback;
    end if
  end with-lock
end method;
