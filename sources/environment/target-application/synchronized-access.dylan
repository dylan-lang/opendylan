Module:    target-application-internals
Synopsis:  Synchronizing access to the target application
Author:    Paul Howard, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// PERFORM-DEBUGGER-TRANSACTION
//    Takes a <target-application> and a <function>.
//    With a claim on debugger access to the application, ensures that a
//    debugger transaction is in effect. (It will force one to begin, if
//    one is not in effect already). Calls back to the supplied function,
//    and then ends the debugger transaction if it had to start one
//    specifically.
//    (All environment protocol queries can take place inside a call to
//    this function).


// Revised the synchronization protocol to meet the higher demands
// of interacting with suspended threads in running applications.
//
// Use explicit synchronization throughout.
//
// Use a single-exclusive/multiple-inclusive lock over the entire
// debugger session, to lock all threads out until the Debugger
// Manager thread itself effortlessly releases its exclusive rights
// to the session lock.
//
//
// Nosa   Mar 31, 1999

define macro with-debugger-transaction
  { with-debugger-transaction (?application:name)
      ?body:body
    end }
 => { perform-debugger-transaction(?application, method () ?body end) }
  { with-debugger-transaction (?application:name, ?options:*)
      ?body:body
    end }
 => { perform-debugger-transaction
        (?application, method () ?body end, ?options) }
end macro with-debugger-transaction;

define method perform-debugger-transaction
    (application :: <target-application>, transaction :: <function>,
     #key continue)
 => (#rest results)

  if (application.performing-debugger-transaction?)
    assert(~continue,
           "Cannot continue from inside another debugger transaction");
    transaction()
  else

  local method do-transaction () => (#rest results)
          block()
            transaction();
          exception(<abort>)
            values(); // No action.
          end block;
        end method;

  let transaction-thread = current-thread();

  if (transaction-thread == application.manager-thread)
    block ()
      do-transaction()
    afterwards
      if (continue) continue() end
    end
  else

    with-lock (application.debugger-transaction-request)

      // Request an application stop

      stop-application-request(application);

      let temporary-stop? = #f;

      block ()
        with-lock (application.debugger-session, mode: #"read")

          // Discard the application stop request as we are now
          // being served

          discard-stop-application-request(application);

          if (application.under-management?)

          temporary-stop? := application-temporary-stop?(application);

          thread-debug-message("Performing debugger transaction");
          block()
            application.thread-being-served := transaction-thread;
            do-transaction();
          cleanup
            application.thread-being-served := #f
          end block;

          end if;

        end with-lock;
      afterwards
        // Continue the application only if explicitly requested to do
        // so by clients, or if we explicitly stopped the running application
        if (continue)
          thread-debug-message
            ("perform-debugger-transaction: continue application");
          continue();
        elseif (temporary-stop?)
          thread-debug-message
            ("temporary-stop-reason for interrupting application");
          continue-target-application(application,
                                      application.application-selected-thread);
        end if
      end
    end with-lock;

  end if;

  end if;
end method;

define inline function performing-debugger-transaction?
    (application :: <target-application>) => (performing-transaction? :: <boolean>)
  application.thread-being-served == current-thread()
end function;

define method manage-debugger-transaction
    (application :: <target-application>) => ()

  unless (application.debugger-session.owned?)
    error("Debugger Manager transaction synchronization protocol error.");
  end unless;

  let thunk :: <interruption-type> = #f;

  with-lock (application.debugger-transaction)

    // with a lock on the current transaction, open the floodgates
    // for threads requiring debugger transactions

    thread-debug-message("Releasing debugger-session");
    release(application.debugger-session);

    // Enter a wait state for continuously serving client thread
    // requests that can only be run by the Debugger Manager

    application.in-debugger-transaction? := #t;

    while (begin
             let wait-state = #"waiting";

             while (wait-state == #"waiting")

             thread-debug-message("Waiting for debugger-transaction-notification");
             if (wait-for(application.debugger-transaction-notification,
                          timeout: application.debugger-transaction-timeout))
               thunk := application.interruption-function;
               wait-state := thunk;
             else
               thread-debug-message("Waiting for debugger-session");
               if (wait-for(application.debugger-session, mode: #"write",
                            timeout: application.debugger-transaction-timeout))
                 wait-state := #f;
               end;
             end;

             end while;

             wait-state
           end)
      block()
        application.interruption-function := #f;
        let thunk-as-function :: <function> = thunk;
        let (#rest results) = thunk-as-function();
        application.interruption-results := results;
      exception(<abort>)
        application.interruption-results := #[];
      end block;

      thread-debug-message("Releasing interruption-evaluated");
      release(application.interruption-evaluated);

    end while;

    application.in-debugger-transaction? := #f;
    application.debugger-transaction-timeout := #f;

    // Now wait until all other threads have released their
    // inclusive claims on the session before going back into
    // exclusive mode

    unless (application.debugger-session.owned?)
      thread-debug-message("Waiting for debugger-session");
      wait-for(application.debugger-session, mode: #"write");
    end;

    // Lastly, signal all client threads that this debugger
    // transaction is now complete, in response to a request
    // to continue the stopped thread

    thread-debug-message("Releasing debugger-transaction-complete");
    release-all(application.debugger-transaction-complete);

  end with-lock;

  thread-debug-message("debugger transaction is complete");

end method;


///// CALL-DEBUGGER-FUNCTION
//    Calls a function with arguments, ensuring that the call
//    happens on the debugger manager's thread. This is achieved
//    by notifying an interruption of the debugger transaction.

define method call-debugger-function
    (application :: <target-application>, function :: <function>,
     #rest arguments)
  => (#rest results)
  thread-debug-message("Entered CALL-DEBUGGER-FUNCTION");
  let transaction-thread = current-thread();
  if (transaction-thread == application.manager-thread)
    apply(function, arguments)
  else

    unless (application.performing-debugger-transaction?)
      error("Debugger transaction synchronization protocol error.");
    end unless;

    with-lock (application.debugger-transaction)

      application.interruption-function :=
        method() apply(function, arguments) end;

      thread-debug-message("Releasing debugger-transaction-notification");
      release(application.debugger-transaction-notification);

      thread-debug-message("Waiting for interruption-evaluated");
      wait-for(application.interruption-evaluated);

      let results = application.interruption-results;
      application.interruption-results := #[];

      apply(values, results);

    end with-lock;

  end if;
end method;



///// PERFORM-REQUIRING-DEBUGGER-TRANSACTION
//    Takes a <target-application> and a <function>.
//    With a claim on debugger access to the application, checks to see
//    that a debugger transaction is in effect. If so, the client's
//    callback is performed, otherwise just return to caller.

define method perform-requiring-debugger-transaction
    (application :: <target-application>, transaction :: <function>) => ()
  if (application.under-management?)
    if (application.in-debugger-transaction?)
      perform-debugger-transaction(application, transaction);
    end if;
  end if;
end method;
