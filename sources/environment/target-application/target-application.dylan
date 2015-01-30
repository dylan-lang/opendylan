Module:    target-application-internals
Synopsis:  Defining a <debug-target> class that has special functionality
           for multi-threaded access.
Author:    Paul Howard, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function thread-debug-message
    (string :: <string>, #rest pants) => ()
  if (*debugging-debugger?*)
    let control =
      concatenate(current-thread().thread-name | "???", " : ", string);
    apply(debugger-message, control, pants)
  end if
end function;

define constant <interruption-type>
  = type-union(<function>, <thread>, singleton(#f));



///// <TARGET-APPLICATION>
//    A <debug-target> that contains synchronization functionality. This
//    class is used by the debugger and by the application-server backend
//    for environment objects.

define class <target-application> (<debug-target>)

  // Lock and serialize all calls into the DM.


  // Use a single-exclusive/multiple-inclusive lock over the entire
  // debugger session, to lock all threads out until the Debugger
  // Manager thread itself effortlessly releases its exclusive rights
  // to the session lock.

  constant slot debugger-session :: <read-write-lock> = make(<read-write-lock>);

  // A lock on the current debugger transaction to synchronize threads
  // requesting spy calls on DM or requesting DM to continue the application

  constant slot debugger-transaction :: <simple-lock> = make(<simple-lock>);

  slot debugger-transaction-timeout = #f;

  // A lock for client threads when requesting debugger transactions to
  // lock out other requesting threads until DM has served our request

  constant slot debugger-transaction-request :: <simple-lock> = make(<simple-lock>);

  // A lock for enabling client threads to optionally synchronize
  // with the Debugger Manager on application termination

  constant slot application-shut-down-lock :: <simple-lock> = make(<simple-lock>);

  // Record the current stop reason for the application

  slot current-stop-reason :: <stop-reason>;

  // Store the thread that runs the DM loop, as well as any thread
  // that gains exclusive debug access.

  slot manager-thread :: <thread>;
  slot thread-being-served :: false-or(<thread>) = #f;

  // Two notifications that can be used anywhere in the UI. These
  // get notified when debugger transactions start and finish.

  slot debugger-transaction-notification :: <notification>;
  slot debugger-transaction-complete :: <notification>;

  // A boolean flag to indicate whether a debugger transaction is
  // currently in effect.

  slot in-debugger-transaction? :: <boolean>,
    init-value: #f;

  // Internal Slots.

  slot under-management? :: <boolean>,
    init-value: #f;

  slot been-managed? :: <boolean>,
    init-value: #f;

  slot stored-interactor-handler :: <function>;

  // Support for "interrupting" transactions (to perform spy calls
  // without officially ending the transaction)

  slot interruption-evaluated :: <notification>;

  slot interruption-function :: <interruption-type> = #f;

  slot interruption-results :: <sequence> = #[];

  slot stored-library-initialization-phase-handler :: <function>;

end class;



///// NULL-STOP-REASON-CALLBACK
//    Should never get called

define method null-stop-reason-callback
    (application :: <target-application>, sr :: <stop-reason>)
        => (bool :: <boolean>)
  #f
end method;


///// NULL-INTERACTOR-CALLBACK
//    We don't care about the return values of interactive evaluations?

define method null-interactor-callback
    (application :: <target-application>, thread :: <remote-thread>,
     transaction-id :: <object>, #rest return-values)
        => (answer :: <boolean>)
  #f
end method;

define method null-library-initialization-phase-callback
    (application :: <target-application>, thread :: <remote-thread>,
     remote-library :: <remote-library>,
     phase :: <library-initialization-phase>, top-level? :: <boolean>)
 => (interested? :: <boolean>)
  #f
end method;

define method null-application-state-callback
    (application :: <target-application>, new-state :: <symbol>) => ()
end method;


/// OBTAIN-COMPONENT-NAME
///
/// Get the DLL name for a given library
/// XXX does not work with library packs! find-library-info is broken
/// we hope nobody calls it (it wasn't called during our debugging sessions)

define method obtain-component-name
    (application :: <target-application>, library-name :: <string>)
 => (component-name :: <string>)
  let info = find-library-info(as(<symbol>, library-name));
  case
    info      => info.info-binary-name;
    otherwise => library-name;
  end
end method obtain-component-name;


///// HANDLE-LIBRARY-INITIALIZATION-PHASE
//    The method for an environment <target-application>.

define method handle-library-initialization-phase
    (application :: <target-application>, thread :: <remote-thread>,
     remote-library :: <remote-library>,
     phase :: <library-initialization-phase>, top-level? :: <boolean>)
 => (interested? :: <boolean>)

  let interested? =
    application.stored-library-initialization-phase-handler
      (application, thread, remote-library, phase, top-level?);

  interested?
end method;


///// HANDLE-INTERACTOR-RETURN
//    The method for an environment <target-application>.

define method handle-interactor-return
    (application :: <target-application>, thread :: <remote-thread>,
     transaction-id :: <object>, #rest return-values)
       => (answer :: <boolean>)

  next-method();
  let answer =
    apply(application.stored-interactor-handler,
          application,
          thread,
          transaction-id,
          return-values);

  answer
end method;


///// <TEMPORARY-INTERNAL-DEBUGGER-TRANSACTION-STOP>
//    A stop reason that is not propagated to the environment.

define class <temporary-internal-debugger-transaction-stop>
                 (<debugger-generated-stop-reason>)
end class;


///// RUN-TARGET-APPLICATION
//    This is basically like MANAGE-RUNNING-APPLICATION, except that it
//    does not require callback parameters. This library exports open
//    generic functions that are used as the callbacks.
//    This function runs on the calling thread, and does not return until
//    the application quits.

define method run-target-application
    (application :: <target-application>,
     #key stop-reason-callback = null-stop-reason-callback,
          dt-prolog = #f,
          dt-epilog = #f,
          interactor-callback = null-interactor-callback,
          library-init-callback =
            null-library-initialization-phase-callback,
          application-state-callback =
            null-application-state-callback)
      => ()

  local method manage-stop-reason
            (app :: <target-application>, sr :: <stop-reason>)
         => (interested? :: <boolean>)

           thread-debug-message
             ("Entering stop-reason callback for %=", sr);
           let stopping? :: <boolean> =
             if (instance?(sr, <temporary-internal-debugger-transaction-stop>))
               #t
             else
               block()
                 stop-reason-callback(app, sr)
               exception(<abort>)
                 #f
               end block;
             end if;
           thread-debug-message("Returning %= from sr callback", stopping?);
           stopping?
        end method;

  local method manage-debugger-transaction-for-stop-reason
            (app :: <target-application>, sr :: <stop-reason>) => ()

          unless (instance?(sr, <temporary-internal-debugger-transaction-stop>))
            application-state-callback(app, #"stopped");

            if (dt-prolog)
              thread-debug-message("Running debugger transaction prolog");
              block()
                dt-prolog(app, sr)
              exception(<abort>)
                values()
              end block
            end if;
          end unless;

          app.current-stop-reason := sr;
          manage-debugger-transaction(app);

          unless (instance?(sr, <temporary-internal-debugger-transaction-stop>))
            if (dt-epilog)
              thread-debug-message("Running debugger transaction epilog");
              block()
                dt-epilog(app, sr)
              exception(<abort>)
                values()
              end block
            end if;

            application-state-callback(app, #"running");
          end unless;

        end method;


  if (application.been-managed?)
    error("This application has run and terminated already");
  elseif (application.under-management?)
    error("This application is already running");
  else

    with-lock (application.application-shut-down-lock)

    application.under-management? := #t;
    application.manager-thread := current-thread();
    application.stored-interactor-handler := interactor-callback;
    application.stored-library-initialization-phase-handler :=
       library-init-callback;
    application-state-callback(application, #"uninitialized");

    with-lock (application.debugger-session, mode: #"write")
      manage-running-application
        (application,
         stop-reason-callback: manage-stop-reason,
         ready-to-continue-callback: manage-debugger-transaction-for-stop-reason);
      application-state-callback(application, #"closed");
      application.under-management? := #f;
      application.been-managed? := #t;
    end with-lock;

    end with-lock;

  end if;
end method;


///// INITIALIZE (<TARGET-APPLICATION>)
//    Sets up the notification fields, and associates them with the
//    debugger transaction lock.

define method initialize
    (application :: <target-application>, #rest keys, #key, #all-keys) => ()
  next-method();

  application.debugger-transaction-notification :=
    make(<notification>,
         lock: application.debugger-transaction);

  application.debugger-transaction-complete :=
    make(<notification>,
         lock: application.debugger-transaction);

  application.interruption-evaluated :=
    make(<notification>,
         lock: application.debugger-transaction);

end method;

