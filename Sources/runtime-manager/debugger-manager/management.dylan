module:      dm-internals
synopsis:    Managing the running application in the DM
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// $DEFAULT-TIMEOUT

define constant $default-timeout = 200;


///// STOP-APPLICATION

define method stop-application 
     (application :: <debug-target>, 
      #key stop-reason = make(<debugger-stop-application-stop-reason>)) => ()
  application.application-stopped? := #t;
  application.debugger-generated-stop-reason := stop-reason;
end method;


///// KILL-APPLICATION

define method kill-application (application :: <debug-target>) => ()
  application.application-killed? := #t;
end method;


///// RESTART-APPLICATION

define method restart-application (application :: <debug-target>) => ()
  forget-absolutely-everything(application);
  application.application-restarted? := #t;
end method;


///// FORGET-ABSOLUTELY-EVERYTHING
//    This completely discards all cached information about the debug
//    target. The only time to call this function is when the app is
//    restarted. The access-path is the only element that isn't
//    forgotten by this function.

define method forget-absolutely-everything (application :: <debug-target>)
    => ()
  application.application-killed? := #f;
  application.registered-debug-points :=  make (<stretchy-vector>, size: 0);
  application.need-to-clear-debug-points? := #f;
  application.application-threads := make(<table>);
end method;


///// DISPOSE-ALL-STATE
//    This must be called at the end of a debugger transaction.
//    It will forget all cached information about the stack for all
//    threads. It will not forget persistant information such as the
//    location of wrappers and spy functions, loaded libraries and
//    active threads.

define method dispose-all-state (application :: <debug-target>) => ()
  let access-path = application.debug-target-access-path;

  do-threads
  (method(thr :: <remote-thread>)
    if (thr.thread-suspended?)
      recover-breakpoint(access-path, thr);
    end if;
   end method,
   access-path);

  for (thr :: <application-thread> in application.application-threads)
    unless (thr.remote-thread-object.thread-suspended?)
      invalidate-thread-state(thr);
    end unless;
  end for;

  application.application-selected-thread := #f;
  application.application-just-interacted? := #f;
end method;


///// OPEN-DEBUGGER-TRANSACTION
//    Performs all work associated with starting off a debugger transaction.
//    This means ensuring that no thread is running within the MM, and
//    telling the MM not to relocate objects.

define method open-debugger-transaction (application :: <debug-target>)
    => ()
//    ensure-mm-function-info-initialized(application);
//    step-all-threads-out-of-mm(application);
//    mm-start-debugger-transaction(application);
  application.new-debugger-transaction? := #t;
end method;


///// CLOSE-DEBUGGER-TRANSACTION
//    Performs all work associated with terminating a debugger transaction.

define method close-debugger-transaction (application :: <debug-target>)
    => ()
//  mm-end-debugger-transaction(application);
end method;


///// MANAGE-RUNNING-APPLICATION

// This is it! The function that fires up the application via its access path
// and goes into the callback-invokation loop.

// First, default callbacks to cover for unsupplied callback functions.

define method default-stop-reason-callback
              (application :: <debug-target>, sr :: <stop-reason>) 
               => (interested? :: <boolean>)
       #t;
end method;

define method default-poll-for-stop-callback 
              (application :: <debug-target>) => ()

end method;

define method default-ready-to-continue-callback
              (application :: <debug-target>, sc :: <stop-reason>) => ()
       
end method;


define method manage-running-application 
  (application :: <debug-target>,
   #key stop-reason-callback = default-stop-reason-callback,
        poll-for-stop-callback = default-poll-for-stop-callback,
        ready-to-continue-callback = default-ready-to-continue-callback)
  => ()

  let access-path = application.debug-target-access-path;

  // MAYBE-CONTINUE
  // Continues the application unless a control API has signalled
  // otherwise. Might kill or restart the application.

  local method maybe-continue (stop-reason :: false-or(<stop-reason>))

    if (instance?(stop-reason, <internal-stop-reason>))

      take-thread-out-of-source-step-mode
         (application, stop-reason.stop-reason-thread);

    end if;

    suspend-interesting-thread(application, stop-reason);

    open-debugger-transaction(application);
    ready-to-continue-callback(application, stop-reason);
    close-debugger-transaction(application);

    if (application.application-killed?)
      clear-deregistered-debug-points(application);
      dispose-all-state(application);
      let success? = kill(access-path);
      if (success?)
	application.up-and-running? := #f;
      else
	application.application-killed? := #f;
      end;
    elseif (application.application-restarted?)
      application.application-restarted? := #f;
      restart(access-path);
    elseif (stop-reason)

    let threads = #();
    let remote-thread = application.application-selected-thread;
    if (remote-thread)
      let resumed-thread =
	resume-selected-thread(application);
      if (resumed-thread)
	threads := pair(resumed-thread, threads);
      end;
    else
      select(stop-reason by instance?)
	<internal-stop-reason>, <debugger-stop-application-stop-reason> =>
	  threads := resume-all-suspended-threads(application);
	otherwise => #f;
      end select;
    end if;

      clear-deregistered-debug-points(application);
      dispose-all-state(application);
      dm-continue(application, stop-reason, resume: threads);
    end if;
  end method;


  // DEFINITELY-CONTINUE
  // Disposes all cached state in the debug target, and continues
  // the application.

  local method definitely-continue (stop-reason :: false-or(<stop-reason>))
    if (stop-reason)
      clear-deregistered-debug-points(application);
      dispose-all-state(application);
      dm-continue(application, stop-reason);
    end if;
  end method;

  // START-IT-ALL-OFF
  // If we attatched to this debug target, it's access-path will already
  // be signalling that the application is running. In this case, we
  // do not have to "jump-start" it with 'restart'. If instead, we have
  // generated this application ourselves, we must call 'restart'.

  local method start-it-all-off ()
    unless(application-state-running?(access-path))
      restart(access-path);
    end unless;
    application.application-killed? := #f;
    application.up-and-running? := #t;
  end method;

  // ---------------------------------------------------------------------
  // ----------------------- MAIN PROCESSING LOOP ------------------------
  // ---------------------------------------------------------------------

  start-it-all-off();

  while (application.up-and-running?)

    // Indicate that threads are beginning to run, so we can't assume any
    // one thread to be 'good' for running spy functions on.

    use-thread-for-spy-functions(application, #f);

    // Obtain the "primitive" stop reason from the access path,
    // supplying the 'profile-interval:' keyword if the profiler
    // controller specifies it.
    let profiling-interval = application.application-profiling-interval;
    let stop-reason = 
      if (profiling-interval)
        wait-for-stop-reason(application.debug-target-access-path,
			     profile-interval: profiling-interval);
      else
        wait-for-stop-reason(application.debug-target-access-path,
			     timeout: $default-timeout);
      end if;

    // Interpret the stop-reason, perform housekeeping and debug-point
    // processing.

    let (dm-stop-reason, interesting-debug-points?, original-stop-reason)
      = if (stop-reason) 
          interpret-stop-reason(application, stop-reason);
        else
          if (application.application-killed?)
            application.up-and-running? := #f
          end if;
          values(#f, #f, #f);
        end if;

    // If a stop reason still remains to be processed, then call the
    // client's callback. 
    // Otherwise, if some debug points are registering interest at this
    // point, enter the required debugger transaction.
    // In all other cases, just continue.

    if (dm-stop-reason)
      let client-interested? =
        stop-reason-callback(application, dm-stop-reason);
      if (client-interested?)
        maybe-continue(dm-stop-reason);
      else
        definitely-continue(dm-stop-reason);
      end if;  
    elseif (interesting-debug-points? |
            instance?(original-stop-reason, <source-step-stop-reason>))
      let client-interested? =
        stop-reason-callback(application, original-stop-reason);
      if (client-interested?)
        maybe-continue(original-stop-reason);
      else
        definitely-continue(original-stop-reason);
      end if;
    else
      definitely-continue(original-stop-reason);
    end if;

    // Perform a periodic poll of the stop
    // button. (The application may be running now).

    poll-for-stop-callback(application);
    if (application.application-stopped?)
      stop (application.debug-target-access-path);
      application.application-stopped? := #f;
      use-thread-for-spy-functions(application, #f);
      maybe-continue(application.debugger-generated-stop-reason);
    end if;
  
  end while;

  close-application(access-path);
end method;

define method dm-continue
  (application :: <debug-target>, stop-reason :: <internal-stop-reason>,
   #key resume)
  let access-path = application.debug-target-access-path;
  if (first-chance-exception?(access-path, stop-reason.stop-reason-thread))
    continue-unhandled(access-path, resume: resume);
  else
    continue(access-path, resume: resume);
  end if;
end method;

define method dm-continue
  (application :: <debug-target>, stop-reason :: <stop-reason>,
   #key resume)
  let access-path = application.debug-target-access-path;
  continue(access-path, resume: resume);
end method;

define method dm-continue
  (application :: <debug-target>, stop-reason :: <unhandled-stop-reason>,
   #key resume)
  let access-path = application.debug-target-access-path;
  continue-unhandled(access-path, resume: resume);
end method;
