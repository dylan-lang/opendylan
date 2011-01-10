Module:    dfmc-application
Synopsis:  Serving Thread environment objects from the application.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// <APPLICATION-THREAD-STATE>
//    An internal class used to store everything interesting (at this level)
//    about the state of a thread.

define class <application-thread-state> (<object>)

  constant slot thread-state-interactor-queue :: <deque> = make(<deque>);

  slot thread-state-requesting-interaction? :: <boolean> = #f,
    init-keyword: requesting-interaction?:;

  constant slot thread-state-thread-index :: <integer>,
    required-init-keyword: thread-index:;

  slot thread-state-thread-name :: false-or(<byte-string>) = #f,
    init-keyword: thread-name:;

  slot thread-spawned-by-environment? :: <boolean> = #f;

  // Need to cache the transaction id for interactions on
  // suspended threads; this will be used to determine whether
  // the interaction has completed, and so the thread can be put
  // back into permanent suspension by the environment

  slot thread-transaction-id = #f;

end class <application-thread-state>;

define inline method thread-state-model
    (application :: <dfmc-application>, thread :: <remote-thread>)
 => (state :: false-or(<application-thread-state>))
  element(application.application-thread-state-model, thread, default: #f)
end method thread-state-model;

define inline method thread-state-model-setter
    (state :: <application-thread-state>, application :: <dfmc-application>, 
     thread :: <remote-thread>)
 => (state :: <application-thread-state>)
  application.application-thread-state-model[thread] := state
end method thread-state-model-setter;

define inline method thread-state-transaction
    (application :: <dfmc-application>, thread :: <remote-thread>)
 => (id)
  let state-model = thread-state-model(application, thread);
  state-model & state-model.thread-transaction-id
end method thread-state-transaction;

define inline method thread-state-transaction-setter
    (id :: <object>, application :: <dfmc-application>, 
     thread :: <remote-thread>)
 => (id)
  let state-model = thread-state-model(application, thread);
  if (state-model)
    state-model.thread-transaction-id := id
  end
end method thread-state-transaction-setter;


///// <THREAD-INTERACTION-REQUEST>
//    An internal class used to store the information about deferred
//    interactor requests. These are created whenever a source-code string
//    is entered into the interactor at a time when the target thread
//    is not paused at an interactive location, and hence is not able
//    to actually evaluate the expression.

define class <thread-interaction-request> (<object>)

  constant slot interaction-request-string :: <byte-string>,
    required-init-keyword: string:;

  constant slot interaction-request-module :: <module-object>,
    required-init-keyword: module:;

  constant slot interaction-request-context :: <runtime-context>,
    required-init-keyword: context:;

  // When we can actually download the interactive code, we will get a
  // real ID from the project manager. This can be stored here, and
  // the app-server can perform the indirection when
  // FETCH-INTERACTOR-RETURN-VALUES is called.

  slot interaction-request-actual-id :: <object>,
    init-keyword: id:;

  // Also remember what the value of APPLICATION-STATE was when the
  // interactive code was entered.

  slot interaction-request-application-state = #"stopped",
    required-init-keyword: application-state:;

end class;


///// PROCESS-NEXT-INTERATION-REQUEST (Internal function)
//    If a thread is at an interactive location, and has a pending
//    interaction request on its queue, then download the code now,
//    and perform the deferred ID management.
//    If the thread is not at an interactive location, then ensure that
//    it is instructed to align.

define method process-next-interaction-request
    (application :: <dfmc-application>, thread :: <remote-thread>)
  => ()
  debugger-message("process-next-interaction-request on %=", thread);
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let state-model = thread-state-model(application, thread);
  let requests-pending? =
     (state-model.thread-state-interactor-queue.size > 0);
  if (requests-pending? & thread-available-for-interaction?(target, thread))
    if (thread-permanently-suspended?(path, thread))
      error("Suspended thread cannot have pending interaction requests");
    end if;
    // resume-evaluator-thread(application, thread);
    let request = pop(state-model.thread-state-interactor-queue);
    let module-actual-name
      = environment-object-primitive-name
          (application.server-project,
	   request.interaction-request-module);
    let actual-id
      = evaluate-expression(application.server-project.project-proxy,
			    request.interaction-request-context,
			    as(<symbol>, module-actual-name),
			    request.interaction-request-string);
    notify-warnings-to-environment
        (application,
         make-environment-object
           (<thread-object>,
            project: application.server-project,
            application-object-proxy: thread),
         request,
         request.interaction-request-module);
    request.interaction-request-actual-id := actual-id;
    application.application-target-app.interactor-deferred-id-table[actual-id]
      := request;
    if (state-model.thread-state-interactor-queue.size == 0)
      state-model.thread-state-requesting-interaction? := #f
    end if;
  end if;
end method;


///// REQUEST-INTERACTION (Internal function).
//    A function called to construct a <THREAD-INTERACTION-REQUEST>.
//    There is no requirement for a debugger transaction to be in effect
//    when this function is called.
//    This function creates the request description, and queues it in the
//    thread state model. If the thread has not already been instructed to
//    align to an interactive source location, then the instruction will
//    also be made here.

define method request-interaction
    (application :: <dfmc-application>, thread :: <remote-thread>,
     context :: <runtime-context>, module :: <module-object>, 
     code-string :: <byte-string>, state :: <application-state>)
 => (request :: <thread-interaction-request>)
  debugger-message("request-interaction on %=", thread);
  let request = make(<thread-interaction-request>,
                     string: code-string, module: module, context: context,
                     application-state: state);

  // Queue this request.
  let state-model = thread-state-model(application, thread);
  push-last(state-model.thread-state-interactor-queue, request);

  // Instruct the thread to align to an interactive source location.
  unless (state-model.thread-state-requesting-interaction?)
    state-model.thread-state-requesting-interaction? := #t;
    align-thread-to-source-location
      (application.application-target-app, thread, interactive?: #t);
  end unless;

  // Return the request object
  request
end method;


///// APPLICATION-STATE-AT-CODE-ENTRY (Environment Protocol)
//    Given the ID for an interactive evaluation, returns the value of
//    APPLICATION-STATE.

define sideways method application-state-at-code-entry
    (id :: <object>) => (state :: <application-state>)
  // The implementation of PROJECT-EXECUTE-CODE will have recorded the
  // application state within the ID.
  id.interaction-request-application-state
end method;


// The class <remote-thread>, defined in the access-path library, is used
// as the application proxy for <thread-object>. Since access-path
// already guarantees that these are interned, there is very little
// work for this library to do.


///// APPLICATION-THREADS (Environment Protocol Method)
//    Returns a vector of all threads currently live in the application.

define method application-threads
     (application :: <dfmc-application>, #key client) 
 => (threads :: <sequence>)
  if (application-tethered?(application))
    let target = application.application-target-app;
    let path = target.debug-target-access-path;
    with-debugger-transaction (target)
      let i = 0;
      let thread-sequence
	= make(<vector>, size: number-of-active-threads(path));
      do-threads(method (t :: <remote-thread>)
		   thread-sequence[i]
		     := make-environment-object(<thread-object>,
						project:
						  application.server-project,
						application-object-proxy: t);
		   i := i + 1;
		 end,
		 path);
      thread-sequence
    end
  else
    #[]
  end
end method application-threads;


///// THREAD-STACK-TRACE (Environment Protocol Method)
//    Returns the frame at the top of the stack for this thread.

define method thread-stack-trace
    (application :: <dfmc-application>, thread :: <thread-object>) 
 => (top-frame :: <stack-frame-object>)
  let top-dm-frame = #f;
  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;
  perform-requiring-debugger-transaction
     (target,
      method ()
        top-dm-frame := first-stack-frame(target, remote-thread);
      end method);
  make-environment-object(<stack-frame-object>,
                          project: application.server-project,
                          application-object-proxy: top-dm-frame);
end method;


///// THREAD-COMPLETE-STACK-TRACE (Environment Protocol Method)
//    Returns an ordered sequence of <stack-frame-object>s - all the frames
//    in the call stack of the specified thread.

define method thread-complete-stack-trace
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (all-frames :: <sequence>)
  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;

  // Within a debugger transaction, get every stack frame known for this
  // thread, and make an environment object for it.
  // If the debugger transaction is not performed (because the application
  // has been closed), then return whatever the stack trace was the last
  // time we examined it.

  with-debugger-transaction (target)
    let top-dm-frame = first-stack-frame(target, remote-thread);
    let this-frame = top-dm-frame;
    let all-frames = make(<stretchy-vector>);
    while (this-frame)
      add!(all-frames,
	   make-environment-object(<stack-frame-object>,
				   project: application.server-project,
				   application-object-proxy: this-frame));
      this-frame := previous-stack-frame(target, this-frame);
    end;
    all-frames
  end
end method;


///// THREAD-INDEX (Environment Protocol Method)
//    Returns the "index" of an application thread.

define method thread-index
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (index :: <integer>)
  let proxy = thread.application-object-proxy;
  let state-model = thread-state-model(application, proxy);
  state-model.thread-state-thread-index
end method;


///// THREAD-STATE (Environment Protocol Method)
//    Returns the state of the application thread as one of four symbols.

define method thread-state
    (application :: <dfmc-application>, thread :: <thread-object>)
      => (state :: <symbol>)
  if (thread.thread-suspended?)
    #"frozen"
  else
    thread.thread-runtime-state
  end if
end method;


///// CREATE-APPLICATION-THREAD (Environment Protocol Method)

// Spawn new application threads on the reserved Thread Manager
// application thread

define method create-application-thread
    (application :: <dfmc-application>, title :: <string>)
 => (thread :: <thread-object>)
  unless (application.dylan-thread-manager)
    error("Thread Manager does not exist: Cannot create a new application thread");
  end;
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  with-debugger-transaction (target)
    block ()
      unless (thread-available-for-interaction?
		(target, application.dylan-thread-manager))
	error("Thread Manager broken: Cannot create a new application thread");
      end;
      thread-permanently-suspended?(path, application.dylan-thread-manager) := #f;
      let thread
	= if (empty?(title))
	    request-evaluator-thread(application);
	  else
	    request-evaluator-thread(application, name: title);
	  end if;
      make-environment-object
	(<thread-object>,
	 project: application.server-project,
	 application-object-proxy: thread)
    cleanup
      thread-permanently-suspended?(path, application.dylan-thread-manager) := #t;
      unless (thread-available-for-interaction?
		(target, application.dylan-thread-manager))
	error("Thread Manager broken while spawning new thread");
      end;
    end block;
  end
end method;


///// SUSPEND-APPLICATION-THREAD (Environment Protocol Method)
//    Freezes a thread. It will not be allowed to run when the application
//    continues. To allow the thread to run again, a call to
//    RESUME-APPLICATION-THREAD must be made in this (or a subsequent)
//    debugger transaction.
//    The thread may not be in the #"frozen" state before this call,
//    but it will be following the call.

define method suspend-application-thread
    (application :: <dfmc-application>, thread :: <thread-object>) => ()

  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let remote-thread = thread.application-object-proxy;

  if (reserved-interactive-thread?(application, remote-thread))
    error("Permission denied: This is a reserved application thread");
  end if;

  with-debugger-transaction (target)
     if (thread-permanently-suspended?(path, remote-thread))
       error("This thread has already been suspended");
     end if;

    unless (thread-available-for-interaction?(target, remote-thread))
      error("Cannot suspend a thread that is not ready for interaction");
    end;
    debugger-message("Suspending environment interactive thread %=", remote-thread);
    suspend-thread(path, remote-thread);
    thread-permanently-suspended?(path, remote-thread) := #t;
  end
end method;


///// RESUME-APPLICATION-THREAD (Environment Protocol Method)
//    Allows a thread to continue running.
//    The thread must be in the #"frozen" state before this call.

// Threads explicitly spawned by the environment are not permitted to
// be resumed

define method resume-application-thread
    (application :: <dfmc-application>, thread :: <thread-object>) => ()
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let remote-thread = thread.application-object-proxy;
  let state-model = thread-state-model(application, remote-thread);

  if (state-model.thread-spawned-by-environment?)
    error("Resume failed: this is a special thread that has been spawned for interaction");
  end if;

  with-debugger-transaction (target)
     unless (thread-permanently-suspended?(path, remote-thread))
       error("This thread is not currently suspended");
     end unless;

    resume-evaluator-thread(application, remote-thread);
    resume-thread(path, remote-thread);
  end
end method;


///// THREAD-CURRENT-INTERACTOR-LEVEL (Environment Protocol Method)
//    Returns an integer that counts the number of times that this
//    thread has recursively entered a debugging context. This will be
//    zero if the code currently running on the thread is non-interactive.

define method thread-current-interactor-level
    (application :: <dfmc-application>, thread :: <thread-object>)
  => (level :: <integer>)
  let target = application.application-target-app;
  let remote-thread = thread.application-object-proxy;
  with-debugger-transaction(target)
    // Ask the DM for the definitive value
    get-thread-interactor-level(target, remote-thread)
  end
end method;


///// ADD-APPLICATION-OBJECT-TO-THREAD-HISTORY (Environment Protocol)
//    Given an <application-object> that contains a Dylan value, add
//    it to the interactor history.
//    This protocol can return #f for objects that cannot be added to the
//    history.

define method add-application-object-to-thread-history
    (application :: <dfmc-application>, thread :: <thread-object>,
     object :: <foreign-object>)
  => (history-varname :: false-or(<string>))
  #f
end method;

define method add-application-object-to-thread-history
    (application :: <dfmc-application>, thread :: <thread-object>,
     object :: <breakpoint-object>)
  => (history-varname :: false-or(<string>))
  #f
end method;

define method add-application-object-to-thread-history
    (application :: <dfmc-application>, thread :: <thread-object>,
     object :: <stack-frame-object>)
  => (history-varname :: false-or(<string>))
  #f
end method;

define method add-application-object-to-thread-history
    (application :: <dfmc-application>, thread :: <thread-object>,
     object :: <application-object>)
  => (history-varname :: false-or(<string>))
  let target = application.application-target-app;
  with-debugger-transaction (target)
    let proxy = object.application-object-proxy;
    if (proxy)
      let value = runtime-proxy-to-remote-value(application, proxy);
      let remote-thread = thread.application-object-proxy;
      let (index, name)
	= record-object-in-thread-history(target, remote-thread, value);
      name
    end if;
  end
end method;


///// NEXT-EVALUATOR-THREAD-NAME (Internal Function)
//    Returns a unique name for a thread to be created in the application
//    by the environment.

define method next-evaluator-thread-name
    (application :: <dfmc-application>) => (name :: <byte-string>)
  let str = format-to-string("Interactive Thread %d",
                             application.evaluator-thread-counter);
  application.evaluator-thread-counter := 
    application.evaluator-thread-counter + 1;
  str;
end method;


//  REQUEST-EVALUATOR-THREAD (Internal Function)
//  Asks the debugger manager to create a new thread in the application.
//  These will be created on the reserved Thread Manager thread by
//  default, and if a name is not supplied it will be automatically
//  generated.

define method request-evaluator-thread
    (application :: <dfmc-application>,
     #key name :: <byte-string> = next-evaluator-thread-name(application),
          thread :: <remote-thread> = application.dylan-thread-manager)
 => (thread :: <remote-thread>)
  debugger-message("request-evaluator-thread %=", name);
  let target = application.application-target-app;
  let success? =
    spawn-interactive-thread(target, name, thread: thread);
  let stop-reason :: <stop-reason> =
    interactive-thread-break-event-handler(application);
  install-evaluator-thread
    (application,
     stop-reason.stop-reason-thread, name);
  stop-reason.stop-reason-thread
end method;


///// INSTALL-EVALUATOR-THREAD (Internal Function)
//    The debugger-manager has sent notification that a requested
//    interactive thread has been created, and has initialized. The
//    application server reacts by calling this function.

define method install-evaluator-thread
    (application :: <dfmc-application>, thread :: <remote-thread>, 
     name :: <byte-string>) => ()
  debugger-message("install-evaluator-thread %= %=", name, thread);
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let state-model = thread-state-model(application, thread);
  suspend-thread(path, thread);
  state-model.thread-spawned-by-environment? := #t;
  thread-permanently-suspended?(path, thread) := #t;
  application-proxy-primitive-name(application, thread);
  values()
end method;


///// APPLICATION-DEFAULT-INTERACTOR-THREAD (Environment Protocol Method)
//    Returns a <thread-object> that can be used to execute some code
//    interactively.


define method application-default-interactor-thread
    (application :: <dfmc-application>)
  => (thread-or-bust :: false-or(<thread-object>))
  application-open-interactor-thread(application);
end method;


// Not currently in use; superseded by application-open-interactor-thread

define method application-available-interactor-thread
    (application :: <dfmc-application>)
  => (thread-or-bust :: false-or(<thread-object>))
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  with-debugger-transaction (target)
    block (return)
      do-threads(method (t :: <remote-thread>) => ()
		   if (thread-available-for-interaction?(target, t))
		     return(make-environment-object
			      (<thread-object>,
			       project: application.server-project,
			       application-object-proxy: t))
		   end if;
		 end method,
		 path);
    end block
  end
end method;

ignore(application-available-interactor-thread);


// The Environment tries to guarantee interaction in an application
// as follows.
// 
// It tries to use only explicitly spawned interactive threads as
// these are known to be in a sound state.
// 
// If it doesn't find one, it just spawns a new one in the application
// space, and uses that one
// 
// This is only currently hooked up to the Editor, as the Interactor
// itself is only currently thread-local (interactions pertain only
// to the currently selected thread)
// 
// Nosa   Mar 31, 1999



define method application-open-interactor-thread
    (application :: <dfmc-application>)
  => (thread :: <thread-object>)
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let interactive-threads? = application.dylan-thread-manager & #t;
  let thread :: <thread-object>
    = with-debugger-transaction (target)
	block (return)
	  do-threads(method (t :: <remote-thread>) => ()
		       unless (reserved-interactive-thread?
				 (application, t))
			 if (thread-permanently-suspended?(path, t) |
			     (~interactive-threads? &
				thread-available-for-interaction?(target, t)))
			   application-proxy-primitive-name(application, t);
			   let thread
			     = make-environment-object
			         (<thread-object>,
				  project: application.server-project,
				  application-object-proxy: t);
			   return(thread);
			 end if;
		       end unless;
		     end method,
		     path);

	  if (interactive-threads?)
	    // If we fall through to here, spawn a new thread
	    debugger-message("application-open-interactor-thread spawning new thread");
	    create-application-thread(application, "")
	  else
	    // If we fall through to here, just use any thread.
	    let remote-threads
	      = key-sequence(application.application-thread-state-model);
	    make-environment-object
	      (<thread-object>,
	       project: application.server-project,
	       application-object-proxy: remote-threads[0]);
	  end;
	end
      end;
  debugger-message("application-open-interactor-thread chose Thread %=",
		   thread.application-object-proxy);
  thread
end method;


// Take a thread out of permanent suspension in order to do an
// interaction on it

define method resume-evaluator-thread
    (application :: <dfmc-application>, thread :: <remote-thread>) => ()

  let target = application.application-target-app;
  let path = target.debug-target-access-path;

  if (reserved-interactive-thread?(application, thread))
    error("Permission denied: This is a reserved application thread");
  end if;

  if (thread-permanently-suspended?(path, thread))
    debugger-message("Resuming environment interactive thread %=", thread);
    thread-permanently-suspended?(path, thread) := #f;
  end if;

end method;


// Restore a thread previously taken out of permanent suspension
// only when we know its interaction has returned, and we are back
// to the same original state

define method suspend-evaluator-thread
    (application :: <dfmc-application>, thread :: <remote-thread>,
     trans-id :: <interactor-return-breakpoint>) => ()

  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let thread-trans-id = thread-state-transaction(application, thread);

  if (thread-trans-id & (thread-trans-id == trans-id))
    debugger-message("Suspending environment interactive thread %=", thread);
    thread-permanently-suspended?(path, thread) := #t;
  end if;

end method;


// Reserved interactive threads spawned for specific purposes

define method reserved-interactive-thread?
    (application :: <dfmc-application>, thread :: <remote-thread>)
 => (reserved? :: <boolean>)
  select (thread)
    
    application.dylan-thread-manager,
    application.application-target-app.target-spy-thread
      => #t;

    otherwise => #f;

  end;
end method;


define method initialize-interactive-threads
    (application :: <dfmc-application>, thread :: <remote-thread>)
  => ()
  // Spawn these three interactive threads after
  // library initialization is complete

  // The Thread Manager is explicitly reserved for spawning
  // new application threads by running a particular deemed
  // safe spy function

  application.dylan-thread-manager :=
  request-evaluator-thread(application,
			   name: "Thread Manager",
			   thread: thread);

  // The Spy Thread is explicitly reserved for running 
  // Debugger Manager spy calls in the application space

  let spy-thread =
    request-evaluator-thread(application,
			     name: "Spy Thread",
			     thread: thread);

  // Register this thread as a reserved Spy Thread in the debug-target

  use-thread-for-spy-functions
    (application.application-target-app,
     spy-thread,
     reserve?: #t);

  // In addition, a regular interactive thread is spawned at 
  // the same time

  request-evaluator-thread(application,
			   thread: thread);

  application.application-initialized-interactive-threads? := #t;
end method;
