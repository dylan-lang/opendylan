module:       dm-internals
synopsis:     The modelling of application threads
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// Stepping modes.
//    These describe the kinds of source-level stepping that a thread can
//    be performing.

define constant $thread-not-stepping = 0;
define constant $thread-stepping-into = 1;
define constant $thread-stepping-over = 2;
define constant $thread-stepping-out = 3;
define constant $thread-aligning-to-recorded-location = 4;


///// <THREAD-PAUSE-STATE-DESCRIPTION>
//    States that describe a thread when the application is stopped.

define constant <thread-pause-state-description>
  = one-of(#"random-location",
           #"known-location",
           #"interactive-location",
           #"unhandled-condition");


///// Thread Environment Block Offsets
//    These give the offsets, in remote-value-sized units, of various
//    fields in the thread environment block.

define constant $TEB-dynamic-environment-offset           = 0;
define constant $TEB-thread-local-variables-vector-offset = 1;
define constant $TEB-dylan-thread-object-offset           = 2;
define constant $TEB-os-thread-handle-offset              = 3;
define constant $TEB-current-handlers-offset              = 4;
define constant $TEB-mv-count-offset                      = 8;
define constant $TEB-first-mv-offset                      = 9;


///// THREAD-AVAILABLE-FOR-INTERACTION?
//    A convenience accessor. Returns #t if the thread is capable of
//    accepting interactively downloaded code within the current
//    debugger transaction, otherwise returns #f.

define method thread-available-for-interaction?
    (application :: <debug-target>, ap-thread :: <remote-thread>)
  => (well? :: <boolean>)
  let thread = find-thread(application, ap-thread);
  (thread.thread-pause-state-description == #"interactive-location") |
  (thread.thread-pause-state-description == #"unhandled-condition") |
  (thread.thread-pause-state-description == #"known-location")
end method;


///// REMOTE-THREAD-INFORMATION
//    Attempts to determine whether a <remote-thread> describes a thread that
//    was created by dylan, and also whether it is a named dylan thread.

define method remote-thread-information
    (application :: <debug-target>, thread :: <remote-thread>)
       => (dylan-thread? :: <boolean>, 
           remote-thread-name :: <string>,
           dylan-thread-object :: false-or(<remote-value>),
           dylan-thread-handle :: false-or(<remote-value>))
  let path = application.debug-target-access-path;
  let teb = dylan-thread-environment-block-address(path, thread);
  let dylan-thread? = #f;
  let dylan-thread-object = #f;
  let dylan-thread-handle = #f;
  let remote-thread-name = "<Error obtaining thread name>";
  block ()
    dylan-thread-object :=
      read-value(path,
                 indexed-remote-value(teb, $TEB-dylan-thread-object-offset));
    let val-type = classify-dylan-object(application, dylan-thread-object);
    if (val-type == $thread-type)
      dylan-thread? := #t;
      let name-object = dylan-thread-name(application, dylan-thread-object);
      let name-type = classify-dylan-object(application, name-object);
      if (name-type == $string-type)
        remote-thread-name := dylan-string-data(application, name-object);
      else
        remote-thread-name := thread.thread-name;
      end if;
      dylan-thread-handle :=
	read-value(path,
		   indexed-remote-value(teb, $TEB-os-thread-handle-offset));
    else
      dylan-thread? := #f;
      dylan-thread-object := #f;
      dylan-thread-handle := #f;
      remote-thread-name := thread.thread-name;
    end if;
  exception (<remote-access-violation-error>)
    dylan-thread? := #f;
    dylan-thread-object := #f;
    dylan-thread-handle := #f;
    remote-thread-name := thread.thread-name;
  end block;
  debugger-message("remote-thread-information %= : [%=, %=, %=, %=]", thread,
		   dylan-thread?, remote-thread-name,
		   dylan-thread-object, dylan-thread-handle);
  values(dylan-thread?, remote-thread-name, dylan-thread-object, dylan-thread-handle);
end method;


///// THREAD-CURRENT-ACTIVE-HANDLERS
//    Returns an ordered sequence (most recently installed to least
//    recently installed) of handlers active on the thread.

define method thread-current-active-handlers
    (application :: <debug-target>, ap-thread :: <remote-thread>)
  => (handlers :: <sequence>)
  let thread = find-thread(application, ap-thread);
  unless (thread.cached-handlers)
    let path = application.debug-target-access-path;
    let teb = dylan-thread-environment-block-address(path, ap-thread);
    let addr = indexed-remote-value(teb, $TEB-current-handlers-offset);
    let handler-list = read-value(path, addr);
    thread.cached-handlers := 
      canonicalize-sequence(application, handler-list)
  end unless;
  thread.cached-handlers
end method;


///// THREAD-CURRENT-LOCAL-VARIABLES
//    Returns a sequence of the thread-local-variables for a thread.
//    Each member of the sequence is a pair. The head of each pair is
//    a <remote-location> for the variable, and the tail is the value.

define method thread-current-local-variables
    (application :: <debug-target>, ap-thread :: <remote-thread>)
  => (location-value-pairs :: <sequence>)
  let thread = find-thread(application, ap-thread);
  unless (thread.cached-tlv)
    let path = application.debug-target-access-path;
    let teb = dylan-thread-environment-block-address(path, ap-thread);
    let addr = 
      indexed-remote-value(teb, $TEB-thread-local-variables-vector-offset);
    let tlv = read-value(path, addr);
    let tlv-canonical = 
      canonicalize-sequence(application, tlv);
    let addr-first = indexed-remote-value(tlv, 2);
    let pairs = make(<vector>, size: tlv-canonical.size);
    for (i from 0 below tlv-canonical.size)
      pairs[i] := pair(indexed-remote-value(addr-first, i), tlv-canonical[i]);
    end for;
    thread.cached-tlv := pairs;
  end unless;
  thread.cached-tlv
end method;


///// EVALUATE-THREAD-LOCAL-VARIABLE
//    Given the address of a value cell, read the special marker that
//    tells us where to obtain the thread-local value from.

define method evaluate-thread-local-variable
    (application :: <debug-target>, ap-thread :: <remote-thread>,
     value-cell :: <remote-value>)
  => (thread-local-value :: <remote-value>)
  let path :: <access-path> = application.debug-target-access-path;
  let marker :: <remote-value> = read-value(path, value-cell);
  let thread-local-value :: <remote-value> = marker;
  let tlv = thread-current-local-variables(application, ap-thread);
  let tlv-size :: <remote-value> = as-remote-value((tlv.size + 2) * 4);
  let tag = inspect-instance-tag(application, marker);
  if ((tag == $dylan-tag-pointer) & remote-value-<(marker, tlv-size))
    let index = truncate/(as-integer(marker), 4) - 2;
    thread-local-value := tail(tlv[index]);
  end if;
  thread-local-value;
end method;


///// THREAD-LOCAL-VARIABLE?
//    Decides whether a value cell contains a thread-local or
//    thread-global variable.

define method thread-local-variable?
    (application :: <debug-target>, value-cell :: <remote-value>)
  => (well? :: <boolean>)
  let path :: <access-path> = application.debug-target-access-path;
  let marker :: <remote-value> = read-value(path, value-cell);
  let master-thread :: false-or(<remote-thread>) = 
    application.application-primary-thread;
  if (master-thread)
    let tlv = thread-current-local-variables(application, master-thread);
    let tlv-size :: <remote-value> = as-remote-value((tlv.size + 2) * 4);
    let tag = inspect-instance-tag(application, marker);
    (tag == $dylan-tag-pointer) & remote-value-<(marker, tlv-size);
  end if
end method;


///// SET-THREAD-LOCAL-VARIABLE

define method set-thread-local-variable
    (application :: <debug-target>, ap-thread :: <remote-thread>,
     value-cell :: <remote-value>, thread-local-value :: <remote-value>)
  => (thread-local-value :: <remote-value>)
  let path = application.debug-target-access-path;
  let marker = read-value(path, value-cell);
  let tlv = thread-current-local-variables(application, ap-thread);
  let tlv-size = as-remote-value((tlv.size + 2) * 4);
  let tag = inspect-instance-tag(application, marker);
  if ((tag == $dylan-tag-pointer) & remote-value-<(marker, tlv-size))
    let index = truncate/(as-integer(marker), 4) - 2;
    tail(tlv[index]) := thread-local-value;
    write-value(path, head(tlv[index]), thread-local-value);
  end if;
  thread-local-value;
end method;


///// GET-THREAD-INTERACTOR-LEVEL
//    How deeply nested is the interactive evaluation state of the thread?
//    The spy has a dynamic thread-local variable that holds this value.
//    It will always be an integer.

define method get-thread-interactor-level
    (application :: <debug-target>, thread :: <remote-thread>)
  => (interactor-level :: <integer>)
  let interactor-level = 0;
  let (val, value-cell) = 
    resolve-dylan-name(application, "*current-interactor-level*",
                       $dylan-internal, indirect?: #t);
  if (value-cell)
    let remote-integer = 
      evaluate-thread-local-variable(application, thread, value-cell);
    interactor-level := dylan-integer-data(application, remote-integer);
  end if;
  interactor-level;
end method;


///// THREAD-CURRENT-MV-VECTOR
//    Vectors up <remote-value> objects for the thread's multiple
//    values area.

define method thread-current-mv-vector
    (application :: <debug-target>, thread :: <remote-thread>)
       => (vals :: <vector>)
  let dm-thread = find-thread(application, thread);
  if (dm-thread.cached-mv)
    dm-thread.cached-mv
  else
    // First, get a pointer into the thread environment block.

    let path = application.debug-target-access-path;
    let teb = dylan-thread-environment-block-address(path, thread);

    // Index to the mv-count and the mv-vector.

    let mv-count-address = indexed-remote-value(teb, $TEB-mv-count-offset);
    let mv-0-address = indexed-remote-value(teb, $TEB-first-mv-offset);
    let mv-space = #[];

    // Read the MV count. It is a raw integer. Create a vector big enough
    // to hold the multiple values.

    block () 
      let mv-count = as-integer(read-value(path, mv-count-address));

      if ((mv-count < 0) | (mv-count > 64))
        mv-count := 0;
      end if;

      mv-space := make(<vector>, size: mv-count);

      // Loop to read the multiple values, of course!

      for (i from 0 below mv-count)
        let addr-mv = indexed-remote-value(mv-0-address, i);
        mv-space[i] := read-value(path, addr-mv);
      end for;
  
      // Cache it while the thread is stopped.
      dm-thread.cached-mv := mv-space;  

    exception (pants :: <remote-access-violation-error>)
      dm-thread.cached-mv := #[];
    end block;

    // And return the vector.
    mv-space;
  end if
end method;


///// THREAD-SET-MV-VECTOR
//    Installs the supplied vector of <remote-value>s into the thread's
//    MV buffer, and also adjusts the buffer count.

define method thread-set-mv-vector
    (application :: <debug-target>, thread :: <remote-thread>,
     vals :: <sequence>) => ()
  let dm-thread = find-thread(application, thread);
  let path = application.debug-target-access-path;
  let teb = dylan-thread-environment-block-address(path, thread);
  let mv-count-address = indexed-remote-value(teb, $TEB-mv-count-offset);
  let mv-0-address = indexed-remote-value(teb, $TEB-first-mv-offset);
  let remotized-count = as-remote-value(size(vals));
  write-value(path, mv-count-address, remotized-count);
  for (i from 0 below size(vals))
    write-value(path,
                indexed-remote-value(mv-0-address, i),
                vals[i])
  end for;
  dm-thread.cached-mv := vals;
end method;


///// <APPLICATION-THREAD>
//    Describes a thread in the application. This basically adapts the
//    <remote-thread> class provided by the access path, and furnishes
//    it with a load of state to do with modelling its stack. During
//    a debugger transaction, we can safely keep this state valid to
//    save doing pointless re-calculation when walking the stack.

define class <application-thread> (<object>)

  // Encapsulate the actual <remote-thread> as presented to us
  // via the access path.

  constant slot remote-thread-object :: <remote-thread>,
    required-init-keyword: remote-thread:;

  // Cache the multiple values vector, the active handlers, and the
  // vector of local variables.

  slot cached-mv :: false-or(<sequence>),
    init-value: #f;

  slot cached-handlers :: false-or(<sequence>),
    init-value: #f;

  slot cached-tlv :: false-or(<sequence>),
    init-value: #f;

  // Cache the restarts.

  slot cached-restarts :: false-or(<sequence>),
    init-value: #f;

  // Finally, slots to model the stepping behaviour of this thread.
  // That is, whether it is stepping, and where it might be stepping
  // to.

  slot stepping-mode :: <integer>,
    init-value: $thread-not-stepping;

  slot call-frame-vector :: <vector> = #[];

  slot dynamic-environment-vector :: <vector> = #[];

  slot thread-stack-trace-valid? :: <boolean>,
    init-value: #f;

  slot thread-stack-trace-ordered? :: <boolean>,
    init-value: #f;

  slot thread-generic-top-frame :: false-or(<application-stack-frame>),
    init-value: #f;

  constant slot thread-runtime-context :: <table> = make(<table>);

  constant slot thread-registered-history :: <stretchy-vector>
    = make(<stretchy-vector>, size: 0);

  slot thread-pause-state-description :: <thread-pause-state-description>,
    init-value: #"random-location";


end class;


///// TAKE-THREAD-OUT-OF-SOURCE-STEP-MODE
//    Ensures that a thread is no longer considered by the DM to be
//    "halfway through" any stepping operations.

define method take-thread-out-of-source-step-mode
    (application :: <debug-target>, thread :: <remote-thread>) => ()
  let path = application.debug-target-access-path;
  let dm-thread = find-thread(application, thread);
  dm-thread.stepping-mode := $thread-not-stepping;
  remove-all-stepping-control-for-thread(path, thread);
end method;


///// INVALIDATE-THREAD-STATE
//    Called when the application has been in (or is about to enter) the
//    "running" state. This invalidates all modelled stack frames for
//    the thread. However, this function is NOT called if we are only
//    entering the running state for the purposes of calling the spy,
//    since the stack is guaranteed to look the same after such a "safe"
//    call.

define method invalidate-thread-state (thread :: <application-thread>) => ()
  thread.cached-mv := #f;
  thread.cached-handlers := #f;
  thread.cached-tlv := #f;
  thread.cached-restarts := #f;
  thread.thread-stack-trace-valid? := #f;
  thread.thread-stack-trace-ordered? := #f;
  thread.thread-generic-top-frame := #f;
  remove-all-keys!(thread.thread-runtime-context);
  thread.thread-pause-state-description := #"random-location";
end method;


///// CREATE-THREAD-DESCRIPTOR
//    When a <create-process-stop-reason> or <create-thread-stop-reason> is
//    received, this function is called with the <remote-thread> in
//    question, and creates a new entry in the <debug-target>'s table of
//    threads.

define method create-thread-descriptor
    (application :: <debug-target>, thread :: <remote-thread>)
       => ()
  let new-thread 
    = make (<application-thread>, remote-thread: thread);
  application.application-threads[thread] := new-thread;
end method;


///// FIND-THREAD
//    Slightly lousy mapping function. We might want to consider exporting
//    <application-thread> and not allowing the UI to talk about
//    <remote-thread>s at all!

define method find-thread 
    (ap :: <debug-target>, remote-t :: <remote-thread>)
      => (mapped-thread :: <application-thread>) 
  ap.application-threads[remote-t];
end method;


///// ******************* The Thread-Local History ********************
//    The debugger manager allows objects to be remembered on a thread-local
//    basis. This is how the interactor maintains "history variables".
//    There are two APIs exported to add and retrieve objects
//    respectively. The recorded objects need not belong to dylan, but
//    the DM will ensure that dylan objects are remotely registered if
//    necessary. The remote registration aspect is completely transparent
//    to DM clients.
//    Finally, there is a client-callable API to remove all objects
//    recorded in a thread-local history.


///// FLUSH-THREAD-HISTORY (Exported API)
//    Remove all objects from the thread history. This includes freeing up
//    the objects that might have been remotely registered.
//    TODO: Implement.

define method flush-thread-history
    (application :: <debug-target>, thread :: <remote-thread>) => ()
  let dm-thread = find-thread(application, thread);
  // Status: not yet implemented.  
end method;


///// RECORD-OBJECT-IN-THREAD-HISTORY (Exported API)
//    Add an object to the thread-local history, and return the integer
//    index at which the object is placed. Also returns the named
//    history variable, which happens to be a dollar ($) followed by
//    the string-ized history index.

define method record-object-in-thread-history
    (application :: <debug-target>, thread :: <remote-thread>,
     value :: <remote-value>)
  => (history-index :: <integer>, history-name :: <string>)
  let dm-thread = find-thread(application, thread);
  let history-index = dm-thread.thread-registered-history.size;
  let item =
    if (dylan-object?(application, value))
      if (object-requires-registration?(application, value))
        block ()
          register-remote-object(application, value)
        exception (<object-registration-error>)
          // This object needs to be tracked, but can't be for one reason or
          // another.
          // TODO: Signal some kind of warning to clients?
          value
        end block;
      else
        value
      end if;
    else
      value
    end if;
  add!(dm-thread.thread-registered-history, item);
  values(history-index, format-to-string("$%d", history-index));
end method;


///// RETRIEVE-OBJECT-FROM-THREAD-HISTORY (Exported API)
//    Returns the value that was stored in the thread local history at the
//    given index. If the index is not valid (ie, it was never returned from
//    a call to RECORD-OBJECT-IN-THREAD-HISTORY), then the unbound value
//    will be returned.

define method retrieve-object-from-thread-history
    (application :: <debug-target>, thread :: <remote-thread>,
     history-index :: <integer>)
  => (value :: <remote-value>)
  let dm-thread = find-thread(application, thread);
  if ((history-index < 0) | 
      (history-index >= dm-thread.thread-registered-history.size))
    lookup-static-object(application, "%unbound", "internal")
  else
    let item = dm-thread.thread-registered-history[history-index];
    if (instance?(item, <remote-value>))
      item
    else
      // We have a registered <remote-object>, so obtain the
      // current value.
      block()
        remote-object-value(application, item) |
          lookup-static-object(application, "%false", "internal")
      exception (<object-registration-error>)
        lookup-static-object(application, "%false", "internal")
      end block
    end if
  end if
end method;


///// SPAWN-INTERACTIVE-THREAD
//    Creates a new dylan thread in the application. The thread will be
//    assigned the given name, and will be allowed to run to a
//    breakpoint. When this breakpoint is reached, the DM will signal
//    a special stop-reason.

define method spawn-interactive-thread
    (application :: <debug-target>, tname :: <byte-string>,
     #key thread)
  => (success? :: <boolean>)

  let spy-thread = thread | select-thread-for-spy(application);
  let static-block = application.interactive-thread-download-block;
  let path = application.debug-target-access-path;

  local method download-byte-string (str :: <byte-string>)
                    => (address-we-hope :: false-or(<remote-value>))
          let wrapper-field =
            lookup-static-wrapper(application, "<byte-string>", "dylan");
          let size-field =
            integer-as-tagged-remote-value(str.size);
          let word-fields = vector(wrapper-field, size-field);
          let address-of-string = #f;
          let address-of-string-data = #f;
          block-align-n(static-block, 4);
          address-of-string :=
            download-remote-value-vector-into(path, static-block, word-fields);
          address-of-string-data :=
            download-byte-string-into(path, static-block, str);
          if (address-of-string & address-of-string-data)
            address-of-string
          else
            #f
          end if
        end method;

  let success? = #f;

  if (spy-thread & static-block)

    let running-dylan-spy-function? =
      remote-symbol-address
      (lookup-runtime-symbol(application, $running-dylan-spy-function?));

    block ()

    write-value(path, running-dylan-spy-function?, as-remote-value(1));

    debugger-message("spawn-interactive-thread %= on Thread %=", tname, spy-thread);

    let address-of-name = download-byte-string(tname);
    if (address-of-name & 
        run-spy-on-thread(application,
                          spy-thread,
                          application.dylan-spy.spy-create-interactive-thread,
                          address-of-name))
      success? := #t;
    end if;

    cleanup
      write-value(path, running-dylan-spy-function?, as-remote-value(0));
    end;
  end if;
  success?;
end method;


define method suspend-interesting-thread
    (application :: <debug-target>, stop-reason) => ()
end method;

define method suspend-interesting-thread
    (application :: <debug-target>, stop-reason :: <internal-stop-reason>)
  let access-path = application.debug-target-access-path;
  let stopped-thread = stop-reason.stop-reason-thread;

  unless (stopped-thread.thread-suspended?)
    unless (application.application-just-interacted-on-running-thread?)
      debugger-message("Suspending thread %= in response to debug-event",
		       stopped-thread);
      suspend-thread(access-path, stopped-thread);
    end;
  end;
end method;

define method suspend-interesting-thread
    (application :: <debug-target>,
     stop-reason :: <source-code-alignment-stop-reason>) => ()
end method;


define method resume-selected-thread
    (application :: <debug-target>) => (resumed-thread)
  let access-path = application.debug-target-access-path;
  let remote-thread = application.application-selected-thread;
  if (remote-thread)
    unless (thread-permanently-suspended?(access-path, remote-thread))
      if (remote-thread.thread-suspended?)
	unless (application.application-just-interacted-on-running-thread?)
	  debugger-message("Resuming thread %= in response to continuation",
			   remote-thread);
	  dylan-resume-thread(access-path, remote-thread);
	  remote-thread;
	end;
      end;
    end;
  end;
end method;

define method resume-all-suspended-threads
  (application :: <debug-target>)
    => (resumed-threads :: <list>)
  debugger-message("Resuming all threads");

  let access-path = application.debug-target-access-path;

  let threads = #();

  do-threads
  (method(remote-thread :: <remote-thread>)
       unless (thread-permanently-suspended?(access-path, remote-thread))
	 if (remote-thread.thread-suspended?)
	   debugger-message("Resuming thread %= in response to continuation",
			    remote-thread);
	   dylan-resume-thread(access-path, remote-thread);
	   threads := pair(remote-thread, threads);
	 end if;
       end unless
   end method,
   access-path);

  threads
end method;

