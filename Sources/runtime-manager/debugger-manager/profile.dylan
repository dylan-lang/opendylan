Module:    dm-internals
Synopsis:  Profiler Manager API implementation
Author:    Andy Armstrong, Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/////////////////////////////////////////////////////////////////////////
// The exported types
/////////////////////////////////////////////////////////////////////////

define constant $default-profiling-interval = 50;
define constant $default-snapshot-limit     = #f;
define constant <instruction-pointers>      = limited(<vector>, of: <remote-value>);

define class <profile-state> (<object>)
  slot profiling? :: <boolean> = #f;
  slot class-profiling? :: <boolean> = #f;
  slot profile-interval :: <integer> = $default-profiling-interval;
  slot profile-breakpoints :: <sequence> = #[];

  // Snapshot limiting
  slot profile-snapshot-count :: <integer> = 0;
  slot profile-snapshot-limit :: false-or(<integer>) = $default-snapshot-limit;

  // profile-stack-depth is the maximum depth to which a thread's
  // stack frames should be traced when taking a snapshot (#f
  // means no limit.
  slot profile-stack-depth :: false-or(<integer>) = #f;

  // profile-threads is a group of <remote-thread> objects which
  // indicates which threads snapshots should be taken for (#f
  // means all active threads at the time of the snapshot).
  slot profile-threads :: false-or(<sequence>) = #f;

  // profile-last-cpu-time-table maps threads to the last
  // CPU time seen on that thread.
  constant slot profile-last-cpu-time-table :: <table> = make(<table>);

  // slot remembering the offset of the allocation counter in the
  // application, so that it can be accessed to determine new allocation.
  slot %allocation-counter-offset :: false-or(<integer>) = #f;

  slot profile-data :: false-or(<application-profile>) = #f;
end class <profile-state>;

define sealed domain make (subclass(<profile-state>));
define sealed domain initialize (<profile-state>);

define inline function ensure-profile-data
    (profile-state :: <profile-state>) => (profile :: <application-profile>)
  profile-state.profile-data
    | error("Trying to process profiling data when non available")
end function ensure-profile-data;

define class <application-profile> (<object>)
  slot application-snapshot-skip :: <integer> = 0;
  slot application-last-wall-time :: <integer> = 0;
  slot application-last-page-faults :: <integer> = 0;
  constant slot application-snapshots :: <stretchy-object-vector>
    = make(<stretchy-object-vector>),
    init-keyword: application-snapshots:;
  constant slot application-profile-threads :: <stretchy-object-vector>
    = make(<stretchy-object-vector>),
    init-keyword: profile-threads:;
end class <application-profile>;

define sealed domain make (subclass(<application-profile>));
define sealed domain initialize (<application-profile>);

define class <application-snapshot> (<object>)
  constant slot wall-time-increment :: <integer>,
    required-init-keyword: wall-time-increment:;
  constant slot page-faults-increment :: <integer>,
    required-init-keyword: page-faults-increment:;
  constant slot thread-snapshots :: <sequence>,
    required-init-keyword: thread-snapshots:;
end class <application-snapshot>;

define sealed domain make (subclass(<application-snapshot>));
define sealed domain initialize (<application-snapshot>);

define class <thread-snapshot> (<object>)
  constant slot profile-thread :: <remote-thread>,
    required-init-keyword: thread:;
  constant slot cpu-time-increment :: <integer>,
    required-init-keyword: cpu-time-increment:;
  constant slot allocation-increment :: <integer>,
    required-init-keyword: allocation-increment:;
  constant slot allocated-class :: false-or(<remote-value>),
    required-init-keyword: allocated-class:;
  constant slot instruction-pointers :: <instruction-pointers>,
    required-init-keyword: instruction-pointers:;
end class <thread-snapshot>;

define sealed domain make (subclass(<thread-snapshot>));
define sealed domain initialize (<thread-snapshot>);

define method application-thread-snapshot
    (snapshot :: <application-snapshot>, thread :: <remote-thread>)
 => (thread-snapshot :: false-or(<thread-snapshot>))
  block (return)
    for (snapshot :: <thread-snapshot> in snapshot.thread-snapshots)
      if (snapshot.profile-thread == thread)
	return(snapshot)
      end
    end
  end
end method application-thread-snapshot;

//////
//  Snapshot the application.
//

define method take-application-snapshot
    (application :: <debug-target>,
     #key snapshots-function :: false-or(<function>))
 => ()
  let path             = application.debug-target-access-path;
  let profile-state    = application.application-profile-state;
  let profile          = profile-state.ensure-profile-data;
  let snapshot-count   = profile-state.profile-snapshot-count;
  if (snapshot-count == 0)
    let last-wall-time   = profile.application-last-wall-time;
    let last-page-faults = profile.application-last-page-faults;
    let wall-time        = get-process-wall-clock-time(path);
    let page-faults      = get-process-page-fault-count(path);
    profile.application-last-wall-time   := wall-time;
    profile.application-last-page-faults := page-faults;
    let snapshots
      = if (snapshots-function)
	  snapshots-function(application)
	else
	  let snapshots :: <stretchy-object-vector>
	    = make(<stretchy-object-vector>);
	  do-profile-threads
	    (method (thread :: <remote-thread>)
	       let snapshot = take-thread-snapshot(application, thread);
	       add!(snapshots, snapshot)
	     end,
	     application);
	  snapshots
	end;
    let (wall-time-increment, page-faults-increment)
      = if (empty?(profile.application-snapshots))
	  values(0, 0)
	else
	  values(wall-time   - last-wall-time,
		 page-faults - last-page-faults)
	end;
    let snapshot
      = make(<application-snapshot>,
	     thread-snapshots:      snapshots,
	     wall-time-increment:   wall-time-increment,
	     page-faults-increment: page-faults-increment);

    let snapshot-limit = profile-state.profile-snapshot-limit;
    let snapshots = profile.application-snapshots;
    add!(snapshots, snapshot);
    if (snapshot-limit & snapshots.size > snapshot-limit)
      prune-application-snapshots(application, snapshots)
    end;
    let snapshot-skip  = profile.application-snapshot-skip;
    profile-state.profile-snapshot-count := snapshot-skip
  else
    profile-state.profile-snapshot-count := snapshot-count - 1
  end;
end method take-application-snapshot;

define method take-application-single-thread-snapshot
    (application :: <debug-target>, thread :: <remote-thread>,
     #key allocation :: false-or(<integer>) = #f,
          class :: false-or(<remote-value>) = #f)
 => ()
  local method take-single-thread-snapshot
	    (application :: <debug-target>)
	 => (snapshots :: <sequence>)
	  let snapshot
	    = take-thread-snapshot(application, thread,
				   allocation: allocation,
				   class: class);
	  vector(snapshot)
	end method take-single-thread-snapshot;
  take-application-snapshot
    (application, 
     snapshots-function: take-single-thread-snapshot)
end method take-application-single-thread-snapshot;

define method take-thread-snapshot
    (application :: <debug-target>, thread :: <remote-thread>,
     #key allocation :: false-or(<integer>) = #f,
          class :: false-or(<remote-value>) = #f)
 => (thread-snapshot :: <thread-snapshot>)
  let profile-state = application.application-profile-state;
  let profile = profile-state.ensure-profile-data;
  let path = application.debug-target-access-path;

  add-new!(profile.application-profile-threads, thread);

  // Find the number of frames on the stack so we can allocate a simple
  // object vector big enough to hold the instruction addresses from
  // the frames.
  let number-of-stack-frames :: <integer>
    = number-of-frames-on-stack(path, thread);
  if (number-of-stack-frames = 0) number-of-stack-frames := 1 end;

  // Limit the depth to which the stack is traced if required.
  let depth = profile-state.profile-stack-depth;
  if (depth & depth < number-of-stack-frames)
      number-of-stack-frames := depth;
  end;

  // Step through the stack frames up to the maximum depth, collecting
  // the instruction pointer for each frame.
  let ips :: <instruction-pointers>
    = make(<instruction-pointers>, size: number-of-stack-frames);
  let stack-frame = initialize-stack-trace(path, thread);
  for (index from 0 below number-of-stack-frames)
    ips[index] := frame-instruction-address(path, stack-frame);
    stack-frame := previous-frame(path, stack-frame);
  end;

  // Measure the CPU time increment
  let last-cpu-time-table = profile-state.profile-last-cpu-time-table;
  let last-cpu-time       = element(last-cpu-time-table, thread, default: #f);
  let cpu-time            = get-thread-cpu-time(path, thread);
  let first-snapshot?     = last-cpu-time == #f;
  let cpu-time-increment
    = if (first-snapshot?)
	0
      else
	cpu-time - last-cpu-time
      end;
  last-cpu-time-table[thread] := cpu-time;

  // Measure the allocation increment
  let allocation-increment
    = if (first-snapshot?)
	allocation | 0
      else
	allocation | get-thread-new-allocation(application, thread)
      end;

  // Create a snapshot
  make(<thread-snapshot>,
       thread:                    thread,
       cpu-time-increment:        cpu-time-increment,
       allocated-class:           class,
       allocation-increment:      allocation-increment,
       instruction-pointers:      ips)
end method take-thread-snapshot;

define method prune-application-snapshots
    (application :: <debug-target>, snapshots :: <stretchy-object-vector>)
 => ()
  let profile-state    = application.application-profile-state;
  let profile          = profile-state.ensure-profile-data;
  let snapshot-skip    = profile.application-snapshot-skip;
  let snapshot-count   = profile-state.profile-snapshot-count;
  let old-size = snapshots.size;
  let new-size = floor/(old-size, 2);
  for (new-index :: <integer> from 0 below new-size)
    let old-index       = new-index * 2;
    let first-snapshot  = snapshots[old-index];
    let second-snapshot = snapshots[old-index + 1];
    let wall-time-increment
      = first-snapshot.wall-time-increment
          + second-snapshot.wall-time-increment;
    let page-faults-increment
      = first-snapshot.page-faults-increment
          + second-snapshot.page-faults-increment;
    let thread-snapshots = make(<stretchy-object-vector>);
    //---*** Fill this in!
    error("Application pruning not implemented yet!");
    let new-snapshot
      = make(<application-snapshot>,
	     thread-snapshots:      thread-snapshots,
	     wall-time-increment:   wall-time-increment,
	     page-faults-increment: page-faults-increment);
    snapshots[new-index] := new-snapshot
  end;
  snapshots.size := new-size;
  profile.application-snapshot-skip  := ((snapshot-skip + 1) * 2) - 1
end method prune-application-snapshots;

define method stop-profiling-thread
    (application :: <debug-target>, thread :: <remote-thread>) => ()
  let profile-state = application.application-profile-state;
  let threads = profile-state.profile-threads;
  if (threads & member?(thread, threads))
    profile-state.profile-threads := remove(threads, thread)
  end
end method stop-profiling-thread;

define method allocation-counter-offset
    (application :: <debug-target>) => (offset :: <integer>)
  let profile-state = application.application-profile-state;
  profile-state.%allocation-counter-offset
    | begin
	let path = application.debug-target-access-path;
	let offset-sym
	  = find-symbol(path, "teb_allocation_counter_offset",
			library: application.application-dylan-runtime-library);
	let offset
	  = if (offset-sym)
	      let value = read-value(path, offset-sym.remote-symbol-address);
	      as-signed-integer(value)
	    else 
	      cerror("Carry on using zero as the counter offset",
		     "Profiler internal error: Cannot find the essential runtime "
		       "variable teb_allocation_counter_offset");
	      0
	    end;
	profile-state.%allocation-counter-offset := offset
      end
end method allocation-counter-offset;

define method get-thread-new-allocation
    (application :: <debug-target>, thread :: <remote-thread>)
 => (allocation :: <integer>)
  let path = application.debug-target-access-path;
  let offset = application.allocation-counter-offset;
  block ()
    let thread-teb = dylan-thread-environment-block-address(path, thread);
    let counter-address = byte-indexed-remote-value(thread-teb, offset);
    let val = read-value(path, counter-address);
    write-value(path, counter-address, as-remote-value(0));
    as-integer(val);
  exception (<remote-access-violation-error>)
    0
  end;
end method get-thread-new-allocation;

////////
// Call a function on each <remote-thread> object that represents a
// thread the client is interested in profiling.
//
define function do-profile-threads
    (f :: <function>, application :: <debug-target>) => ()
  let profile-state = application.application-profile-state;
  let threads = profile-state.profile-threads;
  local method do-thread
	    (thread :: <remote-thread>)
	  unless (thread.thread-suspended?)
	    f(thread)
	  end
	end method do-thread;
  if (threads)
    do(do-thread, threads)
  else
    do-threads(do-thread, application.debug-target-access-path)
  end;
end function do-profile-threads;


/////////////////////////////////////////////////////////////////////////
// The exported functions
/////////////////////////////////////////////////////////////////////////

define method application-profiling?
    (application :: <debug-target>) => (profiling? :: <boolean>)
  let profile-state = application.application-profile-state;
  profile-state.profiling?
end method application-profiling?;

define method application-profiling-interval
    (application :: <debug-target>) => (interval :: false-or(<integer>))
  let profile-state = application.application-profile-state;
  if (profile-state.profiling?
	& ~profile-state.class-profiling?
	& ~application.application-killed?)
    profile-state.profile-interval
  end
end method application-profiling-interval;

//////
// Turn profiling on
//
define method start-profiling
    (application :: <debug-target>,
     #key reset? :: <boolean> = #t,
          snapshot-limit = unsupplied(),
          interval :: false-or(<integer>) = #f,
          class-profiling? :: <boolean> = #f,
	  stack-depth = unsupplied(),
	  threads = unsupplied())
 => ()
  let profile-state = application.application-profile-state;
  let already-profiling? = profile-state.profiling?;
  control-profiling(application,
		    reset?:           reset?,
		    snapshot-limit:   snapshot-limit,
		    interval:         interval,
		    class-profiling?: class-profiling?,
		    stack-depth:      stack-depth,
		    threads:          threads);
  let breakpoints = profile-state.profile-breakpoints;
  do(curry(register-debug-point, application), breakpoints);
  unless (already-profiling? & ~reset?)
    profile-state.profiling? := #t;
    inform-profiling-started(application.debug-target-access-path)
  end
end method start-profiling;


//////
// Turn profiling off
//
define method stop-profiling
    (application :: <debug-target>) => ();
  let profile-state = application.application-profile-state;
  if (profile-state.profiling?)
    inform-profiling-stopped(application.debug-target-access-path);
    profile-state.profiling? := #f;
  end;
  let breakpoints = profile-state.profile-breakpoints;
  do(curry(deregister-debug-point, application), breakpoints);
  profile-state.profile-breakpoints := #[];
  if (profile-state.class-profiling?)
    disable-class-profiling(application)
  end
end method stop-profiling;


//////
// Return the data collected since the last reset to the client.
//
define method profile-data
    (application :: <debug-target>)
 => (data :: <application-profile>)
  let data = application.application-profile-state.profile-data;
  make(<application-profile>,
       application-snapshots: copy-sequence(data.application-snapshots),
       profile-threads:       copy-sequence(data.application-profile-threads))
end method profile-data;

define method reset-profile-data
    (application :: <debug-target>) => ()
  let profile-state = application.application-profile-state;
  profile-state.profile-data := make(<application-profile>);
end method reset-profile-data;


//////
// Select the data which is collected by the profiler manager
//
define method control-profiling
    (application :: <debug-target>,
     #key reset? :: <boolean> = #f,
          snapshot-limit = unsupplied(),
	  interval :: false-or(<integer>) = #f,
          class-profiling? :: <boolean> = #f,
	  stack-depth = unsupplied(),
	  threads = unsupplied())
 => ()
  assert(~(interval & class-profiling?),
	 "Whoops, interval and class-profiling? specified together!");

  let profile-state = application.application-profile-state;

  if (~profile-state.profile-data | reset?)
    reset-profile-data(application)
  end;

  case
    class-profiling? =>
      if (profile-state.profiling?)
	inform-profiling-stopped(application.debug-target-access-path);
	profile-state.profiling? := #f;
      end;
      unless (enable-class-profiling(application))
	cerror("Continue anyway",
	       "Failed to enable class profiling")
      end;
      debug-message("Class profiling enabled");
    interval =>
      profile-state.profile-interval := interval;
      profile-state.profile-breakpoints := #[];
  end;

  if (supplied?(snapshot-limit))
    profile-state.profile-snapshot-limit := snapshot-limit
  end;
  if (supplied?(stack-depth))
    profile-state.profile-stack-depth := stack-depth;
  end;

  if (supplied?(threads))
    if (threads)
      let new-threads :: <stretchy-vector> = make(<stretchy-vector>);
      do(method(thread) add!(new-threads, thread) end, threads);
      profile-state.profile-threads := new-threads;
    else
      profile-state.profile-threads := #f;
    end;
  end;
end method control-profiling;


/// Class-based allocation

define constant $class-breakpoint-class-offset = 2;
define constant $class-breakpoint-size-offset  = 4;

define method find-dylan-library-symbol
    (application :: <debug-target>, name :: <string>)
 => (symbol :: false-or(<remote-symbol>))
  let path = application.debug-target-access-path;
  let dylan-library = application.application-dylan-library;
  find-symbol(path, name, library: dylan-library)
end method find-dylan-library-symbol;

// Profile breakpoint

define class <profile-breakpoint> (<breakpoint>)
end class <profile-breakpoint>;

define sealed domain make (subclass(<profile-breakpoint>));
define sealed domain initialize (<profile-breakpoint>);

define method handle-debug-point-event
    (application :: <debug-target>, breakpoint :: <profile-breakpoint>,
     thread :: <remote-thread>)
 => (stop? :: <boolean>)
  let profile-state = application.application-profile-state;
  let threads = profile-state.profile-threads;
  if (~threads | member?(thread, threads))
    let path = application.debug-target-access-path;
    let size-address 
      = calculate-stack-address(path, thread, $class-breakpoint-size-offset);
    let class-address
      = calculate-stack-address(path, thread, $class-breakpoint-class-offset);
    let (allocation, class)
      = block ()
	  values(as-integer(read-value(path, size-address)),
		 read-value(path, class-address))
	exception (<remote-access-violation-error>)
	  0
	end;
    take-application-single-thread-snapshot
      (application, thread, 
       allocation: allocation,
       class:      class)
  end;
  #f;
end method handle-debug-point-event;

// Activate class profiling

define method enable-class-profiling
    (application :: <debug-target>) => (enabled? :: <boolean>)
  let symbol = application.class-breakpoint-primitive.runtime-symbol;
  if (symbol)
    let profile-state = application.application-profile-state;
    runtime-class-profiling-enabled?(application) := #t;
    profile-state.class-profiling? := #t;
    profile-state.profile-breakpoints
      := vector(make(<profile-breakpoint>,
		     address:  symbol.remote-symbol-address,
		     callback: always(#f)));
    #t
  else
    debug-message("Failed to start class profiling")
  end
end method enable-class-profiling;

define method disable-class-profiling
    (application :: <debug-target>) => ()
  let profile-state = application.application-profile-state;
  runtime-class-profiling-enabled?(application) := #f;
  profile-state.class-profiling? := #f;
end method disable-class-profiling;

define method runtime-class-profiling-enabled?-setter
    (enabled? :: <boolean>, application :: <debug-target>)
 => (enabled? :: <boolean>)
  let variable-sym
    = find-dylan-library-symbol
        (application,
	 mangle-in-context
	   ("*class-profiling-enabled?*", $dylan-extensions));
  let (boolean-true, boolean-false)
    = dylan-runtime-boolean-markers(application);
  if (variable-sym)
    let profile-state = application.application-profile-state;
    let path = application.debug-target-access-path;
    let value = if (enabled?) boolean-true else boolean-false end;
    write-value(path, variable-sym.remote-symbol-address, value)
  end;
  enabled?
end method runtime-class-profiling-enabled?-setter;


// Termination of application allocation profiling may need to wait until
// a class-breakpoint has been cleared

define class <stop-profiling-return-breakpoint> (<interactor-return-breakpoint>)
end class;


define method handle-debug-point-event
    (application :: <debug-target>, breakpoint :: <stop-profiling-return-breakpoint>,
     thread :: <remote-thread>)
 => (stop? :: <boolean>)
  let result = next-method();
  stop-profiling(application);
  result
end method handle-debug-point-event;


define constant C-interactor =
  curry(C-setup-interactor, <interactor-return-breakpoint>);

define constant stop-profiling-after-interaction =
  curry(C-setup-interactor, <stop-profiling-return-breakpoint>);


// Handling of class breakpoints in multi-threaded applications requires
// that a breakpoint-pending primitive be called as a spy on an interactive thread
// immediately; then the set/clear breakpoint primitives will be run as regular 
// interactions when the application continues; this is to enable synchronization 
// with regular application threads that may already be in the allocation breakpointing code.

// Set a remote class breakpoint

define method set-application-class-breakpoint
    (application :: <debug-target>, thread :: <remote-thread>,
     class :: false-or(<remote-value>))
 => (transaction)
  let object-class? = 
    // If the class is #f or <object>, pass argument '1' to
    // the runtime -- all classes will break to debugger
    if (class)
      let object-class =
	lookup-static-object(application, "<object>", "dylan");
      class = object-class
    else #t
    end;

  debugger-message("Setting class breakpoint in stopped application");
  run-spy-on-thread(application,
		    thread,
		    application.C-spy.primitive-class-breakpoint-pending);
  C-interactor(application,
	       thread,
	       application.C-spy.primitive-set-class-breakpoint,
	       if (object-class?) as-remote-value(1)
	       else class end,
	       as-remote-value(5))
end method set-application-class-breakpoint;

// Clear a remote class breakpoint

define method clear-application-class-breakpoint
    (application :: <debug-target>, thread :: <remote-thread>,
     class :: false-or(<remote-value>),
     #key stop-profile?)
 => (transaction)
  let object-class? = 
    // If the class is #f or <object>, pass argument '1' to
    // the runtime -- all classes will break to debugger
    if (class)
      let object-class =
	lookup-static-object(application, "<object>", "dylan");
      class = object-class
    else #t
    end;
  let invoker =
    case
      stop-profile? => stop-profiling-after-interaction;
      otherwise => C-interactor;
    end;
  debugger-message("Clearing class breakpoint in stopped application");
  run-spy-on-thread(application,
		    thread,
		    application.C-spy.primitive-class-breakpoint-pending);
  invoker(application,
	  thread,
	  application.C-spy.primitive-clear-class-breakpoint,
	  if (object-class?) as-remote-value(1)
	  else class end)
end method clear-application-class-breakpoint;

// Clear all remote class breakpoints

define method clear-application-class-breakpoints
    (application :: <debug-target>, thread :: <remote-thread>)
 => (transaction)
  debugger-message("Clearing all class breakpoints in stopped application");
  run-spy-on-thread(application,
		    thread,
		    application.C-spy.primitive-class-breakpoint-pending);
  C-interactor(application,
	       thread,
	       application.C-spy.primitive-clear-class-breakpoint,
	       as-remote-value(0))
end method clear-application-class-breakpoints;
