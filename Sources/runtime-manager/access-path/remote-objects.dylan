module:      access-path-implementation
synopsis:    Modelling remote objects
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// <REMOTE PROCESS>

define abstract class <remote-process> (<object>)
   constant slot nub-descriptor :: <NUBPROCESS>,
     init-keyword: nub-descriptor:;
   constant slot remote-process-name :: <string>,
     init-value: "{anonymous process}",
     init-keyword: remote-process-name:;
   constant slot remote-process-system-identifier :: <string>,
     init-value: "0",
     init-keyword: remote-process-system-identifier:;
   constant slot remote-process-actual-identifier :: <abstract-integer>,
     init-keyword: remote-process-actual-identifier:;
end class;


define class <simple-remote-process> (<remote-process>)
end class;

define method make (class == <remote-process>, #rest keys, #key, #all-keys)
     => (proc)
  apply (make, <simple-remote-process>, keys);
end method;


///// <REMOTE THREAD>

define abstract class <remote-thread> (<object>)

       constant slot nub-descriptor :: <NUBTHREAD>,
            required-init-keyword: nub-descriptor:;

       constant slot rnub-descriptor :: <abstract-integer>,
            required-init-keyword: rnub-descriptor:;

       constant slot thread-access-path :: <access-path>,
            required-init-keyword: access-path:;

       constant slot thread-name :: <string>,
            init-keyword: name:;

       slot thread-state :: <string>,
            init-keyword: state:,
            init-value: "[Can't get thread state]";

       constant slot os-thread-priority :: <integer>,
            init-keyword: os-priority:;

       slot thread-stack :: false-or(<stack-frame>),
            init-value: #f;

       slot thread-suspended? :: <boolean>,
            init-value: #f;

       slot stack-size-valid? :: <boolean>,
            init-value: #f;

       slot stack-size :: <integer>,
            init-value: 0;

       slot stack-trace-valid? :: <boolean>,
            init-value: #f;

       slot source-stepping-control-applied? :: <boolean>,
            init-value: #f;

end class;


define variable *next-thread-id* :: <integer> = 0;

define class <simple-remote-thread> (<remote-thread>)
end class;

define method make (class == <remote-thread>, #rest keys, #key, #all-keys)
     => (thread)
  apply (make, <simple-remote-thread>, keys);
end method;

define method print-object
  (t :: <remote-thread>, stream :: <stream>) => ()
  format(stream, "{Remote Thread [%=, %=, %=, %=, %=, %=]}",
	 t.thread-name, t.nub-descriptor, t.thread-state,
	 t.os-thread-priority, t.thread-stack, t.thread-suspended?);
end;


///// <REMOTE LIBRARY>

define abstract class <remote-library> (<object>)

  constant slot nub-descriptor :: <NUBLIBRARY>,
    init-keyword: nub-descriptor:;

  constant slot rnub-descriptor :: <abstract-integer>,
    init-keyword: rnub-descriptor:;

  constant slot library-version-major :: <integer>,
    init-keyword: version-major:,
    init-value: 0;

  constant slot library-version-minor :: <integer>,
    init-keyword: version-minor:,
    init-value: 0;

  constant slot library-image-name :: <string>,
    init-keyword: locator:;

  constant slot library-core-name :: <string>,
    init-keyword: core-name:,
    init-value: "unknown";

  constant slot library-base-address :: <remote-value>,
    init-keyword: base-address:,
    init-value: as-remote-value(0);

  constant slot library-object-files :: <sequence> = make(<stretchy-vector>);

  slot self-contained-component? :: <boolean> = #f;

/*
       slot static-symbols :: <vector>,
            init-value: #[];

       slot exported-symbols :: <vector>,
            init-value: #[];

       slot global-symbols :: <vector>,
            init-value: #[];
*/
end class;

define generic library-version (lib :: <remote-library>)
 => (major-version-number :: <integer>, minor-version-number :: <integer>);

define class <simple-remote-library> (<remote-library>)
end class;

define method make (class == <remote-library>, #rest keys, #key, #all-keys)
     => (lbry)
  apply (make, <simple-remote-library>, keys);
end method;

define method library-version (lib :: <simple-remote-library>)
 => (major-version-number :: <integer>, minor-version-number :: <integer>)
  values(lib.library-version-major, lib.library-version-minor)
end method;


///// GENERIC FUNCTIONS

define open generic do-processes 
  (function :: <function>, dc :: <debugger-connection>) => ();

define generic do-threads (function :: <function>, ap :: <access-path>) => ();

define generic do-libraries 
  (function :: <function>, ap :: <access-path>) => ();

define generic thread-priority 
  (t :: <remote-thread>, #key normalize? = #t) 
    => (p :: <number>);


///// DO-PROCESSES

define method do-processes 
  (function :: <function>, dc :: <local-debugger-connection>) => ()

  local method find-existing (descr :: <NUBPROCESS>) 
             => (p? :: false-or(<remote-process>))
          block (return)
            for (proc in dc.connection-process-list)
              if (proc.nub-descriptor = descr)
                return(proc)
              end if;
            end for;
            return(#f);
          end block;
        end method;

  let proc-count = update-local-process-list();
  let new-list = make(<stretchy-vector>, size: 0);
  for (i from 0 below proc-count)
    let descr = local-process-nub-descriptor(i);
    add!(new-list,
         find-existing(descr)
         | begin
             let nl = local-process-name-length(i);
             let sys-idl = local-process-system-identifier-length(i);
             let nm = make(<byte-string>, size: nl);
             let sys-id = make(<byte-string>, size: sys-idl);
             local-process-name(i, nl, nm);
             local-process-system-identifier(i, sys-idl, sys-id);
             make(<remote-process>,
                  nub-descriptor: descr,
                  remote-process-name: nm,
                  remote-process-system-identifier: sys-id)
           end)
  end for;
  dc.connection-process-list := new-list;
  for (proc in new-list)
    function(proc)
  end for;
  values()
end method;


///// GET-PROCESS-PAGE-FAULT-COUNT

define method get-process-page-fault-count
  (ap :: <access-path>) => (count :: <integer>)
  get-process-page-fault-count-on-connection(ap.connection);
end method;

define open generic get-process-page-fault-count-on-connection
  (conn :: <access-connection>) => (count :: <integer>);

define method get-process-page-fault-count-on-connection
  (conn :: <local-access-connection>) => (count :: <integer>)
  nub-get-process-page-fault-count(conn.connection-process);
end method;


///// THREAD-PRIORITY

define method thread-priority 
  (t :: <remote-thread>, #key normalize? = #t) 
     => (p :: <number>)
  t.os-thread-priority;
end method;


///// DO-THREADS

define method do-threads (function :: <function>, ap :: <access-path>) => ()
  // Iterate over the vector with the supplied function.
  for (this-thread in ap.threads)
    function(this-thread)
  end for;
end method;


///// NUMBER-OF-ACTIVE-THREADS

define method number-of-active-threads
  (ap :: <access-path>) => (count :: <integer>)
     size(ap.threads)
end method;


///// GET-THREAD-CPU-TIME

define method get-thread-cpu-time
  (ap :: <access-path>, thread :: <remote-thread>)
     => (timer :: <integer>)
  get-thread-cpu-time-on-connection(ap.connection, thread);
end method;

define open generic get-thread-cpu-time-on-connection
  (conn :: <access-connection>, thread :: <remote-thread>)
     => (timer :: <integer>);

define method get-thread-cpu-time-on-connection
  (conn :: <local-access-connection>, thread :: <remote-thread>)
     => (timer :: <integer>)
  nub-get-thread-cpu-time(conn.connection-process, thread.nub-descriptor);
end method;


///// DO-LIBRARIES

define method do-libraries 
    (function :: <function>, ap :: <access-path>) => ()
  for (this-library in ap.libraries)
    function (this-library);
  end for;
end method;


///// CONSTRUCT-THREAD-OBJECT
//    Given a low-level thread descriptor (a <NUBTHREAD>), use it to pull
//    across all thread information from the debugger nub and construct
//    a high-level <remote-thread> object.

define open generic construct-thread-object 
   (conn :: <access-connection>, thread :: <NUBTHREAD>,
    #key)
 => (thread :: <remote-thread>);

define method construct-thread-object 
   (conn :: <local-access-connection>, thread :: <NUBTHREAD>,
     #key path, priority)
      => (thread :: <remote-thread>)

  let thread-name = format-to-string ("DBGTHREAD%d", *next-thread-id*);
  let priority :: <integer>
    = priority | nub-thread-os-priority (conn.connection-process, thread);

  *next-thread-id* := *next-thread-id* + 1;
   make (<remote-thread>,
         name: thread-name,
         access-path: path,
         os-priority: priority,
         nub-descriptor: thread,
         rnub-descriptor: as-integer(thread));

end method;


///// FIND-OR-MAKE-THREAD
//    The access path maintains a list of active threads. This list is
//    extended as new threads are created, and shortened when threads are
//    destroyed. (Stop-reason information is used to do this). Every thread
//    in the list has a unique nub-descriptor (a <NUBTHREAD>). This function
//    searches the current state of the list and looks for a thread with the
//    given descriptor. If it is found, it is returned. Otherwise, a new
//    thread is added to the list and returned.

define method find-or-make-thread 
  (ap :: <access-path>, thread :: <NUBTHREAD>,
   #key priority)
     => (thread :: <remote-thread>)
  let i = 0;
  let remote-thread = #f;
  while ((~remote-thread) & (i < size(ap.threads)))
    if (ap.threads[i].nub-descriptor = thread)
      remote-thread := ap.threads[i];
    else
      i := i + 1;
    end if
  end while;
  if (~remote-thread)
    remote-thread :=
      construct-thread-object
      (ap.connection, thread, path: ap, priority: priority);
    ap.threads := add! (ap.threads, remote-thread);
  end if;
  remote-thread;
end method;


///// CONSTRUCT-LIBRARY-OBJECT
//    Similar to CONSTRUCT-THREAD-OBJECT.

define open generic construct-library-object 
  (conn :: <access-connection>, lib :: <NUBLIBRARY>)
    => (lib :: <remote-library>);

define method construct-library-object 
  (conn :: <local-access-connection>, lib :: <NUBLIBRARY>)
    => (lib :: <remote-library>)
  let name-length :: <integer>
    = nub-get-library-filename-length (conn.connection-process, lib);
  let C-filename = make (<byte-string>, size: name-length);
  let basic-name-length =
    nub-get-library-undecorated-name-length(conn.connection-process, lib);
  let basic-name = make(<byte-string>, size: basic-name-length);
  let (major-v, minor-v) =
    nub-get-library-version(conn.connection-process, lib);
  let base-addr =
    nub-get-library-base-address(conn.connection-process, lib);
  nub-get-library-filename (conn.connection-process, lib, name-length, C-filename);
  nub-get-library-undecorated-name(conn.connection-process, lib, name-length, basic-name);
  make (<remote-library>,
        nub-descriptor: lib,
        locator: as-uppercase(C-filename),
        core-name: as-uppercase(basic-name),
        version-major: major-v,
        version-minor: minor-v,
        base-address: base-addr);
end method;


///// FIND-OR-MAKE-LIBRARY
//    Similar to FIND-OR-MAKE-THREAD

define method find-or-make-library 
  (ap :: <access-path>, lib :: <NUBLIBRARY>)
     => (lib :: <remote-library>)
  let i = 0;
  let remote-library = #f;
  while ((~remote-library) & (i < size(ap.libraries)))
    if (ap.libraries[i].nub-descriptor = lib)
      remote-library := ap.libraries[i];
    else
      i := i + 1;
    end if
  end while;
  if (~remote-library)
    remote-library := construct-library-object (ap.connection, lib);
    ap.libraries := add! (ap.libraries, remote-library);
  end if;
  remote-library;
end method;

/*
///// SUSPEND-ALL-EXCEPT
//    A useful function for suspending all threads in the application
//    except for one.

define method suspend-all-except
  (ap :: <access-path>, thr :: <remote-thread>)
    => ()
  do-threads (method (t :: <remote-thread>)
                unless (t == thr)
                  suspend-thread (ap, t)
                end unless
              end method,
              ap);
end method;


///// RESUME-ALL-EXCEPT
//    Undoes the work of suspend-all-except

define method resume-all-except
  (ap :: <access-path>, thr :: <remote-thread>)
    => ()
  do-threads (method (t :: <remote-thread>)
                unless (t == thr)
                  resume-thread (ap, t)
                end unless
              end method,
              ap);
end method;
*/
