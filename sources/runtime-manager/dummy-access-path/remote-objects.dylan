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

       slot nub-descriptor :: <integer>;

       slot access-path :: <access-path>;

       // other slots?

end class;


///// <REMOTE THREAD>

define abstract class <remote-thread> (<object>)

       slot nub-descriptor :: <simulation-thread>,
            required-init-keyword: nub-descriptor:;

       slot access-path :: <access-path>;

       slot thread-name :: <string>,
            init-keyword: name:;

       slot thread-state :: <string>,
            init-keyword: state:,
            init-value: "[Can't get thread state]";

       slot os-thread-priority :: <integer>,
            init-keyword: os-priority:;

       slot stack :: false-or(<stack-frame>),
            init-value: #f;

       slot thread-suspended? :: <boolean>,
            init-value: #f;

end class;

define variable *next-thread-id* :: <integer> = 0;

define class <simple-remote-thread> (<remote-thread>)
end class;

define method make (class == <remote-thread>, #rest keys, #key, #all-keys)
  apply (make, <simple-remote-thread>, keys);
end method;


///// <REMOTE LIBRARY>

define abstract class <remote-library> (<object>)

       slot nub-descriptor :: <integer>,
            init-keyword: nub-descriptor:;

       slot access-path :: <access-path>;

       slot library-version :: <string>,
            init-keyword: version:,
            init-value: "Version Unknown";

       slot library-image-name :: <string>,
            init-keyword: locator:;

       slot library-core-name :: <string>,
            init-value: "pants";

       slot static-symbols :: <vector>,
            init-value: #[];

       slot exported-symbols :: <vector>,
            init-value: #[];

       slot global-symbols :: <vector>,
            init-value: #[];

end class;

define class <simple-remote-library> (<remote-library>)
end class;

define method make (class == <remote-library>, #rest keys, #key, #all-keys)
  apply (make, <simple-remote-library>, keys);
end method;

define variable *the-only-library*
  = make (<remote-library>,
          nub-descriptor: 0,
          version: "Version Unknown",
          locator: "pants.exe");

          
///// LOCAL FUNCTIONS.

define generic connection-all-threads 
  (conn :: <access-connection>)
    => (_ :: <vector>);

define generic connection-all-libraries 
  (conn :: <access-connection>)
    => (_ :: <vector>);

define generic application-all-threads 
  (ap :: <access-path>)
     => (_ :: <vector>);

define generic update-access-path-libraries 
  (ap :: <access-path>)
     => ();


///// GENERIC FUNCTIONS


define generic host-machine () => (_ :: <debugger-connection>);

define generic do-processes 
  (function :: <function>, dc :: <debugger-connection>) => ();

define generic do-threads (function :: <function>, ap :: <access-path>) => ();

define generic do-libraries 
  (function :: <function>, ap :: <access-path>) => ();

define generic thread-priority 
  (t :: <remote-thread>, #key normalize? = #t) 
    => (_ :: <number>);


///// HOST-MACHINE (is just a hack for now)

define method host-machine () => (_ :: <debugger-connection>);
  *default-local-debugger-connection*;
end method;


///// DO-PROCESSES (is just a hack for now)

define method do-processes 
  (function :: <function>, dc :: <debugger-connection>) => ()
end method;


///// DEBUGGABLE?
//    Just a hack for now...

define method debuggable? (p :: <remote-process>) => (_ :: <boolean>)
  #f
end method;


///// THREAD-PRIORITY

define method thread-priority 
  (t :: <remote-thread>, #key normalize? = #t) 
     => (_ :: <number>)
  t.os-thread-priority;
end method;


///// DO-THREADS

define method do-threads (function :: <function>, ap :: <access-path>) => ()
  // Iterate over the vector with the supplied function.
  for (this-thread in ap.threads)
    function(this-thread)
  end for;
end method;


///// DO-LIBRARIES

define method do-libraries 
    (function :: <function>, ap :: <access-path>) => ()
  function (*the-only-library*);
end method;


///// CONSTRUCT-THREAD-OBJECT
//    Given a low-level thread descriptor (a <NUBTHREAD>), use it to pull
//    across all thread information from the debugger nub and construct
//    a high-level <remote-thread> object.

define method construct-thread-object 
   (conn :: <local-access-connection-32>, thread :: <simulation-thread>)
      => (_ :: <remote-thread>)

  let thread-name = format-to-string ("DBGTHREAD%d", *next-thread-id*);
  let priority :: <integer>
    = 0;

  *next-thread-id* := *next-thread-id* + 1;
   make (<remote-thread>,
         name: thread-name,
         os-priority: priority,
         nub-descriptor: thread);

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
  (ap :: <access-path>, thread :: <simulation-thread>)
     => (_ :: <remote-thread>)
  let i = 0;
  let remote-thread = #f;
  while ((~remote-thread) & (i < size(ap.threads)))
    if (ap.threads[i].nub-descriptor == thread)
      remote-thread := ap.threads[i];
    else
      i := i + 1;
    end if
  end while;
  if (~remote-thread)
    remote-thread := construct-thread-object (ap.connection, thread);
    ap.threads := add! (ap.threads, remote-thread);
  end if;
  remote-thread;
end method;


///// CONSTRUCT-LIBRARY-OBJECT
//    Similar to CONSTRUCT-THREAD-OBJECT.

define method construct-library-object 
  (conn :: <local-access-connection-32>, lib :: <integer>)
    => (_ :: <remote-library>)
  make (<remote-library>,
        nub-descriptor: 0,
        locator: "pants.exe");
end method;


///// FIND-OR-MAKE-LIBRARY
//    Similar to FIND-OR-MAKE-THREAD

define method find-or-make-library 
  (ap :: <access-path>, lib :: <integer>)
     => (_ :: <remote-library>)
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


///// SUSPEND-ALL-EXCEPT
//    A useful function for suspending all threads in the application
//    except for one.

define method suspend-all-except
  (ap :: <access-path>, thr :: <remote-thread>)
    => ()
  do-threads (ap,
              method (t :: <remote-thread>)
                unless (t == thr)
                  suspend-thread (ap, t)
                end unless
              end method);
end method;


///// RESUME-ALL-EXCEPT
//    Undoes the work of suspend-all-except

define method resume-all-except
  (ap :: <access-path>, thr :: <remote-thread>)
    => ()
  do-threads (ap,
              method (t :: <remote-thread>)
                unless (t == thr)
                  resume-thread (ap, t)
                end unless
              end method);
end method;

