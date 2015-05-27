module:      access-path-implementation
synopsis:    Modelling remote objects
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DO-PROCESSES

define sideways method do-processes
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

define method get-process-page-fault-count-on-connection
  (conn :: <local-access-connection>) => (count :: <integer>)
  nub-get-process-page-fault-count(conn.connection-process);
end method;


///// GET-THREAD-CPU-TIME

define method get-thread-cpu-time-on-connection
  (conn :: <local-access-connection>, thread :: <remote-thread>)
     => (timer :: <integer>)
  nub-get-thread-cpu-time(conn.connection-process, thread.nub-descriptor);
end method;


///// CONSTRUCT-THREAD-OBJECT
//    Given a low-level thread descriptor (a <NUBTHREAD>), use it to pull
//    across all thread information from the debugger nub and construct
//    a high-level <remote-thread> object.

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


///// CONSTRUCT-LIBRARY-OBJECT
//    Similar to CONSTRUCT-THREAD-OBJECT.

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
