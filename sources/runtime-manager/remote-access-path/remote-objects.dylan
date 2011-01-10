module:      remote-access-path
synopsis:    Modelling remote objects
author:      Paul Howard, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



///// DO-PROCESSES

define method do-processes 
  (function :: <function>, dc :: <remote-debugger-connection-implementation>) => ()

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

  let proc-count =
    Rtmgr/NubServer/update-local-process-list(dc.connection-server);
  let new-list = make(<stretchy-vector>, size: 0);
  for (i from 0 below proc-count)
    let descr :: <NUBPROCESS> =
      as-remote-pointer(Rtmgr/NubServer/local-process-nub-descriptor
			  (dc.connection-server, i));
    add!(new-list,
         find-existing(descr)
         | begin
             let nm =
	       Rtmgr/NubServer/local-process-name(dc.connection-server, i);
             let sys-id =
	       Rtmgr/NubServer/local-process-system-identifier
               (dc.connection-server, i);
             let actual-id =
	       Rtmgr/NubServer/local-process-actual-identifier
               (dc.connection-server, i);
             make(<remote-process>,
                  nub-descriptor: descr,
                  remote-process-name: nm,
                  remote-process-system-identifier: sys-id,
                  remote-process-actual-identifier: actual-id)
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
  (conn :: <remote-access-connection>) => (count :: <integer>)
  Rtmgr/RemoteNub/get-process-page-fault-count(conn.nub);
end method;


///// GET-THREAD-CPU-TIME

define method get-thread-cpu-time-on-connection
  (conn :: <remote-access-connection>, thread :: <remote-thread>)
     => (timer :: <integer>)
  Rtmgr/RemoteNub/get-thread-cpu-time(conn.nub, thread.rnub-descriptor);
end method;


///// CONSTRUCT-THREAD-OBJECT

define method construct-thread-object 
   (conn :: <remote-access-connection>, thread :: <NUBTHREAD>,
     #key path, priority)
      => (thread :: <remote-thread>)

  let thread-name = format-to-string ("DBGTHREAD%d", *next-thread-id*);
  let rthread = as-integer(thread);
  let priority :: <integer>
    = priority | Rtmgr/RemoteNub/thread-os-priority (conn.nub, rthread);

  *next-thread-id* := *next-thread-id* + 1;
   make (<remote-thread>,
         name: thread-name,
         access-path: path,
         os-priority: priority,
         nub-descriptor: thread,
         rnub-descriptor: rthread);

end method;


///// CONSTRUCT-LIBRARY-OBJECT

define method construct-library-object 
  (conn :: <remote-access-connection>, lib :: <NUBLIBRARY>)
    => (lib :: <remote-library>)
  let rlib = as-integer(lib);
  let (major-v, minor-v) =
    Rtmgr/RemoteNub/get-library-version(conn.nub, rlib);
  let base-addr :: <RTARGET-ADDRESS> =
    Rtmgr/RemoteNub/get-library-base-address(conn.nub, rlib);
  let C-filename =
    Rtmgr/RemoteNub/get-library-filename(conn.nub, rlib);
  let basic-name =
    Rtmgr/RemoteNub/get-library-undecorated-name(conn.nub, rlib);

  make (<remote-library>,
        nub-descriptor: lib,
        rnub-descriptor: rlib,
        locator: as-uppercase(C-filename),
        core-name: as-uppercase(basic-name),
        version-major: major-v,
        version-minor: minor-v,
        base-address: as-remote-value(base-addr));
end method;
