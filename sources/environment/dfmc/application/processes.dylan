Module:    dfmc-application
Synopsis:  The implementation of the <PROCESS> class, and related protocols.
Author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// <DEBUG-PROCESS>
//    The concrete implementation for <PROCESS>. Couldn't think of a
//    better name. The corresponding runtime-manager class is
//    <REMOTE-PROCESS>.

define class <debug-process> (<process>)
  constant slot process-implementation-descriptor :: <remote-process>,
    required-init-keyword: descriptor:;
end class;


///// DO-PROCESSES-ON-MACHINE (Environment Procotols)
//    This method applies to any <connected-machine>, whether it is
//    local or remote.

define method do-processes-on-machine 
    (machine :: <connected-machine>, itr :: <function>) => ()
  do-processes
    (method (p :: <remote-process>) => ()
       let env-process = element(machine.process-mappings, p, default: #f);
       unless (env-process)
	 let filename = as(<file-locator>, p.remote-process-name);
         env-process := make(<debug-process>,
                             process-host-machine: machine,
                             process-executable-file: filename,
                             process-id: p.remote-process-system-identifier,
                             process-debuggable?: #t,
                             descriptor: p);
         machine.process-mappings[p] := env-process;
       end unless;
       itr(env-process);
     end method,
     machine.machine-debug-connection);
end method;


///// ATTACH-LIVE-APPLICATION (Environment Protocols)
//    Our method just shares implementation with RUN-APPLICATION.

define method attach-live-application
   (application :: <dfmc-application>, process :: <debug-process>,
    #key client, system-data)
 => (app :: <dfmc-application>)
  ignore(client);
  run-application
    (application,
     process: process,
     system-data: system-data,
     machine: process.process-host-machine)
end method;
