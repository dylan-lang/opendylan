Module:       system-internals
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $platform-name
    = as(<symbol>,
         concatenate(as(<string>, $machine-name), "-",
                     as(<string>, $os-name)));

define macro with-application-output
  { with-application-output (?stream:variable = ?command:expression,
                             #rest ?keys:expression)
      ?body:body
    end }
  => { begin
         let _stream = #f;
         let _process = #f;
         block ()
           let (exit-code, signal, process, ostream)
             = run-application(?command, asynchronous?: #t, output: #"stream",
                               ?keys);
           _stream := ostream;
           _process := process;
           let ?stream :: <file-stream> = ostream;
           ?body;
         cleanup
           if (_process) wait-for-application-process(_process) end;
           if (_stream & stream-open?(_stream)) close(_stream) end;
         end
       end }
end macro with-application-output;
