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

/// System data offset definitions

///---*** NOTE: We need to extend this technology to handle operating system
///---***       as well as machine architecture differences...

define macro system-offset-value
  { system-offset-value () ?default-offset:expression end }
    =>  { ?default-offset }
  {  system-offset-value (?platform:name ?offset:expression, ?offsets:*) 
                         ?default-offset:expression
     end }
    => { (if ($platform-name = ?#"platform") ?offset
	  else system-offset-value (?offsets) ?default-offset end
	 end);
	}
end macro system-offset-value;

/*
define macro system-offset-definer
  { define system-offset ?:name (?offsets:*) ?default-offset:expression}
    => { define macro "with-" ## ?name ## "-offset"
	   { "with-" ## ?name ## "-offset" () \?:body end }
	     => { begin
		    let \?=offset :: <integer>
                      = system-offset-value (?offsets) ?default-offset end;
		    \?body
		  end }
         end macro "with-" ## ?name ## "-offset"; }
end macro system-offset-definer;
*/

define macro system-offset-definer
  { define system-offset ?:name (?offsets:*) ?default-offset:expression }
    => { define constant "$" ## ?name ## "-offset" :: <integer>
           = system-offset-value (?offsets) ?default-offset end }
end macro system-offset-definer;
