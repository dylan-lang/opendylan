Module:       system-internals
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $platform-name
    = as(<symbol>, 
         concatenate(as(<string>, $machine-name), "-", 
                     as(<string>, $os-name)));


/// System data offset definitions

///---*** NOTE: We need to extend this technology to handle operating system
///---***       as well as machine architecture differences...

define macro system-offset-value
  { system-offset-value () ?default-offset:expression end }
    =>  { ?default-offset }
  {  system-offset-value (?machine:name ?offset:expression, ?offsets:*) 
                         ?default-offset:expression
     end }
    => { (if ($machine-name = ?#"machine") ?offset
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
