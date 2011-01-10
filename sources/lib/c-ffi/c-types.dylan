module:    c-ffi-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// This should really be <abstract-integer>, but until the ffi is fixed...

define constant <ffi-integer> = type-union(<integer>, <machine-word>);


define macro aliased-designators-definer
  { define aliased-designators ?alias-base:name ?existing-base:name }
    => { define constant "<C-" ## ?alias-base ## ">"
           = "<C-" ## ?existing-base ## ">";
         define constant "<C-" ## ?alias-base ## "*>"
           = "<C-" ## ?existing-base ## "*>";
         define constant "<C-unsafe-" ## ?alias-base ## ">"
           = "<C-unsafe-" ## ?existing-base ## ">";
         define constant "<C-unsafe-" ## ?alias-base ## "*>"
           = "<C-unsafe-" ## ?existing-base ## "*>";
         define constant "<C-both-" ## ?alias-base ## ">"
           = "<C-both-" ## ?existing-base ## ">";
         define constant "<C-both-" ## ?alias-base ## "*>"
           = "<C-both-" ## ?existing-base ## "*>" }
  { define aliased-designators #f ?existing-base:name }
    => { }
end macro;



define macro mapped-integer-type-definer
  { define mapped-integer-type "<C-" ## ?base-name:name ## ">"
        ?signedness:name
        ?alias-base:* }
    =>
    { 
define open simple-c-mapped-subtype "<C-" ## ?base-name ## ">"
  ("<C-raw-" ## ?base-name ## ">")
  export-map <integer>,
    export-function:
      method (x :: <integer>) => (m :: <machine-word>)
	check-export-range
	  (x, size-of("<C-raw-" ## ?base-name ## ">" ), ?#"signedness");
	as(<machine-word>, x)
      end;
  import-map <ffi-integer>,
    import-function:
      method (x :: <machine-word>) => (i :: <ffi-integer>);
	check-import-range
	  (x, size-of("<C-raw-"## ?base-name ## ">"), ?#"signedness");
	as(<abstract-integer>, x)
      end;
  pointer-type "<C-" ## ?base-name ## "*>";
end;

define open simple-c-mapped-subtype "<C-unsafe-" ## ?base-name ## ">"
  ("<C-raw-" ## ?base-name ## ">")
  export-map <integer>,
    export-function:
      method (x :: <integer>) => (m :: <machine-word>);
	as(<machine-word>, x)
      end;
  import-map <ffi-integer>,
    import-function:
      method (x :: <machine-word>) => (i :: <ffi-integer>);
	as(<abstract-integer>, x);
      end;
  pointer-type "<C-unsafe-" ## ?base-name ##"*>";
end;

define open simple-c-mapped-subtype "<C-both-" ## ?base-name ## ">"
  ("<C-raw-" ## ?base-name ## ">")
  map <ffi-integer>,
    export-function: method (x :: <ffi-integer>)
		      => (m :: <machine-word>);
		       export-to-machine-word
			 (x, size-of("<C-raw-" ## ?base-name ## ">"),
					      ?#"signedness");
		     end,
    import-function:
      method (x :: <machine-word>) => (i :: <ffi-integer>);
	check-import-range
	  (x, size-of("<C-raw-" ## ?base-name ## ">"), ?#"signedness");
	as(<abstract-integer>, x);
      end;
  pointer-type "<C-both-" ## ?base-name ## "*>";
end;

 define aliased-designators ?alias-base ?base-name
}
end macro;    
        
define mapped-integer-type <C-unsigned-int> unsigned #f;
define mapped-integer-type <C-signed-int> signed int;
define mapped-integer-type <C-unsigned-long> unsigned #f;
define mapped-integer-type <C-signed-long> signed long;
define mapped-integer-type <C-unsigned-short> unsigned #f;
define mapped-integer-type <C-signed-short> signed short;
define mapped-integer-type <C-unsigned-char> unsigned #f;
define mapped-integer-type <C-signed-char> signed #f;


define constant <C-both-char> = <C-both-signed-char>;
define constant <C-unsafe-char> = <C-unsafe-signed-char>;
define constant <C-char> = <C-signed-char>;

define constant <C-both-char*> = <C-both-signed-char*>;
define constant <C-unsafe-char*> = <C-unsafe-signed-char*>;
define constant <C-char*> = <C-signed-char*>;

define constant <C-raw-long> = <C-raw-signed-long>;
define constant <C-raw-short> = <C-raw-signed-short>;

define constant <C-raw-long*> = <C-raw-signed-long*>;
define constant <C-raw-short*> = <C-raw-signed-short*>;


define inline method export-to-machine-word (thing :: <integer>,
					     size :: <integer>,
					     signedness :: <symbol>)
 => (m :: <machine-word>);
  check-export-range(thing, size, signedness);
  as(<machine-word>, thing);
end;

define inline method export-to-machine-word (thing :: <machine-word>,
					     size :: <integer>,
					     signedness :: <symbol>)
 => (m :: <machine-word>);
  check-export-range(thing, size, signedness);
  thing
end;

// !@#$% need to define these

define inline method check-export-range
    (x :: <integer>, size :: <integer>, signedness == #"signed")
end;
define inline method check-export-range
    (x :: <machine-word>, size :: <integer>, signedness == #"signed")
end;
define inline method check-export-range
    (x :: <integer>, size :: <integer>, signedness == #"unsigned")
end;
define inline method check-export-range
    (x :: <machine-word>, size :: <integer>, signedness == #"unsigned")
end;


// this may not be necessary..
define inline method check-import-range
    (x :: <machine-word>, size :: <integer>, signedness == #"signed")
end;
define inline method check-import-range
    (x :: <machine-word>, size :: <integer>, signedness == #"unsigned")
end;
