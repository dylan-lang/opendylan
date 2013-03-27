Module: dfmc-back-end
Author: Jonathan Bachrach, Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *retract-dfm?* = #t;

define compiler-open generic emit-all 
    (back-end :: <back-end>, record, #rest flags, #key, #all-keys);

define compiler-open generic emit-dfm
  (back-end :: <back-end>, stream, object, #rest flags, #key, #all-keys) => ();

define method emit-dfm (back-end :: <back-end>, stream :: <stream>, o :: <&iep>,
                        #rest flags, #key form?, force-emit?, #all-keys) => ()
  print-method(stream, o.function);
  format(stream, "\n");
end method emit-dfm;

define method emit-dfm (back-end :: <back-end>, stream :: <stream>, o,
                        #rest flags, #key, #all-keys) => ()
end method emit-dfm;

// TODO: I guess emit-code should be exported too, but each back-end
// currently just defines a local emit-code generic.
// define compiler-open generic emit-code
//     (back-end :: <back-end>, object) => (); 
define compiler-open generic emit-object     // structure
    (back-end :: <back-end>, stream, object);
define compiler-open generic emit-reference  // reference
    (back-end :: <back-end>, stream, object);
define compiler-open generic emit-indirect-reference  // reference
    (back-end :: <back-end>, stream, object);
define compiler-open generic emit-name-internal       // binding name
    (back-end :: <back-end>, stream, object);
define compiler-open generic string-emitter       // binding name
    (back-end :: <back-end>, stream, object);
define compiler-open generic back-end-word-size 
  (back-end :: <back-end>) => (number-bytes :: <integer>);
define compiler-open generic back-end-record-repeated-object-sizes? 
  (back-end :: <back-end>) => (well? :: <boolean>);
define compiler-open generic back-end-lambda-size
  (back-end :: <back-end>, lambda :: <&lambda>) => (number-bytes :: <integer>);

define method back-end-record-repeated-object-sizes? 
    (back-end :: <back-end>) => (well? :: <boolean>)
  #f
end method;

define method back-end-lambda-size
    (back-end :: <back-end>, lambda :: <&lambda>) => (number-bytes :: <integer>)
  0
end method;

define compiler-open generic initialize-back-end 
  (back-end :: <back-end>) => ();

define method initialize-back-end
    (back-end :: <back-end>) => ()
  #f
end method;

define macro with-back-end-initialization
  { with-back-end-initialization (?back-end:expression)
      ?:body
    end }
    => { let back-end = ?back-end;

	// initialize back-end variables on entry
	initialize-back-end(back-end);

	 ?body;

	// free back-end variables on exit
	initialize-back-end(back-end);
       }
end macro;

define inline sideways method word-size () => (number-bytes :: <integer>)
  back-end-word-size(current-back-end())
end method;

define compiler-open generic print-method (stream :: <stream>, o :: <&lambda>, #key css);
define compiler-open generic print-method-out (o :: <&lambda>, #key css);

define compiler-open generic print-referenced-object (o :: <object>, stream :: <stream>) => ();

define thread variable *current-environment* = #f;
