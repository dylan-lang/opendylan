module:    access-path-implementation
synopsis:  Make <remote-value> the same as an untyped C pointer so that
           this stuff works under SCC.
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// This should eventually be <machine-word>

///// <REMOTE-VALUE>

define constant <remote-value> = <integer>;

// as-integer should be implemented as whatever conversion is provided
// between <machine-word> and <integer>

define method as-integer (x :: <remote-value>)
   x
end method;

define method as-integer-losing-precision (x :: <remote-value>)
   x
end method;

///// INDEXED-REMOTE-VALUE


define method indexed-remote-value (x :: <remote-value>, i :: <integer>)
                                    => (_ :: <remote-value>)
  x + i;
end method;


///// AS-REMOTE-VALUE


define method as-remote-value (x :: <integer>) => (_ :: <remote-value>)
  x
end method;

define method as-remote-value-losing-precision 
     (x :: <integer>) => (_ :: <remote-value>)
  x
end method;

///// TAGGED-REMOTE-VALUE-AS-INTEGER


define method tagged-remote-value-as-integer (x :: <remote-value>)
                                              => (_ :: <integer>)
  truncate (as-integer(x), 4);
end method;


///// TAGGED-REMOTE-VALUE-AS-CHARACTER


define method tagged-remote-value-as-character (x :: <remote-value>)
                                                => (_ :: <character>)
  as (<character>, truncate (as-integer(x), 4));
end method;

///// INTEGER-AS-TAGGED-REMOTE-VALUE
//    Given an integer, this returns the integer as a correctly tagged
//    <remote-value>

define method integer-as-tagged-remote-value (i :: <integer>)
    => (x :: <remote-value>)
  (i * 4) + 1;
end method;


//// CHARACTER-AS-TAGGED-REMOTE-VALUE
//   Given a character, this returns the character as a correctly tagged
//   <remote-value>

define method character-as-tagged-remote-value (c :: <character>)
    => (x :: <remote-value>)
  (as(<integer>, c) * 4) + 2;
end method;


///// REMOTE-VALUE-=

define method remote-value-= (x :: <remote-value>, y :: <remote-value>)
    => (answer :: <boolean>)
  x = y;
end method;


///// REMOTE-VALUE-<

define method remote-value-< (x :: <remote-value>, y :: <remote-value>)
    => (answer :: <boolean>)
  x < y;
end method;


///// REMOTE-VALUE-<=

define method remote-value-<= (x :: <remote-value>, y :: <remote-value>)
    => (answer :: <boolean>)
  x <= y;
end method;



define method as-dylan-string (s :: <string>) => (_ :: <string>)
  s;
end method;

// eof


define method remote-value-as-string
    (ap, val :: <remote-value>, radix :: <integer>)
       => (s :: <string>)
  "00000000"   // I can't be bothered!
end method;

