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

define constant <remote-value> = <DYLAN-VALUE>;

// as-integer should be implemented as whatever conversion is provided
// between <machine-word> and <integer>

define method as-integer (x :: <remote-value>)
  as (<integer>, x.pointer-pointer); // A bit naughty?
end method;


///// INDEXED-REMOTE-VALUE

define method indexed-remote-value (x :: <remote-value>, i :: <integer>)
    => (_ :: <remote-value>)
  x + (i * 4);
end method;


///// AS-REMOTE-VALUE

define method as-remote-value (x :: <integer>) => (_ :: <remote-value>)
  let v = make (<remote-value>);
  v.pointer-pointer := x;
  v;
end method;


///// TAGGED-REMOTE-VALUE-AS-INTEGER

define method tagged-remote-value-as-integer (x :: <remote-value>)
    => (_ :: <integer>)
  nub-primitive-tagged-value-as-integer (x);
end method;


///// TAGGED-REMOTE-VALUE-AS-CHARACTER

define method tagged-remote-value-as-character (x :: <remote-value>)
    => (_ :: <character>)
  nub-primitive-tagged-value-as-character (x);
end method;


///// INTEGER-AS-TAGGED-REMOTE-VALUE

define method integer-as-tagged-remote-value (i :: <integer>)
    => (_ :: <remote-value>)
  nub-primitive-integer-as-tagged-value (i);
end method;


///// CHARACTER-AS-TAGGED-REMOTE-VALUE

define method character-as-tagged-remote-value (c :: <character>)
    => (_ :: <remote-value>)
  nub-primitive-character-as-tagged-value (c);
end method;


///// AS-DYLAN-STRING

define method as-dylan-string (s :: <C-string>) => (_ :: <string>)

  let sz = size(s);
  let ds = make (<string>, size: sz);
  let i = 0;
  for (ch in s)
    ds[i] := as (<character>, ch);
    i := i + 1;
  end for;
  ds;
end method;

// eof

