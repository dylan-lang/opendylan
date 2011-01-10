Module:       common-dylan-internals
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//////////////////////////////////////////////////////////////////////////////
// %+ (x :: <machine-word>, y :: <machine-word>)
//   => result :: <machine-word>, overflow? :: <boolean>
//
// %- (x :: <machine-word>, y :: <machine-word>)
//   => result :: <machine-word>, overflow? :: <boolean>

define macro simple-arithmetic-definer
  { define simple-arithmetic ?:name ?lowlevel:name }
  => { define sealed generic ?name (x :: <object>, y :: <object>)
         => (result :: <machine-word>, overflow? :: <boolean>);
       define inline method ?name
            (x :: <machine-word>, y :: <machine-word>)
         => (result :: <machine-word>, overflow? :: <boolean>);
         ?lowlevel(x, y);
       end method;
       define inline method ?name
            (x :: <machine-word>, y :: <abstract-integer>)
         => (result :: <machine-word>, overflow? :: <boolean>);
         ?lowlevel(x, coerce-abstract-integer-to-machine-word(y));
       end method;
       define inline method ?name
            (x :: <abstract-integer>, y :: <machine-word>)
         => (result :: <machine-word>, overflow? :: <boolean>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(x), y);
       end method;
       define inline method ?name
            (x :: <abstract-integer>, y :: <abstract-integer>)
         => (result :: <machine-word>, overflow? :: <boolean>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(x),
                   coerce-abstract-integer-to-machine-word(y));
       end method;
       }
end macro;

define simple-arithmetic \%+ machine-word-add-with-overflow;
define simple-arithmetic \%- machine-word-subtract-with-overflow;

//////////////////////////////////////////////////////////////////////////////
// %* (x :: <machine-word>, y :: <machine-word>)
//   => low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>

define sealed generic \%* (x :: <object>, y :: <object>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);

define inline method \%* (x :: <machine-word>, y :: <machine-word>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  machine-word-multiply-with-overflow(x, y);
end method;

define inline method \%* (x :: <machine-word>, y :: <abstract-integer>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  machine-word-multiply-with-overflow
      (x, coerce-abstract-integer-to-machine-word(y));
end method;

define inline method \%* (x :: <abstract-integer>, y :: <machine-word>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  machine-word-multiply-with-overflow
      (coerce-abstract-integer-to-machine-word(x), y);
end method;

define inline method \%* (x :: <abstract-integer>, y :: <abstract-integer>)
  => (low :: <machine-word>, high :: <machine-word>, overflow? :: <boolean>);
  machine-word-multiply-with-overflow
      (coerce-abstract-integer-to-machine-word(x),
       coerce-abstract-integer-to-machine-word(y));
end method;

//////////////////////////////////////////////////////////////////////////////
// %negative(x :: <machine-word>)
//   => result :: <machine-word>, overflow? :: <boolean>
//
// %abs(x :: <machine-word>)
//   => result :: <machine-word>, overflow? :: <boolean>

define macro sign-modifier-definer
  { define sign-modifier ?:name ?lowlevel:name }
  => { define sealed generic ?name (x :: <object>)
         => (result :: <machine-word>, overflow? :: <boolean>);
       define inline method ?name (x :: <machine-word>)
         => (result :: <machine-word>, overflow? :: <boolean>);
         ?lowlevel(x);
       end method;
       define inline method ?name (x :: <abstract-integer>)
         => (result :: <machine-word>, overflow? :: <boolean>);
         ?lowlevel(coerce-abstract-integer-to-machine-word(x));
       end method;
       }
end macro;

define sign-modifier %negative machine-word-negative-with-overflow;
define sign-modifier %abs machine-word-abs-with-overflow;

