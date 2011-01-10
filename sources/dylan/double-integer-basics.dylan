Module:    internal
Authors:   Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BOOTED: define ... class <double-integer> ... end;

define inline-only function %double-integer-low (di :: <double-integer>)
 => (low :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw(primitive-initialized-slot-value(di, integer-as-raw(0))))
end function %double-integer-low;

define inline-only function %double-integer-low-setter
    (new-low :: <machine-word>, di :: <double-integer>) => (new-low :: <machine-word>)
  primitive-slot-value(di, integer-as-raw(0))
    := primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(new-low));
  new-low
end function %double-integer-low-setter;

define inline-only function %double-integer-high (di :: <double-integer>)
 => (high :: <machine-word>)
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw(primitive-initialized-slot-value(di, integer-as-raw(1))))
end function %double-integer-high;

define inline-only function %double-integer-high-setter
    (new-high :: <machine-word>, di :: <double-integer>) => (new-high :: <machine-word>)
  primitive-slot-value(di, integer-as-raw(1))
    := primitive-cast-raw-as-pointer(primitive-unwrap-machine-word(new-high));
  new-high
end function %double-integer-high-setter;

define sealed inline method make
    (class == <double-integer>, #key low :: <machine-word>, high :: <machine-word>)
 => (di :: <double-integer>)
  let di = system-allocate-simple-instance(<double-integer>);
  %double-integer-low(di) := low;
  %double-integer-high(di) := high;
  di
end method make;

define macro integer-double-comparison-methods-definer
 // The ?integer argument is the predicate to use in the method where the
 // first argument is an <integer> and ?double is for the method where the
 // first argument is a <double-integer>.  In all cases, the <double-integer>
 // is the argument to the predicate.
 { define integer-double-comparison-methods ?:name ?integer:name ?double:name }
  => { define sealed inline method ?name (x :: <integer>, y :: <double-integer>)
	=> (result :: <boolean>)
	 ?integer(y)
       end method ?name;
       define sealed inline method ?name (x :: <double-integer>, y :: <integer>)
	=> (result :: <boolean>)
	 ?double(x)
       end method ?name }
 // With no predicate arguments, define the methods to return #f
 { define integer-double-comparison-methods ?:name }
  => { define sealed inline method ?name (x :: <integer>, y :: <double-integer>)
	=> (result :: <boolean>)
	 #f
       end method ?name;
       define sealed inline method ?name (x :: <double-integer>, y :: <integer>)
	=> (result :: <boolean>)
	 #f
       end method ?name }
end macro integer-double-comparison-methods-definer;

///---*** NOTE: The DRM states that comparison between <float>s and <rational>s
///---*** should be accomplished by converting the <float> to a <rational> rather
///---*** than the other way around which is how we've implemented it here.
define macro float-double-comparison-methods-definer
 { define float-double-comparison-methods ?:name }
  => { define sealed inline method ?name (x :: <float>, y :: <double-integer>)
	=> (result :: <boolean>)
	 ?name(x, as(<float>, y))
       end method ?name;
       define sealed inline method ?name (x :: <double-integer>, y :: <float>)
	=> (result :: <boolean>)
	 ?name(as(<float>, x), y)
       end method ?name }
end macro float-double-comparison-methods-definer;

define sealed inline method \= (x :: <double-integer>, y :: <double-integer>)
 => (result :: <boolean>)
  machine-word-equal?(%double-integer-low(x), %double-integer-low(y))
  & machine-word-equal?(%double-integer-high(x), %double-integer-high(y))
end method \=;

/// As <integer> and <double-integer> are disjoint, they will never be equal.
define integer-double-comparison-methods \=;
define float-double-comparison-methods \=;

define sealed inline method \< (x :: <double-integer>, y :: <double-integer>)
 => (result :: <boolean>)
  machine-word-less-than?(%double-integer-high(x), %double-integer-high(y))
  | (machine-word-equal?(%double-integer-high(x), %double-integer-high(y))
     & machine-word-unsigned-less-than?(%double-integer-low(x), %double-integer-low(y)))
end method \<;

/// As <integer> and <double-integer> are disjoint, an <integer> is less than
/// a <double-integer> if, and only if, the <double-integer> is positive.
/// Similarly, a <double-integer> is less than an <integer> if, and only if,
/// the <double-integer> is negative.
define integer-double-comparison-methods \< positive? negative?;
define float-double-comparison-methods \<;

define sealed inline method odd? (x :: <double-integer>) => (odd? :: <boolean>)
  machine-word-logbit?(0, %double-integer-low(x))
end method odd?;

define sealed inline method even? (x :: <double-integer>) => (zero? :: <boolean>)
  ~machine-word-logbit?(0, %double-integer-low(x))
end method even?;

/// As <integer> and <double-integer> are disjoint and 0 is an <integer>,
/// no <double-integer> will ever be zero ...
define sealed inline method zero? (x :: <double-integer>) => (zero? :: <boolean>)
  #f
end method zero?;

define sealed inline method positive? (x :: <double-integer>) => (positive? :: <boolean>)
  ~zero?(x) & ~negative?(x)
end method positive?;

define sealed inline method negative? (x :: <double-integer>) => (negative? :: <boolean>)
  machine-word-less-than?(%double-integer-high(x), coerce-integer-to-machine-word(0))
end method negative?;

define sealed inline method integral? (x :: <double-integer>) => (integral? :: <boolean>)
  #t
end method integral?;
