Module:    internal
Authors:   Jonathan Backrach, Kim Barrett, Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// ABSTRACT INTEGER

/// BOOTED: define ... class <abstract-integer> ... end;

//// CONDITIONS

/// This function is invoked by the runtime when the hardware exception is raised...
define function integer-divide-by-0 ()
  error(make(<division-by-zero-error>))
end function integer-divide-by-0;

//// METHODS

define method as (class == <rational>, integer :: <abstract-integer>)
 => (result :: <abstract-integer>)
  integer
end method as;

///---*** SHOULD THIS CONVERSION EXIST?
define method as (class == <abstract-integer>, number :: <number>)
 => (result :: <abstract-integer>)
  // !@#$ should test for length or something
  as(<integer>, number)
end method as;

define sealed inline method as (t == <abstract-integer>, x :: <machine-word>)
 => (result :: <abstract-integer>)
  coerce-machine-word-to-abstract-integer(x)
end method as;

/// Note that there is no method for x :: <integer>, because that would
/// interfere with method selection when x is only typed to <abstract-integer>.
/// Instead, the compiler must optimize coerce-abstract-integer-to-machine-word
/// on an <integer> into coerce-integer-to-machine-word.
define sealed inline method as (t == <machine-word>, x :: <abstract-integer>)
 => (result :: <machine-word>)
  coerce-abstract-integer-to-machine-word(x)
end method as;

///---*** Should we seal any of these generics over some domain (i.e., <complex>)?
define macro generic-binary-logical-function-definer
  { define generic-binary-logical-function ?:name ?initial:expression ?domain:name }
  => { define function "generic-" ## ?name (#rest integers) => (value :: <abstract-integer>)
	 reduce("generic-binary-" ## ?name, ?initial, integers)
       end function "generic-" ## ?name;
       define open generic "generic-binary-" ## ?name
	   (integer-1 :: <abstract-integer>, integer-2 :: <abstract-integer>)
	=> (value :: <abstract-integer>) }
  { define generic-binary-logical-function ?:name ?initial:expression }
  => { define generic-binary-logical-function ?name ?initial <complex> }
end macro generic-binary-logical-function-definer;

define generic-binary-logical-function logior 0;
define generic-binary-logical-function logxor 0;
define generic-binary-logical-function logand -1;


///// INTEGER

/// BOOTED: define ... class <integer> ... end;

define sealed method make (class == <integer>, #rest all-keys, #key) => (res)
  uninstantiable-error(class);
end method;

define constant $minimum-integer
  = interpret-machine-word-as-integer(force-integer-tag($minimum-signed-machine-word));

define constant $maximum-integer
  = interpret-machine-word-as-integer(force-integer-tag($maximum-signed-machine-word));

define inline method contagious-type 
    (x :: <integer>, y :: <integer>) => (result :: <type>)
  <integer>
end method contagious-type;

define sealed inline method as (t == <integer>, x :: <machine-word>)
 => (result :: <integer>)
  coerce-machine-word-to-integer(x)
end method as;

define sealed inline method \= (x :: <integer>, y :: <integer>)
 => (result :: <boolean>)
  machine-word-equal?(interpret-integer-as-machine-word(x),
                      interpret-integer-as-machine-word(y))
end method \=;

define sealed inline method \< (x :: <integer>, y :: <integer>)
 => (result :: <boolean>)
  machine-word-less-than?(interpret-integer-as-machine-word(x),
                          interpret-integer-as-machine-word(y))
end method \<;

define sealed inline method odd?
    (integer :: <integer>) => (result :: <boolean>)
  logbit?(0, integer);
end method odd?;

define sealed inline method even? 
    (integer :: <integer>) => (result :: <boolean>)
  ~logbit?(0, integer);
end method even?;

define sealed inline method zero? 
    (integer :: <integer>) => (result :: <boolean>)
  integer = 0;
end method zero?;

define sealed inline method positive? 
    (integer :: <integer>) => (result :: <boolean>)
  integer > 0;
end method positive?;

define sealed inline method negative? 
    (integer :: <integer>) => (result :: <boolean>)
  integer < 0;
end method negative?;

define sealed inline method integral? (number :: <integer>) => (result == #t)
  #t
end method integral?;

define sealed inline method \+ (x :: <integer>, y :: <integer>)
 => (result :: <integer>)
  let mx = interpret-integer-as-machine-word(x);
  let my = strip-integer-tag(interpret-integer-as-machine-word(y));
  let result = machine-word-add-signal-overflow(mx, my);
  interpret-machine-word-as-integer(result)
end method \+;

define sealed inline method \- (x :: <integer>, y :: <integer>)
 => (result :: <integer>)
  let mx = interpret-integer-as-machine-word(x);
  let my = strip-integer-tag(interpret-integer-as-machine-word(y));
  let result = machine-word-subtract-signal-overflow(mx, my);
  interpret-machine-word-as-integer(result)
end method \-;

define sealed inline method \* (x :: <integer>, y :: <integer>)
 => (result :: <integer>)
  let mx = strip-integer-tag(interpret-integer-as-machine-word(x));
  let my = coerce-integer-to-machine-word(y);
  let result = insert-integer-tag(machine-word-multiply-signal-overflow(mx, my));
  interpret-machine-word-as-integer(result)
end method \*;

/// No / on small <integer>s

define macro unary-integer-division-definer
  { define unary-integer-division ?:name }
    => { define sealed inline method ?name (x :: <integer>)
	  => (quotient :: <integer>, remainder :: <integer>)
	   values(x, 0)
	 end method ?name }
end macro unary-integer-division-definer;

define unary-integer-division floor;
define unary-integer-division ceiling;
define unary-integer-division round;
define unary-integer-division truncate;

define macro integer-division-definer
  { define integer-division ?:name }
    => { define sealed inline method ?name
             (dividend :: <integer>, divisor :: <integer>)
	  => (quotient :: <integer>, remainder :: <integer>)
	   let mdividend = coerce-integer-to-machine-word(dividend);
	   let mdivisor = coerce-integer-to-machine-word(divisor);
	   let (mquot, mrem) = "machine-word-" ## ?name(mdividend, mdivisor);
	   values(coerce-machine-word-to-integer(mquot),
		  coerce-machine-word-to-integer(mrem))
	 end method ?name
       }
end macro integer-division-definer;

define integer-division floor/;
define integer-division ceiling/;
define integer-division round/;
define integer-division truncate/;

/// There are no lowlevel machine-word remainder functions, only primitives ...
define macro integer-division-remainder-definer
  { define integer-division-remainder ?:name ?function:name }
    => { define sealed inline method ?name
	     (dividend :: <integer>, divisor :: <integer>) => (remainder :: <integer>)
	   let rmdividend :: <raw-machine-word> = integer-as-raw(dividend);
	   let rmdivisor :: <raw-machine-word> = integer-as-raw(divisor);
	   let rmremainder :: <raw-machine-word>
	     = "primitive-machine-word-" ## ?function ## "-remainder"(rmdividend, rmdivisor);
	   raw-as-integer(rmremainder)
	 end method ?name }
end macro integer-division-remainder-definer;

define integer-division-remainder modulo floor/;
define integer-division-remainder remainder truncate/;

define macro integer-sign-adjust-definer
  { define integer-sign-adjust ?:name }
    => { define sealed inline method ?name (x :: <integer>)
          => (result :: <integer>)
	   let mx = strip-integer-tag(interpret-integer-as-machine-word(x));
	   let mresult = "machine-word-" ## ?name ## "-signal-overflow"(mx);
	   interpret-machine-word-as-integer(insert-integer-tag(mresult))
	 end method ?name
       }
end macro;

define integer-sign-adjust negative;
define integer-sign-adjust abs;

define method \^ 
    (base :: <integer>, power :: <integer>) => (res :: <rational>)
  if (negative?(power))
    //---*** THIS IS WRONG AS / ISN'T DEFINED FOR <integer>!
    //1 / (base ^ -power)
    error("Negative powers are unimplemented")
  elseif (base = 2)
    ash(1, power)
  elseif (negative?(base))
    if (odd?(power)) -(-base ^ power) else (-base ^ power) end
  else
    // Avoids squaring on last iteration to prevent premature overflow ...
    iterate loop (base :: <integer> = if (power > 1) base * base else base end,
		  p :: <integer> = ash(power, -1),
		  x  :: <integer> = if (odd?(power)) base else 1 end)
      if (zero?(p))
	x
      else
	loop(if (p > 1) base * base else base end, 
	     ash(p, -1),
	     if (odd?(p)) base * x else x end)
      end if
    end iterate
  end if
end method \^;

define function logior (#rest integers) => (logior :: <integer>)
  reduce(binary-logior, 0, integers)
end function logior;

define function logxor (#rest integers) => (logxor :: <integer>)
  reduce(binary-logxor, 0, integers)
end function logxor;

define function logand (#rest integers) => (logand :: <integer>)
  reduce(binary-logand, -1, integers)
end function logand;

// TODO: These can't be inline-only until reduce is inlined.

define macro integer-binary-logical-definer
  { define integer-binary-logical ?:name ?lowlevel:name ?tagger:name }
    => { define inline /* -only */ function ?name
             (x :: <integer>, y :: <integer>)
          => (result :: <integer>)
	   let mx = interpret-integer-as-machine-word(x);
	   let my = interpret-integer-as-machine-word(y);
	   let mresult = ?lowlevel(mx, my);
	   interpret-machine-word-as-integer(?tagger(mresult))
	 end function ?name
       }
end macro;

define integer-binary-logical binary-logior machine-word-logior identity;
define integer-binary-logical binary-logand machine-word-logand identity;
define integer-binary-logical binary-logxor machine-word-logxor force-integer-tag;

define inline function lognot (x :: <integer>) => (result :: <integer>)
  let mw = interpret-integer-as-machine-word(x);
  interpret-machine-word-as-integer(force-integer-tag(machine-word-lognot(mw)))
end function lognot;

define inline function logbit? (index :: <integer>, integer :: <integer>)
 => (set? :: <boolean>)
  machine-word-logbit?
    (as-offset-for-tagged-integer(index), interpret-integer-as-machine-word(integer))
end function logbit?;

define inline function logbit-deposit
    (z :: <boolean>, index :: <integer>, integer :: <integer>) => (res :: <integer>)
  interpret-machine-word-as-integer
    (machine-word-logbit-deposit
       (z, as-offset-for-tagged-integer(index), 
	interpret-integer-as-machine-word(integer)))
end function logbit-deposit;

define inline function bit-field-extract
    (offset :: <integer>, size :: <integer>, x :: <integer>)
 => (res :: <integer>)
  // ash-right(logand(x, ash-left(ash-left(1, size) - 1, offset)), offset)
  interpret-machine-word-as-integer
    (force-integer-tag
       (machine-word-bit-field-extract
	  (offset, as-field-size-for-tagged-integer(size), 
	   interpret-integer-as-machine-word(x))))
end function bit-field-extract;

define inline function bit-field-deposit
    (field :: <integer>, 
     offset :: <integer>, size :: <integer>, x :: <integer>) 
 => (res :: <integer>)
  // logior(logand(x, lognot(ash-left(ash-left(1, size) - 1, offset))),
  // 	    ash-left(field, offset)) 
  interpret-machine-word-as-integer
    (machine-word-bit-field-deposit
       (coerce-integer-to-machine-word(field), 
	as-offset-for-tagged-integer(offset), size, 
	interpret-integer-as-machine-word(x)))
end function bit-field-deposit;

define may-inline function ash (x :: <integer>, shift :: <integer>) => (result :: <integer>)
  if (negative?(shift))
    ash-right(x, -shift)
  else
    ash-left(x, shift)
  end
end function ash;

define inline function ash-right (x :: <integer>, shift :: <integer>)
 => (result :: <integer>)
  if (shift < $machine-word-size)
    let mx = interpret-integer-as-machine-word(x);
    let shift-result = machine-word-shift-right(mx, shift);
    let tagged-result = force-integer-tag(shift-result);
    interpret-machine-word-as-integer(tagged-result)
  else
    // Shifting by the word size (or more) propogates the sign bit ...
    if (negative?(x)) -1 else 0 end
  end
end function ash-right;

define inline function ash-left (x :: <integer>, shift :: <integer>)
 => (result :: <integer>)
  let shift :: <integer> = min(shift, $machine-word-size);
  let mx :: <machine-word>
    = strip-integer-tag(interpret-integer-as-machine-word(x));
  if (shift = $machine-word-size)
    // Primitives can't shift by the full size of a machine word so
    // we'll perform two half-word shifts instead.  (Note that this
    // code presumes the word size is even.)
    shift := ash($machine-word-size, -1);
    mx := machine-word-shift-left-signal-overflow(mx, shift);
  end;
  let shift-result = machine-word-shift-left-signal-overflow(mx, shift);
  let tagged-result = insert-integer-tag(shift-result);
  interpret-machine-word-as-integer(tagged-result)
end function ash-left;

define may-inline function lsh (x :: <integer>, shift :: <integer>) => (result :: <integer>)
  if (negative?(shift))
    lsh-right(x, -shift)
  else
    lsh-left(x, shift)
  end
end function lsh;

define inline function lsh-right (x :: <integer>, shift :: <integer>)
 => (result :: <integer>)
  if (shift < $machine-word-size)
    let mx = interpret-integer-as-machine-word(x);
    let shift-result = machine-word-unsigned-shift-right(mx, shift);
    let tagged-result = force-integer-tag(shift-result);
    interpret-machine-word-as-integer(tagged-result)
  else
    // Logical shifts by the word size (or more) always return 0 ...
    0
  end
end function lsh-right;

define inline function lsh-left (x :: <integer>, shift :: <integer>)
 => (result :: <integer>)
  if (shift < $machine-word-size)
    let mx = strip-integer-tag(interpret-integer-as-machine-word(x));
    let shift-result = machine-word-unsigned-shift-left(mx, shift);
    let tagged-result = insert-integer-tag(shift-result);
    interpret-machine-word-as-integer(tagged-result)
  else
    // Logical shifts by the word size (or more) always return 0 ...
    0
  end
end function lsh-left;

define open generic lcm (n :: <object>, m :: <object>) => (result :: <object>);

define sealed method lcm (n :: <integer>, m :: <integer>) => (result :: <integer>)
  truncate/(max(n, m), gcd(n, m)) * min(n, m)
end method lcm;

define open generic gcd (n :: <object>, m :: <object>) => (result :: <object>);

define sealed method  gcd (n :: <integer>, m :: <integer>)
 => (result :: <integer>)
  case
    n = 0 =>
      m;
    m = 0 =>
      n;
    n = m =>
      n;
    otherwise =>
      for (k :: <integer> from 0,
           u :: <integer> = abs(n) then ash(u, -1),
           v :: <integer> = abs(m) then ash(v, -1),
           until: odd?(logior(u, v)))
      finally
        block (return)
          for (tmp :: <integer>
                 = if (u.odd?)
                     v.negative
                   else
                     ash(u, -1)
                   end if
                 then ash(tmp, -1))
            if (tmp.odd?)
              if (tmp.positive?)
                u := tmp
              else
                v := tmp.negative
              end if;
              tmp := u - v;
              if (tmp.zero?)
                return(ash(u, k))
              end if
            end if;
          end for
        end block;
      end for
  end case
end method gcd;


/// *@@* NEEDED TO BOOT TABLE

define method power-of-two-ceiling (value :: <integer>) => (result :: <integer>)
  iterate search (power-of-two :: <integer> = 1)
    if (power-of-two >= value)
      power-of-two
    else
      search(power-of-two + power-of-two)
    end if
  end iterate
end method power-of-two-ceiling;

// TODO: EXPORT THESE
define constant <bit>         = limited(<integer>, min: 0, max: 1);
define constant <byte>        = limited(<integer>, min: 0, max: 255);
define constant <double-byte> = limited(<integer>, min: 0, max: 65535);

