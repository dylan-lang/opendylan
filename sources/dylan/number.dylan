Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// PROTOCOL

// BOOTED: define ... class <number> ... end;
// BOOTED: define ... class <complex> ... end;
// BOOTED: define ... class <real> ... end;
// BOOTED: define ... class <rational> ... end;

///---*** LET'S ELIMINATE THIS!
define generic contagious-type (x :: <real>, y :: <real>)
 => (result :: <type>);

///---*** LET'S ELIMINATE THIS!
define inline function contagious-call
    (function :: <function>, x :: <real>, y :: <real>)
  let type :: <type> = contagious-type(x, y);
  function(as(type, x), as(type, y))
end function contagious-call;

define macro numeric-properties-predicate-definer
  { define numeric-properties-predicate ?:name (?domain:name) }
  => { define open generic ?name (x :: <object>) => (result :: <boolean>);
       define sealed domain ?name (?domain) }
  // Default sealed domain to <complex>
  { define numeric-properties-predicate ?:name }
  => { define numeric-properties-predicate ?name (<complex>) }
end macro numeric-properties-predicate-definer;

define numeric-properties-predicate zero?;
define numeric-properties-predicate positive?;
define numeric-properties-predicate negative?;
define numeric-properties-predicate integral?;

/// DRM specifies odd? & even? as being functions restricted to <integer>, but
/// we extend them to be open generics with <machine-word> methods.
define numeric-properties-predicate odd?;
define numeric-properties-predicate even?;

define macro binary-arithmetic-function-definer
  { define binary-arithmetic-function ?:name (?domain1:name, ?domain2:name) }
  => { define open generic ?name (x :: <object>, y :: <object>)
         => (#rest values :: <object>);
       define sealed domain ?name (?domain1, ?domain2) }
  // Default sealed domain to (<complex>, <complex>)
  { define binary-arithmetic-function ?:name }
  => { define binary-arithmetic-function ?name (<complex>, <complex>) }
end macro binary-arithmetic-function-definer;

define binary-arithmetic-function \+;
define binary-arithmetic-function \-;
define binary-arithmetic-function \*;
define binary-arithmetic-function \/;
define binary-arithmetic-function \^ (<complex>, <integer>);

define macro unary-arithmetic-function-definer
  { define unary-arithmetic-function ?:name (?domain:name) }
  => { define open generic ?name (x :: <object>)
         => (#rest values :: <object>);
       define sealed domain ?name (?domain) }
  // Default sealed domain to <complex>
  { define unary-arithmetic-function ?:name }
  => { define unary-arithmetic-function ?name (<complex>) }
end macro unary-arithmetic-function-definer;

define unary-arithmetic-function negative;
define unary-arithmetic-function abs;

define generic floor
    (real :: <machine-number>) => (result :: <integer>, remainder :: <machine-number>);
define generic ceiling
    (real :: <machine-number>) => (result :: <integer>, remainder :: <machine-number>);
define generic round
    (real :: <machine-number>) => (result :: <integer>, remainder :: <machine-number>);
define generic truncate
    (real :: <machine-number>) => (result :: <integer>, remainder :: <machine-number>);

define generic floor/
    (real1 :: <machine-number>, real2 :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>);
define generic ceiling/
    (real1 :: <machine-number>, real2 :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>);
define generic round/
    (real1 :: <machine-number>, real2 :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>);
define generic truncate/
    (real1 :: <machine-number>, real2 :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>);

define generic modulo
    (real1 :: <machine-number>, real2 :: <machine-number>) => (result :: <machine-number>);
define generic remainder
    (real1 :: <machine-number>, real2 :: <machine-number>) => (result :: <machine-number>);

//// CONDITIONS

define open abstract class <arithmetic-error> (<error>, <format-string-condition>)
  inherited slot condition-format-string = "Arithmetic error";
end class <arithmetic-error>;

define sealed class <division-by-zero-error> (<arithmetic-error>)
  inherited slot condition-format-string = "Division by zero";
end class <division-by-zero-error>;

define sealed domain make (singleton(<division-by-zero-error>));
define sealed domain initialize (<division-by-zero-error>);

define sealed class <arithmetic-overflow-error> (<arithmetic-error>)
  inherited slot condition-format-string = "Arithmetic overflow";
end class <arithmetic-overflow-error>;

define sealed domain make (singleton(<arithmetic-overflow-error>));
define sealed domain initialize (<arithmetic-overflow-error>);

define sealed class <arithmetic-underflow-error> (<arithmetic-error>)
  inherited slot condition-format-string = "Arithmetic underflow";
end class <arithmetic-underflow-error>;

define sealed domain make (singleton(<arithmetic-underflow-error>));
define sealed domain initialize (<arithmetic-underflow-error>);

//// IMPLEMENTATION
// Currently only <real> is implemented, with domains sealed over <complex>.
// Someday either further restrict the sealed domains or implement <complex>.

///---*** kab, 4-June-96: These comparison methods are wrong.
///---*** Using contagious-call makes them non-transitive (cf CLtL vs ANSI-CL).
///---*** Further, the DRM states that comparing a <rational> and a <float> should
///---*** convert the <float> to a <rational>; we're doing the opposite.
///---*** I don't have time to fix them right now, so just noting it for later.
define macro binary-comparison-method-definer
  { define binary-comparison-method ?:name }
  => { define method ?name
           (x :: <machine-number>, y :: <machine-number>) => (result :: <boolean>)
         contagious-call(?name, x, y)
       end method ?name;
       define sealed domain ?name (<complex>, <complex>) }
end macro binary-comparison-method-definer;

define binary-comparison-method \=;
define binary-comparison-method \<;

define macro binary-arithmetic-method-definer
  { define binary-arithmetic-method ?:name }
  => { define inline method ?name
           (x :: <machine-number>, y :: <machine-number>)
        => (result :: <machine-number>)
         contagious-call(?name, x, y)
       end method ?name }
end macro binary-arithmetic-method-definer;

define binary-arithmetic-method \+;
define binary-arithmetic-method \-;
define binary-arithmetic-method \*;

/// As \/ isn't defined for <integer>, we must define explicit methods
/// directly as the <machine-number> methods above would just result in an
/// infinite recursion as it would be the only applicable method!
define method \/ (x :: <float>, y :: <integer>) => (result :: <float>)
  x / as(<float>, y)
end method \/;

define method \/ (x :: <integer>, y :: <float>) => (result :: <float>)
  as(<float>, x) / y
end method \/;

define method \/ (x :: <float>, y :: <float>) => (result :: <float>)
  contagious-call(\/, x, y)
end method \/;

define inline method floor (real :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>)
  contagious-call(floor/, real, 1);
end method floor;

define inline method ceiling (real :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>)
  contagious-call(ceiling/, real, 1);
end method ceiling;

define inline method round (real :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>)
  contagious-call(round/, real, 1);
end method round;

define inline method truncate (real :: <machine-number>)
 => (result :: <integer>, remainder :: <machine-number>)
  contagious-call(truncate/, real, 1);
end method truncate;

define inline method floor/ (real :: <machine-number>, divisor :: <machine-number>)
 => (integer :: <integer>, remainder :: <machine-number>)
  let (integer :: <integer>, remainder :: <machine-number>) = truncate/(real, divisor);
  if (~zero?(remainder)
      & if (negative?(divisor))
          positive?(real)
        else
          negative?(real)
        end if)
    values(integer - 1, remainder + divisor)
  else
    values(integer, remainder)
  end if
end method floor/;

define inline method ceiling/ (real :: <machine-number>, divisor :: <machine-number>)
 => (integer :: <integer>, remainder :: <machine-number>)
  let (integer :: <integer>, remainder :: <machine-number>) = truncate/(real, divisor);
  if (~zero?(remainder)
      & if (negative?(divisor))
          negative?(real)
        else
          positive?(real)
        end if)
    values(integer + 1, remainder - divisor)
  else
    values(integer, remainder)
  end if
end method ceiling/;

define inline method round/ (real :: <machine-number>, divisor :: <machine-number>)
 => (integer :: <integer>, remainder :: <machine-number>)
  let (integer :: <integer>, remainder :: <machine-number>) = truncate/(real, divisor);
  let threshold :: <machine-number> = abs(divisor) / 2.0;
  case
    remainder > threshold | (remainder = threshold & odd?(integer)) =>
      if (negative?(divisor))
        values(integer - 1, remainder + divisor)
      else
        values(integer + 1, remainder - divisor)
      end if;
    begin
      let minus-threshold = negative(threshold);
      remainder < minus-threshold
      | (remainder = minus-threshold & odd?(integer))
    end =>
      if (negative?(divisor))
        values(integer + 1, remainder - divisor)
      else
        values(integer - 1, remainder + divisor)
      end if;
    otherwise =>
      values(integer, remainder)
  end case
end method round/;

define inline method truncate/ (real :: <machine-number>, divisor :: <machine-number>)
 => (quotient :: <integer>, remainder :: <machine-number>)
  contagious-call(truncate/, real, divisor)
end method truncate/;

define inline method modulo (real :: <machine-number>, divisor :: <machine-number>)
 => (result :: <machine-number>)
  let (_integer :: <integer>, remainder :: <machine-number>) = floor/(real, divisor);
  remainder
end method modulo;

define inline method remainder (real :: <machine-number>, divisor :: <machine-number>)
 => (result :: <machine-number>)
  let (_integer :: <integer>, remainder :: <machine-number>) = truncate/(real, divisor);
  remainder
end method remainder;

define inline method abs (x :: <machine-number>) => (result :: <machine-number>)
  if (negative?(x)) negative(x) else x end
end method abs;
