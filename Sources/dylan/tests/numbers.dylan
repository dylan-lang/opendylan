Module:       dylan-test-suite
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Collection tests

define sideways method class-test-function
    (class :: subclass(<number>)) => (function :: <function>)
  test-number-class
end method class-test-function;

define open generic test-number-class
    (class :: subclass(<number>), #key, #all-keys) => ();

define method test-number-class
    (class :: subclass(<number>), #key abstract?, #all-keys) => ()
  unless (abstract?)
    test-number("0",  as(class, 0));
    test-number("1",  as(class, 1));
    test-number("2",  as(class, 2));
    test-number("-1", as(class, -1));
    test-number("-2", as(class, -2));
  end
end method test-number-class;

//--- An extra method to test special features of arrays
define method test-number-class
    (class :: subclass(<float>), #key abstract?, #all-keys) => ()
  next-method();
  unless (abstract?)
    test-number("1.5", as(class, 1.5));
    test-number("-1.5", as(class, -1.5));
  end
end method test-number-class;


/// Number test functions
/*
define method test-number
    (name :: <string>, number :: <number>) => ()
  //---*** No operations on <number>
end method test-collection;
*/

//---*** This method should be on <complex> but we put it on <number> since
//---*** <complex> isn't implemented properly.
define method test-number
    (name :: <string>, number :: <number>) => ()
  // next-method();
  do(method (function) function(name, number) end,
     vector(// Methods on <complex>
            test-=,
            test-zero?,
            test-+,
            test-*,
            test--,
            test-/,
            test-^
            ))
end method test-number;

define method test-number
    (name :: <string>, number :: <real>) => ()
  next-method();
  do(method (function) function(name, number) end,
     vector(// Functions on <real>
            test-floor,
            test-ceiling,
            test-round,
            test-truncate,
            test-floor/,
            test-ceiling/,
            test-round/,
            test-truncate/,
            test-modulo,
            test-remainder,

            // Methods on <real>
            test-<,
            test-abs,
            test-positive?,
            test-negative?,
            test-integral?,
            test-negative
            ))
end method test-number;

define method test-number
    (name :: <string>, number :: <integer>) => ()
  next-method();
  do(method (function) function(name, number) end,
     vector(// Functions on <integer>
            test-odd?,
            test-even?,
            test-logior,
            test-logxor,
            test-logand,
            test-lognot,
            test-logbit?,
            test-ash,

            // Methods on <integer>
            test-lcm,
            test-gcd,
            test-limited
            ))
end method test-number;


/// Number testing

//---*** These methods should be on <complex> but the hierarchy isn't correct
//---*** in the emulator at least so we do it on <number> instead.

define method test-= 
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method test-=;

define method test-zero? 
    (name :: <string>, number :: <number>) => ()
  check-equal(format-to-string("zero?(%d)", number),
              zero?(number),
              integral?(number) & number < 1 & number > -1)
end method test-zero?;

define method test-+
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method test-+;

define method test-* 
    (name :: <string>, number :: <number>) => ()
  check-equal(format-to-string("%d * 1 = %d", number, number),
              number * 1, number)
end method test-*;

define method test-- 
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method test--;

define method test-/
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method test-/;

define method test-^
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method test-^;


/// Real number testing
define method test-floor
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-floor;

define method test-ceiling
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-ceiling;

define method test-round
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-round;

define method test-truncate
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-truncate;

define method test-floor/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-floor/;

define method test-ceiling/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-ceiling/;

define method test-round/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-round/;

define method test-truncate/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-truncate/;

define method test-modulo
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-modulo;

define method test-remainder
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-remainder;

define method test-<
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-<;

define method test-abs
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-abs;

define method test-positive?
    (name :: <string>, number :: <real>) => ()
  check-equal(format-to-string("positive?(%d)", number),
              positive?(number),
              number > 0)
end method test-positive?;

define method test-negative?
    (name :: <string>, number :: <real>) => ()
  check-equal(format-to-string("negative?(%d)", number),
              negative?(number),
              number < 0)
end method test-negative?;

define method test-integral?
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method test-integral?;

define method test-negative
    (name :: <string>, number :: <real>) => ()
  check-equal(format-to-string("negative(negative(%d)) = %d", number, number),
              negative(negative(number)),
              number)
end method test-negative;


/// Integer number testing

define method test-odd?
    (name :: <string>, number :: <integer>) => ()
  check-equal(format-to-string("odd?(%d)", number),
              odd?(number),
              modulo(number, 2) = 1)
end method test-odd?;

define method test-even?
    (name :: <string>, number :: <integer>) => ()
  check-equal(format-to-string("even?(%d)", number),
              even?(number),
              modulo(number, 2) = 0)
end method test-even?;

define method test-logior
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-logior;

define method test-logxor
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-logxor;

define method test-logand
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-logand;

define method test-lognot
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-lognot;

define method test-logbit?
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-logbit?;

define method test-ash
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-ash;

define method test-lcm
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-lcm;

define method test-gcd
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-gcd;

define method test-limited
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method test-limited;


/// Don't test the functions we are already testing... there must be a better way!

define arithmetic function-test odd? () end;
define arithmetic function-test even? () end;
define arithmetic function-test zero? () end;
define arithmetic function-test positive? () end;
define arithmetic function-test negative? () end;
define arithmetic function-test integral? () end;
define arithmetic function-test \+ () end;
define arithmetic function-test \* () end;
define arithmetic function-test \- () end;
define arithmetic function-test \/ () end;
define arithmetic function-test negative () end;
define arithmetic function-test floor () end;
define arithmetic function-test ceiling () end;
define arithmetic function-test round () end;
define arithmetic function-test truncate () end;
define arithmetic function-test floor/ () end;
define arithmetic function-test ceiling/ () end;
define arithmetic function-test round/ () end;
define arithmetic function-test truncate/ () end;
define arithmetic function-test modulo () end;
define arithmetic function-test remainder () end;
define arithmetic function-test \^ () end;
define arithmetic function-test abs () end;
define arithmetic function-test logior () end;
define arithmetic function-test logxor () end;
define arithmetic function-test logand () end;
define arithmetic function-test lognot () end;
define arithmetic function-test logbit? () end;
define arithmetic function-test ash () end;
define arithmetic function-test lcm () end;
define arithmetic function-test gcd () end;
