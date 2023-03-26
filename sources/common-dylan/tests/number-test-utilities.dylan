Module: common-dylan-test-utilities
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open generic test-number-class
    (class :: subclass(<number>), #key abstract?) => ();

define method test-number-class
    (class :: subclass(<number>), #key abstract?) => ()
  unless (abstract?)
    test-number("0",  as(class, 0));
    test-number("1",  as(class, 1));
    test-number("2",  as(class, 2));
    test-number("-1", as(class, -1));
    test-number("-2", as(class, -2));
  end
end method test-number-class;

//--- An extra method to test floating point values
define method test-number-class
    (class :: subclass(<float>), #key abstract?) => ()
  next-method();
  unless (abstract?)
    test-number("1.5", as(class, 1.5));
    test-number("-1.5", as(class, -1.5));
  end
end method test-number-class;

define method test-number-class
    (class == <integer>, #key abstract?) => ()
  next-method();
  test-limited-integers();
end method;


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
            do-test-=,
            do-test-zero?,
            do-test-+,
            do-test-*,
            do-test--,
            do-test-/,
            do-test-^
            ))
end method test-number;

define method test-number
    (name :: <string>, number :: <real>) => ()
  next-method();
  do(method (function) function(name, number) end,
     vector(// Functions on <real>
            do-test-floor,
            do-test-ceiling,
            do-test-round,
            do-test-truncate,
            do-test-floor/,
            do-test-ceiling/,
            do-test-round/,
            do-test-truncate/,
            do-test-modulo,
            do-test-remainder,

            // Methods on <real>
            do-test-<,
            do-test-abs,
            do-test-positive?,
            do-test-negative?,
            do-test-integral?,
            do-test-negative
            ))
end method test-number;

define method test-number
    (name :: <string>, number :: <integer>) => ()
  next-method();
  do(method (function) function(name, number) end,
     vector(// Functions on <integer>
            do-test-odd?,
            do-test-even?,
            do-test-logior,
            do-test-logxor,
            do-test-logand,
            do-test-lognot,
            do-test-logbit?,
            do-test-ash,

            // Methods on <integer>
            do-test-lcm,
            do-test-gcd,
            do-test-limited
            ))
end method test-number;


/// Number testing

//---*** These methods should be on <complex> but the hierarchy isn't correct
//---*** in the emulator at least so we do it on <number> instead.

define method do-test-= 
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method;

define method do-test-zero? 
    (name :: <string>, number :: <number>) => ()
  check-equal(format-to-string("zero?(%d)", number),
              zero?(number),
              integral?(number) & number < 1 & number > -1)
end method;

define method do-test-+
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method;

define method do-test-* 
    (name :: <string>, number :: <number>) => ()
  check-equal(format-to-string("%d * 1 = %d", number, number),
              number * 1, number)
end method;

define method do-test-- 
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method;

define method do-test-/
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method;

define method do-test-^
    (name :: <string>, number :: <number>) => ()
  //---*** Fill this in...
end method;


/// Real number testing
define method do-test-floor
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-ceiling
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-round
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-truncate
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-floor/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-ceiling/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-round/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-truncate/
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-modulo
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-remainder
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-<
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-abs
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-positive?
    (name :: <string>, number :: <real>) => ()
  check-equal(format-to-string("positive?(%d)", number),
              positive?(number),
              number > 0)
end method;

define method do-test-negative?
    (name :: <string>, number :: <real>) => ()
  check-equal(format-to-string("negative?(%d)", number),
              negative?(number),
              number < 0)
end method;

define method do-test-integral?
    (name :: <string>, number :: <real>) => ()
  //---*** Fill this in...
end method;

define method do-test-negative
    (name :: <string>, number :: <real>) => ()
  check-equal(format-to-string("negative(negative(%d)) = %d", number, number),
              negative(negative(number)),
              number)
end method;


/// Integer number testing

define method do-test-odd?
    (name :: <string>, number :: <integer>) => ()
  check-equal(format-to-string("odd?(%d)", number),
              odd?(number),
              modulo(number, 2) = 1)
end method;

define method do-test-even?
    (name :: <string>, number :: <integer>) => ()
  check-equal(format-to-string("even?(%d)", number),
              even?(number),
              modulo(number, 2) = 0)
end method;

define method do-test-logior
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-logxor
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-logand
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-lognot
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-logbit?
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-ash
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-lcm
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-gcd
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;

define method do-test-limited
    (name :: <string>, number :: <integer>) => ()
  //---*** Fill this in...
end method;


define method test-limited-integers () => ()
  test-limited-integer-instance?();
end method test-limited-integers;

// This is used below to hide some info from the compiler
// so that we don't get a compile time warning in our test
// for some run-time behavior.
define not-inline function hide-type-info(o) => (o)
  o
end;

define macro check-limited-integer-instance?
  { check-limited-integer-instance?(?limited-type:expression) }
  =>
  {
    begin
      let name = ?"limited-type";
      let limited-type = ?limited-type;
      let lower-bound = limited-type.limited-integer-min;
      if (lower-bound)
        assert-instance?(?limited-type, lower-bound);
        assert-not-instance?(?limited-type, lower-bound - 1);
        assert-instance?(?limited-type, lower-bound + 1);
        assert-no-errors(begin
                           let x :: ?limited-type = lower-bound;
                         end);
        assert-signals(<type-error>,
                       begin
                         let x :: ?limited-type = lower-bound - 1;
                       end);
        assert-no-errors(begin
                           let x :: ?limited-type = lower-bound + 1;
                         end);
      end if;
      let upper-bound = limited-type.limited-integer-max;
      if (upper-bound)
        assert-instance?(?limited-type, upper-bound);
        assert-instance?(?limited-type, upper-bound - 1);
        assert-not-instance?(?limited-type, upper-bound + 1);
        assert-no-errors(begin
                           let x :: ?limited-type = upper-bound;
                         end);
        assert-no-errors(begin
                           let x :: ?limited-type = upper-bound - 1;
                         end);
        assert-signals(<type-error>,
                       begin
                         let x :: ?limited-type = upper-bound + 1;
                       end);
      end if;
      assert-not-instance?(?limited-type, "Howdy!");
      assert-not-instance?(?limited-type, 1.0);
      assert-signals(<type-error>,
                     begin
                       let x :: ?limited-type = hide-type-info("Howdy!");
                     end);
      assert-signals(<type-error>,
                     begin
                       let x :: ?limited-type = hide-type-info(1.0);
                     end);
    end;
  }
end macro check-limited-integer-instance?;

define method test-limited-integer-instance? () => ()
  check-limited-integer-instance?(limited(<integer>, min: 0));
  check-limited-integer-instance?(limited(<integer>, min: 1));
  check-limited-integer-instance?(limited(<integer>, min: 0, max: 255));
  check-limited-integer-instance?(limited(<integer>, min: 1, max: 100000000));
  check-limited-integer-instance?(limited(<integer>, min: -128, max: 128));
  check-limited-integer-instance?(limited(<integer>, max: 0));
end method test-limited-integer-instance?;

