module:     string-conversions
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Convert strings to numbers and numbers to strings.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /scm/cvs/fundev/Sources/lib/string-extensions/conversions.dylan,v 1.1 2004/03/12 00:09:20 cgay Exp $

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================

// KJP: Sideways.
define sideways method as (the-class == <string>, c :: <character>)
 => c :: <string>;
  make(<string>, size: 1, fill: c);
end method as;

// Either returns or forces an error.
//
define method check-base (base :: <integer>) => ();
  if (base < 2 | base > 36)
    error("%d is not a legal base.", base);
  end if;
end method check-base;

// Integer-to-string checks a few special cases, and lets
// positive-integer-to-string do the real work.
//
/* KJP: Defined by our extensions.
define method integer-to-string 
    (num :: <general-integer>, #key base = 10, uppercase = #f)
 => number :: <string>;
  check-base(base);
  if (num = 0)
    "0";
  elseif (num < 0)
    concatenate("-", positive-integer-to-string(- num, base, uppercase));
  else
    positive-integer-to-string(num, base, uppercase);
  end if;
end method integer-to-string;

define method integer-to-string (num == $minimum-integer, 
				 #key base = 10, uppercase = #f)
 => number :: <string>;
  integer-to-string(as(<extended-integer>, num), 
		    base: base, uppercase: uppercase);
end method integer-to-string;

define method positive-integer-to-string
    (num :: <general-integer>, base :: <integer>, uppercase :: <object>)
 => number :: <string>;
  if (num = 0)
    "";
  else
    let low-digit = modulo(num, base);
    add-last(positive-integer-to-string(truncate/(num, base), base, uppercase),
	     integer-to-digit(low-digit, base: base, uppercase: uppercase));
  end if;
end method positive-integer-to-string;
*/

// Converts an integer into a character.  Bases beyond 36 probably
// don't come out very well, however.
//
define method integer-to-digit
    (integer :: <integer>, #key base = 10, uppercase = #f)
 => digit :: <character>;
  check-base(base);
  if (integer < 0   |   integer >= base)
    error("%d isn't a digit in base %d", integer, base);
  end if;

  select (integer)
    0 => '0';
    1 => '1';
    2 => '2';
    3 => '3';
    4 => '4';
    5 => '5';
    6 => '6';
    7 => '7';
    8 => '8';
    9 => '9';
    otherwise =>
      as(<character>, 
	 integer - 10 + as(<integer>, if (uppercase) 'A' else 'a' end));
  end select;
end method integer-to-digit;

// Interprets the empty string as 0.  This can be either a bug or a
// feature, depending on your viewpoint.
//
// Will signal an error for invalid strings.
//
/* KJP: Defined by our own extensions.
define method string-to-integer (string :: <sequence>, #key base = 10)
 => int :: <general-integer>;
  check-base(base);
  let number :: <extended-integer> = as(<extended-integer>, 0);
  let sign = if (string[0] == '-')  -1  else  1  end if;
  let start-index = if (sign = -1)  1  else  0  end if;
  for (i from start-index below string.size)
    let digit = digit-to-integer(string[i]);
    if (digit >= base)
      error("\"%s\" isn't in base %d\n", string, base);
    else
      number := number * base  + digit;
    end if;
  end for;
  number := sign * number;
  if (number < $minimum-integer | number > $maximum-integer)
    number;
  else
    as(<integer>, number);
  end if;
end method string-to-integer;
*/

define method digit-to-integer (c :: <character>) => digit :: <integer>;
  if (~alphanumeric?(c))
    error("Invalid digit %=", c);
  end if;
  select (c)
    '0' => 0;
    '1' => 1;
    '2' => 2;
    '3' => 3;
    '4' => 4;
    '5' => 5;
    '6' => 6;
    '7' => 7;
    '8' => 8;
    '9' => 9;
    otherwise =>
      as(<integer>, as-lowercase(c)) - as(<integer>, 'a') + 10;
  end select;
end method digit-to-integer;

// Returns a number.  Hopefully it'll return an integer when
// appropriate and a float at other times, but I'm not real sure.
//
// Has no problem accepting strings with multiple decimal points.
// Decimal points beyond the first one are ignored, though.
// Similarly, there can be any number of + or - in the string (and not
// just at the beginning), with the last one determining whether the
// number is positive or negative.
//
/* KJP: Not used.
define method string-to-number (string :: <sequence>, #key base: base = 10)
 => num :: <number>;
  let number = 0;
  let negate = 1;
  let seen-decimal = #f;
  let decimal-divisor = 1;
  for (c in string)
    select (c)
      '-' =>      negate := -1;
      '+' =>      negate := 1;
      '.' =>      seen-decimal := #t;
      otherwise =>
	let digit = digit-to-integer(c);
	if (digit >= base) 
	  error("\"%s\" isn't in base %d\n", string, base);
	elseif (seen-decimal)  
	  decimal-divisor := decimal-divisor * base;
	  number := number + as(<float>, digit) / as(<float>, decimal-divisor);
	else 
	  number := number * base  + digit;
	end if;
    end select;
  end for;

  number * negate;
end method string-to-number;
*/
