Module:       common-dylan-internals
Author:       Kevin Mitchell and Seth LaForge
Synopsis:     A 29-bit parallel LFSR seeded by simple LCG, based on Sedgewick.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $recent-size :: <integer> = 54;

define class <random> (<object>)
  constant slot recent :: <simple-object-vector>
    = make(<simple-object-vector>, size: $recent-size, fill: 0);
  slot recent-index :: <integer> = 0;
end class <random>;

// 16 bits (almost) of lame-o pseudo-randomness
// Taken from Table 16.1, Applied Cryptography 2nd ed.
define function lcg-rand (prev :: <integer>) => (r :: <integer>)
  modulo(prev * 171 + 11213, 53125)
end function lcg-rand;

define method initialize (r :: <random>, #key seed = default-random-seed()) 
		      => ()
  let a = r.recent;
  // Ensure that there's at least one 1 in each LFSR:
  a[0] := $maximum-integer;
  let prev = modulo(abs(seed), 32768);
  for (j :: <integer> from 1 below $recent-size)
    let v1 = lcg-rand(prev);
    let v2 = lcg-rand(v1);
    prev := v2;
    // Fill a[j] with 29 random bits as LFSR seed.
    a[j] := modulo(v1, 32768) + modulo(v2, 16384) * 32768;
  end;

  // Make sure the history is full of our stuff to deal with bad seed:
  for (j :: <integer> from 0 to 4 * $recent-size)
    random-29(r);
  end for;
end method initialize;

define function random-29 (r :: <random>) => (v :: <integer>)
  let ri :: <integer> = r.recent-index := 
  	modulo(r.recent-index + 1, $recent-size);
  let a = r.recent;
  local method tap (bit :: <integer>) => (value :: <integer>)
    a[modulo(ri + ($recent-size - bit), $recent-size)]
  end method tap;
  // We're using primitive polynomial x^54 + x^8 + x^6 + x^3 + x^0, so we tap
  // bits 54, 8, 6, and 3.  Bits in recent are stored in reverse order, so 
  // the index of bit n is (ri - n) % 54 = (ri + (54 - n)) % 54.
  a[ri] := logxor(tap(54), tap(8), tap(6), tap(3))
end function random-29; 

define constant $default-random = make(<random>);

define method random
	  (range :: <integer>, #key random: r :: <random> = $default-random)
       => (new :: <integer>)
  let M = $maximum-integer;
  let limit = M - modulo(M - range + 1, range);
  local method find-good-value () => (value :: <integer>)
    let value = random-29(r);
    if (value <= limit)
      value
    else
      find-good-value()
    end
  end method find-good-value;
  modulo(find-good-value(), range)
end method random;
