module:    harp-utils
Synopsis:  Vectors of bits
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// Dummy definition of a bit-set for now ...

define constant <vector-32bit> = <simple-object-vector>;


define constant $int-size$ = 16;  // For now, use 16 bits per word

define constant $log-int-size$ = 4;

define constant $int-size-mask$ = lognot(ash(-1, $log-int-size$));


// make bit set vectors in units of 10 words

define constant $bit-set-word-size$ = 10;

define constant $bit-unit-size$ = 10 * $int-size$;

define constant $bit-set-zero$ = 0;

define method make-bit-set (length :: <integer>) => (new :: <vector-32bit>)
  for (word-length :: <integer> from 0 by $bit-set-word-size$,
       len :: <integer> from length to 0 by - $bit-unit-size$)
    finally make(<vector-32bit>, size: word-length, fill: 0);
  end for;
end;

define method set-bit-in-set 
    (bit-set :: <vector-32bit>, num :: <integer>) => (num :: <integer>)
  let offset :: <integer> = ash(num, - $log-int-size$);
  let the-bit :: <integer> = ash(1, logand(num, $int-size-mask$));
  bit-set[offset] := logior(bit-set[offset], the-bit);
  num;
end;

define method stretchy-bit-set(bit-set :: <vector-32bit>, new-size :: <integer>)
 => (new-bit-set :: <vector-32bit>)
  let bit-set-size :: <integer> = bit-set.size;
  let new-bit-set :: <vector-32bit> = make(<vector-32bit>, size: new-size, fill: $bit-set-zero$);
  for (i :: <integer> from 0 below bit-set-size)
    new-bit-set[i] := bit-set[i]
  end for;
  new-bit-set
end method;

define method set-bit-in-set! 
    (bit-set :: <vector-32bit>, num :: <integer>) => (bit-set :: <vector-32bit>)
  let offset :: <integer> = ash(num, - $log-int-size$);
  let bit-set-size :: <integer> = bit-set.size;
  let bit-set :: <vector-32bit> =
    if (offset < bit-set-size)
      bit-set
    else
      stretchy-bit-set(bit-set, offset + 1);
    end if;
  let the-bit :: <integer> = ash(1, logand(num, $int-size-mask$));
  bit-set[offset] := logior(bit-set[offset], the-bit);
  bit-set;
end;

define method unset-bit-in-set 
    (bit-set :: <vector-32bit>, num :: <integer>) => (num :: <integer>)
  let offset :: <integer> = ash(num, - $log-int-size$);
  let the-bit :: <integer> = ash(1, logand(num, $int-size-mask$));
  bit-set[offset] := logand(bit-set[offset], lognot(the-bit));
  num;
end;


define method get-bit-from-set
     (bit-set :: <vector-32bit>, num :: <integer>) => (bit? :: <boolean>)
  let offset :: <integer> = ash(num, - $log-int-size$);
  let the-bit :: <integer> = ash(1, logand(num, $int-size-mask$));
  let bit-val :: <integer> = logand(bit-set[offset], the-bit);
  ~(bit-val == 0);
end;

define method get-bit-from-set!
     (bit-set :: <vector-32bit>, num :: <integer>) => (bit? :: <boolean>)
  let offset :: <integer> = ash(num, - $log-int-size$);
  let bit-set-size :: <integer> = bit-set.size;
  if (offset < bit-set-size)
    let the-bit :: <integer> = ash(1, logand(num, $int-size-mask$));
    let bit-val :: <integer> = logand(bit-set[offset], the-bit);
    ~(bit-val == 0);
  else
    #f
  end if;
end;

define method bit-set-or
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    set1[i] := logior(set1[i], set2[i]);
  end for;
end;

define method bit-set-or!
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => (set1 :: <vector-32bit>)
  let set1-size :: <integer> = set1.size;
  let set2-size :: <integer> = set2.size;
  let set1 :: <vector-32bit> =
    if (set2-size > set1-size)
      stretchy-bit-set(set1, set2-size);
    else
      set1
    end if;
  for (i :: <integer> from 0 below set2-size)
    set1[i] := logior(set1[i], set2[i]);
  end for;
  set1
end;


define method bit-set-and
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    set1[i] := logand(set1[i], set2[i]);
  end for;
end;

define method bit-set-xor
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    set1[i] := logxor(set1[i], set2[i]);
  end for;
end;

define method bit-set-andc2
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    set1[i] := logand(set1[i], lognot(set2[i]));
  end for;
end;

define method bit-set-or-andc2
     (set1 :: <vector-32bit>, set2 :: <vector-32bit>, set3 :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set1))
    set1[i] :=
      logior(set1[i], logand(set2[i], lognot(set3[i])));
  end for;
end;

define method copy-bit-set (dst :: <vector-32bit>, src :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(dst))
    dst[i] := src[i];
  end for;
end;

define method clear-bit-set (set :: <vector-32bit>) => ()
  for (i :: <integer> from 0 below size(set))
    set[i] := 0;
  end for;
end;


define method bit-set-as-vector(set :: <vector-32bit>) => (vector :: <simple-object-vector>)
  set
end method;

define method vector-as-bit-set(vector :: <simple-object-vector>) => (set :: <vector-32bit>)
  vector
end method;

define method pack-bitset(set :: <vector-32bit>) => (value)
  let vector :: <simple-object-vector> = bit-set-as-vector(set);
  select(vector.size by \=)
    0 => 0;
    1 => vector[0];
    otherwise => vector;
  end select;
end method;

define method unpack-value-as-bitset(value) => (set :: <vector-32bit>)
  select (value by instance?)
    <integer> =>
      make(<vector-32bit>, size: 1, fill: value);
    <simple-object-vector> => vector-as-bit-set(value);
  end select;
end method;
