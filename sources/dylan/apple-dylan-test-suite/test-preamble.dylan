Module:    apple-dylan-test-suite
Filename:  test-preamble.dylan
Summary:   Apple dylan test suite, test-preamble
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

define method complex-instance (#key real = 3, imag = 2)
  make(<complex>, real: real, imag: imag)
end method complex-instance;

define method table-instance (#rest contents)
  let v = make(<table>);
  do(method (e)
       v[e.first] := e.second
     end method,
     contents.reverse);
  v
end method table-instance;

define method stretchy-vector-instance (#rest contents)
  let v = make(<stretchy-vector>);
  do(method (e)
       v := add(v, e)
     end method,
     contents.reverse);
  v
end method stretchy-vector-instance;

define method unicode-string-instance (#rest contents)
  let s = make(<unicode-string>, size: contents.size);
  replace-subsequence!(s, contents);
  s
end method unicode-string-instance;

define method byte-string-instance (#rest contents)
  let s = make(<byte-string>, size: contents.size);
  replace-subsequence!(s, contents);
  s
end method byte-string-instance;

define method deque-instance (#rest contents)
  let v = make(<deque>);
  do(method (e)
       v := add(v, e)
     end method,
     contents.reverse);
  v
end method deque-instance;

// compares two sequences to see if they share structure

define method share-struct? (seq1, seq2)
  local method check-id? (a, b)
          if (a.empty? & b.empty?)
            #f
          elseif (a == b)
            #t
          elseif (b.empty?)
            check-id?(a.tail, seq2)
          else
            check-id?(a, b.tail)
          end if
        end method check-id?;
  check-id?(seq1, seq2)
end method share-struct?;

define constant test-variable = 99;

define method always-99 (#rest args)
  99
end method always-99;

define class <dtest-test-class> (<object>)
  slot s;
  slot s1, init-value: 99, init-keyword: s1:;
  slot s2, init-function: always-99;
end class <dtest-test-class>;

define class <dtest-test-subclass> (<dtest-test-class>)
  slot s0;
end class <dtest-test-subclass>;

define method mutable-explicit-key-collection ()
  list(<mutable-explicit-key-collection>,
       <explicit-key-collection>,
       <mutable-collection>,
       <collection>)
end method mutable-explicit-key-collection;

define method mutable-sequence ()
  list(<mutable-sequence>, <mutable-collection>, <sequence>, <collection>)
end method mutable-sequence;

// A caller in test-class.dylan explicitly wants to compare sequences
// using the iteration protocol rather than via 'element'.  --cgay Aug 2012
define method sequences-element-id? (seq1, seq2)
  let len = seq1.size;
  if (len == seq2.size)
    let (initial-state-1, a, next-state-1, b, c, current-element-1)
      = forward-iteration-protocol(seq1);
    let (initial-state-2, d, next-state-2, e, g, current-element-2)
      = forward-iteration-protocol(seq2);
    local method f (i, s1, s2)
            if (i == len)
              ~s1 & ~s2
            elseif (~s1 | ~s2)
              #f
            elseif (current-element-1(seq1, s1) == current-element-2(seq2, s2))
              f(i + 1, next-state-1(seq1, s1), next-state-2(seq2, s2))
            else
              #f
            end if
          end method;
    f(0, initial-state-1, initial-state-2)
  end if
end method sequences-element-id?;

define variable *as-character* = curry(as, <character>);

define variable *as-integer* = curry(as, <integer>);

define variable *lowercase-alphabet* = "abcdefghijklmnopqrstuvwxyz";

define variable *uppercase-alphabet* = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

define variable *digit-characters* = "0123456789";

define variable *misc-characters* = "`~!@#$%*()-=+:'<>,.?\"\\";





