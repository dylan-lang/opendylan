Module:    dfmc-reader
Synopsis:  Code in support of the parser - some collecting datastructures 
           and binary operand series handling.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Utility collections.

define method append-sequence (value) => (seq :: <pair>)
  let cell = list(value);
  pair(cell, cell)
end method;

define method append-element! (seq :: <pair>, value) => (seq :: <pair>)
  let new-cell = list(value);
  let last-cell = seq.tail;
  seq.tail := last-cell.tail := new-cell;
  seq
end method;

define method append-sequence (value :: <list>) => (seq :: <pair>)
  let last-cell = last-pair(value);
  pair(value, last-cell)
end method;

define method append-element! 
    (seq :: <pair>, value :: <list>) => (seq :: <pair>)
  let new-cells = value;
  let last-cell = seq.tail;
  last-cell.tail := new-cells;
  seq.tail := new-cells.last-pair;
  seq
end method;

define function last-pair (l :: <list>) => (p :: <pair>)
  if (empty?(l.tail)) l else last-pair(l.tail) end;
end function;

/*
define method append-2-elements! 
    (seq :: <pair>, value1, value2) => (seq :: <pair>)
  let last-new-cell = list(value2);
  let first-new-cell = pair(value1, last-new-cell);
  let last-cell = seq.tail;
  last-cell.tail := first-new-cell;
  seq.tail := last-new-cell;
  seq
end method;
*/

define method elements (seq :: <pair>) => (contents :: <list>)
  seq.head
end method;

//// Binop sequences.

// Basics:
//
// Existing binops are folded when possible as new binops are appended. 
// You can only fold the top binop if the new binop has a lower
// precedence (or if their precedence is the same and the ops
// in question are right-associative) - e.g. in a * b + c, we know
// we can fold a * b on seeing +.
//
// Pending binops on the stack must be of monotonically non-decreasing
// precedence. A new binop allows all pending binops of higher
// precedence (or favourable associativity) to be folded, only
// stopping when we hit a pending binop which could bind less
// tightly than the new binop - e.g. in a + c * d ^ e * f, we can
// fold the ^ and * (associativity) on seeing * f, but must stop
// at +.
//
// We can be left with pending binops given an input ending with
// ascending precedence - e.g. a + b * c - and must force a reduction
// at the end (logic similar to adding a dummy op with precedence
// below all others, but no test is required since everything pending
// must fold right-to-left).

// Rather than make a binop-sequence structure for every expression,
// we just return the fragment initially and only wrap it if we
// actually see a binary operator.

define class <binop-sequence> (<object>) 
  constant slot binop-previous :: type-union(<fragment>, <binop-sequence>),
    required-init-keyword: previous:;
  constant slot binop-operator :: <operator-fragment>,
    required-init-keyword: operator:;
  slot binop-argument :: <fragment>,
    required-init-keyword: argument:;
end class;

define method binop-fragment (f :: <fragment>) => (f :: <fragment>)
  f
end method;

define method binop-fragment (seq :: <binop-sequence>) => (f :: <fragment>)
  binop-fragment(fold-one-binop!(seq));
end method;

define method append-binop!
    (f :: <fragment>, op :: <operator-fragment>, arg :: <fragment>)
 => (seq :: <binop-sequence>)
  make(<binop-sequence>, previous: f, operator: op, argument: arg);
end method;

define method append-binop!
    (seq :: <binop-sequence>, 
       new-op :: <operator-fragment>, new-arg :: <fragment>)
 => (seq :: <binop-sequence>)
  let new-prec  = operator-precedence(new-op);
  let lead-op   = binop-operator(seq);
  let lead-prec = operator-precedence(lead-op);
  let new-left? = left-associative?(new-op);
  if (lead-prec < new-prec | (lead-prec = new-prec & ~new-left?))
    // Have to shift.
    make(<binop-sequence>, 
         previous: seq, 
         operator: new-op, 
         argument: new-arg);
  else
    // We know we have at least one fold under our belts, so do that
    // one and then see how far we get.
    let reduced 
      = fold-tighter-binops(fold-one-binop!(seq), new-prec, new-left?);
    append-binop!(reduced, new-op, new-arg);
  end;
end method;

define method fold-tighter-binops
    (f :: <fragment>, prec :: <integer>, left? :: <boolean>) 
 => (f :: <fragment>)
  f
end method;

define method fold-tighter-binops
    (seq :: <binop-sequence>, prec :: <integer>, left? :: <boolean>) 
 => (seq-or-f :: type-union(<fragment>, <binop-sequence>))
  let lead-prec = operator-precedence(binop-operator(seq));
  if (lead-prec > prec | (lead-prec = prec & left?))
    fold-tighter-binops(fold-one-binop!(seq), prec, left?);
  else
    seq
  end;
end method;

define method fold-one-binop! (seq :: <binop-sequence>) => (seq)
  fold-one-binop-aux!(binop-previous(seq), seq);
end method;

define method fold-one-binop-aux! 
    (left-arg :: <fragment>, seq :: <binop-sequence>)
 => (folded :: <compound-fragment>)
  let op        = binop-operator(seq);
  let right-arg = binop-argument(seq);
  let rec = fragment-record(left-arg);
  let loc = position-between(left-arg, right-arg);
  if (macro-operator-fragment?(op))
    make(<function-macro-fragment>,
	 record: rec,
         source-position: loc,
         macro: op,
         body-fragment: 
           list(left-arg, 
                make(<comma-fragment>, record: rec, source-position: loc),
                right-arg));
  else
    make(<binary-operator-call-fragment>,
	 record: rec,
         source-position: loc,
         function: op,
         arguments: list(left-arg, right-arg));
  end;
end method;

define method macro-operator-fragment? (op :: <operator-fragment>)
  select (fragment-name(op))
    #":=", #"&", #"|" => #t;
    otherwise       => #f;
  end;
end method;

define method fold-one-binop-aux! 
    (previous :: <binop-sequence>, last :: <binop-sequence>)
  let fragment = fold-one-binop-aux!(binop-argument(previous), last);
  binop-argument(previous) := fragment;
  previous  
end method;

//// Operator parameters.

// TODO: Put this information into table form.

define method operator-precedence (s)
  select (s)
    #"^" => 9;
    #"*", #"/" => 7;
    #"+", #"-" => 5;
    #"=", #"~=", #"==", #"~==", #"<", #">", #"<=", #">=" => 4;
    #"&", #"|" => 2;
    #":=" => 1;
  end;
end method;

define method right-associative? (s)
  select (s)
    #":=", #"^" => #t;
    otherwise => #f;
  end;
end method;

define method left-associative? (s)
  ~right-associative?(s);
end method;

define method present (f :: <symbol>, s :: <stream>)
  write(s, as-lowercase(as(<string>, f)))
end method;

define method operator-precedence (f :: <operator-fragment>)
  operator-precedence(fragment-name(f))
end method;

define method right-associative? (f :: <operator-fragment>)
  right-associative?(fragment-name(f))
end method;

//// Testing.

/*

define function binops (#rest stuff)
  let s = make(<integer-fragment>, 
	       record: #f,
               source-position: #f,
               value: stuff.head);
  for (cursor = stuff.tail then cursor.tail.tail, until: empty?(cursor))
    s := append-binop!(s, cursor.first, 
                       make(<integer-fragment>, 
			    record: #f,
                            source-position: #f,
                            value: cursor.second));
  end;
  binop-fragment(s);
end function;

define macro the-binop-sequence
  { the-binop-sequence ?stuff:* end }
    => { binops(?stuff) }
stuff:
  { ?:token }
    => { ?token }
  { ?:token ?op:token ... }
    => { ?token, ?op, ... }
op:
  { + } => { #"+" }
  { - } => { #"-" }
  { * } => { #"*" }
  { / } => { #"/" }
  { ^ } => { #"^" }
end macro;

*/

// eof
