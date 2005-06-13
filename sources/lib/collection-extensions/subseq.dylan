module:       subseq
Author:       Robert Stockton (rgs@cs.cmu.edu)
synopsis:     Provides "subsequences", which represent an aliased reference to
              some part of an existing sequence.  These are analogous to
              slices (in Ada or Perl) or displaced arrays (in Common Lisp).
              Subsequences are themselves subclasses of <sequence>, and can
              therefore be passed any <collection> or <sequence> operation.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998 - 2004  Gwydion Dylan Maintainers
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
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

//============================================================================
// <Subsequence> is a new subclass of <sequence>.  A subsequence represents an
// aliased reference to some part of an existing sequence.  Although they may
// be created by make (with required keywords source:, start: and end:) on one
// of the instantiable subclasses, they are more often created by calls of the
// form 
// 
//   subsequence(sequence, start: 0, end: 3)
// 
// where start: and end: are optional keywords which default to the beginning
// and end, respectively, of the source sequence.  No other new operations are
// defined for subsequences, since all necessary operations are inherited from
// <sequence>.  
// 
// Because subsequences are aliased references into other sequences, several
// properties must be remembered:  
// 
//  1. The contents of a subsequence are undefined after any destructive
//     operation upon the source sequence.  
//  2. Destructive operations upon subsequences may be reflected in the
//     source.  The results of reverse! and sort! should be expected to affect
//     the source sequence for vector subsequences.  
// 
// If the source sequences are instances of <vector> or <string>, then the
// implementation will use subclasses of <subsequence> which are also
// subclasses of <vector> or <string>.  
// 
// Efficiency notes:  
// 
//  1. The implementation tries to insure that subsequences of subsequences
//     can be accessed as efficiently as the original subsequence.  (For
//     example, the result of 
// 
//       subsequence(subsequence(source, start: 1), start: 2)
// 
//     would produce a subsequence identical to the one produced by 
// 
//       subsequence(source, start: 3)
// 
//  2. Vector subsequences, like all other vectors, implement constant time
//     element access.  
//  3. Non-vector subsequences may take non-constant time to create, but will
//     provide constant-time access to the first element.  This should produce
//     the best performance provided some element of the subsequence is
//     accessed at least once.  
//============================================================================

// Notes while reviewing the code ( -- andreas, 20050531)
//
// * Start and end are insufficiently checked.  It is quite easy to generate
//   a subsequence of negative size by subsequence(foo, start: 5, end: 3).
// * Also, passing a negative start gives access to data outside the
//   subsequence the user is allowed to see.
// * The semantics of omitting the sequence end isn't well-defined,
//   especially given stretchy source sequences.
// * The good news: the above problems are not exploitable because everything
//   is bounds-checked twice.
// * The bad news: the performance leaves to be desired because everything
//   is bounds-checked twice.
// * Design decision: signal bounds error at subsequence creation time vs.
//   element access time. The latter is more dynamic, the former gives
//   better performance.
// * Feature wish: Read-only subsequences.


define abstract class <subsequence> (<sequence>)
   constant slot source :: <sequence>,
     required-init-keyword: source: ;
   constant slot start-index :: <integer>,
     required-init-keyword: start: ;
   // end-index is simply an upper bound, except in the case of
   // <vector-subsequence>s. 
   constant slot end-index :: <integer>,
     required-init-keyword: end: ;
end class <subsequence>;

define method subsequence(seq :: <subsequence>,
			  #key start: first = 0,
			       end: last) => (result :: <subsequence>);
   let old-first = seq.start-index;
   let old-last = seq.end-index;
   let subseq-last = if (last) min(last + old-first, old-last)
		     else old-last end if;
   make(object-class(seq), source: seq.source,
	start: first + old-first, end: subseq-last);
end method subsequence;

define method type-for-copy (seq :: <subsequence>) => type :: <type>;
   type-for-copy(seq.source);
end method type-for-copy;

define class <generic-subsequence> (<subsequence>)
  constant slot init-state, required-init-keyword: init:;
  constant slot limit, required-init-keyword: limit:;
  constant slot next-state, required-init-keyword: next:;
  constant slot finished-state?, required-init-keyword: done:;
  constant slot current-elem, required-init-keyword: elem:;
  constant slot current-elem-sttr, required-init-keyword: elem-setter:;
  constant slot copy-state, required-init-keyword: copy:;
end class;
 
define method subsequence(seq :: <sequence>,
			  #key start: first = 0,
			       end: last) => (result ::
						<generic-subsequence>);
  let subseq-last = if (last) last else max(first, seq.size) end if;
  let (init, limit, next, done?,
       key, elem, elem-setter, copy) = forward-iteration-protocol(seq);
  let state = for (i from 0 below first,
		   state = init then next(seq, state),
		   until: done?(seq,state,limit))
	      finally state;
	      end for;
  make(<generic-subsequence>, source: seq, start: first, end: subseq-last,
       init: state, limit: limit, next: next, done: done?, elem: elem,
       elem-setter: elem-setter, copy: copy);
end method subsequence;

define method subsequence(seq :: <generic-subsequence>,
			  #key start: first = 0,
			       end: last) => (result :: <generic-subsequence>);
   let old-first = seq.start-index;
   let old-last = seq.end-index;
   let subseq-last = if (last) min(last + old-first, old-last)
		     else old-last end if;
   let source = seq.source;
   let (limit, next, done?) = values(seq.limit, seq.next-state,
				     seq.finished-state?);
   let state = for (i from 0 below first,
		    state = seq.init-state then next(source, state),
		    until: done?(source,state, limit))
	       finally state;
	       end for;
   make(object-class(seq), source: source, start: first + old-first,
	end: subseq-last, init: state, limit: limit, next: next, done: done?,
	elem: seq.current-elem, elem-setter: seq.current-elem-sttr,
	copy: seq.copy-state);
end method subsequence;

define inline function gs-fip-next-state (c :: <generic-subsequence>, s)
  head(s) := c.next-state(c.source, head(s));
  tail(s) := tail(s) + 1;
  s;
end function;

define inline function gs-fip-done? (c :: <generic-subsequence>, s, l)
  c.finished-state?(c.source, head(s), l) | tail(s) >= c.end-index;
end function;

define inline function gs-fip-current-key (c :: <generic-subsequence>, s)
  tail(s) - c.start-index;
end function;

define inline function gs-fip-current-element (c :: <generic-subsequence>, s)
  c.current-elem(c.source, head(s));
end function;

define inline function gs-fip-current-element-setter (v, c :: <generic-subsequence>, s)
  c.current-elem-sttr(v, c.source, head(s));
end function;

define inline function gs-fip-copy-state (c :: <generic-subsequence>, s)
  pair(c.copy-state(head(s)), tail(s));
end function;

define method forward-iteration-protocol (seq :: <generic-subsequence>)
 => (initial-state :: <object>, limit :: <object>, next-state :: <function>,
     finished-state? :: <function>, current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
   values(pair(seq.init-state, seq.start-index), seq.limit, gs-fip-next-state,
	  gs-fip-done?, gs-fip-current-key, gs-fip-current-element,
	  gs-fip-current-element-setter, gs-fip-copy-state);
end method forward-iteration-protocol;

define class <vector-subsequence> (<subsequence>, <vector>) end class;
define class <string-subsequence> (<subsequence>, <string>) end class;

// <vs-subsequence> is used for source sequences which are both <vector>s and
// <string>s.  The only such predefined class is <byte-string>.
define class <vs-subsequence> (<string-subsequence>, <vector-subsequence>) end;

define method make(cls == <subsequence>, 
		   #rest keys, #key) => (result :: <vector-subsequence>);
   apply(make, <vector-subsequence>, keys);
end method;

define method subsequence(seq :: <vector>,
			  #key start: first = 0,
			       end: last) => (result :: <vector-subsequence>);
  let subseq-last = if (last) last else max(first, seq.size) end if;
  if (instance?(seq, <string>)) 
    make(<vs-subsequence>, source: seq, start: first, end: subseq-last);
  else
    make(<vector-subsequence>, source: seq, start: first, end: subseq-last);
  end if;
end method subsequence;

define inline function vs-fip-next-element 
    (c :: <subsequence>, s :: <integer>) => (result :: <integer>);
  s + 1;
end function;

define inline function vs-fip-done? 
    (c :: <subsequence>, s :: <integer>, l :: <integer>)
 => (done :: <boolean>);
  s >= l;
end function;

define inline function vs-fip-current-key 
    (c :: <subsequence>, s :: <integer>) => (result :: <integer>);
  s - c.start-index;
end function;

define inline function vs-fip-current-element
    (c :: <subsequence>, s :: <integer>) => (result :: <object>);
  c.source[s];
end function;

define inline function vs-fip-current-element-setter
    (e, c :: <subsequence>, s :: <integer>) => (result :: <object>)
  c.source[s] := e;
end function;

define inline function vs-fip-copy-state
    (c :: <subsequence>, s :: <integer>) => (result :: <integer>);
  s;
end function;

define method forward-iteration-protocol (seq :: <subsequence>)
 => (initial-state :: <object>, limit :: <object>, next-state :: <function>,
     finished-state? :: <function>, current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
   values(seq.start-index, seq.end-index, vs-fip-next-element, vs-fip-done?,
	  vs-fip-current-key, vs-fip-current-element,
	  vs-fip-current-element-setter, vs-fip-copy-state);
end method forward-iteration-protocol;

define inline method size(c :: <vector-subsequence>) => (result :: <integer>);
   c.end-index - c.start-index;
end method size;

define method aref(c :: <vector-subsequence>,
		   #rest rest) => (result :: <object>);
   let index = rest[0];
   if ((index < 0) | (index >= c.size))
      signal("index out of bounds");
   else
      aref(c.source, index + c.start-index);
   end if;
end method;

define method aref-setter(value, c :: <vector-subsequence>,
			  #rest rest) => (result :: <object>);
   let index = rest[0];
   if ((index < 0) | (index >= c.size))
      signal("index out of bounds");
   else
      aref(c.source, index + c.start-index) := value;
   end if;
end method;

define method dimensions(c :: <vector-subsequence>) => (result :: <vector>);
   vector(c.end-index - c.start-index);
end method;

define constant subseq-no-default = pair(#f, #f);

define method element(seq :: <vector-subsequence>, key :: <integer>,
		      #key default = subseq-no-default) => elt :: <object>;
  case 
    key < 0 | key >= seq.size =>
      if (default == subseq-no-default)
	error("No such element in %=: %=", seq, key);
      else
	default
      end if;
    otherwise => seq.source[key + seq.start-index];
  end case;
end method element;

define method element-setter(value, seq :: <vector-subsequence>,
			     key :: <integer>) => (result :: <object>);
   case 
      key < 0 | key >= seq.size =>
         error("No such element in %=: %=", seq, key);
      otherwise => seq.source[key + seq.start-index] := value;
   end case;
end method element-setter;

define method subsequence(seq :: <string>,
			  #key start: first = 0,
			       end: last) => (result :: <string-subsequence>);
  let subseq-last = if (last) last else max(start, seq.size) end;
  
  if (instance?(seq, <vector>)) 
    make(<vs-subsequence>, source: seq, start: first, end: subseq-last);
  else
    make(<string-subsequence>, source: seq, start: first, end: subseq-last);
  end if;
end method subsequence;

