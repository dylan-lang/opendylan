module:       subseq
rcs-header:   $Header: /scm/cvs/fundev/Sources/lib/collection-extensions/subseq.dylan,v 1.1 2004/03/12 00:08:44 cgay Exp $
Author:       Robert Stockton (rgs@cs.cmu.edu)
synopsis:     Provides "subsequences", which represent an aliased reference to
              some part of an existing sequence.  These are analogous to
              slices (in Ada or Perl) or displaced arrays (in Common Lisp).
              Subsequences are themselves subclasses of <sequence>, and can
              therefore be passed any <collection> or <sequence> operation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

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

define abstract class <subsequence> (<sequence>)
   constant slot source           :: <sequence>,
     required-init-keyword: source: ;
   constant slot start-index      :: <integer>, 
     required-init-keyword: start: ;
   // end-index is simply an upper bound, except in the case of
   // <vector-subsequence>s. 
   constant slot end-index        :: <integer>, 
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
  let sz = size(seq);
  let subseq-last = if (last & last < sz) last else sz end if;
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

define constant gs-fip-next-state =
  method (c, s)
    head(s) := c.next-state(c.source, head(s));
    tail(s) := tail(s) + 1;
    s;
  end method;

define constant gs-fip-done? =
  method (c, s, l)
    c.finished-state?(c.source, head(s), l) | tail(s) >= c.end-index;
  end method;

define constant gs-fip-current-key =
  method (c, s) tail(s) - c.start-index end method;

define constant gs-fip-current-element =
  method (c, s) c.current-elem(c.source, head(s)) end method;

define constant gs-fip-current-element-setter =
  method (v, c, s)
    c.current-elem-sttr(v, c.source, head(s));
  end method;

define constant gs-fip-copy-state =
  method (c, s) pair(c.copy-state(head(s)), tail(s)) end method;

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
   let seq-size = size(seq);
   let subseq-last = if (last) min(last, seq-size) else seq-size end;
  if (instance?(seq, <string>)) 
    make(<vs-subsequence>, source: seq, start: first, end: subseq-last);
  else
    make(<vector-subsequence>, source: seq, start: first, end: subseq-last);
  end if;
end method subsequence;

define constant vs-fip-next-element =
  method (c :: <subsequence>, s :: <integer>) => (result :: <integer>);
    s + 1;
  end method;

define constant vs-fip-done? =
  method (c :: <subsequence>, s :: <integer>, l :: <integer>)
    s >= l;
  end method;

define constant vs-fip-current-key =
  method (c :: <subsequence>, s :: <integer>) => (result :: <integer>);
    s - c.start-index;
  end method;

define constant vs-fip-current-element =
  method (c :: <subsequence>, s :: <integer>)
    c.source[s];
  end method;

define constant vs-fip-current-element-setter =
  method (e, c :: <subsequence>, s :: <integer>)
    c.source[s] := e;
  end method;

define constant vs-fip-copy-state =
  method (c :: <subsequence>, s :: <integer>) => (result :: <integer>);
    s;
  end method;

define method forward-iteration-protocol (seq :: <subsequence>)
 => (initial-state :: <object>, limit :: <object>, next-state :: <function>,
     finished-state? :: <function>, current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
   values(seq.start-index, seq.end-index, vs-fip-next-element, vs-fip-done?,
	  vs-fip-current-key, vs-fip-current-element,
	  vs-fip-current-element-setter, vs-fip-copy-state);
end method forward-iteration-protocol;

define method size(c :: <vector-subsequence>) => (result :: <integer>);
   c.end-index - c.start-index;
end method size;

define method aref(c :: <vector-subsequence>,
		   #rest rest) => (result :: <object>);
   let index = rest[0];
   if ((index < 0) | (index >= c.end-index - c.start-index))
      signal("index out of bounds");
   else
      aref(c.source, index + c.start-index);
   end if;
end method;

define method aref-setter(value, c :: <vector-subsequence>,
			  #rest rest) => (result :: <object>);
   let index = rest[0];
   if ((index < 0) | (index >= c.end-index - c.start-index))
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
  let index = seq.start-index + key;
  case 
    key < 0 | index >= seq.end-index =>
      if (default == subseq-no-default)
	error("No such element in %=: %=", seq, key);
      else
	default
      end if;
    otherwise => seq.source[index];
  end case;
end method element;

define method element-setter(value, seq :: <vector-subsequence>,
			     key :: <integer>) => (result :: <object>);
   case 
      key < 0 | key >= seq.end-index - seq.start-index =>
         error("No such element in %=: %=", seq, key);
      otherwise => seq.source[key + seq.start-index] := value;
   end case;
end method element-setter;

define method subsequence(seq :: <string>,
			  #key start: first = 0,
			       end: last) => (result :: <string-subsequence>);
  let seq-size = size(seq);
  let subseq-last = if (last) min(last, seq-size) else seq-size end;
  
  if (instance?(seq, <vector>)) 
    make(<vs-subsequence>, source: seq, start: first, end: subseq-last);
  else
    make(<string-subsequence>, source: seq, start: first, end: subseq-last);
  end if;
end method subsequence;
