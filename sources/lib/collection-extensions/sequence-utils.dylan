module:    Sequence-Utilities
author:    Matthias Hölzl (tc@xantira.com)
copyright: see below
version:   0.1 10 Apr 2004
synopsis:  This Module implements some useful methods on sequences.

// Copyright.
// =========

// Copyright (C) 1998-2004 Matthias Hölzl.

//  Use and copying of this software and preparation of derivative
//  works based on this software are permitted, including commercial
//  use, provided that the following conditions are observed:
// 
//  1. This copyright notice must be retained in full on any copies
//     and on appropriate parts of any derivative works. (Other names
//     and years may be added, so long as no existing ones are removed.)
// 
//  This software is made available "as is".  Neither the authors nor
//  Carnegie Mellon University make any warranty about the software,
//  its performance, or its conformity to any specification.
// 
//  Bug reports, questions, comments, and suggestions should be sent by
//  E-mail to the Internet address "gd-bugs@gwydiondylan.org".

// If you need to receive this library under another license contact
// the author (tc@xantira.com).

// PUSH! -- add an element to the front of a list.
//
define macro push!
    { push!(?location:expression, ?value:expression) }
 => { ?location := pair(?value, ?location) }
end macro push!;

// POP! -- remove the first element of a list.
//
define macro pop!
    { pop!(?location:expression) }
 => { let tmp = head(?location);
      ?location := tail(?location);
      tmp }
end macro pop!;

// PAIR? -- check wether ARG is a pair.
//
define method pair? (arg :: <pair>)
  #t;
end method pair?;

define method pair? (arg :: <object>)
  #f;
end method pair?;

// NULL? -- check whether ARG is the empty list.
//
// NULL?(lst) iff ~PAIR?(lst) holds for all lists, but not for other
// values.
//
define method null? (arg :: <empty-list>)
  #t;
end method null?;

define method null? (arg :: <object>)
  #f;
end method null?;

// LIST? -- check whether ARG is a list.
//
define method list? (arg :: <list>)
  #t;
end method list?;

define method list? (arg :: <object>)
  #f;
end method list?;

// XPAIR -- occasionally useful as a value to be passed to a fold or
// other higher-order procedure.
//
define function xpair (list :: <list>, elt) => new-list :: <list>;
  pair(elt, list);
end function xpair;

// TABULATE -- make a sequence of type TYPE whose i-th element is
// FUNC(i) for 0 <= i < LENGTH.  TYPE must be a subtype of
// <MUTABLE-SEQUENCE>.
//
define function tabulate
    (length :: <integer>, func :: <function>,
     #key type = <list>)
  if (type = <list>)
    list-tabulate(length, func);
  else
    let res = make(type, size: length);
    for (i  from 0 below length)
      res[i] := func(i);
    end for;
    res;
  end if;
end function tabulate;

// LIST-TABULATE -- make a list of length LENTH. The i-th element is
// FUNC(i) for 0 <= i < LENGTH.
//.
define function list-tabulate
    (length :: <integer>, func :: <function>)
 => new-list :: <list>;
  let res = #();
  for (i from length - 1 to 0 by -1)
    res := pair(func(i), res);
  end for;
  res;
end function list-tabulate;

// LIST* -- like LIST, but cons the first elements onto the last
// element of REST.
//
define function list* (#rest rest) => new-list :: <list>;
  let res = rest[rest.size - 1];
  for (i from rest.size - 2 to 0 by -1)
    res := pair(rest[i], res);
  end for;
  res;
end function list*;

// TAKE -- if K > 0 return a new sequence consisting of the first K
// elements of SEQ, otherwise return a new sequence consisting of the
// last K elements of SEQ.
//
define open generic take (collection, k :: <integer>);

define method take (seq :: <sequence>, k :: <integer>)
 => new-sequence :: <sequence>;
  if (negative?(k))
    copy-sequence(seq, start: seq.size + k);
  else
    copy-sequence(seq, end: k + 1);
  end if;
end method take;

// DROP -- if K > 0 return a new sequence consisting of all but the
// first K elements of SEQ, otherwise return a new sequence consisting
// of all but the K last elements of SEQ.
//
define open generic drop (collection, k :: <integer>);

define method drop (seq :: <sequence>, k :: <integer>)
 => new-sequence :: <sequence>;
  if (negative?(k))
    copy-sequence(seq, end: seq.size + k);
  else
    copy-sequence(seq, start: k);
  end if;
end method drop;

// LAST-PAIR -- return the last pair in a non-empty list.
//
define function last-pair (lst :: <pair>) => last-pair :: <pair>;
  if (empty?(tail(lst)))
    lst;
  else
    last-pair(tail(lst));
  end if;
end function last-pair;

// REVERSE-APPEND -- append the reversed REVERSED-HEAD to the TAIL.
//
define open generic reverse-append
    (reversed-head :: <sequence>, tail :: <sequence>)
 => new-sequence :: <sequence>;

define method reverse-append
    (rev-head :: <sequence>, tail :: <sequence>)
 => new-sequence :: <sequence>;
  concatenate(reverse(rev-head), tail);
end method reverse-append;

define method reverse-append
    (rev-head :: <list>, tail :: <list>)
 => list :: <list>;
  foldl(pair, tail, rev-head);
end method reverse-append;

// UNFOLD -- basic list constructor; see the implementation for its
// recursion scheme.
//
define function unfold
    (pred :: <function>, f :: <function>, g :: <function>, seed)
 => new-list :: <list>;
  local method recur (seed)
	  if (pred(seed))
	    #();
	  else
	    pair(f(seed), recur(g(seed)));
	  end if;
	end method;
  recur(seed);
end function unfold;

// UNFOLD/TAIL
//
define function unfold/tail
    (pred :: <function>, f :: <function>, g :: <function>,
     e :: <function>, seed)
 => new-list :: <list>;
  local method recur (seed)
	  if (pred(seed))
	    e(seed);
	  else
	    pair(f(seed), recur(g(seed)));
	  end if;
	end method;
  recur(seed);
end function unfold/tail;

// FOLDL -- if LST is #(e1, ..., en) then foldl(lst) returns
// cons(en, cons(..., cons(e2, cons(e1, nil) ...))).
//
// Should we define FOLDL, FOLDR, etc. for arbitrary sequences?
//
define function foldl (cons :: <function>, nil, lst :: <list>)
  local method recur (lst :: <list>, acc)
	  if (pair?(lst))
	    recur(tail(lst), cons(head(lst), acc));
	  else
	    acc;
	  end if;
	end method recur;
  recur(lst, nil);
end function foldl;

// FOLDR -- if LST is #(e1, ..., en) then foldr(lst) returns
// cons(e1, cons(..., cons(en, nil) ...)).
// For example foldr(pair, #(), lst) copies lst.
//
define function foldr (cons :: <function>, nil, lst :: <list>)
  local method recur (lst :: <list>)
	  if (pair?(lst))
	    cons(head(lst), recur(tail(lst)));
	  else
	    nil;
	  end if;
	end method;
  recur(lst);
end function foldr;

// PAIR-FOLDL -- analogous to FOLDL, but applied to successive
// sublists.
//
define function pair-foldl (cons :: <function>, nil, lst :: <list>)
  local method recur (lst :: <list>, acc)
	  if (pair?(lst))
	    recur(tail(lst), cons(lst, acc));
	  else
	    acc;
	  end if;
	end method;
  recur(lst, nil);
end function pair-foldl;

// PAIR-FOLDR -- analogous to FOLDR, but applied to successive
// sublists.
//
define function pair-foldr (cons :: <function>, nil, lst :: <list>)
  local method recur (lst :: <list>)
	  if (pair?(lst))
	    cons(lst, recur(tail(lst)));
	  else
	    nil;
	  end if;
	end method;
  recur(lst);
end function pair-foldr;	  

// REDUCE-L -- a variant of FOLDL. RZERO should be a "right zero" of
// the two-argument procedure F, that is, for any value X acceptable
// to F,
//	f(x, rzero) = x
// REDUCE-L has the following definition:
//	If LIS = (),  return LZERO.
//	If LIS = (x), return X.
// Otherwise, return (foldl f (car x) (cdr x)).  Note that RZERO is
// used *only* in the empty-list case.  You typically use REDUCE-L when
// applying F is expensive and you'd like to avoid the extra
// application incurred when FOLDL applies F to the head of LIS and
// the zero -- for example, if F involves searching a file directory
// or performing a database query, this can be significant. In
// general, however, FOLDL is useful in many contexts where REDUCE-L is
// not (consider the examples given in the FOLDL definition -- only
// one of the five folds uses a function with a right zero. The other
// four may not be performed with REDUCE-L).
//
define function reduce-l (cons :: <function>, nil, lst :: <list>)
  if (pair?(lst))
    foldl(cons, head(lst), tail(lst));
  else
    nil;
  end if;
end function reduce-l;

// REDUCE-R -- like foldr, but NIL is only used in the empty list
// case.
//
define function reduce-r (cons :: <function>, nil, lst :: <list>)
  if (pair?(lst))
    local method recur(hd, lst)
	    if (pair?(lst))
	      cons(hd, recur(head(lst), tail(lst)));
	    else
	      hd;
	    end if;
	  end method;
    recur(head(lst), tail(lst));
  else
    nil;
  end if;
end function reduce-r;

// HEADS -- returns a list of all the heads of members of LISTS.
//
define function heads (lists :: <list>) => new-list :: <list>;
  local method recur (lst :: <list>)
	  if (pair?(lst))
	    pair(head(head(lst)), recur(tail(lst)));
	  else
	    #();
	  end if;
	end method;
  recur(lists);
end function heads;

// TAILS -- returns a list of all the tails of members of LIST.
//
define function tails (lists :: <list>)
  local method recur (lst :: <list>)
	  if (pair?(lst))
	    pair(tail(head(lst)), recur(tail(lst)));
	  else
	    #();
	  end if;
	end method;
  recur(lists);
end function tails;

// CONCATENATE-MAP -- concatenates SEQ and all members of SEQS
// together and maps FUNC over the resulting list.  The order of
// function applications is unspecified.
//
define method concatenate-map
    (func :: <function>, seq :: <sequence>, #rest seqs)
 => new-sequence :: <sequence>;
  map(func, apply(concatenate, seq, seqs));
end method concatenate-map;

// CONCATENATE-MAP -- specialized version for lists.
//
define method concatenate-map
    (func :: <function>, list :: <list>, #rest lists)
 => new-list :: <list>;
  if (empty?(lists)) // fast path
    foldr(method (elt, acc) concatenate(func(list), acc) end,
	  #(), list);
  else 
    local method recur (lists :: <list>)
	    if (every?(pair?, lists))
	      concatenate(apply(func, head(lists)),
			  recur(tails(lists)));
	    else
	      #();
	    end if;
	  end method recur;
    recur(pair(list, as(<list>, lists)));
  end if;
end method concatenate-map;

// PAIR-DO -- similar to do, but FUNC is applied to successive
// sublists.
//
define function pair-do
    (func :: <function>, lst :: <list>, #rest lists)
 => false :: <boolean>;
  if (empty?(lists)) // fast path
    local method recurse (lst)
	    if (pair?(lst))
	      let t = tail(lst); // grab the tail in case FUNC sets it
	      func(lst);
	      recurse(t);
	    end if;
	  end method;
    recurse(lst);
  else
    local method recur (lst)
	    if (every?(pair?, lst))
	      let t = tails(lst);
	      apply(func, lst);
	      recur(t);
	    end if;
	  end method;
    recur(pair(list, as(<list>, lists)));
  end if;
  #f;
end function pair-do;

// CHOOSE-MAP -- map FUNC across LST and save up all the results that
// satisfy PRED.
//
define method choose-map
    (pred :: <function>, func :: <function>,
     seq :: <sequence>, #rest seqs)
 => new-sequence :: <sequence>;
  choose(pred, apply(map, func, seq, seqs));
end method choose-map;

// CHOOSE-MAP -- specialized version for lists.
//
define method choose-map
    (pred :: <function>, func :: <function>,
     lst :: <list>, #rest lists)
 => new-list :: <list>;
  if (empty?(lists)) // fast path
    foldr(method (elt, acc)
	    let res = func(elt);
	    if (pred(res)) pair(res, acc) else acc end if;
	  end method,
	  #(), lst);
  else
    local method recur (lst)
	    if (every?(pair, lst))
	      let t = tails(lst);
	      let res = apply(func, heads(lst));
	      if (pred(res))
		pair(res, recur(tails(lst)));
	      else
		recur(t);
	      end if;
	    else
	      #();
	    end if;
	  end method;
    recur(pair(list, as(<list>, lists)));
  end if;
end method choose-map;

// PARTITION -- partition a list in values that satisfy PRED and in
// values that don't.
//
// This is a rather lame implementation.
//
define function partition (pred :: <function>, seq :: <sequence>)
 => (winners :: <list>, losers :: <list>);
  let (winners, losers) = values(#(), #());
  for (elt in seq)
    if (pred(elt))
      winners := pair(elt, winners);
    else
      losers := pair(elt, losers);
    end if;
  end for;
  values(reverse(winners), reverse(losers));
end function partition;


// ASSOC -- Find the tuple associated with KEY in the association
// sequence SEQ.
//
define function assoc (elt, seq :: <sequence>,
		       #key key = head, test = \=)
  find(method (entry) test(elt, key(entry)) end, seq);
end function assoc;

// APAIR -- cons a new pair #(KEY, DATUM) on the head of ALIST.
//
define function apair (key, datum, aseq :: <sequence>,
		       #key cons = pair, add: add-fun = xpair)
 => new-aseq :: <sequence>;
  add-fun(aseq, cons(key, datum));
end function apair;

// ALIST-COPY -- copy an "alist", actually any sequence that can act
// like an alist.
//
define function alist-copy (alist :: <sequence>,
			    #key key = head, datum = tail,
			    cons = pair)
 => new-alist :: <sequence>;
  map(method (elt) cons(key(elt), datum(elt)) end, alist);
end function alist-copy;

// ALIST-DELETE -- delete all members keyed by ELT-KEY from ALIST.
//
define function alist-delete (elt, alist :: <sequence>,
			      #key key = head, test = \=)
  choose(method (entry) ~test(elt, key(entry)) end,
	 alist);
end function alist-delete;

// SATISFIES -- find the first element that satifies PRED.
//
define method satisfies
    (pred :: <function>, seq :: <sequence>,
     #key failure = #f)
 => index;
  block (return)
    for (entry keyed-by i in seq)
      if (pred(entry)) return(i) end if;
    end for;
    failure;
  end block;
end method satisfies;

// INDEX -- find the position of ELT is SEQ.
//
define method index (elt, seq :: <sequence>,
		     #key test = \=, failure = #f)
 => index;
  satisfies(method (entry) test(elt, entry) end,
	    seq, failure: failure);
end method index;

// FIND -- returns the first true value produced by PRED.
//
define method find (pred :: <function>, seq :: <sequence>,
		    #key failure = #f)
  let res = find-tail(pred, seq);
  if (res) res[0] else #f end;
end method find;

// FIND-TAIL -- returns the subsequence after (and including) the
// first element that satisfies PRED.
//
define method find-tail (pred :: <function>, seq :: <sequence>,
			 #key failure = #f)
  let index = satisfies(pred, seq);
  if (index = 0)
    seq;
  elseif (index)
    copy-sequence(seq, start: index);
  else
    failure;
  end;
end method find-tail;

// FIND-TAIL -- specialized version for lists.
//
define method find-tail (pred :: <function>, lst :: <pair>,
			 #key failure = #f)
  local method recur (elt, lst :: <list>)
	  if (pair?(lst))
	    if (pred(elt)) lst else recur(head(lst), tail(lst)) end;
	  else
	    failure;
	  end if;
	end method recur;
  recur(head(lst), tail(lst));
end method find-tail;

define method find-tail (pred :: <function>, lst :: <empty-list>,
			 #key failure = #f);
  failure;
end method find-tail;

// PRECEDES? -- checks whether ELT-1 precedes ELT-2 in SEQ.
//
define method precedes?(elt-1, elt-2, seq :: <sequence>,
			#key test = \=, not-found = #f)
 => precedes? :: <boolean>;
  let elt-1-seen? = #f;
  let elt-2-seen? = #f;
  block (return)
    for (elt in seq)
      if (test(elt, elt-1))
	if (elt-2-seen?)
	  return(#f);
	else
	  elt-1-seen? := #t;
	end if;
      elseif (test(elt, elt-2))
	if (elt-1-seen?)
	  return(#t);
	else
	  elt-2-seen? := #t;
	end if;
      end if;
    end for;
    not-found;
  end block;
end method precedes?;

// SPLIT-AT -- split a sequence at a token.
//
define function split-at
    (sequence :: <sequence>, token, #key test = \=)
 => split-sequence :: <sequence>;
  let result = make(<stretchy-vector>);
  let current-item = make(<stretchy-vector>);
  for (elt in sequence)
    if (test(elt, token))
      add!(result, current-item);
      current-item := make(<stretchy-vector>);
    else
      add!(current-item, elt);
    end if;
  finally
    // Add the last part.  If the line ends with Token we add
    // an empty sequence.
    add!(result, current-item);
  end for;
  result;
end function split-at;

