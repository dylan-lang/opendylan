module:		dylan-user

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
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

// The library "collection-extensions" contains a variety of small modules
// which are not in the core language but are expected to be generally useful.
// These include new collection classes implementing heaps (i.e. priority
// queues), "self organizing" lists, and subsequences (also known as
// "slices"), and a routines for efficient search and replace on
// <byte-string>s.  Documentation for these routines may be found in
// collection-extension.doc.

define library collection-extensions
  use dylan;
  export heap, self-organizing-list, vector-search, subseq, sequence-diff;
  export SDE-vector;
  export collection-utilities, sequence-utilities;
end library collection-extensions;

define module self-organizing-list
  use dylan;
  // use extensions, import: {<dictionary>};
  export <self-organizing-list>;
end module self-organizing-list;

define module subseq
  use dylan;
  export subsequence, <subsequence>;
end module subseq;  

define module vector-search
  use dylan;
  use subseq;
  export find-first-key, find-last-key;
end module vector-search;

// <heap> presents all sorts of problems with <deque> G.F. signatures.
// But we need the module defined so that we can exclude it...
//
define module heap
/*
  // Since "<heap>" is a subclass of "<sequence>", most methods are simply
  // added to existing generic functions.  The only "new" operation is
  // "random-iteration-protocol".
  use dylan;
  export <heap>, random-iteration-protocol;
*/
end module heap;

define module SDE-vector
  use dylan;
  export <SDE-vector>;
end module SDE-vector;

define module sequence-diff
  use dylan;
  use SDE-vector;
  export sequence-diff, 
    <script-entry>, <insert-entry>, <delete-entry>, 
    element-count, source-index, dest-index;
end module sequence-diff;

define module collection-utilities
  use dylan;
  export singleton?, key-exists?;
end module collection-utilities;

define module sequence-utilities
  use dylan;
  export push!, pop!;
  export pair?, null?, list?;
  export xpair, tabulate, list*, take, drop, last-pair;
  export reverse-append, unfold, unfold/tail;
  export foldl, foldr, pair-foldl, pair-foldr;
  export reduce-l, reduce-r, heads, tails;
  export concatenate-map, pair-do, choose-map;
  export partition, assoc, apair, alist-copy, alist-delete;
  export satisfies, index, find, find-tail, precedes?;
  export split-at;
end module sequence-utilities;

