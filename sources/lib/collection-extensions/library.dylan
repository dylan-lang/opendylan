module:		dylan-user
rcs-header:	$Header: /scm/cvs/fundev/Sources/lib/collection-extensions/library.dylan,v 1.1 2004/03/12 00:08:43 cgay Exp $
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
