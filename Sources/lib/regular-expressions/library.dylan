module:     dylan-user
author:     Nick Kramer (nkramer@cs.cmu.edu)
synopsis:   Contains the library and module definitions for the Regular
            Expressions library.
copyright:  Copyright (C) 1994, Carnegie Mellon University.
            All rights reserved.
rcs-header: $Header: /scm/cvs/fundev/Sources/lib/regular-expressions/library.dylan,v 1.1 2004/03/12 00:08:52 cgay Exp $

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


define library regular-expressions
  use functional-dylan;
  // use collection-extensions; // KJP: removed
  use string-extensions;
  export
    regular-expressions;
end library regular-expressions;

define module regular-expressions
  use functional-dylan;
  // use extensions;
  use string-conversions;
  use character-type;
  use string-hacking;
  // use subseq;
  use %do-replacement;
  use %parse-string;
  use substring-search;
  export
    regexp-position, make-regexp-positioner,
    regexp-replace, make-regexp-replacer,
    translate, make-translator,
    split, make-splitter,
    join,
    <illegal-regexp>;

  export
    split-string;
end module regular-expressions;
