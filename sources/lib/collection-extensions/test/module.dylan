Module:    dylan-user
Synopsis:  The test suite for the collection-extensions library.
Author:    Matthias Hölzl (tc@xantira.com)
Copyright: See below.

// Copyright.
// =========

// Copyright (C) 2004 Dr. Matthias Hölzl.

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

define module collection-extensions-test
  use common-dylan;
  // These modules are probably not necessary, but
  // they are convenient for interactive debugging.
  use dylan-extensions,
    import: {debug-name};
  use print;
  use pprint;
  use format-out;
  use streams;

  use testworks;

  use self-organizing-list;
  use subseq;
  use vector-search;
  use heap;
  use sde-vector;
  use sequence-diff;
  use collection-utilities;
  use sequence-utilities;
end module collection-extensions-test;
