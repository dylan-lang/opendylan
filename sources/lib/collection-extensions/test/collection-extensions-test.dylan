Module:    collection-extensions-test
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


define suite collection-extensions-suite
	 (description: "Tests for the collection-extensions library.")
  suite self-organizing-list-suite;
  suite subseq-suite;
  suite vector-search-suite;
  // suite heap-suite;
  suite sde-vector-suite;
  suite sequence-diff-suite;
  suite collection-utilities-suite;
  suite sequence-utilities-suite;
end suite collection-extensions-suite;

define method main () => ()
  perform-suite(collection-extensions-suite);
end method main;

begin
  main();
end;

