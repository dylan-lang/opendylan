Module: corba-tests-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite request-tests ()
  suite grid-test-suite;
  suite bank-test-suite;
  suite any-test-suite;
  suite array-test-suite;
  suite chat-test-suite;
  suite constant-test-suite;
  suite enum-test-suite;
  suite pragma-test-suite;
  suite pseudo-objects-test-suite;
  suite sequence-test-suite;
  suite struct-test-suite;
  suite tree-test-suite;
  suite union-test-suite;
  suite ir-test-suite;
end suite;

define suite co-located () // to run servers do "-suite co-located -top" on command-line
  test co-located-test;
end suite;

define test co-located-test () 
  run-servers();
end test;

ignore(co-located);
