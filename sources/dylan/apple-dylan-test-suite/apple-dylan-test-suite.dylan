Module:    apple-dylan-test-suite
Filename:  apple-dylan-test-suite.dylan
Author:    Shri Amit(amit)
Summary:   Apple Dylan test suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite apple-dylan-suite ()
  suite test-array-suite;
  suite test-assignment-suite;
  suite test-character-suite;
  suite test-class-suite;
  suite test-collection-suite;
  suite test-comparison-suite;
  suite test-condition-suite;
  suite test-control-suite;
  suite test-defines-suite;
  suite test-deque-suite;
  suite test-function-suite;
  suite test-functional-suite;
  suite test-intro-mop-suite;
  suite test-iteration-suite;
  suite test-keyword-symbol-suite;
  suite test-list-suite;
  suite test-range-suite;
  suite test-sequence-suite;
end suite apple-dylan-suite;
