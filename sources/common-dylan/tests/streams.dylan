Module:       common-dylan-test-suite
Synopsis:     Common Dylan library test suite
Author:       Andy Armstrong, et al...
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test test-stream-test ()
  test-stream-class(<test-input-stream>,
                    name: "<test-input-stream>",
                    instantiable?: #t);
  test-stream-class(<test-output-stream>,
                    name: "<test-output-stream>",
                    instantiable?: #t)
end test test-stream-test;

define test test-<stream-error> ()
  test-condition-class(<stream-error>, abstract?: #t);
end test;

define test test-<end-of-stream-error> ()
  test-condition-class(<end-of-stream-error>);
end test;

define test test-<incomplete-read-error> ()
  test-condition-class(<incomplete-read-error>);
end test;

define test test-<incomplete-write-error> ()
  test-condition-class(<incomplete-write-error>);
end test;

define test test-stream-error-stream ()
  //---*** Fill this in...
end test;

define test test-stream-error-sequence ()
  //---*** Fill this in...
end test;

define test test-stream-error-count ()
  //---*** Fill this in...
end test;

define test test-open-file-stream ()
  //---*** Fill this in...
end test;

define suite common-dylan-streams-test-suite ()
  test test-stream-test;
  test test-stream-error-stream;
  test test-stream-error-sequence;
  test test-stream-error-count;
  test test-open-file-stream;
  test test-<stream-error>;
  test test-<end-of-stream-error>;
  test test-<incomplete-read-error>;
  test test-<incomplete-write-error>;
end suite;
