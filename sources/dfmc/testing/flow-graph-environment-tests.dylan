Module:    dfmc-flow-graph-environment-testing
Synopsis:  Tests for the environments from dfmc-flow-graph.
Author:    Bruce Mitchener, Jr.
Copyright: Original Code is Copyright (c) 2015, Dylan Hackers.
           All rights reserved.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define test construct-environment-test ()
  assert-no-errors(make(<lambda-lexical-environment>, lambda: #f, outer: #f));
end test;

define test top-level-environment-test ()
  let env = $top-level-environment;
  assert-true(top-level-environment?(env));

  let env = make(<lambda-lexical-environment>, lambda: #f, outer: env);
  assert-false(top-level-environment?(env));

  let env = make(<lambda-lexical-environment>, lambda: #f, outer: #f);
  assert-true(top-level-environment?(env));
end test;

define test inner-environment?-test ()
  let outer = make(<lambda-lexical-environment>, lambda: #f, outer: #f);
  let inner = make(<lambda-lexical-environment>, lambda: #f, outer: outer);
  let isolated = make(<lambda-lexical-environment>, lambda: #f, outer: #f);

  assert-false(inner-environment?(outer, inner));
  assert-true (inner-environment?(inner, outer));
  assert-false(inner-environment?(inner, isolated));
  assert-false(inner-environment?(isolated, inner));
end test;

define test all-environments-test ()
  let outer = make(<lambda-lexical-environment>, lambda: #f, outer: #f);
  let inner1 = make(<lambda-lexical-environment>, lambda: #f, outer: outer);
  let inner2 = make(<lambda-lexical-environment>, lambda: #f, outer: outer);

  assert-equal(all-environments(inner1), list(inner1, outer));
  assert-equal(all-environments(inner2), list(inner2, outer));
  assert-equal(all-environments(outer), list(outer));
end test;

define suite dfmc-flow-graph-environment-suite ()
  test construct-environment-test;
  test top-level-environment-test;
  test inner-environment?-test;
  test all-environments-test;
end suite;
