Module:       dylan-test-suite
Synopsis:     dylan library test suite - regressions tests
Copyright:    Original Code is Copyright (c) 2012 Dylan Hackers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// from "A monotonic superclass linearization for Dylan
// http://dx.doi.org/10.1145/236337.236343

define class <pane> (<object>) end;
define class <scrolling-mixin> (<object>) end;
define class <editing-mixin> (<object>) end;
define class <scrollable-pane> (<pane>, <scrolling-mixin>) end;
define class <editable-pane> (<pane>, <editing-mixin>) end;
define class <editable-scrollable-pane> (<scrollable-pane>, <editable-pane>) end;

define test pane-linearization ()
  let res = vector(<pane>, <object>);
  check-equal("<pane> linearization is correct", res, <pane>.all-superclasses)
end;

define test scrolling-mixin-linearization ()
  let res = vector(<scrolling-mixin>, <object>);
  check-equal("<scrolling-mixin> linearization is correct", res,
              <scrolling-mixin>.all-superclasses)
end;

define test editing-mixin-linearization ()
  let res = vector(<editing-mixin>, <object>);
  check-equal("<editing-mixin> linearization is correct", res,
              <editing-mixin>.all-superclasses)
end;

define test scrollable-pane-linearization ()
  let res = vector(<scrollable-pane>, <pane>, <scrolling-mixin>, <object>);
  check-equal("<scrollable-pane> linearization is correct", res,
              <scrollable-pane>.all-superclasses)
end;

define test editable-pane-linearization ()
  let res = vector(<editable-pane>, <pane>, <editing-mixin>, <object>);
  check-equal("<editable-pane> linearization is correct", res,
              <editable-pane>.all-superclasses)
end;


define test c3-linearization ()
  let res = vector(<editable-scrollable-pane>,
                   <scrollable-pane>, <editable-pane>, <pane>,
                   <scrolling-mixin>, <editing-mixin>,
                   <object>);
  check-equal("C3 linearization paper example works", res, <editable-scrollable-pane>.all-superclasses)
end;

define test limited-is-limited ()
  let <integer-vector> = limited(<vector>, of: <integer>);
  let x = make(<integer-vector>, size: 10, fill: 10);
  check-instance?("x is an instance of object", <object>, x);
  check-instance?("x is an instance of vector", <vector>, x);
  check-instance?("x is an instance of vector of integers",
                  <integer-vector>, x);
  do(method(a) check-equal("element is 10", 10, a) end, x);
  check-equal("size of x is 10", 10, size(x));
  check-instance?("x is an instance of vector with size 10",
                  limited(<vector>, size: 10), x);
  check-instance?("x is an instance of vector of integer with size 10",
                  limited(<vector>, of: <integer>, size: 10), x);
  //---*** sadly, the following fails due to limited-vector being ill-defined
  //       (look especially on the method in vector.dylan on of == <object>
  //       and what happens if of == #f) - hannes (Jan 2012)
  check-false("x is not instance of a vector with size 5",
              instance?(x, limited(<vector>, size: 5)));
  check-false("x is not instance of a vector of float",
              instance?(x, limited(<vector>, of: <single-float>)));
  check-false("x is not instance of a vector of integer with size 5",
              instance?(x, limited(<vector>, of: <integer>, size: 5)));
end;

define suite types ()
  test pane-linearization;
  test scrolling-mixin-linearization;
  test editing-mixin-linearization;
  test scrollable-pane-linearization;
  test editable-pane-linearization;
  test c3-linearization;
  test limited-is-limited;
end;