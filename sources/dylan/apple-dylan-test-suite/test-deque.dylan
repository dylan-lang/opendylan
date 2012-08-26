Module:    apple-dylan-test-suite
Filename:  test-deque.dylan
Summary:   Apple Dylan test suite, test-deque
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// operations on deque

define test push-type (description: "")
  check("", instance?, push, <generic-function>);
end test push-type;

// this tests push & pop

define test push-0 (description: "simple cases")
  let d = make(<deque>);
  let t = d.size;
  check-true("", push(d, #"sym") == d);
  check-equal("", d.size, t + 1);
  check-equal("", d[0], #"sym");
  let d1 = make(<deque>);
  map(method (item)
         push(d1, item)
      end method,
          #(#"a", #"b", #"c", #"d"));
 check-equal("", d1[0], #"d");
end test push-0;

define test pop-type (description: "")
  check("", instance?, pop, <generic-function>);
end test pop-type;

define test pop-0 (description: "")
  let d = make(<deque>);
  let t = 0;
  map(curry(push, d), #(#"a", #"b", #"c", #"d"));
  t := d.size;
  begin
      let p = d.pop;
      check-equal("", p, #"d"); 
      check-equal("", d.size, t - 1);
  end
end test pop-0;

define test push-pop (description: "")
  let d = make(<deque>);
  push(d, 99);
  let p = d.pop;
  check-equal("", p, 99);
  check-true("", d.empty?);
end test push-pop;

define test push-last-pop-last-type (description: "")
  check("", instance?, push-last, <generic-function>);
  check("", instance?, pop-last, <generic-function>);
end test push-last-pop-last-type;

define test pop-last-0 (description: "")
  let d = make(<deque>);
  let t = 0;
  map(curry(push, d), #(#"a", #"b", #"c", #"d"));
  t := d.size;
  begin
      let p = d.pop-last;
      check-equal("", p, #"a"); 
      check-equal("", d.size, t - 1);
    end;
end test pop-last-0;

define test push-last-pop-last (description: "")
  let d = make(<deque>);
  push-last(d, 99);
  let p = d.pop-last;
  check-equal("", p, 99);
  check-true("", d.empty?);
end test push-last-pop-last;

// add!

define test add!-deque (description: "")
  let t = make(<deque>);
  check-equal("", add!(t, 99), t);
  check-equal("",  t.size, 1);
  check-equal("", t[0], 99);
end test add!-deque;

// remove! (deque)

define test remove!-deque (description: "with test: and count:")
    let d = deque-instance(1, 2, 3, 4);
    let nd = remove!(d, 2);
    check-true("", ~member?(2, nd)); 
    check-true("",  member?(1, nd));
    check-true("",  member?(3, nd)); 
    check-true("",  member?(4, nd));
    let d = deque-instance(1, 2, 3, 4);
    let nd = remove!(d, 2, test: \>=, count: 2);
    check-true("", ~member?(2, nd));
    check-true("", member?(1, nd));
    check-true("", ~member?(3, nd));
    check-true("",  member?(4, nd));
end test remove!-deque;

// previous-state (deque)

define test previous-state-0 (description: "")
  check("", instance?, previous-state, <generic-function>);
end test previous-state-0;

define test previous-state-deque (description: "")
  let d = make(<deque>);
  map(method (item)
        push(d, item)
      end method,
      #(#"a", #"b", #"c"));
  check-equal("", #f, previous-state(d, d.initial-state));
  check-true("", ~(#f = previous-state(d, d.final-state)));
end test previous-state-deque;

define suite test-deque-suite ()
  test push-type;
  test push-0;
  test pop-type;
  test pop-0;
  test push-pop;
  test push-last-pop-last-type;
  test pop-last-0;
  test push-last-pop-last;
  test add!-deque;
  test remove!-deque;
  test previous-state-0;
  test previous-state-deque;
end suite;
