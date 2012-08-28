Module:    apple-dylan-test-suite
Filename:  test-list.dylan
Summary:   Apple Dylan test suite, test-list
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// Operations on Lists
// pair

define test pair-type (description: "")
  check-true("", instance?(pair, <function>));
  check-false("", instance?(pair, <generic-function>));
end test pair-type;

/* pair not implemented in the emulator
define test pair-0 (description: "")
  check-true("", pair(1, 2) = #(1 . 2));
  check-true("", pair(#"a", #(#"b", #"c")) = #(#"a", #"b", #"c"));
  check-true("", pair(#(), #(#"b", #"c")) = #(#(), #"b", #"c"));
  check-true("", pair(#"a", #()) = #(#"a"));
  check-true("", pair(#(), #()) = #(#()));
  check-true("", pair(#(#"a", #"b"), #(#"c", #"d")) = #(#(#"a", #"b"), #"c", #"d"));
  check-true("", pair(#"a", pair(#"b", pair(#"c", #()))) = #(#"a", #"b", #"c"));
end test pair-0;
*/
define test list-type (description: "")
  check-true("", instance?(list, <function>));
  check-false("", instance?(list, <generic-function>));
end test list-type;

define test list-0 (description: "")
 check-true("",  list(1, 2, 3) = #(1, 2, 3));
  check-true("", list() = #());
  check-true("", list(#"a") = #(#"a"));
  check-true("", list(#"a", #"b", #(#"c", #"d")) = #(#"a", #"b", #(#"c", #"d")));
  check-true("", list(#(#"a", #"b"), #()) = #(#(#"a", #"b"), #()));
end test list-0;

define test list-quote-0 (description: "")
  check-true("", #"john" = #"john");
      let t
        = #(#"john", #(#"quote", #(#"mary", #(#"quote", #(#"joe", #"cindy")), #"bob")), #(#"quote", #(#"joy")));
      check-true("", t.tail
      = #(#(#"quote", #(#"mary", #(#"quote", #(#"joe", #"cindy")), #"bob")), #(#"quote", #(#"joy"))));
  check-true("", t.tail.tail.head = #(#"quote", #(#"joy")));
  check-true("", t.tail.tail.head.head = #"quote");
  
end test list-quote-0;

define test list-ops-2 (description: "")
  begin
    local method reuse-pair (x, y, x-y)
            if (x = x-y.head & y = x-y.tail)
              x-y
            else
              pair(x, y)
            end if
          end method reuse-pair,
          method remv-list (item, lyst, #key test = \==)
            if (lyst.empty?)
              #()
            elseif (apply(test, item, lyst.first.list))
              remv-list(item, lyst.tail)
            else
              reuse-pair(lyst.first, remv-list(item, lyst.tail), lyst)
            end if
          end method remv-list;
    check-true("", remv-list(3, #(1, 2, 3, 4, 5)) = #(1, 2, 4, 5));
  check-true("", remv-list(2, #(1, 2, 3, 4, 5, 2, 6)) = #(1, 3, 4, 5, 6));
  end
end test list-ops-2;

define test list-ops-3 (description: "")
  local method reuse-pair (x, y, x-y)
          if (x = x-y.head & y = x-y.tail)
            x-y
          else
            pair(x, y)
          end if
        end method reuse-pair,
        method flatten2 (exp, #key so-far = #(), last-pair)
          if (exp = #())
            so-far
          elseif (~instance?(exp, <pair>))
            reuse-pair(exp, so-far, last-pair)
          else
            flatten2
              (exp.first,
               so-far: flatten2(exp.tail, so-far: so-far, last-pair: exp),
               last-pair: exp)
          end if
        end method flatten2;
  check-true("", flatten2(#(#(#"a"), #(#"b", #(#"c"), #"d"))) = #(#"a", #"b", #"c", #"d"));
end test list-ops-3;

define suite test-list-suite () 
  test pair-type;
//  test pair-0;
  test list-type;
  test list-0;
  test list-quote-0;
  test list-ops-2;
  test list-ops-3;
end;
