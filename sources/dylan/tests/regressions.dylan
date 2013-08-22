Module:       dylan-test-suite
Synopsis:     dylan library test suite - regressions tests
Author:       Carl Gay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Try to keep the tests in order by bug number.

define test bug-5800 ()
  check-no-errors("as(<float>, 60962186)", as(<float>, 60962186));
  // Throw these two in for good measure...
  check-no-errors("as(<float>, $maximum-integer)", as(<float>, $maximum-integer));
  check-no-errors("as(<float>, $minimum-integer)", as(<float>, $minimum-integer));
end test bug-5800;

define test bug-5580 ()
  check-no-errors("iteration over a floating point range",
      for (i in range(from: 0.0, to: 1.0, by: 0.1)) end);
end test bug-5580;

define test bug-5325 ()
  check-no-errors("add!(a-range, a-value) doesn't err",
                  add!(range(from: 1, to: 5), 0));
end test bug-5325;

// I guess this and bug-5058 are really about the same thing...
//
define test bug-5281 ()
  // Dimensions differ so call to make should err.
  check-condition("make limited array with incorrect dimensions",
                  <error>,
                  make(limited(<array>, of: <integer>, dimensions: #[2, 2]),
                       dimensions: #[5, 5], fill: 1));
  check-false("limited array dimensions are part of typeness",
              begin
                let t1 = limited(<array>, of: <integer>, dimensions: #[2, 2]);
                let t2 = limited(<array>, of: <integer>, dimensions: #[3, 3]);
                instance?(make(t1, dimensions: #[2, 2], fill: 1), t2);
              end);
end test bug-5281;

define test bug-5247 ()
  check-true("member? always returns a <boolean>?",
             member?(1, #(1,2,3), test: max));
end test bug-5247;

// I guess this and bug-5281 are really about the same thing...
//
define test bug-5058 ()
  check-equal("size of limited vectors",
               begin
                 let <v3> = limited(<vector>, of: <single-float>, size: 3, fill: 0.0);
                 size(make(<v3>))
               end,
               3);
end test bug-5058;

define test bug-5036 ()
  check-equal("subsequence-position handles the count: keyword correctly",
              subsequence-position("abc", "", count: 2),
              1);
end test bug-5036;

define class <bug-4976> (<array>) end;

define method dimensions (cb :: <bug-4976>) => (dims :: <vector>)
  #[8,8]
end;

define test bug-4976 ()
  check-equal("size method on <array> works", size(make(<bug-4976>)), 64);
end test bug-4976;

define test bug-2766 ()
  check-no-errors("bug 2766",
                  begin
                    let <bit> = limited(<integer>, min: 0, max: 1);
                    let <bit-array> = limited(<array>, of: <bit>);
                    let a = make(<bit-array>, dimensions: #(3, 4), fill: 0);
                    dimensions(a);
                    dimension(a, 0);
                    dimension(a, 1);
                  end);
end test bug-2766;

define test bug-2828 ()
  check-true("<sequence> is a superclass of <stretchy-sequence>",
             block ()
               member?(<sequence>, all-superclasses(<stretchy-sequence>))
             exception (<sealed-object-error>)
               // Can't figure it out directly, so try a roundabout method that
               // happens to work in the current release.  The stuff about
               // <stretchy-vector> is because I can't instantiate
               // <stretchy-sequence> directly.
               block ()
                 member?(<stretchy-vector>, direct-subclasses(<stretchy-sequence>))
                 & ~member?(<sequence>, direct-superclasses(<stretchy-vector>))
                 & instance?(make(<stretchy-vector>), <sequence>)
               exception (<sealed-object-error>)
                 #f
               end block
             end block);
end test bug-2828;

define test bug-2263 ()
  check-no-errors("condition-format-{string,arguments} apply to <simple-restart>",
                  begin
                    let restart = make(<simple-restart>,
                                       format-string: "", format-arguments: #[]);
                    condition-format-string(restart);
                    condition-format-arguments(restart);
                  end);
end test bug-2263;

// Not quite sure if this adequately tests bug 2212.
//
define test bug-2212 ()
  check-no-errors("limited(<table>, of: foo)",
                  limited(<table>, of: make(<class>)));
end test bug-2212;

define test bug-2178 ()
  check-true("concatenate-as(<deque>, ...)",
             begin
               let d1 = make(<deque>, size: 2, fill: 0);
               let d2 = make(<deque>, size: 2, fill: 1);
               let d3 = concatenate-as(<deque>, d1, d2);
               size(d3) = 4
               & pop(d3) = 0 & pop(d3) = 0 & pop(d3) = 1 & pop(d3) = 1
             end);
end test bug-2178;

define generic foo-function (i);

define method foo-function (i :: <integer>)
  if (i > 0)
    #"foo"
  else
    values(#"foo", #"bar")
  end
end;

define variable *foo* = #[];

// Don't merge the tests, an optimizer bug will shadow the bug
// (<exit> doesn't get optimized to a branch)

define test bug-7388 ()
  let (foo, bar) =
    block(return)
      return(#"foo");
    end;
  check-equal("First argument of MV return in block", #"foo", foo);
  check-equal("Second argument of MV return in block", #f, bar);
end;

define test bug-7388-a ()
  let (foo, bar) = foo-function(23);
  check-equal("First argument of MV return", #"foo", foo);
  check-equal("Second argument of MV return", #f, bar);
end;

define test bug-7388-b ()
  let (foo, bar) = block(return) apply(return, *foo*) end;
  check-equal("First argument of empty MV return", #f, foo);
  check-equal("Second argument of empty MV return", #f, bar);
end;

define test issue-189 ()
  check-equal("heap-allocated cell compiles correctly",
              begin
                let x :: <double-float> = as(<double-float>, 0.0);
                block ()
                  x := 1.0d0;
                exception (<error>)
                  x := as(<double-float>, 0.0);
                end block;
                x;
              end, 1.0d0)
end;

define test issue-203 ()
  check-equal("float assignment is sane",
              begin
                let a = 0.0;
                let b = 0.100000000001;
                a := b;
                a;
              end, 0.100000000001)
end;

// Class for issue 440 test below.

define class <case-insensitive-set> (<mutable-explicit-key-collection>)
   constant slot elements = make(<stretchy-vector>) /* of <string> */;
   constant slot key-test :: <function> = case-insensitive-equal;
end class;

define method element (cis :: <case-insensitive-set>, key :: <object>, #key default)
=> (val :: <object>)
   if (member?(key, cis.elements, test: cis.key-test))
      key
   else
      default
   end if
end method;

define method element-setter (new-str :: <object>, cis :: <case-insensitive-set>, key :: <object>)
=> (val :: <object>)
  add-new!(cis.elements, new-str, test: cis.key-test);
  new-str
end method;

define method forward-iteration-protocol (cis :: <case-insensitive-set>)
=> (initial-state :: <object>, limit :: <object>, next-state :: <function>,
    finished-state? :: <function>, current-key :: <function>,
    current-element :: <function>, current-element-setter :: <function>,
    copy-state :: <function>)
  let initial-state = 0;
  let limit = cis.elements.size;
  let next-state = method (cis, state) state + 1 end;
  let finished-state? = method (cis, state, limit) state >= limit end;
  let current-key = method (cis, state) cis.elements[state] end;
  let current-element = current-key;
  let current-element-setter =
      method (new-str, cis, state) cis.elements[state] := new-str end;
  let copy-state = method (cis, state) state end;
  values(initial-state, limit, next-state, finished-state?,
      current-key, current-element, current-element-setter, copy-state)
end method;

define test issue-440 ()
  let cis = make(<case-insensitive-set>);
  cis["red"] := "red";
  cis["green"] := "green";
  cis["blue"] := "blue";
  check-equal("supports <explicit-key-collection> subclass",
              map(identity, cis), cis);
end;

define suite dylan-regressions ()
  test bug-5800;
  test bug-5580;
  test bug-5325;
  test bug-5281;
  test bug-5247;
  test bug-5058;
  test bug-5036;
  test bug-4976;
  test bug-2828;
  test bug-2263;
  test bug-2212;
  test bug-2178;
  test bug-7388;
  test bug-7388-a;
  test bug-7388-b;
  test issue-189;
  test issue-203;
  test issue-440;
end suite dylan-regressions;

