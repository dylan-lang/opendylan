Module:    apple-dylan-test-suite
Filename:  test-class2.dylan
Summary:   Apple Dylan test suite, test-class
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit) &
	     James Kirsch(jkirsch)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

// chapter 11. Classes

// Added by design note #3: make <class> is supported now


define generic widget-x (obj);

define generic widget-y (obj);

define generic widget-x-setter (obj);

define generic widget-y-setter (obj);

define constant *widget-slots-spec*
  = list(list(getter: widget-x,
              setter: widget-x-setter,
              init-keyword: #"x",
              init-value: 0,
              type: <integer>),
         list(getter: widget-y,
              setter: widget-y-setter,
              init-keyword: #"y",
              init-value: 11,
              type: <integer>));

define test make-<class> ()
  let wclass1 = make(<class>, slot: *widget-slots-spec*);
  let wclass2
    = make(<class>, superclasses: <object>, slots: *widget-slots-spec*);
  let wclass3
    = make(<class>, superclasses: <object>.list, slots: *widget-slots-spec*);
  every?
    (method (wclass)
           check("", instance?(wclass, <class>));	
           let i = make(wclass);
           // No init-keywords
           check("", \=, i.widget-x, 0);
	   check("", \=, i.widget-y, 11);
           let i = make(wclass, x: 3, y: 7);
           // init-keywords
           check("", \=, i.widget-x, 3);
	   check("", \=, i.widget-y, 7);
           let i = make(wclass, x: 3, y: 7);
           // slot setters
           i.widget-x := 99;
           check("", \=, i.widget-x, 99);
	   check("", \=, i.widget-y, 7);
     end method,
     list(wclass1, wclass2, wclass3));
end test;

define test make-<class>-indirect ()
  let wclass1
    = make(<class>,
           superclasses: <dtest-test-subclass>,
           slots: *widget-slots-spec*);
  let wclass2
    = make(<class>,
           superclasses: <dtest-test-subclass>.list,
           slots: *widget-slots-spec*);
  check-true("", every?
    (method (wclass)
       instance?(wclass, <class>)
       & subtype?(wclass, <object>)
       & subtype?(wclass, <dtest-test-class>)
       & subtype?(wclass, <dtest-test-subclass>)
       & begin
           let i = make(wclass);
           // No init-keywords
           instance?(i, <object>)
           & instance?(i, <dtest-test-class>)
           & instance?(i, <dtest-test-subclass>)
           & 99 == i.s2;
         end
       & begin
           let i = make(wclass);
           // Test setter
           i.s2 := 101;
           101 == i.s2;
         end
     end method,
     list(wclass1, wclass2)));
end test;

define test instance?-type ()
  check-false("", instance?(instance?, <generic-function>));
end test;

// this is where we'll check out the class heterarchy, too.

define test instance?-0 ()
  check-true("", every?	
    (method (c)		
       instance?(make, c)
     end method,
     list(<object>, <function>, <generic-function>)));
end test;

define test instance?-1 ()
  check-true("", every?(curry(instance?, <class>), list(<object>, <class>)));
  check-true("", every?(rcurry(instance?, <class>), list(<object>, <class>)));
end test;

define test instance?-2 ()
  check("", instance?, singleton(99), <singleton>);
end test;

define test instance?-3 ()
  let i = make(<table>);
  check-true("<table> should be an instance of all of these", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-explicit-key-collection>,
          <explicit-key-collection>,
          <mutable-collection>,
          <collection>,
	  <stretchy-collection>)));
end test;

define test instance?-4 ()
  let i = make(<array>, dimensions: #(2));
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
	  <object>,
	  <sequence>,
          <mutable-collection>,
          <collection>)));
end test;

define test instance?-5 ()
  let i = make(<vector>);
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-explicit-key-collection>,
          <explicit-key-collection>,
          <mutable-collection>,
          <collection>,
          <array>,
          <vector>)));
end test;

define test instance?-6 ()
  let i = stretchy-vector-instance();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-explicit-key-collection>,
          <explicit-key-collection>,
          <mutable-collection>,
          <collection>,
          <mutable-sequence>,
          <sequence>,
          <array>,
          <vector>,
          <stretchy-vector>)));
end test;

define test instance?-7 ()
  let i = simple-object-vector-instance();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-explicit-key-collection>,
          <explicit-key-collection>,
          <mutable-collection>,
          <collection>,
          <mutable-sequence>,
          <sequence>,
          <array>,
          <vector>,
          <simple-object-vector>)));
end test;

define test instance?-8 ()
  let i = make(<string>, size: 10);
  check-true("<string> should be an instance of some of these", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
	  <object>)));
end test;

define test instance?-9 ()
  let i = unicode-string-instance();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-explicit-key-collection>,
          <explicit-key-collection>,
          <mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
          <array>,
          <vector>,
          <string>,
          <unicode-string>)))
end test;

define test instance?-10 ()
  let i = byte-string-instance();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
          <array>,
          <vector>,
          <string>,
          <object>)));
end test;

define test instance?-11 ()
  let i = deque-instance();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
          <deque>)))
end test;

define test instance?-12 ()
  let i = range();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<collection>, <sequence>, <range>)));
end test;

define test instance?-13 ()
  let i = make(<list>);
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
          <list>)));
end test;

define test instance?-14 ()
  let i = make(<pair>);
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
          <list>,
          <object>)));
end test;

define test instance?-15 ()
  let i = make(<empty-list>);
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<mutable-sequence>,
          <mutable-collection>,
          <sequence>,
          <collection>,
          <list>,
          <empty-list>)));
end test;

define test instance?-16 ()
  let i = complex-instance();
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<number>, <object>)));
end test;

define test instance?-17 ()
  let i = 4;
  check-true("", every?
    (method (c)
       instance?(i, c)
     end method,
     list(<number>, <real>, <rational>, <integer>)));
end test;

/*
define test instance?-18 ()
  let i = make(<ratio>);
  check-true("", every?
    (method (c)
       instance?(i, c);
     end method,
     list(<number>, <complex>, <real>, <rational>, <ratio>)));
end test;
*/

define test instance?-20 ()
  check-true("", every?
    (method (c)
       instance?(#"foo", c)
     end method,
     list(<object>, <symbol>)));
end test;

define test instance?-21 ()
  check-true("", every?
    (method (c)
       instance?(foo: c)
     end method,
     list(<object>, <keyword>)));
end test;

define test instance?-22 ()
  check-true("", every?
    (method (c)
       instance?('5', c)
     end method,
     list(<object>, <character>)));
end test;

define test instance?-23 ()
  let i = make(<dtest-test-class>);
  check-true("", ~any?(method (c)
		instance?(i, c)
	      end method,
	      list(<function>,
		   <generic-function>,
		   <class>,
		   <collection>,
		   <mutable-collection>,
               <explicit-key-collection>,
		   <mutable-explicit-key-collection>,
		   <sequence>,
		   <mutable-sequence>,
		   <table>,
		   <array>,
		   <vector>,
		   <stretchy-vector>,
		   <simple-object-vector>,
		   <string>,
		   <unicode-string>,
		   <byte-string>,
		   <deque>,
		   <range>)));
      let i = make(<dtest-test-class>);
      check-true("", ~any?(method (c)
		    instance?(i, c)
		  end method,
		  list(<list>,
		       <pair>,
		       <empty-list>,
		       <number>,
		       <complex>,
//		       <rectangular-complex>,
		       <real>,
		       <rational>,
		       <integer>,
//		       <ratio>,
		       <float>,
		       <symbol>,
		       <keyword>,
		       <character>)));
   check-true("", every?
      (method (c)
         instance?(make(<dtest-test-class>), c)
       end method,
       list(<object>, <dtest-test-class>)))
end test;

define test as-type ()
  check("", instance?, as, <generic-function>);
end test;
