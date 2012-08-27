Module:    apple-dylan-test-suite
Filename:  test-class3.dylan
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

define test type-survive-assignment-1 ()
  local method foo (n :: <integer>)
     if (n < 0)
        n := 0;
     end if;
     n
  end method;
  check("", \=, 0, foo(-1));
end test;

// This should signal an error because you shouldn't be able to (set! n #f)
// when N is declared <integer>.
//

define test type-survive-assignment-2 ()
  check-condition
    ("", <error>, 
     method (n :: <integer>) 
       if (n < 0)
	 n := #f
       end if;
       n
     end method(-1));
end test;

define test type-survive-assignment-3 ()
  let n :: <integer> = -1;
  if (n < 0)
    n := 0
  end if;
  check("", \=, n, 0);
end test;

define test subtype?-type ()
  check("", instance?, subtype?, <function>);
end test;

define test subtype?-0 (description: "indirectly")
  check-true("", subtype?(<dtest-test-subclass>, <dtest-test-class>));
  check-true("", ~subtype?(<dtest-test-class>, <dtest-test-subclass>));
  check-true("", subtype?(<sequence>, <collection>));
  check-true("", ~subtype?(<collection>, <sequence>));
  check-true("", subtype?(<integer>, <real>));
end test;

define test object-class-type ()
  check("", instance?, object-class, <function>);
end test;

define test object-class-0 ()
  check-equal("", make(<dtest-test-class>).object-class, <dtest-test-class>);
end test;

define test all-superclasses-type ()
  check("", instance?, all-superclasses, <generic-function>);
end test;

define test all-superclasses-0 ()
  check-equal("", <object>.all-superclasses, <object>.list);
  let c = <dtest-test-class>.all-superclasses;   
  check-equal("", c.size, 2);
  check-equal("", c.first, <dtest-test-class>);
  check-equal("", c.second, <object>);
      let c = <dtest-test-subclass>.all-superclasses;
  check-equal("", c.size, 3);
  check-equal("", c.first, <dtest-test-subclass>);
  check-equal("", c.second, <dtest-test-class>);
  check-equal("", c.third, <object>)
end test;

define test direct-subclasses-type ()
  check("", instance?, direct-subclasses, <generic-function>);
end test;

define test direct-subclasses-0 ()
  check-true("", <dtest-test-subclass>.direct-subclasses.empty?);
  check-equal("", <dtest-test-class>.direct-subclasses, <dtest-test-subclass>.list);
end test;

define class <ro-test> (<object>)
end class <ro-test>;

define class <ro-test-sub> (<ro-test>)
  slot s;
end class <ro-test-sub>;

/* commented out by amit - look in issues
define test seal-type-0 ()
  check("", instance?, seal, <generic-function>);
end test;

define class <sealtest> (<object>)
  slot s;
end class <sealtest>;

define test seal-0 ()
  check("", instance?, <sealtest>.seal, <class>);
  check-true("", <sealtest>.all-superclasses.empty?);
end test;
*/

define test singleton-type ()
  check("", instance?, singleton, <function>);
  check-false("", instance?(singleton, <generic-function>));
end test;

define method double (thing :: <number>)
  thing + thing
end method double;

define method double (thing == #"cup")
  #"pint"
end method double;

define test singleton-0 ()
  check("This failure is the same as test instance?-2", instance?, singleton(1), <singleton>);
  check-equal("", double(#"cup"), #"pint");
  check-equal("", double(3), 6);
end test;

define suite test-class-suite ()
  test as-sequence-permutations;
  test as-table;
  test shallow-copy-type;
  test shallow-copy-0;
  test type-for-copy-class-type;
  test type-for-copy-class-0;
  test type-for-copy-class-1;
  test type-for-copy-class-2;
  test type-for-copy-class-3;
  test type-type;
  test limited-type;
  test limited-integers;
  test limited-collections-1;
  test limited-collections-2;
  test limited-collections-3;
  test limited-collections;
  test union-on-types;
  test union-on-limited-types;
//  test make-<class>;
  test make-<class>-indirect;
  test instance?-type;
  test instance?-0;
  test instance?-1;
  test instance?-2;
  test instance?-3;
  test instance?-4;
  test instance?-5;
  test instance?-6;
  test instance?-7;
  test instance?-8;
  test instance?-9;
  test instance?-10;
  test instance?-11;
  test instance?-12;
  test instance?-13;
  test instance?-14;
  test instance?-15;
  test instance?-16;
  test instance?-17;
  // test instance?-18; - Uses <ratio>
  test instance?-20;
  test instance?-22;
  test instance?-23;
  test as-type;  
  test subtype?-type;
  test subtype?-0;
  test object-class-type;
  test object-class-0;
  test all-superclasses-type;
  test all-superclasses-0;
  test direct-subclasses-type; 
  test direct-subclasses-0;
  test singleton-type;
  test singleton-0;
  test type-survive-assignment-1;
  test type-survive-assignment-2;
  test type-survive-assignment-3;
end suite;




