Module:    apple-dylan-test-suite
Filename:  test-class.dylan
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

/// <byte-string>, <deque>, <list>, <simple-object-vector>, <stretchy-vector>,
/// <string>, <unicode-string>, and a bit of <range>.

define test as-sequence-permutations ()
  check-true("", for (stuff
         in list(list("Direct Assignment", <list>),
                 list("Direct Assignment", <byte-string>, <string>),
                 list("Direct Assignment", <vector>),
                 list("Direct Assignment", <stretchy-vector>, <vector>),
                 list("Direct Assignment", <simple-object-vector>, <vector>),
                 list("Direct Assignment", <deque>, <mutable-sequence>),
                 list("Direct Assignment", <unicode-string>, <string>),
                 list("", <list>),
                 list("", <byte-string>, <string>),
                 list("", <vector>),
                 list("", <stretchy-vector>, <vector>),
                 list("", <simple-object-vector>, <vector>),
                 list("", <deque>, <mutable-sequence>),
                 list("", <unicode-string>, <string>),
                 list(#(#"a", #"b", #"c", #"d"), <list>),
                 list(#(#"a", #"b", #"c", #"d"), <vector>),
                 list(#(#"a", #"b", #"c", #"d"), <stretchy-vector>, <vector>),
                 list(#(#"a", #"b", #"c", #"d"),
                      <simple-object-vector>,
                      <vector>),
                 list(#(#"a", #"b", #"c", #"d"), <deque>, <mutable-sequence>),
                 list(#(), <list>),
                 list(#(), <vector>),
                 list(#(), <stretchy-vector>, <vector>),
                 list(#(), <simple-object-vector>, <vector>),
                 list(#(), <deque>, <mutable-sequence>),
                 list(#[1, 2, 3, 4], <list>),
                 list(#[1, 2, 3, 4], <vector>),
                 list(#[1, 2, 3, 4], <stretchy-vector>, <vector>),
                 list(#[1, 2, 3, 4], <simple-object-vector>, <vector>),
                 list(#[1, 2, 3, 4], <deque>, <mutable-sequence>),
                 list(#['F', 'o', 'o', 'B', 'a', 'r'],
                      <byte-string>,
                      <string>),
                 list(#['F', 'o', 'o', 'B', 'a', 'r'],
                      <unicode-string>,
                      <string>),
                 list(#[], <list>),
                 list(#[], <vector>),
                 list(#[], <stretchy-vector>, <vector>),
                 list(#[], <simple-object-vector>, <vector>),
                 list(#[], <deque>, <mutable-sequence>),
                 list(#[], <byte-string>, <string>),
                 list(#[], <unicode-string>, <string>),
                 list(deque-instance(#"a", #"b", #"c"), <list>),
                 list(deque-instance(#"a", #"b", #"c"), <vector>),
                 list(deque-instance(#"a", #"b", #"c"),
                      <stretchy-vector>,
                      <vector>),
                 list(deque-instance(#"a", #"b", #"c"),
                      <simple-object-vector>,
                      <vector>),
                 list(deque-instance(#"a", #"b", #"c"), <deque>),
                 list(deque-instance('x', 'y', 'z'), <string>),
                 list(deque-instance('x', 'y', 'z'), <byte-string>, <string>),
                 list(deque-instance('x', 'y', 'z'),
                      <unicode-string>,
                      <string>),
                 list(stretchy-vector-instance('A', 'B', 'C'), <list>),
                 list(stretchy-vector-instance('A', 'B', 'C'), <vector>),
                 list(stretchy-vector-instance('A', 'B', 'C'),
                      <stretchy-vector>,
                      <vector>),
                 list(stretchy-vector-instance('A', 'B', 'C'),
                      <simple-object-vector>,
                      <vector>),
                 list(stretchy-vector-instance('A', 'B', 'C'), <deque>),
                 list(stretchy-vector-instance('A', 'B', 'C'), <string>),
                 list(stretchy-vector-instance('A', 'B', 'C'),
                      <byte-string>,
                      <string>),
                 list(stretchy-vector-instance('A', 'B', 'C'),
                      <unicode-string>,
                      <string>),
                 list(range(from: 3, below: 10), <list>),
                 list(range(from: 9, above: 3), <vector>),
                 list(range(from: 9, above: 3), <stretchy-vector>, <vector>),
                 list(range(from: 9, above: 3),
                      <simple-object-vector>,
                      <vector>),
                 list(range(from: 9, above: 3), <deque>)),
       losers = #()
         then begin
                let object = stuff.head;
                let classes = stuff.tail;
                let new-class = classes.head;
                let coercion = as(new-class, object);
                if (instance?(object, <sequence>)
                    & instance?(object, <collection>)
                    & every?(curry(instance?, coercion), classes)
                    & if (instance?(object, new-class))
                        object == coercion
                      else
                        ~(object == coercion)
                        & every?(\==, object, coercion)
                        & instance?(coercion, <sequence>)
                        & instance?(coercion, <collection>)
                        // Do it using element.
                        & begin
                            local method f (i)
                                    if (i == object.size)
                                      #t
                                    elseif (object[i] == coercion[i])
                                      f(i + 1)
                                    else
                                      #f
                                    end if
                                  end method f;
                            f(0)
                          end
                        // Do it using iteration states.
                        & sequences-element-id?(object, coercion)
                      end if)
                  losers
                else
                  pair(stuff, losers)
                end if
              end)
  finally
    if (losers.empty?)
      #t
    else
      losers
    end if;
  end for);
end test;

define test as-table ()
  check-true("", for (collection
         in list("Direct Assignment",
                 "",
                 #(#"a", #"b", #"c", #"d"),
                 #(),
                 #[1, 2, 3, 4],
                 #[],
                 deque-instance(#"a", #"b", #"c"),
                 deque-instance('x', 'y', 'z'),
                 stretchy-vector-instance('A', 'B', 'C'),
                 range(from: 3, below: 10)),
       losers = #()
         then if (begin
                    let objsize = collection.size;
                    let coercion = as(<table>, collection);
                    instance?(coercion, <table>)
                    & objsize = coercion.size
                    & begin
                        local method f (i)
                                if (i == objsize)
                                  #t
                                elseif (coercion[i] == collection[i])
                                  f(i + 1)
                                else
                                  #f
                                end if
                              end method f;
                        f(0)
                      end
                  end)
                losers
              else
                pair(collection, losers)
              end if)
  finally
    if (losers.empty?)
      #t
    else
      losers
    end if;
  end for);
end test;

// shallow-copy

define test shallow-copy-type ()
  check("", instance?, shallow-copy, <generic-function>);
end test;

define test shallow-copy-0 ()
  let d = deque-instance(3, 4);
  let s = list(1, 2, d);
  let ns = s.shallow-copy;
  check-equal("", s, ns);
  check-equal("", s.third, ns.third);
  let s = vector(1, 2, deque-instance(3, 4));
  let ns = s.shallow-copy;
  check-equal("", s, ns);
  check-equal("", s.third, ns.third);
end test;

// type-for-copy

define test type-for-copy-class-type ()
  check("", instance?, type-for-copy, <generic-function>);
end test;

define test type-for-copy-class-0 ()
  check-equal("", type-for-copy(3), object-class(3));
  check-equal("", deque-instance().type-for-copy, deque-instance().object-class);
  check-equal("", complex-instance().type-for-copy, complex-instance().object-class);
  check-equal("",
              stretchy-vector-instance().type-for-copy, 
              stretchy-vector-instance().object-class);
  check-equal("", vector().type-for-copy, vector().object-class);
end test;

// type-for-copy of all sequences should be a subclass of <sequence>
//
// Note that the tests type-for-copy1, type-for-copy2, and type-for-copy3
// only test the standard Dylan collection classes.
// type-for-copy1 should probably be extended to test all implementation-specific sequences.
// type-for-copy2 should probably be extended to test all implementation-specific explicit-key-collections.
// type-for-copy3 should probably be extended to test all implementation-specific collections.

define test type-for-copy-class-1 ()
  check-true("", every?(rcurry(subtype?, <sequence>),
                        map(compose(type-for-copy, make),
                            list(<vector>,
                                 <stretchy-vector>,
                                 <string>,
                                 <deque>,
                                 <range>,
                                 <simple-object-vector>,
                                 <unicode-string>,
                                 <byte-string>,
                                 <list>,
                                 <pair>,
                                 <empty-list>))));
end test;

// type-for-copy of all explicit-key-collections should be a subclass
// of <explicit-key-collection>

define test type-for-copy-class-2 ()
  check-true("", every?(rcurry(subtype?, <explicit-key-collection>),
                        map(compose(type-for-copy, make),
                            list(<table>,
                                 <object-table>))));
end test;

// type-for-copy of all collections should be mutable.
//

define test type-for-copy-class-3 ()
  check-true("",
             every?(rcurry(subtype?, <mutable-collection>),
                    map(compose(type-for-copy, make),
                        list(<table>,
                             <object-table>,
                             <vector>,
                             <stretchy-vector>,
                             <simple-vector>,
                             <simple-object-vector>,
                             <list>,
                             <pair>,
                             <empty-list>,
                             <deque>,
                             <string>,
                             <byte-string>,
                             <unicode-string>,
                             <range>))));
  check-true("", subtype?(type-for-copy(make(<array>, dimensions: #(0))),
                          <mutable-collection>));
end test;

// Design note #5 introduces a new abstract class <type>

define test type-type ()
  check("", instance?, <type>, <class>);
  check("", subtype?, <class>, <type>);
  check("", subtype?, <singleton>, <type>);
end test;

// Design note #6 introduces a new generic function limited

define test limited-type ()
  check("", instance?, limited, <generic-function>);
end test;

define test limited-integers ()
  let positive = limited(<integer>, min: 0);
  let strictly-positive = limited(<integer>, min: 1);
  let small-integer = limited(<integer>, min: 0, max: 255);
  let signed-byte = limited(<integer>, min: -127, max: 127);
  check-true("", instance?(positive, <type>));
  check("", instance?, strictly-positive, <type>);
  check("", instance?, small-integer, <type>);
  check("", instance?, signed-byte, <type>);
  check("", instance?, 0, positive);
  check-false("", instance?(-1, positive));
  check("", instance?, 1, strictly-positive);
  check-false ("", instance?(0, strictly-positive));
  check("", instance?, 0, small-integer);
  check("", instance?, 255, small-integer);
  check-false ("", instance?(-1, small-integer));
  check-false("", instance?(256, small-integer));
  check("", instance?, 0, signed-byte);
  check("", instance?, -127, signed-byte);
  check("", instance?, 127, signed-byte);
  check-false("", instance?(300, signed-byte));
  check-false("", instance?(-300, signed-byte));
end test;

define test limited-collections-1 ()
  check-true("", every?(method (cc)
                          instance?(cc.limited, <type>)
                        end method,
                        list(<table>,
                             <array>,
                             <vector>,
                             <stretchy-vector>,
                             <string>,
                             <deque>,
                             <range>)));
end test;

define test limited-collections-2 ()
  check-true("", every?(method (cc)
                          instance?(limited(cc, of: <integer>), <type>)
                        end method,
                        list(<table>,
                             <array>,
                             <vector>,
                             <stretchy-vector>,
                             <string>,
                             <deque>,
                             <range>)));
end test limited-collections-2;

define test limited-collections-3 ()
  check-true("", every?(method (cc)
                          instance?(limited(cc, size: 10), <type>)
                        end method,
                        list(<array>, <vector>, <string>, <range>)));
end test;

define test limited-collections ()
  let small-int = limited(<integer>, min: 0, max: 255);
  let small-int-vector
    = limited(<vector>, of: limited(<integer>, min: 0, max: 255));
  let small-int-vector-3
    = limited(<vector>, size: 3, of: limited(<integer>, min: 0, max: 255));
  check-false("", instance?(#t, small-int));
  check("", instance?, 3, small-int);
  check-false("", instance?(300, small-int));
  check("", instance?, #[3, 4, 5], small-int-vector);
  check-false("", instance?(vector(300, 4, 5), small-int-vector));
  check("", instance?, vector(3, 4, 5), small-int-vector-3);
  check-false("", instance?(vector(300, 4, 5), small-int-vector-3));
  check-false("", instance?(vector(3, 4, 5, 6), small-int-vector-3));
  check-false("", instance?(vector(3, 4), small-int-vector-3));
  check-false("", instance?(vector(), small-int-vector-3));
end test;

// Design note #7 introduces a new method on the "union" generic function

define test union-on-types ()
  check("", instance?, #t, type-union(singleton(#t), singleton(#f)));
  check("", instance?, #f, type-union(singleton(#t), singleton(#f)));
  check("", instance?, #f, type-union(<integer>, singleton(#f)));
  check("", instance?, 99, type-union(<integer>, singleton(#f)));
  check("", instance?, list(1, 2, 3), type-union(<sequence>, singleton(#f)));
  check("", instance?, list(1, 2, 3), type-union(<list>, singleton(#f)));
  check-false("", instance?(0, type-union(singleton(#f), singleton(0), singleton(#[]))));
  check-false("", instance?(#t, type-union(<list>, singleton(#f))));
  check-false("", instance?(vector(1, 2, 3), type-union(<list>, singleton(#f))));
end test;

define test union-on-limited-types ()
  let small-int = limited(<integer>, min: 0, max: 255);
  let small-int-vector
    = limited(<vector>, of: limited(<integer>, min: 0, max: 255));
  let small-int-vector-3
    = limited(<vector>, size: 3, of: limited(<integer>, min: 0, max: 255));
  check("", instance?, #t, type-union(singleton(#t), small-int));
  check("", instance?, 3, type-union(singleton(#t), small-int));
  check-false("", instance?(300, type-union(singleton(#t), small-int)));
  check("", instance?, #[3, 4, 5], type-union(singleton(#t), small-int-vector));
  check-false("", instance?(vector(300, 4, 5), type-union(singleton(#t), small-int-vector)));
  check("", instance?, vector(3, 4, 5), type-union(singleton(#t), small-int-vector-3));
  check-false("", instance?(vector(300, 4, 5), type-union(singleton(#t), small-int-vector-3)));
  check-false("", instance?(vector(3, 4, 5, 6), type-union(singleton(#t), small-int-vector-3)));
  check-false("", instance?(vector(3, 4), type-union(singleton(#t), small-int-vector-3)));
  check-false("", instance?(vector(), type-union(singleton(#t), small-int-vector-3)));
end test;


