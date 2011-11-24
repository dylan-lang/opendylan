Module:    apple-dylan-test-suite
Filename:  test-sequence.dylan
Summary:   Apple Dylan test suite, test-sequence
Version:   29-Oct-93
Copyright: (c) 1993 Apple Computer, Inc.

/*---------------------------------------------
Modified by: Shri Amit(amit)
Date: August 24 1996
Summary: Converted to new testworks protocol
Copyright: (c) 1996 Functional Objects, Inc. 
           All rights reserved.  
----------------------------------------------*/

define test add-type (description: "")
  check-true("", instance?(add, <generic-function>));
end test add-type;

define test add-1 (description: "add")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let new-collection = add(collection, new-element);
       check-true("", collection == new-collection);
       check-true("", collection = new-collection);
       check-true("", 1 + size1 = new-collection.size);
       check-true("", every?(rcurry(member?, collection), elements));
       check-true("", every?(rcurry(member?, new-collection), pair(new-element, elements)));
     end method,
     list(list(#(), list(), 99),
          list(list(7, 8, 9), list(7, 8, 9), 99),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 99),
          list(vector(7, 8, 9), list(7, 8, 9), 99),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 99),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 99),
          list(simple-object-vector-instance(7, 8, 9), list(7, 8, 9), 99),
          list(byte-string-instance('Y', 'o', 'w'), list('Y', 'o', 'w'), '!'),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!')))
end test add-1;

define test add-2
  (description: "new sequence shares no structure with sequence")
  let s = list(#(#"a", #"b"));
  let new-element = #(#"c", #"d");
  let new-s = add(s, new-element);
  check-false ("", share-struct?(s, new-s));
end test add-2;

define test add!-type (description: "")
  check-true("", instance?(add!, <generic-function>));
end test add!-type;

define test add!-1 (description: "add")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let id-test? = s[3];
       let new-collection = add!(collection, new-element);
       check-true("", 1 + size1 = new-collection.size);
       check-true("", if (id-test?)
           collection == new-collection
         else
           #t
         end if);
       check-true("", every?(rcurry(member?, new-collection), pair(new-element, elements)));
     end method,
     list(list(#(), list(), 99, #f),
          list(list(7, 8, 9), list(7, 8, 9), 99, #f),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 99, #f),
          list(vector(7, 8, 9), list(7, 8, 9), 99, #f),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 99, #t),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 99, #t),
          list(simple-object-vector-instance(7, 8, 9), list(7, 8, 9), 99, #f),
          list(byte-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               #f),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               #f)))
end test add!-1;

define test add!-position
  (description: "makes sure new value is at front or end")
  check-true("", add!(list(7, 8, 9), 88).first = 88);
  check-true("", add!(deque-instance(7, 8, 9), 88).first = 88);
  check-true("", add!(stretchy-vector-instance(7, 8, 9), 88).last = 88);
end test add!-position;

define test add-new-type (description: "")
  check-true("", instance?(add-new, <generic-function>));
end test add-new-type;

define test add-new-1 (description: "add new element")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let new-collection = add-new(collection, new-element);
       check-false("", collection == new-collection);
       check-false("", collection = new-collection);
       check-true("", 1 + size1 = new-collection.size);
       check-true("", every?(rcurry(member?, collection), elements));
       check-true("", every?(rcurry(member?, new-collection), pair(new-element, elements)));
     end method,
     list(list(#(), list(), 99),
          list(list(7, 8, 9), list(7, 8, 9), 99),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 99),
          list(vector(7, 8, 9), list(7, 8, 9), 99),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 99),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 99),
          list(simple-object-vector-instance(7, 8, 9), list(7, 8, 9), 99),
          list(byte-string-instance('Y', 'o', 'w'), list('Y', 'o', 'w'), '!'),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!')))
end test add-new-1;

define test add-new-2 (description: "add old element")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let new-collection = add-new(collection, new-element);
       check-true("", collection == new-collection);
       check-true("", (size1 = new-collection.size & size1 = elements.size));
       check-true("", collection = new-collection);
       check-true("", every?(rcurry(member?, collection), elements));
       check-true("", every?(rcurry(member?, new-collection), elements));
     end method,
     list(list(list(7, 8, 9), list(7, 8, 9), 8),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 8),
          list(vector(7, 8, 9), list(7, 8, 9), 8),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 8),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 8),
          list(simple-object-vector-instance(7, 8, 9), list(7, 8, 9), 8),
          list(byte-string-instance('Y', 'o', 'w'), list('Y', 'o', 'w'), 'o'),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               'o')))
end test add-new-2;

// These tests are used in membership tests on collections later on.

define method divides? (x, y)
  check-true("", 0 = modulo(y, x));
end method divides?;

define constant caseless=?
  = method (c1 :: <character>, c2 :: <character>)
      c1.as-uppercase = c2.as-uppercase
    end method;

// test is always called with an element from sequence as its first argument,
// and new-element as its second argument (p 105)
//

define test add-new-3 (description: "add new element, using test: argument")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let test = s[3];
       let size1 = s.first.size;
       let new-collection = add-new(collection, new-element, test: test);
       check-false("", collection == new-collection);
       check-false("", collection = new-collection);
       check-true("", 1 + size1 = new-collection.size);
       check-true("", every?(rcurry(member?, collection), elements));
       check-true("", every?(rcurry(member?, new-collection), pair(new-element, elements)));
     end method,
     list(list(#(), list(), 17, divides?),
          list(list(7, 8, 9), list(7, 8, 9), 17, divides?),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 17, divides?),
          list(vector(7, 8, 9), list(7, 8, 9), 17, divides?),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 17, divides?),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 17, divides?),
          list(simple-object-vector-instance(7, 8, 9),
               list(7, 8, 9),
               17,
               divides?),
          list(byte-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               caseless=?),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               caseless=?)))
end test add-new-3;

define test add-new-4 (description: "add old element, using test: argument")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let test = s[3];
       let size1 = s.first.size;
       let new-collection = add-new(collection, new-element, test: test);
       check-false("", collection == new-collection);
       check-true("", (size1 = new-collection.size & size1 = elements.size));
       check-true("", collection = new-collection);
       check-true("", every?(rcurry(member?, collection), elements));
       check-true("", every?(rcurry(member?, new-collection), elements));
     end method,
     list(list(list(7, 8, 9), list(7, 8, 9), 64, divides?),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 64, divides?),
          list(vector(7, 8, 9), list(7, 8, 9), 64, divides?),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 64, divides?),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 64, divides?),
          list(simple-object-vector-instance(7, 8, 9),
               list(7, 8, 9),
               64,
               divides?),
          list(byte-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               'O',
               caseless=?),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               'O',
               caseless=?)))
end test add-new-4;

// add-new!

define test add-new!-type (description: "")
  check-true("", instance?(add-new!, <generic-function>));
end test add-new!-type;

define test add-new!-1 (description: "add new element")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let id-test? = s[3];
       let new-collection = add-new!(collection, new-element);
       check-true("", 1 + size1 = new-collection.size);
       check-true("", if (id-test?)
           collection == new-collection
         else
           #t
         end if);
       check-true("", every?(rcurry(member?, new-collection), pair(new-element, elements)));
     end method,
     list(list(#(), list(), 99, #f),
          list(list(7, 8, 9), list(7, 8, 9), 99, #f),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 99, #f),
          list(vector(7, 8, 9), list(7, 8, 9), 99, #f),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 99, #t),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 99, #t),
          list(simple-object-vector-instance(7, 8, 9), list(7, 8, 9), 99, #f),
          list(byte-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               #f),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               #f)))
end test add-new!-1;

define test add-new!-2 (description: "add old element")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let new-collection = add-new!(collection, new-element);
       check-true("", (size1 = new-collection.size & size1 = elements.size));
       check-true("", collection == new-collection);
       check-true("", every?(rcurry(member?, new-collection), elements));
     end method,
     list(list(list(7, 8, 9), list(7, 8, 9), 8),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 8),
          list(vector(7, 8, 9), list(7, 8, 9), 8),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 8),
          list(stretchy-vector-instance(7, 8, 9), list(7, 8, 9), 8),
          list(simple-object-vector-instance(7, 8, 9), list(7, 8, 9), 8),
          list(byte-string-instance('Y', 'o', 'w'), list('Y', 'o', 'w'), 'o'),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               'o')))
end test add-new!-2;

define test add-new!-3 (description: "add new element, using test: argument")
  every?
    (method (s)
       let collection = s.first;
       let elements = s.second;
       let new-element = s.third;
       let size1 = s.first.size;
       let id-test? = s[3];
       let test = s[4];
       let new-collection = add-new!(collection, new-element);
       check-true("", 1 + size1 = new-collection.size);
       check-true("", if (id-test?)
           collection == new-collection
         else
           #t
         end if);
       check-true("", every?(rcurry(member?, new-collection), pair(new-element, elements)));
     end method,
     list(list(#(), list(), 17, #f, divides?),
          list(list(7, 8, 9), list(7, 8, 9), 17, #f, divides?),
          list(range(from: 7, up-to: 10), list(7, 8, 9), 17, #f, divides?),
          list(vector(7, 8, 9), list(7, 8, 9), 17, #f, divides?),
          list(deque-instance(7, 8, 9), list(7, 8, 9), 17, #t, divides?),
          list(stretchy-vector-instance(7, 8, 9),
               list(7, 8, 9),
               17,
               #t,
               divides?),
          list(simple-object-vector-instance(7, 8, 9),
               list(7, 8, 9),
               17,
               #f,
               divides?),
          list(byte-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               #f,
               caseless=?),
          list(unicode-string-instance('Y', 'o', 'w'),
               list('Y', 'o', 'w'),
               '!',
               #f,
               caseless=?)))
end test add-new!-3;

