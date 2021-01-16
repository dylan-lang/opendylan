Module:    apple-dylan-test-suite
Filename:  test-collection.dylan
Summary:   Apple Dylan test suite, test-collection
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

 // Chapter 12. Collections
 // collection operations


 define test size-type ()
   check-true("", instance?(size, <generic-function>));
 end test size-type;

// Size on list objects
 define test size-0 ()
   check-equal("", list(1, 2, 3, 4).size, 4);
 end test size-0;

// Size on empty-lists
 define test size-1 ()
   check-equal("", size(#()), 0);
   check-equal("", list().size, 0);
 end test size-1;

// Size on range objects
 define test size-2 ()
   check-true("", ~range(from: 3, by: 3).size);
   check-equal("", range(from: 1, below: 11, by: 2).size, 5);
   check-equal("", range(from: 1, to: 11, by: 2).size, 6);
 end test size-2;

// size on deque objects
 define test size-3 ()
   check-equal("", deque-instance(1, 2, 3, 4, 5).size, 5);
 end test size-3;

// size on table objects
 define test size-4 ()
   check-equal("", table-instance(#(1, 2), #(3, 4), #(5, 6)).size, 3);
 end test size-4;

// on stretchy-vectors
 define test size-5 ()
   check-equal("", stretchy-vector-instance(1, 2, 3, 4, 5).size, 5);
 end test size-5;

// Size on simple-object-vector objects
 define test size-6 ()
   check-equal("", vector(1, 2, 3).size, 3);
 end test size-6;

// size on byte-string objects
 define test size-7 ()
   check-equal("", size("now "), 4);
 end test size-7;

 // Design note #12: Add size-setter for stretchy sequences

 define test size-setter-type ()
   check-true("", instance?(size-setter, <generic-function>));
 end test size-setter-type;

// deques
 define test size-setter-1 ()
     let my-deque = make(<deque>, size: 5, fill: 10);
     my-deque.size := 10;
     check-equal("", my-deque.size, 10);
	 let my-deque = make(<deque>, size: 5, fill: 10);
       my-deque.size := 3;
     check-equal("", my-deque.size, 3);
 end test size-setter-1;

// stretchy-vectors
 define test size-setter-2 ()
   begin
     let my-sv = make(<stretchy-vector>, size: 5, fill: 10);
     my-sv.size := 10;
     check-equal("", my-sv.size, 10);
   end;
   begin
       let my-sv = make(<stretchy-vector>, size: 5, fill: 10);
       my-sv.size := 3;
      check-equal("", my-sv.size, 3);
     end;
 end test size-setter-2;

 // type-for-copy mutable-collection

 define test type-for-copy-type ()
   check-true("", instance?(type-for-copy, <generic-function>));
 end test type-for-copy-type;

// of a sequence, should be a subclass of <sequence>
 define test type-for-copy-0 ()
   let r = range(from: 2, to: 4);
   check-true("", instance?(r, <sequence>) & subtype?(r.type-for-copy, <sequence>));
 end test type-for-copy-0;

// of an explicit-key-collection, should be a subclass of <e-k-c>
 define test type-for-copy-1 ()
   let t = make(<table>);
   check-true("", instance?(t, <explicit-key-collection>));
   check-true("", subtype?(t.type-for-copy, <explicit-key-collection>));
 end test type-for-copy-1;

// of bot <e-k-c> and <sequence>, be a subclass of <e-k-c> and <seq>
 define test type-for-copy-3 ()
   let v = make(<vector>);
   check-true("", instance?(v, <explicit-key-collection>));
   check-true("", instance?(v, <sequence>));
   check-true("", subtype?(v.type-for-copy, <explicit-key-collection>));
   check-true("", subtype?(v.type-for-copy, <sequence>));
 end test type-for-copy-3;

 define test empty?-type ()
   check-true("", instance?(empty?, <generic-function>));
 end test empty?-type;

// empty? on lists
 define test empty?-0  ()
   check-true("", ~empty?(#(#"a", #"b", #"c", #"d")));
 end test empty?-0;

// empty? on empty-lists
 define test empty?-1 ()
   check-true("", empty?(#()));
 end test empty?-1;

// empy? on ranges
 define test empty?-2 ()
   check-true("", ~range(from: 1, to: 11, by: 2).empty?);
 end test empty?-2;

// emtpy? on deques
 define test empty?-3 ()
   check-true("", deque-instance().empty? & ~deque-instance(1, 2, 3).empty?);
 end test empty?-3;

// empty? on tables
 define test empty?-4 ()
   check-true("", ~table-instance(#(1, 2), #(2, 3)).empty?);
   check-true("", table-instance().empty?);
 end test empty?-4;

// empty? on stretchy-vectors
 define test empty?-5 ()
   check-true("", ~stretchy-vector-instance(1, 2, 3).empty?);
 end test empty?-5;

// empty on vectors
 define test empty?-6 ()
   check-true("", vector().empty?);
   check-equal("", vector(1, 2, 3).empty?, #f);
 end test empty?-6;

// empty? on strings
 define test empty?-7 ()
   check-true("", empty?(""));
   check-true("", ~empty?("now is the time"));
 end test empty?-7;

 define test do-type ()
   check-true("", instance?(do, <generic-function>));
 end test do-type;

// do returns #f
 define test do-0 ()
   check-true("", ~do(method (a, b)
	pair(a, b.list);
       end method,
       #(100, 100, 200, 200),
       #(1, 2, 3, 4)));
 end test do-0;

// do doesn't run if empty list
 define test do-1 ()
   check-equal("", begin
     let x = 0;
     do(method (a)
	  x := 100
	end method,
	#());
     x;
	    end, 0);
 end test do-1;

// with more than one collection
 define test do-2 ()
   check-equal("", begin
     let sum = 0;
     do(method (a, b)
	  sum := sum + a + b;
	end method,
	#(1, 2, 3, 4, 5),
	#(100, 200, 300, 400, 500));
     sum;
   end, 1515);
 end test do-2;

 define test map-type ()
   check-true("", instance?(map, <generic-function>));
 end test map-type;

// List
 define test map-0 ()
   check-equal("", map(method (x)
			x * -1;
                        end method,
                   #(1, 2, 3, 4)),
    		#(-1, -2, -3, -4));
 end test map-0;

// Empty-List
 define test map-1 ()
   check-equal("", map(method (x)
	 x * -1;
       end method,
       #()),
   #());
 end test map-1;

// Range
 define test map-2 ()
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (range(from: 1, below: 5))),
   #(-1, -2, -3, -4));
 end test map-2;

// Deque
 define test map-3 ()
   check-equal("", map(method (x)
	 x * -1;
       end method,
       deque-instance(1, 2, 3)),
    deque-instance(-1, -2, -3));
 end test map-3;

// Table
 define test map-4 ()
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (table-instance(#(1, 2), #(2, 3)))),
    table-instance(#(1, -2), #(2, -3)));
 end test map-4;

// Stretchy-vector
 define test map-5 ()
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (stretchy-vector-instance(1, 2, 3))),
    stretchy-vector-instance(-1, -2, -3));
 end test map-5;

// Simple-object-vector
 define test map-6 ()
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (vector(1, 2, 3))),
    vector(-1, -2, -3));
 end test map-6;

// String
 define test map-7 ()
   check-equal("", map(method (x)
	 x;
       end method,
       ("ABC")),
    "ABC");
 end test map-7;

// returns a collection whose value is an inst of type-for-copy val
 define test map-8 ()
   let s = list(1, 2, 3, 4);
   check-true("", instance?
     (map(method (x)
	    x * -1;
	  end method,
	  (s)),
      s.type-for-copy));
 end test map-8;

// creates a new collection
 define test map-9 ()
   let s = list(1, 2, 3, 4);
   let new-s
     = map(method (x)
	     x;
	   end method,
	   (s));
   check-true("", \= (s, new-s));
 end test map-9;

// more than one collection arg
 define test map-10 ()
   check-equal("", map(\+, #(100, 200, 300, 400), #(1, 2, 3, 4)),
	    #(101, 202, 303, 404));
   check-equal("", map(method (x)
	   if (x.odd?)
	     '1';
	   else
	     '0';
	   end if;
	 end method,
	 #(1, 2, 3, 4)),
      #('1', '0', '1', '0'));
 end test map-10;

// with bind-exit
 define test map-11 ()
   check-equal("", do(method (seq :: <sequence>)
     block (exit)
       map(method (item)
	     if (item > 5)
	       exit(item);
	     end if;
	   end method,
	   seq);
     end block;
   end method,
     #(1, 2, 3, 4, 5, 6, 7, 8)), 6);
 end test map-11;

 define test map-as-type ()
   check-true("", instance?(map-as, <generic-function>));
 end test map-as-type;

// List
 define test map-as-0 ()
   check-equal("", map-as
     (<list>,
      method (x)
	x * -1;
      end method,
      #(1, 2, 3, 4)),
   #(-1, -2, -3, -4));
 end test map-as-0;

// Empty-List
 define test map-as-1 ()
   check-equal("", map-as
     (<list>,
      method (x)
	x * -1;
      end method,
      #()),
     #());
 end test map-as-1;

// Range
 define test map-as-2 ()
   check-equal("", map-as
     (<list>,
      method (x)
	x * -1;
      end method,
      (range(from: 1, below: 5))),
    #(-1, -2, -3, -4));
 end test map-as-2;

// Deque
 define test map-as-3 ()
   check-equal("", map-as
     (<deque>,
      method (x)
	x * -1;
      end method,
      (deque-instance(1, 2, 3))),
    deque-instance(-1, -2, -3));
 end test map-as-3;

// Table
 define test map-as-4 ()
   check-equal("", map-as
     (<table>,
      method (x)
	x * -1;
      end method,
      (table-instance(#(1, 2), #(2, 3)))),
    table-instance(#(1, -2), #(2, -3)));
 end test map-as-4;

// Stretchy-vector
 define test map-as-5 ()
   check-equal("", map-as
     (<stretchy-vector>,
      method (x)
	x * -1;
      end method,
      (stretchy-vector-instance(1, 2, 3))),
    stretchy-vector-instance(-1, -2, -3));
 end test map-as-5;

// Simple-object-vector
 define test map-as-6 ()
   check-equal("", map-as
     (<simple-object-vector>,
      method (x)
	x * -1;
      end method,
      (vector(1, 2, 3))),
    vector(-1, -2, -3));
 end test map-as-6;

// String
 define test map-as-7 ()
   check-equal("", map-as
     (<string>,
      method (x)
	x;
      end method,
      ("ABC")),
    "ABC");
 end test map-as-7;

// new collection
 define test map-as-8 ()
   let s = #('a', 'b');
   check-true("", \~=(map-as
       (<byte-string>,
	method (x)
	  x;
	end method,
	s), s));
 end test map-as-8;

// various coercions
 define test map-as-9 ()
     let l = map-as(<list>, curry(\+, 1), range(below: 5));
     check-true("", instance?(l, <list>));
     check-equal("", l, #(1, 2, 3, 4, 5));
     let v = map-as(<vector>, \+, #(100, 100, 200, 200), #(1, 2, 3, 4));
     check-true("", instance?(v, <vector>));
     check-equal("", v, #[101, 102, 203, 204]);
       let s
	 = map-as
	     (<string>,
	      method (x)
		if (x.odd?)
		  '1';
		else
		  '0';
		end if
	      end method,
	      #(1, 2, 3, 4));
     check-true("", instance?(s, <string>));
     check-equal("", s, "1010");
 end test map-as-9;

 define test map-into-type ()
   check-true("", instance?(map-into, <generic-function>));
 end test map-into-type;

// the simple cases
 define test map-into-0 ()
     let col = #(1, 0, 3, 0);
     map-into (col, zero?, col);
     check-equal("", col, #(#f, #t, #f, #t));
 end test map-into-0;

// List
 define test map-into-1 ()
   let var = #(1, 2, 3, 4, 5);
   check-equal("", map-into
     (var,
      method (x)
	x * -1;
      end method,
      (#(1, 2, 3, 4))),
    #(-1, -2, -3, -4, 5));
   check-equal("", map-into
       (var,
	method (x)
	  x * -1;
	end method,
       (#(1, 2, 3, 4))),
     var);
 end test map-into-1;

// Empty-List
 define test map-into-2 ()
   let var = #();
   check-equal("", map-into
     (var,
      method (x)
	x * -1;
      end method,
      (#())),
    #());
   check-equal("", map-into
       (var,
	method (x)
	  x * -1;
	end method,
	(#())),
       var);
 end test map-into-2;

// Range
 define test map-into-3 ()
   let var = #(1, 2, 3, 4, 5);
   check-equal("", map-into
     (var,
      method (x)
	x * -1;
      end method,
      (range(from: 1, below: 5))),
     #(-1, -2, -3, -4, 5));
   check-equal("", map-into
       (var,
	method (x)
	  x * -1;
	end method,
	(range(from: 1, below: 5))),
      var);
 end test map-into-3;

// Deque
 define test map-into-4 ()
   let var = deque-instance(1, 2, 3, 4);
   check-equal("", map-into
     (var,
      method (x)
	x * -1;
      end method,
      (deque-instance(1, 2, 3))),
    deque-instance(-1, -2, -3, 4));
   check-equal("", map-into
       (var,
	method (x)
	  x * -1;
	end method,
	(deque-instance(1, 2, 3))),
      var);
 end test map-into-4;

// Table
 define test map-into-5 ()
   let var = table-instance(#(1, 2), #(2, 3));
   check-equal("", map-into
     (var,
      method (x)
	x * -1;
      end method,
      (table-instance(#(1, 2), #(2, 3)))),
    table-instance(#(1, -2), #(2, -3)));
   check-equal("", map-into
       (var,
	method (x)
	  x * -1;
	end method,
	(table-instance(#(1, 2), #(2, 3)))),
      var);
 end test map-into-5;

// Stretchy-vector
define test map-into-6 ()
  let var = stretchy-vector-instance(1, 2, 3);
  check-equal("", map-into
    (var,
     method (x)
       x * -1;
     end method,
     (stretchy-vector-instance(1, 2, 3))),
   stretchy-vector-instance(-1, -2, -3));
  check-equal("", map-into
      (var,
       method (x)
         x * -1;
       end method,
       (stretchy-vector-instance(1, 2, 3))),
     var);
end test map-into-6;
