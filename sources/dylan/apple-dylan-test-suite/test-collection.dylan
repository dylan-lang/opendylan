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

 define test size-0 (description: "Size on list objects")
   check-equal("", list(1, 2, 3, 4).size, 4);
 end test size;

 define test size-1 (description: "Size on empty-lists")
   check-equal("", size(#()), 0);
   check-equal("", list().size, 0);
 end test size-1;

 define test size-2 (description: "Size on range objects")
   check-true("", ~range(from: 3, by: 3).size);
   check-equal("", range(from: 1, below: 11, by: 2).size, 5);
   check-equal("", range(from: 1, to: 11, by: 2).size, 6);
 end test size-2;

 define test size-3 (description: "size on deque objects")
   check-equal("", deque-instance(1, 2, 3, 4, 5).size, 5);
 end test size-3;

 define test size-4 (description: "size on table objects")
   check-equal("", table-instance(#(1, 2), #(3, 4), #(5, 6)).size, 3);
 end test size-4;

 define test size-5 (description: "on stretchy-vectors")
   check-equal("", stretchy-vector-instance(1, 2, 3, 4, 5).size, 5);
 end test size-5;

 define test size-6 (description: "Size on simple-object-vector objects")
   check-equal("", simple-object-vector-instance(1, 2, 3).size, 3);
 end test size-6;

 define test size-7 (description: "size on byte-string objects")
   check-equal("", size("now "), 4);
 end test size-7;

 // Design note #12: Add size-setter for stretchy sequences

 define test size-setter-type ()
   check-true("", instance?(size-setter, <generic-function>));
 end test size-setter-type;

 define test size-setter-1 (description: "deques")
     let my-deque = make(<deque>, size: 5, fill: 10);
     my-deque.size := 10;
     check-equal("", my-deque.size, 10);
	 let my-deque = make(<deque>, size: 5, fill: 10);
       my-deque.size := 3;
     check-equal("", my-deque.size, 3);
 end test size-setter-1;

 define test size-setter-2 (description: "stretchy-vectors")
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

 // class-for-copy mutable-collection

 define test class-for-copy-type ()
   check-true("", instance?(class-for-copy, <generic-function>));
 end test class-for-copy-type;

 define test class-for-copy-0
 (description: "of a sequence, should be a subclass of <sequence>")
   let r = range(from: 2, through: 4);
   check-true("", instance?(r, <sequence>) & subtype?(r.class-for-copy, <sequence>));
 end test class-for-copy;

 define test class-for-copy-1 
  (description: "of an explicit-key-collection, should be a subclass of <e-k-c>")
   let t = make(<table>);
   check-true("", instance?(t, <explicit-key-collection>));
   check-true("", subtype?(t.class-for-copy, <explicit-key-collection>));
 end test class-for-copy-1;

 define test class-for-copy-3 
   (description: "of bot <e-k-c> and <sequence>, be a subclass of <e-k-c> and <seq>")
   let v = make(<vector>);
   check-true("", instance?(v, <explicit-key-collection>));
   check-true("", instance?(v, <sequence>));
   check-true("", subtype?(v.class-for-copy, <explicit-key-collection>));
   check-true("", subtype?(v.class-for-copy, <sequence>));
 end test class-for-copy-3;

 define test empty?-type ()
   check-true("", instance?(empty?, <generic-function>));
 end test empty?-type;

 define test empty?-0  (description: "empty? on lists")
   check-true("", ~empty?(#(#"a", #"b", #"c", #"d")));
 end test empty?;

 define test empty?-1 (description: "empty? on empty-lists")
   check-true("", empty?(#()));
 end test empty?-1;

 define test empty?-2 (description: "empy? on ranges")
   check-true("", ~range(from: 1, to: 11, by: 2).empty?);
 end test empty?-2;

 define test empty?-3 (description: "emtpy? on deques")
   check-true("", deque-instance().empty? & ~deque-instance(1, 2, 3).empty?);
 end test empty?-3;

 define test empty?-4 (description: "empty? on tables")
   check-true("", ~table-instance(#(1, 2), #(2, 3)).empty?);
   check-true("", table-instance().empty?);
 end test empty?-4;

 define test empty?-5 (description: "empty? on stretchy-vectors")
   check-true("", ~stretchy-vector-instance(1, 2, 3).empty?);
 end test empty?-5;

 define test empty?-6 (description: "empty on vectors")
   check-true("", simple-object-vector-instance().empty?);
   check-equal("", simple-object-vector-instance(1, 2, 3).empty?, #f);
 end test empty?-6;

 define test empty?-7 (description: "empty? on strings")
   check-true("", empty?(""));
   check-true("", ~empty?("now is the time"));
 end test empty?-7;

 define test do-type ()
   check-true("", instance?(do, <generic-function>));
 end test do-type;

 define test do-0 (description: "do returns #f")
   check-true("", ~do(method (a, b)
	pair(a, b.list);
       end method,
       #(100, 100, 200, 200),
       #(1, 2, 3, 4)));
 end test do;

 define test do-1 (description: "do doesn't run if empty list")
   check-equal("", begin
     let x = 0;
     do(method (a)
	  x := 100
	end method,
	#());
     x;
	    end, 0);
 end test do-1;

 define test do-2 (description: "with more than one collection")
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

 define test map-0 (description: "List")
   check-equal("", map(method (x)
			x * -1;
                        end method,
                   #(1, 2, 3, 4)),
    		#(-1, -2, -3, -4));
 end test map;

 define test map-1 (description: "Empty-List")
   check-equal("", map(method (x)
	 x * -1;
       end method,
       #()),
   #());
 end test map-1;

 define test map-2 (description: "Range")
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (range(from: 1, below: 5))),
   #(-1, -2, -3, -4));
 end test map-2;

 define test map-3 (description: "Deque")
   check-equal("", map(method (x)
	 x * -1;
       end method,
       deque-instance(1, 2, 3)),
    deque-instance(-1, -2, -3));
 end test map-3;

 define test map-4 (description: "Table")
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (table-instance(#(1, 2), #(2, 3)))),
    table-instance(#(1, -2), #(2, -3)));
 end test map-4;

 define test map-5 (description: "Stretchy-vector")
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (stretchy-vector-instance(1, 2, 3))),
    stretchy-vector-instance(-1, -2, -3));
 end test map-5;

 define test map-6 (description: "Simple-object-vector")
   check-equal("", map(method (x)
	 x * -1;
       end method,
       (simple-object-vector-instance(1, 2, 3))),
    simple-object-vector-instance(-1, -2, -3));
 end test map-6;

 define test map-7 (description: "String")
   check-equal("", map(method (x)
	 x;
       end method,
       ("ABC")),
    "ABC");
 end test map-7;

 define test map-8 
   (description: "returns a collection whose value is an inst of class-for-copy val")
   let s = list(1, 2, 3, 4);
   check-true("", instance?
     (map(method (x)
	    x * -1;
	  end method,
	  (s)),
      s.class-for-copy));
 end test map-8;

 define test map-9 (description: "creates a new collection")
   let s = list(1, 2, 3, 4);
   let new-s
     = map(method (x)
	     x;
	   end method,
	   (s));
   check-true("", \= (s, new-s));
 end test map-9;

 define test map-10 (description: "more than one collection arg")
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

 define test map-11 (description: "with bind-exit")
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

 define test map-as-0 (description: "List")
   check-equal("", map-as
     (<list>,
      method (x)
	x * -1;
      end method,
      #(1, 2, 3, 4)),
   #(-1, -2, -3, -4));
 end test map-as;

 define test map-as-1 (description: "Empty-List")
   check-equal("", map-as
     (<list>,
      method (x)
	x * -1;
      end method,
      #()),
     #());
 end test map-as-1;

 define test map-as-2 (description: "Range")
   check-equal("", map-as
     (<list>,
      method (x)
	x * -1;
      end method,
      (range(from: 1, below: 5))),
    #(-1, -2, -3, -4));
 end test map-as-2;

 define test map-as-3 (description: "Deque")
   check-equal("", map-as
     (<deque>,
      method (x)
	x * -1;
      end method,
      (deque-instance(1, 2, 3))),
    deque-instance(-1, -2, -3));
 end test map-as-3;

 define test map-as-4 (description: "Table")
   check-equal("", map-as
     (<table>,
      method (x)
	x * -1;
      end method,
      (table-instance(#(1, 2), #(2, 3)))),
    table-instance(#(1, -2), #(2, -3)));
 end test map-as-4;

 define test map-as-5 (description: "Stretchy-vector")
   check-equal("", map-as
     (<stretchy-vector>,
      method (x)
	x * -1;
      end method,
      (stretchy-vector-instance(1, 2, 3))),
    stretchy-vector-instance(-1, -2, -3));
 end test map-as-5;

 define test map-as-6 (description: "Simple-object-vector")
   check-equal("", map-as
     (<simple-object-vector>,
      method (x)
	x * -1;
      end method,
      (simple-object-vector-instance(1, 2, 3))),
    simple-object-vector-instance(-1, -2, -3));
 end test map-as-6;

 define test map-as-7 (description: "String")
   check-equal("", map-as
     (<string>,
      method (x)
	x;
      end method,
      ("ABC")),
    "ABC");
 end test map-as-7;

 define test map-as-8 (description: "new collection")
   let s = #('a', 'b');
   check-true("", \~=(map-as
       (<byte-string>,
	method (x)
	  x;
	end method,
	s), s));
 end test map-as-8;

 define test map-as-9 (description: "various coercions")
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

 define test map-into-0 (description: "the simple cases")
     let col = #(1, 0, 3, 0);
     map-into (col, zero?, col);
     check-equal("", col, #(#f, #t, #f, #t));
 end test map-into;

 define test map-into-1 (description: "List")
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

 define test map-into-2 (description: "Empty-List")
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

 define test map-into-3 (description: "Range")
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

 define test map-into-4 (description: "Deque")
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

 define test map-into-5 (description: "Table")
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

define test map-into-6 (description: "Stretchy-vector")
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