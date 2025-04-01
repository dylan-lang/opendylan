Module: dylan-test
author: Roger Critchlow (rec@elf.org)
synopsis: A regression test for core Dylan.

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports, questions, comments, and suggestions should be sent by
// E-mail to the Internet address "gwydion-bugs@cs.cmu.edu".
//
//======================================================================
//
// This program runs a set of simple minded tests past the compiler
// and interpreter.  Lots more tests could be added, obviously, but
// even these few have turned up some problems.  A large block are copied
// from the DIRM examples.
//

define constant buggy? = #f;		// not bugs, features!

define variable tautologies =
  #(#"booleans",
    #"comparisons",
    #"numbers",
    #"characters",
    // #"symbols",
    #"collections",
    #"sequences",
    // #"arrays",
    // #"deques",
    #"lists",
    #"ranges",
    #"stretchy vectors",
    #"strings",
    // #"tables",
    #"vectors",
    #"slots",
    #"dispatch-and-keywords-1",
    #"dispatch-and-keywords-2",
    #"simple-typeunion"
      );

define method tautology(arg == #"booleans")
  (#t)				| signal("#t is not true!\n");
  (#T)				| signal("#T is not true!\n");
  (#f)				& signal("#f is not false!\n");
  (#F)				& signal("#F is not false!\n");
  (#t & #t)			| signal("#t & #t is not true!\n");
  (#t | #t)			| signal("#t | #t is not true!\n");
  (#f & #f)			& signal("#f & #f is not false!\n");
  (#f | #f)			& signal("#f | #f is not false!\n");
  (#t & #f)			& signal("#t & #f is not false!\n");
  (#t | #f)			| signal("#t | #f is not true!\n");
  (#f & #t)			& signal("#f & #t is not false!\n");
  (#f | #t)			| signal("#f | #t is not true!\n");
end method;

define method tautology(arg == #"comparisons")
  (1 = 1)			| signal("1 is not equal to 1!\n");
  (1 == 1)			| signal("1 is not really equal to 1!\n");
  (1 ~= 1)			& signal("1 is not equal to 1!\n");
  (1 < 2)			| signal("1 is not less than 2!\n");
  (1 >= 2)			& signal("1 is greater than or equal to 2!\n");
  (1 <= 2)			| signal("1 is not less than or equal to 2!\n");
  (1 > 2)			& signal("1 is greater than 2!\n");
  ('a' < 'b')			| signal("'a' is greater than 'b'!\n");
  ("A" < "B")			| signal("\"A\" is greater than \"B\"!\n");
end method;

define method tautology(arg == #"numbers")
  instance?(1, <number>)		| signal("1 is not a <number>");
  instance?(1, <real>)			| signal("1 is not a <real>!\n");
  instance?(1, <float>)			& signal("1 is a <float>!\n");
  instance?(1, <single-float>)		& signal("1 is a <single-float>!\n");
  instance?(1, <double-float>)		& signal("1 is a <double-float>!\n");
  instance?(1, <extended-float>)	& signal("1 is a <extended-float>!\n");
  instance?(1, <rational>)		| signal("1 is not a <rational>!\n");
  instance?(1, <integer>)		| signal("1 is not a <integer>!\n");
  instance?(1, <complex>)		| signal("1 is not a <complex>!\n");
  odd?(1)				| signal("1 is not odd!\n");
  even?(2)				| signal("2 is not even!\n");
  zero?(0)				| signal("0 is not zero!\n");
  positive?(+1)				| signal("+1 is not positive!\n");
  negative?(-1)				| signal("-1 is not negative!\n");
  integral?(+1)				| signal("+1 is not integral!\n");
  integral?(0)				| signal("0 is not integral!\n");
  integral?(-1) 			| signal("-1 is not integral!\n");
  (1 + 1 = 2)				| signal("1 + 1 is not 2!\n");
  (2 * 2 = 4)				| signal("2 * 2 is not 4!\n");
  (1 - 1 = 0)				| signal("1 - 1 is not 0!\n");
  (negative(1) = -1)			| signal("negative(1) is not -1!\n");
  (abs(1) = 1)				| signal("abs(1) is not 1!: it's %=\n", abs(1));
  (abs(-1) = 1)				| signal("abs(-1) is not 1!: it's %=\n", abs(-1));
  (logior(1,2) = 3)			| signal("logior(1,2) is not 3!: it's %=\n", logior(1,2));
  (logxor(1,3) = 2)			| signal("logxor(1,3) is not 2!: it's %=\n", logxor(1,3));
  (logand(1,3) = 1)			| signal("logand(1,3) is not 1!: it's %=\n", logand(1,3));
  (lognot(4660) = -4661)		| signal("lognot(4660) is not -4661!: it's %=\n", lognot(4660));
//  logbit?(15,#x8000) 			| signal("logbit?(15,#x8000) is not true!\n");
  (ash(1,3) = 8)			| signal("ash(1,3) is not 8!: it's %=\n", ash(1,3));
  (lcm(6,8) = 24)			| signal("lcm(6,8) is not 24!: it's %=\n", lcm(6,8));
  (gcd(6,8) = 2)			| signal("gcd(6,8) is not 2!: it's %=\n", gcd(6,8));
  (min(1,2) = 1)			| signal("min(1,2) is not 1!: it's %=\n", min(1,2));
  (max(1,2) = 2)			| signal("max(1,2) is not 2!: it's %=\n", max(1,2));
end method;

define method tautology(arg == #"characters")
  instance?('a', <character>)		| signal("'a' is not a <character>!\n");
  (as-uppercase('a') = 'A')		| signal("as-uppercase('a') is not 'A'!\n");
  (as-lowercase('A') = 'a')		| signal("as-lowercase('A') is not 'a'!\n");
  (as(<integer>, ' ') = 32)		| signal("as(<integer>, ' ') is not 32!\n");
  (as(<character>, 32) = ' ')		| signal("as(<character>, 32) is not ' '!\n");
end method;

define method tautology(arg == #"collections")
  (size(#()) = 0)
    | signal("size(#()) is not zero!\n");
  (size(#[]) = 0)
    | signal("size(#[]) is not zero!\n");
  empty?(#())
    | signal("#() is not empty!\n");
  empty?(#[])
    | signal("#[] is not empty!\n");
  (size(list()) == 0)
    | signal("size(list()) is not zero!\n");
  (size(vector()) == 0)
    | signal("size(vector()) is not zero!\n");
  empty?(list())
    | signal("list() is not empty!\n");
  empty?(vector())
    | signal("vector() is not empty!\n");
  do(\+, #(1,2), #(3, 2))
    & signal("do returned #t!\n");
  (map(\+, #(100, 100, 200, 200), #(1, 2, 3, 4)) = #(101, 102, 203, 204))
    | signal("map(\\+, #(100, 100, 200, 200), #(1, 2, 3, 4)) is not #(101, 102, 203, 204)! It's %=\n",
	     map(\+, #(100, 100, 200, 200), #(1, 2, 3, 4)));
  (map(\+, #(1,2), #(3,2)) = #(4,4))
    | signal("map(\\+, #(1,2), #(3,2)) is not equal to #(4,4)! It's %=\n",
	     map(\+, #(1,2), #(3,2)));
  (map-as(<vector>, \+, #(1,2), #(3,2)) = #[4,4])
    | signal("map-as(<vector>, \\+, #(1,2), #(3,2)) is not equal to #[4,4]! It's %=\n",
	     map-as(<vector>, \+, #(1,2), #(3,2)));
  // NB - the DIRM example for map-into is in error.
  let x = list(100, 100, 200, 200);
  (map-into(x, \+, #(100, 100, 200, 200), #(1, 2, 3, 4)) = #(101, 102, 203, 204))
    | signal("map-into (x, \\+, #(100, 100, 200, 200), #(1, 2, 3, 4)) is not equal to #(101, 102, 203, 204)! It's %=\n",
	     map-into (x, \+, x, #(1, 2, 3, 4)));
  (x = #(101, 102, 203, 204))
    | signal("map-into'ed x is not equal to #(101, 102, 203, 204)! It's %=\n", x);
  any?(\>, #(1,2,3,4), #(5,4,3,2))
    | signal("any?(\\>, #(1,2,3,4), #(5,4,3,2)) is not true!\n");
  any?(even?, #(1, 3, 5, 7))
    & signal("any?(even?, #(1, 3, 5, 7)) is not false!\n");
  every?(\>, #(1,2,3,4), #(5,4,3,2))
    & signal("every?(\\>, #(1,2,3,4), #(5,4,3,2)) is true!\n");
  every? (odd?, #(1, 3, 5, 7))
    | signal("every? (odd?, #(1, 3, 5, 7)) is false!\n");
  let high-score = 10;
  (reduce (max, high-score, #(3, 1, 4, 1, 5, 9)) = 10)
    | signal("reduce(max, high-score, #(3, 1, 4, 1, 5, 9)) is not 10! It's %=\n",
	     reduce(max, high-score, #(3, 1, 4, 1, 5, 9)));
  (reduce(max, high-score, #(3, 12, 9, 8, 8, 6)) = 12)
    | signal(" reduce(max, high-score, #(3, 12, 9, 8, 8, 6)) is not 12! It's %=\n",
	     reduce(max, high-score, #(3, 12, 9, 8, 8, 6)));
  (reduce1(\+, #(1, 2, 3, 4, 5)) = 15)
    | signal("reduce1(\\+, #(1, 2, 3, 4, 5)) is not 15! It's %=\n",
	     reduce1(\+, #(1, 2, 3, 4, 5)));
  let flavors = #(#"vanilla", #"pistachio", #"ginger");
  member?(#"vanilla", flavors)
    | signal("member?(#\"vanilla\", flavors) is false!\n");
  member?(#"banana", flavors)
    & signal("member?(#\"banana\", flavors) is true!\n");
  local method has-nuts?(flavor) member?(flavor, #(#"pistachio")) end;
  (find-key(flavors, has-nuts?) = 1)
    | signal("find-key(flavors, has-nuts?) is not 1! It's %=\n",
	   find-key(flavors, has-nuts?));
  local method double(n) 2 * n end;
  let numbers = list (10, 13, 16, 19);
  (replace-elements!(numbers, odd?, double) = #(10, 26, 16, 38))
    | signal("replace-elements!(numbers, odd?, double) is not #(10, 26, 16, 38)! It's %=\n",
	     replace-elements!(numbers, odd?, double));
  (fill!(numbers, 3, start: 2) = #(10, 26, 3, 3))
    | signal("fill! (numbers, 3, start: 2) is not #(10, 26, 3, 3)!  It's %=\n",
	     fill! (numbers, 3, start: 2));
  key-test(list())
    | signal("no key-test for list()!\n");
  key-test(vector())
    | signal("no key-test for vector()!\n");
end method;

define method tautology(arg == #"sequences")
  let numbers = #(3, 4, 5);
  (add(numbers, 1) = #(1, 3, 4, 5))
    | signal("add(numbers, 1) is not #(1, 3, 4, 5))!  It's %=\n", add(numbers, 1));
  let numbers = list (3, 4, 5);
  (add!(numbers, 1) = #(1, 3, 4, 5))
    | signal("add!(numbers, 1) is not #(1, 3, 4, 5))!  It's %=\n", add!(numbers, 1));
  (add-new(#(3, 4, 5), 1) = #(1, 3, 4, 5))
    | signal("add-new (#(3, 4, 5), 1) is not #(1, 3, 4, 5)!  It's %=\n", add-new (#(3, 4, 5), 1));
  (add-new(#(3, 4, 5), 4) = #(3, 4, 5))
    | signal("add-new (#(3, 4, 5), 4) is not #(3, 4, 5)!  It's %=\n", add-new (#(3, 4, 5), 4));
  (add-new!(list (3, 4, 5), 1) = #(1, 3, 4, 5))
    | signal("add-new! (list (3, 4, 5), 1) is not #(1, 3, 4, 5)!  It's %=\n", add-new! (list (3, 4, 5), 1));
  (remove(#(3, 1, 4, 1, 5, 9), 1) = #(3, 4, 5, 9))
    | signal("remove (#(3, 1, 4, 1, 5, 9), 1) is not #(3, 4, 5, 9)! It's %=\n", remove (#(3, 1, 4, 1, 5, 9), 1));
  (remove!(list(3, 1, 4, 1, 5, 9), 1) = #(3, 4, 5, 9))
    | signal("remove! (list(3, 1, 4, 1, 5, 9), 1) is not #(3, 4, 5, 9)! It's %=\n", remove! (list(3, 1, 4, 1, 5, 9), 1));
  (choose(even?, #(3, 1, 4, 1, 5, 9)) = #(4))
    | signal("choose (even?, #(3, 1, 4, 1, 5, 9)) is not #(4)!  It's %=\n", choose (even?, #(3, 1, 4, 1, 5, 9)));
  (choose-by(even?, range (from: 1), #("a", "b", "c", "d", "e", "f", "g", "h", "i")) =  #("b", "d", "f", "h"))
    | signal("choose-by(even?, range (from: 1), #(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\")) is %=!\n",
	     choose-by(even?, range (from: 1), #("a", "b", "c", "d", "e", "f", "g", "h", "i")));
  let b = #("john", "paul", "george", "ringo");
  let c = #("richard", "george", "edward", "charles");
  (intersection(b, c, test: \=) = #("george"))
    | signal("intersection(b, c, test: \\=) is not #(\"george\")!  It's %=\n", intersection (b, c, test: \=));
  let a = #("butter", "flour", "sugar", "salt", "eggs");
  let b = #("eggs", "butter", "mushrooms", "onions", "salt");
  let c = #("salt", "butter", "flour", "sugar", "eggs", "mushrooms", "onions");
  (sort(union(a, b, test: \=)) = sort(c))
    | signal("union(a, b, test: \\=) is c! It's %=\n", sort(union(a, b, test: \=)));
  let a = #("spam", "eggs", "spam", "sausage", "spam", "spam");
  let b = #("spam", "eggs", "sausage");
  (sort(remove-duplicates(a, test: \=)) = sort(b))
    | signal("remove-duplicates(a, test: \\=) is not b!  It's %=\n", sort(remove-duplicates(a, test: \=)));
  let a = list("spam", "eggs", "spam", "sausage", "spam", "spam");
  (sort(remove-duplicates!(a, test: \=)) = sort(b))
    | signal("remove-duplicates!(a, test: \\=) is not b!  It's %=\n", sort(remove-duplicates!(a, test: \=)));
  let hamlet = #("to", "be", "or", "not", "to", "be");
  (copy-sequence(hamlet) == hamlet)
    & signal("copy-sequence(hamlet) is identical to hamlet!\n");
  (copy-sequence(hamlet, start: 2, end: 4) = #("or", "not"))
    | signal("copy-sequence(hamlet, start: 2, end: 4) is not #(\"or\", \"not\")!  It's %=\n",
	     copy-sequence(hamlet, start: 2, end: 4));
  (concatenate-as(<string>, #('n', 'o', 'n'), #('f', 'a', 't')) = "nonfat")
    | signal("concatenate-as(<string>, #('n', 'o', 'n'), #('f', 'a', 't')) is not \"nonfat\"! It's %=\n",
	     concatenate-as(<string>, #('n', 'o', 'n'), #('f', 'a', 't')));
  (concatenate("low-", "calorie") = "low-calorie")
    | signal("concatenate(\"low-\", \"calorie\") is not \"low-calorie\"!  It's %=\n",
	     concatenate("low-", "calorie"));
  let x = list("a", "b", "c", "d", "e");
  let abcde = replace-subsequence!(x, #("x", "y", "z"), end: 1);
  (abcde = #("x", "y", "z", "b", "c", "d", "e"))
    | signal("abcde is not #(\"x\", \"y\", \"z\", \"b\", \"c\", \"d\", \"e\")!  It's %=\n", abcde);
  let x = #("bim", "bam", "boom");
  (reverse(x) = #("boom", "bam", "bim"))
    | signal("reverse(x) is not #(\"boom\", \"bam\", \"bim\")! It's %=\n", reverse(x));
  let y = reverse!(x);
  (y = #("boom", "bam", "bim"))
    | signal("reverse!(x) is not #(\"boom\", \"bam\", \"bim\")! It's %=\n", y);
  let numbers = #(3, 1, 4, 1, 5, 9);
  (sort(numbers) = #(1, 1, 3, 4, 5, 9))
    | signal("sort(numbers) is not #(1, 1, 3, 4, 5, 9)!  It's %=\n", sort(numbers));
  let y = sort!(numbers);
  (y = #(1, 1, 3, 4, 5, 9))
    | signal("sort!(numbers) is not #(1, 1, 3, 4, 5, 9)!  It's %=\n", y);
  let numbers = #(3, 1, 4, 1, 5, 9);
  (first(numbers) = 3)
    | signal("first(numbers) is not 3!  It's %=\n", first(numbers));
  (second(numbers) = 1)
    | signal("second(numbers) is not 1!  It's %=\n", second(numbers));
  (third(numbers) = 4)
    | signal("third(numbers) is not 4!  It's %=\n", third(numbers));
  (first-setter(1, numbers) = 1)
    | signal("first-setter(1, numbers) is not 1!  It's %=\n", first-setter(1, numbers));
  (second-setter(2, numbers) = 2)
    | signal("second-setter(2, numbers) is not 2!  It's %=\n", second-setter(2, numbers));
  (third-setter(3, numbers) = 3)
    | signal("third-setter(3, numbers) is not 3!  It's %=\n", third-setter(3, numbers));
  (last (#("emperor", "of", "china")) = "china")
    | signal("last (#(\"emperor\", \"of\", \"china\")) is not \"china\"!  It's %=\n",
	     last (#("emperor", "of", "china")));
  let my-list = list (1, 2, 3);
  (my-list = #(1, 2, 3))
    | signal("my-list is not #(1, 2, 3)!  It's %=\n", my-list);
  ((last (my-list) := 4) = 4)
    | signal("last(my-list) := 4 is not 4! It's %=\n", (last (my-list) := 4));
  (subsequence-position ("Ralph Waldo Emerson", "Waldo") = 6)
    | signal("subsequence-position (\"Ralph Waldo Emerson\", \"Waldo\") is not 6!  It's %=\n",
	     subsequence-position ("Ralph Waldo Emerson", "Waldo"));
  (#(1, 2, 3) = #[1, 2, 3])
    | signal("#(1, 2, 3) is not equal to #[1, 2, 3])!\n");
end method;

define method tautology(arg == #"lists")
/*  
  (pair(1, 2) = #(1 . 2))
    | signal("pair(1, 2) is not #(1 . 2)!  It's %=\n", pair(1, 2));
*/
  (pair(1, #(2, 3, 4, 5)) = #(1, 2, 3, 4, 5))
    | signal("pair(1, #(2, 3, 4, 5)) is not #(1, 2, 3, 4, 5)!  It's %=\n",
	     pair(1, #(2, 3, 4, 5)));
  (list(1, 2, 3) = #(1, 2, 3))
    | signal("list(1, 2, 3) is not #(1, 2, 3)!  It's %=\n", list (1, 2, 3));
  (list(4 + 3, 4 - 3) =  #(7, 1))
    | signal("list(4 + 3, 4 - 3) is not #(7, 1)!  It's %=\n", list (4 + 3, 4 - 3));
  (head(#(4, 5, 6)) = 4)
    | signal("head(#(4, 5, 6)) is not 4!  It's %=\n", head(#(4, 5, 6)));
  (head(#()) = #())
    | signal("head(#()) is not #()!  It's %=\n", head (#()));
  (tail(#(4, 5, 6)) = #(5, 6))
    | signal("tail (#(4, 5, 6)) is not #(5, 6)!  It's %=\n", tail (#(4, 5, 6)));
  let x = list (4, 5, 6);
  ((head(x) := 9) = 9)
    | signal("(head(x) := 9) is not 9!\n");
  (x = #(9, 5, 6))
    | signal("x is not #(9, 5, 6)!  It's %=\n", x);
  ((tail(x) := #(9, 8, 7)) = #(9, 8, 7))
    | signal("(tail(x) := #(9, 8, 7)) is not #(9, 8, 7)!\n");
  (x = #(9, 9, 8, 7))
    | signal("x is not #(9, 9, 8, 7)!  It's %=\n", x);
  let x = add!(x, 1);
  (x = #(1, 9, 9, 8, 7))
    | signal("x is not #(1, 9, 9, 8, 7)!  It's %=\n", x);
  let x = remove!(x, 9);
  (x = #(1, 8, 7))
    | signal("x is not #(1, 8, 7)!  It's %=\n", x);
  (size(x) = 3)
    | signal("size(x) is not 3!  It's %=\n", size(x));
end method;

define method tautology(arg == #"ranges")
  // HACK: to: -> below: in the following because minimal ranges don't
  // support the full monty.
  let a = make(<range>, from: 0, below: 11);
  let b = make(<range>, from: 5, below: 16);
  (first(a) = 0)	| signal("first(a) is not 0! It's %=\n", first(a));
  (first(b) = 5)	| signal("first(b) is not 5! It's %=\n", first(b));
  (last(a) = 10)	| signal("last(a) is not 10! It's %=\n", last(a));
  (last(b) = 15)	| signal("last(b) is not 15! It's %=\n", last(b));
  member?(3, a)		| signal("member?(3, a) is not true!\n");
  member?(12, a)	& signal("member?(12, a) is not false!\n");
  member?(3, b)		& signal("member?(3, b) is not false!\n");
  member?(12, b)	| signal("member?(12, b) is not true!\n");
  (size(a) = 11)	| signal("size(a) is not 11!  It's %=\n", size(a));
  (size(b) = 11)	| signal("size(b) is not 11!  It's %=\n", size(b));
  let c = intersection(a, b);
  (first(c) = 5)	| signal("first(c) is not 5!  It's %=\n", first(c));
  (last(c) = 10)	| signal("last(c) is not 10!  It's %=\n", last(c));
  member?(7, c)		| signal("member?(7, c) is not true!\n");
  member?(12, c)	& signal("member?(12, c) is not false!\n");
  (size(c) = 6)		| signal("size(c) is not 6!  It's %=\n", size(c));
  let d = reverse(c);
  (first(d) = 10)	| signal("first(d) is not 10!  It's %=\n", first(d));
  (last(d) = 5)		| signal("last(d) is not 5!  It's %=\n", last(d));
  let e = copy-sequence(d);
  (d = e)		| signal("d is not equal to e!\n");
  let f = reverse!(reverse!(d));
  (d = f)		| signal("d is not equal to f!\n");
end method;

define method tautology(arg == #"stretchy vectors")
  let a = make(<stretchy-vector>);
end method;

define method tautology(arg == #"strings")
  let a = make(<byte-string>);
end method;

define method tautology(arg == #"vectors")
  let a = make(<vector>);
end method;


define open generic tc1-islot1 (x);
define open generic tc1-islot1-setter (x, y);
define open generic tc1-islot2 (x);
define open generic tc1-islot2-setter (x, y);
define open generic tc1-islot3 (x);
define open generic tc1-islot3-setter (x, y);
define open generic tc1-cslot4 (x);
define open generic tc1-cslot4-setter (x, y);
define open generic tc1-cslot5 (x);
define open generic tc1-cslot5-setter (x, y);
define open generic tc1-eslot6 (x);
define open generic tc1-eslot6-setter (x, y);
define open generic tc2-islot1 (x);
define open generic tc2-islot1-setter (x, y);
define open generic tc2-islot2 (x);
define open generic tc2-islot2-setter (x, y);
define open generic tc2-eslot3 (x);
define open generic tc2-eslot3-setter (x, y);
define open generic tc2-cslot4 (x);
define open generic tc2-cslot4-setter (x, y);
define open generic tc3-islot1 (x);
define open generic tc3-islot1-setter (x, y);
define open generic tc3-eslot2 (x);
define open generic tc3-eslot2-setter (x, y);
define open generic tc3-cslot3 (x);
define open generic tc3-cslot3-setter (x, y);


define open class <test-class-1> (<object>)
  open slot tc1-islot1, init-value: #"tc1-islot1";
  open slot tc1-islot2, init-value: #"tc1-islot2";
  open slot tc1-islot3, init-value: #"tc1-islot3";
  open class slot tc1-cslot4, init-value: #"tc1-cslot4";
  open class slot tc1-cslot5, init-value: #"tc1-cslot5";
  open each-subclass slot tc1-eslot6, init-value: #"tc1-eslot6";
end class;

define open class <test-class-2> (<object>)
  open slot tc2-islot1, init-value: #"tc2-islot1";
  open slot tc2-islot2, init-value: #"tc2-islot2";
  open each-subclass slot tc2-eslot3, init-value: #"tc2-eslot3";
  open class slot tc2-cslot4, init-value: #"tc2-cslot4";
end class;

define open class <test-class-3> (<test-class-1>, <test-class-2>)
  open slot tc3-islot1, init-value: #"tc3-islot1";
  open each-subclass slot tc3-eslot2, init-value: #"tc3-eslot2";
  open class slot tc3-cslot3, init-value: #"tc3-cslot3";
end class;


define method tautology (arg == #"slots")
  let tc1-1 = make(<test-class-1>);
  let tc1-2 = make(<test-class-1>);
  let tc2-1 = make(<test-class-2>);
  let tc2-2 = make(<test-class-2>);
  let tc3-1 = make(<test-class-3>);
  let tc3-2 = make(<test-class-3>);
  let slottest = method (val, str, sym)
		   if (val ~== sym)
		     format-out("%= of %= was %=\n", sym, str, val)
		   end if
		 end method;
  /* Fuck modules.
  for (i from 0 below 6)
    format-out("Class-slot-element %= of tc3-1 is %=\n", i, class-slot-element(tc3-1, i))
  end for;
    */

  slottest(tc1-islot1(tc1-1), "initial tc1-islot1(tc1-1)", #"tc1-islot1");
  slottest(tc1-islot2(tc1-1), "initial tc1-islot2(tc1-1)", #"tc1-islot2");
  slottest(tc1-islot3(tc1-1), "initial tc1-islot3(tc1-1)", #"tc1-islot3");

  slottest(tc2-islot1(tc2-1), "initial tc2-islot1(tc2-1)", #"tc2-islot1");
  slottest(tc2-islot2(tc2-1), "initial tc2-islot2(tc2-1)", #"tc2-islot2");

  slottest(tc1-islot1(tc3-1), "initial tc1-islot1(tc3-1)", #"tc1-islot1");
  slottest(tc1-islot2(tc3-1), "initial tc1-islot2(tc3-1)", #"tc1-islot2");
  slottest(tc1-islot3(tc3-1), "initial tc1-islot3(tc3-1)", #"tc1-islot3");
  slottest(tc2-islot1(tc3-1), "initial tc2-islot1(tc3-1)", #"tc2-islot1");
  slottest(tc2-islot2(tc3-1), "initial tc2-islot2(tc3-1)", #"tc2-islot2");
  slottest(tc3-islot1(tc3-1), "initial tc3-islot1(tc3-1)", #"tc3-islot1");

  slottest(tc1-islot1(tc1-2), "initial tc1-islot1(tc1-2)", #"tc1-islot1");
  slottest(tc1-islot2(tc1-2), "initial tc1-islot2(tc1-2)", #"tc1-islot2");
  slottest(tc1-islot3(tc1-2), "initial tc1-islot3(tc1-2)", #"tc1-islot3");

  slottest(tc2-islot1(tc2-2), "initial tc2-islot1(tc2-2)", #"tc2-islot1");
  slottest(tc2-islot2(tc2-2), "initial tc2-islot2(tc2-2)", #"tc2-islot2");

  slottest(tc1-islot1(tc3-2), "initial tc1-islot1(tc3-2)", #"tc1-islot1");
  slottest(tc1-islot2(tc3-2), "initial tc1-islot2(tc3-2)", #"tc1-islot2");
  slottest(tc1-islot3(tc3-2), "initial tc1-islot3(tc3-2)", #"tc1-islot3");
  slottest(tc2-islot1(tc3-2), "initial tc2-islot1(tc3-2)", #"tc2-islot1");
  slottest(tc2-islot2(tc3-2), "initial tc2-islot2(tc3-2)", #"tc2-islot2");
  slottest(tc3-islot1(tc3-2), "initial tc3-islot1(tc3-2)", #"tc3-islot1");

///

  slottest(tc1-cslot4(tc1-1), "initial tc1-cslot4(tc1-1)", #"tc1-cslot4");
  slottest(tc1-cslot5(tc1-1), "initial tc1-cslot5(tc1-1)", #"tc1-cslot5");
  slottest(tc1-eslot6(tc1-1), "initial tc1-eslot6(tc1-1)", #"tc1-eslot6");

  slottest(tc2-eslot3(tc2-1), "initial tc2-eslot3(tc2-1)", #"tc2-eslot3");
  slottest(tc2-cslot4(tc2-1), "initial tc2-cslot4(tc2-1)", #"tc2-cslot4");

  slottest(tc1-cslot4(tc3-1), "initial tc1-cslot4(tc3-1)", #"tc1-cslot4");
  slottest(tc1-cslot5(tc3-1), "initial tc1-cslot5(tc3-1)", #"tc1-cslot5");
  slottest(tc1-eslot6(tc3-1), "initial tc1-eslot6(tc3-1)", #"tc1-eslot6");
  slottest(tc2-eslot3(tc3-1), "initial tc2-eslot3(tc3-1)", #"tc2-eslot3");
  slottest(tc2-cslot4(tc3-1), "initial tc2-cslot4(tc3-1)", #"tc2-cslot4");
  slottest(tc3-eslot2(tc3-1), "initial tc3-eslot2(tc3-1)", #"tc3-eslot2");
  slottest(tc3-cslot3(tc3-1), "initial tc3-cslot3(tc3-1)", #"tc3-cslot3");

  slottest(tc1-cslot4(tc1-2), "initial tc1-cslot4(tc1-2)", #"tc1-cslot4");
  slottest(tc1-cslot5(tc1-2), "initial tc1-cslot5(tc1-2)", #"tc1-cslot5");
  slottest(tc1-eslot6(tc1-2), "initial tc1-eslot6(tc1-2)", #"tc1-eslot6");

  slottest(tc2-eslot3(tc2-2), "initial tc2-eslot3(tc2-2)", #"tc2-eslot3");
  slottest(tc2-cslot4(tc2-2), "initial tc2-cslot4(tc2-2)", #"tc2-cslot4");

  slottest(tc1-cslot4(tc3-2), "initial tc1-cslot4(tc3-2)", #"tc1-cslot4");
  slottest(tc1-cslot5(tc3-2), "initial tc1-cslot5(tc3-2)", #"tc1-cslot5");
  slottest(tc1-eslot6(tc3-2), "initial tc1-eslot6(tc3-2)", #"tc1-eslot6");
  slottest(tc2-eslot3(tc3-2), "initial tc2-eslot3(tc3-2)", #"tc2-eslot3");
  slottest(tc2-cslot4(tc3-2), "initial tc2-cslot4(tc3-2)", #"tc2-cslot4");
  slottest(tc3-eslot2(tc3-2), "initial tc3-eslot2(tc3-2)", #"tc3-eslot2");
  slottest(tc3-cslot3(tc3-2), "initial tc3-cslot3(tc3-2)", #"tc3-cslot3");

  // *** Propagation of class slot values (tc1-4, tc1-5, tc2-4, tc3-3).
  tc1-cslot4(tc3-1) := #"tc1-cslot4a";
  slottest(tc1-cslot4(tc3-1), "second tc1-cslot4(tc3-1)", #"tc1-cslot4a");
  slottest(tc1-cslot4(tc3-2), "second tc1-cslot4(tc3-2)", #"tc1-cslot4a");
  slottest(tc1-cslot4(tc1-1), "second tc1-cslot4(tc1-1)", #"tc1-cslot4a");
  slottest(tc1-cslot4(tc1-2), "second tc1-cslot4(tc1-2)", #"tc1-cslot4a");

  tc2-cslot4(tc2-2) := #"tc2-cslot4a";
  slottest(tc2-cslot4(tc2-1), "third tc2-cslot4(tc2-1)", #"tc2-cslot4a");
  slottest(tc2-cslot4(tc2-2), "third tc2-cslot4(tc2-2)", #"tc2-cslot4a");
  slottest(tc2-cslot4(tc3-1), "third tc2-cslot4(tc3-1)", #"tc2-cslot4a");
  slottest(tc2-cslot4(tc3-2), "third tc2-cslot4(tc3-2)", #"tc2-cslot4a");

  // . . .

  // *** Non-propagation of each-subclass slot values (tc1-6 tc2-3 tc3-2)
  tc1-eslot6(tc1-1) := #"tc1-eslot6a";
  slottest(tc1-eslot6(tc1-1), "fourth tc1-eslot6(tc1-1)", #"tc1-eslot6a");
  slottest(tc1-eslot6(tc1-2), "fourth tc1-eslot6(tc1-2)", #"tc1-eslot6a");
  slottest(tc1-eslot6(tc3-1), "fourth tc1-eslot6(tc3-1)", #"tc1-eslot6");
  slottest(tc1-eslot6(tc3-2), "fourth tc1-eslot6(tc3-2)", #"tc1-eslot6");

  tc2-eslot3(tc3-1) := #"tc2-eslot3a";
  slottest(tc2-eslot3(tc2-1), "fifth tc2-eslot3(tc2-1)", #"tc2-eslot3");
  slottest(tc2-eslot3(tc2-1), "fifth tc2-eslot3(tc2-1)", #"tc2-eslot3");
  slottest(tc2-eslot3(tc3-1), "fifth tc2-eslot3(tc3-1)", #"tc2-eslot3a");
  slottest(tc2-eslot3(tc3-1), "fifth tc2-eslot3(tc3-1)", #"tc2-eslot3a");
  
end method;


define open generic foo-slot-1 (x);
define open generic foo-slot-1-setter (x, y);
define open generic foo-slot-2 (x);
define open generic foo-slot-2-setter (x, y);
define open generic bar-slot-1 (x);
define open generic bar-slot-1-setter (x, y);
define open generic bar-slot-2 (x);
define open generic bar-slot-2-setter (x, y);
define open generic baz-slot-1 (x);
define open generic baz-slot-1-setter (x, y);
define open generic baz-slot-2 (x);
define open generic baz-slot-2-setter (x, y);
define open generic qwerty-slot-1 (x);
define open generic qwerty-slot-1-setter (x, y);
define open generic qwerty-slot-2 (x);
define open generic qwerty-slot-2-setter (x, y);
define open generic glorp-slot-1 (x);
define open generic glorp-slot-1-setter (x, y);
define open generic glorp-slot-2 (x);
define open generic glorp-slot-2-setter (x, y);

define class <foo> (<object>)
  slot foo-slot-1, init-value: #"foo-slot-1";
  slot foo-slot-2, init-value: #"foo-slot-2";
end class;

define class <bar> (<foo>)
  slot bar-slot-1, init-value: #"bar-slot-1";
  slot bar-slot-2, init-value: #"bar-slot-2";
end class;

define class <baz> (<bar>)
  slot baz-slot-1, init-value: #"baz-slot-1";
  slot baz-slot-2, init-value: #"baz-slot-2";
end class;

define class <quux> (<bar>)
  slot quux-slot-1, init-value: #"quux-slot-1";
  slot quux-slot-2, init-value: #"quux-slot-2";
end class;

define class <qwerty> (<object>)
  slot qwerty-slot-1, init-value: #"qwerty-slot-1";
  slot qwerty-slot-2, init-value: #"qwerty-slot-2";
end class;

define class <glorp> (<qwerty>, <baz>)
  slot glorp-slot-1, init-value: #"glorp-slot-1";
  slot glorp-slot-2, init-value: #"glorp-slot-2";
end class;


define method keytest1 (x :: <foo>, #key foo)
  x; list(foo)
end method;

define method keytest1 (x :: <bar>, #key bar)
  x; pair(bar, next-method())
end method;

define method keytest1 (x :: <baz>, #key baz)
  x; pair(baz, next-method())
end method;

define method keytest1 (x :: <quux>, #key quux)
  x; pair(quux, next-method())
end method;


define method keytest2  (x :: <foo>, #key foo)
  x; foo
end method;

define method keytest2 (x :: <bar>, #key bar)
  x; bar
end method;

define method keytest2 (x :: <baz>, #key baz)
  x; baz
end method;

define method keytest2 (x :: <quux>, #key quux)
  x; quux
end method;

define constant check = method (name, testfn, thunk, value)
  let nval = thunk();
  if (~testfn(nval, value))
    signal("Wrong value for test %s: got %= expected %=", name, nval, value)
  end if
end method;

define constant gcall = method (fn :: <generic-function>, #rest args)
  apply(fn, args)
end method;

define method tautology (test == #"dispatch-and-keywords-1")
  // check("reset", \==, method() reset(keytest1) end, keytest1);
  check("foo", \=, method() gcall(keytest1, make(<foo>), foo: 17) end, 
	#(17));
  check("bar 1", \=, method() gcall(keytest1, make(<bar>), foo: 17, bar: 259) end, 
	#(259, 17));
  check("bar 2", \=, method () gcall(keytest1, make(<bar>), foo: 17) end,
	#(#f, 17));
  check("bar 3", \=, method () gcall(keytest1, make(<bar>)) end,
	#(#f, #f));
  check("bar 4", \=, method () gcall(keytest1, make(<bar>), bar: 259) end,
	#(259, #f));
  check("baz", \=, method () gcall(keytest1, make(<baz>), foo: 17, bar: 259, baz: 11) end,
	#(11, 259, 17));
  check("quux", \=, method () gcall(keytest1, make(<quux>), foo: 17, bar: 259, quux: #"fifi") end,
	#(#"fifi", 259, 17));
  // check-condition("no quux", <error>, gcall(keytest1, make(<bar>), quux: #"fifi"));
  // check-condition("no bar", <error>, gcall(keytest1, make(<foo>), bar: 259));
  // check-condition("no baz", <error>, gcall(keytest1, make(<quux>), baz: 11));
  // check-true("", show(keytest1));
end method;


define method tautology (test == #"dispatch-and-keywords-2")
  // check("reset", \==, method () reset(keytest2) end, keytest2);
  check("foo", \==, method () gcall(keytest2, make(<foo>), foo: 17) end,
	17);
  check("bar", \==, method () gcall(keytest2, make(<bar>), foo: 17, bar: 259) end,
	259);
  check("baz", \==, method () gcall(keytest2, make(<baz>), foo: #"ofo", bar: 259, baz: 11) end,
	11);
  check("quux", \==, method () gcall(keytest2, make(<quux>), foo: 17, bar: 259, quux: #"fifi") end,
	#"fifi");
  // check-condition("no bar 1", <error>, gcall(keytest2, make(<foo>), bar: 259));
  // check-condition("no bar 2", <error>, gcall(keytest2, make(<foo>), foo: 17, bar: 259));
  // check-condition("no baz 1", <error>, gcall(keytest2, make(<bar>), foo: 17, baz: 11));
  // check-condition("no baz 2", <error>, gcall(keytest2, make(<foo>), foo: 17, baz: 11));
  // check-condition("no baz 3", <error>, gcall(keytest2, make(<foo>), baz: 11, foo: 17));
  // check-true("", show(keytest2));
end method;


define method simple-typeunion-method (x)
  x;
  #"object"
end method;

define method simple-typeunion-method (x :: type-union(singleton(#f), <integer>))
  x;
  #"integer-union"
end method;

define method simple-typeunion-method (x :: <symbol>)
  x;
  #"symbol"
end method;


define constant analyze-typeunion = method (x)
  let mlist = generic-function-methods(simple-typeunion-method);
  let m = element(mlist, find-key(mlist, method(m) ~instance?(element(function-specializers(m), 0), <class>) end));
  m(x)
end method;

// define constant analyze-typeunion-2 = method (x)
//   let mlist = generic-function-methods(simple-typeunion-method);
//   let m = element(mlist, find-key(mlist, method(m) ~instance?(element(function-specializers(m), 0), <class>) end));
//   let v = make(<simple-object-vector>, size: 1);
//   v[0] := x;
//   %method-apply-with-optionals(m, #(), v)
// end method;


define method tautology (test == #"simple-typeunion")
  check("typeunion method call 1", \==, method() analyze-typeunion(#f) end, #"integer-union");
  check("typeunion method call 2", \==, method() analyze-typeunion(259) end, #"integer-union");
//  check("typeunion %method call 1", \==, method() analyze-typeunion-2(#f) end, #"integer-union");
//  check("typeunion %method call 2", \==, method() analyze-typeunion-2(259) end, #"integer-union");
  
  check("get union on integer", \==, method() gcall(simple-typeunion-method, 3) end, #"integer-union");
  check("get union on #f", \==, method() gcall(simple-typeunion-method, #f) end, #"integer-union");
  check("get symbol on foo", \==, method() gcall(simple-typeunion-method, #"foo") end, #"symbol");
  check("get object on #t", \==, method() gcall(simple-typeunion-method, #t) end, #"object");
  check("get object on #t v2", \==, method() gcall(simple-typeunion-method, #t) end, #"object");
  check("get symbol foo v2", \==, method() gcall(simple-typeunion-method, #"foo") end, #"symbol");
  check("get union on #f v2", \==, method() gcall(simple-typeunion-method, #f) end, #"integer-union");
  check("get union on integer v2", \==, method() gcall(simple-typeunion-method, 3) end, #"integer-union");
end method;
