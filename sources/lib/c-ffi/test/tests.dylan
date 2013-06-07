Module:    c-ffi-test
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define c-struct <really-minimal>
end;

define c-struct <minimal>
  slot slot-1 :: <C-unsigned-long>;
  slot slot-2 :: <C-unsigned-long*>;
  pointer-type-name: <minimal*>, c-name: "subliminal";
end;

define c-pointer-type <Pminimal> => <minimal>;

define c-struct <maximal>
  slot max-1 :: <minimal>, address-getter: max-1*
  ;
  slot max-2 :: <C-unsigned-char>, address-getter: max-2*;
  array slot max-3 :: <minimal>, length: 7, address-getter: max-3*
  ;
//  bitfield slot max-4 :: <c-unsigned-int>, width: 4;
//  bitfield slot max-5 :: <c-unsigned-int>, width: 12;
  slot max-6 :: <C-unsigned-char>, address-getter: max-6*, setter: #f;
  slot max-7 :: <C-unsigned-char*>, address-getter: max-7*;
  pointer-type-name: <maximal*>;
end;

define c-function simplest-foreign-function
  c-name: "simplest_foreign_function";
end;

define c-function ffi-no-result
  parameter p1 :: <C-unsigned-short>;
  c-name: "ffi_no_result";
end;

define c-function ffi-no-parameters
  result r :: <C-unsigned-short>;
  c-name: "ffi_no_parameters";
end;

define c-function increment-unsigned-long
  parameter x :: <c-unsigned-long>;
  result r :: <c-unsigned-long>;
  c-name: "incr_unsigned_long";
end;

define c-function increment-signed-long
  parameter x :: <c-signed-long>;
  result r :: <c-signed-long>;
  c-name: "incr_signed_long";
end;

define c-function increment-unsigned-int
  parameter x :: <c-unsigned-int>;
  result r :: <c-unsigned-int>;
  c-name: "incr_unsigned_int";
end;

define c-function increment-signed-int
  parameter x :: <c-signed-int>;
  result r :: <c-signed-int>;
  c-name: "incr_signed_int";
end;

define c-function increment-unsigned-short
  parameter x :: <c-unsigned-short>;
  result r :: <c-unsigned-short>;
  c-name: "incr_unsigned_short";
end;

define c-function increment-signed-short
  parameter x :: <c-signed-short>;
  result r :: <c-signed-short>;
  c-name: "incr_signed_short";
end;

define c-function increment-unsigned-char
  parameter x :: <c-unsigned-char>;
  result r :: <c-unsigned-char>;
  c-name: "incr_unsigned_char";
end;

define c-function increment-signed-char
  parameter x :: <c-signed-char>;
  result r :: <c-signed-char>;
  c-name: "incr_signed_char";
end;

/*

define c-function increment-char
  parameter x :: <c-char>;
  result r :: <c-char>;
  c-name: "incr_char";
end;
*/


/*
/// used??
define c-function ffi1
  input parameter p1 :: <C-unsigned-long>;
  parameter p2 :: <C-unsigned-char>;
  output parameter p3 :: <c-unsigned-char*>;
  input output parameter p4 :: <C-unsigned-char*>;
  input output parameter p5 :: <minimal*>; // !@#$ is this meaningful???
  output parameter p6 :: <minimal*>;       // !@#$ how about this?
  result r :: <C-unsigned-char>;
  c-name: "ffi1";
end;
*/

/// called from the C tests
define c-callable-wrapper simple-callable
    of method () => ()
	 values() end
  c-name: "call_me";
end;

define sealed method a-function () => ();
  format-out("From dylan: calling call_me_too\n");
  values();
end;

/// called from the C tests
define c-callable-wrapper simple-callable2
    of a-function
  c-name: "call_me_too";
end;



/// called from C
/// Be very careful to get correspondence right with C code.
define c-callable-wrapper more-interesting-callable of
    method (p1 :: <integer>, p2 :: <integer>, p4 :: <integer>,
	    p5 :: <integer>, p6 ::  <minimal*> ) => (r, p3, p4, p5)
      check-equal("slot value of struct passed into dylan", p6.slot-1, 171717);
      check-equal("parameter 1 passed into interesting callable ", p1, 1);
      check-equal("parameter 2 passed into interesting callable", p2, 2);
      check-equal("parameter 4 passed into interesting callable", p4, 4);
      check-equal("parameter 5 passed into interesting callable", p5, 5);
      p6.slot-1 := #xfffff00;
      values(p4, p1, p2, p4)
    end
  input parameter p1 :: <C-unsigned-long>;
  parameter p2 :: <C-unsigned-char>;
  output parameter p3 :: <c-unsigned-char*>;
  input output parameter p4 :: <C-unsigned-char*>;
  input output parameter p5 :: <C-unsigned-char*>;
  parameter p6 :: <minimal*>;
  result r :: <C-unsigned-char>;
  c-name: "call_me_more";
end;

define c-callable-wrapper another-interesting-callable of
    method (p1 :: <integer>, p2 :: <integer>, p3 :: <c-unsigned-char*>,
	    p4 :: <C-unsigned-char*>, p5 ::  <minimal*>,
	    p6 :: <minimal*>)
     => (r :: <integer>)
      values(p1)
    end
  input parameter p1 :: <C-unsigned-long>;
  parameter p2 :: <C-unsigned-char>;
  input parameter p3 :: <c-unsigned-char*>;
  input parameter p4 :: <C-unsigned-char*>;
  input parameter p5 :: <minimal*>;
  parameter p6 :: <minimal*>;
  result r :: <C-unsigned-char>;
  c-name: "call_me_more2";
end;





// Do some define c-pointer-type's
// one with a pointer-type
define c-pointer-type <signed-intP> => <C-signed-int>;


// probably doesn't have a pointer type.
define c-pointer-type <signed-intP*> => <signed-intP>;

// definitely doesn't have a pointer type.
define c-pointer-type <signed-intP**> => <signed-intP*>;

define c-mapped-subtype <my-thing> (<C-signed-long>)
  map <integer>,
    import-function: method (x) x * 2 end,
    export-function: method (x) truncate/(x, 2) end;
  pointer-type <my-thing*>;
end;


/// A couple of variables for the c-variable-test
define c-address $foos-address :: <c-unsigned-long*>
 c-name: "foo";
end;

define c-variable foo-value :: <c-unsigned-long>
 c-name: "foo";
end;


/// try some things with a variable defined via define c-variable
define test variable-address-test (description: "Variable Address Tests")
  check-equal("set c-address",
	       (pointer-value($foos-address) := 1), 1);
  check-equal("c-address value", pointer-value($foos-address), 1);
  check-equal("c-address value from c-variable", foo-value(), 1);
  check-equal("set c-variable: %s should be %s",
	       (foo-value() := 777), 777);
  check-equal("c-address value 777", pointer-value($foos-address), 777);
  check-equal("c-variable value 777", foo-value(), 777);
  // !@#$ need more tests that check for agreement with C
end;

/// try some C-struct in different ways
/// Could always use some more tests here.
define test c-struct-test (description: "C-Struct Tests")
  format-out("minimal-size: %s", size-of(<minimal>));
  check-equal("<minimal> struct size",
	      struct-minimal-size(), size-of(<minimal>));
  format-out("maximal-size: %s", size-of(<maximal>));
  check-equal("<maximal> struct size",
	      struct-maximal-size(), size-of(<maximal>));
  let minimal-struct = make(<minimal*>);
  check-equal("set simple struct-slot", (minimal-struct.slot-1 := 777), 777);
  check-equal("simple struct slot value", minimal-struct.slot-1, 777);
  check-equal( "set struct simple address valued slot",
	      minimal-struct.slot-2 := $foos-address,
	      $foos-address);
  check-equal("struct simple address valued slot value",
	      pointer-address($foos-address),
	      pointer-address(minimal-struct.slot-2));
  check-equal("stuct set pointer value address valued slot",
	  pointer-value(minimal-struct.slot-2) := 12345,
	  12345);
  check-equal("struct pointer value check alias",
	      foo-value(), pointer-value(minimal-struct.slot-2));
  check-equal("struct pointer value check value",
	      pointer-value(minimal-struct.slot-2), 12345);
  check-equal("define c-pointer-type does correct aliasing",
	      <minimal*>, <Pminimal>);
  let maximal-struct = make(<maximal*>);
  // do some tests here that deal with maximal
  // !@#$ Need more tests to see if accessors get the same thing as C code 
  //      for corresponding C structs
end;


/// Try some C functions in some different ways.
/// this could always use more tests.
define test c-function-test (description: "C Function Tests")
  format-out("Running c-function tests\n");
  for (i from 0 below (ash(1, 8 * size-of(<C-unsigned-char>)) - 2) by 17)
    check-equal("c-function unsigned char increment",
		increment-unsigned-char(i), i + 1);
  end;

  for(i from 0 below (ash(1, (8 * size-of(<C-signed-char>)) - 1) - 2) by 17)
    check-equal("c-function signed char increment",
		increment-signed-char(i), i + 1);
    check-equal("c-function signed char negative increment",
		increment-signed-char(- i), (- i) + 1);
  end;
  for(i from 0 below (ash(1, 8 * size-of(<C-unsigned-char>)) - 2) by 17)
    check-equal("c-function unsigned short increment",
		increment-unsigned-short(i), i + 1);
  end;
  for(i from 0 below (ash(1, (8 * size-of(<C-signed-char>)) - 1) - 2) by 17)
    check-equal("c-function signed short increment",
		increment-signed-short(i), i + 1);
    check-equal("c-function signed short negative increment",
		increment-signed-short(- i), (- i) + 1);
  end;
  // !@#$ do more here
end;  

/// ---------
/// C string tests

/// This just calls the c-callable function below
define c-function c-string-value
  parameter str :: <C-string>;
  parameter offset :: <C-unsigned-int>;
  result r :: <c-character>;
  c-name: "c_string_value";
end;

/// Treat X as a C-string and return the OFFSETth element
define c-callable-wrapper $c-string-value of
    method (x :: <C-string>, offset :: <integer>) => (r :: <Character>);
      pointer-value(x, index: offset);
    end
  parameter x :: <C-string>;
  parameter offset :: <C-unsigned-int>;
  result r :: <C-character>;
  c-name: "c_string_value";
end;

/// This just calls the c-callable function below
define c-function c-string-value-setter
  parameter new :: <C-character>;
  parameter str :: <C-string>;
  parameter offset :: <C-unsigned-int>;
  result r :: <c-character>;
  c-name: "c_string_value_setter";
end;

/// Treat X as a C-string and set the OFFSETth element to NEW
define c-callable-wrapper $c-string-value-setter of
    method (new :: <character>,
	    x :: <C-string>,
	    offset :: <integer>) => (r :: <character>);
      pointer-value(x, index: offset) := new;
    end
  parameter new :: <c-character>;
  parameter x :: <C-string>;
  parameter offset :: <C-unsigned-int>;
  result r :: <C-character>;
  c-name: "c_string_value_setter";
end;


/// check-a-c-string
/// This set of tests modifies the string and accesses it through a
/// foreign function, ans via element to make sure that every view of it
/// is correct.
define function check-a-c-string (name-string, a-string, seed) => ();
  for(i from 0 below size(a-string))
    let this-char = as(<byte-character>, as(<integer>, seed) + i);
    check-equal(concatenate(name-string,
			    " c-string get value from alias before"),
		c-string-value(a-string, i),
		a-string[i]);
    check-equal(concatenate(name-string,
			    " c-string set value"),
		c-string-value(a-string, i) := this-char,
		this-char);
    check-equal(concatenate(name-string,
			    " c-string set value"),
		c-string-value(a-string, i),
		this-char);
    check-equal(concatenate(name-string,
			    " c-string get value alias"),
		a-string[i],
		this-char);
    check-equal(concatenate(name-string,
			    " c-string get value from alias"),
		c-string-value(a-string, i),
		a-string[i]);
  end;
end;



/// c-string-test
/// make sure that <C-string>s behave as strings and collections, and that
/// views onto a <byte-string> as a <c-string> from a foreign function 
/// all correspond.
define test c-string-test () 
  format-out("Running c-string tests\n");
  let a-string = shallow-copy("This is a test string");
  check-a-c-string("<byte-string>", a-string, 'A');
  let c-string = #f;
  check-true("Create a C-string with map-as",
	     instance?(c-string := map-as(<c-string>, identity, a-string),
		       <C-string>));
  check-equal("size works on c-strings", size(a-string), size(c-string));
  for(item in a-string,
      index from 0 below size(a-string))
    check-equal("elements of c-string", item, c-string[index]);
  end;
  for(item in c-string,
      index from 0 below size(c-string))
    check-equal("elements of c-string forward-iteration",
		item, a-string[index]);
  end;
  check-a-c-string("<C-string>", c-string, 'B');
end;


/// macros and tests to just check that all the functions and classes
/// defined by the c-ffi exist.
define macro check-class
  { check-class(?base:name) }
    => { check-true(concatenate("the class <C-", ?"base", "> exists"),
				instance?("<c-" ## ?base ## ">", <class>)) }
end macro;

define macro check-classes
  { check-classes ?base-names:* end }
    => { ?base-names }
base-names:
    { } => { }
    { ?base:name, ... } => { check-class(?base); ... }
end;

define macro check-function
  { check-function(?base:name) }
    => { check-true(concatenate("the function ", ?"base", " exists"),
		      instance?(?base, <function>)) }
end macro;

define macro check-functions
  { check-functions ?base-names:* end }
    => { ?base-names }
base-names:
    { } => { }
    { ?base:name, ... } => { check-function(?base); ... }
end;




define test c-types-test (description: "C Types Tests")
  format-out("Running c-types tests\n");
  /// check that all of these things are classes
  /// All of the names below are wrapped in '<c-` and '>`
  check-classes
    value,
    void,
    int,
    unsigned-int,
    signed-int,
    raw-int,
    raw-unsigned-int,
    raw-signed-int,
    unsafe-int,
    unsafe-unsigned-int,
    unsafe-signed-int,
    both-int,
    both-unsigned-int,
    both-signed-int,
    long,
    unsigned-long,
    signed-long,
    raw-long,
    raw-unsigned-long,
    raw-signed-long,
    unsafe-long,
    unsafe-unsigned-long,
    unsafe-signed-long,
    both-long,
    both-unsigned-long,
    both-signed-long,
    short,
    unsigned-short,
    signed-short,
    raw-short,
    raw-unsigned-short,
    raw-signed-short,
    unsafe-short,
    unsafe-unsigned-short,
    unsafe-signed-short,
    both-short,
    both-unsigned-short,
    both-signed-short,
    char,
    unsigned-char,
    signed-char,
    raw-char,
    raw-unsigned-char,
    raw-signed-char,
    unsafe-char,
    unsafe-unsigned-char,
    unsafe-signed-char,
    both-char,
    both-unsigned-char,
    both-signed-char,
    number,
    float,
    double,
    // long-double,
    pointer,
    void*,
    statically-typed-pointer,
    untyped-pointer,
    struct,
    union,
    function-pointer,
    dylan-object
  end;
  /// check that all of these things are functions
  check-functions
    size-of,
    alignment-of,
    pointer-address,
    null-pointer,
    null-pointer?,
    referenced-type,
    pointer-value,
    pointer-value-setter,
    C-unsigned-char-at,
    C-signed-char-at,
    C-char-at,
    C-unsigned-char-at-setter,
    C-signed-char-at-setter,
    C-char-at-setter,
    C-unsigned-short-at,
    C-signed-short-at,
    C-short-at,
    C-unsigned-short-at-setter,
    C-signed-short-at-setter,
    C-short-at-setter,
    C-unsigned-long-at,
    C-signed-long-at,
    C-long-at,
    C-unsigned-long-at-setter,
    C-signed-long-at-setter,
    C-long-at-setter,
    C-unsigned-int-at,
    C-signed-int-at,
    C-int-at,
    C-unsigned-int-at-setter,
    C-signed-int-at-setter,
    C-int-at-setter
  end;
end;

/// ask C what it thinks the size of the minimal struct is.
define c-function struct-minimal-size
  result r :: <C-signed-int>;
  c-name: "struct_minimal_size";
end;

/// ask C what it thinks the size of the maximal struct is.
define c-function struct-maximal-size
  result r :: <C-signed-int>;
  c-name: "struct_maximal_size";
end;

//-------
// for bug 217
define constant <HENV> = <C-void*>;
define c-pointer-type <HENV*> => <henv>;

define c-function SQLAllocEnv
    output parameter one :: <HENV*>;
    result value :: <C-signed-int>;
    c-name: "SQLAllocEnv";
end c-function;

define test bug-217 ()
  format-out("Running bug-217 tests\n");
  let (success :: <integer>, henv :: <HENV>) = SQLAllocEnv();
  check-equal("bug 217 got correct success code", success, 37);
  check-true("bug 217 henv should be a <HENV>", instance?(henv, <HENV>));
end;


// 
// -------------
// tests for bug 321

define c-union <union-test>
  slot union-1 :: <c-unsigned-short>;
  slot union-2 :: <C-signed-short>;
  slot union-3 :: <C-unsigned-char>;
  slot union-4 :: <C-signed-char>;
end;

define c-struct <union-test-2>
  slot struct-pad-1 :: <C-char>, getter: #f, setter: #f;
  slot union-struct :: <union-test>;
  slot struct-pad-2 :: <C-char>, getter: #f, setter: #f;
  pointer-type-name: <union-test-2*>;
end;

define c-function union-tester-1
  parameter arg :: <union-test-2*>;
  result r :: <C-unsigned-short>;
  c-name: "union_tester_1";
end;

define c-function union-tester-1-setter
  parameter new :: <C-unsigned-short>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_1_setter"
end;

define c-function union-tester-2
  parameter arg :: <union-test-2*>;
  result r :: <C-signed-short>;
  c-name: "union_tester_2";
end;

define c-function union-tester-2-setter
  parameter new :: <C-signed-short>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_2_setter"
end;


define c-function union-tester-3
  parameter arg :: <union-test-2*>;
  result r :: <C-unsigned-char>;
  c-name: "union_tester_3";
end;

define c-function union-tester-3-setter
  parameter new :: <C-unsigned-char>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_3_setter"
end;



define c-function union-tester-4
  parameter arg :: <union-test-2*>;
  result r :: <C-signed-char>;
  c-name: "union_tester_4";
end;

define c-function union-tester-4-setter
  parameter new :: <C-signed-char>;
  parameter arg :: <union-test-2*>;
  c-name: "union_tester_4_setter"
end;


define test bug-321 ()
  let struct = make(<union-test-2*>);
  union-struct(struct).union-1 := #xffff;
  check-equal("bug 321 accessor value 1, #xffff",
              union-struct(struct).union-1,
              #xffff);
  check-equal("bug 321 c-funf value 1, #xffff",
              union-tester-1(struct),
              #xffff);
  union-tester-1(struct) := #xffff;
  check-equal("bug 321 accessor second value 1, #xffff",
              union-struct(struct).union-1,
              #xffff);
  check-equal("bug 321 c-func second value 1, #xffff",
              union-tester-1(struct),
              #xffff);
  union-struct(struct).union-1 := 0;
  check-equal("bug 321 accessor value 1, 0",
              union-struct(struct).union-1,
              0);
  check-equal("bug 321 c-funf value 1, 0",
              union-tester-1(struct),
              0);
  union-tester-1(struct) := 0;
  check-equal("bug 321 accessor second value 1, 0",
              union-struct(struct).union-1,
              0);
  check-equal("bug 321 c-func second value 1, 0",
              union-tester-1(struct),
              0);

  union-struct(struct).union-2 := -1;
  check-equal("bug 321 accessor value 2, -1",
              union-struct(struct).union-2,
              -1);
  check-equal("bug 321 c-funf value 2, -1",
              union-tester-2(struct),
              -1);
  union-tester-2(struct) := -1;
  check-equal("bug 321 accessor second value 2, -1",
              union-struct(struct).union-2,
              -1);
  check-equal("bug 321 c-func second value 2, -1",
              union-tester-2(struct),
              -1);
  union-struct(struct).union-2 := 0;
  check-equal("bug 321 accessor value 2, 0",
              union-struct(struct).union-2,
              0);
  check-equal("bug 321 c-funf value 2, 0",
              union-tester-2(struct),
              0);
  union-tester-2(struct) := 0;
  check-equal("bug 321 accessor second value 2, 0",
              union-struct(struct).union-2,
              0);
  check-equal("bug 321 c-func second value 2, 0",
              union-tester-2(struct),
              0);

  union-struct(struct).union-3 := #xff;
  check-equal("bug 321 accessor value 3, #xff",
              union-struct(struct).union-3,
              #xff);
  check-equal("bug 321 c-funf value 3, #xff",
              union-tester-3(struct),
              #xff);
  union-tester-3(struct) := #xff;
  check-equal("bug 321 accessor second value 3, #xff",
              union-struct(struct).union-3,
              #xff);
  check-equal("bug 321 c-func second value 3, #xff",
              union-tester-3(struct),
              #xff);
  union-struct(struct).union-3 := 0;
  check-equal("bug 321 accessor value 3, 0",
              union-struct(struct).union-3,
              0);
  check-equal("bug 321 c-funf value 3, 0",
              union-tester-3(struct),
              0);
  union-tester-3(struct) := 0;
  check-equal("bug 321 accessor second value 3, 0",
              union-struct(struct).union-3,
              0);
  check-equal("bug 321 c-func second value 3, 0",
              union-tester-3(struct),
              0);

  union-struct(struct).union-4 := -1;
  check-equal("bug 321 accessor value 4, -1",
              union-struct(struct).union-4,
              -1);
  check-equal("bug 321 c-funf value 4, -1",
              union-tester-4(struct),
              -1);
  union-tester-4(struct) := -1;
  check-equal("bug 321 accessor second value 4, -1",
              union-struct(struct).union-4,
              -1);
  check-equal("bug 321 c-func second value 4, -1",
              union-tester-4(struct),
              -1);
  union-struct(struct).union-4 := 0;
  check-equal("bug 321 accessor value 4, 0",
              union-struct(struct).union-4,
              0);
  check-equal("bug 321 c-funf value 4, 0",
              union-tester-4(struct),
              0);
  union-tester-4(struct) := 0;
  check-equal("bug 321 accessor second value 4, 0",
              union-struct(struct).union-4,
              0);
  check-equal("bug 321 c-func second value 4, 0",
              union-tester-4(struct),
              0);
end;

// 
// -------------
// tests for bug 290

define c-pointer-type <c-char**> => <c-char*>;

define variable *arg-vec* :: <C-char**> = make(<C-char**>, element-count: 2);

define test bug-290 ()
  check-true("bug 290 pointer value setter works",
             pointer-value(*arg-vec*, index: 1) := null-pointer(<C-char*>));
end;


// 
// -------------
// tests for bug 313

define c-function mix-it-up
  input output parameter inout :: <C-int*>;
  result value :: <C-int>;
  c-name: "mix_it_up";
end;

define test bug-313 ()
  let (a :: <integer>, b :: <integer>) = mix-it-up(7);
  check-equal("bug 313  mix it up value 1", a, 7 + 1);
  check-equal("bug 313  mix it up value 2", b, 7 + 7);
end;

// 
// -------------
// tests for bug 17


define C-struct <bar>
  slot foo :: <C-int>;
end;

define C-pointer-type <bar-p> => <bar>;

define c-subtype <bar-bar> (<bar-p>)
  slot fff :: <integer>, init-value: 5;
end;

define test bug-17 ()
    check-true("bug 17 make regular pointer",
               instance?(make(<bar-p>), <bar-p>)); // this works fine
    check-true("bug 17 make pointer subtype",
               instance?(make(<bar-bar>), <bar-bar>)); // fails
end;

// 
// -------------
// tests for bug 134



define C-struct <LARGE-INTEGER>
  slot LowPart-value  :: <C-unsigned-long>;
  slot HighPart-value :: <C-signed-long>;
end;

define C-union <variant-union>
  slot lVal-value  :: <C-long>;
  slot bVal-value  :: <C-unsigned-char>;
  slot iVal-value  :: <C-short>;
  slot cyVal-value :: <LARGE-INTEGER>;
  slot ptr-value :: <C-pointer>;
end;

define test bug-134 ()
  check-true("bug 134 union has full size",
             size-of(<variant-union>) >= size-of(<large-integer>));
  check-true("bug 134 large-integer has full size",
             size-of(<large-integer>)
               >= size-of(<C-unsigned-long>) + size-of(<C-signed-long>));
end;


// 
// -------------
// tests for bug 213

define c-struct <HENV-213>
  slot whatever :: <C-signed-int>;
  pointer-type-name: <HENV-213*>;
end C-struct;

define test bug-213 ()
  check-true("bug 213 make returns", 
             instance?(make(<henv-213*>), <henv-213*>));
end;

// 
// -------------
// tests for bug 209

define method betty (a :: <integer>)
  a * a;
end method;

define c-callable-wrapper wilma of betty
  parameter a :: <C-int>;
  result r :: <C-int>;
  c-name: "betty";
end c-callable-wrapper;

define c-function call-betty 
  parameter a :: <c-int>;
  result r :: <C-int>;
  c-name: "betty";
end;

define test bug-209 ()
  check-equal("bug 209 call betty", call-betty(2), 4);
end;

// 
// -------------
// tests for bug 289

define c-function puts-289
  parameter str :: <C-string>;
  result len :: <C-int>;
  c-name: "puts";
end;


define test bug-289 ()
  check-true("Bug 289 puts works", puts-289("a string"));
end;


// 
// -------------
// tests for bug 394

define C-struct <DDEACK>
  bitfield slot bAppReturnCode-value :: <C-unsigned-short>, width: 8; 
  bitfield slot reserved-value :: <C-unsigned-short>, width: 6;
  bitfield slot fBusy-value    :: <C-unsigned-short>, width: 1;
  bitfield slot fAck-value     :: <C-unsigned-short>, width: 1;
  pointer-type-name: <LPDDEACK>;
end C-struct <DDEACK>;


define c-function ack-return-slot
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "return_code_slot";
end;

define c-function ack-return-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "return_code_slot_set";
end;

define c-function ack-reserved-slot
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "reserved_slot";
end;

define c-function ack-reserved-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "reserved_slot_set";
end;

define c-function ack-busy-slot
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "busy_slot";
end;

define c-function ack-busy-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "busy_slot_set";
end;


define c-function ack-ack-slot
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "ack_slot";
end;

define c-function ack-ack-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <LPDDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "ack_slot_set";
end;

define test bug-394 ()
  let dde :: <LPDDEACK> = make(<LPDDEACK>);
  // use c-struct setters and getters
  bAppReturnCode-value(dde) := 127;
  check-equal("by-value ddeack return correct value",
	      Vack-return-slot(dde), 127);
  reserved-value(dde) := 31;
  check-equal("by-value ddeack reserved correct value",
	      Vack-reserved-slot(dde), 31);
  fBusy-value(dde) := 1;
  check-equal("by-value ddeack busy correct value",
	      Vack-busy-slot(dde), 1);
  fAck-value(dde) := 1;
  check-equal("by-value ddeack ack correct value",
	      Vack-ack-slot(dde), 1);
  // Use foreign function setter and c-struct getters
  Vack-return-slot-setter(120, dde);
  check-true("by-value ack return setter doesn't set",
	      (bAppReturnCode-value(dde) ~= 120));
  check-equal("by-value ack return setter leaves value alone",
	      bAppReturnCode-value(dde), 127);
  Vack-reserved-slot-setter(20, dde);
  check-true("by-value ack reserved setter doesn't set",
	      (Reserved-value(dde) ~= 20));
  check-equal("by-value ack reserved setter leaves value alone",
	      Reserved-value(dde), 31);
  Vack-busy-slot-setter(0, dde);
  check-true("by-value ack busy setter doesn't set",
	      (fBusy-value(dde) ~= 0));
  check-equal("by-value ack busy setter leaves value alone",
	      fBusy-value(dde), 1);
  Vack-ack-slot-setter(0, dde);
  check-true("by-value ack ack setter doesn't set",
	      (fAck-value(dde) ~= 0));
  check-equal("by-value ack ack setter leaves value alone",
	      fAck-value(dde), 1);



end;

// 
// -------------
// tests for indirect: option to define c-function

define c-function call-indirect
  indirect: #t;
  parameter param1 :: <C-int>;
  result val :: <C-int>;
end;

define c-function gimme-a-function
  result fun :: <C-function-pointer>;
  c-name: "gimme_a_function";
end;


define test c-function-indirect ()
  let fun = gimme-a-function();
  check-equal("c-function indirect option", call-indirect(fun, 7), 7);
end;


// 
// -------------
// tests for <c-dylan-object>

define c-struct <test-dylan-object>
  slot object-handle :: <c-dylan-object>;
  pointer-type-name: <test-dylan-object*>;
end;

define test c-dylan-object-test ()
  let obj = pair(1, 2);		// make an object
  register-c-dylan-object(obj);  // register it
  let struct = make(<test-dylan-object*>);
  let handle = export-c-dylan-object(obj);
  object-handle(struct) := handle;  
  check-equal("<C-dylan-object> importing handle",
	     import-c-dylan-object(handle), obj);
  check-equal("<C-dylan-object> extracting handle from struct",
	     import-c-dylan-object(object-handle(struct)),
	     obj);
  // do more here.
  // ... 
  unregister-c-dylan-object(obj);
  destroy(struct);
end;



// 
// -------------
// tests for bug 393


define function main-bug-393 ()
  for ( i from 0 below 200 )
    let buf = make(<c-string>, size: 10 + i);
    destroy(buf);
  end for;
  #t;
end main-bug-393;

define test bug-393 ()
  check-true("bug-393 try it",
	     main-bug-393());
end;
	     

// 
// -------------
// tests for bug 414


  define C-struct <OLEMENUGROUPWIDTHS>
      array slot width-array :: <C-long>, length: 6,
     address-getter: width-value,
     getter: #f, setter: #f;
      pointer-type-name: <LPOLEMENUGROUPWIDTHS>;
      c-name: "struct tagOleMenuGroupWidths";
    end C-struct <OLEMENUGROUPWIDTHS>;

define test bug-414 ()
  let menugroupwidths = make(<LPOLEMENUGROUPWIDTHS>);
  // should not generate any warnings when compiling,
  // or errors when running
  check-true("bug 414 no warnings or errors", menugroupwidths.width-value);
  destroy(menugroupwidths);
end;


// 
// -------------
// tests for passing c structs by value

// ***--- We've disabled this for now as it is broken in the C back-end.

/*
define c-function OMGW-width
  parameter struct :: <OLEMENUGROUPWIDTHS>;
  parameter index :: <C-int>;
  result r :: <c-long>;
  c-name: "OMGW_width"
end;

define c-function Vack-return-slot
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vreturn_code_slot";
end;

define c-function Vack-return-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vreturn_code_slot_set";
end;

define c-function Vack-reserved-slot
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vreserved_slot";
end;

define c-function Vack-reserved-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vreserved_slot_set";
end;

define c-function Vack-busy-slot
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vbusy_slot";
end;

define c-function Vack-busy-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vbusy_slot_set";
end;


define c-function Vack-ack-slot
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vack_slot";
end;

define c-function Vack-ack-slot-setter
  parameter v :: <C-unsigned-short>;
  parameter dde :: <DDEACK>;
  result r :: <c-unsigned-short>;
  c-name: "Vack_slot_set";
end;


define test struct-by-value ()
  let dde :: <LPDDEACK> = make(<LPDDEACK>);
  // use c-struct setters and getters
  bAppReturnCode-value(dde) := 127;
  check-equal("by-value return code setter and getter",
	      bAppReturnCode-value(dde),
	      127);
  reserved-value(dde) := 31;
  check-equal("by-value reserved setter and getter",
	      reserved-value(dde),
	      31);
  fBusy-value(dde) := 1;
  check-equal("by-value busy setter and getter",
	      fBusy-value(dde),
	      1);
  fAck-value(dde) := 1;
  check-equal("by-value ack setter and getter",
	      fAck-value(dde),
	      1);  
  // Use foreign function setter and c-struct getters
  ack-return-slot(dde) := 120;
  check-equal("by-value return code ff setter and getter",
	      bAppReturnCode-value(dde),
	      120);
  ack-reserved-slot(dde) := 20;
  check-equal("by-value reserved ff setter and getter",
	      reserved-value(dde),
	      20);
  ack-busy-slot(dde) := 0;
  check-equal("by-value busy ff setter and getter",
	      fBusy-value(dde),
	      0);
  ack-ack-slot(dde) := 0;
  check-equal("by-value ack ff setter and getter",
	      fAck-value(dde),
	      0);
  let widths = make(<LPOLEMENUGROUPWIDTHS>);
  for (i from 0 below 6)
    pointer-value(width-value(widths), index: i) := i + 47;
    check-equal("by-value array-slot values",
		OMGW-width(widths, i), i + 47);
  end;
end;
*/



// --------------
// top level test suite.

define suite c-ffi-suite ()
  test variable-address-test;
  test c-struct-test;
  test c-function-test;
  test c-string-test;
  test c-types-test;
  test bug-217;
  test bug-321;
  test bug-290;
  test bug-313;
  test bug-17;
  test bug-134;
  test bug-213;
  test bug-209;
  test bug-289;
  test bug-394;
  test c-function-indirect;
  test c-dylan-object-test;
  test bug-393;
  test bug-414;
  // test struct-by-value
end suite c-ffi-suite;

/// The dylan top level for the tests
/// This gets called via a C-callable function from C
define method run-dylan-tests () => ();
  *debug?* := #f;
  perform-suite(c-ffi-suite);
end;

define c-callable-wrapper run-dylan-tests-pointer of run-dylan-tests
  c-name: "run_dylan_tests";
end;

/// invokes the C test top level
define c-function run-tests-from-c
  c-name: "run_tests_from_c"
end;

/// signed integer identity function callable from C
define c-callable-wrapper dylan-identity of identity
  parameter j :: <C-raw-signed-long>;
  result j :: <C-raw-signed-long>;
  c-name: "dylan_int_identity"
end;


define constant always-one = method () as(<machine-word>, 1) end;

define c-callable-wrapper dylan-always-one of always-one
  result j :: <C-raw-signed-long>;
  c-name: "dylan_always_one"
end;

// -------------
 

/// set up bookkeeping around running the tests
define method c-ffi-test-begin () => ();
  format-out("Starting to run C FFI tests\n");
end;

define c-variable c-failure-count :: <C-unsigned-long>
  c-name: "failure_count";
end;

define c-variable c-test-count :: <C-unsigned-long>
  c-name: "test_count";
end;


/// testworks doesn't know about the tests we have run in C
/// so we can report them here.
define method c-ffi-test-final-report (abort) => ();
  format-out("%s running C FFI tests\n",
	 if (abort) "Aborted" else "Finished" end);
  let c-lossage-count = c-failure-count();
  format-out("Encountered %= failures from C\n",
	     c-lossage-count);
  format-out("out of a total of %= tests from C.\n", c-test-count());
end;

/// Top level for the whole library
define method run-da-tests() => ();
  let abort = #t;
  block ()
    c-ffi-test-begin();
    run-tests-from-c();
    abort := #f;
  cleanup
    c-ffi-test-final-report(abort);
  end;
end;


run-da-tests();

//-------------


