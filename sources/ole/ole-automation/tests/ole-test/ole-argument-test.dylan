Module:    ole-test
Synopsis:  OLE Automation dispatch properties test.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/* ole-argument-test tests passing ole-variables to and from the server. 
   <expected-values> contains all the variables that will be passed to the 
   server. After a value is passed to the server, it is retrieved and compared
   the original value passed. */

define class <expected-values> (<object>)
  constant slot the-integer-value :: <integer> = 143;
  constant slot the-negative-integer-value :: <integer> = -1234;
  constant slot the-large-integer-value :: <integer> = 5000000;
  constant slot the-machine-word-value :: <machine-word> = 
    as(<machine-word>, #x80000000);
  // constant slot the-character-value :: <character> = 'c'; 
  constant slot the-single-float-value :: <single-float> =
    as(<single-float>, 1.234);
  constant slot the-double-float-value :: <double-float> =
    as(<double-float>, 5.6789);
  constant slot the-string-value :: <string> = "hello";
  constant slot the-sequence-1-value :: <sequence> = list(4, 3);
  constant slot the-sequence-2-value :: <sequence> =
    vector("one", 2, "three", #t);
  constant slot the-sequence-3-value :: <sequence> =
    vector(vector(1, "one", #t),
	   vector("two", 't', #f),
	   "three");
  constant slot the-string-sequence-value :: <sequence> = 
    vector("one", "two", "three", "four");
  constant slot the-array-value :: <array> = 
    begin
      let the-array = make(<array>, dimensions: list(3));
      aref-setter("one", the-array, 0);
      aref-setter(2, the-array, 1);
      aref-setter(#f, the-array, 2);
      the-array
    end;
  constant slot the-multi-dimensional-array-value :: <array> =
    begin
      let the-array = make(<array>, dimensions: list(2, 2));
      aref-setter("yes", the-array, 0, 0);
      aref-setter("sure", the-array, 0, 1);
      aref-setter("nope", the-array, 1, 0);
      aref-setter("no way", the-array, 1, 1);
      the-array
    end;
end class <expected-values>;

define test ole-argument-test 
  (name: "ole-argument-test",
   description: 
     "tests passing arguments from a OLE controller to a OLE server")
  let expected-values = make(<expected-values>);

  // check ole-constant
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "pi"));
  check-equal("checking pi",
	      get-property(*disp-interface*, *disp-id*),
	      3.1416);
 
  // check passing an integer
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "integer"));
  check-not-crash("setting integer",
		  set-property(*disp-interface*, *disp-id*,
			       expected-values.the-integer-value));
  check-equal("checking integer", 
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-integer-value);
  
  // check using name as direct reference instead of using a disp-id
  check-not-crash("setting integer using direct reference",
		  set-property(*disp-interface*, "integer",
			       expected-values.the-integer-value));
  check-equal("checking integer", 
	      get-property(*disp-interface*, "integer"),
	      expected-values.the-integer-value);
  
  // check names of ole variables are case insensitive
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "iNteGer"));
  check-equal("checking integer using case insensitive name",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-integer-value);

  // check using case-insensitive name as direct reference instead 
  // of using a disp-id 
  check-equal("checking integer using case insensitive name directly",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-integer-value);

  // checking passing a negative integer
  check-not-crash("setting negative integer",
		  set-property(*disp-interface*, *disp-id*,
			       expected-values.the-negative-integer-value));
  check-equal("checking negative integer",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-negative-integer-value);

  // checking passing a 32-bit integer
  check-not-crash("setting 32-bit integer",
		  set-property(*disp-interface*, *disp-id*,
			       expected-values.the-large-integer-value));
  check-equal("checking the 32-bit integer",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-large-integer-value);

  // checking passing a machine-word
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, "machine-word"));
  check-not-crash("setting machine-word",
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-machine-word-value));
  check-equal("checking machine-word", 
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-machine-word-value); 

  // checking passing a single-float
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, "single-float"));
  check-not-crash("setting single-float",
		  set-property(*disp-interface*, *disp-id*,
			       expected-values.the-single-float-value));
  check-equal("passing a single-float",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-single-float-value);

  // checking passing a double-float
  check-not-crash("check getting the dispatch id",
		  *disp-id* := 
		    get-id-of-name(*disp-interface*, "double-float"));
  check-not-crash("setting double-float",
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-double-float-value));
  check-equal("passing a double-float",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-double-float-value);

  // checking passing a boolean
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "boolean"));
  check-not-crash("setting true", 
		  set-property(*disp-interface*, *disp-id*, #t));
  check-equal("passing true",
	      get-property(*disp-interface*, *disp-id*),
	      #t);

  // check that you cannot pass the wrong type to a defined ole-variable
  check-condition("setting boolean as integer?", <error>,
	      set-property(*disp-interface*, *disp-id*,
			   123));
  check-equal("getting boolean again",
	      get-property(*disp-interface*, *disp-id*),
	      #t);

  check-not-crash("setting false", 
		  get-property(*disp-interface*, *disp-id*) := #f);
  check-equal("passing false",
	      get-property(*disp-interface*, *disp-id*),
	      #f);

  // check passing a string
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "string"));
  check-not-crash("setting string", 
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-string-value));
  check-equal("passing a string",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-string-value);
  check-true("string property is a <BSTR>",
	     instance?(get-property(*disp-interface*, *disp-id*), 
		       <BSTR>));

  // check passing a sequence
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "sequence"));
  check-not-crash("setting sequence",
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-sequence-1-value));
  
  check-equal("passing a sequence",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-sequence-1-value);
 
  // check passing a heterogenious sequence
  check-not-crash("setting another sequence",
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-sequence-2-value));  
  check-equal("passing another sequence",
 	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-sequence-2-value);

  // check passing a sequence of sequences
  check-not-crash("setting sequence with a sequence of sequnces",
		  set-property(*disp-interface*, *disp-id*,
			       expected-values.the-sequence-3-value));
  check-equal("passing a sequence of sequences",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-sequence-3-value);

  // check passing a sequence of strings (used to crash)
  check-not-crash("setting sequence with a sequence of strings",
		  set-property(*disp-interface*, *disp-id*,
			       expected-values.the-string-sequence-value));
  check-equal("pass a sequnce of strings",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-string-sequence-value);

  // check the ole-vector function, using sequence-2
  let test-ole-vector = #f; 
  check-true("make an ole vector", 
	     test-ole-vector := ole-vector(3, 4, #t));
  check-true("testing size of ole-vector",
	     size(test-ole-vector) = 3);
  check-true("testing <sequence> attribute of ole-vector",
	     first(test-ole-vector) = 3);
  check-true("testing another <sequence> attribute of ole-vector",
	     last(test-ole-vector));
  check-not-crash("setting sequence using ole-vector function",
		  set-property(*disp-interface*, *disp-id*,
			       test-ole-vector));
  check-equal("passing ole-vector sequence",
	      get-property(*disp-interface*, *disp-id*),
	      test-ole-vector);

   // check passing an array
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "array"));
  check-not-crash("setting array",
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-array-value));
  check-equal("passing an array",
	      get-property(*disp-interface*, *disp-id*),
	      expected-values.the-array-value);
  
  // checking passing a multi-dimensional array
  check-not-crash("setting a multi-dimensional array",
		  set-property(*disp-interface*, *disp-id*, 
			       expected-values.the-multi-dimensional-array-value));
  let mda = #f;
  check-not-crash("getting a multi-dimensional array",
		  mda := get-property(*disp-interface*, *disp-id*));

  // Note: the following check fails in Functional Developer 1.0 because the
  // implemention of multi-dimensional <ole-array> was incomplete.  (Bug 1768)
  check-equal("check dimensions are equal (ref Bug 1768)",
	      dimensions(get-property(*disp-interface*, *disp-id*)),
	      dimensions(expected-values.the-multi-dimensional-array-value));

  if ( instance?(mda, <array>) & rank(mda) = 2 ) // When Bug 1768 is fixed
    check-equal("mda[0,0]", mda[0,0],
		expected-values.the-multi-dimensional-array-value[0,0]);
    check-equal("mda[0,1]", as(<byte-string>, mda[0,1]),
		expected-values.the-multi-dimensional-array-value[0,1]);
    check-equal("mda[1,0]", as(<byte-string>, mda[1,0]),
		expected-values.the-multi-dimensional-array-value[1,0]);
    check-equal("mda[1,1]", as(<byte-string>, mda[1,1]),
		expected-values.the-multi-dimensional-array-value[1,1]);
  end if;

  // checking passing $SQL-NULL
  check-not-crash("check getting the dispatch id",
		  *disp-id* := get-id-of-name(*disp-interface*, "nothing"));
  check-not-crash("setting 'nothing' to $SQL-NULL",
		  set-property(*disp-interface*, *disp-id*, $SQL-NULL));
  check-equal("passing $SQL-NULL",
	      get-property(*disp-interface*, *disp-id*),
	      $SQL-NULL);

  // test non-existent property
  check-equal("no such property name",
	      get-property(*disp-interface*, "no-such", default: #"none"),
	      #"none");
  check-equal("no such property ID",
	      get-property(*disp-interface*, 666, default: #"nope"),
	      #"nope");

  // test indexed properties (New feature, January 1998)
  *disp-id* := get-id-of-name(*disp-interface*, "OneIndex");
  for ( item in $single-index-value,
        index from 0 )
    check-equal("get-property with single index",
		get-property(*disp-interface*, *disp-id*, index: index),
		item);
    check-equal("get-indexed-property",
		get-indexed-property(*disp-interface*, *disp-id*, index),
		item);
  end for;
  check-equal("indexed constant",
	      get-indexed-property(*disp-interface*,
			get-id-of-name(*disp-interface*, "indexcon"), 2),
	      "two");
  begin
    let mi-disp-id = get-id-of-name(*disp-interface*, "multindex");
    check-equal("get multi-index prop",
		get-indexed-property(*disp-interface*, mi-disp-id, 1, 2),
		$mult-index-value[1,2]);
    let some-value = 444;
    check-equal("set multi-index prop",
		begin
		  get-indexed-property(*disp-interface*, mi-disp-id, 2, 1)
		     := some-value;
		  $mult-index-value[2,1]
		end,
		some-value);
    check-equal("check multi-index prop",
		get-property(*disp-interface*, mi-disp-id,
			     index: vector(2, 1)),
		some-value);
    let st-disp-id = get-id-of-name(*disp-interface*, "StringTable");
    check-equal("property indexed by string",
		get-indexed-property(*disp-interface*, st-disp-id, "CA"),
		"California");
    check-equal("set property indexed by string",
		begin
		  get-indexed-property(*disp-interface*, st-disp-id, "XX")
		    := "extra";
		  *string-table-value*["XX"]
		end,
		"extra");
  end;

end test ole-argument-test;
