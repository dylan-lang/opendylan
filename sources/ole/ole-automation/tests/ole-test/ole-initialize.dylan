Module: ole-test
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// macro for a check-not-crash that traps errors when the occur

define macro check-not-crash
  { check-not-crash
     (?check-name:expression, ?check-expression:expression) }
    => { check-true (?check-name, begin
				    ?check-expression;
				    #t
				  end) }
end macro check-not-crash;


// Global variables

define variable *disp-id* = #f;

define variable *factory* = #f;

define variable *disp-interface* = #f;

define variable *ole-server-typeinfo* = #f;
 
// Ole server
// 6e5d60a0-e735-11d0-8a24-02070119f639

define constant $class-id =
  make-GUID(#x6E5D60A0, #xE735, #x11D0, #x8A, #x24, #x02, #x07,
	    #x01, #x19, #xF6, #x39);

/* ole-initialize contains the information for creating the ole-server that
   the tests are run off of. */

define constant heterogeneous-vector = ole-array-type(<VARIANT>);

define method make-ole-server-typeinfo 
    () => ()
  let disp-type-info = 
    make(<Disp-Type-Info>,
	 name: "AutoTest",
	 documentation: "ole-communication-test",
	 uuid: make-GUID(#x6E5D60A1, #xE735, #x11D0, #x8A, #x24, #x02, #x07,
			 #x01, #x19, #xF6, #x39),
	 properties: vector(make(<variable-description>,
				 name: "integer",
				 getter: integer-value,
				 setter: integer-value-setter,
				 type: <C-long>),
			    make(<variable-description>,
				 name: "machine-word",
				 getter: machine-word-value,
				 setter: machine-word-value-setter,
				 type: <C-short>), 
			    make(<variable-description>,
				 name: "character",
				 getter: character-value,
				 setter: character-value-setter,
				 type: <C-character>), 
			    make(<variable-description>,
				 name: "boolean",
				 getter: boolean-value,
				 setter: boolean-value-setter,
				 type: <boolean>),
			    make(<variable-description>,
				 name: "single-float",
				 getter: single-float-value,
				 setter: single-float-value-setter,
				 type: <C-float>),
			    make(<variable-description>,
				 name: "double-float",
				 getter: double-float-value,
				 setter: double-float-value-setter,
				 type: <double-float>),
			    make(<variable-description>,
				 name: "string",
				 getter: string-value,
				 setter: string-value-setter,
				 type: <BSTR>),
			    make(<variable-description>,
				 name: "sequence",
				 getter: sequence-value,
				 setter: sequence-value-setter,
				 type: heterogeneous-vector),
			    make(<variable-description>,
				 name: "array",
				 getter: array-value,
				 setter: array-value-setter,
				 type: heterogeneous-vector),
			    make(<variable-description>,
				 name: "nothing",
				 getter: nothing,
				 setter: nothing-setter),
			    make(<variable-description>,
				 name: "OneIndex",
				 getter: single-index-value,
				 type: ole-array-type(<integer>)),
			    make(<variable-description>,
				 name: "multindex",
				 getter: mult-index-value,
				 setter: mult-index-value-setter,
				 type: ole-array-type(<C-short>)),
			     make(<constant-description>,
				 name: "pi",
				 value: 3.1416),
			    make(<variable-description>,
				 name: "StringTable",
				 getter: string-table-value,
				 setter: #f,
				 type: ole-array-type(<string>)),
			    make(<constant-description>,
				 name: "indexcon",
				 value: #["zero", "one", "two"],
				 type: ole-array-type(<string>)) ),
		       methods: 
			 vector(make(<function-description>,
				     name: "bare-method",
				     function: bare-method),
				make(<function-description>,
				     name: "string-returned",
				     function: string-returned,
				     result-type: <BSTR>),
				make(<function-description>,
				     name: "string-to-boolean",
				     function: string-to-boolean,
				     argument-names: #["string"],
				     argument-types: vector(<BSTR>),
				     result-type: <VARIANT-BOOL>),
				make(<function-description>,
				     name: "multiply",
				     function: multiply,
				     argument-names: #["arg1", "arg2"],
				     argument-types: 
				       vector(<C-short>, <C-short>),
				     result-type: <C-short>),
				make(<function-description>,
				     name: "ole-reverse",
				     function: ole-reverse,
				     argument-names: #["sequence"],
				     argument-types:
				       vector(heterogeneous-vector),
				     result-type: heterogeneous-vector),
				make(<function-description>,
				     name: "test-copy-automation-value",
				     function: test-copy-automation-value,
				     argument-names: #["passed-sequence"],
				     argument-types:
				       vector(heterogeneous-vector)),
				make(<function-description>,
				     name: "raise-exception",
				     function: raise-exception),
				make(<function-description>,
				     name: "raise-exception2",
				     function: raise-exception2),
				make(<function-description>,
				     name: "square",
				     function: in-out-square,
				     argument-names: #["ptr"],
				     argument-types: vector(
					make(<ole-by-ref-arg-spec>,
					     type: <C-long>,
					     direction: #"in-out")))
				  )
			 );
  *ole-server-typeinfo* :=
    make(<coclass-type-info>,
	 name: "test-ole-typeinfo",
	 uuid: $class-id,
	 interfaces: vector(make(<component-interface-description>,
				 class: <values>,
				 typeinfo: disp-type-info)));
end method make-ole-server-typeinfo;

define constant $single-index-value :: <vector> = #[7000, 7001];
define variable $mult-index-value :: <array> =
  make(<array>, dimensions: #(3, 4), fill: 999);
$mult-index-value[1,2] := 777;

define class <string-table> ( <mutable-collection> )
  slot string-table-slot :: <table> = make(<table>), setter: #f;
end class;

define method element ( table :: <string-table>, index :: <string>,
		       #rest args, #key default ) => (value :: <object>);
  let key = as(<symbol>, as(<byte-string>, index));
  apply(element, table.string-table-slot, key, args)
end method;

define method element-setter ( value, table :: <string-table>,
			       index :: <string> ) => (value :: <object>);
  let key = as(<symbol>, as(<byte-string>, index));
  element(table.string-table-slot, key) := as(<byte-string>, value)
end method;

define variable *string-table-value* :: <collection> = make(<string-table>);
*string-table-value*["AZ"] := "Arizona";
*string-table-value*["CA"] := "California";
*string-table-value*["TX"] := "Texas";

define COM-interface <values> (<Simple-Dispatch>)
  slot integer-value :: <integer>, init-value: 1; 
  slot machine-word-value :: <machine-word>, init-value: as(<machine-word>, 1);
  slot character-value :: <character>; 
  slot boolean-value :: <boolean>, init-value: #f;
  slot single-float-value :: <single-float>, 
    init-value: as(<single-float>, 1);
  slot double-float-value :: <double-float>, 
    init-value: as(<double-float>, 1); 
  slot string-value :: <string>, init-value: "init-value";
  slot sequence-value :: <sequence>, init-value: list(2, "hi");
  slot array-value :: <array> =  make(<array>, dimensions: #(2));
  slot single-index-value :: <vector> = $single-index-value,
    setter: #f;
  slot mult-index-value :: <array> = $mult-index-value;
  slot nothing;
end COM-interface <values>;

define method string-table-value(this :: <values>) => (table :: <collection>)
  *string-table-value*
end method;
