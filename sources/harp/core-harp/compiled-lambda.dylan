module:    harp-outputter
Synopsis:  definition of the <compiled-lambda> class
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Revised to compact compiled-lambdas    Nosa Omo  May, 1998


//// The <compiled-lambda> class.
//// This represents the results of code generation, and permits code to
//// be output separately from the code generation phase. Instances
//// of <compiled-lambda> might be stored in a data base for example.

define abstract primary open dood-class <compiled-lambda> (<lambda-compiled-data>)
  lazy slot lambda-name-internal, required-init-keyword: name:;
  // The mangled name OR model-object of the lambda

  lazy constant slot lambda-location :: false-or(<absolute-source-position>) = #f,
    init-keyword: location:;

  lazy constant slot lambda-all-locators :: <simple-object-vector> = #[],
    init-keyword: all-locators:;
  // A vector of all debug info locators (as <relative-source-position>s or packed integers)
  // associated with the code.

  lazy constant slot lambda-selected-locators-internal = 0,
    init-keyword: selected-locators:;
  // bit-pattern or bit-vector representing what subset of all-locators is selected
  // -- we should really add an extra bit to locators and get rid of this slot

  lazy constant slot lambda-variable-scopes-internal :: <debug-scopes> = make-zero-debug-scopes(),
            init-keyword: scopes:;
  // Debuggable variable scopes for this lambda

  lazy constant slot lambda-all-variable-scopes :: <simple-object-vector> = #[],
            init-keyword: all-scopes:;
  // All Debuggable variable scopes for this lambda

  lazy constant slot lambda-all-variable-names :: <simple-object-vector> = #[],
            init-keyword: all-names:;
  // All Debuggable variable names for this lambda

  constant slot compiled-lambda-packed-slot :: 
    type-union(<integer>, <compiled-lambda-unpacked-slots>) = 0,
    init-keyword: slots:;
  // packed slot or unpacked-slots of slots with extremely common value ranges

end dood-class;


// The fully-annotated concrete subclass which is the active object
// during compilations; only non-persistent slots reside here

define primary dood-class <fully-compiled-lambda> (<compiled-lambda>)

  constant slot lambda-object-location :: false-or(<source-location>) = #f,
    init-keyword: object-location:;
  // Source location information for writing to object files.
  
  lazy constant slot lambda-model-object, required-init-keyword: model:;
  // The model-object of the lambda

  lazy constant slot lambda-code :: <vector>, 
     required-init-keyword: code:;
  // A vector of code for the lambda, containing only integers (may be 
  // a <byte-vector>). This will contain zeros where there are labels.

  lazy constant slot lambda-referenced-data :: false-or(<vector>) = #f, 
    init-keyword: referenced-data:;
  // A vector of data referenced by the lambda, containing only integers 
  // (the class of vector will be as for lambda-code). This data will be 
  // emitted immediately before the definition of the lambda-name (and
  // hence before the lambda-code). The primary purpose of this data is
  // to get FP constants into the code stream. These wil be referenced
  // as <indirect-constant-reference>s at a negative offset relative to
  // lambda-name. Lambda which do not need referenced data set this to #f.

  lazy constant slot lambda-labels :: <simple-object-vector>, 
    required-init-keyword: labels:;
  // A vector of labels (e.g. constant references) which are 
  // associated with the code.

  lazy constant slot lambda-externals :: <simple-object-vector> = #[], 
    init-keyword: externals:;
  // A vector of constant references which are external references from 
  // this lambda to an external linker unit

end dood-class;

define method external-lambda-location (lambda :: <fully-compiled-lambda>)
 => (start-line :: false-or(<integer>),
     end-line :: false-or(<integer>),
     name :: false-or(<byte-string>))
  let loc = lambda.lambda-location;
  let src-loc = lambda.lambda-object-location;
  let (start-line :: false-or(<integer>), end-line :: false-or(<integer>))
    = if (src-loc)
	object-source-location-lines(src-loc)
      elseif (loc)
	values(loc.source-record-start-line,
	       loc.source-record-end-line)
      else
	values(#f, #f)
      end;
  values(start-line, end-line, loc & loc.source-record-file-name);
end;

// The less-annotated concrete subclass which is stored to databases;
// only persistent slots reside here

define class <stripped-compiled-lambda> (<compiled-lambda>)
end class;


// Conditionally pack these slots

define primary dood-class <compiled-lambda-unpacked-slots> (<object>)

  constant slot lambda-frame-g-size-internal :: <integer> = 0, init-keyword: frame-g-size:;
  // Size (in backend units - e.g. words) of the GCable stack frame

  constant slot lambda-frame-n-size-internal :: <integer> = 0, init-keyword: frame-n-size:;
  // Size (in backend units - e.g. words) of the raw stack frame

  lazy constant slot lambda-harp-print-info-internal = #f, 
    init-keyword: print-info:;
  // Either false, or a <byte-string> which contains the printed
  //  information which is useful for debugging the HARP

  constant slot lambda-is-public?-internal = #t, init-keyword: public:;
  // A boolean which indicates whether the lambda is public or static

  constant slot lambda-is-export?-internal = #t, init-keyword: export:;
  // A boolean which indicates whether the lambda is exported

end dood-class;


define packed-slots?(<compiled-lambda>)
  constant lambda-frame-g-size     :: <integer>, init-keyword: frame-g-size:, width: 15;
  constant lambda-frame-n-size     :: <integer>, init-keyword: frame-n-size:, width: 11;
  constant lambda-harp-print-info  :: <object>,  init-keyword: print-info:;
  constant lambda-is-public?       :: <object>, init-keyword: public:;
  constant lambda-is-export?       :: <object>, init-keyword: export:;
end;


define method make
    (class == <compiled-lambda>,
     #rest keys,
     #key frame-g-size,
          frame-n-size,
          print-info,
          public,
          export,
     #all-keys) 
    => (r :: <fully-compiled-lambda>)
  let slots =
    make-compiled-lambda-packed-slot?(frame-g-size,
				      frame-n-size,
				      print-info,
				      public,
				      export)
  | (make(<compiled-lambda-unpacked-slots>,
	  frame-g-size: frame-g-size,
	  frame-n-size: frame-n-size,
	  print-info: print-info,
	  public: public,
	  export: export));

  apply(make, <fully-compiled-lambda>,
	slots: slots,
	keys);

end method;

// strip fully-compiled-lambdas by shallow-copying

define method flush-compiled-lambda-post-output (cl :: <fully-compiled-lambda>)
  => (cl :: <stripped-compiled-lambda>)

  make(<stripped-compiled-lambda>,
       name: cl.lambda-name-internal,
       location: cl.lambda-location,
       all-locators: cl.lambda-all-locators,
       selected-locators: cl.lambda-selected-locators-internal,
       scopes: cl.lambda-variable-scopes-internal,
       all-scopes: cl.lambda-all-variable-scopes,
       all-names: cl.lambda-all-variable-names,
       slots: cl.compiled-lambda-packed-slot);

end method;

// Encode selected-locators as bitsets representing availability wrt all-locators

define method lambda-selected-locators
    (cl :: <compiled-lambda>) => (selected-locators :: <simple-object-vector>)
  let all-locators :: <simple-object-vector> = cl.lambda-all-locators;
  let bit-set :: <vector-32bit> = unpack-value-as-bitset(cl.lambda-selected-locators-internal);
  let position :: <integer> = -1;

  if (bit-set)
    choose(method(loc :: <relative-source-position>)
	       position := position + 1;
	       get-bit-from-set(bit-set, position)
	   end,
	   all-locators)
  else
    #[]
  end if;
end method;

define method lambda-variable-scopes
    (cl :: <compiled-lambda>) => (variable-scopes :: <simple-object-vector>)
  debug-scopes-as-vector(cl.lambda-variable-scopes-internal,
			 cl.lambda-all-variable-scopes);
end method;
