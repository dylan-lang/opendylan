Module:   dfmc-modeling
Synopsis: Model class definitions.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Booted simple objects.

define variable $dylan-system-subtype-bit-type-names :: <simple-object-vector>
  = #[#"<value-object>",                // 1
      #"<mm-wrapper>",                  // 2
      #"<class>",                       // 4
      #"<implementation-class>",        // 8
      #"<by-class-discriminator>",      // 16
      #"<abstract-integer>",            // 32
      #"<function>",                    // 64
      #"<sequence>",                    // 128
      #"<string>",                      // 256
      #"<error>",                       // 512
      #"<collection>",                  // 1024
      #"<cache-header-engine-node>",    // 2048
      #"<list>",                        // 4096
      #"<keyword-method>",              // 8192
      #"<engine-node>"                  // 16384
        ];

define abstract &top-type <top>
end;

define &bottom-type <bottom>
end;

define open compiler-open abstract compiler-class <object> (<top>)
end;
define open abstract model-class <object> ()
end;

define sealed domain ^make (subclass(<&object>));
define sealed domain ^initialize (<&object>);
define sealed domain initialize-packed-slots (<&object>);

// TODO: Remove this union type when one is defined in the appropriate
// place.

define constant <model-value>
  = type-union(<&top>, <heap-deferred-model>,
               <number>, <character>, <boolean>, <mapped-unbound>,
               <list>, <vector>, <string>, <symbol>); // etc.

define sealed concrete &class <boolean> (<object>) end;
define constant &true = #t;
define constant &false = #f;

define &override-operator \~ = \~ end;

define sealed abstract &class <value-object> (<object>) end;

define sealed abstract &class <character> (<object>) end;
define sealed concrete &class <byte-character> (<character>) end;
define sealed concrete &class <unicode-character> (<character>) end;

define open   abstract &class <number> (<object>) end;
define sealed abstract &class <complex> (<number>) end;
define sealed abstract &class <real> (<complex>) end;
define sealed abstract &class <rational> (<real>) end;
define sealed abstract &class <machine-number> (<real>) end;

define sealed abstract &class <abstract-integer> (<rational>) end;
define sealed concrete &class <integer> (<abstract-integer>, <machine-number>) end;
define sealed abstract &class <big-integer> (<abstract-integer>) end;

define sealed concrete &class <double-integer> (<big-integer>, <value-object>)
  runtime-constant raw &slot %%double-integer-low :: <raw-machine-word>;
  runtime-constant raw &slot %%double-integer-high :: <raw-machine-word>;
  metaclass <value-class>;
end;

define sealed abstract &class <float> (<machine-number>) end;

define sealed concrete &class <single-float> (<float>, <value-object>)
  runtime-constant raw &slot %single-float-data :: <raw-single-float>,
    required-init-keyword: data:;
  metaclass <value-class>;
end &class;

define sealed concrete &class <double-float> (<float>, <value-object>)
  runtime-constant raw &slot %double-float-data :: <raw-double-float>,
    required-init-keyword: data:;
  metaclass <value-class>;
end &class;

///--- NOTE: In our implementation, <extended-float> == <double-float> ...
/// define sealed concrete &class <extended-float> (<float>, <value-object>)
///   raw &slot %extended-float-data :: <raw-extended-float>, required-init-keyword: data:;
///   metaclass <value-class>;
/// end &class;

define sealed concrete &class <machine-word> (<value-object>)
  runtime-constant raw &slot %machine-word-data :: <raw-machine-word>,
     required-init-keyword: data:;
  metaclass <value-class>;
end &class;

define sealed concrete primary &class <symbol> (<object>)
  runtime-constant &slot symbol-name :: <string>, required-init-keyword: name:;
end &class;

define sealed concrete primary &class <namespace> (<object>)
  runtime-constant &slot namespace-name :: <byte-string>, required-init-keyword: name:;
end &class;

define sealed abstract &class <library-version> (<object>)
  runtime-constant &slot library-major-version :: <integer>,
    init-value: 0, init-keyword: major-version:;
  runtime-constant &slot library-minor-version :: <integer>,
    init-value: 0, init-keyword: minor-version:;
  runtime-constant &slot library-build-count :: <integer>,
    init-value: 0, init-keyword: build-count:;
end &class;

define sealed concrete primary &class <library> (<namespace>, <library-version>)
  sealed &slot used-libraries :: <simple-object-vector>,
    init-value: #[],
    init-keyword: used-libraries:;
  sealed &slot all-used-libraries :: <simple-object-vector>,
    init-value: #[],
    init-keyword: all-used-libraries:;
  sealed &slot runtime-module, init-value: #f;
  // Following slot only used in the runtime, constructed
  weak &slot library-defined-generics :: <simple-object-vector>,
    reinit-expression: #[],
    init-value: #[];
  &slot library-number-static-dispatches :: <integer>,
    init-value: 0;
  &slot library-number-dynamic-dispatches :: <integer>,
    init-value: 0;
  slot ^library-description,
    init-keyword: library-description:;
  weak slot library-accumulating-defined-generics :: <stretchy-object-vector>,
    reinit-expression: make(<stretchy-object-vector>),
    init-value: make(<stretchy-object-vector>);
end &class;

define sealed concrete primary &class <used-library> (<library-version>)
  sealed &slot used-library :: <library>,
    required-init-keyword: used-library:;
  sealed &slot used-library-binding :: <symbol>,
    required-init-keyword: binding:;
end &class;

define method ^initialize
    (x :: <&used-library>, #rest all-keys, #key used-library :: <&library>, #all-keys)
  next-method();
  ^library-major-version(x) := ^library-major-version(used-library);
  ^library-minor-version(x) := ^library-minor-version(used-library);
  ^library-build-count(x)   := ^library-build-count(used-library);
end method;

define sealed concrete primary &class <module> (<namespace>)
  constant &slot home-library :: <library>, required-init-keyword: home:;
end &class;

define compiler-sideways method ^make-<&library> (name) => (model :: <&library>)
  ^make(<&library>, name: as-lowercase(as(<string>, name)))
end method;

define compiler-sideways method ^make-<&module> (name, home) => (model :: <&module>)
  ^make(<&module>, name: as-lowercase(as(<string>, name)), home: home)
end method;

// The module model is used for two things:
// (1) by variable-search for mangled variable lookups
// (2) by runtime sealing etc. support, for looking up the library.
//
// For the first purpose, it needs to use the home module of the binding,
// since that's what's used for mangled names.  For the second purpose
// it needs to be sure to get a module in the library the form is defined in.
// These two are the same except for modifying forms, where the form could
// be in a higher level library from the variable it's modifying.
// Fortunately variable search doesn't deal with any modifying objects
// (methods/domains) for now.

define method form-module-model
    (form :: <variable-defining-form>) => (module :: <&module>)
  let binding = form-variable-binding(form);
  namespace-model(binding-home(binding));
end;

define method form-module-model
    (form :: <modifying-form>) => (module :: <&module>)
  namespace-model(compilation-record-module(form-compilation-record(form)))
end;

define function model-module-model (model) => (module :: <&module>)
  form-module-model(model-definition(model));
end;


define sealed concrete &class <unbound> (<object>) end;

define constant &unbound = $unbound;

// These generics get around some forward-reference problems due to
// the emulator not understanding repeated slots.

define generic ^mm-wrapper-number-patterns (object);
define generic ^mm-wrapper-pattern-element (object, index);
define generic ^mm-wrapper-pattern-element-setter (value, object, index);


define sealed primary &class <mm-wrapper> (<object>)
  &slot mm-wrapper-implementation-class :: <implementation-class>,
     required-init-keyword: implementation-class:;
  // Mask of all inherited subtype bits.
  &slot mm-wrapper-subtype-mask :: <integer>, init-value: 0,
     init-keyword: subtype-mask:;
  /* raw */ &slot mm-wrapper-fixed-part :: <raw-machine-word>,
     init-keyword: fixed-part:;
  /* raw */ &slot mm-wrapper-variable-part :: <raw-machine-word>,
     init-keyword: variable-part:;
  /* raw */ weak constant repeated &slot mm-wrapper-pattern-element :: <raw-machine-word>,
    required-init-keyword: fill:,
    size-getter: mm-wrapper-number-patterns,
    size-init-keyword: number-patterns:,
    size-init-value: 0;
  constant slot %mm-wrapper-patterns,
     init-keyword: patterns:;
end &class;

// HACK: shouldn't need the following if mop were complete

define method ^mm-wrapper-number-patterns (object :: <&mm-wrapper>)
  size(%mm-wrapper-patterns(object))
end method;

// TODO: The size-getter: option isn't spotted as an accessor name by the
// define &class macro, so have to hook up the compile-stage function
// by hand. Fix!

define &override-function ^mm-wrapper-number-patterns end;

define method ^mm-wrapper-pattern-element (object :: <&mm-wrapper>, index)
  element(%mm-wrapper-patterns(object), index)
end method;

define method ^mm-wrapper-pattern-element-setter
    (value, object :: <&mm-wrapper>, index)
  element(%mm-wrapper-patterns(object), index) := value
end method;

define method model-<mm-wrapper> ()
  dylan-value(#"<mm-wrapper>")
end method;

//// Booted collections.

// The class <object-with-elements> in an invented common superclass
// of collections and any other object to which element/element-setter
// is applicable. It's here for the sake of the FFI so that pointers
// can have element/element-setter defined for them.

define open abstract &class <object-with-elements> (<object>) end;
define open compiler-open abstract &class <mutable-object-with-elements>
    (<object-with-elements>)
end &class;
define open   abstract &class <collection> (<object-with-elements>) end;
define open   abstract &class <mutable-collection>
    (<collection>, <mutable-object-with-elements>)
end &class;
define open abstract &class <explicit-key-collection> (<collection>)
end &class;
define open abstract &class <mutable-explicit-key-collection>
    (<explicit-key-collection>, <mutable-collection>)
end &class;
define open   abstract &class <sequence> (<collection>) end;
define open   abstract &class <mutable-sequence>
    (<mutable-collection>, <sequence>)
end &class;

define sealed abstract primary &class <list> (<mutable-sequence>)
  inline &slot head, init-value: #f,  init-keyword: head:;
  inline &slot tail, init-value: #(), init-keyword: tail:;
end &class <list>;

define sealed concrete made-inline &class <pair> (<list>)
  inherited &slot tail, init-value: #f, init-keyword: tail:;
end &class <pair>;

define sealed concrete &class <empty-list> (<list>)
  inherited &slot head, init-value: #(), init-keyword: head:;
end &class <empty-list>;

define open abstract primary &class <limited-collection> (<collection>)
  constant &slot element-type :: <type>,
    init-keyword: element-type:,
    init-value:   <object>;
end &class;

define open   abstract &class <array> (<mutable-sequence>) end;
define open   abstract &class <vector> (<array>) end;

// These generics get around some forward-reference problems due to
// the emulator not understanding repeated slots.

define generic ^vector-element (object, index);
define generic ^vector-element-setter (value, object, index);

define open abstract /* primary */ &class <simple-vector> (<vector>)
end &class <simple-vector>;

define sealed concrete primary &class <simple-object-vector> (<simple-vector>)
  repeated &slot vector-element,
    init-keyword: fill:,
    init-value: #f,
    size-getter: size,
    size-init-keyword: size:,
    size-init-value: 0;
end &class <simple-object-vector>;

// HACK: SHOULDN'T GENERATE THESE IN THE FIRST PLACE
ignore(^vector-element-values); ignore(^vector-element-values-setter);

define open abstract &class <string> (<mutable-sequence>) end;

define generic ^string-element (object, index);

define primary &class <byte-string> (<string>, <vector>)
  compiler-constant repeated &slot string-element :: <byte-character>,
    init-value: ' ',
    init-keyword: fill:,
    size-getter: size,
    size-init-keyword: size:,
    size-init-value: 0;
end &class <byte-string>;

// HACK: SHOULDN'T GENERATE THESE IN THE FIRST PLACE
ignore(^string-element-values); ignore(^string-element-setter);

// Built-in collection functions

define generic ^empty? (object :: <object>) => (result :: <boolean>);
define generic ^size (object :: <object>) => (result /* :: <integer> */);
define generic ^element (collection, key, #key default) => (object);

define &override-function ^empty? end;
define &override-function ^size end;
define &override-function ^element end;

define method ^empty? (object :: <object>) => (result :: <boolean>);
  error("NO APPLICABLE EMPTY? METHOD");
end method;

define method ^size (object :: <object>) => (result :: <integer>);
  error("NO APPLICABLE SIZE METHOD");
end method;

define method ^element (collection, key, #key default) => (object)
  error("NO APPLICABLE ELEMENT METHOD");
end method;


define macro compile-time-collection-functions-for-definer
  { define compile-time-collection-functions-for ?:name }
    => { define method ^empty? (collection :: ?name)
             => (result :: <boolean>);
           collection.empty?
         end method;


         define method ^element (collection :: ?name,
                        key :: <integer>, #key default = unsupplied())
             => (object)
           if (supplied?(default))
             element(collection, key, default: default)
           else
             collection[key]
           end
         end method
       }
end;

define compile-time-collection-functions-for <simple-object-vector>;
define compile-time-collection-functions-for <list>;
define compile-time-collection-functions-for <byte-string>;

define method ^size (collection :: <list>)
 => (result :: <integer>);
  collection.size
end method;


//// Booted metaobjects.

define generic ^instance?-iep (type);
define generic ^instance?-iep-setter (value, type);

define sealed abstract primary &class <type> (<object>)
  &computed-slot instance?-iep /* :: <raw-pointer> */, init-value: #f;
//   &slot instance?-iep;
  constant slot ^instance?-function, init-value: #"uninitialized-instance?-function";
end;

define compiler-open generic ^instance?-function (function);

define method ^instance?-iep (t :: <&type>)
  %instance?-iep(t) | (%instance?-iep(t) := ^iep(dylan-value(^instance?-function(t))))
end method;

define method ^instance?-iep-setter (v, t :: <&type>) %instance?-iep(t) := v end;

define generic ^direct-subclasses (cls);
define generic ^direct-subclasses-setter (value, cls);

define generic ^class-constructor (class) => (constructor);
define generic ^class-constructor-setter (constructor, class);

define concrete primary &class <implementation-class> (<object>)

  //// Run-time & compile-time slots.

  // Assorted class properties, packed. See below.
  &slot class-properties :: <integer>,
    init-value: 0;

  // Pointer back to the class object.
  &slot iclass-class :: <class>,
    required-init-keyword: class:;

  // Traceable pointer back to mm-wrapper.
  lazy &slot class-mm-wrapper, // FIX MOMACS: :: false-or(<mm-wrapper>),
    init-value: #f;

  lazy &slot repeated-slot-descriptor, // FIX MOMACS: :: false-or(<repeated-slot-descriptor>),
    init-value: #f;
  lazy &slot instance-slot-descriptors :: <simple-object-vector>,
    init-value: #[];

  &slot iclass-dispatch-key :: <integer>,
    init-value: -1;

  //// **** Slots before this point may be known about in a non-modular ****
  //// ****           fashion by runtime and debugger code.             ****

  //// More slots that are always required in full.

  &computed-slot-no-default class-constructor :: <method>,
    init-keyword: constructor:,
    init-value: default-class-constructor;

  lazy &slot direct-superclasses :: <simple-object-vector>,
    init-keyword: superclasses:;

  // RCPL fast subclass slots, markt, 2-Apr-97

  // The reversed CPL as s-o-v
  lazy &slot class-rcpl-vector :: <simple-object-vector>,
    init-value: #[];
  // The first (hopefully only) RCPL position.
  &slot class-rcpl-position :: <integer>,
    init-value: 0;
  lazy &slot class-rcpl-other-positions :: <simple-object-vector>,
    init-value: #[];

  // Slots for tracking run-time changes to the class hierarchy and general
  // book keeping required to maintain correctness of the compile-time type
  // informed partial dispatch optimizations.

  lazy &slot class-known-joint :: <simple-object-vector>,
    init-value:   #[],
    init-keyword: known-joint:;
  lazy &slot iclass-dependent-generics :: <list>,
    init-value:   #(),
    init-keyword: dependent-generics:;
  lazy &slot iclass-subclass-dependent-generics :: <list>,
    init-value:   #(),
    init-keyword: subclass-dependent-generics:;

  lazy &slot direct-subclasses :: <list>,
    init-value: #();
  lazy &slot direct-methods :: <simple-object-vector>,
    init-value: #[];

  //// Slots that may often be defaulted.

  lazy &slot direct-slot-descriptors :: <simple-object-vector>,
    init-value: #[];
  lazy &slot slot-descriptors :: <simple-object-vector>,
    init-value: #[];
  lazy &slot direct-inherited-slot-descriptors :: <simple-object-vector>,
    init-value: #[];
  lazy &slot direct-initialization-argument-descriptors
               :: <simple-object-vector>,
    init-value: #[];

  lazy &slot class-slot-descriptors :: <simple-object-vector>,
    init-value: #[];

  lazy &slot defaulted-initialization-arguments-slot,
    init-value: 0;

  &slot class-slot-storage :: <simple-object-vector>,
    init-value: #[];

  // Place holders for Caseau gene stuff. In time may implement this for
  // comparison with RCPL.

  // A bit vector?
  // &slot class-Caseau-gene-set;
  // The gene used here (if primary class)
  // &slot class-Caseau-gene :: <integer>;

  //// Optional, for the collection of statistics only.

  // Uncomment the following and comment out the stub definitions in the
  // Dylan library in order to re-enable instance test counting.

  // &slot class-instance?-count :: <integer>, init-value: 0;
  // The constructor method.

  //// Compile-time only slots.

  // Slot tracking the state of lazy compile-time slot initialization.
  lazy slot ^slots-initialized-state :: <symbol>, init-value: #"uninitialized";

  // Slot for incremental building of RCPL position lists.
  lazy slot ^class-incremental-rcpl-positions :: <list> = #();

  lazy slot ^all-superclasses :: <list>,
    init-value: #();

end &class <implementation-class>;

define constant $max-class-log-size = 16;

define leaf packed-slots ^class-properties (<&implementation-class>, <object>)
  field   slot ^instance-storage-size               = 0,
    field-size:   $max-class-log-size;
  boolean slot ^class-abstract?                     = #f,
    init-keyword: abstract?:;
  boolean slot ^class-primary?                      = #f,
    init-keyword: primary?:;
  boolean slot ^class-sealed?                       = #f,
    init-keyword: sealed?:;
  boolean slot ^iclass-type-complete?               = #t, // set to #f if only hollow
    init-keyword: type-complete?:;
  boolean slot ^class-complete?                     = #f,
    init-keyword: complete?:;
  boolean slot ^class-incremental?                  = #f, // set to #t for loose mode
    init-keyword: incremental?:;
  boolean slot ^slots-have-fixed-offsets?-bit       = #f,
    init-keyword: slots-have-fixed-offsets?:;
  boolean slot ^slots-have-fixed-offsets?-computed? = #f,
    init-keyword: slots-have-fixed-offsets?-computed?:;
  boolean slot ^iclass-instantiable?                = #f, // set to #t if ~abstract & ~raw & complete
    init-keyword: instantiable?:;
  boolean slot ^iclass-subclasses-fixed?            = #f,
    init-keyword: subclasses-fixed?:;
  // COMPILE TIME SLOTS
  boolean slot %direct-subclasses-initialized?      = #f;
end packed-slots;

define method ^initialize
    (x :: <&implementation-class>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, x, all-keys);
end method;

define runtime-slot-offset class-mm-wrapper (<implementation-class>);

define runtime-slot-offset class-constructor (<implementation-class>);

define function ^iclass-number-to-key (n :: <integer>)
  ash(n, 1) + 1000
end function;

define macro iclass-transfer-definer
  { define iclass-transfer ?something:name ?slotname:name ; }
    =>
    { define iclass-transfer ?something ?slotname (<object>) ; }

  { define iclass-transfer slot ?slotname:name (?type:*) ; }
    =>
    { define iclass-transfer-compiletime-slot ?slotname ,  ?slotname (?type) ; }

  { define iclass-transfer &computed-slot ?slotname:name (?type:*) ; }
    =>
    { define iclass-transfer-compiletime-slot "%" ## ?slotname , "^" ## ?slotname ( ?type ) ;
      define iclass-transfer-runtime-slot ?slotname ( ?type ) ;
      }

  { define iclass-transfer &slot ?slotname:name (?type:*) ; }
    =>
    { define iclass-transfer-compiletime-slot "^" ## ?slotname, "^" ## ?slotname (?type) ;
      define iclass-transfer-runtime-slot ?slotname (?type ) ;
     }
end macro;

define macro iclass-transfer-compiletime-slot-definer
  { define iclass-transfer-compiletime-slot ?slotname:name , ?otherslotname:name ( ?type:* ) ; }
    =>
    { define inline method ?slotname (c :: <&class>) => (v :: ?type)
        ?otherslotname (^class-implementation-class(c))
      end method;
      define inline method ?slotname ## "-setter" (v :: ?type, c :: <&class>)
          ?otherslotname ## "-setter"(v, ^class-implementation-class(c))
      end method;
    }
type:
  { false-at-compile-time-or(?type) }
    => { false-or(?type) }
  { <object> }
    => { <model-value> }
  { <simple-object-vector> }
    => { <simple-object-vector> }
  { <integer> }
    => { <integer> }
  { <single-float> }
    => { <single-float> }
  { <byte-string> }
    => { <byte-string> }
  { <byte-character> }
    => { <byte-character> }
  { <boolean> }
    => { <boolean> }
  { <symbol> }
    => { <symbol> }
  { <list> }
    => { <list> }
  { "<" ## ?:name ## ">" }
    => { "<&" ## ?name ## ">" }
end macro;


define macro iclass-transfer-runtime-slot-definer
  { define iclass-transfer-runtime-slot ?slotname:name ( ?type:* ) ; }
   =>
    { do-define-evaluator-override(?#"slotname", "^" ## ?slotname);
      do-define-evaluator-override(?#"slotname" ## "-setter", "^" ## ?slotname ## "-setter");
      define function "source-constructor-for-iclass-transfer-" ## ?slotname ()
          #{ define inline method ?slotname (c :: <class>) => (v :: ?type)
               ?slotname(class-implementation-class(c))
             end method;
             define inline method ?slotname ## "-setter" (v :: ?type, c :: <class>)
               ?slotname ## "-setter" (v, class-implementation-class(c))
             end method
           }
      end function;
      do-define-core-unadorned-definition
        (?#"slotname", "source-constructor-for-iclass-transfer-" ## ?slotname)
     }
end macro;


define generic ^class-module (x) => (module);
define generic ^class-module-setter (v, x) => (module);

define compiler-class-open primary &class <class> (<type>)
  // PUBLIC
  lazy &slot debug-name :: <object>,
    init-keyword: debug-name:, init-value: #f;
  &slot class-implementation-class :: <implementation-class>;
  // This is a mask (not bit index) of at most one bit.
  &slot class-subtype-bit :: <integer>, init-value: 0, init-keyword: subtype-bit-mask:;
  &computed-slot-no-default class-module :: <module>,
    init-keyword: module:,
    init-value: $runtime-module;
end &class;

define runtime-slot-offset class-implementation-class (<class>);

define method ^class-module (c :: <&class>) => (module)
  %class-module(c)
end method;

define method ^class-module-setter (v, c :: <&class>) => (value)
  %class-module(c) := v
end method;

define compiler-open generic ^initialize-class
  (x :: <&class>, #rest args, #key, #all-keys);

define method ^initialize
    (x :: <&class>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(^initialize-class, x, all-keys)
end method;

define iclass-transfer &slot direct-superclasses (<simple-object-vector>);
define iclass-transfer &slot all-superclasses (<list>);

define iclass-transfer &computed-slot direct-subclasses (<list>);

define iclass-transfer &slot class-mm-wrapper (/* NOMACS: false-or(<mm-wrapper>) */ <object>);
define iclass-transfer &slot direct-slot-descriptors (<simple-object-vector>);
define iclass-transfer &slot slot-descriptors (<simple-object-vector>);
define iclass-transfer &slot direct-inherited-slot-descriptors (<simple-object-vector>);
define iclass-transfer &slot direct-initialization-argument-descriptors (<simple-object-vector>);
define iclass-transfer &slot direct-methods (<simple-object-vector>);
define iclass-transfer &slot class-abstract? (<boolean>);
define iclass-transfer &slot class-primary? (<boolean>);
define iclass-transfer &slot class-sealed? (<boolean>);
// define iclass-transfer &slot class-type-complete? (<boolean>);
define iclass-transfer &slot class-complete? (<boolean>);
define iclass-transfer &slot class-incremental? (<boolean>);
// define iclass-transfer &slot class-instantiable? (<boolean>);
// define iclass-transfer &slot class-subclasses-fixed? (<boolean>);

define iclass-transfer &slot instance-storage-size (<integer>);
define iclass-transfer &slot repeated-slot-descriptor (/* NOMACS: false-or(<repeated-slot-descriptor>) */ <object>);
define iclass-transfer &slot instance-slot-descriptors (<simple-object-vector>);
define iclass-transfer &slot class-slot-descriptors (<simple-object-vector>);
define iclass-transfer &slot defaulted-initialization-arguments-slot;
define iclass-transfer &slot class-slot-storage (<simple-object-vector>);
// define iclass-transfer &slot class-instance?-count (<integer>);
define iclass-transfer &slot class-constructor (<object>); // <method>
define iclass-transfer &slot class-rcpl-vector (<simple-object-vector>);
define iclass-transfer &slot class-rcpl-position (<integer>);
define iclass-transfer &slot class-rcpl-other-positions (<simple-object-vector>);
define iclass-transfer &slot class-known-joint (<simple-object-vector>);

//  &slot class-Caseau-gene-set;                          // A bit vector?
//  &slot class-Caseau-gene :: <integer>;                 // The gene used here (if primary class)

// compile-time slots

define iclass-transfer slot ^slots-initialized-state (<symbol>);
define iclass-transfer slot %direct-subclasses-initialized? (<boolean>);
define iclass-transfer slot ^slots-have-fixed-offsets?-bit (<boolean>);
define iclass-transfer slot ^slots-have-fixed-offsets?-computed? (<boolean>);
define iclass-transfer slot ^class-incremental-rcpl-positions (<list>);

define compiler-open generic ^debug-name (function);
define compiler-open generic ^debug-name-setter (value, function);

define primary &class <value-class> (<class>)
end &class;

define &class <function-class> (<class>) end;

/// hack for use by &virtual-class-definer
define class <&virtual-object> (<virtual-object>)
end;

define &virtual-class <virtual-class> (<class>)
end;

define &virtual-class <top-type> (<type>)
end;

// <bottom> exists in the runtime, for function result introspection
define &class <bottom-type> (<type>)
end;

define &virtual-class <raw-type> (<type>)
  slot ^debug-name,
    init-keyword: debug-name:;
  constant slot ^raw-type-supertype,
    required-init-keyword: supertype:;
  constant slot raw-type-descriptor-function :: <function>,
    required-init-keyword: descriptor-function:;
end;

define &virtual-class <raw-aggregate-type> (<type>)
  lazy slot ^debug-name,
    init-keyword: debug-name:;
  lazy constant slot raw-aggregate-members, required-init-keyword: members:;
  lazy constant slot raw-aggregate-options, required-init-keyword: options:;
end;

define &virtual-subclass <raw-struct-type> (<raw-aggregate-type>)
end;

define &virtual-subclass <raw-union-type> (<raw-aggregate-type>)
end;


define abstract primary &class <slot-initial-value-descriptor> (<object>)
  // PRIVATE
  &slot slot-descriptor-properties :: <integer>, init-value: 0;
  &slot init-data-slot, init-keyword: init-data:, init-value: #f;
  constant &slot slot-owner :: <class>, required-init-keyword: owner:;
end &class <slot-initial-value-descriptor>;

define method ^initialize
    (x :: <&slot-initial-value-descriptor>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, x, all-keys)
end method;

define packed-slots ^slot-descriptor-properties
    (<&slot-initial-value-descriptor>, <object>)
  boolean  slot ^init-supplied?  = #f, init-keyword: init-supplied?:;
  boolean  slot ^init-value?     = #f, init-keyword: init-value?:;
  boolean  slot ^init-evaluated? = #f, init-keyword: init-evaluated?:;

  // compile-time slot

  boolean  slot slot-value-runtime-only? = #f, init-keyword: runtime-only?:;
end packed-slots;

ignore(slot-value-runtime-only?);

define abstract primary &class <slot-keyword-initialization-descriptor>
  (<slot-initial-value-descriptor>)
  &slot init-keyword, init-keyword: init-keyword:, init-value: #f;
end &class <slot-keyword-initialization-descriptor>;

define packed-slots ^slot-descriptor-properties
    (<&slot-keyword-initialization-descriptor>, <&slot-initial-value-descriptor>)
  boolean  slot ^init-keyword-required? = #f, init-keyword: init-keyword-required?:;
end packed-slots;

define primary &class <slot-descriptor> (<slot-keyword-initialization-descriptor>)
  constant &slot slot-getter, init-keyword: getter:, init-value: #f;
  constant &slot slot-setter, init-keyword: setter:, init-value: #f;
  runtime-constant &slot slot-type :: <type>, init-value: <object>, init-keyword: type:;

  // Compile-time only. These slots hold the functions in the compiler for
  // accessing the slot value from a directly modeled object. These
  // functions are recorded at boot time and installed when the source for
  // the modeled class is compiled.

  slot emitted-type-name = #f;

  // TODO: Should these actually be compile-stage overrides on the
  // accessor functions themselves? That would be a more general
  // purpose mechanism.

  weak slot model-object-getter = #f,
    reinit-expression:
      with-dependent-context ($compilation of model-definition(self))
        compute-compile-stage-getter(self)
      end;
  weak slot model-object-setter = #f,
    reinit-expression:
      with-dependent-context ($compilation of model-definition(self))
        compute-compile-stage-setter(self)
      end;
end &class <slot-descriptor>;

define function compute-compile-stage-getter (slot :: <&slot-descriptor>)
  let getter-object = ^slot-getter(slot);
  lookup-compile-stage-function(getter-object)
end;

define function compute-compile-stage-setter (slot :: <&slot-descriptor>)
  let setter-object = ^slot-setter(slot);
  if (setter-object)
    lookup-compile-stage-function(setter-object);
  else
    // constant at runtime, but maybe has compile-time setter
    let getter-object = ^slot-getter(slot);
    // TODO: this is a kludge!!! Need to record the actual getter -> override
    // mapping.
    let getter-var = model-variable-binding(getter-object);
    let module = getter-var.binding-home;
    if (booted-module?(module))
      let name = as(<symbol>,
                    concatenate(as(<string>, getter-var.binding-identifier),
                                "-setter"));
      lookup-compile-stage-function(untracked-lookup-binding-in(module, name))
    end;
  end;
end function;

/*
define inline method model-object-getter
    (x :: <&slot-descriptor>) => (res :: false-or(<function>))
  lookup-compile-stage-function(^slot-getter(x))
end method;

define inline method model-object-setter
    (x :: <&slot-descriptor>) => (res :: false-or(<function>))
  lookup-compile-stage-function(^slot-setter(x))
end method;
*/

define compiler-open generic ^initialize-slot-descriptor
  (x :: <&slot-descriptor>, #rest args, #key, #all-keys);

define method ^initialize
    (x :: <&slot-descriptor>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(^initialize-slot-descriptor, x, all-keys)
end method;

define leaf packed-slots ^slot-descriptor-properties
    (<&slot-descriptor>, <&slot-keyword-initialization-descriptor>)
  field slot ^slot-storage-size = 1, field-size: 8,
    init-keyword: storage-size:;
end packed-slots;

ignore(^slot-storage-size);

define method ^debug-name (slotd :: <&slot-descriptor>)
  let slot-getter = slotd.^slot-getter;
  slot-getter & slot-getter.^debug-name
end method;

define &class <any-instance-slot-descriptor> (<slot-descriptor>)
end &class <any-instance-slot-descriptor>;

define &class <instance-slot-descriptor> (<any-instance-slot-descriptor>)
end &class <instance-slot-descriptor>;

define &class <virtual-slot-descriptor> (<slot-descriptor>)
end &class <virtual-slot-descriptor>;

define primary &class <repeated-slot-descriptor> (<any-instance-slot-descriptor>)
  runtime-constant &slot size-slot-descriptor;
end &class <repeated-slot-descriptor>;

define &class <any-class-slot-descriptor> (<slot-descriptor>)
end &class <any-class-slot-descriptor>;

define &class <class-slot-descriptor> (<any-class-slot-descriptor>)
end &class <class-slot-descriptor>;

define &class <each-subclass-slot-descriptor> (<any-class-slot-descriptor>)
end &class <each-subclass-slot-descriptor>;

define primary &class <init-arg-descriptor> (<slot-keyword-initialization-descriptor>)
  runtime-constant &slot init-arg-type :: <type>, init-value: <object>, init-keyword: type:;
end &class <init-arg-descriptor>;

define primary &class <inherited-slot-descriptor> (<slot-initial-value-descriptor>)
  constant &slot inherited-slot-getter, required-init-keyword: getter:;
end &class <inherited-slot-descriptor>;

define method ^slot-value
    (object, slot-descriptor :: <&slot-descriptor>) => (value)
  let getter = model-object-getter(slot-descriptor);
  if (getter)
    /*
    if (slot-initialized?(object, getter))
      getter(object)
    else
      &unbound
    end;
    block ()
      getter(object)
    exception (<error>)
      &unbound
    end;
    */
    getter(object)
  else
    error("Can't compute the slot-value for a non-booted slot yet - %=.",
          slot-descriptor);
  end;
end method;

define method ^slot-value-setter
    (new-value, object, slotd :: <&slot-descriptor>) => (value)
  slotd.model-object-setter(new-value, object);
end method;

/*
define method ^repeated-slot-value
    (o, slotd :: <&repeated-slot-descriptor>, i) => (value)
  slotd.model-object-getter(o, i);
end method;
*/

define method ^repeated-slot-value
    (object, descriptor :: <&repeated-slot-descriptor>,
       offset :: <integer>)
  let element-getter = model-object-getter(descriptor);
  // let size-getter = model-object-getter(descriptor.^size-slot-descriptor);
  if (element-getter)
    element-getter(object, offset);
  else
    error("Can't access the repeated slot of a non-booted class.");
  end;
end method ^repeated-slot-value;

define method ^repeated-slot-value-setter
    (new-value, o, slotd :: <&repeated-slot-descriptor>, i) => (value)
  slotd.model-object-setter(new-value, o, i);
end method;

define class <unknown> (<object>) end;

// define &class <dummy> (<sequence>)
//   &slot size;
// end &class;

//// Run-stage/compile-stage mappings.

define method make-compile-time-literal (object :: <object>)
  object
end method;

define method direct-object? (object) #f end;

define ^mapping <unbound> => <mapped-unbound>
  &instance %unbound => &unbound;
end ^mapping;

define ^mapping <integer> => <integer>
end ^mapping;

/// PROXIES

/// UNBOUND PROXY

define class <dood-unbound-proxy> (<dood-proxy>)
end class;

define sealed domain make (subclass(<dood-unbound-proxy>));
define sealed domain initialize (<dood-unbound-proxy>);

define method dood-make-unbound-proxy
    (dood :: <dood>, object :: <mapped-unbound>) => (proxy)
  make(<dood-unbound-proxy>)
end method;

define sideways method dood-disk-object
    (dood :: <dood>, object :: <mapped-unbound>)
 => (proxy :: <dood-unbound-proxy>)
  dood-as-proxy(dood, object, dood-make-unbound-proxy)
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-unbound-proxy>) => (object)
  $unbound
end method;

/// MAKE PROTOCOL

// define generic ^make (type, #key, #all-keys);

// define method ^make (type, #rest initargs, #key, #all-keys)
//   let model = apply(make, type, initargs);
//   apply(^initialize, model, initargs);
//   model
// end method;

// define compiler-open generic ^initialize (object, #key, #all-keys);

// define method ^initialize (object, #key)
// end method;


define generic ^as (class, object) => (coerced-object);

define &override-function ^as end;

define method ^as (type, object) => (object)
  if (^instance?(object, type))
    object
  else
    error("NO APPLICABLE AS METHOD");
  end if;
end method;

// TODO: PERFORMANCE: Floats aren't currently mapped. We might choose to
// map their raw form, or it might simply not be worth it. Note that
// their are technical obstacles to mapping anything which isn't
// either interned or immediate.

define method make-compile-time-literal (object :: <single-float>)
  ^make(<&single-float>, data: make(<&raw-single-float>, value: object));
end method;

define method make-compile-time-literal (object :: <&single-float>)
  if (model-compilation-record(object) ~== current-compilation-record())
    make(<&single-float>, data: ^%single-float-data(object))
  else
    object
  end if
end method;

define method make-compile-time-literal (object :: <double-float>)
  ^make(<&double-float>, data: make(<&raw-double-float>, value: object));
end method;

define method make-compile-time-literal (object :: <&double-float>)
  if (model-compilation-record(object) ~== current-compilation-record())
    make(<&double-float>, data: ^%double-float-data(object))
  else
    object
  end if
end method;

define method make-compile-time-literal (object :: <machine-word>)
  ^make(<&machine-word>, data: make(<&raw-machine-word>, value: object));
end method;

define method make-compile-time-literal (object :: <&machine-word>)
  if (model-compilation-record(object) ~== current-compilation-record())
    make(<&machine-word>, data: ^%machine-word-data(object))
  else
    object
  end if
end method;

/// HACK: MUST COME BEFORE NEXT METHOD (WHICH ISN'T EVEN NEC W/O EMULATOR

define method make-compile-time-literal (object :: <double-integer>)
  let di = ^make(<&double-integer>);
  ^%%double-integer-low(di)
    := make(<&raw-machine-word>, value: %double-integer-low(object));
  ^%%double-integer-high(di)
    := make(<&raw-machine-word>, value: %double-integer-high(object));
  di
end method;

define method make-compile-time-literal (object :: <integer>)
  object
end method;

/*
define method make-compile-time-literal (object :: <extended-float>)
  ^make(<&extended-float>, data: make(<&raw-extended-float>, value: object));
end method;

define ^mapping <single-float> => <single-float>
end ^mapping;

define ^mapping <double-float> => <double-float>
end ^mapping;

define ^mapping <extended-float> => <extended-float>
end ^mapping;
*/

///---*** NOTE: Was <integer> before renaming, meaning <abstract-integer>
///---*** Should it be <abstract-integer> now or was that wrong?!?!?
define method direct-object? (o :: <integer>) #t end;

define ^mapping <byte-character> => <byte-character>
end ^mapping;

define method direct-object? (o :: <byte-character>) #t end;

define ^mapping <boolean> => <boolean>
  &instance %true  => #t;
  &instance %false => #f;
end ^mapping;

define method symbol-name (symbol)
  as(<string>, symbol)
end method;

define ^mapping <symbol> => <symbol>
  constant &slot symbol-name => symbol-name;
end ^mapping;

define method ^symbol? (object) #f end;
define method ^symbol? (object :: <symbol>) #t end;

// Uninterned symbols are a hack to allow us to generate distinct
// instances of symbol models so that independent model versions can
// be emitted. Dylan doesn't support uninterned symbols, hence the
// following.


define class <uninterned-symbol> (<model-properties>)
  slot symbol-name,
    required-init-keyword: name:;
end class;

define method deep-copy-symbol (object :: <symbol>)
  make(<uninterned-symbol>,
       name: mapped-model(as-lowercase!(shallow-copy(as(<string>, object)))));
end method;

define ^mapping <symbol> => <uninterned-symbol>
  &slot symbol-name => symbol-name;
end ^mapping;

define ^mapping <empty-list> => <empty-list>
  &instance %empty-list => #();
  &slot head => head;
  &slot tail => tail;
end ^mapping;

define ^mapping <pair> => <pair>
  &slot head => head;
  &slot tail => tail;
end ^mapping;

define method make-compile-time-literal (object :: <pair>)
  pair(make-compile-time-literal(object.head),
       make-compile-time-literal(object.tail))
end method;

define constant &empty-simple-object-vector = #[];

define ^mapping <simple-object-vector> => <simple-object-vector>
  &instance %empty-vector       => &empty-simple-object-vector;
  constant &slot size           => size;
  repeated &slot vector-element => element;
end ^mapping;

define method make-compile-time-literal (object :: <simple-object-vector>)
  map-as(<simple-object-vector>, make-compile-time-literal, object)
end method;

define constant &empty-byte-string = "";

define ^mapping <byte-string> => <byte-string>
  &instance %empty-string       => &empty-byte-string;
  constant &slot size                    => size;
  repeated &slot string-element => element;
end ^mapping;

define method make-compile-time-literal (object :: <byte-string>)
  copy-sequence(object)
end method;

// Map to the canonical empty strings and vectors in the run-time.

define compiler-sideways method standard-model-object
    (object :: <simple-object-vector>) => (standard :: <simple-object-vector>)
  if (empty?(object))
    &empty-simple-object-vector
  else
    object
  end
end method;

define compiler-sideways method standard-model-object
    (object :: <byte-string>) => (standard :: <byte-string>)
  if (empty?(object))
    &empty-byte-string
  else
    object
  end
end method;

//// Comparison.

define method ^id? (x, y)
  x == y
end method;


define method ^debug-name (object) #f end; // !@#$ PATCH



// We might have different types of obsolete instances for the purpose of
// supporting special funny things like obsolete functions.
define abstract &class <obsolete-instance> (<object>)
end &class;

define primary &class <miscellaneous-obsolete-instance> (<object>)
end &class;



////
//// PROXIES
////

/// MODELS

define class <dood-cross-model-proxy> (<dood-binding-value-proxy>)
end class;

define compiler-sideways method dood-disk-object
    (dood :: <dood>, object :: <model-properties>) => (proxy)
  // format-out("object %= %=\n", object, model-library(object));
  let ld = dood-root(dood);
  if (ld == model-library(object))
    next-method();
  else
    dood-as-proxy(dood, object, dood-make-binding-value-proxy)
  end if
end method;

/// CROSS-MODEL-PROXY

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-model-proxy>) => (object)
  with-dood-context (dood-root(dood))
    let binding = dood-proxy-binding(proxy);
    // with-dood-context (namespace-library-description(binding.binding-home))
      let object = untracked-binding-model-object-if-computed(binding);
      if (instance?(object, <dood-cross-model-proxy>))
        break("CIRCULARITY %=", proxy);
      end if;
      object
    // end with-dood-context;
  end with-dood-context;
end method;

/// SLOT-DESCRIPTORS

define class <dood-cross-model-slot-descriptor-proxy> (<dood-cross-model-proxy>)
  constant slot proxy-slot-getter, required-init-keyword: slot-getter:;
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&slot-descriptor>) => (proxy)
  make(<dood-cross-model-slot-descriptor-proxy>,
       binding: model-variable-binding(^slot-owner(object)),
       slot-getter: ^slot-getter(object))
end method;

define compiler-open generic ^slot-descriptor (class, accessor :: <&function>);

define compiler-open generic ^slot-offset
    (slot-descriptor :: <&slot-descriptor>, class :: <&class>);

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-model-slot-descriptor-proxy>) => (object)
  // HACK: TUNE THIS
  ^slot-descriptor(next-method(), proxy-slot-getter(proxy))
end method;

define class <dood-cross-model-inherited-slot-descriptor-proxy>
    (<dood-cross-model-slot-descriptor-proxy>)
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&inherited-slot-descriptor>) => (proxy)
  make(<dood-cross-model-inherited-slot-descriptor-proxy>,
       binding: model-variable-binding(^slot-owner(object)),
       slot-getter: ^inherited-slot-getter(object))
end method;

define class <dood-cross-model-init-arg-descriptor-proxy> (<dood-cross-model-proxy>)
  constant slot proxy-init-keyword, required-init-keyword: keyword:;
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&init-arg-descriptor>) => (proxy)
  make(<dood-cross-model-init-arg-descriptor-proxy>,
       binding: model-variable-binding(^slot-owner(object)),
       keyword: ^init-keyword(object))
end method;

define method ^init-arg-descriptor
    (class :: <&class>, keyword :: <symbol>) => (descriptor)
  block (return)
    for (d :: <&init-arg-descriptor> in
           ^direct-initialization-argument-descriptors(class))
      if (^init-keyword(d) == keyword &
            (^init-keyword-required?(d) | ^init-supplied?(d)))
        return(d)
      end;
    end;
  end block;
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-model-init-arg-descriptor-proxy>) => (object)
  // HACK: TUNE THIS
  ^init-arg-descriptor(next-method(), proxy-init-keyword(proxy))
end method;

/// MM-WRAPPER

define class <dood-cross-model-mm-wrapper-proxy> (<dood-cross-model-proxy>)
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&mm-wrapper>) => (proxy)
  make(<dood-cross-model-mm-wrapper-proxy>,
       binding: model-variable-binding(^iclass-class(^mm-wrapper-implementation-class(object))))
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-model-mm-wrapper-proxy>) => (object)
  ^class-mm-wrapper(^class-implementation-class(next-method()))
end method;

/// MAPPED MODELS

define dood-class <dood-mapped-object-proxy> (<dood-wrapper-proxy>)
  lazy constant slot dood-proxy-mapped-object-properties,
    required-init-keyword: properties:;
end dood-class;

ignore(dood-proxy-mapped-object-properties);

define sealed domain initialize (<dood-mapped-object-proxy>);
define sealed domain make       (subclass(<dood-mapped-object-proxy>));

define method dood-make-mapped-object-proxy
    (dood :: <dood>, object, properties) => (proxy)
  make(<dood-mapped-object-proxy>,
       object:     object,
       properties: properties);
end method;

define function dood-library-description (dood :: <dood>)
  dood-root(dood)
end function;

define compiler-sideways method dood-disk-object-default
    (dood :: <dfmc-dood>, object) => (object)
  if (instance?(object, <model-properties>))
    object
  else
    let ld = dood-library-description(dood);
    // HACK: SPECIAL CASE FOR SIGNATURE TYPE VECTORS
    if (instance?(object, <simple-object-vector>))
      let properties = find-model-properties-in(ld, object, #f, create?: #f);
      if (properties)
        if (ld == model-library(object))
          dood-as-proxy(dood, object, dood-make-mapped-object-proxy, properties)
        else
          dood-as-proxy(dood, object, dood-make-binding-value-proxy)
        end if
      else
        object
      end if
    else
      let properties = lookup-owned-model-properties-in(ld, object);
      if (properties)
        dood-as-proxy(dood, object, dood-make-mapped-object-proxy, properties)
      else
        object
      end if
    end if
  end if
end method;

define method restore-mapped-object-proxy
    (dood :: <dood>, ld :: <project-library-description>,
     proxy :: <dood-mapped-object-proxy>)
 => (object)
  let model      = dood-wrapper-proxy-object(proxy);
  let properties = private-dood-proxy-mapped-object-properties(proxy);
  install-owned-model-properties-in(ld, model, properties);
  model
end method;

define method restore-mapped-object-proxy
    (dood :: <dood>, ld, proxy :: <dood-mapped-object-proxy>)
 => (object)
  // queue for later booting when dood-root has been set
  dood-root(dood) := pair(proxy, ld | #());
  dood-wrapper-proxy-object(proxy)
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-mapped-object-proxy>) => (object)
  restore-mapped-object-proxy(dood, dood-root(dood), proxy)
end method;

define sideways method dood-boot-mapped-objects
    (dood :: <dood>, proxies :: <list>, ld :: <project-library-description>)
 => ()
  do(curry(restore-mapped-object-proxy, dood, ld), proxies)
end method;
