module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// 
//// CALLABLE-OBJECT
//// 

define abstract class <&callable-object> (<object>)
end class <&callable-object>;

define generic ^iep (function);
define generic ^iep-setter (value, function);

//// 
//// FUNCTION
//// 

define inline-only function ^function-specializers (f :: <&function>)
 => (types)
  let sig  = ^function-signature(f);
  let reqs = ^signature-required(sig);
  if (size(reqs) ~= ^signature-number-required(sig))
    copy-sequence(reqs, end: ^signature-number-required(sig))
  else
    reqs
  end if
end;

// Warning - run-time.h knows about this object's format!
define abstract primary compiler-model-class <function> (<callable-object>, <object>) (<object>)
  &runtime-only-computed-slot xep,
    init-keyword: xep:;
end compiler-model-class <function>;

define generic ^xep (f :: <&function>) => (res);
define generic ^xep-setter (value, f :: <&function>);

define method xep (f :: <&function>) ^xep(f) end;
define method xep-setter (x, f :: <&function>) ^xep(f) := x end;

//// 
//// GENERIC-FUNCTION
//// 


// This object is just a place to store more datastructure in a g.f. (it goes in the cache slot).
// USERS is an enumeration of all of the <cache-header-engine-node>'s which use this
// implementation of the g.f.
define primary &class <gf-cache-info> (<object>)
  &slot gf-cache-info-users :: <simple-object-vector>,
    init-value: #[], init-keyword: users:;
end &class;



define primary &class <simple-typechecked-gf-cache-info> (<gf-cache-info>)
  &slot simple-typechecked-gf-cache-info-entries :: <simple-object-vector>,
    init-value: #[], init-keyword: entries:;
  &slot simple-typechecked-gf-cache-info-argmask :: <integer>,
    init-value: 0, init-keyword: argmask:;
end &class;

define primary &class <partial-dispatch-gf-cache-info> (<simple-typechecked-gf-cache-info>)
  &slot partial-dispatch-gf-cache-info-caches :: <list>,
    init-value: #(), init-keyword: caches:;
end &class;


define compiler-open generic ^generic-function-methods (gf :: <&generic-function>);
define compiler-open generic ^generic-function-methods-setter (value, gf :: <&generic-function>);


// Warning - run-time.h knows about this object's format!
define abstract primary &class <generic-function> (<function>)
  lazy &slot function-signature :: false-at-compile-time-or(<signature>), 
    init-value: #f,
    init-keyword: signature:;
  &slot %gf-cache, init-value: #f;
  lazy &slot debug-name :: <object>,
    init-value:   #f,
    init-keyword: debug-name:;
  lazy &computed-slot generic-function-methods :: <list>, 
    init-value: #();
  // If we start using this it should probably be made lazy, as it would
  // only be used for creating the runtime object, not compilation.
  &slot discriminator, init-value: #f;

  // Compile-time slots.
  slot ^generic-function-properties :: <integer>, init-value: 0;
  lazy slot signature-spec :: <signature-spec>, 
    required-init-keyword: signature-spec:;
  lazy slot %generic-function-domains :: <list> = #();
  slot parameters-dynamic-extent, 
    init-value: #f,
    init-keyword: dynamic-extent:;
  slot ^generic-function-cache-info = #f;
  metaclass <function-class>;
end &class <generic-function>;

// define constant $max-incomplete-methods-log-size = 16;


// COMPILE TIME ONLY
define leaf packed-slots ^generic-function-properties (<&generic-function>, <object>)
  boolean slot %generic-function-methods-initialized? = #f;
  boolean slot %generic-function-domains-initialized? = #f;
  boolean slot ^generic-function-sealed? = #t, init-keyword: sealed?:;
  boolean slot ^generic-function-compiler-open? = #f;
end packed-slots;

ignore(^generic-function-compiler-open?);

define runtime-slot-offset generic-function-methods (<generic-function>);


// TODO: FORWARD DECLARED

define compiler-open generic ^generic-function-domains (gf :: <&generic-function>);

define method ^initialize (gf :: <&generic-function>, #rest all-keys, #key)
  next-method();
  let plugh = dylan-value(#"$absent-engine-node");
  apply(initialize-packed-slots, gf, all-keys);
  ^generic-function-compiler-open?(gf) := form-compiler-open?(model-definition(gf));
  gf.^discriminator := plugh;
end method;


// This is the smaller runtime represetnation for a generic function.  It is
// used when we don't need to keep track of domain and library information.
// By special dispensation, a compiler-open generic function gets made into
// one of these.  Don't check the type, use ^generic-function-sealed? in the
// compiler.
define primary &class <sealed-generic-function> (<generic-function>)
end &class;


// The larger representation of a generic function, where we keep track of
// method libraries and domain info.
define primary &class <incremental-generic-function> (<generic-function>)
  &slot incremental-gf-module :: <module>, init-keyword: module:;
  lazy &slot incremental-gf-domain-info /* :: false-or(<domain>) */, 
    init-value: #f;
  lazy &slot incremental-gf-method-libraries :: <simple-object-vector>,
    init-keyword: method-libraries:,
    init-value: #[];
  &slot incremental-gf-properties, init-value: 0;
  metaclass <function-class>;
end &class <incremental-generic-function>;


define leaf packed-slots ^incremental-gf-properties (<&incremental-generic-function>, <object>)
  boolean slot ^incremental-gf-sealed? = #t,
    init-keyword: sealed?:;
  boolean slot ^incremental-gf-method-complete? = #t;
  boolean slot ^incremental-gf-signatured? = #t,
    init-keyword: signatured?:;
end packed-slots;

ignore(^incremental-gf-signatured?);
ignore(^incremental-gf-method-complete?);
ignore(^incremental-gf-sealed?);

define method ^initialize (g :: <&incremental-generic-function>, #key)
  next-method();
  ^incremental-gf-module(g) := model-module-model(g);
end method;

//// 
//// METHOD
//// 

// HACK: SHOULDN'T NEED THE FOLLOWING BUT &CLASS DOESN'T SUPPORT THESE MIXINS
define abstract class <&referenced-object> (<referenced-object>) end;

// Warning - run-time.h knows about this object's format!
define primary compiler-model-class <method> (<referenced-object>, <function>) (<function>)
end compiler-model-class;

define method lambda-top-level? (f :: <&method>) => (well? :: <boolean>)
  #t
end method;

define method lambda-top-level?-setter 
    (x, f :: <&method>) => (well? :: <boolean>)
  #t
end method;
/*
define method ^debug-name (x :: <&method>)
  debug-name(x) | model-variable-name(x)
end method;
*/

define compiler-open generic parameters (function);
define compiler-open generic parameters-setter (value, function);

define compiler-open generic body (function);
define compiler-open generic body-setter (value, function);

define compiler-open generic environment (function);
define compiler-open generic environment-setter (value, function);

define method environment (f :: <&method>) => (res)
  #[]
end method;

define compiler-open generic function (function);
define compiler-open generic function-setter (value, function);

define compiler-open generic ^next? (function);
define compiler-open generic ^next?-setter (value, function);

define generic ^function-next? (function);
// Extended in dfmc-conversion.
define compiler-open generic ^function-next?-setter (value, function);

define method ^function-next? (x :: <&method>) => (z :: <boolean>)
  #f
end method;

define method ^function-next?-setter
    (y :: <boolean>, x :: <&method>) => (z :: <boolean>)
  #f
end method;


define generic ^mep (function);
define generic ^mep-setter (value, function);

define generic ^keyword-specifiers (function);
define generic ^keyword-specifiers-setter (value, function);

define method ^keyword-specifiers (f :: <&method>) => (res)
  #[]
end method;


//define &class <incremental-method-mixin> (<method>)
//  &slot %incr-method-library :: <library>;
//end &class;


//define method ^initialize (m :: <&incremental-method-mixin>, #key definition)
//  next-method();
//  if (definition)
//    ^%incr-method-library(m) := namespace-model(language-definition(form-library(definition)))
//  end if
//end method;



//// 
//// LAMBDA
//// 

define dood-class <lambda-body> (<object>)
  // TODO: WHAT TYPE IS THIS?
  weak slot lambda-body-spec = #f,
    reinit-expression: #f,
    init-keyword: body-spec:;
  slot lambda-dfm-environment = #f,
    init-keyword: environment:;
  // TODO: TIE TO REAL PARAMETERS
  slot lambda-dfm-parameters :: false-or(<simple-object-vector>) = #f,
    init-keyword: parameters:;
  slot lambda-dfm-body = #f,
    init-keyword: body:;
  // TODO: TIE TO REAL QUEUE
  weak slot lambda-dfm-optimization-queue :: false-or(<stretchy-object-vector>) = #f,
    reinit-expression: #f;
end dood-class;

// Warning - run-time.h knows about this object's format!
define primary &class <lambda> (<method>)
  lazy &slot function-signature :: false-at-compile-time-or(<signature>), 
    init-value: #f,
    init-keyword: signature:;
  &computed-slot mep,
    init-keyword: mep:;
  // &slot function-next? :: <boolean>,
  //   init-value: #f,
  //   init-keyword: next?:;
  lazy slot debug-name,
    init-value: #f,
    init-keyword: compiler-debug-name:;
  // Compile-time slots.
  // slot call-site-summaries = make(<call-site-summary-table>);
  slot function-properties :: <integer> = 0;
  // definition slots
  lazy slot signature-spec :: <signature-spec>, 
    required-init-keyword: signature-spec:;
  // model slots
  slot parameters-dynamic-extent, 
    init-value: #f,
    init-keyword: dynamic-extent:;
  lazy slot lambda-heap = #f;
  // dfm slots
  lazy slot lambda-body :: false-or(<lambda-body>) = make(<lambda-body>);
  metaclass <function-class>;
end &class;

define runtime-slot-offset mep (<lambda>);

define macro lambda-body-transfer-definer
  { define lambda-body-transfer ?slotname:name , ?otherslotname:name ( ?type:* ) ; }
    => { define inline method ?slotname (f :: <&lambda>) => (v :: ?type)
	   let body = lambda-body(f);
           body & ?otherslotname(body);
	 end method;
	 define inline method ?slotname ## "-setter" (v :: ?type , f :: <&lambda>)
	  => (v :: ?type)
           ?otherslotname ## "-setter"(v, lambda-body(f))
	 end method }
end macro;

// TODO: DO THIS ALL IN ONE

define lambda-body-transfer body-spec,
  lambda-body-spec (<object>);
define lambda-body-transfer environment,
  lambda-dfm-environment (<object>);
define lambda-body-transfer parameters,         
  lambda-dfm-parameters (false-or(<simple-object-vector>));
define lambda-body-transfer body,               
  lambda-dfm-body (<object>);
define lambda-body-transfer optimization-queue,
  lambda-dfm-optimization-queue (false-or(<stretchy-object-vector>));

define leaf packed-slots function-properties (<&lambda>, <object>)
  boolean  slot lambda-optimized?             = #f;
  boolean  slot lambda-top-level?             = #f;
  tristate slot lambda-next?                  = #"unknown";
  tristate slot lambda-rest?                  = #"unknown";
  tristate slot lambda-inlineable?            = #"unknown";
  boolean  slot lambda-initializer?           = #f;
  boolean  slot lambda-has-free-references?   = #f;
  field    slot lambda-log-number-temporaries = 0, field-size: 6;
  boolean  slot lambda-copied-down?           = #f;
  // explicitly mark which functions are actually runtime
  boolean  slot lambda-runtime-function?      = #f;
end packed-slots;

define method ^make
    (class == <&method>, #rest all-keys, 
     #key definition, signature-spec, #all-keys)
 => (res :: <&method>)
//  let dyn? = definition & form-dynamic?(definition);
//  let name? = definition & form-variable-name(definition);
//  let bdef = name? & binding-definition(lookup-binding(name?));
//  let gdef = instance?(bdef, <generic-definition>) & bdef;
//  let sealed? = gdef & form-sealed?(gdef);
//  let incr? = gdef & ~sealed?;
//  if (definition)
//    format-out("\ndynamic=%= name=%= gdef=%= sealed=%= incr=%=", dyn?, name?, gdef, sealed?, incr?)
//  end if;
  apply(^make, 
	if (~signature-spec | spec-argument-key?(signature-spec))
	  if (definition & ~form-dynamic?(definition)) // top-level?
	    // if (incr?) <&incremental-keyword-method> else <&keyword-method> end
	    <&keyword-method>
	  else 
	    // if (incr?) <&incremental-keyword-closure-method> else <&keyword-closure-method> end
	    <&keyword-closure-method>
	  end if
	else
	  if (definition & ~form-dynamic?(definition)) // top-level?
	    // if (incr?) <&incremental-simple-method> else <&simple-method> end
	    <&simple-method>
	  else 
	    // if (incr?) <&incremental-simple-closure-method> else <&simple-closure-method> end
	    <&simple-closure-method>
	  end if
	end if,
	all-keys);
end method;


// define variable *dummy-call-site-summaries* = #f;
// 
// define method call-site-summaries (x :: <&lambda>) => (res :: <call-site-summary-table>)
//   *dummy-call-site-summaries*
//     | (*dummy-call-site-summaries* := make(<call-site-summary-table>))
// end method;

define method ^function-next? (x :: <&lambda>) => (z)
  lambda-next?(x)
end method;

///
/// TRAMPOLINE DFM RELATED SLOTS FOR EASIER MANAGEMENT
///

define method ^iep (f :: <&lambda>) ^mep(f) end;
define method ^iep-setter (v, f :: <&lambda>) ^mep-setter(v, f) end;

define method iep (f :: <&lambda>) mep(f) end;

define method make-mep (f :: <&lambda>) => (res :: <&code>)
  make(<&iep>, function: f)
end method;

define method initialize (function :: <&lambda>, #rest all-keys, #key body-spec)
  next-method();
  apply(initialize-packed-slots, function, all-keys);
  body-spec(function) := body-spec;
  %mep(function)      := make-mep(function);
  function
end method;

define method ^mep (lambda :: <&method>) %mep(lambda) end;
define method ^mep-setter (v, lambda :: <&method>) %mep(lambda) := v end;

define method mep (f :: <&method>) %mep(f) end;

//// 
//// SIMPLE-METHOD
//// 

define primary &class <simple-method> (<lambda>)
  metaclass <function-class>;
end &class <simple-method>;

//define primary &class <incremental-simple-method> (<incremental-method-mixin>, <simple-method>)
//  metaclass <function-class>;
//end &class <incremental-simple-method>;


define generic ^environment-size (object);
define generic ^environment-element (object, index);

define &class <closure-method-mixin> (<object>)
  // &slot environment,
  //   init-value: #f;
  compiler-constant repeated &slot environment-element,
    size-getter: environment-size,
    size-init-keyword: size:,
    size-init-value: 0;
  // slot data,
  //   init-value:   vector(),
  //   init-keyword: data:;
end &class;

// HACK: SHOULDN'T GENERATE THESE IN THE FIRST PLACE
ignore(^environment-element-values); 

// HACK: DONT REALLY NEED THESE UNTIL CLOSURES ARE BUILT AT COMPILE-TIME

// TODO: The size-getter: option isn't spotted as an accessor name by the
// define &class macro, so have to hook up the compile-stage function
// by hand. Fix!

define &override-function ^environment-size end;

define method ^environment-size
    (object :: <&closure-method-mixin>)
  0
end method;

define method ^environment-element
    (object :: <&closure-method-mixin>, index)
  #f
end method;

//// 
//// SIMPLE-CLOSURE-METHOD
//// 

define primary &class <simple-closure-method> 
    (<simple-method>, <closure-method-mixin>)
  metaclass <function-class>;
end &class <simple-closure-method>;

//define primary &class <incremental-simple-closure-method> 
//    (<incremental-method-mixin>, <simple-closure-method>)
//  metaclass <function-class>;
//end &class <incremental-simple-closure-method>;

//// 
//// KEYWORD-METHOD
//// 

define primary &class <keyword-method> (<lambda>)
  &computed-slot iep,
    init-keyword: iep:,
    init-value: #f;
  runtime-constant lazy &computed-slot keyword-specifiers :: <simple-object-vector>,
    init-value: #[],
    init-keyword: keyword-specifiers:;
  metaclass <function-class>;
end &class <keyword-method>;

define runtime-slot-offset iep (<keyword-method>);

//define primary &class <incremental-keyword-method> (<incremental-method-mixin>, <keyword-method>)
//  metaclass <function-class>;
//end &class <incremental-keyword-method>;

define method ^iep (f :: <&keyword-method>) %iep(f) end;
define method ^iep-setter (v, f :: <&keyword-method>) %iep(f) := v end;

define method iep (f :: <&keyword-method>) %iep(f) end;

define method make-mep (f :: <&keyword-method>) => (res :: <&mep>)
  make(<&mep>, function: f)
end method;

define method initialize (function :: <&keyword-method>, #key)
  next-method();
  function.%iep := make(<&iep>, function: function);
  function
end method;

define method keyword-specifiers (f :: <&keyword-method>)
  %keyword-specifiers(f)
end method;
define method keyword-specifiers-setter (v, f :: <&keyword-method>) 
  %keyword-specifiers(f) := mapped-model(v)
end method;
define method ^keyword-specifiers (f :: <&keyword-method>)
 %keyword-specifiers(f)
end method;
define method ^keyword-specifiers-setter (v, f :: <&keyword-method>)
 %keyword-specifiers(f) := mapped-model(v)
end method;

//// 
//// KEYWORD-CLOSURE-METHOD
//// 

define primary &class <keyword-closure-method> (<keyword-method>, <closure-method-mixin>)
  metaclass <function-class>;
end &class <keyword-closure-method>;

//define primary &class <incremental-keyword-closure-method> 
//  (<incremental-method-mixin>, <keyword-closure-method>)
//  metaclass <function-class>;
//end &class <incremental-keyword-closure-method>;

//// 
//// INITIALIZER-METHOD
//// 

// Initializer methods are automatically generated by the compiler for
// each (concrete) class. This is a compile-time only distinction.

define class <&initializer-method> (<&keyword-method>) end;

define method ^class-constructor 
    (class :: <&implementation-class>) => (constructor :: <&method>)
  if (slot-initialized?(class, %class-constructor))
    %class-constructor(class)
  else
    let class-name
      = form-variable-name(model-creator(class));
    let creator-name
      = splice-name-hygienically(class-name, "", " constructor");
    // This forces installation.
    if (~lookup-model-object
          (creator-name, default: #f, error-if-circular?: #f))
      // If there's no constructor model, or if we can't use it yet, use 
      // the default for now.
      %class-constructor(class) := dylan-value(#"default-class-constructor");
    end;
    %class-constructor(class)
  end;
end method;

define method ^class-constructor-setter
    (constructor :: <&method>, class :: <&implementation-class>)
 => (constructor :: <&method>)
  %class-constructor(class) := constructor
end method;

//// 
//// SLOT-INITIALIZER-METHOD
//// 

// Slot initializer methods are generated for init functions or expressions,
// another compile-time only distinction.

define class <&slot-initializer-method> (<&lambda>) end;

//// 
//// ACCESSOR-METHOD
//// 

// TODO: figure out how to rearrange things so these can be abstract
define /* abstract */ primary &class <accessor-method> (<method>)
  runtime-constant &slot method-slot-descriptor, init-value: #f,
    init-keyword: slot-descriptor:;
  slot ^function-signature :: false-or(<&signature>), 
    init-value: #f,
    init-keyword: signature:;
end &class <accessor-method>;

/*
define method initialize (function :: <&accessor-method>, #key)
  next-method();
  function
end method;
*/

define /* abstract */ &class <getter-accessor-method> (<accessor-method>)
end &class <getter-accessor-method>;

define /* abstract */ &class <setter-accessor-method> (<accessor-method>)
end &class <setter-accessor-method>;

define /* abstract */ &class <single-accessor-method> (<accessor-method>)
end &class <single-accessor-method>;

define /* abstract */ &class <repeated-accessor-method> (<accessor-method>)
end &class <repeated-accessor-method>;

define &class <getter-method> (<getter-accessor-method>,
			       <single-accessor-method>)
end &class <getter-method>;

//define &class <incremental-getter-method> (<incremental-method-mixin>, <getter-method>)
//end &class <incremental-getter-method>;

define method parameters-dynamic-extent (m :: <&getter-method>)
  #[]
end method;

define &class <setter-method> (<setter-accessor-method>,
			       <single-accessor-method>)
end &class <setter-method>;

//define &class <incremental-setter-method> (<incremental-method-mixin>, <setter-method>)
//end &class <incremental-setter-method>;

define method parameters-dynamic-extent (m :: <&setter-method>)
  #[1]
end method;

define &class <repeated-getter-method> (<getter-accessor-method>,
					<repeated-accessor-method>)
end &class <repeated-getter-method>;

//define &class <incremental-repeated-getter-method> (<incremental-method-mixin>, 
//						    <repeated-getter-method>)
//end &class <incremental-repeated-getter-method>;

define method parameters-dynamic-extent (m :: <&repeated-getter-method>)
  #[1]
end method;

define &class <repeated-setter-method> (<setter-accessor-method>,
					<repeated-accessor-method>)
end &class <repeated-setter-method>;

//define &class <incremental-repeated-setter-method> (<incremental-method-mixin>,
//						    <repeated-setter-method>)
//end &class <incremental-repeated-setter-method>;

define method parameters-dynamic-extent (m :: <&repeated-setter-method>)
  #[1, 2]
end method;

/*
define method ^slot-accessor-name (m :: <&getter-method>)
  #"dynamic-slot-accessor"
end method;

define method ^slot-accessor-name (m :: <&setter-method>)
  #"dynamic-slot-accessor-setter"
end method;

define method ^slot-accessor-name (m :: <&repeated-getter-method>)
  #"dynamic-repeated-slot-accessor"
end method;

define method ^slot-accessor-name (m :: <&repeated-setter-method>)
  #"dynamic-repeated-slot-accessor-setter"
end method;
*/

define generic ^accessor-method-xep-string (m :: <&accessor-method>) => (v :: <string>);

define method ^accessor-method-xep-string (m :: <&repeated-getter-method>) => (v :: <string>)
  "slotacc_repeated_instance_getter_xep"
end method;

define method ^accessor-method-xep-string (m :: <&repeated-setter-method>) => (v :: <string>)
  "slotacc_repeated_instance_setter_xep"
end method;

define method ^accessor-method-xep-string (m :: <&getter-method>) => (v :: <string>)
  if (instance?(^method-slot-descriptor(m), <&any-class-slot-descriptor>))
    "slotacc_single_q_class_getter_xep"
  else
    "slotacc_single_q_instance_getter_xep"
  end if
end method;

define method ^accessor-method-xep-string (m :: <&setter-method>) => (v :: <string>)
  if (instance?(^method-slot-descriptor(m), <&any-class-slot-descriptor>))
    "slotacc_single_q_class_setter_xep"
  else
    "slotacc_single_q_instance_setter_xep"
  end if
end method;


// markt, this represents a copy-down method skeleton.

define &class <copy-down-method> (<method>)
end &class;

define &class <simple-copy-down-method> (<copy-down-method>, <simple-method>)
end &class;

define &class <keyword-copy-down-method> (<copy-down-method>, <keyword-method>)
end &class;

define method ^make
    (class == <&copy-down-method>, #rest all-keys, #key signature-spec, #all-keys)
 => (res :: <&copy-down-method>)
  apply(^make, 
	if (spec-argument-key?(signature-spec))
	  <&keyword-copy-down-method>
	else
	  <&simple-copy-down-method>
	end if,
	all-keys)
end method;

//// 
//// CODE
//// 

define abstract class <&runtime-object> (<object>)
end class;

define method function (fn :: <&callable-object>) fn end;

define abstract primary class <&code> (<emitted-object>, <referenced-object>, <&callable-object>)
  slot function = #f,
    init-keyword: function:;
end class;

/// FOR NOW CODE IS ALWAYS ASSOCIATED WITH ITS CONTAINING FUNCTION
/// THIS MIGHT CHANGE WHEN WE CREATE ON THE FLY IEP'S FOR REP'S

define property-delegation (<&code>, function)
  model-definition, model-creator
end property-delegation;

//// 
//// LAMBDA-OR-CODE
//// 

define constant <&lambda-or-code> = type-union(<&lambda>, <&code>);

//// 
//// ENTRY-POINTS
//// 

define abstract primary compiler-class <any-kernel-ep> (<code>) end;
define abstract primary compiler-class <any-code-based-ep> (<code>) end;

/// TODO: THIS CODE SLOT IS ONLY USED BY C-BACK-END
///       WOULD LIKE TO MOVE THIS TO A SIDE TABLE

define primary compiler-class <iep> (<any-code-based-ep>)
  slot code = #f;
end compiler-class;

define primary compiler-class <deferred-iep> (<iep>) end;

define method function (i :: <&deferred-iep>)
  let f = next-method();
  if (instance?(f, <&function>)) f else dylan-value(f) end
end method;

define method model-creator (i :: <&deferred-iep>)
  model-creator(function(i))
end method;

define abstract class <&shared-entry-point> (<&runtime-object>, <emitted-object>)
  keyword function:;
  constant slot ^entry-point-key? :: <boolean>  = #f, 
    init-keyword: key?:;
  constant slot ^entry-point-rest? :: <boolean> = #f, 
    init-keyword: rest?:;
  constant slot ^entry-point-number-required :: <integer> = 0, 
    init-keyword: number-required:;
  constant slot ^entry-point-number-keys :: <integer> = 0, 
    init-keyword: number-keys:;
end class;

ignore(^entry-point-number-keys); // HACK: STAYING OR GOING?


define method ^entry-point-name (ep :: <&shared-entry-point>) => (res :: <byte-string>)
  ""
end method;

define method ^entry-point-optionals? (ep :: <&shared-entry-point>) => (res :: <boolean>)
  ^entry-point-key?(ep) | ^entry-point-rest?(ep)
end method;

define constant $max-shared-entry-point-top-cache-size = 4;
define constant $max-number-required = 256;
// define constant $max-number-keywords = 256;
define constant <shared-entry-point-cache> = <simple-object-vector>;

define function make-shared-entry-point-cache
    () => (res :: <shared-entry-point-cache>)
  make(<shared-entry-point-cache>, 
       size: $max-shared-entry-point-top-cache-size)
end function;

define inline function lookup-shared-entry-point 
    (cache :: <shared-entry-point-cache>, 
     type :: <class>, sig-spec :: <signature-spec>)
 => (res :: <&shared-entry-point>)
  local method as-int (x :: <boolean>) => (res :: <integer>)
	  if (x) 1 else 0 end
	end method;
  let k  = spec-argument-key?(sig-spec) ~== #f;
  let kn = spec-argument-number-keys(sig-spec);
  let r  = spec-argument-rest?(sig-spec);
  let i  = as-int(k) * 2 + as-int(r);
  let v  = element(cache, i, default: #f)
             | (element(cache, i) := make(<vector>, size: $max-number-required));
  let n  = spec-argument-number-required(sig-spec);
  let kv = element(v, n, default: #f)
             | (element(v, n) := make(<stretchy-vector>));
  let e  = element(kv, kn, default: #f)
             | (element(kv, kn) := make(type, key?: k, rest?: r, 
					      number-required: n, number-keys: kn));
  e
end function;

define abstract class <&xep> (<&shared-entry-point>) 
end class;

define method ^make (class == <&xep>, #key function, #all-keys) => (res :: <&xep>)
  ^make-xep(function)
end method;

define method ^xep (f :: <&function>) => (res :: <&xep>)
  make(<&xep>, function: f);
end method;

define method ^xep-setter (value, f :: <&function>)
end method;

define class <&generic-function-xep> (<&xep>) 
end class;

define constant $generic-function-xeps :: <shared-entry-point-cache> 
  = make-shared-entry-point-cache();

define method ^make-xep (function :: <&generic-function>) => (res :: <&generic-function-xep>)
  lookup-shared-entry-point
    ($generic-function-xeps, <&generic-function-xep>, signature-spec(function))
end method;

define abstract class <&method-xep> (<&xep>) 
end class;

define class <&lambda-xep> (<&method-xep>) 
end class;

define constant $lambda-xeps :: <shared-entry-point-cache> 
  = make-shared-entry-point-cache();

define method ^make-xep (function :: <&lambda>) => (res :: <&lambda-xep>)
  lookup-shared-entry-point
    ($lambda-xeps, <&lambda-xep>, signature-spec(function))
end method;

define abstract class <&slot-accessor-xep> (<&method-xep>) 
  constant slot ^entry-point-name :: <byte-string> = "";
end class;

define class <&slot-getter-xep> (<&slot-accessor-xep>) 
  inherited slot ^entry-point-number-required = 1;
  inherited slot ^entry-point-name = "slotacc_single_q_instance_getter_xep";
end class;

define class <&class-slot-getter-xep> (<&slot-getter-xep>) 
  inherited slot ^entry-point-name = "slotacc_single_q_class_getter_xep";
end class;

define constant $slot-getter-xep = make(<&slot-getter-xep>);
define constant $class-slot-getter-xep = make(<&class-slot-getter-xep>);

define method ^make-xep (function :: <&getter-method>) => (res :: <&slot-getter-xep>)
  if (instance?(^method-slot-descriptor(function), <&any-class-slot-descriptor>))
    $class-slot-getter-xep
  else 
    $slot-getter-xep
  end if
end method;

define class <&slot-setter-xep> (<&slot-accessor-xep>) 
  inherited slot ^entry-point-number-required = 2;
  inherited slot ^entry-point-name = "slotacc_single_q_instance_setter_xep";
end class;

define class <&class-slot-setter-xep> (<&slot-setter-xep>) 
  inherited slot ^entry-point-name = "slotacc_single_q_class_setter_xep";
end class;

define constant $slot-setter-xep = make(<&slot-setter-xep>);
define constant $class-slot-setter-xep = make(<&class-slot-setter-xep>);

define method ^make-xep (function :: <&setter-method>) => (res :: <&slot-setter-xep>)
  if (instance?(^method-slot-descriptor(function), <&any-class-slot-descriptor>))
    $class-slot-setter-xep
  else 
    $slot-setter-xep
  end if
end method;

define class <&repeated-slot-getter-xep> (<&slot-getter-xep>) 
  inherited slot ^entry-point-number-required = 2;
  inherited slot ^entry-point-name = "slotacc_repeated_instance_getter_xep";
end class;

define constant $repeated-slot-getter-xep = make(<&repeated-slot-getter-xep>);

define method ^make-xep 
    (function :: <&repeated-getter-method>) => (res :: <&repeated-slot-getter-xep>)
  $repeated-slot-getter-xep
end method;

define class <&repeated-slot-setter-xep> (<&slot-setter-xep>) 
  inherited slot ^entry-point-number-required = 3;
  inherited slot ^entry-point-name = "slotacc_repeated_instance_setter_xep";
end class;

define constant $repeated-slot-setter-xep = make(<&repeated-slot-setter-xep>);

define method ^make-xep 
    (function :: <&repeated-setter-method>) => (res :: <&repeated-slot-setter-xep>)
  $repeated-slot-setter-xep
end method;

define abstract class <&mep> (<&shared-entry-point>) 
end class;

define method ^make (class == <&mep>, #key function, #all-keys) => (res :: <&mep>)
  ^make-mep(function)
end method;

define abstract class <&method-mep> (<&mep>) 
end class;

define class <&keyword-method-mep> (<&method-mep>) 
end class;

define constant $keyword-method-meps :: <shared-entry-point-cache> 
  = make-shared-entry-point-cache();

define method ^make-mep 
    (function :: <&keyword-method>) => (res :: <&keyword-method-mep>)
  lookup-shared-entry-point
    ($keyword-method-meps, <&keyword-method-mep>, signature-spec(function))
end method;

define constant <&kernel-ep> = type-union(<&any-kernel-ep>, <&xep>);

// define constant <&code-based-ep> = type-union(<&any-code-based-ep>, <&mep>);

define method lambda-top-level? (c :: <&code>) => (top-level? :: <boolean>)
  lambda-top-level?(function(c))
end;

define method lookup-compile-stage-function (accessor :: <&code>)
 => (function :: false-or(<function>))
  lookup-compile-stage-function(function(accessor))
end method;

define property-delegation (<&iep>, function)
  parameters, environment, body, data, xep, keyword-specifiers,
  maximum-label, name, binding
end property-delegation;

define property-delegation-getters (<&iep>, function)
  named? :: <boolean>
end property-delegation-getters;

define compiler-open generic frame-size (x) => (y);

define method frame-size (lambda :: <&lambda>) => (result :: <integer>)
  lambda.environment.frame-size
end method;



////
//// DOMAIN
////

define abstract primary &class <domain> (<object>)
  runtime-constant lazy &slot domain-library :: <library>, 
    init-keyword: library:;
  lazy &slot domain-next /* :: false-or(<domain>) */, 
    init-value: #f, init-keyword: next:;
  constant slot ^domain-types, 
    required-init-keyword: domain-types:;
end &class;

define method ^initialize (d :: <&domain>, #key)
  next-method();
  ^domain-library(d) := ^home-library(model-module-model(d))
end method;


define generic ^domain-type (object :: <&domain>, index :: <integer>) => (t :: <&type>);
define generic ^domain-type-setter (value :: <&type>, object :: <&domain>, index :: <integer>);
define generic ^domain-number-required (object :: <&domain>) => (n :: <integer>);


define method ^domain-number-required (d :: <&domain>) => (n :: <integer>)
  size(^domain-types(d))
end method;

define method ^domain-type (d :: <&domain>, i :: <integer>) => (t :: <&type>)
  element(^domain-types(d), i)
end method;

define primary &class <method-domain> (<domain>)
  lazy constant &slot domain-method :: <method>, required-init-keyword: method:;
end &class;

define primary &class <standalone-domain> (<domain>)
  repeated &slot domain-type :: <type>,
    init-value: <object>,
    size-getter: domain-number-required,
    size-init-keyword: size:,
    size-init-value: 0;
end &class;


define method ^initialize (d :: <&standalone-domain>, #key domain-types :: <simple-object-vector>)
  next-method();
  let siz :: <integer> = size(domain-types);
  ^domain-type-values(d) := make(<simple-object-vector>, size: siz);
  for (i :: <integer> from 0, t :: <&type> in domain-types)
    ^domain-type(d, i) := mapped-model(t)
  end for;
end method;


define method ^domain-number-required (object :: <&standalone-domain>) => (n :: <integer>)
  size(^domain-type-values(object))
end method;

// TODO: The size-getter: option isn't spotted as an accessor name by the
// define &class macro, so have to hook up the compile-stage function
// by hand. Fix!

define &override-function ^domain-number-required end;

define method ^domain-type (object :: <&standalone-domain>, index :: <integer>) => (t :: <&type>)
  element(^domain-type-values(object), index)
end method;

define method ^domain-type-setter 
    (value :: <&type>, object :: <&standalone-domain>, index :: <integer>)
  element(^domain-type-values(object), index) := value
end method;

//// 
//// ENGINE-NODES
//// 

// **** All the following constants are copied out of D-lib-dylan:dispatch-prologue.dylan.
// **** They are also exported from dfmc-modeling.

define constant engine-node$k-absent = 0;

define constant engine-node$k-inapplicable = 1;

define constant engine-node$k-unkeyed-single-method = 2;

define constant engine-node$k-implicit-keyed-single-method = 3;

define constant engine-node$k-explicit-keyed-single-method = 4;

define constant engine-node$k-unrestricted-keyed-single-method = 5;

/*
define constant engine-node$k-reserved-terminal-n-a = 6;

define constant engine-node$k-reserved-terminal-n-b = 7;

define constant engine-node$k-reserved-terminal-n-c = 8;

define constant engine-node$k-reserved-terminal-n-d = 9;

define constant engine-node$k-reserved-terminal-n-e = 10;

define constant engine-node$k-reserved-terminal-n-f = 11;

define constant engine-node$k-reserved-terminal-n-g = 12;

// define constant engine-node$k-reserved-terminal-n-h = 13;

define constant engine-node$k-profiling-cache-header = 13;
*/

define constant engine-node$k-cache-header = 14;

define constant engine-node$k-ambiguous-methods = 15;



/*
define constant engine-node$k-first-slot-engine-node = 16;

define constant engine-node$k-boxed-instance-slot-getter = 16;
define constant engine-node$k-boxed-instance-slot-setter = 17;

define constant engine-node$k-boxed-repeated-instance-slot-getter = 18;
define constant engine-node$k-boxed-repeated-instance-slot-setter = 19;

define constant engine-node$k-boxed-class-slot-getter = 20;
define constant engine-node$k-boxed-class-slot-setter = 21;

define constant engine-node$k-raw-byte-repeated-instance-slot-getter = 22;
define constant engine-node$k-raw-byte-repeated-instance-slot-setter = 23;

define constant engine-node$k-reserved-slot-a-getter = 24;
define constant engine-node$k-reserved-slot-a-setter = 25;

define constant engine-node$k-reserved-slot-b-getter = 26;
define constant engine-node$k-reserved-slot-b-setter = 27;
*/

define constant engine-node$k-reserved-repeated-slot-a-getter = 28;
// define constant engine-node$k-reserved-repeated-slot-a-setter = 29;

// define constant engine-node$k-reserved-repeated-slot-b-getter = 30;
define constant engine-node$k-reserved-repeated-slot-b-setter = 31;


// define constant engine-node$k-slot-engine-node-count = 16;



define constant engine-node$k-typecheck = 32;

define constant engine-node$k-if-type = 33;

define constant engine-node$k-linear-by-class = 34;

define constant engine-node$k-hashed-by-class = 35;

define constant engine-node$k-linear-by-singleton-class = 36;

define constant engine-node$k-hashed-by-singleton-class = 37;

define constant engine-node$k-immediate-linear-singleton = 38;

//define constant engine-node$k-immediate-hashed-noreloc-singleton = 39;

//define constant engine-node$k-immediate-hashed-singleton = 40;

define constant engine-node$k-value-object-linear-singleton = 41;

define constant engine-node$k-monomorphic-by-class = 42;

/*
define constant engine-node$k-reserved-discriminator-a = 43;

define constant engine-node$k-reserved-discriminator-b = 44;

define constant engine-node$k-reserved-discriminator-c = 45;

define constant engine-node$k-reserved-discriminator-d = 46;

define constant engine-node$k-reserved-discriminator-e = 47;

define constant engine-node$k-reserved-discriminator-f = 48;

define constant engine-node$k-reserved-discriminator-g = 49;

define constant engine-node$k-reserved-discriminator-h = 50;

define constant engine-node$k-reserved-discriminator-i = 51;

define constant engine-node$k-reserved-discriminator-j = 52;

define constant engine-node$k-reserved-discriminator-k = 53;

define constant engine-node$k-reserved-discriminator-l = 54;

define constant engine-node$k-reserved-discriminator-m = 55;

define constant engine-node$k-reserved-discriminator-n = 56;

define constant engine-node$k-reserved-discriminator-o = 57;

define constant engine-node$k-reserved-discriminator-p = 58;

define constant engine-node$k-reserved-discriminator-q = 59;

define constant engine-node$k-reserved-discriminator-r = 60;

define constant engine-node$k-reserved-discriminator-s = 61;

define constant engine-node$k-reserved-discriminator-t = 62;

define constant engine-node$k-reserved-discriminator-u = 63;
*/

define constant properties$m-entry-type = 63;
// define constant properties$s-entry-type = 6;
define constant properties$v-entry-type = 0;
// define constant properties$v-data = properties$s-entry-type;

define constant engine-node$v-data-start = 14;

// Single method engine node properties.
define constant smen$v-nrequired = 6;
define constant smen$s-nrequired = 8;
define constant smen$m-nrequired = ash(ash(1, smen$s-nrequired) - 1, smen$v-nrequired);
define constant smen$v-restp = 14;
define constant smen$m-restp = ash(1, smen$v-restp);
// define constant smen$v-data-start = 15;



define constant $simple-typechecked-cache-arguments-limit = 8;

define constant stchen$v-checkedmask = engine-node$v-data-start;
define constant stchen$s-checkedmask = $simple-typechecked-cache-arguments-limit;
define constant stchen$m-checkedmask = ash(ash(1, stchen$s-checkedmask) - 1, stchen$v-checkedmask);


define constant $partial-dispatch-arguments-limit = 8;
define constant pdisp$v-typemask = engine-node$v-data-start;
define constant pdisp$s-typemask = $partial-dispatch-arguments-limit;
define constant pdisp$m-typemask = ash(ash(1, pdisp$s-typemask) - 1, pdisp$v-typemask);



define constant discriminator$v-argnum = 6;

define constant discriminator$s-argnum = 8;

define constant discriminator$m-argnum 
  = ash(ash(1, discriminator$s-argnum) - 1, discriminator$v-argnum);

define constant discriminator$v-nrequired = 14;
define constant discriminator$s-nrequired = 8;
define constant discriminator$m-nrequired 
  = ash(ash(1, discriminator$s-nrequired) - 1, discriminator$v-nrequired);

define constant discriminator$v-restp = 22;
// define constant discriminator$m-restp = ash(1, discriminator$v-restp);

// define constant discriminator$v-data-start = 23;


define constant $engine-node-callback-names :: <simple-object-vector> =
  #[#"%gf-dispatch-absent",		// 0, absent, general-engine-node-n
    #"%gf-dispatch-inapplicable",	// 1, inapplicable, general-engine-node-spread
    #f,					// 2, unkeyed-single-method, single-method
    #f,					// 3, implicit-keyed-single-method, implicit-keyed-single-method
    #f,					// 4, explicit-keyed-single-method, explicit-keyed-single-method
    #f,					// 5, unrestricted-keyed-single-method, unrestricted-keyed-single-method
    #f,					// 6, reserved-terminal-n-a, general-engine-node-n-engine
    #f,					// 7, reserved-terminal-n-b, general-engine-node-n-engine
    #f,					// 8, reserved-terminal-n-c, general-engine-node-n-engine
    #f,					// 9, reserved-terminal-n-d, general-engine-node-n-engine
    #f,					// 10, reserved-terminal-n-e, general-engine-node-n-engine
    #f,					// 11, reserved-terminal-n-f, general-engine-node-n-engine
    #f,					// 12, reserved-terminal-n-g, general-engine-node-n-engine
    #f,					// 13, profiling-cache-header, general-engine-node-n
    #f,					// 14, cache-header, general-engine-node-n
    #"%gf-dispatch-ambiguous-methods",	// 15, ambiguous-methods, general-engine-node-spread
    #f,					// 16, boxed-instance-slot-getter, boxed-instance-slot-getter
    #f,					// 17, boxed-instance-slot-setter, boxed-instance-slot-setter
    #f,					// 18, boxed-repeated-instance-slot-getter, boxed-repeated-instance-slot-getter
    #f,					// 19, boxed-repeated-instance-slot-setter, boxed-repeated-instance-slot-setter
    #"%gf-dispatch-boxed-class-slot-getter",// 20, boxed-class-slot-getter, general-engine-node-1
    #"%gf-dispatch-boxed-class-slot-setter",// 21, boxed-class-slot-setter, general-engine-node-2
    #f,					// 22, raw-byte-repeated-instance-slot-getter, raw-byte-repeated-instance-slot-getter
    #f,					// 23, raw-byte-repeated-instance-slot-setter, raw-byte-repeated-instance-slot-setter
    #f,					// 24, reserved-slot-a-getter, general-engine-node-1
    #f,					// 25, reserved-slot-a-setter, general-engine-node-2
    #f,					// 26, reserved-slot-b-getter, general-engine-node-1
    #f,					// 27, reserved-slot-b-setter, general-engine-node-2
    #f,					// 28, reserved-repeated-slot-a-getter, general-engine-node-2
    #f,					// 29, reserved-repeated-slot-a-setter, general-engine-node-3
    #f,					// 30, reserved-repeated-slot-b-getter, general-engine-node-2
    #f,					// 31, reserved-repeated-slot-b-setter, general-engine-node-3
    #"%gf-dispatch-typecheck",		// 32, typecheck, discriminate-on-argument
    #"%gf-dispatch-if-type",		// 33, if-type, discriminate-on-argument
    #"%gf-dispatch-linear-by-class",	// 34, linear-by-class, discriminate-on-argument
    #"%gf-dispatch-hashed-by-class",	// 35, hashed-by-class, discriminate-on-argument
    #"%gf-dispatch-linear-by-singleton-class",// 36, linear-by-singleton-class, discriminate-on-argument
    #"%gf-dispatch-hashed-by-singleton-class",// 37, hashed-by-singleton-class, discriminate-on-argument
    #"%gf-dispatch-immediate-linear-singleton",// 38, immediate-linear-singleton, discriminate-on-argument
    #"%gf-dispatch-immediate-hashed-noreloc-singleton",// 39, immediate-hashed-noreloc-singleton, discriminate-on-argument
    #"%gf-dispatch-immediate-hashed-singleton",// 40, immediate-hashed-singleton, discriminate-on-argument
    #"%gf-dispatch-slow-linear-singleton",	// 41, value-object-linear-singleton, discriminate-on-argument
    #f,	                                // 42, monomorphic-by-class, discriminate-on-argument
    #f,					// 43, reserved-discriminator-a, discriminate-on-argument
    #f,					// 44, reserved-discriminator-b, discriminate-on-argument
    #f,					// 45, reserved-discriminator-c, discriminate-on-argument
    #f,					// 46, reserved-discriminator-d, discriminate-on-argument
    #f,					// 47, reserved-discriminator-e, discriminate-on-argument
    #f,					// 48, reserved-discriminator-f, discriminate-on-argument
    #f,					// 49, reserved-discriminator-g, discriminate-on-argument
    #f,					// 50, reserved-discriminator-h, discriminate-on-argument
    #f,					// 51, reserved-discriminator-i, discriminate-on-argument
    #f,					// 52, reserved-discriminator-j, discriminate-on-argument
    #f,					// 53, reserved-discriminator-k, discriminate-on-argument
    #f,					// 54, reserved-discriminator-l, discriminate-on-argument
    #f,					// 55, reserved-discriminator-m, discriminate-on-argument
    #f,					// 56, reserved-discriminator-n, discriminate-on-argument
    #f,					// 57, reserved-discriminator-o, discriminate-on-argument
    #f,					// 58, reserved-discriminator-p, discriminate-on-argument
    #f,					// 59, reserved-discriminator-q, discriminate-on-argument
    #f,					// 60, reserved-discriminator-r, discriminate-on-argument
    #f,					// 61, reserved-discriminator-s, discriminate-on-argument
    #f,					// 62, reserved-discriminator-t, discriminate-on-argument
    #f					// 63, reserved-discriminator-u, discriminate-on-argument
];


define constant $engine-node-entry-point-names :: <simple-object-vector> =
  #[#"general-engine-node-n",	        // 0, absent
    #"general-engine-node-spread",	// 1, inapplicable
    #"single-method",			// 2, unkeyed-single-method
    #"implicit-keyed-single-method",	// 3, implicit-keyed-single-method
    #"explicit-keyed-single-method",	// 4, explicit-keyed-single-method
    #"unrestricted-keyed-single-method",// 5, unrestricted-keyed-single-method
    #"general-engine-node-n",		// 6, reserved-terminal-n-a
    #"general-engine-node-n",		// 7, reserved-terminal-n-b
    #"general-engine-node-n",		// 8, reserved-terminal-n-c
    #"general-engine-node-n",		// 9, reserved-terminal-n-d
    #"general-engine-node-n",		// 10, reserved-terminal-n-e
    #"general-engine-node-n",		// 11, reserved-terminal-n-f
    #"general-engine-node-n",		// 12, reserved-terminal-n-g
    #"profiling-cache-header",		// 13, profiling-cache-header
    #"cache-header",			// 14, cache-header
    #"ambiguous-methods",		// 15, ambiguous-methods
    #"boxed-instance-slot-getter",	// 16, boxed-instance-slot-getter
    #"boxed-instance-slot-setter",	// 17, boxed-instance-slot-setter
    #"boxed-repeated-instance-slot-getter",// 18, boxed-repeated-instance-slot-getter
    #"boxed-repeated-instance-slot-setter",// 19, boxed-repeated-instance-slot-setter
    #"general-engine-node-1",		// 20, boxed-class-slot-getter
    #"general-engine-node-2",		// 21, boxed-class-slot-setter
    #"raw-byte-repeated-instance-slot-getter",// 22, raw-byte-repeated-instance-slot-getter
    #"raw-byte-repeated-instance-slot-setter",// 23, raw-byte-repeated-instance-slot-setter
    #"general-engine-node-1",		// 24, reserved-slot-a-getter
    #"general-engine-node-2",		// 25, reserved-slot-a-setter
    #"general-engine-node-1",		// 26, reserved-slot-b-getter
    #"general-engine-node-2",		// 27, reserved-slot-b-setter
    #"general-engine-node-2",		// 28, reserved-repeated-slot-a-getter
    #"general-engine-node-3",		// 29, reserved-repeated-slot-a-setter
    #"general-engine-node-2",		// 30, reserved-repeated-slot-b-getter
    #"general-engine-node-3",		// 31, reserved-repeated-slot-b-setter
    #"typecheck-discriminator",		// 32, typecheck
    #"if-type-discriminator",		// 33, if-type
    #"discriminate-on-argument",	// 34, linear-by-class
    #"discriminate-on-argument",	// 35, hashed-by-class
    #"discriminate-on-argument",	// 36, linear-by-singleton-class
    #"discriminate-on-argument",	// 37, hashed-by-singleton-class
    #"discriminate-on-argument",	// 38, immediate-linear-singleton
    #"discriminate-on-argument",	// 39, immediate-hashed-noreloc-singleton
    #"discriminate-on-argument",	// 40, immediate-hashed-singleton
    #"discriminate-on-argument",	// 41, value-object-linear-singleton
    #"monomorphic-by-class-discriminator", // 42, immediate-hashed-singleton
    #"discriminate-on-argument",	// 43, reserved-discriminator-a
    #"discriminate-on-argument",	// 44, reserved-discriminator-b
    #"discriminate-on-argument",	// 45, reserved-discriminator-c
    #"discriminate-on-argument",	// 46, reserved-discriminator-d
    #"discriminate-on-argument",	// 47, reserved-discriminator-e
    #"discriminate-on-argument",	// 48, reserved-discriminator-f
    #"discriminate-on-argument",	// 49, reserved-discriminator-g
    #"discriminate-on-argument",	// 50, reserved-discriminator-h
    #"discriminate-on-argument",	// 51, reserved-discriminator-i
    #"discriminate-on-argument",	// 52, reserved-discriminator-j
    #"discriminate-on-argument",	// 53, reserved-discriminator-k
    #"discriminate-on-argument",	// 54, reserved-discriminator-l
    #"discriminate-on-argument",	// 55, reserved-discriminator-m
    #"discriminate-on-argument",	// 56, reserved-discriminator-n
    #"discriminate-on-argument",	// 57, reserved-discriminator-o
    #"discriminate-on-argument",	// 58, reserved-discriminator-p
    #"discriminate-on-argument",	// 59, reserved-discriminator-q
    #"discriminate-on-argument",	// 60, reserved-discriminator-r
    #"discriminate-on-argument",	// 61, reserved-discriminator-s
    #"discriminate-on-argument",	// 62, reserved-discriminator-t
    #"discriminate-on-argument"		// 63, reserved-discriminator-u
    ];

define abstract primary compiler-class <engine-node-ep> (<any-kernel-ep>)
  slot ^engine-node-ep-properties :: <integer>, init-value: 0;
  constant slot ^engine-node, required-init-keyword: engine-node:;
end compiler-class;


define method ^initialize (ep :: <&engine-node-ep>, #rest all-keys, #key properties, #all-keys)
  next-method();
  if (~properties) apply(initialize-packed-slots, ep, all-keys) end;
end method;


// An entry point of a <discriminator> object.  We not only associate with a function,
// but also know which argument number will be discriminated on.
define primary compiler-class <discriminator-ep> (<engine-node-ep>)
end compiler-class;

// An entry point of an engine node which is associated with a particular
// function (hence we can know what the calling sequence will be like in advance).
define primary compiler-class <function-linked-engine-node-ep> (<engine-node-ep>)
  // But it inherits the function slot from <code> or somesuch.
end compiler-class;

// An entry point of an engine node which may not be associated with
// any particular function.
define primary compiler-class <rogue-engine-node-ep> (<engine-node-ep>)
  constant slot ^entry-point-name, required-init-keyword: entry-point-name:;
end compiler-class;


define leaf packed-slots ^engine-node-ep-properties (<&function-linked-engine-node-ep>, <object>)
  field   slot ^engine-node-ep-number-required = 0, field-size: 8,
    init-keyword: number-required:;
  boolean slot ^engine-node-ep-optionals? = #f,
    init-keyword: optionals?:;
end packed-slots;

ignore(^engine-node-ep-optionals?);


define abstract &class <dispatch-engine-invocable> (<object>)
end &class;

define abstract primary &class <properties-provider> (<object>)
  &slot properties :: <integer>, init-value: 0;
end &class;


define abstract primary &class <engine-node> (<properties-provider>, 
					      <dispatch-engine-invocable>)
  weak &slot engine-node-callback, 
    reinit-expression: ^compute-engine-node-callback(self);
  weak &slot engine-node-entry-point, 
    reinit-expression: ^compute-engine-node-entry-point(self);
end &class;


define inline method ^entry-type-number (e :: <&engine-node>) => (n :: <integer>);
  ash(logand(e.^properties, properties$m-entry-type), - properties$v-entry-type)
end method;


define method ^entry-type-number-setter (n :: <integer>, e :: <&engine-node>) => ()
  e.^properties := logior(logand(e.^properties, lognot(properties$m-entry-type)),
			  ash(n, properties$v-entry-type));
end method;

define method ^entry-point-name (e :: <&engine-node>) => (name :: false-or(<symbol>))
  $engine-node-entry-point-names[^entry-type-number(e)]
end method;

define method ^entry-point-name (ep :: <&engine-node-ep>) => (name :: false-or(<symbol>))
  $engine-node-entry-point-names[^entry-type-number(^engine-node(ep))]
end method;


define method function (e :: <&engine-node>) => (v)
  #f
end method;


define macro concrete-engine-node-initialization-definer
  { define ?mods:* concrete-engine-node-initialization "<" ## ?:name ## ">" 
          (?self:name, ?params:*)
          ?:body
          end }
    => 
    { define ?mods method initialize (?self :: "<&" ## ?name ## ">", ?params)
	^entry-type-number(?self) := engine-node-constant(?name);
	?=next-method();
	?body
      end method
  }
end macro;

define macro engine-node-constant
  // If you've seen one cache header, you've seen them all.  Unless it's a profiling cache header
  // engine node.
  { engine-node-constant(?:name ## "-profiling-cache-header-engine-node") }
    => { engine-node$k-profiling-cache-header }
  { engine-node-constant(?:name ## "-cache-header-engine-node") }
    => { engine-node$k-cache-header }
  { engine-node-constant(?:name ## "-engine-node") }
    => { "engine-node$k-" ## ?name }
  { engine-node-constant(?:name ## "-gf-cache") }
    => { "engine-node$k-" ## ?name }
  { engine-node-constant(?:name ## "-discriminator") }
    => { "engine-node$k-" ## ?name }
//  { engine-node-constant(?:name) }
//    => { "engine-node$k-" ## ?name }
end macro;


define generic ^compute-engine-node-entry-point (e :: <&engine-node>) => (ep :: <&engine-node-ep>);
define generic ^compute-engine-node-callback (e :: <&engine-node>) => (cb);


define method ^compute-engine-node-callback (e :: <&engine-node>) => (cb)
  with-dependent-context ($compilation of model-creator(e))
    let entry-type :: <integer> = ^entry-type-number(e);
    let cb = $engine-node-callback-names[entry-type];
    if (cb == #"%gf-dispatch-absent" & ~instance?(e, <&singular-terminal-engine-node>))
      ^engine-node-callback(dylan-value(#"$absent-engine-node"))
    else
      cb & ^make(<&deferred-iep>, function: cb)
    end if
  end with-dependent-context;
end method;


define method ^compute-engine-node-entry-point (e :: <&engine-node>) 
 => (ep :: <&engine-node-ep>)
  with-dependent-context ($compilation of model-creator(e))
    ^make(<&rogue-engine-node-ep>,
	  engine-node: e,
  	  entry-point-name: ^entry-point-name(e))
  end with-dependent-context;
end method;


define method initialize (e :: <&engine-node>, #key)
  next-method();
  e.^engine-node-entry-point := ^compute-engine-node-entry-point(e);
  e.^engine-node-callback := ^compute-engine-node-callback(e);
end method;


// Have to do this proxy business because engine nodes (specifically
// <&deferred-iep>'s) mess around with their slots in a way that seems
// to confuse the database... So I decided to finesse this whole thing
// by recreating any nodes that get dumped.
//define class <dood-engine-node-proxy> (<dood-proxy>)
//  slot dood-engine-node-class :: subclass(<&engine-node>),
//    required-init-keyword: class:;
//  slot dood-engine-node-entry-type,
//    required-init-keyword: entry-type:;
//  slot dood-engine-node-creator,
//    required-init-keyword: creator:;
//end class;

//define method dood-restore-proxy
//    (dood :: <dood>, proxy :: <dood-engine-node-proxy>) => (object)
//  with-dood-context (dood-root(dood))
//    with-dependent ($top-level-processing of dood-engine-node-creator(proxy))
//      make(dood-engine-node-class(proxy),
//	   entry-type: dood-engine-node-entry-type(proxy))
//    end with-dependent
//  end with-dood-context
//end method;
  
//define method dood-make-engine-node-proxy
//    (dood :: <dood>, e :: <&engine-node>) => (proxy)
//  make(<dood-engine-node-proxy>,
//       class: object-class(e),
//       entry-type: ^entry-type-number(e),
//       creator: model-creator(e))
//end method;

//define method dood-disk-object (dood :: <dood>, i :: <&engine-node>) => (res)
//  if (dylan-library-library-description?(dood-root(dood)))
//    dood-as-proxy(dood, i, dood-make-engine-node-proxy)
//  else // just a regular cross-library ref...
//    next-method()
//  end;
//end method;

define abstract primary &class <terminal-engine-node> (<engine-node>)
end &class;

define abstract primary &class <singular-terminal-engine-node> (<terminal-engine-node>)
end &class;

define abstract primary &class <uniquified-terminal-engine-node> (<terminal-engine-node>)
end &class;

define abstract primary &class <unshared-terminal-engine-node> (<terminal-engine-node>)
end &class;


define abstract primary &class <cache-header-engine-node> (<engine-node>)
  &slot cache-header-engine-node-next, init-keyword: next:, init-value: #f;
  &slot cache-header-engine-node-parent, init-keyword: parent:, init-value: #f;
  slot function,  required-init-keyword: function:;
end &class;


define method ^initialize (e :: <&cache-header-engine-node>, #key parent, function)
  next-method();
  if (~^cache-header-engine-node-parent(e)) 
    ^cache-header-engine-node-parent(e) := function 
  end;
  if (~^cache-header-engine-node-next(e)) 
    ^cache-header-engine-node-next(e) := dylan-value(#"$absent-engine-node");
  end;
end method;


define method ^compute-engine-node-entry-point (e :: <&cache-header-engine-node>)
 => (ep :: <&engine-node-ep>)
  with-dependent-context ($compilation of model-creator(e))
    let g :: <&generic-function> = function(e);
//    if (instance?(^%gf-cache(g), <&engine-node>))
      let sig = ^function-signature(g);
      ^make(<&function-linked-engine-node-ep>, 
	    number-required: ^signature-number-required(sig),
	    optionals?:      ^signature-optionals?(sig),
	    function:        g, 
	    engine-node:     e)
//    else
//      ^make(<&rogue-engine-node-ep>, 
//	    entry-point-name: #"general-engine-node-n",
//	    engine-node: e)
//    end if
  end with-dependent-context;
end method;
	  
	  

define primary &class <common-root-cache-header-engine-node> (<cache-header-engine-node>)
end &class;


define concrete-engine-node-initialization <common-root-cache-header-engine-node>
    (e, #key)
end concrete-engine-node-initialization;



define primary &class <simple-typechecked-cache-header-engine-node> (<cache-header-engine-node>)
end &class;


define concrete-engine-node-initialization <simple-typechecked-cache-header-engine-node>
    (e, #key checkedmask :: <integer> = 0)
  ^properties(e) := logior(ash(checkedmask, stchen$v-checkedmask), ^properties(e));
end concrete-engine-node-initialization;


define inline-only function ^stchen-checkedmask (e :: <&simple-typechecked-cache-header-engine-node>)
 => (checkedmask :: <integer>)
  ash(logand(^properties(e), stchen$m-checkedmask), - stchen$v-checkedmask)
end function;


define generic ^partial-dispatch-type (object, index :: <integer>) => (t :: <&type>);
define generic ^partial-dispatch-type-setter (value :: <&type>, object, index :: <integer>);
define generic ^number-types (object) => (n :: <integer>);

define primary &class <partial-dispatch-cache-header-engine-node> (<cache-header-engine-node>)
  repeated &slot partial-dispatch-type :: <type>,
    init-value: <object>,
    size-getter: number-types,
    size-init-keyword: size:,
    size-init-value: 0;
end &class;


define inline function ^pdisp-type-mask (e :: <&partial-dispatch-cache-header-engine-node>)
 => (checkedmask :: <integer>)
  ash(logand(^properties(e), pdisp$m-typemask), - pdisp$v-typemask)
end function;


define concrete-engine-node-initialization <partial-dispatch-cache-header-engine-node>
    (e, #key types :: false-or(<simple-object-vector>), type-mask :: <integer> = 0)
  // HACK: CALLED WHEN FINALIZING COPYING, 
  //       ENTRY-POINT INITIALIZATION SHOULD BE SPLIT OUT
  when (types)
    let siz :: <integer> = size(types);
    let vals = make(<simple-object-vector>, size: siz);
    ^partial-dispatch-type-values(e) := vals;
    ^properties(e) := logior(ash(type-mask, pdisp$v-typemask), ^properties(e));
    for (i :: <integer> from 0, t :: <&type> in types)
      vals[i] := mapped-model(t)
    end for;
  end when;
end concrete-engine-node-initialization;


define method ^number-types 
    (object :: <&partial-dispatch-cache-header-engine-node>) => (res :: <integer>)
  size(^partial-dispatch-type-values(object))
end method;

// TODO: The size-getter: option isn't spotted as an accessor name by the
// define &class macro, so have to hook up the compile-stage function
// by hand. Fix!

define &override-function ^number-types end;

define method ^partial-dispatch-type 
    (object :: <&partial-dispatch-cache-header-engine-node>, index :: <integer>) => (t :: <&type>)
  element(^partial-dispatch-type-values(object), index)
end method;

define method ^partial-dispatch-type-setter 
    (value :: <&type>, object :: <&partial-dispatch-cache-header-engine-node>, index :: <integer>)
  element(^partial-dispatch-type-values(object), index) := value
end method;


define primary &class <simple-call-site-cache-header-engine-node> (<cache-header-engine-node>)
end &class;

define concrete-engine-node-initialization <simple-call-site-cache-header-engine-node>
    (e, #key )
  
end concrete-engine-node-initialization;

define primary &class <profiling-call-site-cache-header-engine-node> (<cache-header-engine-node>)
  &slot profiling-call-site-cache-header-engine-node-count-1;
  &slot profiling-call-site-cache-header-engine-node-count-2;
  &slot profiling-call-site-cache-header-engine-node-id;
  &slot profiling-call-site-cache-header-engine-node-library;
  slot  profiling-call-site-cache-header-engine-node-call;
end &class;

define concrete-engine-node-initialization <profiling-call-site-cache-header-engine-node>
  (e, #key )
  let zero = make(<&raw-machine-word>, value: 0);
  ^profiling-call-site-cache-header-engine-node-count-1(e) := zero;
  ^profiling-call-site-cache-header-engine-node-count-2(e) := zero;
  let ld = current-library-description();
  ^profiling-call-site-cache-header-engine-node-id(e) 
    := library-generate-call-site-id(ld);
  ^profiling-call-site-cache-header-engine-node-library(e) 
    := namespace-model(language-definition(ld));
end concrete-engine-node-initialization;


define abstract primary &class <discriminator> (<engine-node>)
  slot function,  required-init-keyword: function:;
end &class;


define method ^discriminator-argnum (d :: <&discriminator>) 
 => (argnum :: <integer>);
  ash(logand(d.^properties, discriminator$m-argnum), - discriminator$v-argnum)
end method;

define method ^discriminator-nrequired (d :: <&discriminator>)
 => (argnum :: <integer>);
  ash(logand(d.^properties, discriminator$m-nrequired), - discriminator$v-nrequired)
end method;

define method ^discriminator-optionals? (d :: <&discriminator>)
 => (optionals? :: <boolean>);
  logbit?(discriminator$v-restp, d.^properties)
end method;


define method initialize (d :: <&discriminator>, 
			  #key entry-type :: <integer>, 
			       argnum :: <integer>,
			       function :: <&generic-function>)
  next-method();
  let sig-spec = signature-spec(function);
  let req-size = spec-argument-number-required(sig-spec);
  let p :: <integer> = ^properties(d);
  let p :: <integer>
    = logior(ash(spec-argument-number-required(sig-spec), discriminator$v-nrequired), p);
  let p :: <integer> 
    = logior(ash(if (spec-argument-optionals?(sig-spec)) 1 else 0 end, discriminator$v-restp), p);
  let p :: <integer> = logior(ash(argnum, discriminator$v-argnum), p);
  ^properties(d) := p;
end method;


define method ^compute-engine-node-entry-point (d :: <&discriminator>) 
 => (ep :: <&engine-node-ep>);
  ^make(<&discriminator-ep>,
	engine-node: d,
	number-required: ^discriminator-nrequired(d),
	optionals?: ^discriminator-optionals?(d),
	function: function(d))
end method;


// The absent engine node.  There is only one, so they are all ==.
define primary &class <absent-engine-node> (<singular-terminal-engine-node>)
end &class;

define concrete-engine-node-initialization <absent-engine-node> (self, #key)
end concrete-engine-node-initialization;


// define &override-function ^get-absent-engine-node ()
//   ^make(<&absent-engine-node>, entry-type: 0)
// end;


define function source-constructor-for-$absent-engine-node ()
  let a = ^make(<&absent-engine-node>, entry-type: engine-node$k-absent);
  #{ define constant $absent-engine-node = ?a /* get-absent-engine-node() */; }
end function;

/*
define ^mapping <absent-engine-node> => <&absent-engine-node>
  // &instance $absent-engine-node => source-constructor-for-$absent-engine-node();
end ^mapping;
*/

do-define-core-unadorned-definition(#"$absent-engine-node", source-constructor-for-$absent-engine-node);

// The inapplicable engine node.  There is only one, so they are all ==.
define primary &class <inapplicable-engine-node> (<singular-terminal-engine-node>)
end &class;


define concrete-engine-node-initialization <inapplicable-engine-node> (e, #key)
end concrete-engine-node-initialization;


// define &override-function ^get-inapplicable-engine-node ()
//   ^make(<&inapplicable-engine-node>, entry-type: 0)
// end;


define function source-constructor-for-$inapplicable-engine-node ()
  let a = ^make(<&inapplicable-engine-node>, entry-type: engine-node$k-inapplicable);
  #{ define constant $inapplicable-engine-node = ?a /* get-inapplicable-engine-node() */; }
end function;

/*
define ^mapping <inapplicable-engine-node> => <&inapplicable-engine-node>
  // &instance $inapplicable-engine-node => source-constructor-for-$inapplicable-engine-node();
end ^mapping;
*/

do-define-core-unadorned-definition(#"$inapplicable-engine-node", source-constructor-for-$inapplicable-engine-node);


define primary &class <ambiguous-methods-engine-node> (<unshared-terminal-engine-node>)
  &slot ambiguous-methods-engine-node-ordered :: <sequence>, required-init-keyword: ordered:;
  &slot ambiguous-methods-engine-node-ambig :: <sequence>, required-init-keyword: ambig:;
end &class;

define concrete-engine-node-initialization <ambiguous-methods-engine-node> (e, #key)
end concrete-engine-node-initialization;


// This is for when we have a method and must give it next-method
// data.  We hope and expect that many more effective methods will not
// require this than will, and that assumption is based on our ability
// to tell at runtime whether a method needs its extra-argument.
// Overlay: method, data, keys
define abstract primary &class <single-method-engine-node> (<unshared-terminal-engine-node>)
  &slot single-method-engine-node-method :: <method>, required-init-keyword: method:;
  &slot single-method-engine-node-data, required-init-keyword: data:;
  slot function, required-init-keyword: function:;
end &class;

define inline-only function ^smen-nrequired (e :: <&single-method-engine-node>) => (nreq :: <integer>)
  ash(logand(^properties(e), smen$m-nrequired), - smen$v-nrequired)
end function;

define inline-only function ^smen-optionals? (e :: <&single-method-engine-node>) => (optionals? :: <boolean>)
  logbit?(smen$v-restp, ^properties(e))
end function;


define method initialize (e :: <&single-method-engine-node>, #key method: meth)
  let sig-spec = signature-spec(meth);
  ^properties(e) := logior(^properties(e),
			   ash(spec-argument-number-required(sig-spec), smen$v-nrequired),
			   if (spec-argument-optionals?(sig-spec)) smen$m-restp else 0 end);
  next-method()
end method;


define method ^compute-engine-node-entry-point (e :: <&single-method-engine-node>)
 => (ep :: <&function-linked-engine-node-ep>)
  make(<&function-linked-engine-node-ep>, 
       engine-node: e,
       function: ^single-method-engine-node-method(e),
       number-required: ^smen-nrequired(e),
       optionals?: ^smen-optionals?(e))
end method;


define primary &class <unkeyed-single-method-engine-node> (<single-method-engine-node>)
end &class;

define concrete-engine-node-initialization <unkeyed-single-method-engine-node> (e, #key)
end concrete-engine-node-initialization;

define abstract primary &class <keyed-single-method-engine-node> (<single-method-engine-node>)
end &class;

define primary &class <explicit-keyed-single-method-engine-node> (<keyed-single-method-engine-node>)
  &slot single-method-engine-node-keys :: <simple-object-vector>, required-init-keyword: keys:;
end &class;

define concrete-engine-node-initialization <explicit-keyed-single-method-engine-node> (e, #key)
end concrete-engine-node-initialization;


define primary &class <implicit-keyed-single-method-engine-node> (<single-method-engine-node>)
end &class;

define concrete-engine-node-initialization <implicit-keyed-single-method-engine-node> (e, #key)
end concrete-engine-node-initialization;

define primary &class <unrestricted-keyed-single-method-engine-node> (<single-method-engine-node>)
end &class;

define concrete-engine-node-initialization <unrestricted-keyed-single-method-engine-node> (e, #key)
end concrete-engine-node-initialization;

define abstract primary &class <class-keyed-discriminator> (<discriminator>)
end &class;

define abstract primary &class <linear-class-keyed-discriminator> (<class-keyed-discriminator>)
  &slot lckd-index :: <integer>, init-value: 0;
  &slot lckd-hits  :: <integer>, init-value: 0; // ONLY FOR PROFILING
end &class;

define abstract primary &class <hashed-class-keyed-discriminator> (<class-keyed-discriminator>)
  &slot %hckd-count :: <integer>, init-value: 0;
  &slot %hckd-limit :: <integer>, init-value: 0;
end &class;

define abstract &class <by-class-discriminator> (<class-keyed-discriminator>)
end &class;

define abstract &class <by-singleton-class-discriminator> (<class-keyed-discriminator>)
  &slot class-keyed-discriminator-default;
end &class;

define generic ^class-keyed-discriminator-table-element (object, index :: <integer>) => (value);
define generic ^class-keyed-discriminator-table-element-setter (value, object, index :: <integer>);
define generic ^class-keyed-discriminator-table-size (object) => (n :: <integer>);

ignore(^class-keyed-discriminator-table-size);

define primary &class <linear-by-class-discriminator> (<by-class-discriminator>, <linear-class-keyed-discriminator>)
  repeated &slot class-keyed-discriminator-table-element,
    init-value:        #f,
    size-getter:       class-keyed-discriminator-table-size,
    size-init-keyword: size:,
    size-init-value:   0;
end &class;

// HACK: SHOULDN'T GENERATE THESE IN THE FIRST PLACE
ignore(^class-keyed-discriminator-table-element-values);
ignore(^class-keyed-discriminator-table-element-values-setter);


define concrete-engine-node-initialization <linear-by-class-discriminator> (e, #key)
end concrete-engine-node-initialization;

define primary &class <hashed-by-class-discriminator> (<by-class-discriminator>, <hashed-class-keyed-discriminator>)
  repeated &slot class-keyed-discriminator-table-element,
    init-value:        #f,
    size-getter:       class-keyed-discriminator-table-size,
    size-init-keyword: size:,
    size-init-value:   0;
end &class;

define concrete-engine-node-initialization <hashed-by-class-discriminator> (e, #key)
end concrete-engine-node-initialization;

define primary &class <linear-by-singleton-class-discriminator> (<by-singleton-class-discriminator>,
								 <linear-class-keyed-discriminator>)
  repeated &slot class-keyed-discriminator-table-element,
    init-value:        #f,
    size-getter:       class-keyed-discriminator-table-size,
    size-init-keyword: size:,
    size-init-value:   0;
end &class;

define concrete-engine-node-initialization <linear-by-singleton-class-discriminator> (e, #key)
end concrete-engine-node-initialization;

define primary &class <hashed-by-singleton-class-discriminator> (<by-singleton-class-discriminator>,
								 <hashed-class-keyed-discriminator>)
  repeated &slot class-keyed-discriminator-table-element,
    init-value:        #f,
    size-getter:       class-keyed-discriminator-table-size,
    size-init-keyword: size:,
    size-init-value:   0;
end &class;

define concrete-engine-node-initialization <hashed-by-singleton-class-discriminator> (e, #key)
end concrete-engine-node-initialization;

// data slots overlay: typecheck-discriminator-type, typecheck-discriminator-next.
define primary &class <typecheck-discriminator> (<discriminator>)
  &slot typecheck-discriminator-type, required-init-keyword: type:;
  &slot typecheck-discriminator-next, required-init-keyword: next:;
end &class;


define concrete-engine-node-initialization <typecheck-discriminator> (e, #key)
end concrete-engine-node-initialization;


define primary &class <monomorphic-by-class-discriminator> (<by-class-discriminator>, <class-keyed-discriminator>)
  &slot monomorphic-by-class-discriminator-key,  required-init-keyword: key:;
  &slot monomorphic-by-class-discriminator-next, required-init-keyword: next:;
end &class;


define concrete-engine-node-initialization <monomorphic-by-class-discriminator> (e, #key)
end concrete-engine-node-initialization;


// Do one thing or another depending on whether a type test is satisfied.  We
// could make a typecase-like version of this if we found there were lots of
// chains of them.
define primary &class <if-type-discriminator> (<discriminator>)
  &slot if-type-discriminator-type, required-init-keyword: type:;
  &slot if-type-discriminator-then, init-value: #f;
  &slot if-type-discriminator-else, init-value: #f;
end &class;

define concrete-engine-node-initialization <if-type-discriminator> (e, #key)
end concrete-engine-node-initialization;

// Data slots overlay:  singleton-discriminator-table, singleton-discriminator-default.
define abstract primary &class <singleton-discriminator> (<discriminator>)
  &slot singleton-discriminator-table :: <simple-object-vector>,
    required-init-keyword: table:;
  &slot singleton-discriminator-default, init-value: #f;
end &class;

define abstract primary &class <linear-singleton-discriminator> (<singleton-discriminator>)
  &slot lsd-index :: <integer>, init-value: 0;
  &slot lsd-hits  :: <integer>, init-value: 0;  // HACK: ONLY FOR PROFILING
end &class;

define primary &class <immediate-linear-singleton-discriminator> 
    (<linear-singleton-discriminator>, <singleton-discriminator>)
end &class;

define concrete-engine-node-initialization <immediate-linear-singleton-discriminator> (e, #key)
end concrete-engine-node-initialization;


define primary &class <value-object-linear-singleton-discriminator>
  (<linear-singleton-discriminator>, <singleton-discriminator>)
end &class;

define concrete-engine-node-initialization <value-object-linear-singleton-discriminator> (e, #key)
end concrete-engine-node-initialization;


////
//// DOOD PROXIES
////


/// ABSENT-ENGINE-NODE

define class <dood-cross-model-absent-engine-node-proxy>
    (<dood-cross-model-proxy>)
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&absent-engine-node>) => (proxy)
  make(<dood-cross-model-absent-engine-node-proxy>,
       binding: dylan-canonical-binding(#"$absent-engine-node"));
end method;

/// INAPPLICABLE-ENGINE-NODE

define class <dood-cross-model-inapplicable-engine-node-proxy>
    (<dood-cross-model-proxy>)
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&inapplicable-engine-node>) => (proxy)
  make(<dood-cross-model-inapplicable-engine-node-proxy>,
       binding: dylan-canonical-binding(#"$inapplicable-engine-node"));
end method;

//// SLOT ACCESS ENGINE NODES

define abstract primary &class <slot-access-engine-node>
  (<terminal-engine-node>)
end &class;

define abstract &class <slot-setter-engine-node>
  (<slot-access-engine-node>)
end &class;

define abstract &class <slot-getter-engine-node>
  (<slot-access-engine-node>)
end &class;

define abstract &class <single-slot-access-engine-node>
    (<slot-access-engine-node>)
end &class;

define abstract primary &class <repeated-slot-access-engine-node>
    (<slot-access-engine-node>)
  &slot slot-engine-node-size-offset :: <integer>, init-value: 0;
end &class;

define abstract &class <instance-slot-engine-node>
  (<slot-access-engine-node>)
end &class;

define abstract &class <class-slot-engine-node>
    (<slot-access-engine-node>)
end &class;

define abstract &class <boxed-instance-slot-engine-node>
  (<instance-slot-engine-node>)
end &class;

define abstract &class <byte-slot-engine-node>
    (<instance-slot-engine-node>)
end &class;

define primary &class <boxed-instance-slot-getter-engine-node>
  (<slot-getter-engine-node>, 
   <single-slot-access-engine-node>,
   <boxed-instance-slot-engine-node>)
end &class;

define primary &class <boxed-instance-slot-setter-engine-node>
    (<slot-setter-engine-node>, 
     <single-slot-access-engine-node>, 
     <boxed-instance-slot-engine-node>)
end &class;

define primary &class <boxed-repeated-instance-slot-getter-engine-node>
  (<slot-getter-engine-node>, 
   <repeated-slot-access-engine-node>,
   <boxed-instance-slot-engine-node>)
end &class;

define primary &class <boxed-repeated-instance-slot-setter-engine-node>
    (<slot-setter-engine-node>, 
     <repeated-slot-access-engine-node>, 
     <boxed-instance-slot-engine-node>)
end &class;

define primary &class <byte-slot-getter-engine-node>
    (<slot-getter-engine-node>,
     <single-slot-access-engine-node>,
     <byte-slot-engine-node>)
end &class;

define primary &class <byte-slot-setter-engine-node>
    (<slot-setter-engine-node>,
     <single-slot-access-engine-node>,
     <byte-slot-engine-node>)
end &class;

define primary &class <repeated-byte-slot-getter-engine-node>
  (<slot-getter-engine-node>, 
   <repeated-slot-access-engine-node>,
   <byte-slot-engine-node>)
end &class;

define primary &class <repeated-byte-slot-setter-engine-node>
    (<slot-setter-engine-node>, 
     <repeated-slot-access-engine-node>, 
     <byte-slot-engine-node>)
end &class;

define abstract primary &class <boxed-class-slot-engine-node>
    (<class-slot-engine-node>)
end &class;

define primary &class <boxed-class-slot-getter-engine-node>
    (<slot-getter-engine-node>, 
     <single-slot-access-engine-node>, 
     <boxed-class-slot-engine-node>)
end &class;

define primary &class <boxed-class-slot-setter-engine-node>
    (<slot-setter-engine-node>,
     <single-slot-access-engine-node>, 
     <boxed-class-slot-engine-node>)
end &class;


/*
define function ^slot-method-requiring-class-discrimination? (m :: <&method>)
  if (instance?(m, <&accessor-method>))
    let m :: <&accessor-method> = m;
    let sd :: <&slot-descriptor> = ^method-slot-descriptor(m);
    let c :: <&class> = ^slot-owner(sd);
    ~^class-primary?(c)
  else
    #f
  end if
end function;


define constant slotdiscrim$v-offset 
  = engine-node$v-data-start;

define function ^slot-engine-node-offset-setter 
    (offset :: <integer>, e :: <&slot-access-engine-node>)
 => (offset :: <integer>);
  let mask :: <integer> = ash(1, slotdiscrim$v-offset) - 1;
  let props :: <integer> = ^properties(e);
  ^properties(e) := logior(ash(offset, slotdiscrim$v-offset), logand(props, mask));
  let callbacks :: <simple-object-vector> = $engine-node-callback-names;
  if (~(element(callbacks, logand(ash(props, - properties$v-entry-type),
				  ash(1, properties$s-entry-type) - 1))))
    // ^engine-node-raw-integer(e) := offset
    ^engine-node-callback(e) := offset
  end if;
  offset
end function;

define function ^callback-slot-engine-node-offset (e :: <&slot-access-engine-node>)
 => (offset :: <integer>);
  ash(^properties(e), - slotdiscrim$v-offset)
end function;
*/


//// METHOD

define method method-binding-and-library (model :: <&method>)
 => (binding, library)
  values(model-variable-binding(model), model-library(model))
end method;

define method method-number
    (defn :: <method-definition>) => (number :: <integer>)
  // the position of this defn among the method definitions for this gf 
  // defined in this library.
  local-definition-number(defn, <method-definition>)
end method;

define function domain-number
    (defn :: <domain-definition>) => (number :: <integer>)
  local-definition-number(defn, <domain-definition>)
end function;


define function local-definition-number (defn :: <modifying-form>, kind :: <class>)
 => (number :: <integer>)
  with-dependent-context ($compilation of defn)
    let local-defs
      = untracked-lookup-local-modifying-definitions(form-variable-binding(defn));
    let i :: <integer> = 0;
    block (return)
      for (def in local-defs)
	when (instance?(def, kind))
	  when (def == defn)
	    return(i)
	  end when;
	  i := i + 1;
	end when;
      end for;
      error("UNABLE TO FIND METHOD %= IN LOCAL DEFINITIONS %=", 
	    defn, local-defs);
    end block
  end;
end function;


define method method-number
    (defn :: <constant-definition>) => (res :: singleton(#f))
  #f
end method;

define method lookup-method-by-number
    (binding :: <module-binding>, index :: <integer>)
 => (res :: <&method>)
  let local-models
    = untracked-lookup-certain-local-modifying-models
        (binding-variable-name(binding), method-definition?);
  let i :: <integer> = 0;

  let model =
    block (return)
      for (model in local-models)
	when (instance?(model, <&method>))
	  when (index = i)
	    return(model)
	  end when;
	  i := i + 1;
	end when;
      end for
    end block;
  model
end method;

define class <dood-cross-method-proxy> (<dood-cross-model-proxy>)
  constant slot dood-proxy-method-index :: false-or(<integer>), 
    required-init-keyword: index:;
  constant slot dood-proxy-method-library :: false-or(<library>), 
    required-init-keyword: library:;
end class;

define method dood-make-cross-method-proxy
    (dood :: <dood>, class :: subclass(<dood-cross-method-proxy>), 
     object :: <&method>) 
=> (proxy :: <dood-cross-method-proxy>)
  let (binding, library) = method-binding-and-library(object);
  make(class,
       binding: binding,
       library: language-definition(library),
       index:   method-number(model-definition(object)))
end method;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&method>) => (proxy)
  dood-make-cross-method-proxy(dood, <dood-cross-method-proxy>, object)
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-method-proxy>) => (object)
  let index = dood-proxy-method-index(proxy);
  if (index)
    with-dood-context (namespace-original-library(dood-proxy-method-library(proxy)))
      lookup-method-by-number(dood-proxy-binding(proxy), index)
    end with-dood-context;
  else
    next-method();
  end;
end method;

/// IEP

define class <dood-cross-model-iep-proxy> (<dood-cross-method-proxy>)
end class;

define method dood-make-binding-value-proxy
    (dood :: <dood>, object :: <&iep>) => (proxy)
  dood-make-cross-method-proxy
    (dood, <dood-cross-model-iep-proxy>, function(object))
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-cross-model-iep-proxy>) => (object)
  ^iep(next-method())
end method;

// eof
