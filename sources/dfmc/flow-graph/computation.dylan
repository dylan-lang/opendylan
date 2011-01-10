Module:   dfmc-flow-graph
Author:   Jonathan Bachrach, Keith Playford, and Paul Haahr
Synopsis: computation classes -- dylan flow machine program graph nodes
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract dood-class <computation> (<queueable-item-mixin>)

  slot computation-source-location :: false-or(<source-location>) 
         = parent-source-location(),
    init-keyword: source-location:;

  weak slot previous-computation :: false-or(<computation>) = #f,
    reinit-expression: #f,
    init-keyword: previous-computation:;

  slot next-computation :: false-or(<computation>) = #f,
    init-keyword: next-computation:;

  slot temporary :: false-or(<temporary>) = #f, 
    init-keyword: temporary:;

  // ONLY NEED THIS IF BACK-END NEEDS TO KNOW VARIABLES IN SCOPE
  // slot lexical-environment :: false-or(<local-lexical-environment>) = #f, 
  //  /* required- */ init-keyword: lexical-environment:;

  weak slot environment :: false-or(<lambda-lexical-environment>),
    reinit-expression: #f,
    required-init-keyword: environment:;

  weak slot computation-type = #f,
    reinit-expression: #f;

end dood-class <computation>;

// Seal construction over the <computation> world.

define sealed domain make (subclass(<computation>));
define sealed domain initialize (<computation>);
define sealed domain initialize-packed-slots (<computation>);

// TODO: CORRECTNESS: Why this type-union?

define generic computation-value 
    (computation :: type-union(<computation>, <binding-reference>)) => value;

//// <computation> operations

define generic next-computation (computation :: <computation>)
 => (next);
define generic next-computation-setter
    (next, computation :: <computation>)
 => (next);
define generic previous-computation (computation :: <computation>)
 => (previous);
define generic previous-computation-setter
    (previous, computation :: <computation>)
 => (previous);

//// <computation> initialization

define inline method make-in-environment 
   (env :: <environment>, class :: <class>, #rest initargs, #key, #all-keys)
  let lambda-environment = lambda-environment(env);
  apply(make, class,
        environment: lambda-environment,
        // lexical-environment: env,
        initargs);
end method;

define inline method make-with-temporary
    (env :: <environment>, class :: <class>,
     #rest initargs,
     #key temporary-class = <temporary>, #all-keys)
 => (computation :: <computation>, temporary :: false-or(<temporary>))
  let lambda-environment = lambda-environment(env);
  let computation = apply(make, class,
			  environment: lambda-environment,
                          // lexical-environment: env,
			  initargs);
  let the-temporary =
    temporary-class &
      make(temporary-class, 
	   generator:   computation,
	   environment: lambda-environment);
  computation.temporary := the-temporary;
  values(computation, the-temporary)
end method;

// define method initialize
//     (computation :: <computation>, #key environment, #all-keys)
//   next-method();
//   lambda-environment(lexical-environment(computation)) := environment;
// end method;

//// <temporary> tracking

define /* inline */ method make
    (class :: subclass(<computation>), #rest initargs, #key, #all-keys)
 => (object)
  let c = next-method();
  register-used-temporaries(c);
  c
end method;

define inline function register-used-temporaries (c :: <computation>)
  do-used-temporaries(method (t) add-user!(t, c) end,
		      c);
end;

define class <temporary-accessors> (<object>)
  constant slot temporary-getter :: <function>, 
    required-init-keyword: getter:;
  constant slot temporary-zetter :: <function>, 
    required-init-keyword: setter:;
end class;

define sealed domain make (singleton(<temporary-accessors>));
define sealed domain initialize (<temporary-accessors>);


define inline function do-used-temporaries
    (fn :: <function>, c :: <computation>)
  for (accessors :: <temporary-accessors> in c.used-temporary-accessors)
    let getter = temporary-getter(accessors);
    let t = getter(c);
    if (instance?(t, <sequence>))
      for (t in t)
	fn(t);
      end;
    else
      fn(t);
    end;
  end;
end;

define inline function do-used-value-references
    (fn :: <function>, c :: <computation>)
  do-used-temporaries(fn, c)
end function;

////
//// Computation accessor methods
////

define method used-temporary-accessors
    (c :: <computation>) => (res :: <simple-object-vector>)
  #[]
end method;

define method class-used-temporary-accessors 
    (c :: subclass(<computation>)) => (res :: <simple-object-vector>)
  #[]
end method;

////
//// <computation> classes
////

/// NOP

define abstract class <nop-computation> (<computation>)
end class <nop-computation>;

define class <nop> (<nop-computation>)
end class <nop>;

/// REFERENCE

define class <binding-reference> (<object>)
end class;

define class <module-binding-reference> (<binding-reference>)
  /* constant */ slot referenced-binding :: <module-binding>, 
    required-init-keyword:  value:;
end class;

// define method referenced-binding (c :: <module-binding-reference>)
//   local-binding-in-requesting-library(c.%referenced-binding);
// end method;

define method computation-value 
    (reference :: <module-binding-reference>) => (cv)
  reference.referenced-binding.binding-value-slot
end method;

define class <variable-reference> (<computation>, <module-binding-reference>)
end class;

define class <defined-constant-reference>
    (<value-reference>, <module-binding-reference>)
end class;

define class <interactor-binding-reference>
    (<value-reference>, <binding-reference>)
  constant slot referenced-binding :: <interactor-binding>,
    required-init-keyword: value:;
end class;                                                                     

define class <object-reference> (<value-reference>)
  slot reference-value, required-init-keyword: value:;
end class;

define class <immutable-object-reference> (<object-reference>)
end class;

define /* inline */ method make
    (class == <object-reference>, #rest initargs, #key value, #all-keys)
 => (object)
  if (instance?(value, <&method>))
    apply(make, <method-reference>, initargs)
  else
    next-method()
  end if
end method;

define class <method-reference> (<object-reference>)
end class;

/// TODO: COULD SPLIT OUT COMPUTATION-SIGNATURE-VALUE

define graph-class <make-closure> (<computation>)
  slot computation-closure-method :: <&method>, 
    required-init-keyword: method:;
  slot computation-init-closure :: false-or(<initialize-closure>) = #f,
    init-keyword: init-closure:;
  temporary slot computation-signature-value = #f, 
    init-keyword: signature:;
end graph-class;

define leaf packed-slots item-properties (<make-closure>, <queueable-item-mixin>)
  boolean slot computation-no-free-references? = #f;
  boolean slot closure-has-dynamic-extent? = #f;
end packed-slots;

define function method-top-level? (m :: <&method>) => (well? :: <boolean>)
  model-has-definition?(m)
end function;

define function computation-top-level-closure? 
    (c :: <make-closure>) => (res :: <boolean>)
  method-top-level?(computation-closure-method(c))
end function;

define function computation-init-closure? 
    (c :: <make-closure>) => (res :: <boolean>)
  ~computation-init-closure(c)
end function;

define method initialize
    (computation :: <make-closure>, 
     #rest all-keys, #key method: the-method, #all-keys)
  next-method();
  apply(initialize-packed-slots, computation, all-keys);
  add-user!(the-method, computation);
end method;

define graph-class <initialize-closure> (<computation>)
  slot computation-closure-method :: <&method>, 
    required-init-keyword: method:;
  temporary slot computation-closure :: false-or(<value-reference>),
    required-init-keyword: closure:;
end graph-class;

/// ASSIGNMENT

define abstract graph-class <assignment> (<computation>)
  temporary slot computation-value :: false-or(<value-reference>), 
    required-init-keyword: value:;
  constant slot %assigned-binding :: <binding>, 
    required-init-keyword: binding:;
end graph-class;

define function assigned-binding (c :: <assignment>)
  let binding = c.%assigned-binding;
  if (instance?(binding, <module-binding>))
    local-binding-in-requesting-library(binding)
  else
    binding
  end
end function;

define abstract graph-class <any-definition> (<assignment>)
end graph-class <any-definition>;

define graph-class <definition> (<any-definition>)
end graph-class <definition>;

// Only used in interactive mode...
define graph-class <redefinition> (<any-definition>)
end graph-class <redefinition>;

define graph-class <set!> (<assignment>)
end graph-class <set!>;

define graph-class <conditional-update!> (<assignment>)
  temporary slot computation-test-value, init-keyword: test-value:;
end graph-class <conditional-update!>;

/// TYPE REFERENCE

define class <type-reference> (<value-reference>)
  constant slot %typed-binding :: <module-binding>, 
    required-init-keyword:  binding:;
end class;

define method typed-binding (c :: <type-reference>)
  let binding = c.%typed-binding;
  if (instance?(binding, <module-binding>))
    local-binding-in-requesting-library(binding)
  else
    binding
  end
end method;

/// TYPE ASSIGNMENT

// This is distinct from <assignment> just to be safe since we're
// changing the sense of the assigned-binding slot, defining its
// type rather than its value.

define abstract graph-class <any-type-definition> (<computation>)
  temporary slot computation-value :: false-or(<value-reference>), 
    required-init-keyword: value:;
  constant slot %typed-binding :: <binding>, 
    required-init-keyword: binding:;
end graph-class;

define method typed-binding (c :: <any-type-definition>)
  let binding = c.%typed-binding;
  if (instance?(binding, <module-binding>))
    local-binding-in-requesting-library(binding)
  else
    binding
  end
end method;

define graph-class <type-definition> (<any-type-definition>)
end graph-class;

// Only used in interactive mode...
define graph-class <type-redefinition> (<any-type-definition>)
end graph-class;

define abstract graph-class <merge> (<computation>)
end graph-class;

/// TEMPORARY-TRANSFER

define abstract graph-class <temporary-transfer-computation> (<computation>)
  temporary slot computation-value :: false-or(<value-reference>),
    required-init-keyword: value:;
end graph-class;

define graph-class <temporary-transfer> (<temporary-transfer-computation>)
end graph-class;

define graph-class <keyword-default> (<temporary-transfer-computation>)
  temporary slot keyword-default-value-keyword-variable :: false-or(<value-reference>),
                 required-init-keyword: keyword-variable:;
  constant slot keyword-default-value-specifiers :: <simple-object-vector>,
       required-init-keyword: specifiers:;
end graph-class;

// TODO: SPECIFY MAX NUMBER OF PARAMETERS LIMIT
define leaf packed-slots item-properties
    (<keyword-default>, <queueable-item-mixin>)
  field slot keyword-default-value-index = 0, field-size: 8,
    required-init-keyword: index:;
end packed-slots;

define /* inline */ method initialize
    (c :: <keyword-default>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, c, all-keys);
end method;

/// MERGE

define abstract graph-class <binary-merge> (<merge>, <nop-computation>)
  temporary slot merge-left-value :: false-or(<value-reference>),
    required-init-keyword: left-value:;
  temporary slot merge-right-value :: false-or(<value-reference>),
    required-init-keyword: right-value:;
  slot merge-left-previous-computation :: false-or(<computation>) = #f,
    init-keyword: left-previous-computation:;
  slot merge-right-previous-computation :: false-or(<computation>) = #f,
    init-keyword: right-previous-computation:;
end graph-class;

define graph-class <if-merge> (<binary-merge>)
end graph-class;

define graph-class <loop-merge> (<binary-merge>)
end graph-class;

define leaf packed-slots item-properties (<loop-merge>, <queueable-item-mixin>)
  // FIRST SELF-TAIL CALL?
  boolean slot loop-merge-initial? = #t;
end packed-slots;

define constant loop-merge-loop
  = merge-left-previous-computation;
define constant loop-merge-loop-setter      
  = merge-left-previous-computation-setter;
define constant loop-merge-call
  = merge-right-previous-computation;
define constant loop-merge-call-setter 
  = merge-right-previous-computation-setter;

define constant loop-merge-parameter
  = merge-left-value;
define constant loop-merge-parameter-setter
  = merge-left-value-setter;
define constant loop-merge-argument
  = merge-right-value;
define constant loop-merge-argument-setter
  = merge-right-value-setter;

define /* inline */ method make
    (class :: subclass(<loop-merge>), #rest all-keys,
     #key loop, call, parameter, argument)
 => (res :: <loop-merge>)
  apply(next-method, class, 
        left-value: parameter, right-value: argument,
        left-previous-computation:  loop, 
        right-previous-computation: call,
        all-keys)
end method;

define inline method initialize
    (c :: <loop-merge>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, c, all-keys);
end method;

define graph-class <bind-exit-merge> (<binary-merge>)
end graph-class;

//define constant bind-exit-merge-block-return-temporary
//  = merge-left-value;
define constant bind-exit-merge-body-temporary
  = merge-right-value;


/// SLOT-VALUE

define abstract graph-class <any-slot-value> (<computation>)
  slot computation-slot-descriptor :: <&slot-descriptor>, 
    required-init-keyword: slot-descriptor:;
  temporary slot computation-instance :: <value-reference>,
    required-init-keyword: instance:;
end graph-class;

define constant $log-max-number-slots = 16;
// define constant $max-number-slots     = 2 ^ $log-max-number-slots;

define packed-slots item-properties (<any-slot-value>, <queueable-item-mixin>)
  boolean slot computation-guaranteed-initialized? = #f, 
   init-keyword: guaranteed-initialized?:;
  field   slot computation-slot-offset = 0, field-size: $log-max-number-slots,
   init-keyword: slot-offset:;
end packed-slots;

define inline method initialize
    (c :: <any-slot-value>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, c, all-keys);
end method;

define compiler-open generic computation-repeated-byte? 
  (c :: <any-repeated-slot-value>) => (res :: <boolean>);

define abstract graph-class <any-repeated-slot-value> (<any-slot-value>)
  temporary slot computation-index :: <value-reference>,
    required-init-keyword: index:;
end graph-class;

define leaf packed-slots item-properties (<any-repeated-slot-value>, <any-slot-value>)
  boolean slot computation-index-tagged? = #f, init-keyword: index-tagged?:;
end packed-slots;

define graph-class <slot-value> (<any-slot-value>)
end graph-class;

define graph-class <slot-value-setter> (<any-slot-value>)
  temporary slot computation-new-value :: false-or(<value-reference>),
    required-init-keyword: new-value:;
end graph-class;

define graph-class <repeated-slot-value> (<any-repeated-slot-value>)
end graph-class;

define graph-class <repeated-slot-value-setter> (<any-repeated-slot-value>)
  temporary slot computation-new-value :: <value-reference>,
    required-init-keyword: new-value:;
end graph-class;

/// CALL

define constant $compatibility-unchecked            = 0;
define constant $compatibility-checked-compatible   = 1;
define constant $compatibility-checked-incompatible = 2;

define abstract graph-class <call> (<computation>)
  temporary slot arguments :: <simple-object-vector>, 
    required-init-keyword: arguments:;
end graph-class;

define packed-slots item-properties (<call>, <queueable-item-mixin>)
  field slot compatibility-state = $compatibility-unchecked, field-size: 2;
end packed-slots;

define inline method initialize
    (call :: <call>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, call, all-keys);
end method;

define graph-class <stack-vector> (<call>)
end graph-class;

/// FUNCTION CALL

define constant $dispatch-untried = 0;
define constant $dispatch-tried   = 1;

define abstract graph-class <function-call> (<call>)
  temporary slot function :: false-or(<value-reference>), 
    required-init-keyword: function:;
end graph-class;

define packed-slots item-properties (<function-call>, <call>)
  field   slot dispatch-state  = $dispatch-untried, field-size: 1;
  boolean slot call-congruent? = #f;
  boolean slot call-iep?       = #f;
end packed-slots;

/// PRIMITIVE CALL

define graph-class <primitive-call> (<call>)
  slot primitive :: false-or(<&primitive>), required-init-keyword: primitive:;
end graph-class;

/// SIMPLE CALL -- a direct call to a function, with explicit arguments

define thread variable *inlining-depth* = 0;

define graph-class <simple-call> (<function-call>) 
end graph-class;

define constant $log-max-inlining-depth = 4;
define constant $max-inlining-depth     = ash(1, $log-max-inlining-depth) - 1;

define leaf packed-slots item-properties (<simple-call>, <function-call>)
  field slot call-inlining-depth = 0, field-size: $log-max-inlining-depth;
end packed-slots;

/// CONGRUENT CALL

define graph-class <congruent-call-mixin> (<call>)
end graph-class;

define method call-congruent? (c :: <congruent-call-mixin>) => (well? :: <boolean>)
  #t
end method;

/// METHOD CALL

define graph-class <method-call> (<congruent-call-mixin>, <simple-call>)
  temporary slot next-methods :: <value-reference>,
                 required-init-keyword: next-methods:;
end graph-class;

define /* inline */ method make
    (call-class == <method-call>, 
     #rest all-keys, #key next-methods, arguments, #all-keys)
 => (object)
  if (next-methods)
    next-method()
  else
    apply(next-method,  call-class, 
          next-methods: first(arguments), 
          arguments:    copy-sequence(arguments, start: 1), 
          all-keys)
  end if;
end method;


define graph-class <engine-node-call> (<congruent-call-mixin>, <simple-call>)
  temporary slot engine-node :: <value-reference>,
                 required-init-keyword: engine-node:;
end graph-class;


/// APPLY

define graph-class <apply> (<function-call>) end graph-class;

define graph-class <method-apply> (<congruent-call-mixin>, <apply>) 
  temporary slot next-methods :: <value-reference>,
                 required-init-keyword: next-methods:;
end graph-class;

define /* inline */ method make
    (call-class == <method-apply>, 
      #rest all-keys, #key next-methods, arguments, #all-keys)
 => (object)
  if (next-methods)
    next-method()
  else
    apply(next-method,  call-class, 
          next-methods: first(arguments), 
          arguments:    copy-sequence(arguments, start: 1), 
          all-keys)
  end
end method;


define graph-class <engine-node-apply> (<congruent-call-mixin>, <apply>)
  temporary slot engine-node :: <&engine-node>,
                 required-init-keyword: engine-node:;
end graph-class;
  

/// IF

define graph-class <if> (<computation>)
  temporary slot test :: <value-reference>, required-init-keyword: test:;
  slot consequent :: false-or(<computation>),
    required-init-keyword: consequent:;
  slot alternative :: false-or(<computation>),
    required-init-keyword: alternative:;
end graph-class;

/// BLOCK

define abstract graph-class <block> (<computation>)
  temporary slot entry-state :: <entry-state>,
    required-init-keyword: entry-state:;
  slot body :: false-or(<computation>) = #f,
    init-keyword: body:;
end graph-class;

/// BIND-EXIT

define graph-class <bind-exit> (<block>)
  slot %label, init-value: #f;
end graph-class;

/// UNWIND-PROTECT

define graph-class <unwind-protect> (<block>)
  // (gts,98feb12) temporary slot protected-temporary = #f;
  slot protected-end :: false-or(<computation>) = #f;
  slot cleanups :: false-or(<computation>) = #f,
    init-keyword: cleanups:;
  slot cleanups-end :: false-or(<end-cleanup-block>) = #f;  // to support deletion 
end graph-class;

define method protected-temporary(c :: <unwind-protect>) 
    => (t)
//  let temp = c.protected-end & temporary(previous-computation(c.protected-end));
  let temp = c.protected-end & return-temp(c.protected-end);
  if (temp & temp.used?)
    temp
  else
    #f
  end if;
end method;

define function has-cleanups?(c :: <unwind-protect>)
  c.cleanups
    & (previous-computation(c.cleanups-end) ~== c)
end function;

define function has-body?(c :: <unwind-protect>)
  ~instance?(c.body, <end-protected-block>)
end function;


/// TERMINATING COMPUTATIONS

define abstract graph-class <end> (<computation>) end graph-class;

define graph-class <return> (<end>)
  temporary slot computation-value :: false-or(<value-reference>), 
    required-init-keyword: value:;
end graph-class;

define graph-class <exit> (<end>)
  temporary slot computation-value :: <value-reference>,
                 required-init-keyword: value:;
  temporary slot entry-state :: <entry-state>,
                 required-init-keyword: entry-state:;
end graph-class;

define abstract graph-class <end-block> (<computation>)
  temporary slot entry-state :: <entry-state>,
                 required-init-keyword: entry-state:;
end graph-class <end-block>;

define graph-class <end-exit-block> (<end-block>)
  // This is a terminating computation, because it terminates execution of
  // an interpreter thread, but not an <end>, because it doesn't terminate
  // control flow in the DFM graph -- there is a next-compution, which is
  // outside of the block.  For data flow, that's right.
end graph-class <end-exit-block>;

define graph-class <end-protected-block> (<end-block>, <end>)
  temporary slot return-temp :: false-or(<value-reference>) = #f;      // gts,98feb04
end graph-class <end-protected-block>;

define graph-class <end-cleanup-block> (<end-block>, <end>)
end graph-class <end-cleanup-block>;

/// LOOP

define graph-class <loop> (<nop-computation>)
  slot %label, init-value: #f;
  slot loop-merges :: <simple-object-vector>,
    required-init-keyword: merges:;
  slot loop-body :: false-or(<computation>), init-value: #f,
    init-keyword: body:;
end graph-class;

define graph-class <end-loop> (<end>)
  constant slot ending-loop :: <loop>, required-init-keyword: loop:;
end graph-class;

define function loop-parameters (c :: <loop>)
  collecting ()
    for (merge in loop-merges(c))
      collect(loop-merge-parameter(merge))
    end for;
  end collecting;
end function;

define function loop-call-arguments (c :: <loop-call>)
  collecting ()
    for (merge in loop-call-merges(c))
      collect(loop-merge-argument(merge))
    end for;
  end collecting;
end function;

/// LOOP CALL

define graph-class <loop-call> (<computation>)
  constant slot loop-call-loop :: <loop>, 
   required-init-keyword: loop:;
  slot loop-call-merges :: <simple-object-vector> = #[], 
   init-keyword: merges:;
end graph-class;

/// BIND

define graph-class <bind> (<computation>)
  slot bind-return :: <return>;
end graph-class;

define method previous-computation-setter 
    (new-value :: false-or(<computation>), c :: <bind>) => (new-value)
  if (new-value)
    error("<bind> computations may not have previous-computations");
  end if;
  next-method();
end method previous-computation-setter;

/// MULTIPLE VALUES

define graph-class <values> (<computation>)
  temporary slot fixed-values :: <simple-object-vector>, 
    required-init-keyword: values:;
  temporary slot rest-value :: false-or(<value-reference>)= #f, 
    init-keyword: rest-value:;
end graph-class <values>;

define abstract graph-class <extract-value-computation> (<computation>) 
  temporary slot computation-value :: <multiple-value-temporary>,
    required-init-keyword: value:;
end graph-class <extract-value-computation>;

define inline method initialize
    (c :: <extract-value-computation>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, c, all-keys);
end method;

// TODO: SPECIFY MAX NUMBER OF VALUES LIMIT
define packed-slots item-properties (<extract-value-computation>, <queueable-item-mixin>)
  field slot index = 0, field-size: 8;
end packed-slots;

define graph-class <extract-single-value> (<extract-value-computation>) 
end graph-class <extract-single-value>;

define leaf packed-slots item-properties (<extract-single-value>, <extract-value-computation>)
  boolean slot extract-value-index-guaranteed? = #f;
end packed-slots;

define graph-class <extract-rest-value> (<extract-value-computation>) 
end graph-class <extract-rest-value>;

define graph-class <multiple-value-spill> (<temporary-transfer-computation>)
end graph-class <multiple-value-spill>;

define graph-class <multiple-value-unspill> (<temporary-transfer-computation>)
end graph-class <multiple-value-unspill>;

define abstract graph-class <adjust-multiple-values-computation> 
    (<temporary-transfer-computation>)
end graph-class <adjust-multiple-values-computation>;

define inline method initialize
    (c :: <adjust-multiple-values-computation>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, c, all-keys);
end method;

// TODO: SPECIFY MAX NUMBER OF VALUES LIMIT
define leaf packed-slots item-properties
    (<adjust-multiple-values-computation>, <queueable-item-mixin>)
  field slot number-of-required-values = 0, field-size: 8,
    required-init-keyword: number-of-required-values:;
end packed-slots;

define graph-class <adjust-multiple-values>
  (<adjust-multiple-values-computation>)
end graph-class <adjust-multiple-values>;

define graph-class <adjust-multiple-values-rest>
  (<adjust-multiple-values-computation>)
end graph-class <adjust-multiple-values-rest>;

/// TYPES

define abstract graph-class <check-type-computation> 
    (<temporary-transfer-computation>)
end graph-class;

define abstract graph-class <single-value-check-type-computation>
    (<check-type-computation>)
  temporary slot type :: false-or(<value-reference>), 
    required-init-keyword: type:;
end graph-class;

define graph-class <check-type> (<single-value-check-type-computation>)
end graph-class;

// added this to allow back-end's discriminate this case so as to force
// emission of these in dylan library for size :: <integer> for vectors
define graph-class <keyword-check-type> (<check-type>)
end graph-class;

define graph-class <constrain-type> (<single-value-check-type-computation>)
 end graph-class;

// keep the original variable name around for meaningful
// error messages
define graph-class <assignment-check-type> (<check-type>)
  constant slot lhs-variable-name,
    required-init-keyword: lhs-variable-name:;
end graph-class;

define abstract graph-class <multiple-value-check-type-computation>
  (<check-type-computation>)
  temporary slot types :: <simple-object-vector>, required-init-keyword: types:;
end graph-class;

define graph-class <multiple-value-check-type>
    (<multiple-value-check-type-computation>)
end graph-class;

define graph-class <multiple-value-check-type-rest>
    (<multiple-value-check-type-computation>)
  temporary slot rest-type :: false-or(<value-reference>), 
    required-init-keyword: rest-type:;
end graph-class;

define abstract graph-class <result-check-type-computation> 
    (<check-type-computation>)
end graph-class;

define abstract graph-class <single-value-result-check-type> 
    (<single-value-check-type-computation>, <result-check-type-computation>)
end graph-class;

define graph-class <multiple-value-result-check-type>
    (<multiple-value-check-type>, <result-check-type-computation>)
end graph-class;

define graph-class <multiple-value-result-check-type-rest>
    (<multiple-value-check-type-rest>, <result-check-type-computation>)
end graph-class;

define graph-class <guarantee-type> (<temporary-transfer-computation>)
  temporary slot guaranteed-type :: false-or(<value-reference>),
    init-value: #f,
    init-keyword: type:;
  slot static-guaranteed-type :: false-or(<&type>),
    init-value: #f,
    init-keyword: static-type:;
end graph-class;

/// INDIRECT ASSIGNMENT

define graph-class <make-cell> (<computation>)
  temporary slot computation-value :: false-or(<value-reference>), 
    required-init-keyword: value:;
end graph-class <make-cell>;

define graph-class <get-cell-value> (<computation>)
  temporary slot computation-cell :: <cell>, 
    required-init-keyword: cell:;
end graph-class <get-cell-value>;

define graph-class <set-cell-value!> (<computation>)
  temporary slot computation-cell :: <cell>, 
    required-init-keyword: cell:;
  temporary slot computation-value :: false-or(<value-reference>), 
    required-init-keyword: value:;
end graph-class <set-cell-value!>;

/// SOURCE LOCATION PROTOCOL

define generic dfm-source-location 
    (c :: <computation>) => (location-or-false);

define method dfm-source-location (c :: <computation>) => (location)
  computation-source-location(c)
end method;

define compiler-open generic dfm-context-id
    (c :: type-union(<computation>, <environment>)) 
 => (context-id-or-false);

define inline function do-with-parent-computation 
    (f :: <function>, c :: false-or(<computation>))
  with-parent-source-location (c & computation-source-location(c))
    f();
  end;
end function;

define macro with-parent-computation
  { with-parent-computation (?c:expression) ?:body end }
    => { do-with-parent-computation(method () ?body end, ?c) }
end macro;
