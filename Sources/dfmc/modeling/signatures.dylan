module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $max-signature-size              = 256;
define constant $max-default-signature-size      = 16;
define constant $max-mini-default-signature-size = 2;

define function source-constructor-for-$signature-types (types-name, size)
  let types = ^make(<simple-object-vector>, size: size);
  // initialization is completed in compute-signature at which
  // the <object> definition is available
  let types-form-name = make-variable-name-fragment(types-name);
  #{ define constant ?types-form-name = ?types; }
end function;

define class <signature-type-vector> (<object>)
  constant slot signature-type-vector-type-name :: <symbol>, 
    required-init-keyword: type-name:;
  constant slot signature-type-vector-definition-name :: <symbol>, 
    required-init-keyword: definition-name:;
  constant slot signature-type-vector-size :: <integer>, 
    required-init-keyword: size:;
end class;

define function fill-model-objects (stv :: <signature-type-vector>)
  local method safe-dylan-value (name)
	  block ()
	    dylan-value(name)
	  exception (<error>)
	    dylan-value(#"<object>")
	  end block
	end method;
  let type-name = signature-type-vector-type-name(stv);
  let defn-name = signature-type-vector-definition-name(stv);
  let types     = dylan-value(defn-name);
  let type      = safe-dylan-value(type-name);
  fill!(types, type);
  // HACK: SHOULDN'T BE NECESSARY
  types.model-definition := types.model-creator
end function;

define function ensure-signature-type-vector-initialized
    (x :: <simple-object-vector>)
  unless (x[0])
    for (stv in $signature-type-vectors)
      fill-model-objects(stv);
    end for;
  end unless;
  x
end function;

define function compute-signature-type-vector-definition-name 
    (type-name :: <symbol>) => (definition-name :: <symbol>)
  as(<symbol>, format-to-string("$signature-%s-types", type-name))
end function;

define macro signature-type-vector-table-definer
  { define signature-type-vector-table ?table-name:name = { ?entries } }
    => { define constant ?table-name :: <table> = make(<table>);
         begin let the-table = ?table-name; ?entries end; }
 entries:
  { } => { }
  { ?:name => ?size:expression, ... }
    => { the-table[ ?#"name" ] 
	   := make(<signature-type-vector>, 
		   type-name: 
                     ?#"name",
		   definition-name: 
                     compute-signature-type-vector-definition-name(?#"name"),
		   size: 
                     ?size); 
          ... }
end macro signature-type-vector-table-definer;

define signature-type-vector-table $signature-type-vectors = {
  // MOST-USED DRM CLASS

  <object> =>                          $max-signature-size,  

  // OFT-USED DRM CLASSES

  <boolean> =>                         $max-default-signature-size,
  <machine-word> =>                    $max-default-signature-size,
  <byte-character> =>                  $max-default-signature-size,
  <abstract-integer> =>                $max-default-signature-size,
  <big-integer> =>                     $max-default-signature-size,
  <double-integer> =>                  $max-default-signature-size,
  <integer> =>                         $max-default-signature-size,
  <number> =>                          $max-default-signature-size,
  <single-float> =>                    $max-default-signature-size,
  <double-float> =>                    $max-default-signature-size,
  <byte-string> =>                     $max-default-signature-size,
  <simple-object-vector> =>            $max-default-signature-size,
  <list> =>                            $max-default-signature-size,
  <empty-list> =>                      $max-default-signature-size,
  <pair> =>                            $max-default-signature-size,

  // REST OF DRM CLASSES

  <function> =>                        $max-mini-default-signature-size,
  <method> =>                          $max-mini-default-signature-size,
  <generic-function> =>                $max-mini-default-signature-size,
  <type> =>                            $max-mini-default-signature-size,
  <class> =>                           $max-mini-default-signature-size,
  <singleton> =>                       $max-mini-default-signature-size,
  <collection> =>                      $max-mini-default-signature-size,
  <mutable-collection> =>              $max-mini-default-signature-size,
  <stretchy-collection> =>             $max-mini-default-signature-size,
  <sequence> =>                        $max-mini-default-signature-size,
  <mutable-sequence> =>                $max-mini-default-signature-size,
  <explicit-key-collection> =>         $max-mini-default-signature-size,
  <mutable-explicit-key-collection> => $max-mini-default-signature-size,
  <array> =>                           $max-mini-default-signature-size,
  <deque> =>                           $max-mini-default-signature-size,
  <range> =>                           $max-mini-default-signature-size,
  <string> =>                          $max-mini-default-signature-size,
  <unicode-string> =>                  $max-mini-default-signature-size,
  <table> =>                           $max-mini-default-signature-size,
  <object-table> =>                    $max-mini-default-signature-size,
  <vector> =>                          $max-mini-default-signature-size,
  <stretchy-vector> =>                 $max-mini-default-signature-size,
  <simple-vector> =>                   $max-mini-default-signature-size,
  <condition> =>                       $max-mini-default-signature-size,
  <serious-condition> =>               $max-mini-default-signature-size,
  <error> =>                           $max-mini-default-signature-size,
  <simple-error> =>                    $max-mini-default-signature-size,
  <type-error> =>                      $max-mini-default-signature-size,
  <sealed-object-error> =>             $max-mini-default-signature-size,
  <warning> =>                         $max-mini-default-signature-size,
  <simple-warning> =>		       $max-mini-default-signature-size,
  <restart> =>			       $max-mini-default-signature-size,
  <simple-restart> =>		       $max-mini-default-signature-size,
  <abort> =>			       $max-mini-default-signature-size,
  <complex> =>			       $max-mini-default-signature-size,
  <real> =>			       $max-mini-default-signature-size,
  <rational> =>			       $max-mini-default-signature-size,
  <float> =>			       $max-mini-default-signature-size,
  // <extended-float> =>	       $max-mini-default-signature-size,

  // DYLAN EXTENSIONS

  <bottom> =>                          $max-mini-default-signature-size,
  <bottom-type> =>	               $max-mini-default-signature-size,
  <format-string-condition> =>	       $max-mini-default-signature-size,
  <simple-condition> =>		       $max-mini-default-signature-size,
  <stretchy-sequence> =>	       $max-mini-default-signature-size,
  <not-found-error> =>		       $max-mini-default-signature-size,
  <unbound> =>			       $max-mini-default-signature-size,
  <slot-descriptor> =>		       $max-mini-default-signature-size,
  <repeated-slot-descriptor> =>	       $max-mini-default-signature-size,
  <union> =>			       $max-mini-default-signature-size,
  <limited-integer> =>		       $max-mini-default-signature-size,
  <signature> =>		       $max-mini-default-signature-size,
  <limited-type> =>		       $max-mini-default-signature-size,
  <slot-descriptor> =>		       $max-mini-default-signature-size,
  <repeated-slot-descriptor> =>	       $max-mini-default-signature-size,
  <obsolete-instance> =>	       $max-mini-default-signature-size,
  <miscellaneous-obsolete-instance> => $max-mini-default-signature-size,
  <implementation-class> =>	       $max-mini-default-signature-size,
  <subclass> =>			       $max-mini-default-signature-size,
  <set> =>			       $max-mini-default-signature-size,
  <object-set> =>		       $max-mini-default-signature-size,
  <object-deque> =>		       $max-mini-default-signature-size,
  <stretchy-object-vector> =>	       $max-mini-default-signature-size,
  <table-vector> =>		       $max-mini-default-signature-size,
  <string-table> =>		       $max-mini-default-signature-size,
  <hash-state> =>		       $max-mini-default-signature-size
};

define constant $basic-object-types-name
  = signature-type-vector-definition-name
      ($signature-type-vectors[#"<object>"]);

for (sig-type-vec in $signature-type-vectors)
  do-define-core-unadorned-definition
    (signature-type-vector-definition-name(sig-type-vec),
     curry(source-constructor-for-$signature-types, 
	   signature-type-vector-definition-name(sig-type-vec),
	   signature-type-vector-size(sig-type-vec)));
end for;

define inline function ^signature-default-types-if-computed ()
  let binding = dylan-binding($basic-object-types-name);
  untracked-binding-model-object-if-computed(binding)
end function;

define inline function ^signature-default-types ()
  dylan-value($basic-object-types-name);
end function;

define inline function ^signature-default-rest-value ()
  dylan-value(#"<object>")
end function;

define method as-sig-types
    (x :: <sequence>) => (types :: <simple-object-vector>)
  as-sig-types(as(<simple-object-vector>, x))
end method;

define method as-sig-types 
    (x :: <simple-object-vector>) => (types :: <simple-object-vector>)
  let sig-size = size(x);
  if (sig-size > 0 & ^instance?(x[0], dylan-value(#"<class>")))
    let type-name = as(<symbol>, ^debug-name(x[0]));
    let sig-type-vec :: false-or(<signature-type-vector>)
       = element($signature-type-vectors, type-name, default: #f);
    if (sig-type-vec & sig-size <= signature-type-vector-size(sig-type-vec))
      let type = dylan-value(type-name);
      if (every?(curry(\==, type), x))
	let types-name = signature-type-vector-definition-name(sig-type-vec);
	ensure-signature-type-vector-initialized(dylan-value(types-name))
      else
        immutable-model(x)
      end if
    else
      immutable-model(x)
    end if
  else 
    immutable-model(x)
  end if
end method;

//// 
//// SIGNATURE
//// 

define primary &class <signature> (<object>)
  &slot signature-properties :: <integer>,
    init-keyword: properties:,
    init-value: 0;
  runtime-constant &slot signature-required :: <simple-object-vector>,
    required-init-keyword: required:;
end &class;

define primary &class <object-signature> (<signature>)
end &class;

define function compute-signature-definition-name 
    (type-name :: <symbol>, rest-value? :: <boolean>, size :: <integer>) 
 => (definition-name :: <symbol>)
  as(<symbol>, 
     format-to-string
       ("$signature-%s-%s-rest-value-%d", type-name, 
	if (rest-value?) "object" else "no" end, size))
end function;

define constant $object-no-rest-value-signature-definition-names :: <simple-object-vector>
  = make(<vector>, size: $max-default-signature-size);

define constant $object-object-rest-value-signature-definition-names :: <simple-object-vector>
  = make(<vector>, size: $max-default-signature-size);

define function source-constructor-for-$object-signature 
    (sig-name, rest-value?, size)
  let sig = ^make(<&object-signature>, 
		  number-required:     size, 
		  required:            #[], 
                  rest-value?:         rest-value?,
		  default-values?:     #t);
  // initialization is completed in compute-signature at which
  // the <object> definition is available
  let sig-form-name = make-variable-name-fragment(sig-name);
  #{ define constant ?sig-form-name = ?sig; }
end function;

define function object-signature-definition-names
    (rest-value? :: <boolean>) => (res :: <simple-object-vector>)
  if (rest-value?)
    $object-object-rest-value-signature-definition-names;
  else
    $object-no-rest-value-signature-definition-names;
  end if
end function;

define function object-signature-definition-name
    (rest-value? :: <boolean>, size :: <integer>)
  object-signature-definition-names(rest-value?)[size]
end function;

define function object-signature-definition-name-setter
    (name, rest-value? :: <boolean>, size :: <integer>)
  object-signature-definition-names(rest-value?)[size] := name
end function;

define function lookup-object-signature 
    (number-required :: <integer>, rest-value?, rest-value) 
 => (res :: false-or(<&object-signature>))
  let object-sig-types
    = ^signature-default-types();
  let sig-name 
    = object-signature-definition-name(rest-value?, number-required);
  let sig
    = dylan-value(sig-name);
  unless (^signature-required(sig) == object-sig-types)
    for (rest-value? in #[#t, #f])
      for (number-required from 0 below $max-default-signature-size)
	let sig-name = object-signature-definition-name(rest-value?, number-required);
	let sig      = dylan-value(sig-name);
	^signature-required(sig) := object-sig-types;
      end for;
    end for;
  end unless;
  sig
end function;

for (rest-value? in #[#t, #f])
  for (size from 0 below $max-default-signature-size)
    let definition-name
      = compute-signature-definition-name(#"<object>", rest-value?, size);
    object-signature-definition-name(rest-value?, size) := definition-name;
    do-define-core-unadorned-definition
      (definition-name,
       curry(source-constructor-for-$object-signature, 
	     definition-name, rest-value?, size))
  end for;
end for;

define method ^signature-values 
    (sig :: <&signature>) => (res :: <simple-object-vector>)
  if (^signature-default-values?(sig))
    ^signature-default-types()
  else
    #[]
  end if
end method;

define &class <signature-values-mixin> (<signature>)
  runtime-constant &slot signature-values :: <simple-object-vector>,
    required-init-keyword: values:;
end &class;

define method ^signature-rest-value
    (sig :: <&signature>) => (res)
  ^signature-rest-value?(sig)
    & ^signature-default-rest-value()
end method;

define &class <signature-rest-value-mixin> (<signature>)
  runtime-constant &slot signature-rest-value, // :: &false-or(<&type>)
    required-init-keyword: rest-value:;
end &class;

define &class <signature+values> (<signature-values-mixin>, <signature>)
end &class;

define &class <signature+rest-value> 
    (<signature-rest-value-mixin>, <signature>)
end &class;

define &class <signature+values+rest-value> 
    (<signature-rest-value-mixin>, <signature+values>)
end &class;

define primary &class <keyword-signature> (<signature>)
  runtime-constant &slot signature-keys :: <simple-object-vector>,
    required-init-keyword: keys:;
  runtime-constant &slot signature-key-types :: <simple-object-vector>,
    required-init-keyword: key-types:;
end &class;

define method ^signature-keys (sig :: <&signature>)
  #[]
end method;

define &class <keyword-signature+values> 
    (<signature-values-mixin>, <keyword-signature>)
end &class;

define &class <keyword-signature+rest-value> 
    (<signature-rest-value-mixin>, <keyword-signature>)
end &class;

define &class <keyword-signature+values+rest-value> 
    (<signature-rest-value-mixin>, <keyword-signature+values>)
end &class;

define method ^signature-number-keys 
    (sig :: <&keyword-signature>) => (result :: <integer>)
  size(^signature-keys(sig))
end method ^signature-number-keys;

define method ^make
    (class == <&signature>, #rest all-keys, 
     #key number-required, required, key?, 
          values, rest-value?, rest-value, number-values, 
          rest?, next?, sealed-domain?, 
     #all-keys) 
 => (res :: <&signature>)
  let default-values?
    = every?(curry(\==, dylan-value(#"<object>")), values);
  let default-rest-value? 
    = ~rest-value | rest-value == dylan-value(#"<object>");
  if (~key? & default-values? & default-rest-value?)
    let (sig-default-types, sig-default-types-computed?)
      = ^signature-default-types-if-computed();
    if (sig-default-types-computed?
	  & required == sig-default-types
    	  & number-required < $max-default-signature-size
          & ~rest? & ~next? & ~sealed-domain?
          & number-values = 0
	  & (~rest-value? | default-rest-value?))
      lookup-object-signature(number-required, rest-value?, rest-value)
    else
      apply(next-method, <&signature>,
	    default-values?: default-values?, all-keys)
    end if
  else
    apply(^make,
          if (key?)
            if (default-values?)
              if (default-rest-value?)
                <&keyword-signature>
              else 
                <&keyword-signature+rest-value>
              end if
            else            
              if (default-rest-value?)
                <&keyword-signature+values>
              else 
                <&keyword-signature+values+rest-value>
              end if
            end if
          else
            if (default-values?)
              <&signature+rest-value>
            else 
              if (default-rest-value?)
                <&signature+values> 
              else 
                <&signature+values+rest-value> 
              end if 
            end if
          end if,
          default-values?: default-values?,
          all-keys)
   end if;
end method;

/*
define inline method ^pack-signature-properties
    (#rest all-keys, #key, #all-keys)
  apply(compute-initial-packed-slot, 
        0, make(<&signature>, required: #()), all-keys);
end method;
*/

define method ^pack-signature-properties
    (#rest all-keys, #key, #all-keys)
  // HACK: WASTEFUL --- PICK SMALLEST CONCRETE CLASS
  let sig = apply(^make, <&signature+values>, required: #[], values: #[], all-keys);
  // apply(initialize-packed-slots, sig, all-keys);
  ^signature-properties(sig)
end method;

define method ^initialize
    (sig :: <&signature>, #rest all-keys, 
     #key properties, next?, sealed-domain?, default-values?, #all-keys)
  next-method();
  if (properties)
    ^signature-next?(sig)               := next?;
    ^signature-sealed-domain?(sig)      := sealed-domain?;
    ^signature-default-values?(sig)     := default-values?;
  else
    apply(initialize-packed-slots, sig, all-keys)
  end if;
end method;

define leaf packed-slots ^signature-properties (<&signature>, <object>)
  field   slot ^signature-number-required = 0, field-size: 8,
    init-keyword: number-required:;
  field   slot ^signature-number-values   = 0, field-size: 8,
    init-keyword: number-values:;
  boolean slot ^signature-key? = #f,
    init-keyword: key?:;
  boolean slot ^signature-all-keys? = #f,
    init-keyword: all-keys?:;
  boolean slot ^signature-rest? = #f,
    init-keyword: rest?:;
  boolean slot ^signature-rest-value? = #f,
    init-keyword: rest-value?:;
  boolean slot ^signature-next? = #f,
    init-keyword: next?:;
  boolean slot ^signature-default-values? = #f,
    init-keyword: default-values?:;
  boolean slot ^signature-sealed-domain? = #f,
    init-keyword: sealed-domain?:;
  boolean slot ^signature-complete? = #t,
    init-keyword: complete?:;
end packed-slots;

ignore(^signature-next?);
ignore(^signature-complete?);

define method ^signature-optionals? (sig :: <&signature>)
 => (result :: <boolean>)
  ^signature-key?(sig) | ^signature-rest?(sig)
end method ^signature-optionals?;

define method ^signature-number-keys
    (sig :: <&signature>) => (result :: <integer>)
  0
end method ^signature-number-keys;

// <ABORT>, 
// <ABSTRACT-INTEGER>, 
// <ACCESSOR-METHOD>, 
// <ACCUMULATOR>, 
// <ALREADY-OWNED-ERROR>, 
// <AMBIGUOUS-METHODS-ENGINE-NODE>, 
// <AMBIGUOUS-METHODS-ERROR>, 
// <AMBIGUOUS-METHODS-WARNING>, 
// <AMBIGUOUS-METHODS>, 
// <ANY-CLASS-SLOT-DESCRIPTOR>, 
// <ANY-INSTANCE-SLOT-DESCRIPTOR>, 
// <ARGUMENT-COUNT-ERROR>, 
// <ARGUMENT-COUNT-OVERFLOW-ERROR>, 
// <ARGUMENT-ERROR>, 
// <ARRAY>, 
// <ASSERT-ERROR>, 
// <BIG-INTEGER>, 
// <BOOLEAN>, 
// <BOTTOM-TYPE>, 
// <BOXED-CLASS-SLOT-ENGINE-NODE>, 
// <BOXED-CLASS-SLOT-GETTER-ENGINE-NODE>, 
// <BOXED-CLASS-SLOT-SETTER-ENGINE-NODE>, 
// <BOXED-INSTANCE-SLOT-ENGINE-NODE>, 
// <BOXED-INSTANCE-SLOT-GETTER-ENGINE-NODE>, 
// <BOXED-INSTANCE-SLOT-SETTER-ENGINE-NODE>, 
// <BOXED-REPEATED-INSTANCE-SLOT-GETTER-ENGINE-NODE>, 
// <BOXED-REPEATED-INSTANCE-SLOT-SETTER-ENGINE-NODE>, 
// <BY-CLASS-DISCRIMINATOR>, 
// <BY-SINGLETON-CLASS-DISCRIMINATOR>, 
// <BYTE-CHARACTER>, 
// <BYTE-SLOT-ENGINE-NODE>, 
// <BYTE-SLOT-GETTER-ENGINE-NODE>, 
// <BYTE-SLOT-SETTER-ENGINE-NODE>, 
// <BYTE-STRING>, 
// <CACHE-HEADER-ENGINE-NODE>, 
// <CALLABLE-OBJECT>, 
// <CHARACTER>, 
// <CLASS-KEYED-DISCRIMINATOR>, 
// <CLASS-SLOT-DESCRIPTOR>, 
// <CLASS-SLOT-ENGINE-NODE>, 
// <CLASS>, 
// <CLOSURE-METHOD-MIXIN>, 
// <CODE-BASED-EP>, 
// <CODE>, 
// <COLLECTION>, 
// <COMPLEX>, 
// <CONDITION>, 
// <CONDITIONAL-UPDATE-ERROR>, 
// <CONSTANT-RANGE>, 
// <CONSTANT-SLOT-DESCRIPTOR>, 
// <COPY-DOWN-METHOD>, 
// <COUNT-EXCEEDED-ERROR>, 
// <DEFERRED-IEP>, 
// <DEQUE>, 
// <DESIGNATOR-CLASS>, 
// <DISCRIMINATOR-EP>, 
// <DISCRIMINATOR>, 
// <DISPATCH-ENGINE-INVOCABLE>, 
// <DISPATCH-ERROR>, 
// <DISPATCH-STATE>, 
// <DOMAIN>, 
// <DOUBLE-FLOAT>, 
// <DOUBLE-INTEGER>, 
// <DUMMY>, 
// <DUPLICATE-JOIN-ERROR>, 
// <EACH-SUBCLASS-SLOT-DESCRIPTOR>, 
// <EMPTY-COLLECTION-ERROR>, 
// <EMPTY-LIST>, 
// <EMPTY-RANGE>, 
// <ENGINE-NODE-EP>, 
// <ENGINE-NODE>, 
// <ENTRY-VECTOR>, 
// <ERROR>, 
// <EXCLUSIVE-LOCK>, 
// <EXPLICIT-KEY-COLLECTION>, 
// <EXPLICIT-KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <FINITE-RANGE>, 
// <FLOAT>, 
// <FUNCTION-CLASS>, 
// <FUNCTION>, 
// <GENERIC-FUNCTION>, 
// <GETTER-ACCESSOR-METHOD>, 
// <GETTER-METHOD>, 
// <GF-CALL-SITE-CACHE>, 
// <HANDLER>, 
// <HASH-STATE>, 
// <HASHED-BY-CLASS-DISCRIMINATOR>, 
// <HASHED-BY-SINGLETON-CLASS-DISCRIMINATOR>, 
// <HASHED-CLASS-KEYED-DISCRIMINATOR>, 
// <IF-TYPE-DISCRIMINATOR>, 
// <IMMEDIATE-LINEAR-SINGLETON-DISCRIMINATOR>, 
// <IMMUTABLE-ERROR>, 
// <IMPLEMENTATION-CLASS>, 
// <IMPLICIT-KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <IMPROPER-LIST-ERROR>, 
// <INAPPLICABLE-ENGINE-NODE>, 
// <INCOMPATIBLE-RANGE-ERROR>, 
// <INCONSISTENT-PRECEDENCE-CLASS-ERROR>, 
// <INFINITE-RANGE-ERROR>, 
// <INFINITE-RANGE>, 
// <INHERITED-SLOT-DESCRIPTOR>, 
// <INIT-ARG-DESCRIPTOR>, 
// <INSTANCE-SLOT-DESCRIPTOR>, 
// <INSTANCE-SLOT-ENGINE-NODE>, 
// <INTEGER>, 
// <INVALID-INDEX-ERROR>, 
// <ISLAND-DEQUE>, 
// <ITERATION-ERROR>, 
// <ITERATION-STATE>, 
// <KERNEL-EP>, 
// <KEY-NOT-FOUND-ERROR>, 
// <KEY-TEST-ERROR>, 
// <KEYED-ACCUMULATOR>, 
// <KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <KEYWORD-CLOSURE-METHOD>, 
// <KEYWORD-ERROR>, 
// <KEYWORD-METHOD>, 
// <KEYWORD-SIGNATURE+REST-VALUE>, 
// <KEYWORD-SIGNATURE+VALUES+REST-VALUE>, 
// <KEYWORD-SIGNATURE+VALUES>, 
// <KEYWORD-SIGNATURE>, 
// <LAMBDA-OR-CODE>, 
// <LAMBDA>, 
// <LIBRARY>, 
// <LIMITED-INTEGER>, 
// <LIMITED-TYPE>, 
// <LINEAR-BY-CLASS-DISCRIMINATOR>, 
// <LINEAR-BY-SINGLETON-CLASS-DISCRIMINATOR>, 
// <LINEAR-CLASS-KEYED-DISCRIMINATOR>, 
// <LINEAR-SINGLETON-DISCRIMINATOR>, 
// <LIST>, 
// <LOCK-ERROR>, 
// <LOCK>, 
// <MACHINE-NUMBER>, 
// <MACHINE-WORD-OVERFLOW>, 
// <MACHINE-WORD>, 
// <METHOD>, 
// <MISC-ENGINE-NODE-EP>, 
// <MISC-KERNEL-EP>, 
// <MISCELLANEOUS-OBSOLETE-INSTANCE>, 
// <MISSING-KEYWORD-ERROR>, 
// <MM-WRAPPER>, 
// <MODULE>, 
// <MULTIDIMENSIONAL-ARRAY>, 
// <MULTIPLE-EXPLICIT-KEY-COLLECTION>, 
// <MULTIPLE-MIXED-COLLECTION>, 
// <MULTIPLE-SEQUENCE>, 
// <MUTABLE-COLLECTION>, 
// <MUTABLE-EXPLICIT-KEY-COLLECTION>, 
// <MUTABLE-OBJECT-WITH-ELEMENTS>, 
// <MUTABLE-SEQUENCE>, 
// <NAMESPACE>, 
// <NATURAL-NUMBER-ERROR>, 
// <NOT-FOUND-ERROR>, 
// <NOT-OWNED-ERROR>, 
// <NOTIFICATION>, 
// <NUMBER>, 
// <OBJECT-DEQUE>, 
// <OBJECT-SET>, 
// <OBJECT-TABLE>, 
// <OBJECT-WITH-ELEMENTS>, 
// <OBJECT>, 
// <OBSOLETE-INSTANCE>, 
// <ODD-KEYWORD-ARGUMENTS-ERROR>, 
// <PAIR>, 
// <PORTABLE-CONTAINER>, 
// <PORTABLE-DOUBLE-CONTAINER>, 
// <PROPERTIES-PROVIDER>, 
// <RANGE>, 
// <RATIONAL>, 
// <RAW-AGGREGATE-TYPE>, 
// <RAW-STRUCT-TYPE>, 
// <RAW-UNION-TYPE>, 
// <READ-WRITE-LOCK-I>, 
// <READ-WRITE-LOCK>, 
// <REAL>, 
// <RECURSIVE-LOCK-I>, 
// <RECURSIVE-LOCK>, 
// <REPEATED-ACCESSOR-METHOD>, 
// <REPEATED-BYTE-SLOT-GETTER-ENGINE-NODE>, 
// <REPEATED-BYTE-SLOT-SETTER-ENGINE-NODE>, 
// <REPEATED-GETTER-METHOD>, 
// <REPEATED-SETTER-METHOD>, 
// <REPEATED-SLOT-ACCESS-ENGINE-NODE>, 
// <REPEATED-SLOT-DESCRIPTOR>, 
// <RESTART>, 
// <SCU-NODE>, 
// <SEALED-GENERIC-FUNCTION-ERROR>, 
// <SEALED-OBJECT-ERROR>, 
// <SEMAPHORE-I>, 
// <SEMAPHORE>, 
// <SEQUENCE-ACCUMULATOR>, 
// <SEQUENCE>, 
// <SERIOUS-CONDITION>, 
// <SET>, 
// <SETTER-ACCESSOR-METHOD>, 
// <SETTER-METHOD>, 
// <SIGNATURE+REST-VALUE>, 
// <SIGNATURE+VALUES+REST-VALUE>, 
// <SIGNATURE+VALUES>, 
// <SIGNATURE-REST-VALUE-MIXIN>, 
// <SIGNATURE-VALUES-MIXIN>, 
// <SIGNATURE>, 
// <SIMPLE-CLOSURE-METHOD>, 
// <SIMPLE-CONDITION>, 
// <SIMPLE-ERROR>, 
// <SIMPLE-LOCK-I>, 
// <SIMPLE-LOCK>, 
// <SIMPLE-METHOD>, 
// <SIMPLE-OBJECT-VECTOR>, 
// <SIMPLE-RESTART>, 
// <SIMPLE-SLOT-ERROR>, 
// <SIMPLE-TYPECHECKED-CACHE-HEADER-ENGINE-NODE>, 
// <SIMPLE-TYPECHECKED-GF-CACHE>, 
// <SIMPLE-VECTOR>, 
// <SIMPLE-WARNING>, 
// <SINGLE-ACCESSOR-METHOD>, 
// <SINGLE-FLOAT>, 
// <SINGLE-METHOD-ENGINE-NODE>, 
// <SINGLE-SLOT-ACCESS-ENGINE-NODE>, 
// <SINGLETON-DISCRIMINATOR>, 
// <SINGLETON>, 
// <SINGULAR-TERMINAL-ENGINE-NODE>, 
// <SLOT-ACCESS-ENGINE-NODE>, 
// <SLOT-ACCESS-ENGINE-REPOSITORY>, 
// <SLOT-DESCRIPTOR>, 
// <SLOT-ERROR>, 
// <SLOT-GETTER-ENGINE-NODE>, 
// <SLOT-INITIAL-VALUE-DESCRIPTOR>, 
// <SLOT-KEYWORD-INITIALIZATION-DESCRIPTOR>, 
// <SLOT-SETTER-ENGINE-NODE>, 
// <SLOT-TYPE-ERROR>, 
// <STANDARD-OBJECT-TABLE>, 
// <STRETCHY-COLLECTION>, 
// <STRETCHY-MUTABLE-SEQUENCE>, 
// <STRETCHY-OBJECT-VECTOR>, 
// <STRETCHY-SEQUENCE>, 
// <STRETCHY-VECTOR-REPRESENTATION>, 
// <STRETCHY-VECTOR>, 
// <STRING-TABLE>, 
// <STRING>, 
// <SUBCLASS>, 
// <SUBJUNCTIVE-CLASS-UNIVERSE>, 
// <SUBSCRIPT-OUT-OF-BOUNDS-ERROR>, 
// <SYMBOL-TABLE>, 
// <SYMBOL>, 
// <SYNCHRONIZATION-CREATION-ERROR>, 
// <SYNCHRONIZATION-ERROR>, 
// <SYNCHRONIZATION-FINALIZATION-ERROR>, 
// <SYNCHRONIZATION>, 
// <TABLE-VECTOR>, 
// <TABLE>, 
// <TERMINAL-ENGINE-NODE>, 
// <THREAD-CREATION-ERROR>, 
// <THREAD-ERROR>, 
// <THREAD-FINALIZATION-ERROR>, 
// <THREAD-INACTIVE-ERROR>, 
// <THREAD-PRIORITY-ERROR>, 
// <THREAD>, 
// <TIMEOUT-EXPIRED>, 
// <TRACEABLE-VALUE-CELL>, 
// <TYPE-ERROR>, 
// <TYPE>, 
// <TYPECHECK-DISCRIMINATOR>, 
// <UNBOUND>, 
// <UNEXPECTED-SYNCHRONIZATION-ERROR>, 
// <UNEXPECTED-THREAD-ERROR>, 
// <UNICODE-NOT-SUPPORTED-ERROR>, 
// <UNICODE-STRING>, 
// <UNION>, 
// <UNIQUIFIED-TERMINAL-ENGINE-NODE>, 
// <UNKEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <UNKNOWN-KEYWORD-ARGUMENT-ERROR>, 
// <UNRESTRICTED-KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <UNSHARED-TERMINAL-ENGINE-NODE>, 
// <VALUE-CLASS>, 
// <VALUE-NOT-FOUND-ERROR>, 
// <VALUE-OBJECT>, 
// <VECTOR>, 
// <VIRTUAL-SLOT-DESCRIPTOR>, 
// <WARNING>, 
// 
// 
// <ABORT>, 
// <ABSTRACT-INTEGER>, 
// <ACCESSOR-METHOD>, 
// <ACCUMULATOR>, 
// <ALREADY-OWNED-ERROR>, 
// <AMBIGUOUS-METHODS-ENGINE-NODE>, 
// <AMBIGUOUS-METHODS-ERROR>, 
// <AMBIGUOUS-METHODS-WARNING>, 
// <AMBIGUOUS-METHODS>, 
// <ANY-CLASS-SLOT-DESCRIPTOR>, 
// <ANY-INSTANCE-SLOT-DESCRIPTOR>, 
// <ARGUMENT-COUNT-ERROR>, 
// <ARGUMENT-COUNT-OVERFLOW-ERROR>, 
// <ARGUMENT-ERROR>, 
// <ARRAY>, 
// <ASSERT-ERROR>, 
// <BIG-INTEGER>, 
// <BOOLEAN>, 
// <BOTTOM-TYPE>, 
// <BOXED-CLASS-SLOT-ENGINE-NODE>, 
// <BOXED-CLASS-SLOT-GETTER-ENGINE-NODE>, 
// <BOXED-CLASS-SLOT-SETTER-ENGINE-NODE>, 
// <BOXED-INSTANCE-SLOT-ENGINE-NODE>, 
// <BOXED-INSTANCE-SLOT-GETTER-ENGINE-NODE>, 
// <BOXED-INSTANCE-SLOT-SETTER-ENGINE-NODE>, 
// <BOXED-REPEATED-INSTANCE-SLOT-GETTER-ENGINE-NODE>, 
// <BOXED-REPEATED-INSTANCE-SLOT-SETTER-ENGINE-NODE>, 
// <BY-CLASS-DISCRIMINATOR>, 
// <BY-SINGLETON-CLASS-DISCRIMINATOR>, 
// <BYTE-CHARACTER>, 
// <BYTE-SLOT-ENGINE-NODE>, 
// <BYTE-SLOT-GETTER-ENGINE-NODE>, 
// <BYTE-SLOT-SETTER-ENGINE-NODE>, 
// <BYTE-STRING>, 
// <CACHE-HEADER-ENGINE-NODE>, 
// <CALLABLE-OBJECT>, 
// <CHARACTER>, 
// <CLASS-KEYED-DISCRIMINATOR>, 
// <CLASS-SLOT-DESCRIPTOR>, 
// <CLASS-SLOT-ENGINE-NODE>, 
// <CLASS>, 
// <CLOSURE-METHOD-MIXIN>, 
// <CODE-BASED-EP>, 
// <CODE>, 
// <COLLECTION>, 
// <COMPLEX>, 
// <CONDITION>, 
// <CONDITIONAL-UPDATE-ERROR>, 
// <CONSTANT-RANGE>, 
// <CONSTANT-SLOT-DESCRIPTOR>, 
// <COPY-DOWN-METHOD>, 
// <COUNT-EXCEEDED-ERROR>, 
// <DEFERRED-IEP>, 
// <DEQUE>, 
// <DESIGNATOR-CLASS>, 
// <DISCRIMINATOR-EP>, 
// <DISCRIMINATOR>, 
// <DISPATCH-ENGINE-INVOCABLE>, 
// <DISPATCH-ERROR>, 
// <DISPATCH-STATE>, 
// <DOMAIN>, 
// <DOUBLE-FLOAT>, 
// <DOUBLE-INTEGER>, 
// <DUMMY>, 
// <DUPLICATE-JOIN-ERROR>, 
// <EACH-SUBCLASS-SLOT-DESCRIPTOR>, 
// <EMPTY-COLLECTION-ERROR>, 
// <EMPTY-LIST>, 
// <EMPTY-RANGE>, 
// <ENGINE-NODE-EP>, 
// <ENGINE-NODE>, 
// <ENTRY-VECTOR>, 
// <ERROR>, 
// <EXCLUSIVE-LOCK>, 
// <EXPLICIT-KEY-COLLECTION>, 
// <EXPLICIT-KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <FINITE-RANGE>, 
// <FLOAT>, 
// <FUNCTION-CLASS>, 
// <FUNCTION>, 
// <GENERIC-FUNCTION>, 
// <GETTER-ACCESSOR-METHOD>, 
// <GETTER-METHOD>, 
// <GF-CALL-SITE-CACHE>, 
// <HANDLER>, 
// <HASH-STATE>, 
// <HASHED-BY-CLASS-DISCRIMINATOR>, 
// <HASHED-BY-SINGLETON-CLASS-DISCRIMINATOR>, 
// <HASHED-CLASS-KEYED-DISCRIMINATOR>, 
// <IF-TYPE-DISCRIMINATOR>, 
// <IMMEDIATE-LINEAR-SINGLETON-DISCRIMINATOR>, 
// <IMMUTABLE-ERROR>, 
// <IMPLEMENTATION-CLASS>, 
// <IMPLICIT-KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <IMPROPER-LIST-ERROR>, 
// <INAPPLICABLE-ENGINE-NODE>, 
// <INCOMPATIBLE-RANGE-ERROR>, 
// <INCONSISTENT-PRECEDENCE-CLASS-ERROR>, 
// <INFINITE-RANGE-ERROR>, 
// <INFINITE-RANGE>, 
// <INHERITED-SLOT-DESCRIPTOR>, 
// <INIT-ARG-DESCRIPTOR>, 
// <INSTANCE-SLOT-DESCRIPTOR>, 
// <INSTANCE-SLOT-ENGINE-NODE>, 
// <INTEGER>, 
// <INVALID-INDEX-ERROR>, 
// <ISLAND-DEQUE>, 
// <ITERATION-ERROR>, 
// <ITERATION-STATE>, 
// <KERNEL-EP>, 
// <KEY-NOT-FOUND-ERROR>, 
// <KEY-TEST-ERROR>, 
// <KEYED-ACCUMULATOR>, 
// <KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <KEYWORD-CLOSURE-METHOD>, 
// <KEYWORD-ERROR>, 
// <KEYWORD-METHOD>, 
// <KEYWORD-SIGNATURE+REST-VALUE>, 
// <KEYWORD-SIGNATURE+VALUES+REST-VALUE>, 
// <KEYWORD-SIGNATURE+VALUES>, 
// <KEYWORD-SIGNATURE>, 
// <LAMBDA-OR-CODE>, 
// <LAMBDA>, 
// <LIBRARY>, 
// <LIMITED-INTEGER>, 
// <LIMITED-TYPE>, 
// <LINEAR-BY-CLASS-DISCRIMINATOR>, 
// <LINEAR-BY-SINGLETON-CLASS-DISCRIMINATOR>, 
// <LINEAR-CLASS-KEYED-DISCRIMINATOR>, 
// <LINEAR-SINGLETON-DISCRIMINATOR>, 
// <LIST>, 
// <LOCK-ERROR>, 
// <LOCK>, 
// <MACHINE-NUMBER>, 
// <MACHINE-WORD-OVERFLOW>, 
// <MACHINE-WORD>, 
// <METHOD>, 
// <MISC-ENGINE-NODE-EP>, 
// <MISC-KERNEL-EP>, 
// <MISCELLANEOUS-OBSOLETE-INSTANCE>, 
// <MISSING-KEYWORD-ERROR>, 
// <MM-WRAPPER>, 
// <MODULE>, 
// <MULTIDIMENSIONAL-ARRAY>, 
// <MULTIPLE-EXPLICIT-KEY-COLLECTION>, 
// <MULTIPLE-MIXED-COLLECTION>, 
// <MULTIPLE-SEQUENCE>, 
// <MUTABLE-COLLECTION>, 
// <MUTABLE-EXPLICIT-KEY-COLLECTION>, 
// <MUTABLE-OBJECT-WITH-ELEMENTS>, 
// <MUTABLE-SEQUENCE>, 
// <NAMESPACE>, 
// <NATURAL-NUMBER-ERROR>, 
// <NOT-FOUND-ERROR>, 
// <NOT-OWNED-ERROR>, 
// <NOTIFICATION>, 
// <NUMBER>, 
// <OBJECT-DEQUE>, 
// <OBJECT-SET>, 
// <OBJECT-TABLE>, 
// <OBJECT-WITH-ELEMENTS>, 
// <OBJECT>, 
// <OBSOLETE-INSTANCE>, 
// <ODD-KEYWORD-ARGUMENTS-ERROR>, 
// <PAIR>, 
// <PORTABLE-CONTAINER>, 
// <PORTABLE-DOUBLE-CONTAINER>, 
// <PROPERTIES-PROVIDER>, 
// <RANGE>, 
// <RATIONAL>, 
// <RAW-AGGREGATE-TYPE>, 
// <RAW-STRUCT-TYPE>, 
// <RAW-UNION-TYPE>, 
// <READ-WRITE-LOCK-I>, 
// <READ-WRITE-LOCK>, 
// <REAL>, 
// <RECURSIVE-LOCK-I>, 
// <RECURSIVE-LOCK>, 
// <REPEATED-ACCESSOR-METHOD>, 
// <REPEATED-BYTE-SLOT-GETTER-ENGINE-NODE>, 
// <REPEATED-BYTE-SLOT-SETTER-ENGINE-NODE>, 
// <REPEATED-GETTER-METHOD>, 
// <REPEATED-SETTER-METHOD>, 
// <REPEATED-SLOT-ACCESS-ENGINE-NODE>, 
// <REPEATED-SLOT-DESCRIPTOR>, 
// <RESTART>, 
// <SCU-NODE>, 
// <SEALED-GENERIC-FUNCTION-ERROR>, 
// <SEALED-OBJECT-ERROR>, 
// <SEMAPHORE-I>, 
// <SEMAPHORE>, 
// <SEQUENCE-ACCUMULATOR>, 
// <SEQUENCE>, 
// <SERIOUS-CONDITION>, 
// <SET>, 
// <SETTER-ACCESSOR-METHOD>, 
// <SETTER-METHOD>, 
// <SIGNATURE+REST-VALUE>, 
// <SIGNATURE+VALUES+REST-VALUE>, 
// <SIGNATURE+VALUES>, 
// <SIGNATURE-REST-VALUE-MIXIN>, 
// <SIGNATURE-VALUES-MIXIN>, 
// <SIGNATURE>, 
// <SIMPLE-CLOSURE-METHOD>, 
// <SIMPLE-CONDITION>, 
// <SIMPLE-ERROR>, 
// <SIMPLE-LOCK-I>, 
// <SIMPLE-LOCK>, 
// <SIMPLE-METHOD>, 
// <SIMPLE-OBJECT-VECTOR>, 
// <SIMPLE-RESTART>, 
// <SIMPLE-SLOT-ERROR>, 
// <SIMPLE-TYPECHECKED-CACHE-HEADER-ENGINE-NODE>, 
// <SIMPLE-TYPECHECKED-GF-CACHE>, 
// <SIMPLE-VECTOR>, 
// <SIMPLE-WARNING>, 
// <SINGLE-ACCESSOR-METHOD>, 
// <SINGLE-FLOAT>, 
// <SINGLE-METHOD-ENGINE-NODE>, 
// <SINGLE-SLOT-ACCESS-ENGINE-NODE>, 
// <SINGLETON-DISCRIMINATOR>, 
// <SINGLETON>, 
// <SINGULAR-TERMINAL-ENGINE-NODE>, 
// <SLOT-ACCESS-ENGINE-NODE>, 
// <SLOT-ACCESS-ENGINE-REPOSITORY>, 
// <SLOT-DESCRIPTOR>, 
// <SLOT-ERROR>, 
// <SLOT-GETTER-ENGINE-NODE>, 
// <SLOT-INITIAL-VALUE-DESCRIPTOR>, 
// <SLOT-KEYWORD-INITIALIZATION-DESCRIPTOR>, 
// <SLOT-SETTER-ENGINE-NODE>, 
// <SLOT-TYPE-ERROR>, 
// <STANDARD-OBJECT-TABLE>, 
// <STRETCHY-COLLECTION>, 
// <STRETCHY-MUTABLE-SEQUENCE>, 
// <STRETCHY-OBJECT-VECTOR>, 
// <STRETCHY-SEQUENCE>, 
// <STRETCHY-VECTOR-REPRESENTATION>, 
// <STRETCHY-VECTOR>, 
// <STRING-TABLE>, 
// <STRING>, 
// <SUBCLASS>, 
// <SUBJUNCTIVE-CLASS-UNIVERSE>, 
// <SUBSCRIPT-OUT-OF-BOUNDS-ERROR>, 
// <SYMBOL-TABLE>, 
// <SYMBOL>, 
// <SYNCHRONIZATION-CREATION-ERROR>, 
// <SYNCHRONIZATION-ERROR>, 
// <SYNCHRONIZATION-FINALIZATION-ERROR>, 
// <SYNCHRONIZATION>, 
// <TABLE-VECTOR>, 
// <TABLE>, 
// <TERMINAL-ENGINE-NODE>, 
// <THREAD-CREATION-ERROR>, 
// <THREAD-ERROR>, 
// <THREAD-FINALIZATION-ERROR>, 
// <THREAD-INACTIVE-ERROR>, 
// <THREAD-PRIORITY-ERROR>, 
// <THREAD>, 
// <TIMEOUT-EXPIRED>, 
// <TRACEABLE-VALUE-CELL>, 
// <TYPE-ERROR>, 
// <TYPE>, 
// <TYPECHECK-DISCRIMINATOR>, 
// <UNBOUND>, 
// <UNEXPECTED-SYNCHRONIZATION-ERROR>, 
// <UNEXPECTED-THREAD-ERROR>, 
// <UNICODE-NOT-SUPPORTED-ERROR>, 
// <UNICODE-STRING>, 
// <UNION>, 
// <UNIQUIFIED-TERMINAL-ENGINE-NODE>, 
// <UNKEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <UNKNOWN-KEYWORD-ARGUMENT-ERROR>, 
// <UNRESTRICTED-KEYED-SINGLE-METHOD-ENGINE-NODE>, 
// <UNSHARED-TERMINAL-ENGINE-NODE>, 
// <VALUE-CLASS>, 
// <VALUE-NOT-FOUND-ERROR>, 
// <VALUE-OBJECT>, 
// <VECTOR>, 
// <VIRTUAL-SLOT-DESCRIPTOR>, 
// <WARNING>, 
// 
// 
//    <object>,
//    <list>,
//    <empty-list>,
//    <pair>,
//    <byte-string>,
//    <simple-object-vector>,
//    <integer>,
//    <symbol>,
//    <character>,
//    <boolean>,
//    <byte-character>;
//    <single-float>,
//    <double-float>,
//    <abstract-integer>,
//    <big-integer>,
//    <machine-word>,
//    <double-integer>,
//    <number>,
// 
//    <function>,
//    <method>,
//    <generic-function>,
//    <type>,
//    <class>,
//    <singleton>,
//    <collection>,
//    <mutable-collection>,
//    <stretchy-collection>,
//    <sequence>,
//    <mutable-sequence>,
//    <explicit-key-collection>,
//    <mutable-explicit-key-collection>,
//    <array>,
//    <deque>,
//    <range>,
//    <string>,
//    <unicode-string>,
//    <table>,
//    <object-table>,
//    <vector>,
//    <stretchy-vector>,
//    <simple-vector>,
//    <condition>,
//    <serious-condition>,
//    <error>,
//    <simple-error>,
//    <type-error>,
//    <sealed-object-error>,
//    <warning>,
//    <simple-warning>,
//    <restart>,
//    <simple-restart>,
//    <abort>,
//    <complex>,
//    <real>,
//    <rational>,
//    <float>,
//    <extended-float>,
// 
//     <bottom>, 
//     <bottom-type>;
//     <format-string-condition>,
//     <simple-condition>,
//     <stretchy-sequence>,
//     <not-found-error>,
//     <unbound>,
//     <slot-descriptor>,
//     <repeated-slot-descriptor>,
//     <union>, 
//     <limited-integer>, 
//     <signature>, 
//     <keyword-signature>,
//     <simple-method>, 
//     <lambda>, 
//     <keyword-method>, 
//     <closure-method>, 
//     <keyword-closure-method>,
//     <accessor-method>, 
//     <getter-accessor-method>, 
//     <setter-accessor-method>,
//     <single-accessor-method>, 
//     <repeated-accessor-method>,
//     <limited-type>,
//     <slot-descriptor>, 
//     <any-instance-slot-descriptor>, 
//     <any-class-slot-descriptor>,
//     <repeated-slot-descriptor>, 
//     <instance-slot-descriptor>, 
//     <inherited-slot-descriptor>,
//     <init-arg-descriptor>, 
//     <slot-keyword-initialization-descriptor>, 
//     <obsolete-instance>,
//     <miscellaneous-obsolete-instance>,
//     <implementation-class>,
//     <subclass>, 
//     <set>, 
//     <object-set>;
//     <object-deque>, 
//     <stretchy-object-vector>;
//     <table-vector>, 
//     <string-table>, 
//     <hash-state>, 
