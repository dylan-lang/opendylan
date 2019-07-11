module: dfmc-modeling
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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
  as(<symbol>, format-to-string("$signature-%s-types", as(<string>, type-name)))
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
  <simple-warning> =>                  $max-mini-default-signature-size,
  <restart> =>                         $max-mini-default-signature-size,
  <simple-restart> =>                  $max-mini-default-signature-size,
  <abort> =>                           $max-mini-default-signature-size,
  <complex> =>                         $max-mini-default-signature-size,
  <real> =>                            $max-mini-default-signature-size,
  <rational> =>                        $max-mini-default-signature-size,
  <float> =>                           $max-mini-default-signature-size,
  // <extended-float> =>               $max-mini-default-signature-size,

  // DYLAN EXTENSIONS

  <bottom> =>                          $max-mini-default-signature-size,
  <bottom-type> =>                     $max-mini-default-signature-size,
  <format-string-condition> =>         $max-mini-default-signature-size,
  <simple-condition> =>                $max-mini-default-signature-size,
  <stretchy-sequence> =>               $max-mini-default-signature-size,
  <not-found-error> =>                 $max-mini-default-signature-size,
  <unbound> =>                         $max-mini-default-signature-size,
  <slot-descriptor> =>                 $max-mini-default-signature-size,
  <repeated-slot-descriptor> =>        $max-mini-default-signature-size,
  <union> =>                           $max-mini-default-signature-size,
  <limited-integer> =>                 $max-mini-default-signature-size,
  <signature> =>                       $max-mini-default-signature-size,
  <limited-type> =>                    $max-mini-default-signature-size,
  <slot-descriptor> =>                 $max-mini-default-signature-size,
  <repeated-slot-descriptor> =>        $max-mini-default-signature-size,
  <obsolete-instance> =>               $max-mini-default-signature-size,
  <miscellaneous-obsolete-instance> => $max-mini-default-signature-size,
  <implementation-class> =>            $max-mini-default-signature-size,
  <subclass> =>                        $max-mini-default-signature-size,
  <set> =>                             $max-mini-default-signature-size,
  <object-set> =>                      $max-mini-default-signature-size,
  <object-deque> =>                    $max-mini-default-signature-size,
  <stretchy-object-vector> =>          $max-mini-default-signature-size,
  <table-vector> =>                    $max-mini-default-signature-size,
  <string-table> =>                    $max-mini-default-signature-size,
  <hash-state> =>                      $max-mini-default-signature-size
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
       ("$signature-%s-%s-rest-value-%d", as(<string>, type-name),
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
