Module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// !@#$ todo
//      get rid of uses of model-class-name


///                  Glossary
///   Model
/// Model classes from the compiler. The descriptors for a class may turn
/// out to be the same thing as the model for the class, but that
/// shouldn't affect this.

///   Definition or Form
/// A definition is the top level form object from the compiler.  It could
/// represent a method definition, a class definition, A slot def

/// TODO: We can't currently handle FFI definitions in loose-mode
/// libraries. We check, warn, and skip in the problem cases.

define serious-program-warning <dynamic-ffi-definition>
  format-string
    "This FFI definition cannot be processed in a loose mode library"
    " - skipping.";
end serious-program-warning;

define macro unless-ffi-definition-dynamic
  { unless-ffi-definition-dynamic (?form:expression) ?:body end }
    => { if (~library-forms-dynamic?(current-library-description()))
           ?body
         else
           note(<dynamic-ffi-definition>,
                source-location: fragment-source-location(?=form));
         end }
end macro;


define abstract class <abstract-c-struct/union-slot-descriptor> (<object>)
  constant slot getter-name :: <variable-name-fragment>,
    init-keyword: getter-name:;
  constant slot c-type /* :: <&class> */, init-keyword: c-type:;
  slot setter-name /* :: false-or(<variable-name-fragment>) */,
    init-keyword: setter:, init-value: #f;
  constant slot address-getter-name :: false-or(<variable-name-fragment>),
    init-keyword: address-getter:, init-value: #f;
//  slot struct-pointer-type-name, init-keyword: struct-pointer-type-name:;
  constant slot slot-pointer-type-name, init-keyword: slot-pointer-type-name:;
  constant slot slot-modifiers, init-keyword: modifiers:;
  constant slot slot-getter, init-keyword: getter:;
  // more options??
end;

define function maybe-slot-pointer-type-name (slot-rep) => (name)
// (slot-rep :: <abstract-c-struct/union-slot-descriptor>) => (name)
  // slot-rep.slot-pointer-type-name;
  // The dynamic punt version...
  let type = slot-rep.c-type;
  #{ abstract-pointer-type(?type) }
end function;

define class <c-struct/union-slot-descriptor>
    (<abstract-c-struct/union-slot-descriptor>)
end;

define class <c-struct/union-array-slot-descriptor>
    (<abstract-c-struct/union-slot-descriptor>)
  constant slot array-length :: <fragment>, required-init-keyword: array-length:;
end;

define class <c-struct/union-bitfield-slot-descriptor>
    (<abstract-c-struct/union-slot-descriptor>)
  constant slot bitfield-width :: <fragment>, required-init-keyword: width:;
end;

define class <c-struct/union-option-descriptor> (<object>)
  constant slot c-name, init-keyword: c-name:;
  constant slot pointer-type-name, init-keyword: pointer-type-name:;
  constant slot option-descriptor-pack, init-keyword: pack:;
end class <c-struct/union-option-descriptor>;


define method ^initialize-class
    (designator :: <&C-struct/union-designator-class>,
     #rest keys, #key struct-slots)
  next-method();
  let cooked-slots  = fragment-arguments(struct-slots);

  // now pointer type business is set in the designator;
  let (slots, pointer-type-name-expr, c-name-expr, pack-expr)
    = parse-syntax-c-struct-slots(cooked-slots);
  designator.struct-fields := slots;

  unless (pack-expr = #"not-given")
    let pack = ^top-level-eval(pack-expr);
    if (instance?(pack, <integer>))
      ^options(designator) := pair(#"pack", pair(pack, ^options(designator)));
    else
      note(<invalid-pack-value>,
           source-location: fragment-source-location(pack-expr),
           pack-expression: pack-expr);
    end if;
  end unless;

// Need to do this lazily
//  let raw-info = compute-raw-type-info(designator, slots);
//  designator.^raw-type-info := raw-info;
//  designator.^alignment-of := raw-info.raw-type-info-alignment;
//  designator.^size-of := raw-info.raw-type-info-size;
  designator
end method;


define method assure-raw-type-info
    (designator :: <&class>)
 => ();
  values()
end method;

define method assure-raw-type-info
    (designator :: <&C-struct/union-designator-class>)
 => ();
  unless (designator.^raw-type-info)
    let slots = designator.struct-fields;
    let raw-info = compute-raw-type-info(designator, slots);
    designator.^raw-type-info := raw-info;
    designator.^alignment-of := raw-info.raw-type-info-alignment;
    designator.^size-of := raw-info.raw-type-info-size;
  end;
  values()
end;


define method parse-name-or-false (form :: <fragment>)
 => (v :: type-union(<boolean>, <fragment>, <symbol>));
  macro-case (form)
    { #f } => #f;
    { ?name:name } => name;
    { #"not-given" } => #"not-given" ;
    { ?any:* } => any;
  end;
end;


define method parse-syntax-c-struct-slots (parsed-slot-specs :: <sequence>)
 => (slots :: <sequence>,
     pointer-type-name :: type-union(<symbol>, <fragment>),
     c-name :: type-union(<symbol>, <fragment>),
     pack :: type-union(<symbol>, <fragment>));
  let slots = make(<stretchy-vector>);
  let see-name = #"not-given";
  let pointer-name = #"not-given";
  let pack = #"not-given";
  for (frag in parsed-slot-specs)
    let spec-kind = fragment-identifier(fragment-function(frag));
    let descriptor
      = parse-slot-descriptor(spec-kind, cook-keys(fragment-arguments(frag)));
    if(instance?(descriptor, <abstract-c-struct/union-slot-descriptor>))
      add!(slots, descriptor);
    else
      if (parse-name-or-false(descriptor.pointer-type-name) ~== #f)
        pointer-name := descriptor.pointer-type-name;
      end if;
      if (parse-name-or-false(descriptor.c-name) ~== #f)
        see-name := descriptor.c-name;
      end if;
      if (parse-name-or-false(descriptor.option-descriptor-pack) ~== #f)
        pack := descriptor.option-descriptor-pack;
      end if;
    end if;
  end for;
  values(slots, pointer-name, see-name, pack);
end method;

define method cook-keys (raw-keys :: <list>) => (key-list :: <sequence>);
  let limit = size(raw-keys);
  let result = make(<vector>, size: limit);
  for (i from 0 below limit by 2)
    let key = as(<symbol>, fragment-value(head(raw-keys)));
    result[i] := key;
    raw-keys := tail(raw-keys);
    result[i + 1]
      := select (key)
           #"setter", #"address-getter"
             => parse-name-or-false(head(raw-keys));
           otherwise => head(raw-keys);
         end select;
    raw-keys := tail(raw-keys);
  end for;
  result
end method;

define method parse-slot-descriptor (kind == #"struct-slot-spec",
                                     key-values :: <vector>)
 => (slotd :: <c-struct/union-slot-descriptor>);
  apply(make, <c-struct/union-slot-descriptor>,
        as(<list>, key-values))
end method;

define method parse-slot-descriptor (kind == #"array-slot-spec",
                                     key-values :: <vector>)
 => (slotd :: <c-struct/union-array-slot-descriptor>);
  apply(make, <c-struct/union-array-slot-descriptor>,
        key-values)
end method;

define method parse-slot-descriptor (kind == #"bitfield-slot-spec",
                                     key-values :: <vector>)
 => (slotd :: <c-struct/union-bitfield-slot-descriptor>);
  apply(make, <c-struct/union-bitfield-slot-descriptor>,
        key-values)
end method;


define method parse-slot-descriptor (kind == #"struct-options",
                                     key-values :: <vector>)
 => (option :: <c-struct/union-option-descriptor>);
  apply(make, <c-struct/union-option-descriptor>,
        key-values)
end method;

define method expand-define-c-struct/union (struct-name, metaclass-spec,
                                            specs, form)

  let cooked-specs
    = map(method (spec)
            macro-case (spec)
              { ?expr:expression } => expr;
            end
         end, specs);
  let (slots, pointer-type-name, c-name, pack)
    = parse-syntax-c-struct-slots(cooked-specs);
  unless-ffi-definition-dynamic (form)
    do-define-c-struct/union
      (form, struct-name, metaclass-spec, cooked-specs, slots,
         pointer-type-name, c-name, pack);
  end;
end;


define &macro c-struct-definer
  { define C-struct ?struct-name:name ?spec:* end }
  =>
  begin
    expand-define-c-struct/union
      (struct-name, #{ <c-struct-designator-class> },
       map(curry(process-struct-spec, struct-name), spec), form);
  end;
spec:
  { } => #();
  { ?clause:*; ... }
  => pair(clause, ...);
end;

define function build-struct-slot-spec (struct-name,
                                        modifiers, slot-kind, slot-name,
                                        c-type, slot-options)
  let slot-pointer-type-name
    = slot-name & gensym(slot-name, "-in-", struct-name, " type*");
  select (slot-kind)
    #f => #{ struct-slot-spec(getter-name: ?slot-name,
                              modifiers: ?modifiers,
                              c-type: ?c-type,
                              slot-pointer-type-name:
                                ?slot-pointer-type-name,
                              ?slot-options ) };
    #"array" => #{ array-slot-spec(getter-name: ?slot-name,
                                   modifiers: ?modifiers,
                                   c-type: ?c-type,
                                   slot-pointer-type-name:
                                     ?slot-pointer-type-name,
                                   ?slot-options) };
    #"bitfield" => #{ bitfield-slot-spec(getter-name: ?slot-name,
                                         modifiers: ?modifiers,
                                         c-type: ?c-type,
                                         slot-pointer-type-name:
                                           ?slot-pointer-type-name,
                                         ?slot-options) }
  end;
end;


define method split-slot-spec (spec, #key struct? :: <boolean> = #f)
 => (modifiers, spec);
  let (modifiers, spec) =
    macro-case (spec)
      { ?modifiers:* slot ?rest:* } => values(modifiers, #{ member ?rest });
      { ?modifiers:* member ?rest:* } => values(modifiers, #{ member ?rest });
      { ?options:* } => values(#(), spec);
    modifiers:
      { ?mod:* } => mod;
      { } => #();
    mod:
      { } => #()
      { ?m:name ... } => pair(m, ...);
    end;
  // Pick out and remove from modifiers array (and bitfield, for C-structs),
  // and place it in spec
  block (return)
    for (mod in modifiers)
      select (as(<symbol>, mod))
        #"array" =>
          return(remove!(modifiers, mod), #{ array ?spec });
        #"bitfield" =>
          if (struct?) return(remove!(modifiers, mod), #{ bitfield ?spec });
          else #f
          end if;
        #"constant" =>
          return(remove!(modifiers, mod), #{ ?spec, constant: #t });
        otherwise =>
          #f;
      end;
    end for;
    return(modifiers, spec);
  end;
end;


define function fragment-true? (fragment) => (well? :: <boolean>)
  macro-case (fragment)
    { #t } => #t;
    { ?other:* } => #f;
  end;
end function;

define function fragment-false? (fragment) => (well? :: <boolean>)
  macro-case (fragment)
    { #f } => #t;
    { ?other:* } => #f;
  end;
end function;

define function fragment-false-or-name? (fragment) => (well? :: <boolean>)
  macro-case (fragment)
    { #f } => #t;
    { ?:name } => #t;
    { ?other:* } => #f;
  end;
end function;

define function fragment-name? (fragment) => (well? :: <boolean>)
  macro-case (fragment)
    { ?:name } => #t;
    { ?other:* } => #f;
  end;
end function;

define function process-struct-options (name, clause,
    #key c-name = unsupplied(), pointer-type-name = unsupplied(),
         pack = unsupplied())
  if (supplied?(pointer-type-name))
    unless (fragment-name?(pointer-type-name))
      note(<invalid-pointer-type-name-value>,
           source-location: fragment-source-location(pointer-type-name),
           definition-name: name,
           pointer-type-name-expression: pointer-type-name);
      pointer-type-name := unsupplied();
    end unless;
  end if;

  unless (supplied?(pointer-type-name))
    pointer-type-name := #{ #f };
  end unless;
  unless (supplied?(c-name))
    c-name := #{ #f };
  end unless;
  unless (supplied?(pack))
    pack := #{ #f };
  end unless;

  #{ struct-options(c-name: ?c-name,
                    pointer-type-name: ?pointer-type-name,
                    pack: ?pack) };
end function;

define function process-address-getter-slot-option (address-getter)
 => (address-getter)
  unless (fragment-false-or-name?(address-getter))
    note(<invalid-address-getter-value>,
         definition-name: name,
         address-getter-expression: address-getter,
         source-location: fragment-source-location(address-getter));
    address-getter := #{ #f };
  end unless;
  address-getter
end function;

define function process-setter-slot-option (constant, setter) => (setter)
  if (supplied?(constant))
    if (fragment-true?(constant))
      if (supplied?(setter) & ~fragment-false?(setter))
        note(<constant-setter-value>,
             definition-name: name,
             setter-expression: setter,
             source-location: fragment-source-location(setter));
      end if;
      setter := #{ #f };
    else
      note(<invalid-constant-value>,
           definition-name: name,
           constant-expression: constant,
           source-location: fragment-source-location(constant));
    end if;
  end if;
  if (supplied?(setter) & ~fragment-false-or-name?(setter))
    note(<invalid-setter-value>,
         definition-name: name,
         setter-expression: setter,
         source-location: fragment-source-location(setter));
    setter := unsupplied()
  end if;
  setter
end function;

define function process-getter-slot-option (getter) => (getter)
  if (supplied?(getter) & ~fragment-false-or-name?(getter))
    note(<invalid-getter-value>,
         definition-name: name,
         getter-expression: getter,
         source-location: fragment-source-location(getter));
    unsupplied()
  else
    getter
  end if;
end function;

define function process-slot-options (name, clause,
    #key address-getter = #{ #f },
         constant = unsupplied(),
         setter = unsupplied(),
         getter = unsupplied(),
         c-name = #{ #f })
  address-getter := process-address-getter-slot-option(address-getter);
  setter := process-setter-slot-option(constant, setter);
  getter := process-getter-slot-option(getter);

  unless (supplied?(setter))
    setter := #{ #"not-given" };
  end unless;
  unless (supplied?(getter))
    getter := #{ #"not-given" };
  end unless;

  #{ address-getter: ?address-getter,
     setter: ?setter,
     getter: ?getter,
     c-name: ?c-name }
end function;

define function process-array-slot-options (name, clause,
    #key address-getter = #{ #f },
         constant = unsupplied(),
         setter = unsupplied(),
         getter = unsupplied(),
         c-name = #{ #f },
         length = #f)
  address-getter := process-address-getter-slot-option(address-getter);
  setter := process-setter-slot-option(constant, setter);
  getter := process-getter-slot-option(getter);
  unless (length)
    note(<missing-length-keyword-option>,
         source-location: fragment-source-location(clause),
         definition-name: name);
    length := #{ 1 };
  end unless;

  unless (supplied?(setter))
    setter := #{ #"not-given" };
  end unless;
  unless (supplied?(getter))
    getter := #{ #"not-given" };
  end unless;

  #{ address-getter: ?address-getter,
     setter: ?setter,
     getter: ?getter,
     c-name: ?c-name,
     array-length: ?length }
end function;

define function process-bitfield-slot-options (name, clause,
    #key setter = unsupplied(),
         constant = unsupplied(),
         getter = unsupplied(),
         c-name = #{ #f },
         width = #f)
  setter := process-setter-slot-option(constant, setter);
  getter := process-getter-slot-option(getter);
  unless (width)
    note(<missing-width-keyword-option>,
         source-location: fragment-source-location(clause),
         definition-name: name);
    width := #{ 1 };
  end unless;

  unless (supplied?(setter))
    setter := #{ #"not-given" };
  end unless;
  unless (supplied?(getter))
    getter := #{ #"not-given" };
  end unless;

  #{ setter: ?setter,
     getter: ?getter,
     c-name: ?c-name,
     width: ?width }
end function;


//
// Type options for C-struct
//
define option <c-struct-c-name-option>
  => c-name: :: expression
end option;

define option <c-struct-pointer-type-name-option>
  => pointer-type-name: :: expression
end option;

define option <c-struct-pack-option>
  => pack: :: expression
end option;

define constant $c-struct-options =
  list(<c-struct-c-name-option>,
       <c-struct-pointer-type-name-option>,
       <c-struct-pack-option>);


//
// Options for C-struct slots
//
define option <c-struct-c-name-slot-option>
  => c-name: :: expression
end option;

define option <c-struct-constant-slot-option>
  => constant: :: expression
end option;

define option <c-struct-setter-slot-option>
  => setter: :: expression
end option;

define option <c-struct-getter-slot-option>
  => getter: :: expression
end option;

define option <c-struct-address-getter-slot-option>
  => address-getter: :: expression
end option;

define option <c-struct-length-array-slot-option>
  => length: :: expression
end option;

define option <c-struct-width-bitfield-slot-option>
  => width: :: expression
end option;

define constant $c-struct-slot-options =
  list(<c-struct-address-getter-slot-option>,
       <c-struct-constant-slot-option>,
       <c-struct-setter-slot-option>,
       <c-struct-getter-slot-option>,
       <c-struct-c-name-slot-option>);

define constant $c-struct-array-slot-options =
  list(<c-struct-address-getter-slot-option>,
       <c-struct-constant-slot-option>,
       <c-struct-setter-slot-option>,
       <c-struct-getter-slot-option>,
       <c-struct-c-name-slot-option>,
       <c-struct-length-array-slot-option>);

define constant $c-struct-bitfield-slot-options =
  list(<c-struct-setter-slot-option>,
       <c-struct-constant-slot-option>,
       <c-struct-getter-slot-option>,
       <c-struct-c-name-slot-option>,
       <c-struct-width-bitfield-slot-option>);

define method process-struct-spec (struct-name, clause)
 => (spec);
  let (modifiers, spec) = split-slot-spec(clause, struct?: #t);
  let modifiers = #{ modifiers(??modifiers, ...) };
  macro-case (spec)
    { member ?slot-name:name :: ?c-type:expression, ?slot-options:* }
    => build-struct-slot-spec
         (struct-name, modifiers, #f, slot-name,
          c-type, slot-options);
    { array member ?slot-name:name :: ?c-type:expression,
          ?array-slot-options:* }
    => build-struct-slot-spec
         (struct-name,
          modifiers, #"array", slot-name,
          c-type, array-slot-options);
    { bitfield member ?slot-name:name :: ?c-type:expression,
          ?bitfield-slot-options:* }
    => build-struct-slot-spec
         (struct-name, modifiers, #"bitfield", slot-name,
          c-type, bitfield-slot-options);
    { ?options:* }
    => apply(process-struct-options, struct-name, options,
         parse-options($c-struct-options, options, struct-name));

  slot-options:
    { ?options:* }
    => apply(process-slot-options, struct-name, clause,
         parse-options($c-struct-slot-options, options, struct-name));

  array-slot-options:
    { ?options:* }
    => apply(process-array-slot-options, struct-name, clause,
         parse-options($c-struct-array-slot-options, options, struct-name));

  bitfield-slot-options:
    { ?options:* }
    => apply(process-bitfield-slot-options, struct-name, clause,
         parse-options($c-struct-bitfield-slot-options, options, struct-name));
  end
end method;


define &macro c-union-definer
  { define C-union ?union-name:name ?spec:* end }
  =>
  begin
    expand-define-c-struct/union
      (union-name, #{ <c-union-designator-class> },
       map(curry(process-union-spec, union-name), spec),
       form);
  end;
spec:
  { } => #()
  { ?stuff:*; ... }
  => pair(stuff, ...);
end;

define constant $c-union-options = $c-struct-options;
define constant $c-union-slot-options = $c-struct-slot-options;
define constant $c-union-array-slot-options = $c-struct-array-slot-options;

define method process-union-spec (union-name, clause)
 => (spec);
  let (modifiers, spec) = split-slot-spec(clause, struct?: #f);
  let modifiers = #{ modifiers(??modifiers, ...) };
  macro-case (spec)
    { member ?slot-name:name :: ?c-type:expression, ?slot-options:* }
    => build-struct-slot-spec
         (union-name, modifiers, #f, slot-name,
          c-type, slot-options);
    { array member ?slot-name:name :: ?c-type:expression,
          ?array-slot-options:* }
    => build-struct-slot-spec
         (union-name, modifiers, #"array", slot-name,
          c-type, array-slot-options);
    { ?options:* }
    => apply(process-struct-options, union-name, options,
             parse-options($c-union-options, options, union-name));

  slot-options:
    { ?options:* }
    => apply(process-slot-options, union-name, clause,
             parse-options($c-union-slot-options, options, union-name));

  array-slot-options:
    { ?options:* }
    => apply(process-array-slot-options, union-name, clause,
             parse-options($c-union-array-slot-options, options, union-name));
  end
end method;



/*

define &macro c-mapped-subtype-definer
  { define c-mapped-subtype ?:name (?supers:*)
      ?specs:*
  end }
  => #{ define class ?name (?supers)
         metaclass <designator-class>,
           ?specs;
       end }
supers:
  { ?e:expression, ... } => #{ ?e, ... }
specs:
  { ?spec:*; ... } => #{ ?spec, ... }
spec:
  { map ?map-type:expression, #key ?import-function:expression = identity,
                                   ?export-function:expression = identity }
  => #{ partial-import-function: import-function,
       partial-export-function: export-function,
       import-type: map-type,
       export-type: map-type }
  { import-map ?import-type:expression,
                 import-function: ?import-function:expression }
  => #{ partial-import-function: import-function,
       import-type: import-type }
  { export-map ?export-type:expression,
                 export-function: ?export-function:expression }
  => #{ partial-export-function: export-function,
       export-type: export-type }
  { pointer-type ?pointer-type-name:name,
             #key ?pointer-value-setter:expression = #f }
  => #{ pointer-type-name: pointer-type-name,
       define-pointer-value-setter: pointer-value-setter }
end macro;
*/

define method do-define-c-struct/union
    (form :: <fragment>,
     struct-name :: <variable-name-fragment>,
     metaclass-fragment,
     spec :: <sequence>,
     slots :: <sequence>,
     pointer-type-name,
     c-name,
     pack)
 => (f);
  if (pointer-type-name == #"not-given")
    pointer-type-name := gensym("pointer-to-", struct-name);
  end if;
  let raw-struct-options = if (pack = #"not-given")
                             #{ };
                           else
                             #{ #"pack", ?pack };
                           end if;
  let raw-struct-name = #{ "raw-struct-for-" ## ?struct-name };
  let class-definition-fragment
    = #{ define abstract class ?struct-name (<c-struct>)
           metaclass ?metaclass-fragment,
             struct-slots: struct-slots( ??spec, ...),
             pointer-type-name: ?pointer-type-name,
             raw-struct-name: ?raw-struct-name,
             boxer-function-name: #"primitive-wrap-c-pointer",
             unboxer-function-name: #"primitive-unwrap-c-pointer",
             low-level-type: ?pointer-type-name,
             self: ?struct-name;
         end };
  let pointer-type-definition-fragment
    = create-automatic-c-pointer-definition-fragment
        (pointer-type-name, struct-name, // pointer-value-method: #f,
         pointer-to-pointer: #f);
  let sz = size(slots);
  let kind
    = macro-case (metaclass-fragment)
        { <c-union-designator-class> }
        => #{ union }
        { <c-struct-designator-class> }
        => #{ struct }
      end;
  let raw-slots = make(<stretchy-vector>);
  local
  method loopy (index, accum, error-checkers) => (accum, error-checkers)
      if (index >= sz)
        values(accum, error-checkers);
      else
        let slot = slots[index];
        let type = c-type(slot);
        // slot.struct-pointer-type-name := pointer-type-name;
        let getter? = parse-name-or-false(slot.slot-getter);
        let slot-name = slot.getter-name;
        error-checkers
          := pair(#{ check-designator-defined(?type, ?struct-name, ?kind) },
                  error-checkers);
        raw-slots := add!(raw-slots, generate-raw-slot-spec(type, slot));
        // add definition fragments for pointers to the slot types
        // since they might be needed.
        if (getter? | slot.setter-name | slot.address-getter-name)
          // define any methods for this slot at all?
          let getter = slot-name;
          let new-pointer-type = slot.slot-pointer-type-name;
          let modifiers = fragment-arguments(slot.slot-modifiers);
          if (slot.address-getter-name)
            accum
              := pair(#{ define c-pointer-type ?new-pointer-type => ?type },
                      accum);
          end;
          // add definitions for setter and address-getter if needed.
          if(slot.setter-name)
            let real-setter-name
              // do defaulting for setter function name
              = if (slot.setter-name == #"not-given")
                  // none given, use default
                  macro-case (#{ ?getter ## "-setter" })
                    { ?foo:name }
                      => slot.setter-name := foo;
                  end macro-case;
                else
                  slot.setter-name        // given, but not #f so take it
                end if;
            // add setter definition
            accum := pair(generate-struct-setter
                            (slot,
                             modifiers,
                             struct-name,
                             real-setter-name,
                             type,
                             pointer-type-name,
                             index),
                          accum);
          end if;
          if (slot.address-getter-name)
            // add address-getter definition
            accum := pair(generate-struct-address-getter
                            (slot,
                             modifiers,
                             struct-name,
                             slot.address-getter-name,
                             type,
                             new-pointer-type,
                             pointer-type-name,
                             index),
                          accum);
          end if;
          if (getter?)
            accum := pair(generate-struct-getter
                            (slot,
                             modifiers,
                             struct-name,
                             slot-name,
                             type,
                             pointer-type-name,
                             index),
                          accum);
          end;
      end if;  // define any methods for this slot at all?
        loopy(index + 1, accum, error-checkers);
      end if;  // loopy iteration termination test
    end method loopy;
  // return the code for the accessors, and the code that forces the
  // compiler to do the error checking that must be delayed.
  let (accessor-fragments, error-checking) = loopy(0, #(), #());
  let raw-struct-definition
    = generate-raw-struct-definition(raw-struct-name, struct-name, kind,
                                     raw-struct-options, raw-slots);
  let implicit-exports
    = generate-implicit-exports(raw-struct-name, pointer-type-name);
  #{ // TODO: this is really a trick.
     // With the error-checking inside a method it puts off processing of
     // the error-checking forms until all the models are available.
     // if the compiler starts optimizing better it may optimize this
     // away before it even gets to processing the error checking forms.
     method () ??error-checking; ... end;
     ?class-definition-fragment;
     ?pointer-type-definition-fragment;
     ?raw-struct-definition;
     /*
     define method pointer-value-address
         (p :: ?pointer-type-name, #key index = 0)
      => (pn :: ?pointer-type-name)
       if (index == 0)
         p
       else
         make-c-pointer(concrete-class(?pointer-type-name),
                        primitive-machine-word-add
                          (primitive-cast-pointer-as-raw
                             (primitive-unwrap-c-pointer(p)),
                           integer-as-raw
                             (index * size-of(?struct-name))),
                        #[])
       end if;
     end method;
     */
     ??accessor-fragments; ...;
     ?implicit-exports };
end method do-define-c-struct/union;

define method generate-raw-slot-spec
    (type, slot :: <c-struct/union-slot-descriptor>)
 => (slot-spec-template)
  #{ member ?type }
end;

define method generate-raw-slot-spec
    (type, slot :: <c-struct/union-array-slot-descriptor>)
 => (slot-spec-template)
  let length = array-length(slot);
  #{ array-member ?type ?length}
end;

define method generate-raw-slot-spec
    (type, slot :: <c-struct/union-bitfield-slot-descriptor>)
 => (slot-spec-template)
  let width = bitfield-width(slot);
  #{ bitfield-member ?type ?width}
end;

define function generate-raw-struct-definition
    (raw-struct-name, struct-name, kind, options,
     raw-slots)
 => (form)
  macro-case (kind)
    { union } =>
    #{ define raw-union-type ?raw-struct-name
         ?struct-name (?options)
         ??raw-slots;
         ...
       end };
    { struct } =>
    #{ define raw-struct-type ?raw-struct-name
         ?struct-name (?options)
         ??raw-slots;
         ...
       end };
  end;
end;



// generate an empty bodied method definition for a c struct
// setter method.  The body doesn't get generated until much later.
define generic generate-struct-setter
    (slotd :: <abstract-c-struct/union-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     real-setter-name :: <fragment>,
     type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);

define method generate-struct-setter
    (slotd :: <abstract-c-struct/union-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     real-setter-name :: <fragment>,
     type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);
  let policy = c-ffi-default-inline-policy();
  #{ define ??modifiers ... ?policy method ?real-setter-name
         (new-value :: export-type-for(?type),
          struct :: ?pointer-type-name)
      => (new-value :: export-type-for(?type));
       slot-accessor-body setter (new-value, struct)
         ?type,
         ?struct-name,
         ?slot-number
       end
     end}
end method generate-struct-setter;


define method generate-struct-setter
    (slotd :: <c-struct/union-array-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     real-setter-name :: <fragment>,
     type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);
  let policy = c-ffi-default-inline-policy();
  #{ define ??modifiers ... ?policy method ?real-setter-name
         (new-value :: export-type-for(?type),
          struct :: ?pointer-type-name,
          offset :: <integer>)
      => (new-value :: export-type-for(?type));
       slot-accessor-body setter (new-value, struct, offset)
         ?type,
         ?struct-name,
         ?slot-number
       end
     end}
end method generate-struct-setter;

// generate an empty bodied method definition for a c struct
// getter method.  The body doesn't get generated until much later.
define generic generate-struct-getter
    (slotd :: <abstract-c-struct/union-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     getter-name :: <fragment>,
     type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);


define method generate-struct-getter
    (slotd :: <abstract-c-struct/union-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     getter-name :: <fragment>,
     type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);
  let policy = c-ffi-default-inline-policy();
  #{ define ??modifiers ... ?policy method ?getter-name
         (struct :: ?pointer-type-name)
      => (v :: import-type-for(?type));
       slot-accessor-body getter (struct)
         ?type,
         ?struct-name,
         ?slot-number
       end
     end}
end method generate-struct-getter;

define method generate-struct-getter
    (slotd :: <c-struct/union-array-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     getter-name :: <fragment>,
     type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);
  let policy = c-ffi-default-inline-policy();
  #{ define ??modifiers ... ?policy method ?getter-name
         (struct :: ?pointer-type-name,
          offset :: <integer>)
      => (v :: import-type-for(?type));
       slot-accessor-body getter (struct, offset)
         ?type,
         ?struct-name,
         ?slot-number
       end
     end}
end method generate-struct-getter;

define generic generate-struct-address-getter
    (slotd :: <abstract-c-struct/union-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     getter-name :: <fragment>,
     type :: <fragment>,
     slot-pointer-type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);

define method generate-struct-address-getter
    (slotd :: <abstract-c-struct/union-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     getter-name :: <fragment>,
     type :: <fragment>,
     slot-pointer-type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);
  let policy = c-ffi-default-inline-policy();
  #{ define ??modifiers ... ?policy method ?getter-name
         (struct :: ?pointer-type-name)
      => (v :: ?slot-pointer-type);
       slot-accessor-body address-getter (struct)
         ?type,
         ?struct-name,
         ?slot-number
       end
     end}
end method generate-struct-address-getter;



// delete this method !!!!!!
define method generate-struct-address-getter
    (slotd :: <c-struct/union-array-slot-descriptor>,
     modifiers,
     struct-name :: <fragment>,
     getter-name :: <fragment>,
     type :: <fragment>,
     slot-pointer-type :: <fragment>,
     pointer-type-name :: <fragment>,
     slot-number :: <integer>)
 => (f :: <template>);
  let policy = c-ffi-default-inline-policy();
  #{ define ??modifiers ... ?policy method ?getter-name
         (struct :: ?pointer-type-name)
      => (v :: ?slot-pointer-type);
       slot-accessor-body address-getter (struct)
         ?type,
         ?struct-name,
         ?slot-number
       end
     end}
end method generate-struct-address-getter;

/*
define method create-automatic-c-pointer-definition-fragment
       (pointer-type-name, struct-name,
        #key // pointer-value-method :: <boolean>,
             pointer-to-pointer :: <object> = #f, // #"dunno",
             concrete-class-name)
 => (f :: <template>);
  unless (concrete-class-name)
    concrete-class-name := gensym("instantiation-of-", pointer-type-name);
  end unless;
  let abstract-pointer-pointer-name = gensym(pointer-type-name, "-pointer");
  let concrete-pointer-pointer-name = gensym(concrete-class-name, "-pointer");
  let superclass-name
    = if (pointer-to-pointer == #t)
        #{ <C-pointer-to-pointer> }
      elseif (pointer-to-pointer)
        #{ <object> }                // indicates "can't tell yet"
      else
        #{ <C-statically-typed-pointer> }
      end if;

  let implicit-exports
    = generate-implicit-exports
        (concrete-class-name,
         abstract-pointer-pointer-name,
         concrete-pointer-pointer-name);

  // TODO: Should we seal make over abstract pointers?

  #{ define abstract open class ?pointer-type-name (?superclass-name)
       metaclass <C-automatic-pointer-designator-class>,
         referenced-type: ?struct-name,
         low-level-type: ?pointer-type-name,
         self: ?pointer-type-name,
         concrete-class-name: ?concrete-class-name,
         pointer-type-name: ?abstract-pointer-pointer-name;
     end;
//     define sealed domain make(singleton(?pointer-type-name));

     define sealed concrete class ?concrete-class-name (?pointer-type-name)
       metaclass <C-automatic-pointer-designator-class>,
         abstract-super: ?pointer-type-name,
         low-level-type: ?concrete-class-name,
         self: ?concrete-class-name;
     end;
     define sealed domain make(singleton(?concrete-class-name));
     define sealed domain initialize(?concrete-class-name);

     concrete-class(?pointer-type-name) := ?concrete-class-name;

     define abstract open class ?abstract-pointer-pointer-name
         (<C-pointer-to-pointer>)
       metaclass <C-automatic-pointer-designator-class>,
         referenced-type: ?pointer-type-name,
         low-level-type: ?abstract-pointer-pointer-name,
         self: ?abstract-pointer-pointer-name,
         concrete-class-name: ?concrete-pointer-pointer-name;
     end;
//     define sealed domain make(singleton(?abstract-pointer-pointer-name));

     define sealed concrete class ?concrete-pointer-pointer-name
         (?abstract-pointer-pointer-name)
       metaclass <C-automatic-pointer-designator-class>,
         abstract-super: ?abstract-pointer-pointer-name,
         low-level-type: ?concrete-pointer-pointer-name,
         self: ?concrete-pointer-pointer-name;
     end;
     define sealed domain make(singleton(?concrete-pointer-pointer-name));
     define sealed domain initialize(?concrete-pointer-pointer-name);
     ?implicit-exports
   }
end method create-automatic-c-pointer-definition-fragment;
*/

define method create-automatic-c-pointer-definition-fragment
       (pointer-type-name, struct-name,
        #key // pointer-value-method :: <boolean>,
             pointer-to-pointer :: <object> = #f, // #"dunno",
             concrete-class-name)
 => (f :: <template>);
  unless (concrete-class-name)
    concrete-class-name := gensym("instantiation-of-", pointer-type-name);
  end unless;
  let abstract-pointer-pointer-name = gensym(pointer-type-name, "-pointer");
  let concrete-pointer-pointer-name = gensym(concrete-class-name, "-pointer");
  let superclass-name
    = if (pointer-to-pointer == #t)
        #{ <C-pointer-to-pointer> }
      elseif (pointer-to-pointer)
        #{ <object> }                // indicates "can't tell yet"
      else
        #{ <C-statically-typed-pointer> }
      end if;

  let implicit-exports
    = generate-implicit-exports
        (abstract-pointer-pointer-name);

  // TODO: anybody who knows why these are open, please document...
  #{ define dynamic primary class ?pointer-type-name (?superclass-name)
       metaclass <C-automatic-pointer-designator-class>,
         referenced-type: ?struct-name,
         low-level-type: ?pointer-type-name,
         self: ?pointer-type-name,
         concrete-class-name: ?pointer-type-name,
         pointer-type-name: ?abstract-pointer-pointer-name;
     end;
     define sealed domain make(singleton(?pointer-type-name));

     concrete-class(?pointer-type-name) := ?pointer-type-name;

     define dynamic primary class ?abstract-pointer-pointer-name
         (<C-pointer-to-pointer>)
       metaclass <C-automatic-pointer-designator-class>,
         referenced-type: ?pointer-type-name,
         low-level-type: ?abstract-pointer-pointer-name,
         self: ?abstract-pointer-pointer-name,
         concrete-class-name: ?abstract-pointer-pointer-name;
     end;
     define sealed domain make(singleton(?abstract-pointer-pointer-name));

     ?implicit-exports
   }
end method create-automatic-c-pointer-definition-fragment;

define abstract class <struct-accessor> (<object>)
  constant slot method-argument-names :: <sequence>,
    required-init-keyword: arguments:;
end;

define class <struct-getter> (<struct-accessor>) end;
define class <struct-setter> (<struct-accessor>) end;
define class <struct-address-getter> (<struct-accessor>) end;

define &macro slot-accessor-body
  { slot-accessor-body
     ?kind
     (?arg-names)
     ?slot-type-expr:expression,
     ?struct-type-name:name,
     ?slot-number:expression
  end }
  =>
  begin
    expand-accessor-body(form,
                         struct-type-name,
                         slot-type-expr,
                         slot-number,
                         make(kind, arguments: arg-names))
  end;
arg-names:
  { ?arg-name:name, ...} => pair(arg-name, ...);
  {  } => #();

kind:
  { getter } => <struct-getter>
  { setter } => <struct-setter>
  { address-getter } => <struct-address-getter>
end;

define method expand-accessor-body (form,
                                    struct-type-name,
                                    slot-type-expr,
                                    slot-number,
                                    method-kind)
 => (body);
  let struct-type = ^eval-designator(struct-type-name);
  let slot-type = ^eval-designator(slot-type-expr);
  if (designator-class?(slot-type))
    assure-raw-type-info(struct-type);
    assure-raw-type-info(slot-type);
    ^ensure-pointer-types-initialized(slot-type);
    ^ensure-pointer-types-initialized(struct-type);
    let slot-number = fragment-value(slot-number);
    let slotd = struct-type.struct-fields[slot-number];
    expand-slot-accessor(method-kind, slot-type, slotd, struct-type,
                         slot-number);
  else
    // Don't need to generate an error as that is done by the calls to
    // check-designator-defined in the generated code for C-struct/unions
    #{ };
  end if;
end;


define method expand-slot-accessor
    (m :: <struct-getter>, // getter method
     model-slot-type :: <&C-struct/union-designator-class>, // slot is a struct
     slot-rep :: <c-struct/union-slot-descriptor>, // not an array or bitfield
     struct-descriptor :: <&c-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let slot-pointer-type-name = slot-rep.maybe-slot-pointer-type-name;
  let struct = method-argument-names(m)[0];
  #{begin
      make-c-pointer(?slot-pointer-type-name,
                     primitive-machine-word-add
                       (primitive-cast-pointer-as-raw
                          (primitive-unwrap-c-pointer(?struct)),
                        integer-as-raw
                          (%c-struct-slot-offset(?slot-number,
                                                 ?struct-name))),
                     #[])
    end}
end method expand-slot-accessor;


define method expand-slot-accessor
    (m :: <struct-setter>, // setter method
     model-slot-type :: <&C-struct/union-designator-class>, // slot is a struct
     slot-rep :: <c-struct/union-slot-descriptor>, // not an array or bitfield
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let export = ^export-function(model-slot-type) | #{ identity };
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let slot-size = ^size-of(model-slot-type);
  let slot-pointer-type-name = slot-rep.maybe-slot-pointer-type-name;
  let names = method-argument-names(m);
  let (new-value, struct) = values(names[0], names[1]);
  #{ begin
       let exported-new-value :: ?slot-pointer-type-name
         = ?export(?new-value);
       %pointer-replace-bytes
         (?struct, exported-new-value,
          byte-offset1: %c-struct-slot-offset(?slot-number,
                                              ?struct-name),
          byte-offset2: 0,
          size: ?slot-size);
       ?new-value
     end}
end method expand-slot-accessor;

define method expand-slot-accessor
    (m :: <struct-address-getter>, // address getter
     model-slot-type :: <&C-struct/union-designator-class>, // slot is a struct
     slot-rep :: <abstract-c-struct/union-slot-descriptor>, // array or not
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let slot-pointer-type-name = slot-rep.maybe-slot-pointer-type-name;
  let struct = method-argument-names(m)[0];
  #{begin
      make-c-pointer(?slot-pointer-type-name,
                     primitive-machine-word-add
                       (primitive-cast-pointer-as-raw
                          (primitive-unwrap-c-pointer(?struct)),
                        integer-as-raw
                          (%c-struct-slot-offset(?slot-number,
                                                 ?struct-name))),
                     #[])
    end}
/** could do this if only we really could
  #{ define constant ?address-getter = ?getter-name; }
 **/
end method expand-slot-accessor;


//// define methods for array slots of structs

define method expand-slot-accessor
    (m :: <struct-getter>, // getter method
     model-slot-type :: <&C-struct/union-designator-class>, // slot is a struct
     slot-rep :: <c-struct/union-array-slot-descriptor>, // array
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);
  let slot-type-size = ^size-of(model-slot-type);
  let len = ^top-level-eval(slot-rep.array-length);
  unless (instance?(len, <integer>) & len > 0)
    note(<invalid-array-slot-bounds>,
         source-location: fragment-source-location(slot-rep.array-length),
         definition-name: #{ *unkown* },
         length-expression: slot-rep.array-length);
    len := 1;
  end unless;
  let max-index = len - 1;
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let slot-pointer-type-name = slot-rep.maybe-slot-pointer-type-name;
  let names = method-argument-names(m);
  let (struct, offset) = values(names[0], names[1]);
  // TODO: check bounds by using limited specializer
  // offset :: limited(<integer>, min: 0, max: ?max-index)
  #{ begin
       make-c-pointer(?slot-pointer-type-name,
                      primitive-machine-word-add
                        (primitive-cast-pointer-as-raw
                           (primitive-unwrap-c-pointer(?struct)),
                         integer-as-raw
                           (%c-struct-slot-offset(?slot-number,
                                                  ?struct-name)
                              + (?offset * ?slot-type-size))),
                      #[])
     end }
end method expand-slot-accessor;


define method expand-slot-accessor
    (m :: <struct-setter>, // setter method
     model-slot-type :: <&C-struct/union-designator-class>, // slot is a struct
     slot-rep :: <c-struct/union-array-slot-descriptor>, //  array
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let export = ^export-function(model-slot-type) | #{ identity };
  let slot-type-size = ^size-of(model-slot-type);
  let len = ^top-level-eval(slot-rep.array-length);
  unless (instance?(len, <integer>) & len > 0)
    note(<invalid-array-slot-bounds>,
         source-location: fragment-source-location(slot-rep.array-length),
         definition-name: #{ *unkown* },
         length-expression: slot-rep.array-length);
    len := 1;
  end unless;
  let max-index = len - 1;
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (new-value, struct, offset) = values(names[0], names[1], names[2]);
  // TODO: check bounds by using limited specializer
  // offset :: limited(<integer>, min: 0, max: ?max-index)
  #{ begin
       %pointer-replace-bytes
         (?struct, ?export(?new-value),
          byte-offset1:
            %c-struct-slot-offset(?slot-number,
                                  ?struct-name)
            + (?offset * ?slot-type-size),
          byte-offset2: 0,
          size: ?slot-type-size);
       ?new-value
         end}
end method expand-slot-accessor;

//// accessors for non-struct non-array, non-bitfield struct slots

define method expand-slot-accessor
    (m :: <struct-getter>, // getter method
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <c-struct/union-slot-descriptor>, // not an array or bitfield
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let low-type = ^low-level-type(model-slot-type);
  let boxer = ^boxer-function-name(model-slot-type);
  let dereferencer = ^raw-dereferencer-name(model-slot-type);
  let import = ^import-function(model-slot-type) | #{ identity };
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (struct) = values(names[0]);
  #{ begin
       ?import
         (boxer-for-designator
            (?low-type,
             (?dereferencer
               (primitive-unwrap-c-pointer(?struct),
                integer-as-raw(0),
                integer-as-raw
                  (%c-struct-slot-offset(?slot-number,
                                         ?struct-name)))),
             ?boxer))
     end}
end method expand-slot-accessor;


define method expand-slot-accessor
    (m :: <struct-setter>, // setter method
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <c-struct/union-slot-descriptor>, // not an array or bitfield
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let export = ^export-function(model-slot-type) | #{ identity };
  let unboxer = ^unboxer-function-name(model-slot-type);
  let dereferencer = ^raw-dereferencer-name(model-slot-type);
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (new-value, struct) = values(names[0], names[1]);
  #{ begin
      ?dereferencer(primitive-unwrap-c-pointer(?struct),
                    integer-as-raw(0),
                    integer-as-raw
                      (%c-struct-slot-offset(?slot-number,
                                             ?struct-name)))
        := ?unboxer(?export(?new-value));
       ?new-value
     end}
end method expand-slot-accessor;

define method expand-slot-accessor
    (m :: <struct-address-getter>, // address getter
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <abstract-c-struct/union-slot-descriptor>, // array or not
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);
  let slot-pointer-type-name = slot-rep.maybe-slot-pointer-type-name;
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (struct) = values(names[0]);
  #{ begin
       make-c-pointer(?slot-pointer-type-name,
                      primitive-machine-word-add
                        (primitive-cast-pointer-as-raw
                           (primitive-unwrap-c-pointer(?struct)),
                         integer-as-raw
                           (%c-struct-slot-offset(?slot-number,
                                                  ?struct-name))),
                      #[])
     end}
end method expand-slot-accessor;


//// bitfield struct slot accessors

define method expand-slot-accessor
    (m :: <struct-getter>, // getter method
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <c-struct/union-bitfield-slot-descriptor>, // bitfield
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let low-type = ^low-level-type(model-slot-type);
  let boxer = ^boxer-function-name(model-slot-type);
  let dereferencer
    = ^bitfield-dereferencer-name(model-slot-type);
  let import = ^import-function(model-slot-type) | #{ identity };
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (byte-offset, bit-offset, bit-size) =
    compute-aggregate-field-offset
          (struct-descriptor.^raw-type-info, slot-number);
  let (struct) = values(names[0]); // variable name
  #{ begin
       ?import
         (boxer-for-designator
            (?low-type,
             (?dereferencer
               (primitive-unwrap-c-pointer(?struct),
                integer-as-raw(?byte-offset),
                integer-as-raw(?bit-offset),
                integer-as-raw(?bit-size))),
             ?boxer))
     end }
end method expand-slot-accessor;


define method expand-slot-accessor
    (m :: <struct-setter>, // setter method
     model-slot-type :: <&designator-class>, // slot is not struct
     slot-rep :: <c-struct/union-bitfield-slot-descriptor>, // bitfield
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let export = ^export-function(model-slot-type) | #{ identity };
  let unboxer = ^unboxer-function-name(model-slot-type);
  let dereferencer
   = ^bitfield-dereferencer-name(model-slot-type);
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (byte-offset, bit-offset, bit-size) =
    compute-aggregate-field-offset
          (struct-descriptor.^raw-type-info, slot-number);
  let (new-value, struct) = values(names[0], names[1]);
  #{ begin
       ?dereferencer
         (primitive-unwrap-c-pointer(?struct),
          integer-as-raw(?byte-offset),
          integer-as-raw(?bit-offset),
          integer-as-raw(?bit-size))
         := ?unboxer(?export(?new-value));
       ?new-value
     end}
end method expand-slot-accessor;

//// struct accressors for array slot whose elements are not structs


define method expand-slot-accessor
    (m :: <struct-getter>, // getter method
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <c-struct/union-array-slot-descriptor>, // array
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let low-type = ^low-level-type(model-slot-type);
  let boxer = ^boxer-function-name(model-slot-type);
  let dereferencer = ^raw-dereferencer-name(model-slot-type);
  let import = ^import-function(model-slot-type) | #{ identity };
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let len = ^top-level-eval(slot-rep.array-length);
  unless (instance?(len, <integer>) & len > 0)
    note(<invalid-array-slot-bounds>,
         source-location: fragment-source-location(slot-rep.array-length),
         definition-name: #{ *unkown* },
         length-expression: slot-rep.array-length);
    len := 1;
  end unless;
  let max-index = len - 1;
  let names = method-argument-names(m);
  let (struct, offset) = values(names[0], names[1]);
  // TODO: check bounds by using limited specializer
  // offset :: limited(<integer>, min: 0, max: ?max-index)
  #{ begin
       ?import
         (boxer-for-designator
            (?low-type,
             (?dereferencer
               (primitive-unwrap-c-pointer(?struct),
                integer-as-raw(?offset),
                integer-as-raw
                  (%c-struct-slot-offset(?slot-number,
                                         ?struct-name)))),
             ?boxer))
     end }
end method expand-slot-accessor;


define method expand-slot-accessor
    (m :: <struct-setter>, // setter method
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <c-struct/union-array-slot-descriptor>, // array
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment);

  let export = ^export-function(model-slot-type) | #{ identity };
  let unboxer = ^unboxer-function-name(model-slot-type);
  let dereferencer = ^raw-dereferencer-name(model-slot-type);
  // TODO: bogus use of model-class-name
  let struct-name = struct-descriptor.model-class-name;
  let len = array-length(slot-rep);
  let len = ^top-level-eval(array-length(slot-rep));
  unless (instance?(len, <integer>) & len > 0)
    note(<invalid-array-slot-bounds>,
         source-location: fragment-source-location(slot-rep.array-length),
         definition-name: #{ *unkown* },
         length-expression: slot-rep.array-length);
    len := 1;
  end unless;
  let max-index = len - 1;
  let names = method-argument-names(m);
  let (new-value, struct, offset) = values(names[0], names[1], names[2]);
  // TODO: check bounds by using limited specializer
  // offset :: limited(<integer>, min: 0, max: ?max-index)
  #{ begin
       ?dereferencer(primitive-unwrap-c-pointer(?struct),
                     integer-as-raw(?offset),
                     integer-as-raw
                       (%c-struct-slot-offset(?slot-number,
                                              ?struct-name)))
         := ?unboxer(?export(?new-value));
       ?new-value
     end}
end method expand-slot-accessor;

// TODO: remove this method!!
define method expand-slot-accessor
    (m :: <struct-address-getter>, // address getter
     model-slot-type :: <&designator-class>, // slot is not a struct
     slot-rep :: <c-struct/union-array-slot-descriptor>, // array
     struct-descriptor :: <&C-struct/union-designator-class>,
     slot-number :: <integer>)
 => (fragment)
  let slot-pointer-type-name = slot-rep.maybe-slot-pointer-type-name;
  let struct-name = struct-descriptor.model-class-name;
  let names = method-argument-names(m);
  let (struct) = values(names[0]);
  #{ begin
       make-c-pointer(?slot-pointer-type-name,
                      primitive-machine-word-add
                        (primitive-cast-pointer-as-raw
                           (primitive-unwrap-c-pointer(?struct)),
                         integer-as-raw
                           (%c-struct-slot-offset(?slot-number,
                                                  ?struct-name))),
                      #[])
     end}
end method expand-slot-accessor;

/// This is the api so that the back end can resolve the lengths and
//  offsets of structs

/*

define method &C-struct/union-slot-descriptors
    (c :: <&designator-class>)
 => (_ :: <sequence>);
  c.struct-fields
end method &C-struct/union-slot-descriptors;

define method &C-struct/union-slot-raw-type
    (d :: <abstract-c-struct/union-slot-descriptor>)
 => (v :: <&class>)
  d.c-type.^raw-type-name;
end method &C-struct/union-slot-raw-type;

define method &C-struct-slot-bitfield?
    (d :: <abstract-c-struct/union-slot-descriptor>)
 => (v :: <boolean>);
  #f
end method &C-struct-slot-bitfield?;

define method &C-struct-slot-bitfield?
    (d :: <c-struct/union-bitfield-slot-descriptor>)
 => (v :: <boolean>);
  #t
end method &C-struct-slot-bitfield?;


define method &C-struct-slot-bitfield-width
    (d :: <c-struct/union-bitfield-slot-descriptor>)
 => (v :: <integer>);
  d.bitfield-width
end method &C-struct-slot-bitfield-width;

define method &C-struct/union-slot-array?
    (d :: <abstract-c-struct/union-slot-descriptor>)
 => (v :: <boolean>);
  #f
end method &C-struct/union-slot-array?;

define method &C-struct/union-slot-array?
    (d :: <c-struct/union-array-slot-descriptor>)
 => (v :: <boolean>);
  #t
end method &C-struct/union-slot-array?;

define method &C-struct/union-slot-array-length
    (d :: <c-struct/union-array-slot-descriptor>)
 => (v :: <integer>);
  d.array-length
end method &C-struct/union-slot-array-length;

*/

define &macro %pointer-replace-bytes
  { %pointer-replace-bytes
         (?destination:expression, ?source:expression,
          byte-offset1: ?byte-offset1:expression,
          byte-offset2: ?byte-offset2:expression,
          size: ?size-expr:expression) }
  => #{ primitive-replace-bytes!
        (primitive-unwrap-c-pointer(?destination),
         integer-as-raw(0),
         integer-as-raw(?byte-offset1),
         primitive-unwrap-c-pointer(?source),
         integer-as-raw(0),
         integer-as-raw(?byte-offset2),
         integer-as-raw(?size-expr)) }
end;

define &macro %c-dereference-bitfield
  { %c-dereference-bitfield(?raw-pointer:expression,
                            ?struct-name:expression,
                            ?slot-number:expression) }
  => begin
       let struct-model = ^eval-designator(struct-name);
               unless (struct-model)
          generate-unresolved-designator-error
            (struct-model, struct-model, #{ unknown}, #());
        end;

       let raw-type = ^raw-type-info(struct-model);
       let slot-number = ^top-level-eval(slot-number);
       let (byte-offset, bit-offset, bit-size) =
         compute-aggregate-field-offset(raw-type, slot-number);
       #{ primitive-access-field
           (?raw-pointer, integer-as-raw(?byte-offset), integer-as-raw(?bit-offset),
            integer-as-raw(?bit-size)) };
     end;
end;


define &macro %c-dereference-bitfield-setter
  { %c-dereference-bitfield-setter (?raw-new-value:expression,
                                    ?raw-pointer:expression,
                                    ?struct-name:expression,
                                    ?slot-number:expression) }
  => begin
       let struct-model = ^eval-designator(struct-name);
               unless (struct-name)
          generate-unresolved-designator-error
            (struct-name, struct-name, #{ unknown}, #());
        end;
       let raw-type = ^raw-type-info(struct-model);
       let slot-number = ^top-level-eval(slot-number);
       let (byte-offset, bit-offset, bit-size) =
         compute-aggregate-field-offset(raw-type, slot-number);
       #{ primitive-access-field-setter
           (?raw-new-value, ?raw-pointer, integer-as-raw(?byte-offset),
            integer-as-raw(?bit-offset), integer-as-raw(?bit-size)) };
     end;
end;



/// TODO: belongs with some procdural macro support tools.

define variable *gensym-counter* :: <integer> = 0;

define sideways method as
    (c == <variable-name-fragment>, o :: <symbol>)
 => (c :: <variable-name-fragment>);
  make-in-expansion(<variable-name-fragment>, name: o)
end method;


// TODO: belongs elsewhere...
define method gensym (#rest things) => (s :: <variable-name-fragment>);
  *gensym-counter* := *gensym-counter* + 1;
  if (empty?(things))
    as(<variable-name-fragment>,
       as(<symbol>, concatenate("gensym", hex-string(*gensym-counter*))));
  else
    let base-name = #f;
    let symbol
      = as(<symbol>,
           apply(concatenate,
                 map(method (x)
                       if (instance?(x, <variable-name-fragment>))
                         base-name := (base-name | x);
                       end;
                       as(<string>, x);
                     end method,
                     things)));
    if (base-name)
      make-variable-name-like
        (base-name, name: symbol,
         record: fragment-record(base-name),
         source-position: fragment-source-position(base-name));
    else
      as(<variable-name-fragment>, symbol);
    end;
  end if // (empty?)
end method gensym;

/* TODO: big hack */
define method hex-string (i :: <integer>)
  local method hexit (i :: <integer>, l :: <list>) => (l :: <string>);
          let (quotient :: <integer>, remainder :: <integer>)
            = truncate/(i, #x10);
          if(zero?(quotient))
            as(<string>,
               map(method (i :: <integer>) "0123456789ABCDEF"[i] end,
                   pair(remainder,l)))
            else
             hexit(quotient, pair(remainder, l))
          end if;
        end method hexit;
  hexit(i, #())
end method hex-string;


// TODO: temporary hack

// TODO: Rather than refer to it by name, just insert the model object
// itself as a literal fragment? I think some of the optimizers do this
// successfully.

define method model-class-name (model :: <&class>)
 => <f :: <variable-name-fragment>;
  model-variable-name(model);
end;


/// the following go with the definition for <&designator-class>


define &macro import-type-for
  { import-type-for(?class-expr:expression) }
  => begin
       let class = ^eval-designator(class-expr);
       if (designator-class?(class))
         let import-type = ^import-type-for(class);
         if (import-type)
           #{ ?import-type };
         else
           #{ <object> };
         end if;
       else
         #{ <object> };
       end if;
     end;
end;

define &macro export-type-for
  { export-type-for(?class-expr:expression) }
  => begin
       let class = ^eval-designator(class-expr);
       if (designator-class?(class))
         let export-type = ^export-type-for(class);
         if (export-type)
           #{ ?export-type };
         else
           #{ <object> };
         end if;
       else
         #{ <object> };
       end if;
     end;
end;

define method ^export-type-for (type :: <&designator-class>)
  => (type :: type-union(<&type>, <fragment>))
  ^ensure-pointer-types-initialized(type);
  type.^mapped-export-type
end;


define method ^export-type-for (type :: <&c-struct/union-designator-class>)
  => (type :: <&type>);
  ^ensure-pointer-types-initialized(type);
  type.^abstract-pointer-type;
end;


define method ^import-type-for (type :: <&designator-class>)
  => (type :: type-union(<&type>, <fragment>))
  ^ensure-pointer-types-initialized(type);
  type.^mapped-import-type
end;

define method ^import-type-for (type :: <&c-struct/union-designator-class>)
  => (type :: <&type>);
  ^ensure-pointer-types-initialized(type);
  type.^abstract-pointer-type;
end;


define &macro export-type-for-reference
  { export-type-for-reference(?class-expr:expression) }
  => begin
       let class = ^eval-designator(class-expr);
               if (designator-class?(class))
          ^ensure-pointer-types-initialized(class);
          let referenced-type = ^referenced-type(class);
          if (designator-class?(referenced-type))
            let export-type = ^export-type-for(referenced-type);
            #{ ?export-type };
          else
            #{ <object> };
          end if;
        else
          #{ <object> };
        end if;
     end;
end;

define &macro import-type-for-reference
  { import-type-for-reference(?class-expr:expression) }
  => begin
       let class = ^eval-designator(class-expr);
               if (designator-class?(class))
          ^ensure-pointer-types-initialized(class);
          let referenced-type = ^referenced-type(class);
          if (designator-class?(referenced-type))
            let import-type = ^import-type-for(referenced-type);
            #{ ?import-type };
          else
            #{ <object> };
          end if;
        else
          #{ <object> };
        end if;
     end;
end;

define &macro low-level-type-for
  { low-level-type-for(?class-expr:expression) }
  => begin
       let class = ^eval-designator(class-expr);
       unless (designator-class?(class))
         // TODO: Generate error?
//         format-out("*** low-level-type-for: Error, cannot resolve designator %=\n", class-expr);
//          generate-unresolved-designator-error
//            (class-expr, class-expr, #{ unknown }, #());
         class := ^eval-designator(#{ <C-void*> });
       end unless;

       ^ensure-pointer-types-initialized(class);
       let type-expr = class.^low-level-type;
       #{ ?type-expr }
     end;
end;


define &macro check-designator-defined
  { check-designator-defined(?type:expression, ?object-name:expression,
                             ?kind:name) }
    =>
    begin
      if (designator-class?(^eval-designator(type)))
        #{ ?type };
      else
        generate-unresolved-designator-error(type, object-name, kind, #());
        #{ <object> };
      end;
    end;
end;
