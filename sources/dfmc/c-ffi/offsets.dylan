Module: dfmc-c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define abstract dood-class <abstract-raw-type> (<object>)
  weak slot back-end = #f,
    reinit-expression: current-back-end();
  slot initialized? :: <boolean> = #f;
  slot raw-size :: false-or(<integer>) = #f;
  slot raw-alignment :: false-or(<integer>) = #f;
end;

define class <basic-raw-type> (<abstract-raw-type>)
  // a thing to pass on to the back end that it can determine the size, etc.
  constant slot raw-type-name, required-init-keyword: raw-name:;
end;

define abstract class <abstract-aggregate-field-indicator> (<object>)
  constant slot field-raw-type :: <abstract-raw-type>,
    required-init-keyword: raw-type:;
end;

define class <simple-field-indicator> (<abstract-aggregate-field-indicator>)
end;

define method field-width (f :: <abstract-aggregate-field-indicator>)
 => (i :: <integer>);
  0
end;

define class <bitfield-field-indicator> (<abstract-aggregate-field-indicator>)
  constant slot field-width :: <integer>, required-init-keyword: field-width:;
end;

define class <repeated-field-indicator> (<abstract-aggregate-field-indicator>)
  constant slot field-repeat-count :: <integer>,
    required-init-keyword: repeat-count:;
end;

define abstract class <abstract-aggregate-raw-type> (<abstract-raw-type>)
  constant slot raw-fields :: <sequence>, required-init-keyword: fields:;
  constant slot raw-options :: <sequence>, required-init-keyword: options:;
end;

define class <union-aggregate-raw-type> (<abstract-aggregate-raw-type>)
end;

define sealed domain make(singleton(<union-aggregate-raw-type>));
define sealed domain initialize(<union-aggregate-raw-type>);

define class <struct-aggregate-raw-type> (<abstract-aggregate-raw-type>)
end;

define sealed domain make(singleton(<struct-aggregate-raw-type>));
define sealed domain initialize(<struct-aggregate-raw-type>);

/*
define method c-ffi-type-size (designator :: <&designator-class>)
 => (size :: <integer>);
  raw-type-info-size(designator.^raw-type-info)
end;  

define method c-ffi-type-alignment (designator :: <&designator-class>)
 => (align :: <integer>);
  raw-type-info-alignment(designator.^raw-type-info)
end;  

*/

define method initialize-raw-type (raw-type :: <basic-raw-type>) => ();
  // !@#$ pass something to back end to get underlying alignment
  //      and size and set them in the raw type
  raw-type.raw-size := get-raw-type-size(raw-type.raw-type-name);
  raw-type.raw-alignment := get-raw-type-alignment(raw-type.raw-type-name);
  raw-type.back-end := current-back-end();
  raw-type.initialized? := #t;
end;

  
define method assure-initialized (raw-type :: <abstract-raw-type>) => ();
  unless (raw-type.initialized? & raw-type.back-end == current-back-end())
    initialize-raw-type(raw-type);
  end;
  values();
end;

define method initialize-raw-type
    (raw-type :: <abstract-aggregate-raw-type>) => ();
  unless (raw-type.initialized? & raw-type.back-end == current-back-end())
    do(compose(assure-initialized, field-raw-type),
       raw-type.raw-fields)
  end unless;
  raw-type.raw-size := compute-aggregate-size(raw-type);
  raw-type.raw-alignment := compute-aggregate-alignment(raw-type);
  values();
end;


define method raw-type-info-alignment (raw-type :: <abstract-raw-type>)
 => (a :: <integer>);
  assure-initialized(raw-type);
  raw-alignment(raw-type);
end;

define method raw-type-info-size (raw-type :: <abstract-raw-type>)
 => (a :: <integer>);
  assure-initialized(raw-type);
  raw-size(raw-type);
end;

define method compute-aggregate-alignment
    (raw-type :: <abstract-aggregate-raw-type>)
 => (i :: <integer>); 
  let running-alignment = 1;
  let pack-option
    = get-property(raw-type.raw-options, #"pack",
                   default: get-default-pack-option());
  let fields = raw-type.raw-fields;
  for (field :: <abstract-aggregate-field-indicator> in raw-type.raw-fields)
    let field-type = field.field-raw-type;
    let field-align = raw-type-info-alignment(field-type);
    let adjusted-alignment = min(field-align, pack-option);
    running-alignment := max(running-alignment, adjusted-alignment);
  end;
  running-alignment
end;

define method compute-field-size (field :: <abstract-aggregate-field-indicator>)
 => (size :: <integer>);
  raw-type-info-size(field.field-raw-type);
end;

define method compute-field-size (field :: <repeated-field-indicator>)
 => (size :: <integer>)
  field.field-repeat-count * next-method()
end;

define method compute-aggregate-size
    (raw-type :: <struct-aggregate-raw-type>)
 => (size :: <integer>)
  let running-size = 0;
  let running-bitfield-offset :: <integer> = 0;
  let running-alignment = 1;
  let pack-option
    = get-property(raw-type.raw-options, #"pack",
                   default: get-default-pack-option());
  let force-new-field? = #t;
  let previous-field-size = 0;
  for (field :: <abstract-aggregate-field-indicator> in raw-type.raw-fields)
    let field-type = field.field-raw-type;
    let field-size :: <integer> = compute-field-size(field);
    let field-width :: <integer> = field-width(field);
    let field-align = raw-type-info-alignment(field-type);
    let adjusted-alignment = min(field-align, pack-option);
    let next-bitfield-offset = running-bitfield-offset + field-width;
    if (field-width = 0)  // not a bitfield
      running-size := round-up-to-mod(running-size, adjusted-alignment)
                        + field-size;
      running-bitfield-offset := 0;
      next-bitfield-offset := 0;
      force-new-field? := #t;
    elseif (force-new-field?
	    | field-size ~= previous-field-size  // change in size of type
	    | (next-bitfield-offset > field-size * address-unit-bit-size()))
      running-size := round-up-to-mod(running-size, adjusted-alignment)
                        + field-size;
      running-bitfield-offset := 0;
      next-bitfield-offset := field-width;
      force-new-field? := #f;
    end if;
    running-alignment := max(running-alignment, adjusted-alignment);
    running-bitfield-offset := next-bitfield-offset;
    previous-field-size := field-size;
  end for;
  round-up-to-mod(running-size, running-alignment);
end method;

define method compute-aggregate-size
    (raw-type :: <union-aggregate-raw-type>)
 => (size :: <integer>);
  let running-size = 0;
  let running-alignment = 1;
  let pack-option
    = get-property(raw-type.raw-options, #"pack",
                   default: get-default-pack-option());
  for (field in raw-type.raw-fields)
    let field-type = field.field-raw-type;
    let field-size = compute-field-size(field);
    let field-align = raw-type-info-alignment(field-type);
    let adjusted-alignment = min(field-align, pack-option);
    running-size := max(running-size, field-size);
    running-alignment := max(running-alignment, adjusted-alignment);
  end;
  round-up-to-mod(running-size, running-alignment)
end;

define method compute-aggregate-field-offset
    (raw-type :: <union-aggregate-raw-type>,
     field :: <integer>)
 => (byte-offset :: <integer>, bit-offset :: <integer>,
     bit-size :: <integer>);
  values(0,0,0);
end;

define method compute-aggregate-field-offset
    (raw-type :: <struct-aggregate-raw-type>,
     field-index :: <integer>)
 => (byte-offset :: <integer>,
     bit-offset :: <integer>,
     bit-size :: <integer>);
  let fields = raw-type.raw-fields;
  if (field-index = 0)
    values(0, 0, field-width(fields[0]))
  else
    block (return)
      let running-offset = 0;
      let running-bitfield-offset :: <integer> = 0;
      let running-alignment = 1;
      let pack-option
        = get-property(raw-type.raw-options, #"pack",
                       default: get-default-pack-option());
      let force-new-field? = #t;
      let previous-field-size = 0;
      for (i :: <integer> from 0 to field-index)
        let field :: <abstract-aggregate-field-indicator> = fields[i];
        let field-type = field.field-raw-type;
        let field-size :: <integer> = compute-field-size(field);
        let field-width :: <integer> = field-width(field);
        let field-align = raw-type-info-alignment(field-type);
        let adjusted-alignment = min(field-align, pack-option);
        let next-bitfield-offset = running-bitfield-offset + field-width;
        if (field-width = 0)  // not a bitfield
          running-offset
            := round-up-to-mod(running-offset + previous-field-size,
                               adjusted-alignment);
          running-bitfield-offset := 0;
          next-bitfield-offset := 0;
          force-new-field? := #t;
        elseif (force-new-field?
	        | field-size ~= previous-field-size  // change in size of type
	        | (next-bitfield-offset > field-size * address-unit-bit-size()))
          running-offset
            := round-up-to-mod(running-offset + previous-field-size,
                               adjusted-alignment);
          running-bitfield-offset := 0;
          next-bitfield-offset := field-width;
          force-new-field? := #f;
        end if;
        if (i = field-index)
          return(running-offset, running-bitfield-offset, field-width);
        end if;
	running-alignment := max(running-alignment, adjusted-alignment);
	running-bitfield-offset := next-bitfield-offset;
	previous-field-size := field-size;
      end for;
    end block;
  end if;
end method;

define &macro %c-struct-slot-offset
  { %c-struct-slot-offset(?slot-number:expression,
			  ?struct-name:name) }
  =>
  begin
    let struct-class = ^eval-designator(struct-name);
    let slot-number = fragment-value(slot-number);
    let offset = compute-aggregate-field-offset
                   (struct-class.^raw-type-info, slot-number);
    #{ ?offset }
  end;
end;


define method compute-raw-type-info
    (designator :: <&C-struct-designator-class>,
     slots :: <sequence>)
 => (raw-type-info :: <struct-aggregate-raw-type>);
  make(<struct-aggregate-raw-type>,
       fields: map(curry(as, <abstract-aggregate-field-indicator>),
		   slots),
       options: designator.^options)
end;




define method compute-raw-type-info
    (designator :: <&C-union-designator-class>,
     slots :: <sequence>)
 => (raw-type-info :: <union-aggregate-raw-type>);
  make(<union-aggregate-raw-type>,
       fields: map(curry(as, <abstract-aggregate-field-indicator>),
		   slots),
       options: designator.^options)
end;

define method as (c == <abstract-aggregate-field-indicator>,
		  slot :: <c-struct/union-slot-descriptor>)
 => (res :: <simple-field-indicator>)
  let slot-type-model = ^eval-designator(slot.c-type);
  unless (designator-class?(slot-type-model))
    slot-type-model := ^eval-designator(#{ <C-void*> });
  end unless;
  assure-raw-type-info(slot-type-model);
  make(<simple-field-indicator>,
       raw-type: ^raw-type-info(slot-type-model))
end;

define method as (c == <abstract-aggregate-field-indicator>,
		  slot :: <c-struct/union-array-slot-descriptor>)
 => (res :: <repeated-field-indicator>)
  let slot-type-model = ^eval-designator(slot.c-type);
  unless (designator-class?(slot-type-model))
    slot-type-model := ^eval-designator(#{ <C-void*> });
  end unless;
  assure-raw-type-info(slot-type-model);
  let len = ^top-level-eval(slot.array-length);
  unless(instance?(len, <integer>) & len > 0)
    // Don't need to generate an error as expand-slot-accessor will do that
    len := 1;
  end unless;
  make(<repeated-field-indicator>,
       raw-type: ^raw-type-info(slot-type-model),
       repeat-count: len)
end;

define method as (c == <abstract-aggregate-field-indicator>,
		  slot :: <c-struct/union-bitfield-slot-descriptor>)
 => (res :: <bitfield-field-indicator>)
  let slot-type-model = ^eval-designator(slot.c-type);
  unless (designator-class?(slot-type-model))
    // TODO: Shouldn't this be an integer type?
    slot-type-model := ^eval-designator(#{ <C-void*> });
  end unless;
  assure-raw-type-info(slot-type-model);
  let width = ^top-level-eval(slot.bitfield-width);
  unless(instance?(width, <integer>) & width > 0)
    note(<invalid-bitfield-slot-width>,
	 source-location: fragment-source-location(slot.bitfield-width),
	 definition-name: #{ *unkown* },
	 width-expression: slot.bitfield-width);
    width := 1;
  end unless;
  make(<bitfield-field-indicator>,
       raw-type: ^raw-type-info(slot-type-model),
       field-width: width)
end;




// !@#$ some stubs to get us going...

define method get-raw-type-size (raw-name) => (s :: <integer>);
  let raw-type = ^top-level-eval(raw-name);
  if (raw-type)
    let raw-size = raw-type-size(raw-type);
    if (instance?(raw-size, <integer>))
      raw-size
    else
      4  // sometimes raw-size is `W'
    end if
  else
    // !@#$ just do something
    1
  end;
end;

define method get-raw-type-alignment (raw-name) => (s :: <integer>);
  let raw-type = ^top-level-eval(raw-name);
  if (raw-type)
    let raw-align = raw-type-alignment(raw-type);
    if (instance?(raw-align, <integer>))
      raw-align
    else
      4
    end if;
  else
    // !@#$ just do something
    1
  end;
end;

/*
define method combine-field-alignment
    (accumulated :: <integer>,
     this-align :: <integer>,
     options :: <sequence>)
  => (a :: <integer>);
  max(accumulated, this-align);
end;
*/

define method round-up-to-mod (int :: <integer>, modulus :: <integer>)
 => (i :: <integer>);
  modulus * ceiling/(int, modulus);
end;


// !@#$ this is really a function of the back end
define function get-default-pack-option () => (opt :: <integer>)
  // windows VC++
  8;
end;

// Are we considering any platforms that do not have 8 bit address units.
define constant $address-unit-bit-size = 8;

define function address-unit-bit-size () => (size :: <integer>)
  $address-unit-bit-size;
end function;
