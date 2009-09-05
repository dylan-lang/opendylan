Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

ignore(<raw-unicode-character>);
ignore(<raw-extended-float>);
ignore(environment-element);
ignore(%machine-word-data);
ignore(%single-float-data);
ignore(%double-float-data);
ignore(%%double-integer-low);
ignore(%%double-integer-high);

////
//// TAG CHECK
////
//// | data or address | 2 tag bits |
////

define constant $direct-object-classes :: <raw-pointer>
  = primitive-untraced-allocate
      (primitive-machine-word-multiply-low
	 (primitive-word-size(), integer-as-raw(4)));

define constant $direct-object-mm-wrappers :: <raw-pointer>
  = primitive-untraced-allocate
      (primitive-machine-word-multiply-low
	 (primitive-word-size(), integer-as-raw(4)));

define macro tag-bits
  { tag-bits(?x:expression) }
    => { primitive-machine-word-logand(primitive-cast-pointer-as-raw(?x), integer-as-raw(3))
       }
end macro;

define macro indirect-object-tag-bits?
  { indirect-object-tag-bits? (?bits:expression) }
    => { primitive-machine-word-equal?(?bits, integer-as-raw(0)) }
end macro;

/*
define inline function indirect-object? (x) => (value :: <boolean>)
  indirect-object-tag-bits?(tag-bits(x))
end function;
*/

define macro indirect-object?
  { indirect-object? (?x:expression) }
    => { indirect-object-tag-bits?(tag-bits(?x)) }
end macro;

////
//// LOW-LEVEL COMPARISONS
////

define macro pointer-id? 
  { pointer-id?(?x:expression, ?y:expression) }
    => { primitive-id?(?x, ?y) }
end macro;

define macro value-object?
  { value-object?(?x:expression) }
    => /* { indirect-object?(?x) 
	   & pointer-id?(indirect-object-class(indirect-object-class(?x)), <value-class>) } */
      { indirect-object?(?x) & logbit?(0 /* <value-class> subtype bit number */, 
				       mm-wrapper-subtype-mask(indirect-object-mm-wrapper(?x))) }
end macro;

////
//// DIRECT OBJECTS
////

define function install-direct-object-class
    (tag :: <integer>, class :: <class>) => ()
  primitive-element($direct-object-classes, integer-as-raw(tag), integer-as-raw(0))
    := class;
  primitive-element($direct-object-mm-wrappers, integer-as-raw(tag), integer-as-raw(0))
    := class-mm-wrapper(class);
end function;

install-direct-object-class(0, <unbound>);

////
//// <integer> | data | 01 |
////

install-direct-object-class(1, <integer>);

define constant <integer>-instance? = method (x, c == <integer>) => (well? :: <boolean>);
  primitive-machine-word-equal?(tag-bits(x), integer-as-raw(1))
end method;

ignore(<integer>-instance?);

////
//// <BYTE-CHARACTER> | ascii code | 10 |
////

install-direct-object-class(2, <byte-character>);

define constant <byte-character>-instance? = method (x, c == <byte-character>) => (well? :: <boolean>);
  primitive-machine-word-equal?(tag-bits(x), integer-as-raw(2))
end method;

ignore(<byte-character>-instance?);

////
//// <UNICODE-CHARACTER> | ascii code | 11 |
////

install-direct-object-class(3, <unicode-character>);

define constant <unicode-character>-instance? = method (x, c == <unicode-character>) => (well? :: <boolean>);
  primitive-machine-word-equal?(tag-bits(x), integer-as-raw(3))
end method;

ignore(<unicode-character>-instance?);

// BOOTED: define ... class <mm-wrapper> ... end;

ignore(mm-wrapper-fixed-part);
ignore(mm-wrapper-variable-part);
ignore(mm-wrapper-pattern-element);

define function make-mm-wrapper
    (implementation-class :: <implementation-class>, 
     fixed-part-header :: <integer>, 
     variable-part-header :: <integer>)
  let wrapper :: <mm-wrapper> = system-allocate-wrapper();
  wrapper.mm-wrapper-implementation-class := implementation-class;
  wrapper.mm-wrapper-fixed-part := integer-as-raw(fixed-part-header);
  wrapper.mm-wrapper-variable-part // store version 2 in high byte
    := primitive-machine-word-bit-field-deposit
         (integer-as-raw(2),
          integer-as-raw($machine-word-size - 8),
          integer-as-raw(8),
          integer-as-raw(variable-part-header));
  // wrapper.mm-wrapper-number-patterns := 0;
  wrapper
end function;

define macro %mm-wrapper-implementation-class
  { %mm-wrapper-implementation-class (?instance:expression) }
    => { primitive-element(?instance, integer-as-raw(0), primitive-header-size()) }
end macro;

define macro %mm-wrapper-implementation-class-setter
  { %mm-wrapper-implementation-class-setter (?value:expression, ?instance:expression) }
    => { primitive-element(?instance, integer-as-raw(0), primitive-header-size())
	   := ?value }
end macro;

define constant $number-header-words = 1;

/*
define inline-only function instance-header (instance) 
    => (value :: <mm-wrapper>)
  primitive-element(instance, integer-as-raw(0), integer-as-raw(0))
end function;
*/

define macro instance-header 
  { instance-header (?instance:expression) }
    => { primitive-element(?instance, integer-as-raw(0), integer-as-raw(0)) }
end macro;

define inline-only function instance-header-setter
    (new-value :: <mm-wrapper>, instance) => (value :: <mm-wrapper>)
  primitive-element(instance, integer-as-raw(0), integer-as-raw(0))
    := new-value
end function;

//define macro indirect-object-mm-wrapper
//  { indirect-object-mm-wrapper (?instance:expression) }
//    => { instance-header(?instance) }
//end macro;

define inline-only function indirect-object-mm-wrapper (instance) => (mm-wrapper ::<mm-wrapper>)
  let mm-wrapper :: <mm-wrapper> = instance-header(instance);
  mm-wrapper
end function;

////
//// INSTANCE
////

/*
define inline-only function indirect-object-class (instance) 
    => (value :: <class>)
  instance.instance-header.%mm-wrapper-implementation-class.%implementation-class-class
end function;
*/

/// TODO: NEED SETTER

//define macro indirect-object-implementation-class
//  { indirect-object-implementation-class (?instance:expression) }
//    => { %mm-wrapper-implementation-class
//	  (indirect-object-mm-wrapper(?instance)) }
//end macro;

//define macro indirect-object-class
//  { indirect-object-class (?instance:expression) }
//    => { %implementation-class-class
//	  (%mm-wrapper-implementation-class
//	     (indirect-object-mm-wrapper(?instance))) }
//end macro;

define inline-only function indirect-object-implementation-class (x) => (ic :: <implementation-class>)
  let mm-wrapper :: <mm-wrapper> = indirect-object-mm-wrapper(x);
  mm-wrapper-implementation-class(mm-wrapper)
end function;

define inline-only function indirect-object-class (x) => (c :: <class>)
  iclass-class(mm-wrapper-implementation-class(indirect-object-mm-wrapper(x)))
end function;


define macro direct-object-class-with-tag-bits
  { direct-object-class-with-tag-bits(?tag-bits:expression) }
    => { primitive-element($direct-object-classes, ?tag-bits, integer-as-raw(0)) }
end macro;


define inline-only function direct-object-class (instance) 
    => (value :: <class>)
  direct-object-class-with-tag-bits(tag-bits(instance))
end function;

/*
define macro direct-object-class
  { direct-object-class(?instance:expression) }
    => { direct-object-class-with-tag-bits(tag-bits(?instance)) }
end macro;
*/

define function object-class (instance) => (value :: <class>)
  let bits :: <raw-address> = tag-bits(instance);
  if (indirect-object-tag-bits?(bits))
    indirect-object-class(instance)
  else
    direct-object-class-with-tag-bits(bits)
  end if
end function;

//// OBJECT-MM-WRAPPER

define inline-only function direct-object-mm-wrapper-with-tag-bits
    (bits :: <machine-word>) => (res :: <mm-wrapper>)
  let mm-wrapper :: <mm-wrapper> 
    = primitive-element($direct-object-mm-wrappers, primitive-unwrap-machine-word(bits), integer-as-raw(0));
  mm-wrapper
end function;

define inline-only function direct-object-mm-wrapper
    (instance) => (res :: <mm-wrapper>)
  direct-object-mm-wrapper-with-tag-bits(primitive-wrap-machine-word(tag-bits(instance)))
end function;

/*
define macro direct-object-mm-wrapper
  { direct-object-mm-wrapper (?instance:expression) }
    => { direct-object-mm-wrapper-with-tag-bits(tag-bits(?instance)) }
end macro;

define macro direct-object-mm-wrapper-with-tag-bits
  { direct-object-mm-wrapper-with-tag-bits(?tag-bits:expression) }
    => { primitive-element($direct-object-mm-wrappers, ?tag-bits, integer-as-raw(0)) }
end macro;
*/

define inline function object-mm-wrapper (instance) => (value :: <mm-wrapper>)
  let bits :: <raw-address> = tag-bits(instance);
  if (indirect-object-tag-bits?(bits))
    indirect-object-mm-wrapper(instance)
  else
    direct-object-mm-wrapper-with-tag-bits(primitive-wrap-machine-word(bits))
  end if
end function;

define inline function object-implementation-class
    (instance) => (value :: <implementation-class>)
  let bits :: <raw-address> = tag-bits(instance);
  if (indirect-object-tag-bits?(bits))
    indirect-object-implementation-class(instance)
  else
    let wrapper :: <mm-wrapper> 
      = direct-object-mm-wrapper-with-tag-bits(primitive-wrap-machine-word(bits));
    mm-wrapper-implementation-class(wrapper)
  end if
end function;

//// SLOT ACCESS

define inline-only function initialized-slot-element (instance, offset :: <integer>)
  primitive-initialized-slot-value(instance, integer-as-raw(offset))
end function;

define inline-only function slot-element (instance, offset :: <integer>)
  primitive-slot-value(instance, integer-as-raw(offset))
end function;

define inline-only function slot-element-setter
    (new-value, instance, offset :: <integer>)
  primitive-slot-value(instance, integer-as-raw(offset))
    := new-value
end function;

define inline-only function repeated-slot-element
    (instance, offset :: <integer>, index :: <integer>)
  primitive-repeated-slot-value
    (instance, integer-as-raw(offset), integer-as-raw(index))
end function;

define inline-only function repeated-slot-element-setter
    (new-value, instance, offset :: <integer>, index :: <integer>)
  primitive-repeated-slot-value
      (instance, integer-as-raw(offset), integer-as-raw(index))
    := new-value
end function;


define inline-only function byte-slot-element
    (instance, base-offset :: <integer>, byte-offset :: <integer>)
 => (value :: <byte-character>)
  primitive-raw-as-byte-character
    (primitive-byte-element
       (instance, integer-as-raw(base-offset), integer-as-raw(byte-offset)))
end function;

define inline-only function byte-slot-element-setter
    (new-value :: <byte-character>, instance, 
     base-offset :: <integer>, byte-offset :: <integer>)
 => (value :: <byte-character>)
  primitive-byte-element
      (instance, integer-as-raw(base-offset), integer-as-raw(byte-offset))
    := primitive-byte-character-as-raw(new-value);
  new-value
end function;

////
//// ALLOCATION
////

/// !@#$ THESE SHOULD BE UNIFIED

/*

 New Interface to primitive-allocate:

  size of memory to be allocated
  MM class wrapper
  number of slots to fill
  fill value
  repeated-size(tagged)
  repeated-size slot offset

*/

// TODO: Put back inline-only when all calls can be inlined.

 /* inline-only */
define inline function system-allocate-simple-instance-i
    (iclass :: <implementation-class>, #key fill = %unbound)
  let storage-size = iclass.instance-storage-size;
  primitive-object-allocate-filled
    (integer-as-raw($number-header-words + storage-size),
     iclass.iclass-instance-header,
     integer-as-raw(storage-size),
     fill,
     integer-as-raw(0),
     integer-as-raw(0),
     fill);
end function;

/* inline-only ? */
define inline function system-allocate-simple-instance
    (class :: <class>, #key fill = %unbound)
  system-allocate-simple-instance-i
    (class-implementation-class(class), fill: fill)
end function;


define generic system-allocate-repeated-instance
    (class :: <class>, type :: <type>, fill, 
     repeated-size :: <integer>, repeated-fill)
 => (instance);

/// REPEATED OBJECT INSTANCE ALLOCATION -- DEFAULT

define inline method system-allocate-repeated-instance
    (class :: <class>, type :: <type>, fill, 
     repeated-size :: <integer>, repeated-fill)
 => (instance)
  system-allocate-repeated-object-instance(class, fill, repeated-size, repeated-fill)
end method;

define macro repeated-instance-allocator-definer
  { define ?adj:* repeated-instance-allocator (?:name, ?alloc:name, ?type:name, ?unboxer:name) }
    => { define ?adj repeated-instance-allocator-aux (?name, ?alloc, ?type, ?unboxer);

	 define inline-only function "system-allocate-repeated-" ## ?name ## "-instance"
	     (class :: <class>, fill, repeated-size :: <integer>, repeated-fill :: ?type)
	   "system-allocate-repeated-" ## ?name ## "-instance-i"
	     (class-implementation-class(class), fill, repeated-size, repeated-fill)
	 end function;

	 define inline method system-allocate-repeated-instance
	     (class :: <class>, type == ?type, fill, 
	      repeated-size :: <integer>, repeated-fill :: ?type)
          => (instance)
	   "system-allocate-repeated-" ## ?name ## "-instance"
	     (class, fill, repeated-size, repeated-fill)
	 end method; }
end macro;

/*
/// TODO: USE THIS OF ICLASS PROPERTY FOR LEAF OBJECTS

define function mm-wrapper-raw-fixed-part? 
    (mm-wrapper :: <mm-wrapper>) => (well? :: <boolean>)
  let fixed-part = raw-as-integer(mm-wrapper.mm-wrapper-fixed-part);
  logbit?(1, fixed-part) &
    block (return)
      for (i :: <integer> from 0 below mm-wrapper-number-patterns(mm-wrapper))
	let pattern = mm-wrapper-pattern-element(mm-wrapper, i);
	unless (primitive-machine-word-equal?(pattern, integer-as-raw(0)))
	  return(#f)
	end unless;
      end for;
      #t
    end block;
end function;
*/

define macro repeated-instance-allocator-aux-definer
  { define repeated-instance-allocator-aux (?:name, ?alloc:name, ?type:name, ?unboxer:name) }
    => { define inline-only function "system-allocate-repeated-" ## ?name ## "-instance-i"
	     (iclass :: <implementation-class>, fill, 
	      repeated-size :: <integer>, repeated-fill :: ?type)
	   let size-offset       = iclass.instance-storage-size;
	   let raw-size-offset   = iclass.instance-storage-size;
	   let raw-number-words  = integer-as-raw($number-header-words + size-offset);
	   let mm-wrapper        = iclass.iclass-instance-header;
	   let raw-number-slots  = integer-as-raw(size-offset - 1);
	   let raw-repeated-size = integer-as-raw(repeated-size);
	   let raw-size-offset   = integer-as-raw(size-offset);
	   let raw-repeated-fill = ?unboxer(repeated-fill);
	   "primitive-" ## ?alloc ## "-allocate-filled"
	     (raw-number-words, mm-wrapper, raw-number-slots, fill,
	      raw-repeated-size, raw-size-offset, raw-repeated-fill);
	 end function; }
  { define leaf repeated-instance-allocator-aux (?:name, ?alloc:name, ?type:name, ?unboxer:name) }
    => { define inline-only function "system-allocate-repeated-" ## ?name ## "-instance-i"
	     (iclass :: <implementation-class>, fill, 
	      repeated-size :: <integer>, repeated-fill :: ?type)
	   let size-offset       = iclass.instance-storage-size;
	   let raw-size-offset   = iclass.instance-storage-size;
	   let raw-number-words  = integer-as-raw($number-header-words + size-offset);
	   let mm-wrapper        = iclass.iclass-instance-header;
	   let raw-number-slots  = integer-as-raw(size-offset - 1);
	   let raw-repeated-size = integer-as-raw(repeated-size);
	   let raw-size-offset   = integer-as-raw(size-offset);
	   let raw-repeated-fill = ?unboxer(repeated-fill);
	   // if (mm-wrapper-raw-fixed-part?(mm-wrapper))
	   // HACK: FOR NOW UNTIL WE CAN IDENTIFY THESE CLASSES 
	   //       EITHER BY MM-WRAPPER OR ICLASS PROPERTY BIT
	   if (~subclass?(iclass-class(iclass), <simple-array>))
 	     "primitive-" ## ?alloc ## "-allocate-leaf-filled"
	       (raw-number-words, mm-wrapper, raw-number-slots, fill,
	        raw-repeated-size, raw-size-offset, raw-repeated-fill);
	   else 
	     "primitive-" ## ?alloc ## "-allocate-filled"
	       (raw-number-words, mm-wrapper, raw-number-slots, fill,
	        raw-repeated-size, raw-size-offset, raw-repeated-fill);
	   end if
	 end function; }
end macro;

define repeated-instance-allocator
  (object, object, <object>, identity);
define leaf repeated-instance-allocator
  (byte-character, byte, <byte-character>, primitive-byte-character-as-raw);
define repeated-instance-allocator
  (unicode-character, word, <unicode-character>, primitive-unicode-character-as-raw);
define leaf repeated-instance-allocator
  (byte, byte, <byte>, integer-as-raw);
define repeated-instance-allocator
  (double-byte, double-byte, <double-byte>, integer-as-raw);
define repeated-instance-allocator
  (word, word, <machine-word>, primitive-unwrap-machine-word);
// define repeated-instance-allocator
//   (double-word, double-word, <double-integer>, primitive-unwrap-double-integer);
define repeated-instance-allocator
  (single-float, single-float, <single-float>, primitive-single-float-as-raw);
define repeated-instance-allocator
  (double-float, double-float, <double-float>, primitive-double-float-as-raw);

/// TERMINATED REPEATED BYTE ALLOCATION

define inline-only function system-allocate-repeated-byte-instance-terminated-i
    (iclass :: <implementation-class>, repeated-size :: <integer>, fill)
  let size-offset = iclass.instance-storage-size;
  let nul-adjust = 1;  // extra byte for nul terminator
  primitive-byte-allocate-leaf-filled-terminated
    (integer-as-raw($number-header-words + size-offset),
     integer-as-raw(repeated-size + nul-adjust),
     iclass.iclass-instance-header,
     integer-as-raw(size-offset - 1),
     fill,
     integer-as-raw(repeated-size),
     integer-as-raw(size-offset));
end function;

define inline-only function system-allocate-repeated-byte-instance-terminated
    (class :: <class>, repeated-size :: <integer>, fill)
  system-allocate-repeated-byte-instance-terminated-i(class-implementation-class(class), repeated-size, fill)
end function;

/// WEAK REPEATED INSTANCES

define inline-only function system-allocate-weak-repeated-instance
    (class :: <class>, repeated-size :: <integer>, fill, assoc-link)
  system-allocate-weak-repeated-instance-i(class-implementation-class(class), 
					   repeated-size, fill, assoc-link)
end function;

define inline-only function system-allocate-weak-repeated-instance-i
    (iclass :: <implementation-class>, repeated-size :: <integer>, fill, assoc-link)
  let size-offset = iclass.instance-storage-size;
  primitive-allocate-weak-in-awl-pool
    (integer-as-raw($number-header-words + size-offset + repeated-size),
     iclass.iclass-instance-header,
     integer-as-raw(size-offset + repeated-size),
     fill,
     integer-as-raw(repeated-size),
     integer-as-raw(size-offset),
     assoc-link);
end function;


define inline-only function system-allocate-strong-repeated-instance
    (class :: <class>, repeated-size :: <integer>, fill)
  system-allocate-strong-repeated-instance-i
    (class-implementation-class(class), repeated-size, fill)
end function;

define inline-only function system-allocate-strong-repeated-instance-i
    (iclass :: <implementation-class>, repeated-size :: <integer>, fill)
  let size-offset = iclass.instance-storage-size;
  primitive-allocate-in-awl-pool
    (integer-as-raw($number-header-words + size-offset + repeated-size),
     iclass.iclass-instance-header,
     integer-as-raw(size-offset + repeated-size),
     fill,
     integer-as-raw(repeated-size),
     integer-as-raw(size-offset),
     #f);
end function;


define inline-only function system-allocate-wrapper ()
  let class = <mm-wrapper>;
  let repeated-size = 0;
  let size-offset = class.instance-storage-size;
  primitive-allocate-wrapper
    (integer-as-raw($number-header-words + size-offset + repeated-size),
     class.class-instance-header,
     integer-as-raw(size-offset + repeated-size),
     %unbound,
     integer-as-raw(repeated-size),
     integer-as-raw(size-offset));
end function;

////
//// DYNAMIC SPECIALIZERS SUPPORT
////

define not-upgrade not-inline function make-<signature>
    (next? :: <boolean>,
     required :: <simple-object-vector>,
     values :: <simple-object-vector>,
     rest-value /* false-or(<type>) */,
     signature-properties :: <integer>)
 => (signature :: <signature>)
  make(<signature>,
       key?:       #f,
       required:   required,
       values:     values,
       rest-value: rest-value,
       next?:      next?,
       properties: signature-properties)
end;

define not-upgrade not-inline function make-<keyword-signature>
    (next? :: <boolean>,
     required :: <simple-object-vector>,
     values :: <simple-object-vector>,
     rest-value /* false-or(<type>) */,
     signature-properties :: <integer>,
     keys :: <simple-object-vector>,
     key-types :: <simple-object-vector>)
 => (signature :: <signature>)
  make(<signature>,
       key?:       #t,
       required:   required,
       keys:       keys,
       key-types:  key-types,
       values:     values,
       rest-value: rest-value,
       next?:      next?,
       properties: signature-properties)
end;

define function function-required-type
    (function :: <function>, index :: <integer>)
 => (type :: <type>)
  signature-required(function-signature(function))[index]
end;

define function function-key-type
    (function :: <function>, index :: <integer>)
 => (type :: <type>)
  signature-key-types(function-signature(function))[index]
end;

define function function-value-type
    (function :: <function>, index :: <integer>)
 => (type :: <type>)
  signature-values(function-signature(function))[index]
end;

define function function-rest-value-type (function :: <function>)
 => (type :: false-or(<type>))
  signature-rest-value(function-signature(function))
end;

/*
////
//// CLOSURE SUPPORT !@#$ NOT IMPLEMENTED YET
////

define function %copy-method (function :: <lambda>) => (copy :: <lambda>)
  function.copy-simple-instance
end function;

define function %copy-method-using-environment
    (function :: <lambda>, new-environment)
  let new-function :: <lambda> = function.%copy-method;
  new-function.environment := new-environment;
  new-function
end function;

define function %copy-method-using-signature
    (function :: <lambda>, new-signature :: <signature>)
  let new-function :: <lambda> = function.%copy-method;
  new-function.function-signature := new-signature;
  new-function
end function;
*/

////
//// <TRACEABLE-VALUE-CELL>
////

define primary class <traceable-value-cell> (<object>)
  constant slot value-cell-object :: <object>;
end class;

ignore(<traceable-value-cell>);
ignore(value-cell-object);

////
//// <UNTRACEABLE-VALUE-CELL>
////

define primary class <untraceable-value-cell> (<object>)
  constant slot value-cell-raw-object :: <raw-pointer>;
end class;

ignore(<untraceable-value-cell>);
ignore(value-cell-raw-object);

define primary class <untraceable-double-value-cell> (<object>)
  constant slot value-cell-raw-object-1 :: <raw-pointer>;
  constant slot value-cell-raw-object-2 :: <raw-pointer>;
end class;

ignore(<untraceable-double-value-cell>);
ignore(value-cell-raw-object-1);
ignore(value-cell-raw-object-2);


////
//// <UNBOUND>
////

// BOOTED: define class <unbound> ... end;
// BOOTED: define constant %unbound ...;

// !@#$ BOOT ME

define inline function unbound ()
  %unbound
end function;

define inline function unbound? (object)
  object == unbound()
end function;

////
//// <EMPTY-LIST>
////

// BOOTED: define constant %empty-list = ...;

////
//// SPECIAL STUFF
////

define constant %bs-empty = ""; 

%bs-empty;

define constant %sv-empty = #(); // Hack!!! #[]

%sv-empty;

// UNUSED AT PRESENT
// (define %supplied?? (system-allocate-simple-instance <object>))
//

define constant %unsupplied? = %unsupplied?;

// !@#$ BOOT ME

// UNUSED AT PRESENT
// (define %special-rest-marker (system-allocate-simple-instance <object>))

// NEED THIS TO INTRODUCE KEYWORD

// define constant %allow-other-keys = #(#"allow-other-keys");

////
//// MULTIPLE-VALUES SUPPORT
////

define function values (#rest arguments)
  %dynamic-extent(arguments);
  primitive-values(arguments);
end function;

////
//// <BOOLEAN> SUPPORT
////

// BOOTED: define class <boolean> ... end;
// BOOTED: define constant %true = ...;
// BOOTED: define constant %false = ...;

define method uninstantiable-error (class)
  error("Cannot instantiate %=, it is not an instantiable type.", class)
end method;

define sealed method make (class == <boolean>, #rest all-keys, #key) => (res)
  uninstantiable-error(class);
end method;

/////
///// LOW-LEVEL ERROR SUPPORT
/////

/// !@#$ THIS SHOULD GO ELSEWHERE
/// !@#$ IT SHOULD BE BOUND AS IN INTERPRETER
/// !@#$ NEED A VALUE FOR RUNNING STANDALONE

// define variable *last-top-level* = #f;


define class <immutable-error> (<simple-error>) end;

define class <argument-error> (<simple-error>) end;
define class <keyword-error> (<argument-error>) end;
define class <missing-keyword-error> (<keyword-error>) end;


define class <odd-keyword-arguments-error> (<keyword-error>) end;

define not-inline function odd-keyword-arguments-error (function :: <function>)
 => (will-never-return :: <bottom>)
  error(make(<odd-keyword-arguments-error>,
             format-string: "Attempted to call %= with an odd number of keywords", 
             format-arguments: list(function)))
end function;


define class <unknown-keyword-argument-error> (<keyword-error>) end;

define not-inline function unknown-keyword-argument-error
    (function :: <function>, keyword :: <symbol>)
 => (will-never-return :: <bottom>)
  error(make(<unknown-keyword-argument-error>,
             format-string: "Attempted to call %= with an unknown keyword %=",
             format-arguments: list(function, keyword)))
end function;


define class <argument-count-error> (<argument-error>) end;

define not-inline function argument-count-error
    (function :: <function>, argument-count :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<argument-count-error>,
             format-string: "Attempted to call %= with %d arguments",
             format-arguments: list(function, argument-count)))
end function;


define class <argument-count-overflow-error> (<argument-error>) end;

define not-inline function argument-count-overflow-error
    (function :: <function>, 
     argument-count :: <integer>, argument-count-max :: <integer>)
 => (will-never-return :: <bottom>)
  error(make(<argument-count-overflow-error>,
             format-string: "Function %= called with %d > %d arguments", 
             format-arguments: list(function, argument-count, argument-count-max)))
end function;

define not-inline function bad-function-error (function)
 => (will-never-return :: <bottom>)
  error(make(<type-error>, value: function, type: <function>));
end function;

bad-function-error;


define not-inline function type-check-error (value, type)
 => (will-never-return :: <bottom>)
  error(make(<type-error>, value: value, type: type));
end function;

define class <stack-overflow-error> (<simple-error>) end;

define not-inline function stack-overflow-error ()
 => (will-never-return :: <bottom>)
  let name = thread-name(current-thread());
  let condition 
    = if (name)
        make(<stack-overflow-error>,
             format-string: "Stack overflow on current thread, %=.",
             format-arguments: list(name))
      else
        make(<stack-overflow-error>,
             format-string: "Stack overflow on current (unnamed) thread.",
             format-arguments: #());
      end;
  error(condition);
end function;


////
//// ERROR FUNCTIONS FOR NEW DISPATCH
////


define class <dispatch-error> (<simple-error>) end;

define open generic ambiguous-method-error (gf :: <generic-function>, args :: <sequence>, 
					    ordered :: <sequence>, ambig :: <collection>);



define method ambiguous-method-error (gf :: <generic-function>, args :: <sequence>,
				      ordered :: <sequence>, ambig :: <collection>)
  error(make(<dispatch-error>,
             format-string:
               "Method selection is ambiguous applying %= to %= - got %= ordered methods, %= unorderable", 
             format-arguments: list(gf, args, ordered, ambig)))
end method;



define not-inline function no-applicable-method-error
    (gf :: <generic-function>, args :: <simple-object-vector>)
  block ()
    error(make(<dispatch-error>,
  	       format-string: "No applicable method, applying %= to %=.", 
	       format-arguments: list(gf, args)))
  exception (<simple-restart>,
             init-arguments: 
                vector(format-string: 
                         "Try calling %= again with arguments: %=.",
                       format-arguments: 
                         vector(gf, args)))
    apply(gf, args);
  end;
end function;


define constant repeated-slot-getter-index-out-of-range-trap = method
    (inst, idx :: <integer>)
  let sd :: <repeated-slot-descriptor> = repeated-slot-descriptor(object-class(inst));
  error(make(<invalid-index-error>,
             format-string: "Out of range attempting to fetch %= of %= at index %=.",
	     format-arguments: list(slot-getter(sd) | sd, inst, idx)))
end method;

define constant repeated-slot-setter-index-out-of-range-trap = method
    (value, inst, idx :: <integer>)
  let sd :: <repeated-slot-descriptor> = repeated-slot-descriptor(object-class(inst));
  error(make(<invalid-index-error>,
             format-string: "Out of range attempting to store %= into %= of %= at index %=.",
             format-arguments: list(value, slot-getter(sd) | sd, inst, idx)))
end method;
	

// Re-spread arguments from mepargs format.
// We always return a heap-consed vector for the sake of storing the args in conditions.
define constant reconstruct-args-from-mepargs = method (gf :: <generic-function>, 
							mepargs :: <simple-object-vector>)
  let signature :: <signature> = function-signature(gf);
  let n :: <integer> = size(mepargs);
  let (nreq :: <integer>, nopt :: <integer>, optvec :: <simple-object-vector>)
    = if (signature-optionals?(signature))
	let optvec :: <simple-object-vector> = vector-element(mepargs, n - 1);
	values(n - 1, size(optvec), optvec)
      else
	values(n, 0, #[])
      end if;
  let args :: <simple-object-vector> = make(<simple-object-vector>, size: nopt + nreq);
  for (i :: <integer> from 0 below nreq)
    vector-element(args, i) := vector-element(mepargs, i) 
  end for;
  for (i :: <integer> from nreq, j :: <integer> from 0 below nopt)
    vector-element(args, i) := vector-element(optvec, j)
  end for;
  args
end method;


define constant reconstruct-keywords = method (keyvec :: <simple-object-vector>, 
					       method-keyword-table-format?)
  if (method-keyword-table-format?)
    let ndata :: <integer> = size(keyvec);
    let nkeys :: <integer> = ash(ndata, -1);
    let nkeyvec :: <simple-object-vector> = make(<simple-object-vector>, size: nkeys);
    for (i :: <integer> from 0 below nkeys, j :: <integer> from 0 by 2)
      vector-element(nkeyvec, i) := vector-element(keyvec, j)
    end for;
    nkeyvec
  else
    // generic functions use just a vector of the keywords currently.
    keyvec
  end if
end method;

     
define constant odd-number-of-keyword-args-trap = method
    (mepargs :: <simple-object-vector>, disphdr :: <dispatch-starter>, engine-node)
  engine-node;			// Maybe someday.
  let gf :: <generic-function> = parent-gf(disphdr);
  error(make(<odd-keyword-arguments-error>,
             format-string: 
               "The function %= was called with an odd number of keyworded arguments in args %=",
             format-arguments: list(gf, reconstruct-args-from-mepargs(gf, mepargs))))
end method;
  


define variable *gf-invalid-keyword-error-is-warning* = #t;



define constant invalid-keyword-trap = method
    ( mepargs :: <simple-object-vector>, disphdr :: <dispatch-starter>,
     engine-node :: <single-method-engine-node>, key,
     keyvec :: <simple-object-vector>, implicit? :: <boolean>)
  engine-node;			// Maybe someday.
  let gf :: <generic-function> = parent-gf(disphdr);
  let args = reconstruct-args-from-mepargs(gf, mepargs);
  if (~instance?(key, <symbol>))
    error(make(<missing-keyword-error>,
               format-string: 
                 "The function %= was given %= where a keyword was expected in the call with arguments %=",
               format-arguments: list(gf, key, args)))
  elseif (*gf-invalid-keyword-error-is-warning*)
    signal("The function %= was given the unrecognized keyword %= in the call with arguments %=.\n"
	     "The keywords recognized for this call are %=.",
	   gf, key, args, reconstruct-keywords(keyvec, implicit?));
    %method-apply-with-optionals(single-method-engine-node-method(engine-node),
				 single-method-engine-node-data(engine-node),
				 mepargs)
  else
    error(make(<unknown-keyword-argument-error>,
	       format-string: 
                 "The function %= was given the unrecognized keyword %= in the call with arguments %=.\n"
                 "The keywords recognized for this call are %=.",
               format-arguments: list(gf, key, args, reconstruct-keywords(keyvec, implicit?))))
  end if
end method;



////
//// EXTRAS IN LOW RUN-TIME
////

define function apply (function :: <function>, #rest arguments)
  %dynamic-extent(arguments);
  let size :: <integer> = arguments.size;
  vector-element(arguments, size - 1)
    := as(<simple-object-vector>, vector-element(arguments, size - 1));
  primitive-apply(function, arguments);
end function;

define inline function %method-apply-with-optionals
    (function, next-methods, arguments)
  primitive-mep-apply-with-optionals(function, next-methods, arguments)
end function;

/// MEMORY MANAGEMENT FUNCTIONS

define inline function address-of (object :: <object>) => (address :: <machine-word>)
  primitive-wrap-machine-word(primitive-cast-pointer-as-raw(object))
end function address-of;

/// @@@@ SHOULD GO ELSEWHERE

define method invoke-debugger (condition :: <condition>)
  primitive-invoke-debugger
    ("Condition of class %= occurred",
     vector(object-class(condition)));
  #f
end method invoke-debugger;

define method invoke-debugger (condition :: <simple-condition>)
  primitive-invoke-debugger
    (condition-format-string(condition), 
     as(<simple-object-vector>, condition-format-arguments(condition)));
  #f
end method invoke-debugger;

define method inside-debugger? () => (debugging? :: <boolean>)
  primitive-inside-debugger?()
end method inside-debugger?;

/// $NOT-FOUND GLOBAL CONSTANT

// define constant $not-found = system-allocate-simple-instance(<object>);
define constant $not-found :: <list> = #("NOT FOUND");



// define constant $dummy-implementation-class = <object>;

/// LIBRARY VERSION CHECKING

define class <library-version-error> (<simple-error>) end;

define generic library-version-error (library :: <library>, used-library :: <used-library>);

define method library-version-error (lib :: <library>, used-lib :: <used-library>)
  let used-lib-lib = used-library(used-lib);
  error(make(<library-version-error>,
	     format-string:
	       "Version mismatch: Library %= expected version %=.%= of library %=, "
	       "but got version %=.%= instead",
	     format-arguments:
	       list(namespace-name(lib),
		    library-major-version(used-lib),
		    library-minor-version(used-lib),
		    namespace-name(used-lib-lib),
		    library-major-version(used-lib-lib),
		    library-minor-version(used-lib-lib))))
end method;

// define class <library-incompatibility-error> (<library-version-error>) end;

define generic library-incompatibility-error
    (library :: <library>, used-library :: <used-library>);

define method library-incompatibility-error
    (lib :: <library>, used-lib :: <used-library>)
  let used-lib-lib = used-library(used-lib);
  error(make(<library-version-error>,
	     format-string:
	       "Library incompatibility: library %= was compiled in production mode "
	       "against an older version of used library %=, and is not compatible "
	       "with the newer version.  Library %= must be recompiled to use this "
	       "version of library %=",
	     format-arguments:
	       list(namespace-name(lib),
		    namespace-name(used-lib-lib),
		    namespace-name(lib),
		    namespace-name(used-lib-lib))))
end method;

define constant $library-build-count-wildcard = -1;
define constant $library-build-count-only-wildcard = -2;

define inline function system-developer-library? (lib :: <library>) => (well? :: <boolean>)
  library-build-count(lib) == $library-build-count-wildcard
end function;

define inline function major-minor-only-library? (lib :: <library>) => (well? :: <boolean>)
  library-build-count(lib) == $library-build-count-only-wildcard
end function;

define variable *version-checks?* = #t;

define function version-checks?-setter (well?)
  *version-checks?* := well?;
end function;

define inline function version-checks? () => (well? :: <boolean>)
  *version-checks?*
end function;

define function %used-library-version-check
    (lib :: <library>, used-lib :: <used-library>)
  let used-lib-lib = used-library(used-lib);
  unless (~version-checks?()
	    | system-developer-library?(used-lib-lib)
	    | system-developer-library?(lib))
    if (library-major-version(used-lib-lib) ~== library-major-version(used-lib)
	  | library-minor-version(used-lib-lib) < library-minor-version(used-lib))
      library-version-error(lib, used-lib)
    elseif (used-library-binding(used-lib) == #"tight"
	      & ~major-minor-only-library?(lib)
	      & ~major-minor-only-library?(used-lib-lib)
	      & library-build-count(used-lib-lib) ~== library-build-count(used-lib))
      library-incompatibility-error(lib, used-lib)
    end if
  end unless
end function;

define function %library-version-check (lib :: <library>, module)
  for (ul in used-libraries(lib))
    %used-library-version-check(lib, ul);
  end for;
  // register the runtime module
  dylan-runtime-module-handle(lib.namespace-name) := module;
end function;


// A simple runtime mapping from internal dylan-library names (as symbols)
// to their runtime DLL module handles (what addresses the DLLs are actually
// loaded into memory)

define variable *dylan-runtime-modules* = #f;
define variable *dylan-runtime-module* = #f;

define function dylan-runtime-module-handle(library :: <symbol>) => (module)
  if (*dylan-library-initialized?*)
    if (library == #"dylan")
      *dylan-runtime-module*
    else
      unless (*dylan-runtime-modules*)
	*dylan-runtime-modules* := make(<table>);
      end unless;
      element(*dylan-runtime-modules*, library, default: #f);
    end if;
  else
    *dylan-runtime-module*
  end if;
end function;

define function dylan-runtime-module-handle-setter(module, library :: <string>)
 => (module)
  if (*dylan-library-initialized?*)
    if (library = "dylan")
      *dylan-runtime-module* := module
    else
      unless (*dylan-runtime-modules*)
	*dylan-runtime-modules* := make(<table>);
      end unless;
      *dylan-runtime-modules*[as(<symbol>, library)] := module
    end if;
  else
    *dylan-runtime-module* := module
  end if;
end function;

define function lookup-runtime-module(library :: <symbol>) => (module)
  dylan-runtime-module-handle(library)
end function;


/// SHARED SYMBOLS

define shared-symbols %shared-dylan-symbols
  #"above", 
  #"abstract?", 
  #"all-keys?", 
  #"allocation", 
  #"allow-other-keys", 
  #"arguments", 
  #"below", 
  #"by", 
  #"capacity", 
  #"class", 
  #"code", 
  #"collections", 
  #"constant", 
  #"count", 
  #"debug-name", 
  #"default", 
  #"dimensions", 
  #"domain-types", 
  #"each-subclass", 
  #"element-type", 
  #"end", 
  #"failure", 
  #"fill", 
  #"fixed-part", 
  #"format-argument", 
  #"format-arguments", 
  #"format-string", 
  #"from", 
  #"function", 
  #"generic-function", 
  #"getter", 
  #"grow-size-function", 
  #"hash-function", 
  #"high", 
  #"implementation-class", 
  #"inherited-slots", 
  #"init-arguments", 
  #"init-data", 
  #"init-evaluated?", 
  #"init-function", 
  #"init-keyword", 
  #"init-keyword-required?", 
  #"init-supplied?", 
  #"init-value", 
  #"init-value?", 
  #"initial-count", 
  #"instance-storage-size", 
  #"key", 
  #"key-test", 
  #"key-types", 
  #"key?", 
  #"keys", 
  #"keyword-specifiers", 
  #"keywords", 
  #"lock", 
  #"low", 
  #"max", 
  #"maximum-count", 
  #"min", 
  #"mode", 
  #"name", 
  #"next", 
  #"next?", 
  #"number-patterns", 
  #"number-required", 
  #"number-values", 
  #"object", 
  #"operation", 
  #"ordered", 
  #"owner", 
  #"password", 
  #"primary?", 
  #"priority", 
  #"processing", 
  #"properties", 
  #"read", 
  #"rehash-limit", 
  #"repeated", 
  #"required", 
  #"required-init-keyword", 
  #"rest-value", 
  #"rest-value?", 
  #"rest?", 
  #"sealed?", 
  #"sequences", 
  #"setter", 
  #"signature", 
  #"size", 
  #"skip", 
  #"slot-descriptor", 
  #"slots", 
  #"stable", 
  #"start", 
  #"storage-size", 
  #"superclasses", 
  #"synchronization", 
  #"test", 
  #"test-function", 
  #"thread", 
  #"timeout", 
  #"to", 
  #"token", 
  #"type", 
  #"type1", 
  #"type2", 
  #"unknown", 
  #"users", 
  #"value", 
  #"values", 
  #"values?", 
  #"vector", 
  #"virtual", 
  #"weak", 
  #"write"
end shared-symbols;

define shared-symbols %shared-streams-symbols
  #"element-type",
  #"direction",
  #"locator",
  #"encoding",
  #"outer-stream",
  #"if-exists",
  #"if-does-not-exist",
  #"abort?",
  #"wait?",
  #"input",
  #"output",
  #"input-output",
  #"new-version",
  #"overwrite",
  #"replace",
  #"truncate",
  #"signal",
  #"append"
end shared-symbols;

/// CPL ABSTRACTION

define method all-superclasses 
    (iclass :: <implementation-class>) => (supers :: <list>)
  let supers :: <list> = #();
  for (super :: <class> in class-rcpl-vector(iclass), 
       i :: <integer> from class-rcpl-position(iclass) to 0 by -1)
    supers := pair(super, supers);
  end;
  supers
end method;

define method all-superclasses-setter
    (supers :: <list>, iclass :: <implementation-class>) => (supers :: <list>)
  supers
end method;

define macro for-each-superclass
  { for-each-superclass (?:name :: ?:expression of ?class:expression, ?more:*)
      ?loopbody:*
    end }
    => { let super-vector :: <simple-object-vector> 
           = class-rcpl-vector(?class);
         for (super-i :: <integer> from class-rcpl-position(?class) to 0 by -1,
              ?more)
           let ?name :: <class> = super-vector[super-i];
           ?loopbody
         end }
end macro;

/// HACK BEGIN: NEEDED FOR GROUNDING OUT PARTIAL DISPATCH

define constant $empty-subjunctive-class-universe :: <subjunctive-class-universe>
  = make-empty-subjunctive-class-universe();

/// HACK END:
