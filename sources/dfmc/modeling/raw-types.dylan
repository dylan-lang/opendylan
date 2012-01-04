module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TODO: SHOULD BE SPLIT OUT TO ALLOW C-FUNCTION TO INHERIT SELECTIVELY

define virtual-compiler-model-class <primitive> (<callable-object>, <object>) (<object>)
  constant slot primitive-descriptor-getter-name :: <symbol>,   
    init-keyword: getter-name:;
  constant slot primitive-signature :: <&signature>,   
    init-keyword: signature:;
  constant slot primitive-value = #f,
    init-keyword: value:;
  slot primitive-properties :: <integer> = 0;
end;

define leaf packed-slots primitive-properties (<&primitive>, <object>)
  boolean slot primitive-side-effecting?,      init-keyword: side-effecting?:;
  boolean slot primitive-stateless?,           init-keyword: stateless?:;
  boolean slot primitive-dynamic-extent?,      init-keyword: dynamic-extent?:;
end packed-slots;

define method initialize
    (x :: <&primitive>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, x, all-keys)
end method;

define function raw-type-descriptor (raw-type :: <&raw-type>)
  raw-type-descriptor-function(raw-type)(current-back-end())
end;

// modified define &raw-type
define class <&raw-object> (<&top>)
  constant slot ^raw-object-value, required-init-keyword: value:;
end class;

define method raw-object(back-end)
  error("raw-object has no raw-type-descriptor");
end method;

do-define-raw-type(#"<raw-object>", #f, raw-object);

define method ^subtype? (t1 :: <&class>, t2 :: <&raw-type>) 
 => (subtype? :: <boolean>)
  #f
end method;

define method ^subtype? (t1 :: <&raw-type>, t2 :: <&class>) 
 => (subtype? :: <boolean>)
  #f
end method;

define method ^subtype? (t1 :: <&raw-type>, t2 :: <&raw-type>)
 => (subtype? :: <boolean>)
  t1 == t2 |
  begin
    let t1-supertype = ^raw-type-supertype(t1);
    t1-supertype &
      ^subtype?(t1-supertype, t2)
  end
end;

define method ^known-disjoint?
    (t1 :: <&class>, t2 :: <&raw-type>) => (disjoint? :: <boolean>)
  #t
end method;

define method ^known-disjoint?
    (t1 :: <&raw-type>, t2 :: <&class>) => (disjoint? :: <boolean>)
  #t
end method;



// is this needed??
define method ^known-disjoint?
    (t1 :: <&class>, t2 :: <&raw-aggregate-type>) => (disjoint? :: <boolean>)
  #t
end method;

define method ^known-disjoint?
    (t1 :: <&raw-aggregate-type>, t2 :: <&class>) => (disjoint? :: <boolean>)
  #t
end method;

define method raw-type? (object :: <&raw-aggregate-type>) #f end;


define method raw-type? (object) #f end;
define method raw-type? (object :: <&raw-type>) #t end;

define method direct-object? (object :: <&raw-object>) #t end;

define compiler-open generic raw-type-size (raw-type) => (res);
define compiler-open generic raw-type-alignment (raw-type) => (res);
define compiler-open generic raw-type-getter (raw-type) => (res);
define compiler-open generic raw-type-setter (raw-type) => (res);
define compiler-open generic raw-type-boxed-class (raw-type) => (res);
define compiler-open generic raw-type-boxer (raw-type) => (res);
define compiler-open generic raw-type-unboxer (raw-type) => (res);
define compiler-open generic raw-type-getter-name (raw-type) => (res);
define compiler-open generic raw-type-setter-name (raw-type) => (res);
define compiler-open generic raw-type-boxed-class-name (raw-type) => (res);
define compiler-open generic raw-type-boxer-name (raw-type) => (res);
define compiler-open generic raw-type-unboxer-name (raw-type) => (res);
define compiler-open generic raw-type-c-name (raw-type) => (res);

define property-delegation-getters (<&raw-type>, raw-type-descriptor)
  raw-type-size, raw-type-alignment, 
  raw-type-getter, raw-type-setter, 
  raw-type-boxed-class, raw-type-boxer, raw-type-unboxer,
  raw-type-getter-name, raw-type-setter-name, 
  raw-type-boxed-class-name, raw-type-boxer-name, raw-type-unboxer-name,
  raw-type-c-name
end property-delegation-getters;


define &dylan-raw-types

  export
    machine-word,
    single-float,
    double-float,
    double-integer,
    extended-float;

end &dylan-raw-types;


define &raw-types-and-accessors

  export
    c-float,
    c-double,
    c-long-double;

end &raw-types-and-accessors;


define &raw-machine-word-subtypes-and-accessors

  export

    /*
    // concrete raw types

    8-bit-integer,
    16-bit-integer,
    32-bit-integer,
    64-bit-integer,
    ieee-single-float,
    ieee-double-float,
    // ieee-extended-float,
    */

    // c raw types

    c-signed-char,
    c-unsigned-char,
    // c-char,
    c-signed-short,
    c-unsigned-short,
    c-signed-int,
    c-unsigned-int,
    c-signed-long,
    c-unsigned-long,
    c-signed-long-long,
    c-unsigned-long-long,
    c-pointer;

end;

define &dylan-raw-machine-word-subtypes

  export

    // dylan raw types

    boolean,
    byte-character, 
    unicode-character,
    byte,
    double-byte,
    integer,
    pointer,
    address,
    byte-string;

end;

define &raw-machine-word-subtypes

  export

    // c raw types

    c-void;

end;

define abstract class <raw-aggregate-member> (<object>)
  constant slot member-raw-type, required-init-keyword: raw-type:;
end;

define class <raw-aggregate-ordinary-member> (<raw-aggregate-member>)
end;

define generic member-bitfield-width (member :: <raw-aggregate-member>)
 => (w :: <integer>);

define method member-bitfield-width (member :: <raw-aggregate-member>)
 => (w :: <integer>);
  0
end;

define compiler-open generic compute-raw-aggregate-member (member-list)
 => (member :: <raw-aggregate-member>);

define class <raw-struct-bitfield-member> (<raw-aggregate-member>)
  constant slot member-bitfield-width :: <integer>,
    required-init-keyword: bitfield-width:;
end;

define class <raw-aggregate-array-member> (<raw-aggregate-member>)
  constant slot member-array-length :: <integer>, 
    required-init-keyword: array-length:;
end;

define method raw-type-size (raw-type :: <&raw-aggregate-type>)
 => (sz :: <integer>)
  compute-raw-aggregate-layout(raw-type)
end;

define method raw-type-alignment (raw-type :: <&raw-aggregate-type>)
 => (sz :: <integer>)
  let (size, alignment, member-bit-offsets)
    = compute-raw-aggregate-layout(raw-type);
  alignment
end;

define method compute-raw-aggregate-layout
    (raw-type :: <&raw-struct-type>)
 => (size :: <integer>,
     alignment :: <integer>,
     member-bit-offsets :: <object-table>);
  let running-size = 0;
  let running-bitfield-offset = 0;
  let running-alignment = 1;
  let pack-option
    = get-property(raw-type.raw-aggregate-options, #"pack",
                   default: get-default-pack-option());
  let force-new-member? = #t;
  let previous-member-size = 0;
  let member-bit-offsets = make(<object-table>);
  for (member in raw-type.raw-aggregate-members)
    member-bit-offsets[member] := running-size;
    let member-type = member.member-raw-type;
    let member-size = compute-member-size(member);
    let member-width = member-bitfield-width(member);
    let member-align = raw-type-alignment(member-type);
    let adjusted-alignment = min(member-align, pack-option);
    let next-bitfield-offset = running-bitfield-offset + member-width;
    if (member-width = 0)  // not a bitfield
      running-size := round-up-to-mod(running-size, adjusted-alignment)
	                + member-size;
      running-bitfield-offset := 0;
      next-bitfield-offset := 0;
      force-new-member? := #t;
    elseif (force-new-member?
	    | member-size ~= previous-member-size  // change in type size
	    // this field would spill over into the next member
	    | (next-bitfield-offset > member-size * address-unit-bit-size()))
      running-size := round-up-to-mod(running-size, adjusted-alignment)
	                + member-size;
      running-bitfield-offset := 0;
      next-bitfield-offset := member-width;
      force-new-member? := #f;
    end if;
    running-alignment := max(running-alignment, adjusted-alignment);
    running-bitfield-offset := next-bitfield-offset;
    previous-member-size := member-size;
  end;
  values(round-up-to-mod(running-size, running-alignment),
         running-alignment,
         member-bit-offsets)
end;

// unions max the member sizes, but they have no bitfield so it's simpler
define method compute-raw-aggregate-layout
    (raw-type :: <&raw-union-type>)
 => (size :: <integer>,
     alignment :: <integer>,
     member-bit-offsets :: <object-table>);
  let running-size = 0;
  let running-alignment = 1;
  let pack-option
    = get-property(raw-type.raw-aggregate-options, #"pack",
                   default: get-default-pack-option());
  let member-bit-offsets = make(<object-table>);
  for (member in raw-type.raw-aggregate-members)
    member-bit-offsets[member] := 0;
    let member-type = member.member-raw-type;
    let member-size = compute-member-size(member);
    let member-align = raw-type-alignment(member-type);
    let adjusted-alignment = min(member-align, pack-option);
    running-size := max(running-size, member-size);
    running-alignment := max(running-alignment, adjusted-alignment);
  end;
  values(round-up-to-mod(running-size, running-alignment),
         running-alignment,
         member-bit-offsets)
end;

/// The number returned determines the default packing for structs.
/// No struct element is required to be aligned on a boundary larger
/// than this.  Each raw type has its own alignement requirements,
/// but this overrides that when packing struct members.
/// this might better be done as a method on the current back end.
define function get-default-pack-option () => (x :: <integer>)
  if (current-processor-name() == #"x86" & current-os-name() == #"win32")
    8
  else
    // anything harmlessly large
    32
  end;
  // might need to handle different os's
end;


/// This is correct for all byte addressable machine with 8 bit bytes
/// this also should also be a function of the back end, but it may be
/// a very long time before it's something other than 8, if ever.
define function address-unit-bit-size ()
  8
end;




define method compute-member-size
    (member :: <raw-aggregate-member>)
 => (size :: <integer>);
  raw-type-size(member.member-raw-type);
end;

define method compute-member-size (member :: <raw-aggregate-array-member>)
  member.member-array-length
    * raw-type-size(member.member-raw-type);
end;


// useful
define method round-up-to-mod (int :: <integer>, modulus :: <integer>)
 => (i :: <integer>);
  modulus * ceiling/(int, modulus);
end;


/// some utilities

define method make-raw-literal (object :: <boolean>)
  object
end method;

define method make-raw-literal (object :: <character>)
  ^make(<&raw-byte-character>, value: object)
end method;

define method make-raw-literal (object :: <byte-string>)
  ^make(<&raw-byte-string>, value: object)
end method;

define method make-raw-literal (object :: <integer>)
  ^make(<&raw-integer>, value: object)
end method;

define method make-raw-literal (object :: <single-float>)
  ^make(<&raw-single-float>, value: object)
end method;

define method make-raw-literal (object :: <double-float>)
  ^make(<&raw-double-float>, value: object)
end method;

/// NOTE: <double-integer> is currently only used as an intermediate
/// value when folding <machine-word> expressions.
define method make-raw-literal (object :: <double-integer>)
  ^make(<&raw-machine-word>, value: generic/logand(object, target-machine-word-mask()))
end method;

define method make-raw-literal (object :: <machine-word>)
  ^make(<&raw-machine-word>, value: object)
end method;

////
//// REPEATED SLOT REPRESENTATION TYPES
////

define inline method repeated-representation-byte? 
     (type :: <&type>) => (res :: <boolean>)
  repeated-representation-size(type) = 1
end method;

define method repeated-representation-size
    (type :: <&type>) => (res :: <integer>)
  word-size()
end method;

define method repeated-representation-size
    (type :: <&singleton>) => (res :: <integer>)
  repeated-representation-size(^object-class(^singleton-object(type)))
end method;

define method repeated-representation-size 
    (type :: <&class>) => (res :: <integer>)
  select (type)
    dylan-value(#"<byte-character>")    => 1;
    dylan-value(#"<unicode-character>") => word-size();
    dylan-value(#"<single-float>")      => 4;
    dylan-value(#"<double-float>")      => 8;
    otherwise                           => word-size();
  end select;
end method;

define inline function in-range? 
    (x :: <integer>, min :: <integer>, max :: <integer>) => (res :: <boolean>)
  x >= min & x <= max
end function;

define inline function subrange? 
    (min :: <integer>, max :: <integer>, 
     in-min :: <integer>, in-max :: <integer>)
 => (res :: <boolean>)
  in-range?(min, in-min, in-max) & in-range?(max, in-min, in-max)
end function;

define inline function fits-in-bits?
    (min :: <integer>, max :: <integer>, n :: <integer>) => (res :: <boolean>)
  let unsigned-min = 0;
  let unsigned-max = 2 ^ n - 1;
  let signed-min   = - (2 ^ (n - 1));
  let signed-max   = 2 ^ (n - 1) - 1;
  subrange?(min, max, unsigned-min, unsigned-max) 
    | subrange?(min, max, signed-min, signed-max)
end function;

define method repeated-representation-size 
     (type :: <&limited-integer>) => (res :: <integer>)
  let min = ^limited-integer-min(type);
  let max = ^limited-integer-max(type);
  if (min & max)
    case
      fits-in-bits?(min, max, 8)  => 1;
      fits-in-bits?(min, max, 16) => 2;
      otherwise                   => word-size();
    end case;
  else
    word-size()
  end if
end method;

define constant <boxer> = false-or(type-union(<&primitive>, <&method>));

define method raw-repeated-representation? 
    (type :: <&type>)
 => (well? :: <boolean>, 
     boxer :: <boxer>, unboxer :: <boxer>,
     raw-type :: false-or(<&raw-type>))
  values(#f, #f, #f, #f)
end method;

define method raw-repeated-representation? 
    (type :: <&limited-integer>)
 => (well? :: <boolean>, 
     boxer :: <boxer>, unboxer :: <boxer>,
     raw-type :: false-or(<&raw-type>))
  let size = repeated-representation-size(type);
  let raw-type-name :: false-or(<symbol>)
    = select (size)
	1         => #"<raw-byte>";
	2         => #"<raw-double-byte>";
	otherwise => #f;
      end;
  if (raw-type-name)
    let raw-type = dylan-value(raw-type-name);
    values(#t, 
           raw-type-boxer(raw-type), 
           raw-type-unboxer(raw-type), 
           raw-type)
  else
    values(#f, #f, #f, #f)
  end if
end method;

define constant $raw-repeated-class-names
  = #[#"<single-float>", #"<double-float>", 
      #"<machine-word>", 
      #"<byte-character>", #"<unicode-character>"];

define method raw-repeated-representation? 
    (type :: <&singleton>)
 => (well? :: <boolean>, 
     boxer :: <boxer>, unboxer :: <boxer>,
     raw-type :: false-or(<&raw-type>))
  raw-repeated-representation?(^object-class(^singleton-object(type)))
end method;

define method raw-repeated-representation? 
    (type :: <&class>)
 => (well? :: <boolean>, 
     boxer :: <boxer>, unboxer :: <boxer>,
     raw-type :: false-or(<&raw-type>))
  block (return)
    for (class-name in $raw-repeated-class-names)
      let class = dylan-value(class-name);
      if (type == class)
        let raw-type = dylan-value(raw-representation-name(class-name));
        return(#t, 
               raw-type-boxer(raw-type), 
               raw-type-unboxer(raw-type), 
               raw-type)
      end if;
    end for;
    values(#f, #f, #f, #f)
  end block;
end method;

define sealed method repeated-representation 
    (type :: <&type>) => (type :: <&type>)
  let (raw?, boxer, unboxer, raw-type)
    = raw-repeated-representation?(type);
  if (raw?)
    raw-type
  else
    type
  end if;
end method;

define sealed method cell-representation 
    (type :: <&type>) => (type :: <&type>)
  repeated-representation(type)
end method;
