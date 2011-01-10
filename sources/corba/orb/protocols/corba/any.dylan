Module: corba-protocol
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ANY

define class corba/<any> (<object>)
  virtual slot corba/any/type :: corba/<typecode>, init-keyword: type:;
  virtual slot corba/any/value :: <object>, init-keyword: value:;
  slot corba/any/%type :: corba/<typecode>;
  slot corba/any/%value :: <object>;
end class;

define sealed domain make (subclass(corba/<any>));
define sealed domain initialize (corba/<any>);

define method \= (any1 :: corba/<any>, any2 :: corba/<any>)
  => (equal? :: <boolean>)
  (corba/any/value(any1) = corba/any/value(any2))
    & (corba/any/type(any1) = corba/any/type(any2))
end method;

define method initialize (object :: corba/<any>, #key type, value)
  next-method();
  corba/any/%type(object) := type;
  corba/any/%value(object) := value;
  check-any-consistent(object);
end method;

define method corba/any/type (object :: corba/<any>)
  corba/any/%type(object)
end method;

define method corba/any/type-setter (type, object :: corba/<any>)
  corba/any/%type(object) := type;
  check-any-consistent(object);
end method;

define method corba/any/value (object :: corba/<any>)
  let value = corba/any/%value(object);
  check-any-value(object, value);
end method;

define method corba/any/value-setter (value, object :: corba/<any>)
  corba/any/%value(object) := value;
  check-any-consistent(object);
end method;


define method check-any-consistent (object :: corba/<any>)
  if (any-consistency-testable?(object))
    ensure-any-consistent(object);
    if (~any-consistent?(object))
      error(make(<inconsistent-any>, any: object));
    end if;
  end if;
end method;


define method any-consistency-testable? (object :: corba/<any>)
  slot-initialized?(object, corba/any/%type)
    & slot-initialized?(object, corba/any/%value)
    & corba/any/%type(object)
    & corba/any/%value(object)
end method;


define method any-consistent? (object :: corba/<any>)
  let typecode = corba/any/%type(object);
  let value = corba/any/%value(object);
  any-typecode-consistent?(typecode, value);
end method;


define method any-typecode-consistent? (typecode :: corba/<typecode>, value)
  any-native-type-consistent?(typecode-native-type(typecode), value)
end method;

define method any-typecode-consistent? (typecode :: corba/<typecode>, value :: corba/<object>)
  corba/object/is-a(value, typecode-repository-id(typecode))
end method;


define method any-native-type-consistent? (type, value)
  #t
end method;

define method any-native-type-consistent? (type :: <type>, value)
  instance?(value, type)
end method;

define method any-native-type-consistent? (type :: <type>, value :: <anonymous-object>)
  #t // ---*** could count slots and check types within them
end method;


define method ensure-any-consistent (object :: corba/<any>)
  let typecode = corba/any/%type(object);
  let value = corba/any/%value(object);
  let (coerce? :: <boolean>, new-value) = ensure-any-typecode-consistent(typecode, value);
  if (coerce?)
    corba/any/%value(object) := new-value
  end if;
end method;

define open generic ensure-any-typecode-consistent (typecode, value) 
 => (coerce? :: <boolean>, new-value);

define method ensure-any-typecode-consistent (typecode, value) 
 => (coerce? :: <boolean>, new-value)
  ensure-any-native-type-consistent(typecode-native-type(typecode), value) 
end method; 

define open generic ensure-any-native-type-consistent (type, value) 
 => (coerce? :: <boolean>, new-value);

define method ensure-any-native-type-consistent (type, value) 
 => (coerce? :: <boolean>, new-value)
  values(#f, #f)
end method; 

define method check-any-value (any :: corba/<any>, object :: <object>)
  object
end method;

define method check-any-value (any :: corba/<any>, object :: <anonymous-object>)
  block ()
    error(make(<unknown-type-any-coercion-error>, any: any));
  exception (restart :: <unknown-type-any-coercion-restart>)
    as(restart-native-type(restart), any);
  end block;
end method;


/// ANY COERCION

define method as (class == corba/<any>, object :: corba/<any>)
 => (any :: corba/<any>)
  object
end method;

define method as (class == corba/<any>, object :: <object>)
 => (any :: <object>)
  make(corba/<any>, value: object, type: object-typecode(object));
end method;

/// STRUCT/ANY COERCION

define method as (class :: subclass(corba/<struct>), object :: corba/<any>)
 => (struct :: corba/<struct>)
  check-any-as-class(class, object);
  as(class, corba/any/%value(object));
end method;
       
/// STRUCT/ANONYMOUS-OBJECT COERCION

define method as (class :: subclass(corba/<struct>), value :: <anonymous-object>)
 => (struct :: corba/<struct>)
  let typecode = class-typecode(class);
  let initargs = make(<stretchy-vector>);
  for (member in typecode-members(typecode),
       property in anonymous-object-properties(value)) // native-type = typecode-member-native-type(member))
    initargs := add!(initargs, typecode-member-init-keyword(member));
    initargs := add!(initargs, property); // if (native-type) as(native-type, property) else property end if);
  end for;
  let struct = apply(make, class, initargs);
  struct
end method;

/// CHECK-ANY-AS-CLASS

define method check-any-as-class (type :: <type>, object :: corba/<any>)
  let any-value = corba/any/%value(object);
  let any-typecode = corba/any/type(object);
  let any-native-type = typecode-native-type(any-typecode);
  unless ((any-value == #"ignore")
          | (any-typecode == class-typecode(type))
          | (any-native-type & subtype?(any-native-type, type)))
    error(make(<incompatible-type-any-coercion-error>, any: object, class: type));
  end unless;
end method;

/// UNION/ANY COERCION

define method as (class :: subclass(corba/<union>), object :: corba/<any>)
  => (union :: corba/<union>)
  check-any-as-class(class, object);
  as(class, corba/any/%value(object));
end method;

/// UNION/ANONYMOUS-UNION COERCION

define method as (class :: subclass(corba/<union>), object :: <anonymous-union>)
 => (union :: corba/<union>)
  make(class,
       discriminator: corba/union/discriminator(object),
       value: corba/union/value(object))
end method;

/// SEQUENCE AND ARRAY COERCION
///
/// ---*** Workaround the fact that
///     class :: subclass(corba/<sequence>)
/// does not match
///     limited(corba/<sequence>, of: <foo>)
/// by using "subtype?"

define method as (type :: <type>, object :: corba/<any>)
  => (result)
  local method as-aux (type :: <type>, object :: corba/<any>)
          => (result :: type)
          select (type by subtype?)
            corba/<sequence> => as-sequence(type, object);
            corba/<array> => as-array(type, object);
            otherwise => as-default(type, object);
          end select;
        end method;
  as-aux(type, object)
end method;

define method as-default (type :: <type>, object :: corba/<any>)
 => (object :: <object>)
  check-any-as-class(type, object);
  corba/any/value(object);
end method;

/// SEQUENCE/ANY COERCION

define method as-sequence (class :: <type>, object :: corba/<any>)
 => (sequence :: corba/<sequence>)
  let typecode = corba/any/type(object);
  let value = corba/any/value(object);
  let element-typecode = typecode-element-typecode(typecode);
  let native-type = typecode-native-type(element-typecode);
  if (zero?(size(value)))
    make(class)
  else
    let fill = as(native-type, value[0]);
    let seq = make(class, size: size(value), fill: fill);
    for (i :: <integer> from 0 below size(value))
      seq[i] := as(native-type, value[i]);
    end for;
    seq;
  end if;
end method;

/// ARRAY/ANY COERCION

define method as-array (class :: <type>, object :: corba/<any>)
 => (array :: corba/<array>)
  let typecode = corba/any/type(object);
  let value = corba/any/value(object);
  let dims = make(<stretchy-vector>);
  let element-typecode = typecode;
  while (instance?(element-typecode, <array-typecode>))
    dims := add!(dims, typecode-length(element-typecode));
    element-typecode := typecode-element-typecode(element-typecode);
  end while;
  let native-type = typecode-native-type(element-typecode);
  if (every?(curry(\=, 0), dims))
    make(class, dimensions: dims)
  else
    let fill = as(native-type, value[0]); // NB just use first element
    let array = make(class, dimensions: dims, fill: fill);
    for (i :: <integer> from 0 below size(value))
      array[i] := as(native-type, value[i]);
    end for;
    array;
  end if;
end method;


