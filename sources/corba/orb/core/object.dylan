Module: orb-core
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open abstract class <object-reference> (corba/<object>)
  slot object-reference-non-existent? :: <boolean> = #f;
  slot object-reference-ior :: false-or(iop/<ior>) = #f, init-keyword: ior:;
  slot object-reference-forwarded-ior :: false-or(iop/<ior>) = #f;
  slot object-reference-collocated-cache = #f;
  slot object-reference-collocated-ticket :: <boolean-ticket> = invalid-ticket(<boolean-ticket>);
end class;

// NB compare original IORs because forwarded ones will be different
// from original and it can be useful to compare original and new e.g.
// in test suites. Also there may be rubbish left in padding bytes of
// encapsulated profile so we fall back to comparing the unencapsulated
// profiles in the cache.
define method \= (object1 :: <object-reference>, object2 :: <object-reference>)
  => (equal? :: <boolean>)
  let ior1 = object-reference-ior(object1);
  let ior2 = object-reference-ior(object2);
  (ior1 = ior2)
  | (get-iiop-profile(ior1) = get-iiop-profile(ior2))
end method;  

define method initialize (object :: <object-reference>, #key ior-file, ior-string, #all-keys)
  next-method();
  when (ior-file)
    object-reference-ior(object) := parse-ior-from-file(ior-file);
  end when;
  when (ior-string)
    object-reference-ior(object) := parse-ior-from-string(ior-string);
  end when;
end method;

define class <unspecific-object-reference> (<object-reference>)
end class;

define sealed domain make (subclass(<unspecific-object-reference>));
define sealed domain initialize (<unspecific-object-reference>);

define sideways method make (class == corba/<object>, #rest initargs, #key)
 => (object :: <unspecific-object-reference>)
  apply(make, <unspecific-object-reference>, initargs);
end method;

define constant $nil-ior =
  make(iop/<ior>,
       type-id: "",
       profiles: make(limited(corba/<sequence>, of: iop/<taggedprofile>)));

define sideways method make-nil (class :: <class>)
 => (object :: <object-reference>)
  make(class, ior: $nil-ior);
end method;

define method corba/object/is-nil (object :: <object-reference>)
 => (is-nil? :: <boolean>)
  corba/object/ior(object) = $nil-ior
end method;

define method corba/object/duplicate (object :: <object-reference>)
 => (object :: <object-reference>)
  object
end method;

define method corba/object/release (object :: <object-reference>)
 => ()
  values()
end method;

define method corba/object/non-existent (object :: <object-reference>) 
 => (non-existent? :: <boolean>)
  object-reference-non-existent?(object)
    | block ()
	corba/object/-non-existent(object);
      exception (corba/<object-not-exist>)
	object-reference-non-existent?(object) := #t;
      end block;
end method;

define method corba/object/ior (object :: <object-reference>, #key direct? :: <boolean> = #f)
 => (ior :: iop/<ior>)
  (~direct? & object-reference-forwarded-ior(object))
    | object-reference-ior(object)
    | error(make(<missing-ior-error>, object: object))
end method;

define method corba/object/ior-setter (ior, object :: <object-reference>)
 => (ior :: iop/<ior>)
  object-reference-collocated-ticket(object) := invalid-ticket(<boolean-ticket>);
  object-reference-forwarded-ior(object) := ior
end method;

/*
---*** redo integer-length (and move into utils)

count-high-zeros (m :: <machine-word>) => _ :: <integer>          [Function]
    Returns the number of consecutive zero bits in m counting from the most
    significant bit.

      [Note: The position of the most significant non-zero bit in m can be
      computed by subtracting this result from $machine-word-size - 1.
      So if i is the result and p = ($machine-word-size - i - 1), then
      logbit?(p, m) is true and for all values of j such that
      p < j < $machine-word-size, logbit?(j, m) is false.]
*/

define method integer-length (n :: <integer>)
  block (return)
    for (bit from 32 to 0)
      if (logbit?(bit, n))
	return(bit)
      end if;
    end for;
    return(0);
  end block;
end method;

define variable $orb-hash-state = make(<hash-state>);

define method corba/object/hash (object :: <object-reference>, max :: <integer>)
 => (hash :: corba/<unsigned-long>)
  let (hash-id, hash-state) = object-hash(iop/ior/type-id(object-reference-ior(object)), $orb-hash-state);
  if (hash-id > max)
    logand(ash(1, integer-length(max)) - 1, hash-id);
  else
    hash-id
  end if;
end method;

define method corba/object/is-equivalent (object :: <object-reference>, other :: <object-reference>)
 => (equivalent? :: <boolean>)
  object = other
end method;

define method corba/object/is-a (object :: <object-reference>, desired-repository-id :: corba/<string>)
 => (is-a? :: <boolean>)
  corba/object/is-nil(object) // NB a nil can be coerced to anything
  | begin
      let desired-typecode = as(<typecode>, desired-repository-id);
      let desired-type = desired-typecode & typecode-native-type(desired-typecode);
      let object-repository-id = iop/ior/type-id(corba/object/ior(object));
      let object-typecode = as(<typecode>, object-repository-id);
      let object-type = object-typecode & typecode-native-type(object-typecode);
      (desired-type & instance?(object, desired-type))
	| (object-type & desired-type & subtype?(object-type, desired-type))
	| corba/object/-is-a(object, desired-repository-id)
    end;
end method;

define method as (class :: subclass(<object-reference>), object :: <object-reference>)
 => (object :: <object-reference>)
  if (object-class(object) = class)
    object
  else
    if (corba/object/is-nil(object))
      make-nil(class)
    else
      let typecode = class-typecode(class);
      let type-id = typecode-repository-id(typecode);
      unless (corba/object/is-a(object, type-id))
	error(make(<coerce-object-error>, class: class, object: object, type-id: type-id));
      end unless;
      let ior = object-reference-ior(object);
      let new = shallow-copy(ior);
      iop/ior/type-id(new) := type-id;
      make(class, ior: new);
    end if;
  end if;
end method;

define method as (class :: subclass(<object-reference>), any :: corba/<any>)
  => (object :: <object-reference>)
  let typecode = corba/any/type(any);
  unless (instance?(typecode, <object-reference-typecode>))
    error(make(<coerce-any-object-error>, any: any, class: class));
  end unless;
  as(class, corba/any/value(any));
end method;

define constant $corba-object-name = "corba-object";
define constant $corba-object-interface-repository-id = concatenate("LOCAL:", $corba-object-name, ":1.0");

define constant $object-reference-typecode = 
  make(<object-reference-typecode>,
       repository-id: $corba-object-interface-repository-id,
       name: $corba-object-name);

define method object-typecode (object :: <object-reference>)
 => (typecode :: <typecode>)
 $object-reference-typecode
end method;

define method class-typecode (class == <object-reference>)
 => (typecode :: <typecode>)
 $object-reference-typecode
end method;

define sideways method class-typecode (class == corba/<object>)
 => (typecode :: <typecode>)
  class-typecode(<object-reference>)
end method;

/*
define method print-object (object :: <object-reference>, stream :: <stream>)
 => ()
  next-method();
  let the-ior = object-reference-ior(object);
  when (the-ior)
    format(stream, "%s ", iop/ior/type-id(the-ior));
    let profile = get-iiop-profile(the-ior);
    when (profile)
      format(stream, "%s %d ",
	     iiop/ProfileBody-1-0/host(profile),
	     iiop/ProfileBody-1-0/port(profile));
      let key =  iiop/ProfileBody-1-0/object-key(profile);
      if (every?(method (x) 46 <= x <= 122 end method, key))
	print(as(<string>, key), stream);
      else
	if (size(key) < 12)
	  for (i from 0 below size(key))
	    format(stream, "%x", key[i]);
	  end for;
	else
	  for (i from 0 below 6)
	    format(stream, "%x", key[i]);
	  end for;
	  print("...", stream);
	  for (i from size(key) - 6 to size(key))
	    format(stream, "%x", key[i]);
	  end for;
	end if;
      end if;
    end; 
  end;
end method;
*/

/// Manual stubs for built-in object operations

define open generic corba/object/-is-a (object :: <object>, logical-type-id :: corba/<string>)
 => (is-a? :: <boolean>);

define method corba/object/-is-a (object :: <object-reference>, logical-type-id :: corba/<string>)
 => (is-a? :: <boolean>)
  let context = corba/orb/get-default-context(corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB"));
  let (result, request) = corba/object/create-request(object,
						      context,
						      "_is_a",
						      make(corba/<nvlist>),
						      make(corba/<namedvalue>,
							   name: "result",
							   argument: make(corba/<any>,
									  type: corba/$boolean-typecode),
							   len: 0,
							   arg-modes: 0),
						      0);
  corba/request/add-arg(request, "value", corba/$string-typecode, logical-type-id, 0, corba/$arg-in);
  dynamic-bind (*optimize-collocation?* = #t) // force this here even if off elsewhere
    corba/request/invoke(request, 0);
  end;
  as(corba/<boolean>, corba/namedvalue/argument(result));
end method;

define method corba/object/-non-existent (object :: <object-reference>)
  => (result :: corba/<boolean>)
  let context = corba/orb/get-default-context(corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB"));
  let (result, request) = corba/object/create-request(object,
						      context,
						      "_non_existent",
						      make(corba/<nvlist>),
						      make(corba/<namedvalue>,
							   name: "result",
							   argument: make(corba/<any>,
									  type: corba/$boolean-typecode),
							   len: 0,
							   arg-modes: 0),
						      0);
  dynamic-bind (*optimize-collocation?* = #t) // force this here even if off elsewhere
    corba/request/invoke(request, 0);
  end;
  as(corba/<boolean>, corba/namedvalue/argument(result));
end method;

