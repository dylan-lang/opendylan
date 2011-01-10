Module: orb-core
Author: Clive Tong, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ---*** link into registry via settings?

define constant $contexts = make(<string-table>);

define abstract class <context> (corba/<context>)
  constant slot corba/context/context-name :: corba/<Identifier>, required-init-keyword: name:;
  constant slot context-values :: <string-table> = make(<string-table>);
end class;

define sealed domain make (subclass(<context>));
define sealed domain initialize (<context>);

define class <root-context> (<context>)
end class;

define class <child-context> (<context>)
  constant slot corba/context/parent :: corba/<context>, required-init-keyword: parent:;
end class;

define sideways method make (class == corba/<context>, #rest initargs, #key)
 => (context :: <child-context>)
  apply(make, <child-context>, initargs);
end method;

define method initialize (object :: <context>, #key)
  next-method();
  let name = corba/context/context-name(object);
  if (element($contexts, name, default: #f))
    error(make(<duplicate-context-name>, name: name));
  end if;
  element($contexts, name) := object;
end method;

define method find-context (name :: <string>)
  element($contexts, name)
end method;

define method corba/context/parent (object :: <root-context>)
 => (no-parent-of-root == #f)
  #f
end method;

define method corba/context/create-child (object :: <context>, name :: corba/<identifier>)
 => (child :: <context>)
  make(<child-context>,
       parent: object,
       name: name);
end method;

/// ---*** perhaps allow <object> here and coerce to <any>?
define method corba/context/set-one-value
    (object :: <context>, propname :: corba/<identifier>, propvalue :: corba/<any>)
 => ()
  element(context-values(object), propname) := propvalue;
end method;

define method corba/context/set-values
    (object :: <context>, values :: corba/<NVList>)
 => ()
  for (item in values)
    element(context-values(object), corba/NamedValue/name(item)) := corba/namedvalue/argument(item);
  end for;
end method;

define method corba/context/get-values
    (object :: <context>,
     start-scope :: corba/<Identifier>,
     op-flags :: corba/<Flags>,
     pattern :: corba/<Identifier>)
 => (value :: corba/<NVList>)
  let restrict-scope? = (logand(op-flags, corba/$ctx-restrict-scope) ~= 0);
  let exact-matches? = ~(pattern[size(pattern) - 1] = '*');
  let context = if (start-scope ~= "")
		  find-context(start-scope)
		else
		  object
		end if;
  let pattern = if (exact-matches?)
		  pattern
		else
		  copy-sequence(pattern, end: (size(pattern) - 1))
		end if;
  let values = make(corba/<NVList>);
  let done = make(<string-table>);
  block (return)
    local method get-exact-values-in-context ()
	    let value = element(context-values(context), pattern, default: #f);
	    if (value)
	      values := add!(values, make(corba/<NamedValue>,
					  name: pattern,
					  argument: value,
					  len: 0,
					  arg-modes: 0));
	      return();
	    end if;
	  end method;
    local method get-matching-values-in-context ()
	    for (value keyed-by key in context-values(context))
	      if (subsequence-position(key, pattern))
		unless (element(done, key, default: #f))
		  element(done, key) := #t;
		  values := add!(values, make(corba/<NamedValue>,
					      name: key,
					      argument: value,
					      len: 0,
					      arg-modes: 0));
		end unless;
	      end if;
	    end for;
	  end method;
    if (restrict-scope?)
      if (exact-matches?)
	get-exact-values-in-context();
      else
	get-matching-values-in-context();
      end if;
    else
      if (exact-matches?)
	while (context)
	  get-exact-values-in-context();
	  context := corba/context/parent(context);
	end while;
      else
	while (context)
	  get-matching-values-in-context();
	  context := corba/context/parent(context);
	end while;
      end if;
    end if;
  end block;
  if (empty?(values))
    error(make(<no-matching-context-values>, pattern: pattern));
  end if;
  values;
end method;

// ---*** must deal with trailing wildcard
// ---*** must signal an error if no property found
define method corba/context/delete-values
    (object :: <context>, propname :: corba/<identifier>)
 => ()
  remove-key!(context-values(object), propname);
end method;

define constant $default-context = make(<root-context>, name: "Default Context");

corba/context/set-one-value($default-context,
			    "Orb Type",
			    make(corba/<any>,
				 type: corba/$string-typecode,
				 value: "Functional Developer ORB"));

define method corba/orb/get-default-context (orb :: <orb>)
 => (ctx :: <context>)
  $default-context
end method;

