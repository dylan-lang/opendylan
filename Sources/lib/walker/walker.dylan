module:    walker
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro dont-walk-slots-definer
  { define dont-walk-slots ?class:name using ?walker:name = { ?entries } }
    => { define method walker-shallow-getters 
	     (walker :: ?walker, x :: ?class) => (res :: <sequence>)
           concatenate(?=next-method(), list(?entries))
	 end method }
entries:
  { } => { }
  { ?getter:name, ... } => { ?getter, ... }
  { ?getter:name => ?default:expression, ... } => { pair(?getter, method () ?default end), ... }
end macro;

define macro dont-walk-object-definer 
  { define dont-walk-object ?:name ?walker }
    => { define method deep-walk
             (walker :: ?walker, function :: <function>, 
              parent, object :: ?name)
	   object
	 end method }
walker:
  { }              => { <walker> }
  { using ?:name } => { ?name }
end macro;

define constant $dummy-walker-table = make(<table>);

define open primary class <walker> (<object>)
  slot walker-walked :: <table>  = $dummy-walker-table;
  constant slot walker-classes :: <table> = make(<table>);
end class;

define constant $default-walker-table-size = 128;

define method initialize
    (x :: <walker>, #rest all-keys, 
     #key capacity = $default-walker-table-size, #all-keys) 
  next-method();
  walker-walked(x) := make(<table>, size: capacity);
end method;

define dont-walk-object <empty-list>   using <walker>;
define dont-walk-object <number>       using <walker>;
define dont-walk-object <boolean>      using <walker>;
define dont-walk-object <symbol>       using <walker>;
define dont-walk-object <character>    using <walker>;
define dont-walk-object <function>     using <walker>;
define dont-walk-object <class>        using <walker>;
define dont-walk-object <condition>    using <walker>;
define dont-walk-object <machine-word> using <walker>;

define method walker-repeated-slot? (class)
  #f
end method;

define method walker-slot-descriptors
    (class :: <class>) => (slotds :: <slot-sequence-type>)
  let slotds :: <slot-sequence-type>
    = as(<slot-sequence-type>, slot-descriptors(class));
  choose(method (sd :: <slot-descriptor>) slot-allocation(sd) ~= #"virtual" end, slotds);
end method;

define inline function walker-reset (walker :: <walker>, #key capacity)
  ignore(capacity);
  remove-all-keys!(walker-walked(walker));
  walker
end function;

define inline function walker-register-walked
    (walker :: <walker>, parent, object, value)
  walker-walked(walker)[object] := parent
end function;

define inline function walked (walker :: <walker>, object, #key default)
  element(walker-walked(walker), object, default: default)
end function;

define method do-deep-walk
    (walker :: <walker>, function :: <function>, parent, object)
  let class = object-class(object);
  walker-register-walked(walker, parent, object, object); 
  function(object);
  for (slotd in walker-shallow-slot-descriptors(walker, class))
    function(walker-slot-value(object, slotd))
  end for;
  for (slotd/default in walker-defaulted-slot-descriptors(walker, class))
    let slotd         = walker-default-slot-descriptor(slotd/default);
    let default-thunk = walker-default-thunk(slotd/default);
    function(default-thunk());
  end for;
  for (slotd in walker-deep-slot-descriptors(walker, class))
    deep-walk(walker, function, object, walker-slot-value(object, slotd));
  end for;
  if (walker-repeated-slot?(class))
    for (i from 0 below size(object))
      deep-walk(walker, function, object, object[i]);
    end for;
  end if;
end method;

define method do-deep-walk
    (walker :: <walker>, function :: <function>, parent, object :: <pair>)
  walker-register-walked(walker, parent, object, object); 
  function(object);
  deep-walk(walker, function, object, head(object));
  deep-walk(walker, function, object, tail(object));
end method;

define inline function maybe-do-deep-walk
    (walker :: <walker>, function :: <function>, parent, object)
  unless (walked(walker, object))
    do-deep-walk(walker, function, parent, object)
  end unless;
end function;

define open generic deep-walk
  (walker :: <walker>, function :: <function>, parent, object);

define method deep-walk
    (walker :: <walker>, function :: <function>, parent, object)
  maybe-do-deep-walk(walker, function, parent, object)
end method;

/*
define function walker-error (walker, object, slot-descriptor, condition)
  #f
end function;
*/

// eof
