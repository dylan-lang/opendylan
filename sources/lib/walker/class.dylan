module:    walker
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <walker-slot-descriptor> = <integer>;

define constant <slot-sequence-type>
  = limited(<vector>, of: <slot-descriptor>);

define constant <walker-slot-sequence-type>
  = limited(<vector>, of: <walker-slot-descriptor>);

define inline function walker-slot-value 
    (object, slot-descriptor :: <walker-slot-descriptor>) => (value)
  initialized-slot-element(object, slot-descriptor)
end function;

define inline function walker-slot-value-setter 
    (new-value, object, slot-descriptor :: <walker-slot-descriptor>)
  slot-element(object, slot-descriptor) := new-value;
end function;

define inline function as-walker-slot-descriptor
    (class :: <class>, slot-descriptor :: <slot-descriptor>)
 => (walker-slot-descriptor :: <walker-slot-descriptor>)
  slot-offset(slot-descriptor, class)
end function;

/*
define inline function walker-slot-value (object, slot-descriptor)
  slot-value(object, slot-descriptor)
end function;

define inline function walker-slot-value-setter 
    (new-value, object, slot-descriptor)
  slot-value(object, slot-descriptor) := new-value;
end function;

define inline function as-walker-slot-descriptor
    (class :: <class>, slot-descriptor :: <slot-descriptor>)
  slot-descriptor
end function;
*/

define open generic walker-shallow-getters
  (walker :: <walker>, class :: <class>) => (res :: <sequence>);

define method walker-shallow-getters
    (walker :: <walker>, class :: <class>) => (res :: <list>)
  #()
end method;

define class <walker-defaulted-descriptor> (<object>)
  constant slot walker-default-slot-descriptor :: <walker-slot-descriptor>, 
    required-init-keyword: slot-descriptor:;
  constant slot walker-default-thunk :: <function>, 
    required-init-keyword: thunk:;
end class;

define function make-walker-defaulted-descriptor
    (slot-descriptor :: <walker-slot-descriptor>, thunk :: <function>)
  make(<walker-defaulted-descriptor>,
       slot-descriptor: slot-descriptor,
       thunk:           thunk);
end function;


define constant <walker-defaulted-spec> = <pair>;

define inline function walker-defaulted-getter? (spec)
  instance?(spec, <walker-defaulted-spec>)
end function;

define inline method walker-default-getter (spec :: <walker-defaulted-spec>)
  head(spec)
end method;

define inline method walker-default-thunk (spec :: <walker-defaulted-spec>)
  tail(spec)
end method;


define constant <walker-defaulted-slot-sequence-type> 
  = limited(<vector>, of: <walker-defaulted-descriptor>);

define function walker-defaulted-shallow-getters
    (walker :: <walker>, class :: <class>) => (res :: <list>)
  choose(walker-defaulted-getter?, walker-shallow-getters(walker, class))
end function;

define constant $walker-simple   = #"simple";
define constant $walker-complex  = #"complex";
define constant $walker-repeated = #"repeated";

define class <walker-class> (<object>)
  constant slot walker-class-repeated-slot? :: <boolean>, 
    required-init-keyword: repeated-slot?:;

  constant slot walker-class-deep-slot-descriptors :: <walker-slot-sequence-type>, 
    required-init-keyword: deep-slot-descriptors:;
  constant slot walker-class-shallow-slot-descriptors :: <walker-slot-sequence-type>, 
    required-init-keyword: shallow-slot-descriptors:;
  constant slot walker-class-defaulted-slot-descriptors :: <walker-defaulted-slot-sequence-type>, 
    required-init-keyword: defaulted-slot-descriptors:;

  slot walker-class-kind :: <symbol> = #"default";
end class;

define method initialize (class :: <walker-class>, #key) => ()
  next-method();
  let kind
    = case
	walker-class-repeated-slot?(class) 
	  => $walker-repeated;
	empty?(walker-class-shallow-slot-descriptors(class))
	  & empty?(walker-class-defaulted-slot-descriptors(class)) 
	  => $walker-simple;
	otherwise
	  => $walker-complex;
      end case;
  walker-class-kind(class) := kind;
end method;

define function walker-real-shallow-getters 
     (walker :: <walker>, class :: <class>) => (res :: <list>)
  choose(complement(walker-defaulted-getter?), 
         walker-shallow-getters(walker, class))
end function;

define method walker-compute-defaulted-slot-descriptors 
    (walker :: <walker>, class :: <class>)
 => (res :: <walker-defaulted-slot-sequence-type>)
  let shallow-getters = walker-defaulted-shallow-getters(walker, class);
  collecting (as <walker-defaulted-slot-sequence-type>)
    for (sd in walker-slot-descriptors(class))
      for (shallow-getter in shallow-getters)
	if (slot-getter(sd) == walker-default-getter(shallow-getter))
	  collect(make-walker-defaulted-descriptor
		    (as-walker-slot-descriptor(class, sd), 
		     walker-default-thunk(shallow-getter)))
	end if;
      end for;
    end for;
  end collecting;
end method;

define method walker-compute-shallow-slot-descriptors 
    (walker :: <walker>, class :: <class>) => (res :: <walker-slot-sequence-type>)
  let shallow-getters = walker-real-shallow-getters(walker, class);
  map-as(<walker-slot-sequence-type>, curry(as-walker-slot-descriptor, class),
         choose(method (sd) member?(sd.slot-getter, shallow-getters) end, 
                walker-slot-descriptors(class)))
end method;

define method walker-compute-deep-slot-descriptors 
    (walker :: <walker>, class :: <class>) => (res :: <walker-slot-sequence-type>)
  let defaulted-getters = walker-defaulted-shallow-getters(walker, class);
  let shallow-getters   = walker-real-shallow-getters(walker, class);
  map-as(<walker-slot-sequence-type>, curry(as-walker-slot-descriptor, class),
	 if (empty?(defaulted-getters) & empty?(shallow-getters))
	   walker-slot-descriptors(class)
	 else
	   choose(method (sd) 
		    ~member?(sd.slot-getter, 
			     map(walker-default-getter, defaulted-getters)) &
		      ~member?(sd.slot-getter, shallow-getters)
		  end method, 
		  walker-slot-descriptors(class))
	 end)
end method;

define function walker-class
    (walker :: <walker>, class :: <class>) => (res :: <walker-class>)
  element(walker-classes(walker), class, default: #f) |
    (element(walker-classes(walker), class)
       :=  make(<walker-class>, 
		repeated-slot?:
		  walker-repeated-slot?(class),
		shallow-slot-descriptors:
		  walker-compute-shallow-slot-descriptors(walker, class),
		defaulted-slot-descriptors:
		  walker-compute-defaulted-slot-descriptors(walker, class),
		deep-slot-descriptors:
		  walker-compute-deep-slot-descriptors(walker, class)));
end function;

/*
define function walker-all-slot-descriptors
    (walker :: <walker>, class :: <class>)
 => (shallow-slotds :: <walker-slot-sequence-type>,
     defaulted-slotds :: <walker-defaulted-slot-sequence-type>,
     deep-slotds :: <walker-slot-sequence-type>)
  let walker-class = walker-class(walker, class);
  values(walker-class-shallow-slot-descriptors(walker-class),
	 walker-class-defaulted-slot-descriptors(walker-class),
	 walker-class-deep-slot-descriptors(walker-class))
end function;
*/

define inline function walker-slot-descriptors-of
    (walker :: <walker>, class :: <class>, access :: <function>) => (res)
  access(walker-class(walker, class))
end function;

define function walker-shallow-slot-descriptors
    (walker :: <walker>, class :: <class>) 
 => (res :: <walker-slot-sequence-type>)
  walker-slot-descriptors-of
    (walker, class, walker-class-shallow-slot-descriptors)
end function;

define function walker-defaulted-slot-descriptors
    (walker :: <walker>, class :: <class>)
 => (res :: <walker-defaulted-slot-sequence-type>)
  walker-slot-descriptors-of
    (walker, class, walker-class-defaulted-slot-descriptors)
end function;

define function walker-deep-slot-descriptors
    (walker :: <walker>, class :: <class>)
 => (res :: <walker-slot-sequence-type>)
  walker-slot-descriptors-of
    (walker, class, walker-class-deep-slot-descriptors)
end function;
