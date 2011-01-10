Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method slot-allocation (descriptor :: <instance-slot-descriptor>)
    => (result :: <symbol>);
  #"instance"
end method slot-allocation;

define method as-slot-descriptor-class (symbol == #"constant")
  <constant-slot-descriptor>
end method as-slot-descriptor-class;

define method add-setter-method
    (class :: <class>,
     slot-setter :: <generic-function>,
     descriptor :: <constant-slot-descriptor>,
     override-sealing? :: <boolean>)
  values(#f, #f)
end method add-setter-method;

// define method remove-setter-method
//     (class :: <class>,
//      slot-setter :: <generic-function>,
//      descriptor :: <constant-slot-descriptor>)
//   #f
// end method remove-setter-method;

define method initialize
    (descriptor :: <constant-slot-descriptor>,
     #rest all-keys,
     #key init-value = $not-found,
          init-function,
          init-keyword,
          required-init-keyword,
          setter: setter-var)
  next-method();
  if (init-value == $not-found & ~init-function)
    error(make(<simple-slot-error>,
               format-string: "Must specify init-value or init-function for a constant slot %=",
               format-arguments: list(descriptor)))
  end if;
  if (init-keyword | required-init-keyword | setter-var)
    error(make(<simple-slot-error>,
               format-string: "Illegal slot-definition for constant slot %=.", 
               format-arguments: list(all-keys)))
  end if;
  descriptor
end method initialize;

define method as-slot-descriptor-class (symbol == #"class")
  <class-slot-descriptor>
end method as-slot-descriptor-class;

define method as-slot-descriptor-class (symbol == #"each-subclass")
  <each-subclass-slot-descriptor>
end method as-slot-descriptor-class;

define method slot-allocation (descriptor :: <virtual-slot-descriptor>)
    => (result :: <symbol>);
  #"virtual"
end method slot-allocation;

define method as-slot-descriptor-class (symbol == #"repeated")
  <repeated-slot-descriptor>
end method as-slot-descriptor-class;

define method initialize
    (descriptor :: <slot-initial-value-descriptor>,
     #rest all-keys,
     #key  init-value = $not-found,
           init-function = $not-found,
     #all-keys) 
 => ()
  next-method();
  apply(initialize-packed-slots, descriptor, all-keys);
  let init-value? = init-value ~== $not-found;
  let init-function? = init-function ~== $not-found;
  if (init-value? & init-function?)
    error(make(<simple-slot-error>,
               format-string: "Init-value: may not be specified with init-function: %=",
               format-arguments: list(descriptor)))
  end if;
  if (init-value?)
    descriptor.init-evaluated? := #t;
    descriptor.init-supplied? := #t;
    descriptor.init-value? := #t;
    descriptor.init-data-slot := init-value;
  elseif (init-function?)
    descriptor.init-evaluated? := #t;
    descriptor.init-supplied? := #t;
    descriptor.init-data-slot := init-function;
  end;
end method initialize;

define method initialize
    (descriptor :: <slot-keyword-initialization-descriptor>,
     #key required-init-keyword,
          init-keyword,
          init-value = $not-found,
          init-function = $not-found,
     #all-keys)
 => ()
  next-method();
  if (init-keyword & ~instance?(init-keyword, <symbol>))
    error(make(<simple-slot-error>,
               format-string: "Init-keyword: must be a <symbol>: %=", 
               format-arguments: list(init-keyword)))
  end if;
  let init-value? = init-value ~== $not-found;
  let init-function? = init-function ~== $not-found;
  if (required-init-keyword
      & (init-keyword | init-value? | init-function?))
    error(make(<simple-slot-error>,
               format-string: "Required-init-keyword: may not be specified with "
                              "init-keyword, init-value, or init-function: %=",
               format-arguments: list(descriptor)))
  end if;
  if (required-init-keyword)
    descriptor.init-keyword-required? := #t;
    descriptor.init-keyword := required-init-keyword
  end if;
end method initialize;

////
//// SLOTS-COMPATIBLE?
////
//// determines whether a slot-descriptor is compatible with an inherited one.
////

// define method slots-compatible?
//     (new-descriptor :: <slot-descriptor>, old-descriptor :: <slot-descriptor>)
//   local method eqv (a, b, default)
//           (~(a == default) & ~(b == default)) | (a == default & b == default)
//         end method eqv;
//   eqv(new-descriptor.slot-type, old-descriptor.slot-type, <object>)
//   & subtype?(new-descriptor.slot-type, old-descriptor.slot-type)
//   & new-descriptor.init-supplied? == old-descriptor.init-supplied?
//   & new-descriptor.init-value? == old-descriptor.init-value?
//   & eqv(new-descriptor.init-keyword, old-descriptor.init-keyword, #f)
//   & eqv(new-descriptor.slot-allocation,
//         old-descriptor.slot-allocation,
//         #"instance")
//   & eqv(new-descriptor.slot-setter, old-descriptor.slot-setter, #f)
//   // @@@@ getter ???
// end method slots-compatible?;


/*
  xep:             xep
  slot-descriptor: method-slot-descriptor
*/

define method initialize (m :: <getter-method>, #rest initargs,
			  #key slot-descriptor :: <slot-descriptor>) => ()
  primitive-set-accessor-method-xep(m, if (instance?(slot-descriptor, <any-instance-slot-descriptor>))
					 0
				       else
					 2
				       end if);
  // The next-method() is deliberately after the above initializations;
  // the xep because I feel better with it being initialized as early as possible.
  next-method();
end method;

define method initialize (m :: <setter-method>, #rest initargs,
			  #key slot-descriptor :: <slot-descriptor>) => ()
  primitive-set-accessor-method-xep(m, if (instance?(slot-descriptor, <any-instance-slot-descriptor>))
					 1
				       else
					 3
				       end if);
  // The next-method() is deliberately after the above initializations;
  // the xep because I feel better with it being initialized as early as possible.
  next-method();
end method;
  
define method initialize (m :: <repeated-getter-method>, #rest initargs,
			  #key slot-descriptor :: <slot-descriptor>) => ()
  primitive-set-accessor-method-xep(m, 4);
  // The next-method() is deliberately after the above initializations;
  // the xep because I feel better with it being initialized as early as possible.
  next-method();
end method;

define method initialize (m :: <repeated-setter-method>, #rest initargs,
			  #key slot-descriptor :: <slot-descriptor>) => ()
  primitive-set-accessor-method-xep(m, 5);
  // The next-method() is deliberately after the above initializations;
  // the xep because I feel better with it being initialized as early as possible.
  next-method();
end method;


define generic slot-accessor-method-classes (sd :: <slot-descriptor>)
 => (getter-class :: false-or(<class>), setter-class :: false-or(<class>));


// Default.
define method slot-accessor-method-classes (sd :: <slot-descriptor>)
 => (getter-class :: <class>, setter-class :: <class>);
  values(<getter-method>, <setter-method>)
end method;

define method slot-accessor-method-classes (sd :: <virtual-slot-descriptor>)
 => (false == #f, false == #f);
  values(#f, #f)
end method;

define method slot-accessor-method-classes (sd :: <repeated-slot-descriptor>)
 => (getter-class :: <class>, setter-class :: <class>);
  values(<repeated-getter-method>, <repeated-setter-method>)
end method;



//define function make-a-slot-method (sd :: <slot-descriptor>, setter?)
//  let owner :: <class> = slot-owner(sd);
//  let type :: <type> = slot-type(sd);
//  select (sd by instance?)
//    <virtual-slot-descriptor> => #f;
//    <repeated-slot-descriptor> =>
//      if (setter?)
//	method (value :: type, object :: owner, idx :: <integer>)
//	  repeated-slot-value(object, sd, idx) := value
//	end method
//      else
//	method (object :: owner, idx :: <integer>) repeated-slot-value(object, sd, idx) end
//      end if;
//    otherwise => 
//      if (setter?)
//	method (value :: type, object :: owner) slot-value(object, sd) := value end
//      else
//	method (object :: owner) slot-value(object, sd) end
//      end if;
//  end select;
//end function;


define function make-a-slot-method (sd :: <slot-descriptor>, setter?)
  let (gtype, stype) = slot-accessor-method-classes(sd);
  let thetype = if(setter?) stype else gtype end;
  thetype & make(thetype, slot-descriptor: sd)
end function;


// define function batch-create-slot-methods (slot-descriptors :: <sequence>)
//   map(method (sd :: <slot-descriptor>)
// 	let g = slot-getter(sd);
// 	let s = slot-setter(sd);
// 	let (gtype, stype) = slot-accessor-method-classes(sd);
// 	vector(gtype & g & make(gtype, slot-descriptor: sd),
// 	       stype & s & make(stype, slot-descriptor: sd))
//       end method,
//       slot-descriptors)
// end function;



define method add-getter-method
    (class-NOT :: <class>,
     slot-getter :: <generic-function>,
     descriptor :: <slot-descriptor>,
     override-sealing? :: <boolean>)
  let new-method = make-a-slot-method(descriptor, #f);
  %add-a-method(slot-getter, new-method, home-library(class-module(class-NOT)),
		#t, ~ override-sealing?, slot-method-sealed?(descriptor))
end method add-getter-method;


define method add-setter-method
    (class-NOT :: <class>,
     slot-setter :: <generic-function>,
     descriptor :: <slot-descriptor>,
     override-sealing? :: <boolean>)
  let new-method = make-a-slot-method(descriptor, #t);
  %add-a-method(slot-setter, new-method, home-library(class-module(class-NOT)),
		#t, ~ override-sealing?, slot-method-sealed?(descriptor))
end method add-setter-method;


// define method remove-getter-method
//     (class :: <class>,
//      slot-getter :: <generic-function>,
//      descriptor :: <slot-descriptor>)
//   let getter-method = find-method(slot-getter, class.list);
//   if (instance?(getter-method, <getter-accessor-method>))
//     let getter-method :: <getter-accessor-method> = getter-method;
//     
//     remove-method(slot-getter, getter-method)
//   end if
// end method remove-getter-method;

// define method remove-setter-method
//     (class :: <class>,
//      slot-setter :: <generic-function>,
//      descriptor :: <slot-descriptor>)
//   let setter-method
//     = find-method(slot-setter, list(descriptor /* .slot-type */, class));
//   if (setter-method)
//     %remove-method(slot-setter, setter-method)
//   end if
// end method remove-setter-method;

define method add-getter-method
    (class :: <class>,
     slot-getter :: <generic-function>,
     descriptor :: <virtual-slot-descriptor>,
     override-sealing? :: <boolean>)
  values(#f, #f)
end method add-getter-method;

define method add-setter-method
    (class :: <class>,
     slot-setter :: <generic-function>,
     descriptor :: <virtual-slot-descriptor>,
     override-sealing? :: <boolean>)
  values(#f, #f)
end method add-setter-method;

// define method remove-getter-method
//     (class :: <class>,
//      slot-getter :: <generic-function>,
//      descriptor :: <virtual-slot-descriptor>)
//   #f
// end method remove-getter-method;

// define method remove-setter-method
//     (class :: <class>,
//      slot-setter :: <generic-function>,
//      descriptor :: <virtual-slot-descriptor>)
//   #f
// end method remove-setter-method;
