Module:   dfmc-conversion
Synopsis: The compile-time slot protocol.
Author:   Keith Playford, from Jonathan's run-time code.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// ANY SLOT DESCRIPTOR

define method install-slot-descriptor-in-accessor-methods
    (slotd :: <&slot-descriptor>, class :: <&class>) => ()
  /*
  let getter = ^slot-getter(slotd);
  if (getter)
    let getter-method = find-getter-method(getter, class);
    if (getter-method)
      ^method-slot-descriptor(getter-method) := slotd;
    end;
  end;
  // "Setter" is reserved in the emulator...
  let zetter = ^slot-setter(slotd);
  if (zetter)
    let setter-method = find-setter-method(zetter, class);
    if (setter-method)
      ^method-slot-descriptor(setter-method) := slotd;
    end;
  end;
  */
end method;

define compiler-sideways method ^slot-descriptor (class, accessor :: <&function>)
 => (res :: false-or(<&slot-descriptor>))
  ^ensure-slots-initialized(class);
  block (return)
    for (a-slot-descriptor in class.^slot-descriptors)
      if (accessor == a-slot-descriptor.^slot-getter
            | accessor == a-slot-descriptor.^slot-setter)
        return(a-slot-descriptor)
      end if;
    finally
      let repeated = class.^repeated-slot-descriptor;
      if (repeated)
        let a-slot-descriptor = repeated.^size-slot-descriptor;
        if (accessor == a-slot-descriptor.^slot-getter)
          return(a-slot-descriptor)
        end;
      end;
    end for
  end block
end method ^slot-descriptor;

////
//// <ANY-INSTANCE-SLOT-DESCRIPTOR>
////

//// RUN-TIME

define compiler-sideways method ^slot-offset
    (slot-descriptor :: <&any-instance-slot-descriptor>, class :: <&class>)
  ^ensure-slots-initialized(class);
  let v :: <simple-object-vector> = ^instance-slot-descriptors(class);
  let n :: <integer> = size(v);
  local method loop1 (i :: <integer>)
          if (i == n)
            values(#f, #f)
          else
            let e :: <&slot-descriptor> = ^vector-element(v, i);
            if (^getter=(e, slot-descriptor))
              values(i, #f)
            else
              loop1(i + 1)
            end if
          end if
        end method;
  loop1(0);
end method ^slot-offset;

define function ^effective-initialization-argument-descriptor
    (descriptor :: <&slot-descriptor>, class :: <&class>) =>
    (descriptor :: <&slot-keyword-initialization-descriptor>)
  let owner = ^slot-owner(descriptor);
  if (owner == class)
    descriptor				// optimize common case
  else
    ^ensure-slots-initialized(class);
    let keyword = ^init-keyword(descriptor);
    if (keyword)
      block (return)
	for (c :: <&class> in ^all-superclasses(class),
	     until: c == owner)
	  for (d :: <&init-arg-descriptor> in
		 ^direct-initialization-argument-descriptors(c))
	    if (^init-keyword(d) == keyword &
		  (^init-keyword-required?(d) | ^init-supplied?(d)))
	      return(d)
	    end;
	  end;
	end;
	descriptor
      end
    else
      descriptor
    end
  end
end;

define function ^effective-initial-value-descriptor
    (descriptor :: <&slot-descriptor>, class :: <&class>) =>
    (descriptor :: <&slot-initial-value-descriptor>)
  let owner = ^slot-owner(descriptor);
  if (owner == class)
    descriptor				// optimize common case
  else
    ^ensure-slots-initialized(class);
    block (return)
      let getter = ^slot-getter(descriptor);
      for (c :: <&class> in ^all-superclasses(class),
	   until: c == owner)
	for (d :: <&inherited-slot-descriptor> in
	       ^direct-inherited-slot-descriptors(c))
	  if (^inherited-slot-getter(d) == getter & ^init-supplied?(d))
	    return(d)
	  end;
	end;
      end;
      descriptor
    end
  end
end;

////
//// <VIRTUAL-SLOT-DESCRIPTOR>
////

// define &class <virtual-slot-descriptor> (<slot-descriptor>)
// end &class <virtual-slot-descriptor>;

//// CREATION

define method ^as-slot-descriptor-class (symbol == #"virtual")
  <&virtual-slot-descriptor>
end method ^as-slot-descriptor-class;

define method ^slot-allocation (descriptor :: <&virtual-slot-descriptor>)
  #"virtual"
end method;


////
//// <CLASS-SLOT-DESCRIPTOR>
////

define method ^as-slot-descriptor-class (symbol == #"class")
  <&class-slot-descriptor>
end method ^as-slot-descriptor-class;

define method ^slot-allocation (descriptor :: <&class-slot-descriptor>)
  #"class"
end method;


define method ^as-slot-descriptor-class (symbol == #"each-subclass")
  <&each-subclass-slot-descriptor>
end method ^as-slot-descriptor-class;

define method ^slot-allocation (descriptor :: <&each-subclass-slot-descriptor>)
  #"each-subclass"
end method;

////
//// <REPEATED-SLOT-DESCRIPTOR>
////

// define class <repeated-slot-descriptor> (<any-instance-slot-descriptor>)
//   slot size-slot-descriptor;
// end class <repeated-slot-descriptor>;

define compiler-sideways method ^initialize-slot-descriptor
   (descriptor :: <&repeated-slot-descriptor>, 
      #key definition, owner, #all-keys)
  next-method();
  let type = dylan-value(#"<integer>");
  let init-supplied? = spec-size-init-supplied?(definition);
  let (init-data, init-value?, init-evaluated?) =
    if (init-supplied?)
      make-slot-init-model
        (spec-size-init-expression(definition), #f, #t, #{ <integer> }, type);
    else
      values(#f, #f, #f)
    end if;
  let size-descriptor 
    = ^make(<&instance-slot-descriptor>,
            definition:            definition,
            owner:                 owner,
	    type:                  type,
            getter:                ^top-level-eval(spec-size-getter(definition)),
            setter:                #f,
            init-keyword:          mapped-model(^top-level-eval(spec-size-init-keyword(definition))),
            init-supplied?:        init-supplied?,
	    init-value?:           #t,
	    init-evaluated?:       init-evaluated?,
	    init-data:             mapped-model(init-data));
  descriptor.^size-slot-descriptor := size-descriptor;
  descriptor
end method;

define method ^as-slot-descriptor-class (symbol == #"repeated")
  <&repeated-slot-descriptor>
end method ^as-slot-descriptor-class;

define compiler-sideways method ^slot-offset
    (slot-descriptor :: <&repeated-slot-descriptor>, class :: <&class>)
  ^ensure-slots-initialized(class);
  let v :: <simple-object-vector> = ^instance-slot-descriptors(class);
  values(size(v), #f)
end method ^slot-offset;

//// CREATION

define method ^slot-allocation (descriptor :: <&repeated-slot-descriptor>)
  #"repeated"
end method ^slot-allocation;

//// METHODS

//define generic accessor-method-dispatch-arg (m :: <&accessor-method>) =>
//    // TODO: limited(<integer>, min: 0, max: 1)
//    (index :: <integer>);

//define method accessor-method-dispatch-arg (m :: <&getter-accessor-method>)
// => (index :: singleton(0))
//  0
//end;

//define method accessor-method-dispatch-arg (m :: <&setter-accessor-method>)
// => (index :: singleton(1))
//  1
//end;


define inline function accessor-method-dispatch-arg (m :: <&accessor-method>) => (i :: <integer>)
  if (instance?(m, <&setter-accessor-method>)) 1 else 0 end
end function;


define function get-method-slot-descriptor (m :: <&accessor-method>)
 => (sd :: <&slot-descriptor>, class :: <&class>)
  let class =
    m.^function-signature.^signature-required[accessor-method-dispatch-arg(m)];
  ^ensure-slots-initialized(class);
  values(^method-slot-descriptor(m), class);
end;

define program-warning <slot-descriptor-missing>
  slot condition-accessor-method,
    required-init-keyword: accessor-method:;
  format-string 
    "Couldn't compute accessor method body at compile-time for %= - "
    "generating stub body.";
  format-arguments accessor-method;
end program-warning;

define method compute-method-body (m :: <&accessor-method>)
  #f
end method;

// TODO: rather than asking if the class has fixed-offset slots, ask
//       whether a given slot is at a fixed offset in all subclasses
define function ^instance-slots-have-fixed-offsets? (class :: <&class>)
  // How to determine if a given class has its slots fixed, for ALL possible
  // instances below class, from which they can be referenced.
  if (^slots-have-fixed-offsets?-computed?(class))
    // It's already been computed, so recycle instead of recompute.
    ^slots-have-fixed-offsets?-bit(class)
  elseif (~ ^ensure-slots-initialized(class))
    // We just don't know for sure.
    #f
  else
    // Look at all concrete subclasses, ensuring that the slots in this
    // class are in the same places in each.
    // Have to compute it, but memoize the result for recycling.
    let class-instance-slots = ^instance-slot-descriptors(class);
    let first-concrete-instance-slots = #f;
    local method ^subclass-slots-at-same-offsets?(subclass :: <&class>)
	   => (same? :: <boolean>)
	    if (~ ^ensure-slots-initialized(subclass))
              // We don't know for sure.
              #f
            elseif (^class-abstract?(subclass))
              // Not interesting.
              #t 
            elseif (~first-concrete-instance-slots)
              // Everything else must conform with the layout of this
              // first concrete subclass.
              first-concrete-instance-slots
                := ^instance-slot-descriptors(subclass);
              #t
            else
              // Slots in the argument class must be in the same position
              // in all concrete subclasses. Different slots from other
              // origins can be ignored when seen together.
              every?(method (first, test)
                       first == test
                         | ~(member?(first, class-instance-slots)
                               | member?(test, class-instance-slots))
                     end,
                     first-concrete-instance-slots,
		     ^instance-slot-descriptors(subclass))
            end;
          end;
    let fixed-offsets? =
      // [Primary classes already set the fixed-offsets? bit.]
      // Sealed class with sealed subclasses, each of which has all of
      // class's slots in the same places.
      // Remember that all-subclasses includes the class itself.
      begin
	let subclasses = ^all-subclasses-if-sealed(class);
	subclasses & every?(^subclass-slots-at-same-offsets?, subclasses)
      end;
    ^slots-have-fixed-offsets?-computed?(class) := #t;
    ^slots-have-fixed-offsets?-bit(class) := fixed-offsets?
  end
end;

define function slot-offset-fixed-in-class?
    (sd :: <&slot-descriptor>, class :: <&class>)
  let slot-owner = ^slot-owner(sd);
  ^instance-slots-have-fixed-offsets?(slot-owner) |  // slight optimization
    class ~== slot-owner &
    ^subtype?(class, slot-owner) &
    (^instance-slots-have-fixed-offsets?(class) |
       begin
	 // is the slot inherited from the nearest fixed-offset class?
	 let fixed-offset-superclass =
	   any?(method (class :: <&class>)
		  ^instance-slots-have-fixed-offsets?(class) & class
		end,
		^all-superclasses(class));
	 fixed-offset-superclass &
	   ^subtype?(fixed-offset-superclass, slot-owner)
       end)
end;

// The accessor used having previously determined that a slot has a fixed
// offset using the above predicates.

define function ^slot-fixed-offset 
    (sd :: <&slot-descriptor>, class :: <&class>)
 => (offset :: <integer>)
  if (^class-abstract?(class))
    // Locate the slot's position in the first concrete subclass.
    let subclasses = ^all-subclasses-if-sealed(class);
    if (~subclasses)
      ^slot-offset(sd, class);
    else
      block (return)
        for (subclass in subclasses)
          if (~(^class-abstract?(subclass)))
            return(^slot-offset(sd, subclass));
          end;
        finally
          ^slot-offset(sd, class);
          // SEALED ABSTRACT CLASS WITH NO CONCRETE SUBCLASSES
          // CHECK FOR THIS ELSEWHERE
          // error("Failed to find first concrete subclass of %= from which "
          //       "to take the fixed offset of %=.", class, sd);
        end;
      end;
    end;
  else
    ^slot-offset(sd, class);
  end;
end;

define function slot-guaranteed-initialized-in-class?
    (sd :: <&slot-descriptor>, class :: <&class>)
  ^init-supplied?(^effective-initial-value-descriptor(sd, class)) |
    begin
      let key-descriptor =
	^effective-initialization-argument-descriptor(sd, class);
      ^init-keyword-required?(key-descriptor) |
	^init-supplied?(key-descriptor)
    end
end;

// default methods

define method ^slot-allocation (slotd :: <&instance-slot-descriptor>)
  #"instance"
end method;

define method ^as-slot-descriptor-class (allocation == #"instance")
  <&instance-slot-descriptor>
end method;

// These functions are called by the back-end at link time in order
// to work out how to emit load-time patchups.

define method fixed-slot-primitive-fixup-info
    (class :: <&class>, sd :: <&any-instance-slot-descriptor>)
 => (primitive, offset)
  values(dylan-value(#"primitive-slot-value-setter"), ^slot-offset(sd, class));
end method;

define method repeated-slot-primitive-fixup-info
    (class :: <&class>, sd :: <&repeated-slot-descriptor>)
 => (primitive, base-offset)
  values(dylan-value(#"primitive-repeated-slot-value-setter"), 
         ^slot-offset(sd, class));
end method;

/*
// Note that these work for locating repeated getters and setters too.

define method find-getter-method 
    (getter :: <&method>, class :: <&class>) => (method-or-false)
  #f
end method;

define method find-setter-method 
    (zetter :: <&method>, class :: <&class>) => (method-or-false)
  #f
end method;

define method find-getter-method 
    (getter :: <&generic-function>, class :: <&class>) => (method-or-false)
  find-accessor-method-with-class-at-position(getter, class,
					      <&getter-accessor-method>, 0);
end method;

define method find-setter-method 
    (zetter :: <&generic-function>, class :: <&class>) => (method-or-false)
  find-accessor-method-with-class-at-position(zetter, class,
					      <&setter-accessor-method>, 1);
end method;
*/

/*
define method ^function-required 
    (m :: <&lambda>, position :: <integer>) => (res)
  element(m.^function-signature.^signature-required, position, default: #f)
end method;

define method ^function-required 
    (m :: <&getter-method>, position :: <integer>) => (res)
  let slotd = ^method-slot-descriptor(m);
  select (position)
    0         => ^slot-owner(slotd);
    otherwise => #f;
  end select
end method;

define method ^function-required 
    (m :: <&setter-method>, position :: <integer>) => (res)
  let slotd = ^method-slot-descriptor(m);
  select (position)
    0         => ^slot-type(slotd);
    1         => ^slot-owner(slotd);
    otherwise => #f;
  end select
end method;
*/

/*
define method ^accessor-class 
    (m :: <&accessor-method>, position :: <integer>) => (res)
  element(m.^function-signature.^signature-required, position, default: #f)
end method;

define function find-accessor-method-with-class-at-position
    (accessor :: <&generic-function>, class :: <&class>,
     accessor-class, position :: <integer>)
 => (method-or-false)
  any?(method(m)
	 instance?(m, accessor-class) &
           ^accessor-class(m, position) == class &
	   m
       end,
       ^generic-function-methods-known(accessor))
end;

*/
