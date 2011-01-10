Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////
//// ANCIENT HISTORY
////

define method concatenate-2
    (list-1 :: <list>, list-2 :: <list>) => (result :: <list>)
  if (list-1.empty?)
    list-2
  else
    let rest :: <list> = list-1.tail;
    pair(list-1.head, concatenate-2(rest, list-2))
  end if
end method concatenate-2;

define method concatenate-2
    (v1 :: <simple-object-vector>, v2 :: <simple-object-vector>)
 => (result :: <simple-object-vector>)
  let size-1 :: <integer> = v1.size;
  let size-2 :: <integer> = v2.size;
  if (size-1 == 0)
    v2
  elseif (size-2 == 0)
    v1
  else
    let new-vector :: <simple-object-vector>
      = make(<simple-object-vector>, size: size-1 + size-2);
    for (index :: <integer> from 0 below size-1)
      vector-element(new-vector, index) := vector-element(v1, index);
    end for;
    for (index-2 :: <integer> from 0 below size-2,
	 index :: <integer> from size-1)
      vector-element(new-vector, index) := vector-element(v2, index-2);
    end for;
    new-vector
  end if;
end method concatenate-2;

// define function precedes? (object1, object2, objects :: <list>)
//   iterate walk (cursor :: <list> = objects)
//     let object = cursor.head;
//     case
//       object == object1 =>
//         #t;
//       object == object2 =>
//         #f;
//       otherwise =>
//         walk(cursor.tail)
//     end case;
//   end iterate
// end function;

// bubble sort -- yuck!

// define method bubble-sort! (items :: <list>, #key test = \<, stable) 
//  => (result :: <list>)
//   let tmp = #f;
//   until (tmp == items.head)
//     tmp := items.head;
//     for (prev :: <list> = items then prev.tail,
//          next :: <list> = items.tail then next.tail,
//          until: next.empty?)
//       //	(unless (test (head prev) (head next))
//       //      Only rearrange on a definite positive outcome;
//       if (test(next.head, prev.head))
//         tmp := prev.head;
//         prev.head := next.head;
//         next.head := tmp
//       end if;
//     end for
//   end until;
//   items
// end method bubble-sort!;

/////
///// GROUNDING METHODS
/////

/// We ground the potential infinite recursion on method look up by
/// constraining instance? and subtype? to allow only class and
/// singleton specializers. These are handled specially in the
/// dispatch without calling either generic function through use
/// of the following grounded versions:

//define function class? (type :: <type>)
//  let class :: <class> = type.object-class;
//  class == <class>
//    | subclass?(class, <class>)
//end function;

//define function singleton? (object)
//  object.object-class == <singleton>
//end function;

define inline function class? (type :: <type>)
  instance?(type, <class>)
end function;


define inline function singleton? (object)
  instance?(object, <singleton>)
end function;


define inline function grounded-instance? (o, type :: <type>)
  primitive-instance?(o, type)
end function;


define function grounded-subtype? (type1 :: <type>, type2 :: <type>)
  case
    type1.class? & type2.class? =>
      let type1 :: <class> = type1;
      let type2 :: <class> = type2;
      subclass?(type1, type2);
    type1.class? & type2.singleton? =>
      #f;
    type1.singleton? & type2.class? =>
      let type1 :: <singleton> = type1;
      let type2 :: <class> = type2;
      grounded-instance?(type1.singleton-object, type2);
    type1.singleton? & type2.singleton? =>
      let type1 :: <singleton> = type1;
      let type2 :: <singleton> = type2;
      type1.singleton-object == type2.singleton-object;
    otherwise =>
      subtype?(type1, type2)
  end case
end function;


// concrete-subtype?(<back-end>, <c-back-end>) => #t
//
// For classes C1 and C2, 
// C1 is a concrete subtype of C2 if all concrete subclasses of C1 are
// subclasses of C2.  So if <c-back-end> is the only subclass <back-end>,
// and <back-end> is abstract and <c-back-end> concrete, <back-end>
// is a concrete subtype of <c-back-end>.
// Consider also the lattice, with A, B, and C abstract:
//                      A
//                     / \
//                    B   C
//                     \ /
//                      D
// We can similarly deduce that A, B, and C are all concrete subtypes
// of D.  Even more unusual is deducing that B and C are mutually
// concrete subtypes.

define function concrete-subtype? (type1 :: <type>, type2 :: <type>, dep :: false-or(<generic-function>))
 => (well? :: <boolean>)
  grounded-subtype?(type1, type2)
    | (type1 ~== <object>
	 & class?(type1) 
	 & class?(type2)
	 & (begin
	      let c1 :: <class> = type1;
	      let c2 :: <class> = type2;
	      let ic1 :: <implementation-class> = class-implementation-class(c1);
	      let ic2 :: <implementation-class> = class-implementation-class(c2);
	      // Only do this if the classes are joint.  Otherwise, we get frequent
	      // quasi-false positives where unrelated classes with no concrete subclasses
	      // are noted - correctly - as being subtypes.
	      (subiclass?(ic2, c2, ic1, c1) 
		| member?(c1, class-known-joint(ic2))
		| member?(c2, class-known-joint(ic1)))
		&
		(~*conservative-concrete-subtype? | (iclass-subclasses-fixed?(ic1) & iclass-subclasses-fixed?(ic2)))
		& 
		begin
		  let st? = concrete-subtype?-internal(c1, c2);
		  when (st? & dep)
		    %register-subclass-dependent-generic(dep, c1)
		  end when;
		  st?
		end 
	    end))
end function;


define variable *conservative-concrete-subtype? = #f;



define function concrete-subtype?-internal (c1 :: <class>, c2 :: <class>)
 => (well? :: <boolean>)
  let ic1 :: <implementation-class> = class-implementation-class(c1);
  let ic2 :: <implementation-class> = class-implementation-class(c2);
  (class-abstract?(ic1) | subiclass?(ic1, c1, ic2, c2))
    &
    every?(rcurry(concrete-subtype?-internal, c2), direct-subclasses(ic1))
end function;


define function grounded-has-instances? (class :: <class>, type :: <type>)
  select (type by instance?)
    <class> =>
      let type :: <class> = type;
      if (subclass?(class, type))
	values(#t, #t)
      else
	values(#f, #f)
      end if;
    <singleton> =>
      let type :: <singleton> = type;
      values(class == object-class(type.singleton-object), #f);
    <union> =>
      let type :: <union> = type;
      has-instances?(class, type, $empty-subjunctive-class-universe);
    <subclass> =>
      let type :: <subclass> = type;
      has-instances?(class, type, $empty-subjunctive-class-universe);
    <limited-integer> =>
      let type :: <limited-integer> = type;
      has-instances?(class, type, $empty-subjunctive-class-universe);
    otherwise =>
      has-instances?(class, type, $empty-subjunctive-class-universe);
  end select
end function;


/////
///// CORE DISPATCHING BUILDING BLOCKS
/////

////
//// PREDICATE USED TO ORDER SPECIALIZERS WRT ARGUMENT
////

define function every-2?
    (function :: <function>,
 argument-1 :: <simple-object-vector>,
 argument-2 :: <simple-object-vector>)
  let size-1 :: <integer> = argument-1.size;
  let size-2 :: <integer> = argument-2.size;
  let min-size :: <integer>
    = if (size-1 < size-2)
	size-1
      else
	size-2
      end if;
  // MIN
  iterate search (index :: <integer> = 0)
    if (index = min-size)
      #t
    else
      function(vector-element(argument-1, index),
	       vector-element(argument-2, index))
      & search(index + 1)
    end if
  end iterate
end function;

////
//// IS ARGUMENT CONSISTENT WITH SPECIALIZER?
////

define function applicable-argument?
    (argument, specializer) => (result :: <boolean>)
  grounded-instance?(argument, specializer)
end function;

////
//// IS METHOD CONSISTENT WITH GIVEN ARGUMENTS?
//// ASSUMING THAT THE NUMBER OF ARGUMENTS IS NOT IN QUESTION
////

define function applicable-method-assuming-number-required?
    (function :: <method>, arguments :: <simple-object-vector>)
 => (result :: <boolean>)
  every-2?(applicable-argument?, arguments, function.function-specializers)
end function;

define function unbound-instance-slot (object, offset :: <integer>)
 => (will-never-return :: <bottom>)
  let sd :: <slot-descriptor>
    = vector-element(instance-slot-descriptors(object-class(object)), offset);
  
  error(make(<simple-slot-error>,
             format-string: "The %s slot is unbound in %s.", 
             format-arguments: list(slot-getter(sd) | sd, object)))
end function;


define function unbound-repeated-slot (object, idx :: <integer>)
 => (will-never-return :: <bottom>)
  let sd :: <slot-descriptor> = repeated-slot-descriptor(object-class(object));
  error(make(<simple-slot-error>,
             format-string: "%s at index %s is unbound in %s.", 
             format-arguments: list(slot-getter(sd) | sd, idx, object)))
end function;


define function unbound-class-slot (inst, offset :: <integer>)
 => (will-never-return :: <bottom>)
  let cls :: <class> = if (instance?(inst, <class>))
			 inst
		       else
			 object-class(inst)
		       end if;
  let sd :: <slot-descriptor>
    = vector-element(class-slot-descriptors(cls), offset);
  error(make(<simple-slot-error>, 
             format-string: "The %s slot is unbound in %s.", 
             format-arguments: list(slot-getter(sd) | sd, inst)))
end function;

// eof;
