module:   sealing-workbench
author:   Paul Haahr
synopsis: Sealing optimizations for dispatching.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////
//// disjointness
////

define generic &disjoint? (type-1 :: <&type>, type-2 :: <&type>) => boolean;

define method &disjoint? (type-1 :: <&type>, type-2 :: <&type>)
  #f
end method &disjoint?;

define method &disjoint? (type-1 :: <&singleton>, type-2 :: <&singleton>)
  type-1.singleton-&object ~== type-2.singleton-&object;
end method &disjoint?;

define method &disjoint? (type-1 :: <&singleton>, type-2 :: <&type>)
  ~&instance?(type-1.singleton-&object, type-2)
end method &disjoint?;

define method &disjoint? (type-1 :: <&type>, type-2 :: <&singleton>)
  ~&instance?(type-2.singleton-&object, type-1)
end method &disjoint?;

define method &disjoint? (class-1 :: <&class>, class-2 :: <&class>)
  // TODO: primary classes
  // TODO: open subclasses of sealed classes
  case
    (~class-1.open?) =>
      ~any?(rcurry(&subtype?, class-1), &all-subclasses(class-2));
    (~class-2.open?) =>
      ~any?(rcurry(&subtype?, class-2), &all-subclasses(class-1));
    otherwise =>
      disjointness-required?(class-1, class-2)
  end case
end method &disjoint?;

// please suggest a better name for &joint?

define generic &joint? (type-1 :: <&type>, type-2 :: <&type>) => boolean;

define method &joint? (type-1 :: <&type>, type-2 :: <&type>)
  #f
end method &joint?;

define method &joint? (type-1 :: <&singleton>, type-2 :: <&singleton>)
  type-1.singleton-&object == type-2.singleton-&object;
end method &joint?;

define method &joint? (type-1 :: <&singleton>, type-2 :: <&type>)
  &instance?(type-1.singleton-&object, type-2)
end method &joint?;

define method &joint? (type-1 :: <&type>, type-2 :: <&singleton>)
  &instance?(type-2.singleton-&object, type-1)
end method &joint?;

define method &joint? (class-1 :: <&class>, class-2 :: <&class>)
  any?(rcurry(&subtype?, class-1), &all-subclasses(class-2))
  | any?(rcurry(&subtype?, class-2), &all-subclasses(class-1))
end method &joint?;


////
//// type of an instance
////

define method abstract-instance? (instance) // for non-&objects
  #f
end method abstract-instance?;

define method instance-&type (object)
  (if (object.abstract-instance?)
     object-&class
   else
     &singleton
   end if)
    (object)
end method instance-&type;


////
//// method specificity in for single argument position (DIRM page 56)
////

/// Beta-like method combination (least specific methods called first,
/// can call inner) would be best for this.

define generic precedes-at-position?
    (type-1 :: <&type>, type-2 :: <&type>, argument-type :: <&type>)
 => (precedes? :: <boolean>, certain? :: <boolean>);

define method precedes-at-position?
    (type-1 :: <&type>, type-2 :: <&type>, argument-type :: <&type>)
 => (precedes?, certain?);
  case
    &subtype?(type-1, type-2) =>
      values(~ &subtype?(type-2, type-1), #t);
    &subtype?(type-2, type-1) =>
      values(#f, #t);
    otherwise =>
      values(#f, #f);
  end case;
end method precedes-at-position?;

define method precedes-at-position?
    (class-1 :: <&class>, class-2 :: <&class>, argument-type :: <&type>)
 => (precedes?, certain?);
  let (precedes-by-type?, certain?) = next-method();
  if (certain?)
    precedes-by-type?
  else
    // harder in the presence of not yet defined multiply inheriting classes
    let cpl = argument-type.&all-superclasses;
    let key-1 = find-key(cpl, curry(\==, class-1));
    let key-2 = find-key(cpl, curry(\==, class-2));
    if (key-1 & key-2)
      values(key-1 < key-2, #t)
    else
      values(#f, #f)
    end if
  end if
end method precedes-at-position?;


////
//// sorted applicable methods
////

define method potentially-applicable-method?
    (&method :: <&method>, types :: <sequence>)
  block (return)
    for (type in types, specializer in &method.&function-parameters)
      if (&disjoint?(type, specializer))
	return(#f)
      end if;
    end for;
    #t
  end block
end method potentially-applicable-method?;

define method more-specific?(method1, method2, types)
  let precedes? = #f;
  block (return)
    for (specializer1 in method1.&function-parameters,
	 specializer2 in method2.&function-parameters,
	 type in types)
      if (precedes-at-position?(specializer1, specializer2, type))
	precedes? := #t
      elseif (precedes-at-position?(specializer2, specializer1, type))
	return(#f)
      end if;
    end for;
    precedes?
  end block
end method more-specific?;

// Way too iterative an implementation, but for now it works.

define method most-specific-&method (methods, types)
  // all methods must be applicable
  select (methods.size)
    0 =>
      #f;
    1 =>
      methods.first;
    otherwise =>
      let best = #f;
      let best-index = 0;
      for (challenge in methods, index from 0)
	if (~best | more-specific?(challenge, best, types))
	  best := challenge;
	  best-index := index
	elseif (~more-specific?(best, challenge, types))
	  best := #f
	end if
      end for;
      if (best)
	block (return)
	  for (challenge in methods, index from 0 below best-index)
	    unless (more-specific?(best, challenge, types))
	      return(#f)
	    end unless
	  end for
	end block;
      end if;
      best
  end select
end method most-specific-&method;

// The sorted-methods sequence returned by sorted-applicable-&methods
// is a full next method chain.  It ends either at the first method
// that doesn't call a next method or when it's impossible to determine
// what method comes next.  The second value, if non-empty, means the
// first result was empty or ended in a method which calls next.
//
// This is written as an O(n^2) sort, at least, but its results should
// be cacheable.

define method sorted-applicable-&methods
    (&generic :: <&generic>, types :: <sequence>) =>
    (sorted-methods :: <sequence>, unsorted-methods :: <sequence>)
  let reversed-sorted-methods = #();
  let unsorted-methods
    = choose(rcurry(potentially-applicable-method?, types),
             &generic.&generic-methods);

  block (exit)
    local method return-reversed (reversed-sorted-methods, unsorted-methods)
	    exit(reverse!(reversed-sorted-methods), unsorted-methods)
	  end method return-reversed;
    while (#t)
      let most-specific = most-specific-&method(unsorted-methods, types);
      unless (most-specific)
	return-reversed(reversed-sorted-methods, unsorted-methods)
      end unless;
      reversed-sorted-methods := pair(most-specific, reversed-sorted-methods);
      unless (most-specific.calls-next-method?)
	return-reversed(reversed-sorted-methods, #())
      end unless;
      unsorted-methods := remove(unsorted-methods, most-specific);
      if (empty?(unsorted-methods))
	return-reversed
          (pair(no-applicable-&method, reversed-sorted-methods), #())
      end if
    end while
  end block

end method sorted-applicable-&methods;


////
//// dispatching
////

define method more-specific-applicable-method?
    (&method :: <&method>, types :: <sequence>)
  block (return)
    let more-specific? = #f;
    for (type in types, specializer in &method.&function-parameters)
      if (&subtype?(specializer, type))
        unless (&subtype?(type, specializer)) // type equivalence
          more-specific? := #t
        end unless
      elseif (&disjoint?(type, specializer))
        return(#f)
      end if;
    end for;
    more-specific?
  end block
end method more-specific-applicable-method?;

define method more-specific-applicable-methods
    (&generic :: <&generic>, types :: <sequence>)
  choose(rcurry(more-specific-applicable-method?, types),
         &generic.&generic-methods)
end method more-specific-applicable-methods;

define method dispatch(generic :: <&generic>, arguments :: <sequence>)

  unless (arguments.size == generic.&function-parameters.size)
    error("call to %= with %= arguments", generic, arguments.size)
  end unless;

  format-out("DISPATCHING %=\n", generic);

  let types = map(instance-&type, arguments);

  for (argument in arguments,
       type in types,
       parameter in generic.&function-parameters)

    if (*verbose?*)
      format-out("  ARGUMENT %=\n", argument);
    end if;
    unless (&instance?(argument, parameter))
      if (&disjoint?(type, parameter))
	format-out("  TYPE ERROR: %= is disjoint from %=\n", type, parameter);
      else
        format-out("  TYPECHECK %= :: %=\n", argument, parameter);
      end if;
    end unless;
  end for;

  block (exit)

    if (sealed-over?(generic, types))
      let more-specific = more-specific-applicable-methods(generic, types);

      if (more-specific.empty?)
        let (call-chain, dynamic)
          = sorted-applicable-&methods(generic, types);
	for (call in call-chain)
	  format-out("  CALL %=\n", call);
	end for;
        if (~dynamic.empty?)
          format-out("  DYNAMIC DISPATCH AMONG\n");
          for (unordered in dynamic)
            format-out("    %=\n", unordered);
          end for;
        end if;
        exit();
      end if;

      // TODO: inverted-vtable
    end if;
      
    format-out("  FULL DYNAMIC\n");

  end block; // exit

end method dispatch;
