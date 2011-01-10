Module:   dfmc-modeling
Synopsis: Class type and context-sensitive operations
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Context sensitive queries.

// Direct subclasses queries.

define method ^all-direct-subclasses-known? 
    (class :: <&class>) => (well? :: <boolean>)
  ^class-sealed?(class) 
    // HACK: JB ADDED THIS FOR LOOSE MODE
    & ~form-dynamic?(model-definition(class)) 
    & ~library-contains-wildcard-subclasses?
         (language-definition(model-library(class)))
end method;

define inline function map-collecting-unless-false 
    (f :: <function>, s :: <sequence>) => (s :: <list>)
  collecting ()
    for (item in s)
      let result = f(item);
      if (result) collect(result) end;
    end;
  end;
end function;

define method ^direct-subclasses-known
    (class :: <&class>) => (subclasses :: <sequence>)
  let binding = model-variable-binding(class);
  if (binding)
    let definitions = binding-direct-subclass-definitions(binding);
    map-collecting-unless-false(form-model-object, definitions);
  else
    #() // This class didn't have an explicit definition
  end
end method;

define method ^sealed-with-no-subclasses? (class :: <&class>) => well?;
  ^all-direct-subclasses-known?(class) & empty?(^direct-subclasses(class))
end method;


define method ^direct-subclasses
    (class :: <&class>) => (subclasses :: <sequence>)
  unless (%direct-subclasses-initialized?(class))
    let form = model-definition(class);
    if (form)
      with-dependent-context ($compilation of form)
	%direct-subclasses(class)
	  := mapped-model(^direct-subclasses-known(class))
      end;
    end;
    %direct-subclasses-initialized?(class) := #t;
  end;
  %direct-subclasses(class)
end method;

// Other utilities.


// Returns a list of all subclasses or #f if the class or some subclass is open
// or otherwise unavailable.
define function ^all-subclasses-if-sealed (class :: <&class>)
 => (subclasses :: false-or(<list>));
  let subclasses = #();
  local method add-if (class)
	  if (^all-direct-subclasses-known?(class))
	    member?(class, subclasses) |
              (~member?(class, ^direct-subclasses(class)) &    // gts,98feb09
	      if (every?(add-if, ^direct-subclasses(class)))
		subclasses := pair(class, subclasses)
	      end )
	  end
	end method;
  add-if(class) & subclasses
end function;

// ^sealed-with-no-subclasses? is implemented in the class definition code.

define function ^least-primary-superclass (cl :: <&class>) 
    => (result :: false-or(<&class>))
  // Shared subroutine used by inheritance consistency checking and in the 
  // typist.
  // Cl if it's primary, otherwise least primary superclass of leftmost
  // CPL element that has one.  Least primary superclass of <object> is #f.
  // That amounts to: find the leftmost primary class in the CPL.
  any?(conjoin(^class-primary?, identity), ^all-superclasses(cl))
end;

//// Type operations.

//// Base type.

define method ^base-type (type :: <&class>) => (type :: <&class>)
  type
end method ^base-type;

//// Subtype? relationships.

define method ^subtype? (c1 :: <&class>, c2 :: <&class>) 
    => (value :: <boolean>)
  member?(c2, ^all-superclasses(c1))
end method;

//// Disjointness relationships.

// Disjointness is library-relative for compile-time sealing analysis.
// A method that was allowed to be defined after testing disjoint from
// some sealed domain becomes a potential blocking method (in a 
// particular argument position) of any class that might be defined 
// later to contravene the disjointness test (at that argument 
// position).

// TODO: On the grounds that rule 3 requires more information to be
// computed to do checking, should we define all classes "first" in a 
// compiler pass and mop-up as many problems as possible on the method
// tests? I guess you'd really like error messages in both places to
// indicate the problem.

// Are the classes disjoint as far as a particular library knows?

// "Two classes are disjoint if they have no common subclasses".

// This is only used to possibly give a domain sealing violation warning
// if #f is returned.  So it should return #t unless it's sure such a
// warning would be appropriate.
define method ^known-disjoint?
    (class1 :: <&class>, class2 :: <&class>) => (value :: <boolean>)
  local method guaranteed-joint-2? (class)
	  member?(class2, ^all-superclasses(class))
	    | any?(guaranteed-joint-2?,
		   if (^class-sealed?(class) |
			 current-library-description?(model-library(class)))
		     ^direct-subclasses(class)
		   else // else depends on current library, need to compute
		     ^direct-subclasses-known(class)
		   end)
	end;
  // This all-superclasses membership test should be an optimization because
  // the all-superclasses vector is precomputed and readily available in  
  // all contexts, whereas the direct subclasses may have to be computed.
  // This is potentially very expensive, particularly for classes towards
  // the top of the heterarchy (e.g. <object>).
  ~member?(class1, ^all-superclasses(class2))
    & ~member?(class2, ^all-superclasses(class1))
    & ~guaranteed-joint-2?(class1)
end method;
