Module:    doss-internals
Author:    Eliot Miranda
Synopsis:  DOSS policies (for dumping)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Policy objects decide which objects to treat specially during a dump
// and provide mechanism for various special treatments.

define method put-specially
    (obj :: <object>, policy :: <doss-policy>, dd :: <doss-dumper>)
 => (object-dumped? :: <boolean>)
  #f
end method put-specially;


// variable lookup hook to allow policies to interfere with the name lookup process,
// possibly renaming, etc.
define method locate-variable-via-policy
    (obj, policy :: <doss-policy>)
 => (encoding, module-name, library-name)
  locate-variable(obj)
end method locate-variable-via-policy;


// Basic DOSS policy dumps all classes and functions by name.
define class <basic-doss-policy> (<doss-policy>)
end class <basic-doss-policy>;

define method put-specially
    (obj :: <function>, policy :: <basic-doss-policy>, dd :: <doss-dumper>)
 => (object-dumped? :: <boolean>)
  put-reference(obj, dd);
  #t
end method put-specially;

define method put-specially
    (obj :: <class>, policy :: <basic-doss-policy>, dd :: <doss-dumper>)
 => (object-dumped? :: <boolean>)
  put-class-description(obj, dd);
  #t
end method put-specially;

// slot descriptors are not named (not referred to by variables) so we must compute them.
// We need two pieces of information to track down a slot-descriptor
// a) either the getter or setter generic function
// b) the class in which the slot descriptor is defined
//
// We can do b) by searching the method-specializers of the getter or setter's
// generic-function-methods.
define method all-subclasses-do-given
    (f :: <function>, class :: <class>, visited :: <object-set>) => ()
  if (~member?(class,visited))
    f(class);
    add!(visited,class);
    do(method (sc) all-subclasses-do-given(f,sc,visited) end, class.direct-subclasses)
  end
end method all-subclasses-do-given;

define method all-subclasses-do (f :: <function>, class :: <class>) => ()
  all-subclasses-do-given(f,class,make(<object-set>, size: 32))
end method all-subclasses-do;

define method put-specially 
    (obj :: <slot-descriptor>, policy :: <basic-doss-policy>, dd :: <doss-dumper>)
 => (object-dumped? :: <boolean>)
  let getter = obj.slot-getter;
  // my-format("put-specially slot-descriptor %=\n", obj);
  block (return)
    do(method (m)
	 // my-format("  put-specially method %=\n", m);
	 do(method (specializer)
	      // my-format("    put-specially specializer %=\n", specializer);
	      if (instance?(specializer,<class>)
		  & any?(method (sd)
			   sd.slot-getter == getter
			 end method,
			 specializer.slot-descriptors))
		// my-format("    put-specially found candidate!! %=\n", specializer);
		all-subclasses-do(method (class)
				    if (member?(obj,class.slot-descriptors))
				      put-apply(obj,
						dd,
						resolve-slot-descriptor,
						obj.slot-getter,
						class);
				      return(#t);
				    end
				  end method,
				  specializer);
	      end
	    end method,
	    m.specializers);
       end method,
       getter.generic-function-methods);
    // No, failed to find the class, so invoke the default bebahiour.
    next-method()
  end
end method put-specially;

// eof
