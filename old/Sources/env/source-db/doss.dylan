Module: source-db
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *dd-debug-print* = #t;
define variable *dd-debug-stream* = *standard-output*;

define method dbg-format (#rest all-args)
  if (*dd-debug-print*)
    apply(format, *dd-debug-stream*,  all-args);
  end if;
end method;

// doss policy for source db

define class <source-db-doss-policy> (<doss-policy>)
end class;

define method false-slot-value(getter :: <method>, object)
  if(slot-initialized?(object,getter))
    dbg-format("Applying %= \n   to %=\n", getter, object);
    dbg-format("   = %=\n", object.getter);
  end;
  block()
    dbg-format("Debug name: %s \n", getter.debug-name);
  exception(err :: <error>)
    dbg-format("Cannot access debug name: %s\n", err);
  end;
  #f;
end;

// Don't dump the source
//
define method doss-slot-value(getter == section-text, object, dd :: <doss-dumper>)
  false-slot-value(getter, object);
end;

// Reverse to #f
define method doss-slot-value(getter == in-core, object, dd :: <doss-dumper>)
  false-slot-value(getter, object);
end;

define constant include-slot-descriptors = access(front-end,include-slot-descriptors);

define constant $included-section-getters = vector(id, section-text, section-start,
						   section-length, referencors, predecessor,
						   successors);

define method doss-dumpable-slots(class :: subclass(<section>), 
				  policy :: <source-db-doss-policy>)
  dbg-format("*section dump %= \ndumpable %=\n", class, 
	     include-slot-descriptors(class, $included-section-getters));
  include-slot-descriptors(class, $included-section-getters);
end;


