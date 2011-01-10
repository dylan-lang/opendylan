module:    walker
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method walker-repeated-slot? (class :: <class>)
  repeated-slot-descriptor(class)
end method;

define inline function walker-allocate-simple-object (class :: <class>) => (value)
  system-allocate-simple-instance(class)
end function;

define inline function walker-allocate-repeated-object 
    (class :: <class>, size) => (value)
  let repeated-slot :: <repeated-slot-descriptor> = repeated-slot-descriptor(class);
  let size-slot :: <slot-descriptor> = size-slot-descriptor(repeated-slot);
  allocate-instance(class, vector(init-keyword(size-slot), size))
end function;

define inline method walker-allocate-object (class :: <class>, size) => (value)
  let repeated-slot = repeated-slot-descriptor(class);
  if (repeated-slot)
    walker-allocate-repeated-object(class, size);
  else
    walker-allocate-simple-object(class)
  end if
end method;

define dont-walk-object <unbound> using <copier>;
