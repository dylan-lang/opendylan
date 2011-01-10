module: walker
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// STATISTICS

define method do-deep-walk
    (walker :: <walker>, function :: <function>, parent, 
     object :: <byte-vector>) 
  walker-register-walked(walker, parent, object, object); 
  function(object);
end method;

define method walker-required-instance-size (class == <simple-object-vector>)
  2
end method;

define method walker-required-instance-size (class == <byte-string>)
  2
end method;

define method walker-required-instance-size (class == <symbol>)
  2
end method;

define method walker-required-instance-size (class :: subclass(<list>))
  3
end method;

define constant $default-walker = make(<walker>);

define method walker-required-instance-size (class :: <class>)
  size(walker-deep-slot-descriptors($default-walker, class))
end method;

define method walker-required-instance-size (class == <boolean>)
  1
end method;

define method walker-instance-size (element)
  walker-required-instance-size(object-class(element))
end method;

define method walker-instance-class (element)
  object-class(element)
end method;

define method walker-debug-name (element :: <class>)
  debug-name(element)
end method;

define method walker-instance-size (element :: <byte-string>)
  round/(size(element), 4) + 2
end method;

define method walker-instance-size (element :: <byte-vector>)
  round/(size(element), 4) + 2
end method;

define method walker-instance-size (element :: <simple-object-vector>)
  size(element) + 2
end method;

define function walker-stats (walker :: <walker>, display?)
  walker-instance-statistics
    (display?, identity, walker-instance-class, walker-debug-name,
     walker-instance-size, walker-required-instance-size,
     walker-walked(walker))
end function;
