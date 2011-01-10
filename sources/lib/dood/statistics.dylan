Module:       dood
Synopsis:     The Dylan object-oriented database
Author:       Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method dood-as-memory-object 
    (dood :: <dood>, disk-object) => (res)
  disk-object
end method;

define method dood-as-memory-object 
    (dood :: <dood>, disk-object :: <dood-proxy>) => (res)
  // dood-proxy-value(disk-object)
  disk-object
end method;

define function dood-statistics 
    (dood :: <dood>, #key filter-set = #[], aggregate-set = #[])
  walker-instance-statistics
    (#t, curry(dood-as-memory-object, dood), object-class,
     debug-name, curry(dood-instance-size, dood),
     curry(dood-compute-instance-size, dood), dood-back-pointers(dood),
     filter-set: filter-set, aggregate-set: aggregate-set);
end function;

define function dood-diff-last-two-statistics (dood :: <dood>)
  walker-diff-last-two-statistics(debug-name, curry(dood-instance-size, dood))
end function;
