module: fast-subclass
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


import-cl-classes
  (clos(dylan-class) (as: <dylan-class>)
     );

import-cl-functions
  (clos(class-finalized-p) (as: class-finalized?)
     );

     

define constant $cl-dylan-classes =
    vector(<object>, <class>, <boolean>, <character>,
	   <symbol>, <keyword>, <list>, <pair>, <empty-list>, <array>,
	   <vector>, <simple-vector>, <simple-object-vector>, <string>,
	   <byte-string>, <method>, <function>, <generic-function>,
	   <collection>, <explicit-key-collection>,
	   <mutable-collection>, <sequence>, <mutable-explicit-key-collection>,
	   <mutable-sequence>, <condition>, <warning>, <serious-condition>,
	   <error>, <restart>, <simple-restart>);



define function safe-class? (class :: <class>) => (yes? :: <boolean>)
  instance?(class, <dylan-class>) 
    | (class-finalized?(class)
	 & member?(class, $cl-dylan-classes))
end;


define method d-direct-superclasses (class :: <class>) => (s :: <sequence>);
  let supers = remove(direct-superclasses(class), class);
  remove-duplicates(choose(safe-class?,
			   supers))
end;
    
define method d-all-superclasses (class :: <class>) => (s :: <sequence>);
  let direct-supes = d-direct-superclasses(class);
  remove-duplicates(reduce(union, direct-supes,
			   map(d-all-superclasses,
			       remove(direct-supes, class))));
end;


define method d-direct-subclasses (class :: <class>) => (s :: <sequence>);
  let subs = direct-subclasses(class);
  remove-duplicates(choose(safe-class?, subs))
end;

