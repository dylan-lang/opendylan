Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// BOOTED: define ... class <mutable-collection> ... end;


////////////
// INTERFACE
////////////


define constant <mutable-explicit-key-collection-type>
  = type-union(subclass(<mutable-explicit-key-collection>),
               <limited-mutable-explicit-key-collection-type>);

// Functions on <mutable-collection>

define open generic element-setter
  (new-value, collection :: <mutable-object-with-elements>, key) => new-value;

define open generic replace-elements!
  (collection :: <mutable-collection>,
   predicate :: <function>, new-value-fn :: <function>,
   #key count :: false-or(<integer>) = #f)
 => (mutable-collection :: <mutable-collection>);

define open generic fill!
  (collection :: <mutable-collection>, value,
   #key start :: <integer> = 0, end: last :: <integer>)
 => (mutable-collection :: <mutable-collection>);



/////////////////
// IMPLEMENTATION
/////////////////


//
// REPLACE-ELEMENTS!
//

define method replace-elements!
    (collection :: <mutable-collection>,
     predicate? :: <function>, new-value-function :: <function>,
     #key count :: false-or(<integer>) = #f)
 => (collection :: <mutable-collection>)
  with-fip-of collection /* Use with-setter? */
    if (count)
      let count :: <integer> = count;
      for (state = initial-state then next-state(collection, state),
           until: count = 0 | finished-state?(collection, state, limit))
        let element = current-element(collection, state);
        if (predicate?(element))
          current-element(collection, state) := new-value-function(element);
          count := count - 1
        end if;
      end for
    else
      for (state = initial-state then next-state(collection, state),
           until: finished-state?(collection, state, limit))
        let element = current-element(collection, state);
        if (predicate?(element))
          current-element(collection, state) := new-value-function(element);
        end if;
      end for;
    end;
    collection
  end with-fip-of
end method replace-elements!;


//
// FILL!
//

define method fill!
    (collection :: <mutable-collection>, value, #key start, end: last)
 => (collection :: <mutable-collection>)
  // Ignore start: and end: in the non-sequence case
  with-fip-of collection
    for (state = initial-state then next-state(collection, state),
         until: finished-state?(collection, state, limit))
      current-element(collection, state) := value;
    end for; /* Use with-setter? */
  end with-fip-of;
  collection
end method fill!;



//
// Specialized inherited generic methods
//


//
// TYPE-FOR-COPY
//

define method type-for-copy (collection :: <mutable-collection>)
 => (class :: subclass(<mutable-collection>))
  collection.object-class
end method type-for-copy;



//
// ELEMENT-NO-BOUNDS-CHECK-SETTER
//

define open generic element-no-bounds-check-setter
  (new-value, collection :: <mutable-collection>, key) => new-value;

define inline method element-no-bounds-check-setter
    (new-value, collection :: <mutable-collection>, key) => new-value;
  element-setter(new-value, collection, key)
end method element-no-bounds-check-setter;
