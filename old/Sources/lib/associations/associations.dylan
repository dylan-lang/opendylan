Module:    associations
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <association> (<object>)
  slot key, init-keyword: key:;
  slot value, init-keyword: value:;
end class <association>;

define method association-key (association :: <association>)
  association.key
end method association-key;

define method association-key-setter (new, association :: <association>)
  association.key := new
end method association-key-setter;

define method association-value (association :: <association>)
  association.value
end method association-value;

define method association-value-setter (new, association :: <association>)
  association.value := new
end method association-value-setter;

define class <associations> (<mutable-explicit-key-collection>)
  slot associations :: <list>, init-value: #();
end class <associations>;

define class <value-associations> (<associations>)
end class <value-associations>;

define method key-test (collection :: <value-associations>)
  \=
end method key-test;

define class <object-associations> (<associations>)
end class <object-associations>;

define method key-test (collection :: <object-associations>)
  \==
end method key-test;

define method make (class == <associations>, #rest all-keys)
  apply(make, <value-associations>, all-keys)
end method make;

define method size (assocs :: <associations>)
  assocs.associations.size
end method size;

define method element
    (assocs :: <associations>, a-key, #key default = unsupplied())
  iterate grovel (associations = assocs.associations)
    if (associations.empty?)
      if (default.unsupplied?)
        error("ELEMENT outside of range: %= %=", associations, a-key)
      else
        default
      end if
    else
      if (assocs.key-test(associations.head.key, a-key))
        associations.head.value
      else
        associations.tail.grovel
      end if
    end if
  end iterate
end method element;

define method element-setter (new-element, assocs :: <associations>, a-key)
  iterate grovel (the-associations = assocs.associations)
    if (the-associations.empty?)
      assocs.associations
        := add(assocs.associations,
               make(<association>, key: a-key, value: new-element))
    else
      if (assocs.key-test(the-associations.head.key, a-key))
        the-associations.head.value := new-element
      else
        the-associations.tail.grovel
      end if
    end if
  end iterate
end method element-setter;

define method clear! (assocs :: <associations>)
  assocs.associations := #();
  assocs
end method clear!;

define method remove-key! (assocs :: <associations>, a-key)
  assocs.associations
    := remove!
         (assocs.associations,
          #f,
          test: method (item, dummy)
                  assocs.key-test(item.key, a-key)
                end method);
  assocs
end method remove-key!;

//// ITERATION

define method initial-state (assocs :: <associations>)
  let next-state = assocs.associations;
  if (next-state.empty?)
    #f
  else
    next-state
  end if
end method initial-state;

define method next-state (assocs :: <associations>, state :: <list>)
  let next-state = state.tail;
  if (next-state.empty?)
    #f
  else
    next-state
  end if
end method next-state;

define method current-element (assocs :: <associations>, state :: <list>)
  state.head.value
end method current-element;

define method current-key (assocs :: <associations>, state :: <list>)
  state.head.key
end method current-key;

define method current-element-setter
    (new-value, assocs :: <associations>, state :: <list>)
  state.head.value := new-value
end method current-element-setter;

// eof
