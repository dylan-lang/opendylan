Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The <C-pointer> class

// Structure:

define class <C-pointer> (<C-value>) 
  slot pointer-pointer,
    init-value:   #f,
    init-keyword: pointer:;
  slot pointer-element-count,
    init-value: #f,
    init-keyword: element-count:;
end class;

define subclass-slot referenced-type = #f of <C-pointer> end;

size-of(<C-pointer>)      := 4;
alignment-of(<C-pointer>) := 4;

// Protocol:

define generic default-allocator (ptr-class) => (allocator);

define generic null-pointer (ptr-class :: <C-pointer>)
  => (ptr :: <boolean>);

define generic null-pointer? (ptr :: <C-pointer>) 
  => (well? :: <boolean>);

// Implementation: 

define method default-allocator (pc :: subclass(<C-pointer>))
  method (count)
    format(#t, "malloc(~a)", count);
    let addr = C-malloc(count);
    format(#t, " -> ~x~%", addr);
    addr
  end
end method;

define method null-pointer (ptr-class :: subclass(<C-pointer>))
  make(ptr-class, pointer: $null-address)
end method;

define method null-pointer? (ptr :: <C-pointer>)
  pointer-pointer(ptr) == $null-address
end method;

define method pointer-type (class :: subclass(<C-value>))
  stored-pointer-type(class) |
    begin
      let ptr-class = make(<class>,
                           debug-name:   #"<pointer-class>",
                           superclasses: 
                             list(superclass-for-pointer-type(class)));
      copy-wrapper-properties(ptr-class, <C-pointer>);
      stored-pointer-type(class) := ptr-class;
      referenced-type(ptr-class) := class;
      ptr-class
    end
end method;

// Pointer arithmetic:

define method \+ (ptr :: <C-pointer>, i :: <integer>)
  make(object-class(ptr), pointer: pointer-pointer(ptr) + i)
end method;

define method \+ (i :: <integer>, ptr :: <C-pointer>)
  make(object-class(ptr), pointer: pointer-pointer(ptr) + i)
end method;

define method \- (ptr :: <C-pointer>, i :: <integer>)
  make(object-class(ptr), pointer: pointer-pointer(ptr) - i)
end method;

define method \- (ptr1 :: <C-pointer>, ptr2 :: <C-pointer>)
  pointer-pointer(ptr1) - pointer-pointer(ptr2)
end method;

define method \= (ptr1 :: <C-pointer>, ptr2 :: <C-pointer>)
  pointer-pointer(ptr1) == pointer-pointer(ptr2)
end method;

// Pointer conversion:

define method as (ptr-class :: subclass(<C-pointer>), ptr :: <C-pointer>)
  make(ptr-class, pointer: pointer-pointer(ptr))
end method;

//// The <C-typed-pointer> class

// Structure:

define class <C-typed-pointer> (<C-pointer>) end class;

// Protocol:

define generic referenced-type (ptr-class);

define generic pointer-value 
    (ptr :: <C-typed-pointer>, #key index);
define generic pointer-value-setter
    (new-value, ptr :: <C-typed-pointer>, #key index);

// Implementation:

// Dog to optimise. Must think harder. Maybe local overrides don't actually
// make sense.

define method pointer-value 
    (ptr :: <C-typed-pointer>, #key index = 0, map = #f)
  let element-count = pointer-element-count(ptr);
  if (element-count & (element-count <= index | index < 0))
    error("Index out of range in pointer-value on %=, %=", ptr, index)
  end;
  let ptr-class = object-class(ptr);
  let contents-type = referenced-type(ptr-class);
  let map = map | class-for-map(contents-type);
  let low-level-value =
    low-level-value-at(contents-type,
                       pointer-pointer(ptr) + index * size-of(contents-type));
  if (map) 
    import-value(map, contents-type, low-level-value)
  else
    low-level-value
  end
end method;

define method pointer-value-setter
    (value, ptr :: <C-typed-pointer>, #key index = 0)
  let contents-type = referenced-type(object-class(ptr));
  let map = class-for-map(contents-type);
  let low-level-value = 
    if (map)
      export-value(contents-type, value)
    else
      value
    end;
  low-level-value-at(contents-type,
                     pointer-pointer(ptr) + index * size-of(contents-type))
    := low-level-value;
end method;

define method element
    (ptr :: <C-typed-pointer>, i :: <integer>, #key default)
  pointer-value(ptr, index: i)
end method;

define method element-setter 
    (new-value, ptr :: <C-typed-pointer>, i :: <integer>)
  pointer-value(ptr, index: i) := new-value
end method;

// Allocation: 
//
//   Is this a problem with the Dylan initialisation protocol?
//   The pointer slot isn't initialized until after initialize
//   has been called. Do it in initialize instead?

define method initialize
    (wrapper :: <C-typed-pointer>, 
       #key allocator = default-allocator(object-class(wrapper)), 
            element-count = 1, extra-bytes = 0)
  unless (pointer-pointer(wrapper))
    let space = 
      (size-of(referenced-type(object-class(wrapper))) + extra-bytes)
        * element-count;
    pointer-pointer(wrapper) := allocator(space);
  end;
  wrapper
end method;


//// The <C-untyped-pointer> class

define class <C-untyped-pointer> (<C-pointer>) end class;

define method initialize
    (wrapper :: <C-untyped-pointer>, 
       #key allocator = default-allocator(object-class(wrapper)), bytes = 1)
  unless (pointer-pointer(wrapper))
    pointer-pointer(wrapper) := allocator(bytes);
  end;
  wrapper
end method;

define method low-level-value-at 
    (c :: subclass(<C-pointer>), addr)
  make(c, pointer: pointer-at(addr))
end method;

define method low-level-value-at-setter 
    (val :: <C-pointer>, c :: subclass(<C-pointer>), addr)
  pointer-at(addr) := pointer-pointer(val)
end method;

define method abstract-supertype (c :: subclass(<C-pointer>))
  <C-pointer>
end method;

define method abstract-supertype (c :: subclass(<C-typed-pointer>))
  <C-typed-pointer>
end method;

define method copy-wrapper-properties (to, from :: subclass(<C-pointer>))
  next-method();
  referenced-type(to) := referenced-type(from);
end method;

define method canonical-type (class :: subclass(<C-pointer>))
  pointer-type(canonical-type(referenced-type(class)))
end method;

define method copy-supers-properties (new, #rest supers)
  for (super in supers)
    when (subtype?(super, <C-value>))
      copy-wrapper-properties(new, super)
    end
  end
end method;

// LW hack!!!

define lw-type <C-pointer>        = unsigned-long: end;

define method lw-export (v :: <C-pointer>)
  pointer-pointer(v)
end method;

define method lw-import (class :: subclass(<C-pointer>), v :: <integer>)
  make(class, pointer: v)
end method;

// eof
