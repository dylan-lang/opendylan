Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The <C-struct> class

define open abstract class <C-struct> (<C-value>) end;

define subclass-slot struct-fields = #f of <C-struct> end;

// Implicitly coerce an immediate struct value up to a pointer to that
// struct on access.
// Should you get a pointer of the canonical type (as here) or a pointer
// of the same type as is being dereferenced? Probably the latter.

define method low-level-value-at (c :: subclass(<C-struct>), addr)
  make(c.pointer-type, pointer: addr)
end method;

// Structs are canonical bases:

define method canonical-type (wrapper :: subclass(<C-struct>))
  wrapper
end method;

// I guess assigning into a structure involves copying bytes? This is
// certainly odd, especially since that's not what accessing does. Perhaps
// accessing shouldn't work like it does above at all... 
// Punt for now.

define class <field-descriptor> (<object>)
  slot getter,
    init-keyword: getter:;
  slot zetter,
    init-keyword: setter:;
  slot type,
    init-keyword: type:;
  slot mapping,
    init-value:   #f,
    init-keyword: map:;
end class;

define method initialize-C-struct (struct, fields)
  let (offsets, struct-size, struct-alignment)
    = compute-struct-layout(map(type, fields));
  struct-fields(struct) := fields;
  size-of(struct) := struct-size;
  alignment-of(struct) := struct-alignment;
  for (field in fields, offset in offsets)
    let field-type* = field.type.pointer-type;
    let field-map = field.mapping;
    add-method(field.getter,
               if (field-map)
                 method (ptr :: struct.pointer-type, #next next-method)
                   let addr = pointer-pointer(ptr);
                   pointer-value(make(field-type*, pointer: addr + offset), 
                                 map: field-map)
                 end
               else
                 method (ptr :: struct.pointer-type, #next next-method)
                   let addr = pointer-pointer(ptr);
                   pointer-value(make(field-type*, pointer: addr + offset)) 
                 end
               end);
    add-method(field.zetter,
               method (val, ptr :: struct.pointer-type, #next next-method)
                 let addr = pointer-pointer(ptr);
                 pointer-value(make(field-type*, pointer: addr + offset)) := val
               end);
  end;
end method;

// Structure layout

define method align-offset (offset :: <integer>, alignment :: <integer>)
  ceiling/(offset, alignment) * alignment;
end method;

define method compute-field-offset
    (struct-type, field-type, 
     current-offset :: <integer>, current-alignment :: <integer>)
  let field-alignment = alignment-of(field-type);
  let field-size      = size-of(field-type);
  let field-offset    = align-offset(current-offset, field-alignment);
  let new-alignment   = max(current-alignment, field-alignment);
  let new-offset      = field-offset + field-size;
  values(field-offset, new-offset, new-alignment)
end method;

define method compute-struct-layout (fields)
  let offsets = make(<deque>);
  let current-alignment = 1;
  let current-offset    = 0;
  for (field in fields)
    let (field-offset, new-offset, new-alignment) =
      compute-field-offset(#f, field, current-offset, current-alignment);
    push-last(offsets, field-offset);
    current-alignment := new-alignment;
    current-offset    := new-offset;
  end;
  values(as(<list>, offsets), 
         align-offset(current-offset, current-alignment), // size
         current-alignment)
end method;

// eof
