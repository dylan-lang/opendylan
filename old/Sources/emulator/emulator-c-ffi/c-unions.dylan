Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The <C-union> class

define open abstract class <C-union> (<C-value>) end;

define subclass-slot union-fields = #f of <C-union> end;

define method low-level-value-at (c :: subclass(<C-union>), addr)
  make(c.pointer-type, pointer: addr)
end method;

define method initialize-C-union (union, fields)
  let (union-size, union-alignment)
    = compute-union-layout(map(type, fields));
  union-fields(union) := fields;
  size-of(union) := union-size;
  alignment-of(union) := union-alignment;
  for (field in fields)
    let field-type* = field.type.pointer-type;
    let field-map = field.mapping;
    add-method(field.getter,
               if (field-map)
                 method (ptr :: union.pointer-type, #next next-method)
                   let addr = pointer-pointer(ptr);
                   pointer-value(make(field-type*, pointer: addr),
                                 map: field-map)
                 end
               else
                 method (ptr :: union.pointer-type, #next next-method)
                   let addr = pointer-pointer(ptr);
                   pointer-value(make(field-type*, pointer: addr)) 
                 end
               end);
    add-method(field.zetter,
               method (val, ptr :: union.pointer-type, #next next-method)
                 let addr = pointer-pointer(ptr);
                 pointer-value(make(field-type*, pointer: addr))
                   := val
               end);
  end;
end method;

define method compute-union-layout (fields)
  let current-alignment = 1;
  let current-offset    = 0;
  for (field in fields)
    let (field-offset, new-offset, new-alignment) =
      compute-field-offset(#f, field, 0, current-alignment);
    current-alignment := new-alignment;
    current-offset    := new-offset;
  end;
  values(align-offset(current-offset, current-alignment), // size
         current-alignment)
end method;

// eof
