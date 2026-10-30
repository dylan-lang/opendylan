Module:    c-ffi-test-suite
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// -------------
// tests for <c-dylan-object>

define c-struct <test-dylan-object>
  slot object-handle :: <c-dylan-object>;
  pointer-type-name: <test-dylan-object*>;
end;

define test c-dylan-object-test ()
  let obj = pair(1, 2);         // make an object
  register-c-dylan-object(obj); // register it
  let struct = make(<test-dylan-object*>);
  let handle = export-c-dylan-object(obj);
  object-handle(struct) := handle;
  check-equal("<C-dylan-object> importing handle",
             import-c-dylan-object(handle), obj);
  check-equal("<C-dylan-object> extracting handle from struct",
             import-c-dylan-object(object-handle(struct)),
             obj);
  // do more here.
  // ...
  unregister-c-dylan-object(obj);
  destroy(struct);
end;
