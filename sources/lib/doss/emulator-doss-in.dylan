Module:    emulator-doss
Author:    Eliot Miranda
Synopsis:  Emulator DOSS loader patches
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define class <emulator-doss-loader> (<doss-loader>)
end class <emulator-doss-loader>;

define method make
    (class == <doss-loader>, #rest args, #key stream, #all-keys)
 => (dl :: <emulator-doss-loader>)
  apply(make, <emulator-doss-loader>, args)
end method make;
*/

define constant <emulator-doss-loader> = <doss-loader>;

/*
// c.f. dump-variable
define method get-variable
    (dl :: <emulator-doss-loader>) => (obj :: <object>)
  let id      = dl.get-next-int;
  let name    = dl.get-next-object;
  let module  = dl.get-next-object;
  let library = dl.get-next-object;
  // my-format("get-variable %d %= %= %=\n", id, name, module, library);
  restore(if (name)
            variable-value(name, module, library)
          else
            find-translator-module(module)
          end, 
          id, dl)
end method get-variable;
*/

/*
// c.f. dump-variable
define method get-anonymous-variable 
    (dl :: <emulator-doss-loader>) => (obj :: <object>)
  let name    = dl.get-next-object;
  let module  = dl.get-next-object;
  let library = dl.get-next-object;
  // my-format("get-anonymous-variable %= %= %=\n", name, module, library);
  variable-value(name, module, library)
end method get-anonymous-variable;
*/

define method doss-allocate (class == <simple-object-vector>, size)
  make(<simple-object-vector>, size: size)
end method doss-allocate;

define method doss-allocate (class == <byte-vector>, size)
  make(<byte-vector>, size: size)
end method doss-allocate;

define method doss-allocate (class == <stretchy-vector>, size)
  make(<stretchy-vector>, size: size)
end method doss-allocate;

define method doss-allocate (class == <object-table>, size)
  make(<object-table>, size: 0)
end method doss-allocate;

define method doss-allocate (class :: <class>, size)
  allocate(class, size: size)
end method doss-allocate;

// c.f. store-and-traverse
define method get-object-definition
    (dl :: <emulator-doss-loader>) => (obj :: <object>)
  let id = dl.get-next-int;
  let class = dl.get-next-object;
  let rpt = if (repeated-slots-stored(class, dl))
              dl.get-next-int
            end;
  // my-format("get obj def id:%= c:%= rpt:%=\n", id, class, rpt);
  let obj = doss-allocate(class, rpt);
  let unbound = dl.unbound-proxy;
  restore(obj, id, dl);
  local method fixer (val, obj, setter-func) setter-func(val, obj) end;
  let setters = dl.class-slot-info[class];
  for (i from 0 below size(setters), setter-func in setters)
    unless (i = 0)
      let next-obj = dl.get-next-object;
      add-fixup!(next-obj, fixer, obj, setter-func);
      if (unbound ~== next-obj)
        /* doss-slot-value(obj, setter-func) := next-obj; */
        setter-func(next-obj, obj)
      end
    end
  end;
  local method rpt-fixer (val, obj, index) obj[index] := val end;
  if (rpt)
    for (index from 0 below rpt)
      /* doss-repeated-slot-element(obj, index) := dl.get-next-object; */
      let next-obj = dl.get-next-object;
      add-fixup!(next-obj, rpt-fixer, obj, index);
      obj[index] := next-obj
    end
  end;
  obj
end method get-object-definition;

define method get-keyword (dl :: <doss-loader>) => (k :: <keyword>)
  as-keyword(dl.get-string)
end method get-keyword;
