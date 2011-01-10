Module:    ir-browser
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method info-table-items (def :: <object>)
 => (items :: <list>)
  #();
end method;

define method info-table-items (def :: CORBA/<IRObject>)
 => (items :: <list>)
  let items = next-method();
  let kind = as(<string>, CORBA/IRObject/def-kind(def));
  pair(list("Kind", kind), items);
end method;

define method info-table-items (def :: CORBA/<Contained>)
 => (items :: <list>)
  let items = next-method();
  let id = CORBA/Contained/id(def);
  let name = CORBA/Contained/name(def);
  let version = CORBA/Contained/version(def);
  let defined-in = CORBA/Contained/defined-in(def);
  let absolute-name = CORBA/Contained/absolute-name(def);
  concatenate(items,
              list(list("ID", id),
                   list("Name", name),
                   list("Version", version),
                   list("Defined in", defined-in),
                   list("Absolute Name", absolute-name)));
end method;

define method info-table-items (def :: CORBA/<IDLType>)
 => (items :: <list>)
  let items = next-method();
  let type = CORBA/IDLType/type(def);
  pair(list("Type", type), items);
end method;

define method info-table-items (def :: CORBA/<ConstantDef>)
 => (items :: <list>)
  let items = next-method();
  let type = CORBA/ConstantDef/type(def);
//  let type-def = CORBA/ConstantDef/type-def(def);
  let value = CORBA/ConstantDef/value(def);
  concatenate(items,
              list(list("Type", type),
                   list("Value", value)));
end method;

define method info-table-items (def :: CORBA/<ExceptionDef>)
 => (items :: <list>)
  let items = next-method();
  let type = CORBA/ExceptionDef/type(def);
//  let members = CORBA/ExceptionDef/members(def);
  pair(list("Type", type), items);
end method;

define method info-table-items (def :: CORBA/<AttributeDef>)
 => (items :: <list>)
  let items = next-method();
  let type = CORBA/AttributeDef/type(def);
  let mode = as(<string>, CORBA/AttributeDef/mode(def));
  concatenate(items,
              list(list("Type", type),
                   list("Mode", mode)));
end method;

define method info-table-items (def :: CORBA/<OperationDef>)
 => (items :: <list>)
  let items = next-method();
  let result = CORBA/OperationDef/result(def);
//  let result-def = CORBA/OperationDef/result-def(def);
//  let params = CORBA/OperationDef/params(def);
  let mode = as(<string>, CORBA/OperationDef/mode(def));
  concatenate(items,
              list(list("Result", result),
                   list("Mode", mode)));
end method;

/*
define method info-table-items (def :: corba/<InterfaceDef>)
 => (items :: <list>)
  let gen-des = def.corba/Contained/describe;
//  assert(gen-des.corba/Contained/Description/kind = #"dk-Interface");
  let description = as(corba/<InterfaceDescription>, gen-des.corba/Contained/Description/value);
  let name = description.corba/InterfaceDescription/name;
  let id = description.corba/InterfaceDescription/id;
  let defined-in = description.corba/InterfaceDescription/defined-in;
  let version = description.corba/InterfaceDescription/version;
  list(list("Name", name),
       list("Repository ID", id),
       list("Defined in", defined-in),
       list("Version", version));
end method;
*/

