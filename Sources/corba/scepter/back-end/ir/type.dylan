Module:    scepter-ir-back-end-internal
Author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic get-primitive-type (repository :: corba/<Repository>, type :: <idl-type>)
 => (def :: corba/<PrimitiveDef>);

define method load-definition (back-end :: <ir-back-end>, type :: <ast-sequence>, #key container)
 => (def :: corba/<SequenceDef>)
  let expr = type.sequence-max-size;
  let bound = if (expr) expr.expression-value else 0 end;
  assert(instance?(bound, <integer>), "Sequence bound expression, %s, is not an integer", expr);
  let element-type = load-definition(back-end, type.sequence-base-type);
  corba/Repository/create-sequence(back-end.ir-back-end-repository, bound, element-type);
end method;

define method load-definition (back-end :: <ir-back-end>, type :: <ast-array>, #key container)
 => (def :: corba/<ArrayDef>)
  let dimensions = type.array-dimensions;
  let element-type = load-definition(back-end, type.array-base-type);
  for (i from (dimensions.size - 1) to 0 by -1)
    let length = expression-value(dimensions[i]);
    element-type := corba/Repository/create-array(back-end.ir-back-end-repository, length, element-type);
  end for;
  element-type;
end method;

//define method load-definition (back-end :: <ir-back-end>, type :: <ast-fixed>, #key container)
// => (def :: corba/<FixedDef>)
//end method;

define method load-definition (back-end :: <ir-back-end>, type :: <ast-predefined-type>, #key container)
 => (def :: corba/<PrimitiveDef>)
  let idl-type = type.predefined-type;
  assert(instance?(idl-type, <idl-type>));
  get-primitive-type(back-end.ir-back-end-repository, idl-type);
end method;

define method load-definition (back-end :: <ir-back-end>, type :: <ast-string>, #key container)
 => (def :: corba/<IDLType>)
  let repository = back-end.ir-back-end-repository;
  let expr = type.string-max-size;
  if (expr)
    let bound = expression-value(expr);
    assert(instance?(bound, <integer>));
    assert(bound > 0);
    corba/Repository/create-string(repository, bound);
  else
    get-primitive-type(repository, $string-idl-type);
  end if;
end method;

define method load-definition (back-end :: <ir-back-end>, type :: <ast-wstring>, #key container)
 => (def :: corba/<IDLType>)
  let repository = back-end.ir-back-end-repository;
  let expr = type.string-max-size;
  if (expr)
    let bound = expression-value(expr);
    assert(instance?(bound, <integer>));
    assert(bound > 0);
    corba/Repository/create-wstring(repository, bound);
  else
    get-primitive-type(repository, $wstring-idl-type);
  end if;
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <void-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-void");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <short-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-short");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <long-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-long");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <ushort-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-ushort");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <ulong-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-ulong");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <float-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-float");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <double-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-double");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <boolean-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-boolean");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <char-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-char");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <octet-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-octet");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <any-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-any");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <TypeCode-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-TypeCode");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <Principal-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-Principal");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <string-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-string");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <Object-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-objref");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <longlong-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-longlong");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <ulonglong-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-ulonglong");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <longdouble-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-longdouble");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <wchar-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-wchar");
end method;

define method get-primitive-type (repository :: corba/<Repository>, type :: <wstring-idl-type>)
 => (def :: corba/<PrimitiveDef>)
  corba/repository/get-primitive(repository, #"pk-wstring");
end method;

define method emit-declarator? (back-end :: <ir-back-end>, declarator :: <ast-predefined-type>)
 => (emit? :: <boolean>)
  let idl-type = declarator.predefined-type;
  emit-idl-type?(back-end, idl-type);
end method;

define method emit-idl-type? (back-end :: <ir-back-end>, type :: <idl-type>)
 => (emit? :: <boolean>)
  #t;
end method;

define method emit-idl-type? (back-end :: <ir-back-end>, type :: <pseudo-idl-type>)
 => (emit? :: <boolean>)
  #f;
end method;

define method emit-idl-type? (back-end :: <ir-back-end>, type :: <Object-idl-type>)
 => (emit? :: <boolean>)
  #t;
end method;

define method emit-idl-type? (back-end :: <ir-back-end>, type :: <TypeCode-idl-type>)
 => (emit? :: <boolean>)
  #t;
end method;

define method emit-idl-type? (back-end :: <ir-back-end>, type :: <Principal-idl-type>)
 => (emit? :: <boolean>)
  #t;
end method;

define method emit-idl-type? (back-end :: <ir-back-end>, type :: <NamedValue-idl-type>)
 => (emit? :: <boolean>)
  #t;
end method;

