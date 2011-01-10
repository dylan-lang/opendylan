Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <tablegen-record> (<object>)
  constant slot record-name :: <string>,
    required-init-keyword: name:;
  constant slot record-superclasses :: <sequence>,
    init-value: #[], init-keyword: superclasses:;
  constant slot record-initializers :: <sequence>,
    init-value: #[], init-keyword: initializers:;
end class;

define class <tablegen-class> (<tablegen-record>)
  constant slot record-template-parameters :: <sequence>,
    init-value: #[], init-keyword: template-parameters:;
end class;

define class <tablegen-definition> (<tablegen-record>)
end class;

define class <tablegen-initializer> (<object>)
  constant slot initializer-name :: <string>,
    required-init-keyword: name:;
  constant slot initializer-type :: <tablegen-type>,
    required-init-keyword: type:;
  constant slot initializer-value :: <tablegen-value>,
    required-init-keyword: value:;
end class;

define constant $tablegen-classes :: <string-table>
  = make(<string-table>);

define constant $tablegen-definitions :: <string-table>
  = make(<string-table>);

define function tablegen-subclass?
    (record :: <tablegen-record>, class-name :: <string>)
 => (subclass? :: <boolean>);
  record.record-name = class-name
    | any?(rcurry(tablegen-subclass?, class-name), record.record-superclasses)
end function;

define function record-field-value
    (record :: <tablegen-record>, field-name :: <string>,
     #key default = unsupplied())
 => (value);
  block (return)
    for (initializer in record.record-initializers)
      if (initializer.initializer-name = field-name)
        let mapper = value-mapper(initializer.initializer-type);
        return(mapper(initializer.initializer-value));
      end if;
    end for;
    if (supplied?(default))
      default
    else
      error("%s record does not contain a %s field",
            record.record-name, field-name);
    end if
  end block
end function;

define method value-mapper
    (type :: <tablegen-simple-type>) => (mapper :: <function>);
  select (type.simple-type-kind)
    #"STRING", #"INT", #"BIT" =>
      simple-value;
    otherwise =>
      error("Can't map %s values", type.simple-type-kind);
  end select
end method;

define method value-mapper
    (type :: <tablegen-list-type>) => (mapper :: <function>);
  let field-mapper = value-mapper(type.list-type-of);
  method (value :: <tablegen-list-value>)
    map(field-mapper, value.list-value)
  end method
end method;

define method value-mapper
    (type :: <tablegen-class-type>) => (mapper :: <function>);
  // FIXME check subtype?
  method (value :: <tablegen-named-value>)
    $tablegen-definitions[value.named-value-name]
  end method
end method;
