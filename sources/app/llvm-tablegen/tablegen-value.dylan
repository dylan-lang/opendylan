Module:       llvm-tablegen
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <tablegen-value> (<object>)
end class;

define method resolve-value
    (environment :: <string-table>, value :: <tablegen-value>)
 => (value :: <tablegen-value>);
  value
end method;

// Unspecified/unknown value
define class <tablegen-unknown-value> (<tablegen-value>)
end class;

define sealed method print-message
    (value :: <tablegen-unknown-value>, stream :: <stream>)
    => ();
  write-element(stream, '?');
end method;

// Directly represented value
define class <tablegen-simple-value> (<tablegen-value>)
  constant slot simple-value :: type-union(<string>, <integer>),
    required-init-keyword: value:;
end class;

define sealed method print-message
    (value :: <tablegen-simple-value>, stream :: <stream>)
    => ();
  print-object(value.simple-value, stream);
end;

// Named reference to a parameter or definition
define class <tablegen-named-value> (<tablegen-value>)
  constant slot named-value-name :: <string>,
    required-init-keyword: name:;
end class;

define method resolve-value
    (environment :: <string-table>, value :: <tablegen-named-value>)
 => (value :: <tablegen-value>);
  let resolved = element(environment, value.named-value-name, default: #f);
  if (resolved)
    resolve-value(environment, resolved)
  else
    value
  end
end method;

define sealed method print-message
    (value :: <tablegen-named-value>, stream :: <stream>)
    => ();
  write-text(stream, value.named-value-name);
end method;

// List value
define class <tablegen-list-value> (<tablegen-value>)
  constant slot list-value :: <sequence>,
    required-init-keyword: list:;
end class;

define method resolve-value
    (environment :: <string-table>, value :: <tablegen-list-value>)
 => (value :: <tablegen-value>);
  make(<tablegen-list-value>,
       list: map(curry(resolve-value, environment), value.list-value))
end method;

define sealed method print-message
    (value :: <tablegen-list-value>, stream :: <stream>)
    => ();
  write-element(stream, '[');
  for (item in value.list-value, first? = #t then #f)
    unless (first?) write(stream, ", ") end;
    print-message(item, stream);
  end for;
  write-element(stream, ']');
end method;

// Special operator value
define class <tablegen-operator-value> (<tablegen-value>)
  constant slot operator-kind :: <symbol>,
    required-init-keyword: kind:;
  constant slot operator-operands :: <sequence>,
    required-init-keyword: operands:;
end class;

define method resolve-value
    (environment :: <string-table>, value :: <tablegen-operator-value>)
 => (value :: <tablegen-value>);
  let resolved-operands
    = map(curry(resolve-value, environment), value.operator-operands);
  if (every?(rcurry(instance?, <tablegen-simple-value>), resolved-operands))
    select (value.operator-kind)
      #"strconcat" =>
        make(<tablegen-simple-value>,
             value: apply(concatenate, map(simple-value, resolved-operands)));
      otherwise =>
        error("can't resolve operator !%s", value.operator-kind);
    end select
  else
    make(<tablegen-operator-value>,
         kind: value.operator-kind,
         operands: resolved-operands)
  end if
end method;

define sealed method print-message
    (value :: <tablegen-operator-value>, stream :: <stream>)
    => ();
  format(stream, "!%s(", value.operator-kind);
  for (item in value.operator-operands, first? = #t then #f)
    unless (first?) write(stream, ", ") end;
    print-message(item, stream);
  end for;
  write-element(stream, ')');
end method;
