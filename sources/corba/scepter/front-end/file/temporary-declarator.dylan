Module: scepter-file-front-end
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <fe-declarator> (<object>)
end class;

define class <fe-simple-declarator> (<fe-declarator>)
  slot declarator-scoped-name :: <ast-scoped-name>, init-keyword: scoped-name:;
end class;

define method initialize (fe-declarator :: <fe-simple-declarator>, #rest args, #key local-name)
  next-method();
  if (local-name)
    fe-declarator.declarator-scoped-name := add!(make(<ast-scoped-name>), local-name);
  end if;
end method;

define class <fe-complex-declarator> (<fe-simple-declarator>)
  constant slot declarator-complex-part :: <ast-declarator>, init-keyword: complex-part:;
end class;

define method compose-declarator-type (fe-declarator :: <fe-declarator>, declarator :: <ast-declarator>)
  error(make(<idl-illegal-type>, declarator: declarator));
end method;

define method compose-declarator-type (fe-declarator :: <fe-simple-declarator>, type :: <ast-type>)
  type;
end method;

define method compose-declarator-type (fe-declarator :: <fe-complex-declarator>, type :: <ast-type>)
  compose-complex-declarator-type(fe-declarator, fe-declarator.declarator-complex-part, type);
end method;

define method compose-complex-declarator-type (fe-declarator :: <fe-complex-declarator>, array :: <ast-array>, type :: <ast-type>)
  array.array-base-type := type;
  let scepter = get-scepter();
  add-declarator(scepter.scepter-root, array);
  array;
end method;


