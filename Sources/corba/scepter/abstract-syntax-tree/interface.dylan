Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-interface> (<ast-type>, <scope>)
  slot interface-inherits :: <stretchy-vector> = make(<stretchy-vector>), init-keyword: inherits:;
  slot interface-defined? :: <boolean> = #t, init-keyword: defined?:;
end class;

define method can-be-redefined? (interface :: <ast-interface>)
  #t;
end method;

define method can-be-redefined-after-use? (scope :: <scope>, interface :: <ast-interface>)
  interface.declarator-scope == scope &
  ~ interface.interface-defined?
end method;

define method check-before-add (interface :: <ast-interface>, declarator :: <ast-declarator>)
  if (~ interface.interface-defined?)
    error(make(<idl-declarator-not-defined>, declarator: interface));
  else
    next-method();
  end if;
end method;

