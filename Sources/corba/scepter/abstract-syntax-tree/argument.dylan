Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <argument-direction> (<object>)
  constant slot argument-direction-name :: <string>, required-init-keyword: name:;
end class;

define constant $in-argument-direction = make(<argument-direction>, name: "in");
define constant $out-argument-direction = make(<argument-direction>, name: "out");
define constant $inout-argument-direction = make(<argument-direction>, name: "inout");

define class <ast-argument> (<ast-field>)
  constant slot argument-direction :: <argument-direction> = $in-argument-direction, init-keyword: direction:;
end class;

define method can-be-redefined? (argument :: <ast-argument>)
  #t;
end method;

define method add-declarator-to-scope (scope :: <scope>, argument :: <ast-argument>)
  next-method();
  unless (argument.field-type.declarator-added?)
    add-type(argument.field-type)
  end unless;
end method;

