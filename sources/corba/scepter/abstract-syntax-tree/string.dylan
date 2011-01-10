Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-string> (<ast-predefined-type>)
  inherited slot predefined-type = $string-idl-type;
  constant slot string-max-size :: false-or(<ast-expression>) = #f, init-keyword: size:;
  constant slot string-local-name :: <string> = "string";
end class;

define class <ast-wstring> (<ast-string>)
  inherited slot predefined-type = $wstring-idl-type;
  inherited slot string-local-name = "wstring";
end class;

define method initialize (string :: <ast-string>, #key)
  next-method();
  string.declarator-scoped-name := add!(string.declarator-scoped-name, string.string-local-name);
end method;

define method can-be-redefined? (string :: <ast-string>)
  #t;
end method;

define method add-type (string :: <ast-string>)
  add-declarator-to-scope(scepter-root(get-scepter()), string);
end method;

