Module:    scepter-ast
Author:    Jason Trenouth, Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2019 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-forward-structure> (<ast-type>)
  slot full-definition :: false-or(<ast-structure>) = #f,
    init-keyword: full-definition:;
end class;

define method initialize (forward-struct :: <ast-forward-structure>,
                          #rest args, #key, #all-keys)
  next-method();
  *ast-forward-references* := add!(*ast-forward-references*, forward-struct);
end method;

define method can-be-redefined? (struct :: <ast-forward-structure>)
  #t;
end method;
