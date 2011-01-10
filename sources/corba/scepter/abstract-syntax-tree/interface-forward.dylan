Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-forward-interface> (<ast-type>)
  slot full-definition :: false-or(<ast-interface>) = #f, init-keyword: full-definition:;
end class;

define variable *ast-forward-references* = make(<stretchy-vector>);

define method initialize (forward-interface :: <ast-forward-interface>, #rest args, #key, #all-keys)
  next-method();
  forward-interface.full-definition := apply(make, <ast-interface>, defined?: #f, args);
  *ast-forward-references* := add!(*ast-forward-references*, forward-interface);
end method;

define method full-definition (declarator :: <ast-declarator>)
  declarator;
end method;

define method can-be-redefined? (interface :: <ast-forward-interface>)
  #t;
end method;

// unused
//
// define method check-forward-interfaces ()
//   for (forward-reference in *ast-forward-references*)
//     let full-definition = forward-reference.full-definition;
//     unless (full-definition & full-definition.interface-defined?)
//       error(make(<idl-declarator-not-defined>, declarators: vector(forward-reference)));
//     end unless;
//   end for;
// end method;


