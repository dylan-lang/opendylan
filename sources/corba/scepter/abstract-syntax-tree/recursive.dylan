Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// ILLEGAL-RECURSIVE-TYPE?

define method illegal-recursive-type? (object :: <object>)
  => (result :: <boolean>)
  #f;
end method;

define method illegal-recursive-type? (struct :: <ast-structure>)
  => (result :: <boolean>)
  do-illegal-recursive-type?(struct);
end method;
  
define method illegal-recursive-type? (union :: <ast-union>)
  => (result :: <boolean>)
  do-illegal-recursive-type?(union);
end method;
  
// DO-ILLEGAL-RECURSIVE-TYPE?

define method do-illegal-recursive-type? (declarator :: <ast-declarator>)
  block (return)
    for (scope in scepter-scopes(get-scepter()))
      case
        (scope.sequence-allows-recursive-types?) => return(#f);
        (scope == declarator) => return(#t);
      end case;
    end for;
  end block;
end method;
