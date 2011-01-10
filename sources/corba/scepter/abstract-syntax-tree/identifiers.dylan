Module:    scepter-ast
Author:    Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <ast-identifier> (<object>)
  constant slot identifier-label :: <string>, required-init-keyword: label:;
end class;

define method as (class == <symbol>, identifier :: <ast-identifier>)
  => (object :: <symbol>)
  as(<symbol>, identifier.identifier-label);
end method;

define method \= (id1 :: <ast-identifier>, id2 :: <ast-identifier>)
  => (result :: <boolean>)
  if (scepter-case-sensitive-reserved-words?(get-scepter()))
    id1.identifier-label = id2.identifier-label;
  else
    case-insensitive-equal(id1.identifier-label, id2.identifier-label);
  end if;
end method;

define method identifier-hash (identifier :: <ast-identifier>, old-state)
 => (id, new-state)
  if (scepter-case-sensitive-reserved-words?(get-scepter()))
    string-hash(identifier.identifier-label, old-state);
  else
    case-insensitive-string-hash(identifier.identifier-label, old-state);
  end if;
end method;

define constant <ast-scoped-name> = <stretchy-vector> /*---***limited(<stretchy-vector>, of: <ast-identifier>)***---*/ ;

define method scoped-name-as-string (name :: <ast-scoped-name>)
 => (name :: <string>)
  let string-name = "";
  for (i from 0 below name.size)
    string-name := concatenate!(string-name, "::", identifier-label(name[i]));
  end for;
  string-name;
end method;

define class <identifier-table> (<table>)
end class;

define method table-protocol (table :: <identifier-table>)
  => (test-fn :: <function>, hash-fn :: <function>)
  values(\=, identifier-hash);
end method;

// GLOBAL-NAME?

define constant $double-colon-identifier :: <ast-identifier> =
  make(<ast-identifier>, label: "::");

define constant $empty-identifier :: <ast-identifier> =
  make(<ast-identifier>, label: "");

define method global-name? (identifier :: <ast-identifier>)
  identifier = $double-colon-identifier | 
  identifier = $empty-identifier;
end method;
  
