module:   sealing-workbench
author:   Paul Haahr
synopsis: Fake libraries for enforcing sealing constraints.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

////
//// libraries
////

define class <library> (<object>)
  slot name :: <symbol>, required-init-keyword: name:;
  // slot uses :: (sequence of libraries);
  slot definitions :: <table>, init-function: curry(make, <table>);
  slot sealed-domains :: <sequence>, init-value: #();
end class <library>;

// define constant *library-table* = make(<object-table>);

define method library (name :: <symbol>) => (library :: <library>)
  // let library = element(*library-table*, name, default: #f);
  // library | (*library-table*[name] := make(<library>, name: name))
  make(<library>, name: name)
end method library;

define variable *library* = library(#"user");

define constant $dylan-library = library(#"dylan");

/*
  let (initial, limit, next, finished?, key, element)
    = library.definitions.forward-iteration-protocol;
  for (state = initial then next(library.definitions, state),
       until: finished?(library.definitions, state, limit))
*/

define method finalize-definition (library :: <library>)
  for (object in library.definitions)
    finalize-definition(object)
  end for;
  for (domain in library.sealed-domains)
    finalize-definition(domain)
  end for
end method finalize-definition;

define method finalize-definition (object)
  if (*verbose?*)
    format-out("finalizing definition of %=\n", object)
  end if
end method finalize-definition;
