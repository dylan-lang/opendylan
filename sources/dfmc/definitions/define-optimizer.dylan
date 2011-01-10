Module: dfmc-definitions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// define &optimizer-function:
//
//   define &optimizer-function values (env, arguments) => (dfm-stuff)
//     [...]
//   end &optimizer-function;
//
//   define &optimizer-function values = values-optimizer;

define macro &optimizer-function-definer
  { define &optimizer-function ?:name = ?optimizer:name end }
    => { do-define-optimizer-override(?#"name", ?optimizer); };
  { define &optimizer-function ?:name ?etc:* end }
    => { define method ?name ## "-optimizer" ?etc end;
         do-define-optimizer-override
           (?#"name", ?name ## "-optimizer"); }
end macro;

define constant *optimizer-overrides* = make(<table>);

define method do-define-optimizer-override (name, function) => ()
  *optimizer-overrides*[name] := function;
end;

// TODO: Note that optimizers could be looked up and cached at definition
// time of the function to which they're attached so that the lookup
// doesn't have to be done repeatedly.

define method lookup-optimizer-function 
    (model-or-name) => (function :: false-or(<function>))
  lookup-override-function(*optimizer-overrides*, model-or-name)
end;

// Example of a values optimizer.

/*
define &optimizer-function values (env, arguments) => (stuff)
  format-out("Optimizing values in %= for %=\n", env, arguments);
end &optimizer-function;
*/
