Module: metering
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*

  Interface:

  define metering-set *optimizers*
    modules 
      dfmc-reader, dfmc-common;
    functions
      optimize-lambdas in dfmc-optimization,
      optimize-dispatch in dfmc-optimization;
  end metering-set;

  define metering-set *compiler*
    include *drivers*;
    include *optimizers*;
    include *back-ends*;
  end metering-set;

  with-metering (*compiler*, foo: #t, bar: #f)
    ...
  end;

*/

define macro metering-set-definer
  { define metering-set ?:name ?specs:* end }
    => { define constant ?name = make-metering-set(?specs) }
specs:
  { }
    => { }
  { ?spec; ... }
    => { ?spec, ... }
spec:
  { include ?set:name }
    => { ?set }
  { modules ?modules:* }
    => { ?modules }
  { functions ?functions:* }
    => { ?functions }
modules:
  { }
    => { }
  { ?module:*, ... }
    => { ?module, ... }
module:
  { ?:name }
    => { emulator-module-package(?name) }
functions:
  { }
    => { }
  { ?function, ... }
    => { ?function, ... }
function:
  { ?:name }
    => { emulator-local-binding(?name) }
  { ?:name in ?function-module:name }
    => { emulator-binding(?name, ?function-module) }
  { ?:name in-package ?function-package:name }
    => { emulator-symbol(?name, ?function-package) }
end macro;

// The places list may be nested at most one level deep. Order of the
// result doesn't matter.

define function make-metering-set (#rest places)
  local method conc-or-cons (acc, item)
    if (instance?(item, <list>))
      concatenate(acc, item)
    else
      pair(item, acc)
    end;
  end;
  reduce(conc-or-cons, #(), places);
end function;

define lisp-interface
  functions do-with-metering from dylan;
end lisp-interface;

define macro with-metering 
  { with-metering (?set:expression, #rest ?options) ?:body end }
    => { do-with-metering(?set, method () ?body end, ?options) }
end macro;

// eof
