Module: emulator-patches-1
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Lisp interface definition macro.

// This becomes globally visible because of the emulator's handling of the
// macro namespace.

define macro lisp-interface-definer
  { define lisp-interface ?imports end }
    => { ?imports }
imports:
  { } => { }
  { ?import; ... } => { ?import; ... }
import:
  { functions ?import-specs }
    => { import-cl-functions(?import-specs) }
  { values ?import-specs }
    => { import-cl-values(?import-specs) }
  { classes ?import-specs }
    => { import-cl-classes(?import-specs) }
import-specs:
  { } => { }
  { ?import-spec, ... } => { ?import-spec, ... }
import-spec:
  { ?symbol:name from ?package:name as ?alias:name }
    => { ?package(?symbol) (as: ?alias) }
  { ?symbol:name from ?package:name }
    => { ?package(?symbol) (as: ?symbol) }
  { ?symbol:name as ?alias:name }
    => { ?symbol (as: ?alias) }
  { ?symbol:name }
    => { ?symbol (as: ?symbol) }
end macro;

//// The define domain macro.

// Again, globally visible because of the emulator.
// We ensure the name and types are at least evaluable.

define macro domain-definer 
  { define ?mods:* domain ?:name (?types:*) }
    => { begin ?name; ?types end; values() } 
types:
  { }
    => { }
  { ?:expression, ... }
    => { ?expression; ... }
end macro;

//// The define function macro.

define macro function-definer
  { define ?modifiers:* function ?:name ?signature-and-body:* end }
  => { define ?modifiers constant ?name = method ?signature-and-body end }
end macro function-definer;

// eof
