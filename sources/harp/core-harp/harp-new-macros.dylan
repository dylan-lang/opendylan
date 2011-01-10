module:    base-harp
Synopsis:  General macro support for HARP
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// with-harp
///
/// A handy macro for binding tags and registers when writing HARP assembler.
///
/// Typical usage:
///
///   with-harp (be)
///     reg r1, r2;
///     named reg variable1, variable2;
///     tag t1;
///     ins--tag(be, t1);
///     ins--move(be, r1, r1);
///     ins--bge(be, t1, variable1, variable2);
///   end;

define macro with-harp

  { with-harp (?backend:expression)
      ?binding-body
    end }
    => 
    { begin
        let $backend$ = ?backend;
        let $registers$ = $backend$.registers;
        ?binding-body
      end }

  binding-body:
    { tag  ?tag-bindings ; ... }
      => { ?tag-bindings ; ... }

    { reg  ?greg-bindings ; ... }
      => { ?greg-bindings ; ... }
    { greg ?greg-bindings ; ... }
      => { ?greg-bindings ; ... }
    { nreg ?nreg-bindings ; ... }
      => { ?nreg-bindings ; ... }
    { sfreg ?sfreg-bindings ; ... }
      => { ?sfreg-bindings ; ... }
    { dfreg ?dfreg-bindings ; ... }
      => { ?dfreg-bindings ; ... }
    { tmp ?temp-bindings ; ... }
      => { ?temp-bindings ; ... }

    { named reg  ?named-greg-bindings ; ... }
      => { ?named-greg-bindings ; ... }
    { named greg ?named-greg-bindings ; ... }
      => { ?named-greg-bindings ; ... }
    { named nreg ?named-nreg-bindings ; ... }
      => { ?named-nreg-bindings ; ... }
    { named sfreg ?named-sfreg-bindings ; ... }
      => { ?named-sfreg-bindings ; ... }
    { named dfreg ?named-dfreg-bindings ; ... }
      => { ?named-dfreg-bindings ; ... }

    { ?reg:name ?:name ; ...}
      => {let ?name = $registers$ . "reg-" ## ?reg; ... }

    { ?:body }
      => { ?body }
  
  tag-bindings:
    { ?tag:name, ... }
      => { let ?tag = make-tag($backend$); ... }
    { } => { }

  greg-bindings:
    { ?reg:name, ... }
      => { let ?reg = make-g-register($backend$); ... }
    { } => { }

  nreg-bindings:
    { ?reg:name, ... }
      => { let ?reg = make-n-register($backend$); ... }
    { } => { }

  sfreg-bindings:
    { ?reg:name, ... }
      => { let ?reg = make-sf-register($backend$); ... }
    { } => { }

  dfreg-bindings:
    { ?reg:name, ... }
      => { let ?reg = make-df-register($backend$); ... }
    { } => { }

  temp-bindings:
    { ?n:expression, ?more-temp-bindings }
      => { let $temporary$ = ?n; ?more-temp-bindings }

  more-temp-bindings:
    { ?reg:name, ... }
      => { let ?reg = make-temp-register($backend$, $temporary$); ... }
    { } => { }

  named-greg-bindings:
    { ?reg:name, ... }
      => { let ?reg 
             = make-g-register($backend$, name: ?"reg" . as-lowercase); 
           ... }
    { } => { }

  named-nreg-bindings:
    { ?reg:name, ... }
      => { let ?reg 
             = make-n-register($backend$, name: ?"reg" . as-lowercase); 
           ... }
    { } => { }

  named-sfreg-bindings:
    { ?reg:name, ... }
      => { let ?reg 
             = make-sf-register($backend$, name: ?"reg" . as-lowercase); 
           ... }
    { } => { }

  named-dfreg-bindings:
    { ?reg:name, ... }
      => { let ?reg 
             = make-df-register($backend$, name: ?"reg" . as-lowercase); 
           ... }
    { } => { }

end macro;

