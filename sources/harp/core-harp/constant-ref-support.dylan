module:    base-harp
Synopsis:  The <constant-reference> class, for symbolic constants.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Support functions for the <constant-reference> class (and related classes)


// INS--CONSTANT-REF a general way of making a constant reference for
// a constant.

define instruction-function constant-ref
   (backend :: <harp-back-end>, reference, 
    #key offset :: <integer> = 0,
         mode :: <symbol> = #"address",
         import? :: <boolean> = #f) 
   => (new :: <i-constant-reference>)
  make(<i-constant-reference>, 
       const-offset: offset, refers-to: reference,
       address-mode: mode, import?: import?);
end instruction-function;

define instruction-function indirect-constant-ref
   (backend :: <harp-back-end>, reference, 
    #key offset :: <integer> = 0,
         import? :: <boolean> = #f) 
   => (new :: <i-constant-reference>)
  make(<i-constant-reference>, 
       const-offset: offset, refers-to: reference,
       address-mode: #"indirect", import?: import?);
end instruction-function;

define instruction-function interactor-constant-ref
   (backend :: <harp-back-end>, reference) 
   => (new :: <i-constant-reference>)
  make(<interactor-constant-reference>, 
       const-offset: 0, refers-to: reference);
end instruction-function;


define instruction-function sf-constant-ref
   (backend :: <harp-back-end>, reference, 
    #key offset :: <integer> = 0,
         mode :: <symbol> = #"address",
         import? :: <boolean> = #f) 
   => (new :: <sf-constant-reference>)
  make(<sf-constant-reference>, 
       const-offset: offset, refers-to: reference,
       address-mode: mode, import?: import?);
end instruction-function;

define instruction-function sf-indirect-constant-ref
   (backend :: <harp-back-end>, reference,
    #key offset :: <integer> = 0,
         import? :: <boolean> = #f)
   => (new :: <sf-indirect-constant-reference>)
  make(<sf-constant-reference>, 
       const-offset: offset, refers-to: reference,
       address-mode: #"indirect", import?: import?);
end instruction-function;


define instruction-function df-constant-ref
   (backend :: <harp-back-end>, reference, 
    #key offset :: <integer> = 0,
         mode :: <symbol> = #"address",
         import? :: <boolean> = #f) 
   => (new :: <df-constant-reference>)
  make(<df-constant-reference>, 
       const-offset: offset, refers-to: reference,
       address-mode: mode);
end instruction-function;

define instruction-function df-indirect-constant-ref
   (backend :: <harp-back-end>, reference,
    #key offset :: <integer> = 0,
         import? :: <boolean> = #f)
   => (new :: <df-indirect-constant-reference>)
  make(<df-constant-reference>, 
       const-offset: offset, refers-to: reference,
       address-mode: #"indirect", import?: import?);
end instruction-function;



define open generic labelled-constant-increment 
    (backend :: <harp-back-end>) => (res :: <integer>);

define method labelled-constant-increment 
    (backend :: <harp-back-end>) => (res :: <integer>)
  4;
end method;



define instruction-function register-external
    (backend :: <harp-back-end>, reference :: <constant-reference>) => ()
  let vars = backend.variables;
  vars.external-references := add-new!(vars.external-references, reference);
end instruction-function;


// Support for referenced data
//
// Each time we want to reference data, we stick the data at the head of a list
// so that it will be dumped in reverse order. The start-of-code will immediately
// follow the end of this data - so the first element put on the list will be 
// referenced as (name - 4), the next as (name - 8) etc.


define method add-referenced-data
    (backend :: <harp-back-end>, data :: <abstract-integer>, ref-class :: <class>)
     => (ref :: <indirect-constant-reference>)
  let vars = backend.variables;
  let new-refs = pair(data, vars.referenced-data-words);
  let offset = new-refs.size * -4; // negative offset from start of code, in bytes
  vars.referenced-data-words := new-refs;
  make(ref-class, 
       const-offset: offset, 
       refers-to: vars.function-name, 
       address-mode: #"indirect");
end method;

// For double-word data (e.g. double-floats), avoid having to make decisions about
// endian-ness at this stage. Encode the data with 2 elements on the list, the first
// being #f, and the second being a pair of low/high values
//
define method add-referenced-double-data
    (backend :: <harp-back-end>, 
     low :: <abstract-integer>, high :: <abstract-integer>, 
     ref-class :: <class>)
     => (ref :: <indirect-constant-reference>)
  let vars = backend.variables;
  let second-ref = pair(pair(low, high), vars.referenced-data-words);
  let new-refs = pair(#f, second-ref);
  let offset = new-refs.size * -4; // negative offset from start of code, in bytes
  vars.referenced-data-words := new-refs;
  make(ref-class, 
       const-offset: offset, 
       refers-to: vars.function-name, 
       address-mode: #"indirect");
end method;


define instruction-function reference-data
    (backend :: <harp-back-end>, data :: <abstract-integer>)
     => (ref :: <indirect-constant-reference>)
  add-referenced-data(backend, data, <i-constant-reference>);
end instruction-function;


define instruction-function reference-sf-data
    (backend :: <harp-back-end>, data :: <abstract-integer>)
     => (ref :: <indirect-constant-reference>)
  add-referenced-data(backend, data, <sf-constant-reference>);
end instruction-function;


define instruction-function reference-df-data
    (backend :: <harp-back-end>, low :: <abstract-integer>, high :: <abstract-integer>)
     => (ref :: <indirect-constant-reference>)
  add-referenced-double-data(backend, low, high, <df-constant-reference>);
end instruction-function;
