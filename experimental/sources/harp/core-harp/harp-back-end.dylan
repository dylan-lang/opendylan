module:    base-harp
Synopsis:  The definition of the <harp-back-end> class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// The abstract class from which all HARP backends are derived

define abstract open primary class <harp-back-end> (<harp-cg-back-end>)
  slot registers :: <register-model>, init-keyword: registers:;
  slot variables :: <harp-variables>, init-keyword: variables:;
end;

define open generic instructions (backend :: <harp-back-end>)
 => (instructions :: <abstract-instruction-set>);

define method initialize
   (backend :: <harp-back-end>, #key) => (new :: <harp-back-end>)
  next-method();
  // Each individual backend is responsible for  initializing
  // the registers and instruction-set
  backend.variables := make(<harp-variables>);
  backend;
end;


// CODE-ITEM-INCREMENT
// Back ends should specialize this to describe the granularity
// of instructions.
//

define open generic code-item-increment 
    (backend :: <harp-back-end>) => (res :: <integer>);

define method code-item-increment 
    (backend :: <harp-back-end>) => (res :: <integer>)
  1;
end;

