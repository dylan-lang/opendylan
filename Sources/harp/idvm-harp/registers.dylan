module:    idvm-harp
Synopsis:  The register model definition for the HARP In Dylan Virtual Machine (IDVM) backend.
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The register model definition for the HARP IDVM backend

define class <idvm-register-model> (<register-model>) end;


/// IDVM register model

define class <idvm-register> (<real-register>) end;

define constant res = make(<idvm-register>, number: 0, pname: "res", mask: ash(1, 0));
define constant env = make(<idvm-register>, number: 1, pname: "env", mask: ash(1, 1));

define constant reg--mlist = res;

 // Don't define res as a temp because if we did we couldn't allocate it
define constant temps-list :: <list> = #();

/// Some ref predicates for back end

define method env-ref (x) env == x & x end;
define method res-ref (x) res == x & x end;

define method not-res-ref (x) 
  if (res == x) #f else x end;
end method;

define constant zero-args-mask = rset-from-args();

define constant idvm-arg-masks = 
  vector(zero-args-mask, zero-args-mask, zero-args-mask, zero-args-mask, 
         zero-args-mask, zero-args-mask, zero-args-mask, zero-args-mask);

define constant idvm-allocatable-registers = vector(res, env);

define constant idvm-real-registers = vector(res, env);

define constant idvm-savable-registers = vector(env); // unused, but NB res is call-clobbered

define constant idvm-preserved-registers = idvm-savable-registers;
define constant idvm-non-preserved = vector(res);

define constant $ev = vector();

define method initialize
    (model :: <idvm-register-model>, #rest r) => <idvm-register-model>;
  next-method();

  model.reg-environment := env;
  model.reg-mlist       := res;
  model.reg-result      := res;
  model.reg-result-out  := res;
  model.reg-machine-arguments       := $ev;
  // Since the IDVM does not support the C calling convention, 
  // all C calling convention descriptions are null
  model.reg-c-machine-arguments     := $ev;
  model.reg-arg-masks               := idvm-arg-masks;
  model.reg-arg-masks-out           := idvm-arg-masks;
  model.reg-c-arg-masks             := $ev;
  model.reg-c-arg-masks-out         := $ev;

  model.real-register-vector        := idvm-real-registers;
  model.preserved-register-vector   := idvm-preserved-registers;
  model.c-preserved-register-vector := $ev;
  model.savable-registers           := idvm-savable-registers;
  model.arguments-passed-in-registers   := 0;
  model.c-arguments-passed-in-registers := 0;
  model.all-allocatable  := idvm-allocatable-registers;
  model.not-preserved    := idvm-non-preserved;
  model.c-not-preserved  := $ev;

  model
end;


define method make-pref-vector
    (backend :: <idvm-back-end>) => <simple-object-vector>;
 make(<simple-object-vector>, size: 2, fill: 0);
end;


define constant idvm-floating-registers = rset-from-args(); // yep thats right 

define constant idvm-allowable-colours = rset-from-list(idvm-allocatable-registers);

/// this is a bit simpler than for the other processors !

define method allowable-colours
   (backend :: <idvm-back-end>, vr :: <integer-virtual-register>) => <integer>;
    idvm-allowable-colours
end;


define constant res-fn = constant-fn(vector(res));


define variable idvm-registers = make(<idvm-register-model>);
