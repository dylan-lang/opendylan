module:    harp-registers
Synopsis:  The <vreg-state> class, which describes virtual register usage
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// <vreg-state>
//// This represents the state of virtual registers - and is 
//// private to each lambda being compiled. The analogous mechanism 
//// in LispWorks is to use special variables. Wherever LW uses
//// with-saved-vreg-state we must create a new <vreg-state> object
//// in Dylan.


define primary open class <vreg-state> (<object>)
  slot vr-vect :: <stretchy-virtual-register-vector>;
  slot green-vr-vect :: <simple-virtual-register-vector> = $evrv;
  slot next-vreg-id :: <integer> = -1;
  slot allow-vreg-reuse :: <boolean> = #T;
  slot gregs-to-reuse :: <stretchy-vector>;
  slot nregs-to-reuse :: <stretchy-vector>;
  slot reused-vreg-count :: <integer> = 0;
  slot fresh-vreg-count :: <integer> = 0;
  slot dont-reuse-vregs :: <boolean> = #t;
  slot report-on-vreg-reuse :: <boolean> = #F;
  slot unique-registers :: <list> = #();
  slot allocated-reals :: <integer> = 0;
  slot allocated-preserved :: <integer> = 0;
  slot number-preserved :: <integer> = 0;
  slot raw-size :: <integer> = 0;
  slot next-ng-spill :: <integer> = 0;
  slot next-gc-spill :: <integer> = 0;
  slot next-sf-spill :: <integer> = 0;
  slot next-df-spill :: <integer> = 0;
  slot ng-spill-central;
  slot gc-spill-central;
  slot sf-spill-central;
  slot df-spill-central;
end;

define method initialize 
    (vreg-state :: <vreg-state>, #key) => (new :: <vreg-state>)
  let dont-reuse :: <boolean> = ~ allow-vreg-reuse;
  next-method();
  vreg-state.dont-reuse-vregs := dont-reuse;
  unless (dont-reuse)
    vreg-state.nregs-to-reuse := make(<stretchy-vector>);
    vreg-state.gregs-to-reuse := make(<stretchy-vector>);
  end;
  vreg-state;
end;
  
