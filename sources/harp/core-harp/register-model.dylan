module:    harp-register-model
Synopsis:  Definition of the <register-model> class.
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define class <invalid-register> (<real-register>) end;

define constant invalid-register = make(<invalid-register>);

define constant $ev :: <simple-object-vector> = vector();

define constant $eiv :: <simple-integer-vector> =
  empty(<simple-integer-vector>);

define open primary class <register-model> (<object>)

  slot reg-frame                 :: <real-register>, init-value: invalid-register;
  slot reg-stack                 :: <real-register>, init-value: invalid-register;
  slot reg-caller-stack          :: <real-register>, init-value: invalid-register;

  slot reg-environment           :: <real-register>, init-value: invalid-register;
  slot reg-function              :: <real-register>, init-value: invalid-register;
  slot reg-mlist                 :: <real-register>, init-value: invalid-register;
  slot reg-result                :: <real-register>, init-value: invalid-register;
  slot reg-result-out            :: <real-register>, init-value: invalid-register;
  slot reg-float-result          :: <real-register>, init-value: invalid-register;

  slot reg-arg-count             :: <real-register>, init-value: invalid-register;
  slot reg-mv-count              :: <real-register>, init-value: invalid-register;

  slot reg-arg0                  :: <real-register>, init-value: invalid-register;
  slot reg-arg1                  :: <real-register>, init-value: invalid-register;
  slot reg-arg2                  :: <real-register>, init-value: invalid-register;
  slot reg-arg3                  :: <real-register>, init-value: invalid-register;
  slot reg-arg4                  :: <real-register>, init-value: invalid-register;
  slot reg-arg5                  :: <real-register>, init-value: invalid-register;
  slot reg-arg6                  :: <real-register>, init-value: invalid-register;
  slot reg-arg7                  :: <real-register>, init-value: invalid-register;

  slot reg-float-arg0            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg1            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg2            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg3            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg4            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg5            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg6            :: <real-register>, init-value: invalid-register;
  slot reg-float-arg7            :: <real-register>, init-value: invalid-register;

  slot reg-machine-arguments         :: <simple-object-vector>, init-value: $ev;
  slot reg-c-machine-arguments       :: <simple-object-vector>, init-value: $ev;
  slot reg-float-machine-arguments   :: <simple-object-vector>, init-value: $ev;
  slot reg-c-float-machine-arguments :: <simple-object-vector>, init-value: $ev;

  slot reg-arg-masks             :: <simple-integer-vector>, init-value: $eiv;
  slot reg-arg-masks-out         :: <simple-integer-vector>, init-value: $eiv;
  slot reg-c-arg-masks           :: <simple-integer-vector>, init-value: $eiv;
  slot reg-c-arg-masks-out       :: <simple-integer-vector>, init-value: $eiv;

  slot reg-link                  :: <real-register>, init-value: invalid-register;

  slot reg-tmp1                  :: <real-register>, init-value: invalid-register;
  slot reg-tmp2                  :: <real-register>, init-value: invalid-register;
  slot reg-tmp3                  :: <real-register>, init-value: invalid-register;
  slot reg-tmp4                  :: <real-register>, init-value: invalid-register;
  slot reg-tmp5                  :: <real-register>, init-value: invalid-register;

  slot reg-ftmp1                 :: <real-register>, init-value: invalid-register;
  slot reg-ftmp2                 :: <real-register>, init-value: invalid-register;

  slot reg-c-result              :: <real-register>, init-value: invalid-register;
  // Return of C structures across FFI barrier may require 2 machine registers
  slot reg-c-result2             :: <real-register>, init-value: invalid-register;
  slot reg-c-float-result        :: <real-register>, init-value: invalid-register;
  slot reg-c-stack               :: <real-register>, init-value: invalid-register;
  slot reg-c-frame               :: <real-register>, init-value: invalid-register;

  slot real-register-vector         :: <simple-object-vector>, init-value: $ev;
  slot preserved-register-vector    :: <simple-object-vector>, init-value: $ev;
  slot c-preserved-register-vector  :: <simple-object-vector>, init-value: $ev;
  slot savable-registers            :: <simple-object-vector>, init-value: $ev;
  slot arguments-passed-in-registers         :: <integer>, init-value: 0;
  slot c-arguments-passed-in-registers       :: <integer>, init-value: 0;
  slot float-arguments-passed-in-registers   :: <integer>, init-value: 0;
  slot c-float-arguments-passed-in-registers :: <integer>, init-value: 0;
  slot all-allocatable           :: <simple-object-vector>, init-value: $ev;
  slot not-preserved             :: <simple-object-vector>, init-value: $ev;
  slot c-not-preserved           :: <simple-object-vector>, init-value: $ev;
  

end;


// Generic c-arg-count register for Dylan call-ins. Need this level of abstraction for
// back-ends like the PowerPC because of Dylan/C register-model mismatch.

define open generic reg-c-arg-count(register-model :: <register-model>)
 => (arg-count :: <real-register>);

