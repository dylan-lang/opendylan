module:    idvm-harp
Synopsis:  The IDVM backend class.
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The IDVM backend class. Nothing very interesting here. (Compared to a class backend that is)

define class <idvm-back-end> (<harp-back-end>) end;

define method initialize (obj :: <idvm-back-end>, #rest r) => <idvm-back-end>;
  next-method();
  obj.registers    := idvm-registers;
  obj.instructions := idvm-instructions;
  obj
end;



define method make-harp-variables
    (backend :: <idvm-back-end>, name, #key)
     => (new :: <idvm-harp-variables>)
  let vars = next-method();
  vars.optimize-leaf-case := #f;
  vars;
end;
