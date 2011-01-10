module:    native-main-harp
Synopsis:  Native code generator functions and definitions
Author:    Tony Mann, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Default methods assume registers preserved below the frame pointer.

define method signed-frame-pointer-offset
    (backend :: <harp-native-back-end>, x :: <gspill>) => (i :: <integer>)
  let state = backend.variables.vreg-state;
  -4 * (1 // one for the spill location itself
        + x.spill-offset
        + state.raw-size
        + state.number-preserved
       );
end method;


define method signed-frame-pointer-offset 
    (backend :: <harp-native-back-end>, x :: <nspill>) => (i :: <integer>)
  let state = backend.variables.vreg-state;
  -4 * (1 // one for the spill location itself
        + x.spill-offset
        + state.number-preserved
       );
end method;

define method signed-frame-pointer-offset 
    (backend :: <harp-native-back-end>, x :: <sfspill>) => (i :: <integer>)
  let state = backend.variables.vreg-state;
  -4 * (1 // one for the spill location itself
        + x.spill-offset
        + state.number-preserved
        + state.next-ng-spill 
       );
end method;

define method signed-frame-pointer-offset 
    (backend :: <harp-native-back-end>, x :: <dfspill>) => (i :: <integer>)
  let state = backend.variables.vreg-state;
  -4 * (2 // for the spill location itself
        + (2 * x.spill-offset)
        + state.number-preserved
        + state.next-ng-spill 
        + state.next-sf-spill 
       );
end method;

define method spill-frame-pointer-offset
    (backend :: <harp-native-back-end>, spill :: <spill>, with-frame :: <boolean>)
    => (offset :: <integer>)
  if (spill.arg-spill?)
    arg-offset(backend, spill, with-frame: with-frame);
  else
    signed-frame-pointer-offset(backend, spill);
  end if;
end method;


// Uncoloured arg-spills popo up in debug info, so we implement a way to map them here
define method spill-frame-pointer-offset
    (backend :: <harp-native-back-end>, arg-number :: <integer>, with-frame :: <boolean>)
    => (offset :: <integer>)
  arg-offset-from-arg-number(backend, arg-number, with-frame: with-frame);
end method;

define method arg-offset
    (backend :: <harp-native-back-end>, operand :: <spill>, 
     #key with-frame = backend.variables.with-stack)
    => (i :: <integer>)
   4 * (if (with-frame) 2 else return-address-size(backend) end
        + arg-spill-offset-to-arg-number(operand.spill-offset)
       );
end method;


define method arg-offset-from-arg-number
    (backend :: <harp-native-back-end>, operand :: <integer>, 
     #key with-frame = backend.variables.with-stack)
    => (i :: <integer>)
   4 * (if (with-frame) 2 else return-address-size(backend) end
        + operand
       );
end method;


define macro if-return-address
  { if-return-address () ?:body end }
    => { 
	 if (?=be.return-address-on-stack?)
           ?body;
	 end;
       }

    { if-return-address () ?body-1:body else ?body-2:body end }
    => { 
	 if (?=be.return-address-on-stack?)
           ?body-1
	 else
           ?body-2
	 end;
       }
end macro;

define open generic return-address-on-stack?
    (be :: <harp-native-back-end>) => (on-stack? :: <boolean>);

define method return-address-on-stack?
    (be :: <harp-native-back-end>) => (on-stack? :: <boolean>)
  #f
end method;

define method return-address-size
    (be :: <harp-native-back-end>) => (size :: <integer>)
  if (be.return-address-on-stack?) 1
  else 0
  end;
end method;

define method return-address-size-in-bytes
    (be :: <harp-native-back-end>) => (size :: <integer>)
  4 * be.return-address-size
end method;


define method runtime-reference 
    (name :: <byte-string>) => (c :: <constant-reference>)
  make(<constant-reference>, 
       refers-to: name,
       address-mode: #"address",
       const-offset: 0);
end method;

/// size-of-preserved-regs returns the size in bytes of registers
/// preserved on the stack.

define method size-of-preserved-registers
    (be :: <harp-native-back-end>)
  let state = be.variables.vreg-state;
  4 * state.number-preserved
end method size-of-preserved-registers;

/// General outputter support for implicit externals

define method output-implicit-externals
    (backend :: <harp-native-back-end>, outputter :: <harp-outputter>)
  output-external(backend, outputter, remove-optionals-runtime);
end method;
