module:    powerpc-harp
Synopsis:  PowerPC code generator macros
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Macro support for the PowerPC instruction set


//  powerpc-template defines the PowerPC backend pattern matchers.

define template-definer-macro powerpc-template
  (<powerpc-back-end>, powerpc-instructions)
end;

define local-template-definer-macro local-powerpc-template
  (<powerpc-back-end>)
end;



// powerpc-method defines methods for PowerPC clash functions etc.


define macro powerpc-method 
  { powerpc-method () ?:body end }
    => { method (?=backend :: <powerpc-back-end>, ?=ins :: <integer>)
           ignore(?=backend); 
           ignore(?=ins); 
           ?body
         end }

  { powerpc-method (?type:name) ?:body end }
    => { method (?=backend :: <powerpc-back-end>, ?=ins :: <integer>)
           let ?=sv-ins = ?=backend.variables.sv-instructions;
           "with-" ## ?type (?=sv-ins at ?=ins)
             ?body
           end
         end }

end macro;


define macro mw-hi-part
  // get top 16 bits of 32
  { mw-hi-part(?bits:expression) } =>
  { coerce-machine-word-to-integer(machine-word-shift-right(?bits, 16))  }
end macro;

define macro mw-lo-part
  // get bottom 16 bits of 32
  { mw-lo-part(?bits:expression) } =>
  { coerce-machine-word-to-integer
      (machine-word-logand(?bits, coerce-integer-to-machine-word(#xffff)))  }
end macro;

define macro hi-part
  // get top 16 bits of 32
  { hi-part(?bits:expression) } => { ash(?bits, -16)  }
end macro;

define macro lo-part
  // get bottom 16 bits of 32
  { lo-part(?bits:expression) } => { logand(?bits, #xffff)  }
end macro;


define macro high-16
  // these work for negative offsets
  { high-16(?bits:expression) } => { logand(generic-ash(?bits, -16), #xffff)  }
end macro;

define macro low-16
  { low-16(?bits:expression) } => { generic-logand(?bits, #xffff)  }
end macro;



define macro low-5
  { low-5(?bits:expression) } => { logand(?bits, #x1f)  }
end macro;


define macro mw-ash
  { mw-ash(?bits:expression, ?n:expression) } =>
  { machine-word-unsigned-shift-left
        (coerce-integer-to-machine-word(?bits), ?n)  }
end macro;


define macro mw-zero?
  { mw-zero?(?bits:expression) } =>
  { machine-word-equal?(?bits, coerce-integer-to-machine-word(0)) }
end macro;


define macro mw-add
  { mw-add(?bits:expression, ?n:expression) } =>
  { machine-word-add-signal-overflow
	 (?bits, coerce-integer-to-machine-word(?n)) }
end macro;

