module:    dylan-rtg
Synopsis:  C call-in Primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND





/// C primitives ...
///
/// These all follow normal C (SYSCALL) calling conventions


define c-runtime-primitive call-first-dylan-function
  // On entry:
  //   func       - A Dylan <function> object to be called
  //   n          - the number of args to pass to the Dylan function
  //   #rest args - the args to pass to func
  // A new TEB will be created, & then the function will be called 
  // via its XEP with all the args
  // On exit:
  //  Any MVs from the call to the function, in Dylan format

  // Set up the Thread Environment Block before the call
  op--call-dylan-function-from-c(be); 
end c-runtime-primitive;




define c-runtime-primitive call-dylan-function
  // On entry:
  //   func       - A Dylan <function> object to be called
  //   n          - the number of args to pass to the Dylan function
  //   #rest args - the args to pass to func
  // The function will be called via its XEP with all the args
  // On exit:
  //  The first Dylan return value (as a C return value)

  local method epilog (be :: <harp-back-end>)
          with-harp (be)
            result result;
            c-result c-result;
            ins--move(be, c-result, result);
          end with-harp;
        end method;

  op--call-dylan-function-from-c(be, code-after: epilog); 
end c-runtime-primitive;



define c-full-indirect runtime-variable running-dylan-spy-function? =
  "%running-dylan-spy-function?";

define c-runtime-primitive spy-call-dylan-function
  // On entry:
  //   func       - A Dylan <function> object to be called
  //   n          - the number of args to pass to the Dylan function
  //   #rest args - the args to pass to func
  // The function will be called via its XEP with all the args
  // On exit:
  //  The first Dylan return value (as a C return value)

  local method prolog (be :: <harp-back-end>)
	  ins--move(be, running-dylan-spy-function?, 1);
        end method;

  local method epilog (be :: <harp-back-end>)
          with-harp (be)
            result result;
            c-result c-result;
            ins--move(be, c-result, result);
	    ins--move(be, running-dylan-spy-function?, 0);
          end with-harp;
        end method;

  op--call-dylan-function-from-c
    (be, code-before: prolog, code-after: epilog); 
end c-runtime-primitive;



define c-runtime-primitive call-dylan-function-returning-all-values
  // On entry:
  //   func       - A Dylan <function> object to be called
  //   n          - the number of args to pass to the Dylan function
  //   #rest args - the args to pass to func
  // The function will be called via its XEP with all the args
  // On exit:
  //  A sequence of all the Dylan return values (as a C return value)

  local method epilog (be :: <harp-back-end>)
          with-harp (be)
            result result;
            c-result c-result;
            nreg size, buffer, start;
            
            ins--move(be, start, 0);
            op--buffer-up-remaining-values(be, buffer, size, start);
            ins--move(be, c-result, op--make-vector-from-data(be, buffer, size));
          end with-harp;
        end method;

  op--call-dylan-function-from-c(be, code-after: epilog); 
end c-runtime-primitive;



define method op--call-dylan-function-from-c
    (be :: <harp-back-end>,
     #key code-before = method (be) end,
          code-after  = method (be) end)
  with-harp (be)
    greg func;
    nreg count;
    arg0 arg0;
    arg-count argc;
    function function;
    stack stack;
  
    let max-num-arg-regs = be.registers.arguments-passed-in-registers;

    op--c-load-arguments(be, func, count);
  
    // Do anything necessary before the call
    code-before(be);
  
    // Setup any args for the XEP call

    op--map-calling-convention(be, count, 2);
 
    // Call the XEP
    ins--move(be, function, func);
    ins--move(be, argc, count);
    ins--call-indirect(be, function, be.function-xep-offset, max-num-arg-regs);

    // Do anything necessary after the call
    code-after(be);
  
    ins--rts(be);

  end with-harp;
end method;



define c-runtime-primitive make-dylan-vector
  // On entry:
  //   size  - the size in words of the vector
  // On exit:
  //   A pointer to freshly allocated memory

  nreg size-in-words;
  reg vec;
  c-result c-result;

  op--c-load-arguments(be, size-in-words);
  op--allocate-vector(be, vec, size-in-words);
  ins--move(be, c-result, vec);
  ins--rts(be);

end c-runtime-primitive;


define c-runtime-primitive get-current-teb
  // On entry:
  // On exit:
  //   The value of the TEB

  c-result result;

  ins--get-teb(be, result);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive get-tlv-vector
  // On entry:
  // On exit:
  //   The value of the TLV slot in the TEB

  c-result result;

  ins--ld-teb(be, result, be.teb-thread-local-variables-offset);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive set-tlv-vector
  // On entry:
  //   TLV - The value to store in the TLV slot of the TEB
  // On exit:

  greg tlv;

  op--c-load-arguments(be, tlv);
  ins--st-teb(be, tlv, be.teb-thread-local-variables-offset);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive get-current-thread
  // On entry:
  // On exit:
  //   The value of the TLV slot in the TEB

  c-result result;

  ins--ld-teb(be, result, be.teb-current-thread-offset);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive set-current-thread
  // On entry:
  //   TLV - The value to store in the TLV slot of the TEB
  // On exit:

  greg tlv;

  op--c-load-arguments(be, tlv);
  ins--st-teb(be, tlv, be.teb-current-thread-offset);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive get-current-thread-handle
  // On entry:
  // On exit:
  //   The value of the TLV slot in the TEB

  c-result result;

  ins--ld-teb(be, result, be.teb-current-thread-handle-offset);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive set-current-thread-handle
  // On entry:
  //   TLV - The value to store in the TLV slot of the TEB
  // On exit:

  greg tlv;

  op--c-load-arguments(be, tlv);
  ins--st-teb(be, tlv, be.teb-current-thread-handle-offset);
  ins--rts(be);

end c-runtime-primitive;



define internal-variable runtime-external invoke-dylan-under-coded-restart 
   = "spy-invoke-dylan-under-coded-restart";


define c-runtime-primitive spy-call-interactive-function
  // On entry:
  //   IEP for interactive function (taking no args)
  // On exit:
  //   The sequence of return values from the interactive function

  nreg iep;
  greg func;
  result result;
  c-result c-result;

  op--c-load-arguments(be, iep);
  op--make-thunk-function-object-from-iep(be, func, iep);
  let count-in-dylan = tag-as-integer(be, -1);
  op--call-xep(be, invoke-dylan-under-coded-restart, count-in-dylan, func);
  ins--move(be, c-result, result);
  ins--rts(be);

end c-runtime-primitive;



define threads-prims-constant runtime-external dylan-make-simple-lock
   = "make-simple-lock";

define method op--make-thunk-function-object-from-iep
    (be :: <harp-back-end>, func :: <register>, iep)
  with-harp (be)
    greg template;

    ins--move(be, template, dylan-make-simple-lock); // happens to be congruent
    op--allocate-method(be, func, template, #f, #f);
    ins--st(be, iep, func, be.function-mep-offset);
  end with-harp;
end method; 


define c-runtime-primitive spy-read-location-through-barrier
  // On entry:
  //   address to de-reference
  // On exit:
  //   value found at that address

  nreg address;
  c-result value;

  op--c-load-arguments(be, address);
  ins--ld(be, value, address, 0);
  ins--rts(be);

end c-runtime-primitive;



define c-runtime-primitive spy-write-location-through-barrier
  // On entry:
  //   address, value
  // On exit:
  //   No values. The value is written at the address

  nreg address, value;

  op--c-load-arguments(be, address, value);
  ins--st(be, value, address, 0);
  ins--rts(be);

end c-runtime-primitive;


define c-runtime-primitive spy-read-thread-variable-at-offset
  // On entry:
  //   index
  // On exit:
  //   thread-local-value for current thread

  nreg index;
  greg tlv-vec;
  c-result value;

  op--c-load-arguments(be, index);
  ins--ld-teb(be, tlv-vec, be.teb-thread-local-variables-offset);
  op--load-index(be, value, tlv-vec, index, 0);
  ins--rts(be);

end c-runtime-primitive;


define c-runtime-primitive spy-start-debugger-transaction
  // On entry: no args
  // On exit:  no values
  //   debugger transaction is started

  ins--jmp-alien(be, mps-park, 0)

end c-runtime-primitive;

define c-runtime-primitive spy-end-debugger-transaction
  // On entry: no args
  // On exit:  no values
  //   debugger transaction is started

  ins--jmp-alien(be, mps-release, 0)

end c-runtime-primitive;



define generic-c-runtime-primitive spy-fixup-imported-dylan-data;

define generic-c-runtime-primitive spy-fixup-unimported-dylan-data;

define generic-c-runtime-primitive spy-exit-application;


// This one is not a primitive - but it seems vaguely related somehow:


// default-tlv-vector is a variable used by the threads primitives. It is 
// defined here simply so that we can get it traced as an ambiguous root by the GC.
//
define c runtime-variable default-tlv-vector = "default_tlv_vector",
  section: #"ambiguous-data";


// Linux runtime uses this to map to Dylan strings

define c-runtime-primitive c-primitive-raw-as-string
/*
  On entry:
    Str  - a raw C string
  On exit:
    A pointer to a freshly boxed Dylan <byte-string>
*/
  nreg str;

  op--c-load-arguments(be, str);
  op--call-iep(be, primitive-raw-as-string-ref, str);
  ins--rts(be);
end c-runtime-primitive;
