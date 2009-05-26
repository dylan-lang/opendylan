module:    native-rtg
Synopsis:  Managing the FFI barrier, allocation of TEBs & entry points
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Each Dylan thread needs the following:
//  1.  a TEB
//  2.  registration of the stack as an ambiguous root for GC
//  3.  trap handlers for the GC and Dylan (by means of a trampoline)
//
// These are created via different mechanisms depending on the "type" of the
// thread. We distinguish 3 "types": 
//  a.  the master thread (which is used to initialize the Dylan library)
//  b.  Dylan threads (created by the threads library)
//  c   foreign threads
//
// The needs of each thread type are met as follows:-
//
// 1a:  Static space is reserved, & initialization happens in the Dylan DLL entry
// 1b:  Stack space is reserved by the trampoline which calls the first Dylan fn
// 1c:  Heap space is allocated from a pool of pages
//
// 2a:  The Dylan DLL entry registers the stack (even though it will later return)
// 2b:  The trampoline function registers the stack
// 2c:  The FFI code will register the stack whenever a first-time call-in is 
//      detected. It will register from the bottom of stack, and the stack 
//      will remain registered until the thread dies.
//
// 3a:  The entry point for each Dylan DLL and EXE file will invoke the DLL's
//      initialization function inside the combined Dylan/MM trampoline
// 3b:  The thread trampoline function is also a Dylan/MM trampoline
// 3c:  All FFI call-ins will go via the Dylan/MM trampoline (by default)


// Whenever control is passed through the FFI barrier (e.g. because of a callout
// or a call-in) some special actions are required so that the runtime system
// knows what state we are in.
//
// This is necessary for 2 reasons. First, the Dylan SEH handling mechanism needs 
// to be able to determine whether Dylan code is currently running. It will 
// handle various exceptions, but it should only do this if it is Dylan code which
// is currently executing. Any exceptions signalled from foreign code get passed
// on to any existing foreign handler. Secondly, we need to ensure that the TEB 
// doesn't get lost when calling out to foreign code. This might conceivably happen
// because we store the value in a location which is efficient to access, and yet
// which might also be used by foreign code.
//
// As a mechanism for supporting both requirements, we use a normal Windows thread
// local variable to store the TEB for the thread. The value of this variable will 
// be 0 if the thread doesn't have a TEB (yet). Within the TEB, we use a slot to 
// store the FFI barrier state. The value of this slot will be -1 while Dylan code
// is running in the thread, and will be 0 while foreign code is running.
//
// The FFI code and sundry other places must be careful to make use of the primitives
// defined in this file to ensure that the runtime system has accurate information
// about the FFI state.


define constant $inside-dylan = -1;       // value of the TEB runtime state in Dylan

define constant $outside-dylan = 0;       // value of the TEB runtime state outside Dylan

define constant $uninitialized-teb = 0;   // value of the TLV when we don't have a TEB


// Allocation of TEBs: 
// The TEB data structure is defined in native-harp/thread-environment-offsets.dylan
// However, the GC interface of the runtime system also uses slots in the TEB (unknown
// to Dylan). TEB's are allocated by code in this file - and we always ensure that
// there is also a GC-TEB immediately before the TEB. I.e. the GC-TEB is always at a 
// known negative offset from the TEB

define method gc-teb-total-size (be :: <harp-back-end>) => (size :: <integer>)
  8 * 4;
end method;

define method teb-allocation-size (be :: <harp-back-end>) => (size :: <integer>)
  be.teb-total-size + be.gc-teb-total-size;
end method;


// Define the master TEB as a static block. 
// NB: this must appear in the set of ambiguously traced roots of the GC.
// It must be immediately preceeded by the gc-teb.

define direct runtime-variable master-gc-teb = "%master-gc-teb", data: 0, 
  repeat: method(be) ash(gc-teb-total-size(be), -2) end,
  section: #"ambiguous-data";

define direct runtime-variable master-teb = "%master-teb", data: 0, 
  repeat: method(be) ash(teb-total-size(be), -2) end,
  section: #"ambiguous-data";


// TEB-tlv-index is the variable which contains the handle for the Windows TLV which
// holds the TEB for each thread.
//
define runtime-variable TEB-tlv-index = "%teb-tlv-index", data: -1;


// TEB-chain is a variable holding the current TEB in a chain of live TEBs 

define runtime-variable TEB-chain = "%teb-chain", data: 0;


define c-fun runtime-external dylan-init-thread          = "dylan_init_thread";
define c-fun runtime-external dylan-callin-handler       = "dylan_callin_handler";
define c-fun runtime-external trampoline-body            = "trampoline_body";
define c-fun runtime-external mm-dylan-register-thread   = "dylan_mm_register_thread";
define c-fun runtime-external mm-dylan-deregister-thread = "dylan_mm_deregister_thread_from_teb";
define c-fun runtime-external mm-dylan-init-mm           = "dylan_init_memory_manager";
define c-fun runtime-external mm-dylan-stop-mm           = "dylan_shut_down_memory_manager";
define c-fun runtime-external mm-RootStatic = "MMRegisterRootStatic";
define c-fun runtime-external mm-RootImmut  = "MMRegisterRootImmut";
define c-fun runtime-external mm-RootAmbig  = "MMRegisterRootAmbig";
define c-fun runtime-external mm-RootExact  = "MMRegisterRootExact";
define c-fun runtime-external mm-RemoveRoot = "MMDeregisterRoot";
define c-fun runtime-external mm-FreeMemory = "MMFreeMisc";

define threads-prims-iep runtime-external make-foreign-thread = "make-foreign-thread";

define extensions-iep runtime-external call-application-exit-functions-iep
  = "call-application-exit-functions";


define direct runtime-variable %static-root = "%static-root", 
  data: 0, client?: #t, base?: #t, public?: #f;

define direct runtime-variable %immut-root = "%immut-root", 
  data: 0, client?: #t, base?: #t, public?: #f;

define direct runtime-variable %ambig-root = "%ambig-root", 
  data: 0, client?: #t, base?: #t, public?: #f;

define direct runtime-variable %exact-root = "%exact-root", 
  data: 0, client?: #t, base?: #t, public?: #f;



define runtime-variable %started-unloading = "%started-unloading", data: 0;


// module_hInstance holds the module handle for Dylan DLLs
define c runtime-variable module-hinstance = "module_hInstance", 
   data: 0, client?: #t, base?: #t;

define runtime-variable init-dylan-library = "-init-dylan-library",
   data: 0, client?: #t, base?: #t;

// The magic glue symbols:

define constant $data-start-symbol  = "_dylan_data_start";
define constant $data-end-symbol    = "_dylan_data_end";
define constant $vars-start-symbol  = "_dylan_vars_start";
define constant $vars-end-symbol    = "_dylan_vars_end";
define constant $objs-start-symbol  = "_dylan_objs_start";
define constant $objs-end-symbol    = "_dylan_objs_end";
define constant $fixup-start-symbol = "_dylan_fixup_start";
define constant $fixup-end-symbol   = "_dylan_fixup_end";
define constant $import-start-symbol = "_dylan_import_start";
define constant $import-end-symbol   = "_dylan_import_end";


// A spinlock mechanism for the runtime ...
// 0 => unlocked; 1 => locked

define runtime-variable runtime-spin-lock = "%runtime-spin-lock", data: 0;


define macro with-spin-lock
  { with-spin-lock (?be:expression)
      ?:body
    end }
  => { with-harp (?be)
         tag try-lock;
         ins--tag(?be, try-lock);
         ins--conditional-move(?be, try-lock, runtime-spin-lock, 1, 0);
         ?body;
         ins--move(?be, runtime-spin-lock, 0);
       end with-harp }
end macro;


// A count of the number of threads which are registered with the GC

define runtime-variable runtime-thread-count = "%runtime-thread-count", data: 0;

define method op--increment-thread-count (be :: <harp-back-end>) => ()
  with-harp (be)
    tag try-to-update;
    nreg val, updated-val;
    ins--tag(be, try-to-update);
    ins--move(be, val, runtime-thread-count);
    ins--add(be, updated-val, val, 1);
    ins--conditional-move(be, try-to-update, runtime-thread-count, updated-val, val);
  end with-harp;
end method;

define method op--decrement-thread-count (be :: <harp-back-end>) => ()
  with-harp (be)
    tag try-to-update;
    nreg val, updated-val;
    ins--tag(be, try-to-update);
    ins--move(be, val, runtime-thread-count);
    ins--sub(be, updated-val, val, 1);
    ins--conditional-move(be, try-to-update, runtime-thread-count, updated-val, val);
  end with-harp;
end method;



/// Management of the TEB and its corresponding TLV:

define open generic op--create-TEB-tlv-index 
    (be :: <harp-back-end>) => ();


define method op--dylan-registration-error (be :: <harp-back-end>) => ()
  // What should we do if we can't initialize Dylan?
  // It's not ideal to call error because Dylan won't be properly set up
  // For now we just break to the debugger
  ins--halt(be);
end method;


define open generic op--get-teb-tlv
    (be :: <harp-back-end>, dest :: <register>) => ();

define open generic op--set-teb-tlv
    (be :: <harp-back-end>, val) => ();

define open generic op--free-teb-tlv
    (be :: <harp-back-end>) => ();


/// Support for dynamically allocating chains of TEBs.
/// This happens on demand.

// define constant TEB-root-offset = 0;
define constant TEB-prev-TEB-offset = 4;
define constant TEB-next-TEB-offset = 8;
define constant TEB-start-offset = 12;


// Link a new TEB into the TEB chain

define method op--chain-teb (be :: <harp-back-end>, gc-teb-root :: <register>)
  with-harp (be)
    tag continue;
    with-spin-lock (be)
      // Empty TEB chain?
      ins--beq(be, continue, TEB-chain, 0);
      ins--st(be, gc-teb-root, TEB-chain, TEB-next-TEB-offset);

      ins--tag(be, continue);
      ins--st(be, TEB-chain, gc-teb-root, TEB-prev-TEB-offset);
      ins--st(be, 0, gc-teb-root, TEB-next-TEB-offset);
      ins--move(be, TEB-chain, gc-teb-root);
    end with-spin-lock;
  end with-harp;
end method;

// Unlink deleted TEB from the TEB chain

define method op--unchain-teb (be :: <harp-back-end>, gc-teb-root :: <register>)
  with-harp (be)
    nreg prev-teb-root, next-teb-root;
    tag update-next-with-previous, update-previous-with-next, done;

    with-spin-lock (be)
      ins--ld(be, prev-teb-root, gc-teb-root, TEB-prev-TEB-offset);
      ins--ld(be, next-teb-root, gc-teb-root, TEB-next-TEB-offset);

      // head of TEB chain?
      ins--bne(be, update-next-with-previous, TEB-chain, gc-teb-root);
      ins--move(be, TEB-chain, prev-teb-root);
      ins--bra(be, update-previous-with-next);

      ins--tag(be, update-next-with-previous);
      ins--st(be, prev-teb-root, next-teb-root, TEB-prev-TEB-offset);

      ins--tag(be, update-previous-with-next);
      // tail of TEB chain?
      ins--beq(be, done, prev-teb-root, 0);
      ins--st(be, next-teb-root, prev-teb-root, TEB-next-TEB-offset);

      ins--tag(be, done);
    end with-spin-lock;
  end with-harp;
end method;

// Heap allocate a TEB

define method op--allocate-dynamic-teb (be :: <harp-back-end>, teb :: <register>)
  with-harp (be)
    c-result result;
    nreg gc-teb-root, gc-teb, gc-teb-limit;

    let teb-wanted-size = be.teb-allocation-size;


    // NOTE: This should just use raw-malloc-ambig

    // Allocate TEB (plus header)
    op--call-c(be, raw-malloc, teb-wanted-size + TEB-start-offset);
    ins--move(be, gc-teb-root, result);

    ins--add(be, gc-teb, gc-teb-root, TEB-start-offset);

    // Register TEB as ambiguous root
    ins--add(be, gc-teb-limit, gc-teb, teb-wanted-size);
    op--call-c(be, mm-RootAmbig, gc-teb-root, gc-teb-root, gc-teb-limit);

    // Link TEB into TEB chain
    op--chain-teb(be, gc-teb-root);

    ins--add(be, teb, gc-teb, be.gc-teb-total-size);

    // Initalize the TEB & GC-TEB to show we are still outside Dylan
    op--initialize-GC-TEB(be, gc-teb);
    op--initialize-TEB(be, teb, $outside-dylan);
  
  end with-harp;
end method;

// Heap deallocate a TEB

define method op--deallocate-dynamic-teb (be :: <harp-back-end>, gc-teb)
  let gc-teb-root = make-n-register(be);

  // Unlink TEB from TEB chain
  ins--sub(be, gc-teb-root, gc-teb, TEB-start-offset);
  op--unchain-teb(be, gc-teb-root);

  // NOTE: This should just use raw-free-root

  // Deregister TEB as ambiguous root
  ins--ld(be, gc-teb-root, gc-teb, - TEB-start-offset);
  op--call-c(be, mm-RemoveRoot, gc-teb-root);

  // Deallocate the TEB (plus header)
  ins--sub(be, gc-teb-root, gc-teb, TEB-start-offset);
  op--call-c(be, mm-FreeMemory, gc-teb-root,
	     be.teb-allocation-size + TEB-start-offset);
end method;


define method op--maybe-deregister-thread-from-TEB 
    (be :: <harp-back-end>, gc-teb)
  with-harp (be)
    tag not-registered;
    nreg main-ap;

    ins--ld(be, main-ap, gc-teb, 4);
    ins--beq(be, not-registered, main-ap, 0);
    op--uninitialize-thread-from-TEB(be, gc-teb);
    ins--tag(be, not-registered);
  end with-harp;
end method;

// Deregister any dynamically created threads which haven't been 
// deregistered yet.
// Also deregister/deallocate the TEB chain itself.

define method op--deregister-dynamic-teb-chain (be :: <harp-back-end>)
  with-harp (be)
    tag done, loop;
    nreg gc-teb-root, gc-teb;

    ins--move(be, gc-teb-root, TEB-chain);

    ins--tag(be, loop);
    ins--beq(be, done, gc-teb-root, 0);
    ins--add(be, gc-teb, gc-teb-root, TEB-start-offset);
    // This should now always deregister thread since all threads
    // in TEB chain are live but it won't hurt to do the check
    op--maybe-deregister-thread-from-TEB(be, gc-teb);
    ins--ld(be, gc-teb-root, gc-teb-root, TEB-prev-TEB-offset);
    // Heap deallocate TEB
    op--deallocate-dynamic-teb(be, gc-teb);
    ins--bra(be, loop);

    ins--tag(be, done);
  end with-harp;
end method;

define method op--initialize-dylan-thread (be :: <harp-back-end>, teb)
  with-harp(be)
    // Call dylan-init-thread, which will setup the MM trampoline etc.
    // Pass the make-foreign-thread IEP function

    nreg result;
    stack stack;

    ins--push(be, 0);
    ins--move(be, result, stack);

    op--set-runtime-state(be, $inside-dylan, teb);
    op--call-c(be, dylan-init-thread, result, 
               primitive-make-foreign-thread-internal-ref, 
               0, 0);
    ins--add(be, stack, stack, 4); // pop the result from the stack

    op--set-runtime-state(be, $outside-dylan, teb);
  end with-harp;
end method;



define method op--get-valid-teb (be :: <harp-back-end>, teb :: <register>)
  with-harp (be)
    tag done;
  
    op--get-teb-tlv(be, teb);        // Get the TEB
    ins--bne(be, done, teb, 0);      // Check it's valid

    // We don't have a TEB, so this is a foreign created thread.
    // Hence we must do the following
    //   1. allocate & initialize a new TEB in an ambiguous root
    //   2. initialize the thread with the GC (which involves finding bot of stack)
    //   3. Initialize the thread with Dylan (inside the MM trampoline)
    
    op--allocate-dynamic-teb(be, teb);
    op--initialize-thread-with-gc(be);
    op--initialize-dylan-thread(be, teb);

    ins--tag(be, done);
    ins--set-teb(be, teb);           // Store it in Dylan's place

  end with-harp;
end method;



// make-foreign-thread-internal exists as an impedance matcher for C & Dylan calling
// conventions. It's job is to ensure that all the C preserved registers
// are properly looked after around the call to make-foreign-thread

define c-runtime-primitive make-foreign-thread-internal
  // On entry:
  //    no args
  // On exit:
  //    Returns the result of the call to make-foreign-thread

  op--call-iep(be, make-foreign-thread);
  ins--rts(be);
end c-runtime-primitive;


define used-by-client runtime-primitive ensure-valid-teb
  // On entry:
  //    no args
  // On exit:
  //    A valid TEB for this thread (registering the thread with the GC if necessary)
  
  result result;
  nreg teb;

  op--get-valid-teb(be, teb);
  ins--move(be, result, teb);
  ins--rts(be);
end runtime-primitive;



define method op--ensure-valid-teb (be :: <harp-back-end>, teb :: <register>)
  with-harp (be)
    result result;
    op--call-iep(be, primitive-ensure-valid-teb-ref);
    ins--move(be, teb, result);
  end with-harp;
end method;


define method op--get-runtime-state
    (be :: <harp-back-end>, dest :: <register>, teb) => ()
  ins--ld(be, dest, teb, be.teb-runtime-state-offset);
end method;


define method op--set-runtime-state
    (be :: <harp-back-end>, val, teb) => ()
  ins--st(be, val, teb, be.teb-runtime-state-offset);
end method;



/// Management of the FFI barrier:


define method op--dylan-call-in (be :: <harp-back-end>, stdcall? :: <boolean>)
  with-harp (be)

    nreg teb, the-result, first-arg, func-args-struct;
    nreg arg-amount, the-function;
    c-result c-result;
    arg0 arg0;
    c-arg-count arg-count;
    stack stack;
    tag done;
  
    ins--move(be, arg-amount, arg-count);
    ins--move(be, the-function, arg0);
    op--ensure-valid-teb(be, teb);        // Get the TEB & store it in Dylan's place
    op--set-runtime-state(be, $inside-dylan, teb);
  
    // Setup a struct { real-callin-function, base-of-args }
    ins--load-address-of-stack-arg-n(be, first-arg, 0);
    ins--push(be, first-arg);
    ins--push(be, the-function);
    ins--move(be, func-args-struct, stack); 
  
    // Call dylan-callin-handler, which will setup the MM trampoline etc
    // and then call dylan-callin-internal with the same args
    op--call-c(be, dylan-callin-handler, func-args-struct, arg-amount);
    ins--add(be, stack, stack, 8); // pop the struct from the stack
  
    ins--move(be, the-result, c-result);
    op--set-runtime-state(be, $outside-dylan, teb);
    ins--move(be, c-result, the-result);

    // Pop the args for stdcall functions only
    if (stdcall?)
      // Support for the Windows-specific STDCALL convention
      with-harp (be)
	arg-count arg-amount-in-bytes;
	ins--asl(be, arg-amount-in-bytes, arg-amount, 2);
	ins--rts-and-drop(be, arg-amount-in-bytes);
      end;
    else
      ins--rts(be);
    end if;
  end with-harp;
end method;



define c-runtime-primitive dylan-call-in
  // Purpose: To wrap the MM & Dylan trap handlers around a stdcall call-in
  //
  // On entry: SPECIAL CALLING CONVENTION (likely to be platform specific)
  //           C args setup for a call-in
  //           PLUS: 
  //              c-arg-count - size of arguments to call-in in words
  //              arg0        - function to be called inside MM trampoline
  // Behaviour:
  //  * Sets the runtime state to inside-dylan
  //  * Calls the call-in inside the trampoline with c-arg-count bytes of arguments copied
  //  * Sets the runtime state back to outside-dylan
  //
  // On exit:
  //  Returns the result of the call-in function
  op--dylan-call-in(be, #t);
end c-runtime-primitive;


define c-runtime-primitive dylan-call-in-syscall
  // Purpose: To wrap the MM & Dylan trap handlers around a syscall call-in
  //
  // On entry: SPECIAL CALLING CONVENTION (likely to be platform specific)
  //           C args setup for a call-in
  //           PLUS: 
  //              c-arg-count - size of arguments to call-in in words
  //              arg0        - function to be called inside MM trampoline
  // Behaviour:
  //  * Sets the runtime state to inside-dylan
  //  * Calls the call-in inside the trampoline with c-arg-count bytes of arguments copied
  //  * Sets the runtime state back to outside-dylan
  //
  // On exit:
  //  Returns the result of the call-in function
  op--dylan-call-in(be, #f);
end c-runtime-primitive;






define c-runtime-primitive dylan-callin-internal
  // On entry: 
  //    arg-info  pointer to struct { real-callin-function, base-of-args }
  //    arg-size  size of args to copy
  // Purpose: 
  //    Calls real-callin-function with a copied set of args
  // On exit
  //    Returns the result of real-callin-function

  nreg arg-info, arg-size;
  nreg real-callin-function, base-of-args;
  stack stack;

  op--c-load-arguments(be, arg-info, arg-size);
  ins--ld(be, real-callin-function, arg-info, 0);
  ins--ld(be, base-of-args, arg-info, 4);

  setup-args-for-c-call-from-memory(be, base-of-args, arg-size);

  ins--call-alien(be, real-callin-function, 0);
  // Don't know whether the callee pops the args or not.
  // So rely on the frame exit to restore stack discipline
  // ins--add(be, stack, stack, size-in-bytes);
  ins--rts(be);
end c-runtime-primitive;
 


define c-runtime-primitive inside-dylan-ffi-barrier
  // On entry: no args
  //
  // On exit:
  //  Returns 0 if we are outside Dylan FFI barrier
  //          -1 if we are inside Dylan FFI barrier

  c-result c-result;
  nreg teb, current-state;
  tag done;

  op--get-teb-tlv(be, teb);        // Get the TEB
  op--get-runtime-state(be, current-state, teb);
  ins--beq(be, done, current-state, $inside-dylan);
  // The value is already correct (-1) if we're inside
  // Explicitly set 0 if not.
  ins--move(be, current-state, 0);
  ins--tag(be, done);
  ins--move(be, c-result, current-state);
  ins--rts(be);
end c-runtime-primitive;





/// Initialization support


define method op--initialize-TEB 
    (be :: <harp-back-end>, teb, state) => ()
  with-harp (be)
    // Fill in some important fields
    ins--st(be, 0, teb, be.teb-dynamic-environment-offset);
    ins--st(be, dylan-empty-list, teb, be.teb-current-handler-offset);
    ins--st(be, dylan-unbound, teb, be.teb-thread-local-variables-offset);
    // Install it for Dylan
    ins--set-teb(be, teb);
    // Install it in the TLV for the runtime system
    op--set-teb-tlv(be, teb);
    // Set the runtime state 
    op--set-runtime-state(be, state, teb);
  end with-harp;
end method;

define method op--initialize-GC-TEB 
    (be :: <harp-back-end>, gc-teb) => ()
  with-harp (be)
    // Fill the "inside_tramp" field
    ins--st(be, 0, gc-teb, 0);
    op--mark-unregistered-gc-teb(be, gc-teb);
  end with-harp;
end method;

define method op--mark-unregistered-GC-TEB 
    (be :: <harp-back-end>, gc-teb) => ()
  with-harp (be)
    // Zero the main allocation point to show that the TEB is not registered
    ins--st(be, 0, gc-teb, 4);
  end with-harp;
end method;

define open generic op--get-stack-bottom
    (be :: <harp-back-end>, dest :: <register>) => ();

define method op--initialize-thread-with-gc
    (be :: <harp-back-end>) => ()
  with-harp (be)
    nreg stack-bot;
    c-result c-result;
    tag ok;

    // Initialize the thread with the memory manager
    op--get-stack-bottom(be, stack-bot);
    op--call-c(be, mm-dylan-register-thread, stack-bot);
    ins--beq(be, ok, c-result, 0);

    // If registration failed, then we're in big trouble.
    // Set the TEB-TLV to 0 so we know not to deregister, and HALT
    op--set-teb-tlv(be, 0);
    op--dylan-registration-error(be);

    ins--tag(be, ok);
    op--increment-thread-count(be);
    // Initialize FPU;
	// initialize coprocessor and reset all registers and flags;
	// clear condition codes of status word;
	// initialize control word so floating-point exceptions
    // get signalled on this thread
    ins--init-fpu(be);
  end with-harp;
end method;


define method op--uninitialize-thread-from-TEB
     (be :: <harp-back-end>, gc-teb)
  op--call-c(be, mm-dylan-deregister-thread, gc-teb);
  op--mark-unregistered-GC-TEB(be, gc-teb);
  op--decrement-thread-count(be);
end method;


define method op--uninitialize-thread (be :: <harp-back-end>, gc-teb)
  op--uninitialize-thread-from-TEB(be, gc-teb);
  op--set-teb-tlv(be, 0); // set the TEB to 0 to show it's not registered
  // Heap deallocate TEB
  op--deallocate-dynamic-teb(be, gc-teb);
end method;


ignore(op--maybe-uninitialize-thread);

define method op--maybe-uninitialize-thread (be :: <harp-back-end>)
  with-harp (be)
    nreg teb, gc-teb;
    tag done;

    op--get-teb-tlv(be, teb);
    ins--beq(be, done, teb, 0);
    // If the TEB is non-zero, then we must unregister the thread 
    // with the MM when it dies.
    ins--sub(be, gc-teb, teb, be.gc-teb-total-size);
    op--uninitialize-thread(be, gc-teb);

    ins--tag(be, done);
  end with-harp;
end method;


// The DLL entry-point for the master thread uses this:

define method op--initialize-master-thread (be :: <harp-back-end>) => ()

  // Create the state variable
  op--create-TEB-tlv-index(be);

  // Initalize the TEB & GC-TEB
  op--initialize-GC-TEB(be, master-gc-teb);
  op--initialize-TEB(be, master-teb, $outside-dylan);

  // Initialize the memory manager
  op--call-c(be, mm-dylan-init-mm);

  // set up the thread's stack with the MM
  op--initialize-thread-with-gc(be);
end method;


// The entry-point for Dylan-created threads uses this:

define variable *allow-teb-stack-allocation* = #f;

define method op--init-thread-and-TEB (be :: <harp-back-end>) => (gc-teb)
  with-harp (be)
    stack stack;
    nreg teb, gc-teb, gc-teb-result;

    if (*allow-teb-stack-allocation*)
      // Stack-allocate a new TEB
      ins--sub(be, stack, stack, be.teb-total-size);
      ins--move(be, teb, stack);
      // Stack-allocate a new GC-TEB contiguously
      ins--sub(be, stack, stack, be.gc-teb-total-size);
      ins--move(be, gc-teb, stack);
      ins--move(be, gc-teb-result, stack);
      
      // Initalize the TEB & GC-TEB to show we are inside Dylan
      op--initialize-GC-TEB(be, gc-teb);
      op--initialize-TEB(be, teb, $inside-dylan);
    else
      // Heap allocate a new TEB
      op--allocate-dynamic-teb(be, teb);

      // Set the runtime state to show we are inside Dylan
      op--set-runtime-state(be, $inside-dylan, teb);
      
      ins--sub(be, gc-teb-result, teb, be.gc-teb-total-size);
    end if;

    // Initialize the thread with the GC
    op--initialize-thread-with-gc(be);
    gc-teb-result;
  end with-harp;
end method;


define method op--dylan-thread-trampoline
    (be :: <harp-back-end>, stdcall?) => ()
  // On entry:
  //    pointer to a thread
  // Purpose:
  //    Initializes the TEB for a Dylan threads library thread
  //    and the invokes the thread via dylan-init-thread & trampoline-body
  // On exit:
  //    returns the results of the call to dylan-init-thread
  with-harp (be)
    stack stack;
    nreg thread-ptr;
    greg thread;
    nreg the-result;
    c-result c-result;

    op--c-load-arguments(be, thread-ptr);

    let gc-teb = op--init-thread-and-TEB(be);
    ins--push(be, 0); // a dummy variable
    ins--move(be, the-result, stack); // address of that variable

    // Now the thread has been registered with the GC,
    // it is safe to load the "thread" register;
    // Also, free the thread-pointer at the same time
    ins--ld(be, thread, thread-ptr, 0);
    op--call-c(be, raw-free-root, thread-ptr, 4);

    op--call-c(be, dylan-init-thread, the-result, 
	       trampoline-body, thread, 0);

    // Eagerly uninitialize the thread before we lose the TEB
    op--uninitialize-thread(be, gc-teb);

    ins--move(be, c-result, 0);

    // Pop the args for stdcall functions only
    if (stdcall?)
      ins--rts-and-drop(be, 4);
    else
      ins--rts(be);
    end;

  end with-harp;
end method;




/// Entry point support


define used-by-client init runtime-primitive register-traced-roots
  // On entry:
  //    (ambig-start, ambig-end, ambig-root-var,
  //     heap-start, heap-end, static-root-var,
  //     var-start, var-end, exact-root-var)
  // On exit:
  //    no result - but the MM will have been initialized for this DLL
  
  tag ambig-done, heap-done, var-done;
  nreg ambig-start, ambig-end, ambig-root-var;
  nreg heap-start, heap-end, static-root-var;
  nreg var-start, var-end, exact-root-var;

  op--load-arguments(be, ambig-start, ambig-end, ambig-root-var, 
		     heap-start, heap-end, static-root-var,
		     var-start, var-end, exact-root-var);

  ins--beq(be, ambig-done, ambig-start, ambig-end);
  op--call-c(be, mm-RootAmbig, ambig-root-var, ambig-start, ambig-end);
  ins--tag(be, ambig-done);

  ins--beq(be, heap-done, heap-start, heap-end);
  op--call-c(be, mm-RootStatic, static-root-var, heap-start, heap-end);
  ins--tag(be, heap-done);

  ins--beq(be, var-done, var-start, var-end);
  op--call-c(be, mm-RootExact, exact-root-var, var-start, var-end);
  ins--tag(be, var-done);

  op--rts-dropping-n-args(be, 9);
end runtime-primitive;


// Unloading support

define c-runtime-primitive call-dylan-exit-functions-internal
  // Purpose: Perform any Dylan cleanups at process shutdown time
  //          This is simply a calling-convention converter to the IEP
  //
  // On entry: No args
  // On exit: No values
  op--call-iep(be, call-application-exit-functions-iep);
  ins--rts(be);
end c-runtime-primitive;


define c-runtime-primitive call-dylan-exit-functions
  // Purpose: Perform any Dylan cleanups at process shutdown time
  //          arranging to establish trampolines etc.
  //
  // On entry: No args
  // On exit: No values
  c-arg-count argc;
  arg0 arg0;

  ins--move(be, argc, 0);
  ins--move(be, arg0, primitive-call-dylan-exit-functions-internal-ref);
  ins--jmp-alien(be, primitive-dylan-call-in-syscall-ref, 1, 
		 arg-count: #t);
end c-runtime-primitive;


define method op--perform-dylan-shutdown (be :: <harp-back-end>) => ()
  // Call the application exit functions.
  // This must be done after detecting that a shutdown is happening 
  // but before anything drastic happens like parking the GC.
  op--call-c(be, primitive-call-dylan-exit-functions-ref);
end method;


define used-by-client init runtime-primitive deregister-traced-roots
  // On entry:
  //    (ambig-root-var, static-root-var, exact-root-var)
  // On exit:
  //    no result - but the MM roots for this DLL will have been destroyed
  
  tag mm-is-parked;
  nreg root, ambig-root-var, static-root-var, exact-root-var;

  op--load-arguments(be, ambig-root-var, static-root-var, exact-root-var);

  // The MM must be parked before anything can be deregistered
  // because of the danger of dangling pointers
  ins--bne(be, mm-is-parked, %started-unloading, 0);
  op--perform-dylan-shutdown(be);  // This is the last chance for an orderly shutdown
  ins--move(be, %started-unloading, 1);
  op--call-c(be, mps-park);
  ins--tag(be, mm-is-parked);

  ins--ld(be, root, ambig-root-var, 0);
  op--call-c(be, mm-RemoveRoot, root);
  ins--ld(be, root, static-root-var, 0);
  op--call-c(be, mm-RemoveRoot, root);
  ins--ld(be, root, exact-root-var, 0);
  op--call-c(be, mm-RemoveRoot, root);
  op--rts-dropping-n-args(be, 3);
end runtime-primitive;



define method op--shut-down-library (be :: <harp-back-end>) => ()
  op--call-iep(be, primitive-deregister-traced-roots-ref, 
	       %ambig-root, %static-root, %exact-root);
end method;


define method op--shut-down-dylan-library (be :: <harp-back-end>) => ()
  // Assumption: all threads created from within Dylan must have been
  // deregistered by now. This had better be true, if there is any 
  // possibility of that thread continuing - otherwise there will be a 
  // Dylan return address on the stack somewhere - but all Dylan code is 
  // about to be unmapped. Hence the only threads we need to clean up
  // explicitly are (potentially) those with dynamically created TEBs, and 
  // the one with the master TEB. We do a test to see if all threads 
  // have been cleaned up before stopping the MM, because if this we are just
  // about to exit the process we don't really care whether the MM is 
  // shut down, and we know that any remaining threads cannot be re-activated.
  
  with-harp (be)
    tag done;
    // Deregister the master thread if necessary
    op--maybe-deregister-thread-from-TEB(be, master-gc-teb);
    // Deregister any TEB memory and any remaining threads known to Dylan
    op--deregister-dynamic-teb-chain(be);
    // Deallocate the TEB-tlv-index
    op--free-teb-tlv(be);
    // Test whether all threads have been deregistered & avoid shutdown if not
    ins--bne(be, done, runtime-thread-count, 0);
    // close down the MM
    op--call-c(be, mm-dylan-stop-mm);
    ins--tag(be, done);
  end with-harp;
end method;



// To manage the trampoline:

define used-by-client c-runtime-primitive dylan-init-thread-local
  // On entry:
  //    no args
  // On exit:
  //    jumps to dylan_init_thread in the MM interface code,
  //    which sets up the MM trampoline.
  //    This stub exists simply to manage the DLL export here
  //    rather than in collector.c

  ins--jmp(be, dylan-init-thread, 0);
end c-runtime-primitive;


// TEMPORARY: this is here for backwards compatibility reasons only
define used-by-client init runtime-primitive call-first-dylan-iep
  // On entry:
  //   iep  - A Dylan IEP to call with no args
  //        This IEP will be called inside a fresh TEB
  // On exit:
  //  Any MVs from the IEP

  arg0 arg0;
  nreg iep;

  ins--move(be, iep, arg0);

  // Call the IEP
  ins--jmp(be, iep, 0);

end runtime-primitive;



// INIT-DYLAN-DATA

define shared init no-public c-runtime-primitive init-dylan-data
  op--init-dylan-data(be);
end c-runtime-primitive;


define open generic op--init-dylan-data (be :: <harp-back-end>) => ();


// call-init-dylan exists as an impedance matcher for C & Dylan calling
// conventions. It's job is to ensure that all the C preserved registers
// are properly looked after around the call to init-dylan-library

define shared init no-public c-runtime-primitive call-init-dylan
  // On entry:
  //    no args
  // On exit:
  //    Returns the result of the call to init-dylan-library

  op--call-iep(be, init-dylan-library);
  ins--rts(be);
end c-runtime-primitive;


/// The entry points themselves



// DYLAN-INITIALIZE
//
// This is the inner entry point for both EXE & DLL files.
// It performs the 2-stage initialization for the library, fixing 
// up data first, and then calling the initialization code for the library 
// inside an MM trampoline.

define shared init no-public c-runtime-primitive dylan-initialize
  // On entry:
  //    no args
  // On exit:
  //    Returns 0
  
  c-result c-result;
  stack stack;
  nreg the-result, teb;

  ins--push(be, 0); // a dummy variable
  ins--move(be, the-result, stack); // address of that variable
  op--call-c(be, primitive-init-dylan-data-ref);

  // Find the TEB. For the base runtime, we know it's in the fast-access
  // location, because the cold-start put it there. For client DLLs, 
  // we might even have to craete a new one.
  when-client op--ensure-valid-teb(be, teb) end;
  when-base ins--get-teb(be, teb) end;

  op--set-runtime-state(be, $inside-dylan, teb);
  op--call-c(be, primitive-dylan-init-thread-local-ref, the-result, 
             primitive-call-init-dylan-ref, 0, 0);
  op--set-runtime-state(be, $outside-dylan, teb);

  ins--move(be, c-result, 0);
  ins--rts(be);
end c-runtime-primitive;


define open generic op--get-module-handle(be :: <harp-back-end>) => ();


// DYLAN-MAIN
//
// This is the normal entry point for EXE files. It runs the library's code and then
// exits the process.

define shared init c-runtime-primitive dylan-main
  // On entry:
  //    no args
  // On exit:
  //    Never returns - it kills the process instead.
  c-result c-result;
  op--get-module-handle(be);
  op--call-c(be, primitive-dylan-initialize-ref);
  op--shut-down-library(be);
  op--call-iep(be, primitive-exit-application-ref, c-result);
  ins--rts(be);    // except that we'll never get to here.
end c-runtime-primitive;

// Entry-point for Dylan executables with no dependent Dylan DLLs

define init c-runtime-primitive dylan-main-0
  // On entry:
  //    no args
  // On exit:
  //    Never returns - it kills the process instead.
  c-result c-result;

  // Cold start the runtime system ...

  op--initialize-teb-register(be);
  op--initialize-master-thread(be);

  op--get-module-handle(be);

  op--call-c(be, primitive-dylan-initialize-ref);

  op--shut-down-library(be);

  // Do any deregistration of the MM state for the master thread here
  op--maybe-uninitialize-thread-for-p-detach(be);
  // completely close down the MM etc.
  op--shut-down-dylan-library(be);

  op--call-iep(be, primitive-exit-application-ref, c-result);
  ins--rts(be);    // except that we'll never get to here.
end c-runtime-primitive;


define open generic op--initialize-teb-register
    (be :: <harp-back-end>) => ();

define method op--initialize-teb-register
    (be :: <harp-back-end>) => ()
end method;


define method op--maybe-uninitialize-thread-for-p-detach
    (be :: <harp-back-end>)
  // This now does nothing.
  // Used to unregister the current thread, but this caused problems 
  // in Win95 because the detach callback might happen after
  // the OS has silently trashed the TEB on the stack if this happened
  // to be a Dylan thread. If this is a non-Dylan thread (or the master thread)
  // then the final dylan library shutdown will deregister the thread 
  // anyway. But if this is a Dylan thread, then it's safe to not deregister
  // it since this will leave the thread count as non-zero and so the final gc 
  // shutdown won't happen. We never need the final gc shutdown if a Dylan 
  // thread is still active, because the lack of a running Dylan thread is 
  // specified as a shutdown condition.

  // Do any deregistration of the MM state for this thread here
  // op--maybe-uninitialize-thread(be);
end method;



/// Dumping of glue symbols:

define method output-glue
    (be :: <harp-back-end>, outputter)
  output-glue-symbols(be, outputter,
                      data-start:      $data-start-symbol,
                      data-end:        $data-end-symbol,
                      variables-start: $vars-start-symbol,
                      variables-end:   $vars-end-symbol,
                      objects-start:   $objs-start-symbol,
                      objects-end:     $objs-end-symbol,
                      fixup-start:     $fixup-start-symbol,
                      fixup-end:       $fixup-end-symbol,
                      import-start:    $import-start-symbol,
                      import-end:      $import-end-symbol);
end method;



define shared runtime-primitive runtime-module-handle
  nreg hinstDll;

  ins--ld(be, hinstdll, module-hinstance, 0);  // load the module handle for DLLs
  op--call-iep(be, primitive-wrap-machine-word-ref, hinstdll);
  op--rts-dropping-n-args(be, 0);
end runtime-primitive;


define runtime-primitive manual-allocate
  // On entry:
  //   size  - the size in bytes of the allocation
  // On exit:
  //   A pointer to freshly allocated memory in a raw area
  //   suitable for freeing with manual-free

  arg0 arg0;
  result result;

  op--call-iep(be, primitive-unwrap-abstract-integer-ref, arg0);
  op--call-c(be, mps-malloc, result);
  op--call-iep(be, primitive-wrap-machine-word-ref, result);
  ins--rts-and-drop(be, 0);

end runtime-primitive;


define runtime-primitive manual-free
  // On entry:
  //   old  - memory allocated with manual-allocate
  // On exit:
  //   no result. The memory is freed

  arg0 arg0;
  nreg raw-pointer;
  result result;

  ins--ld(be, raw-pointer, arg0, 4);
  op--call-c(be, mps-free, raw-pointer);
  ins--move(be, result, dylan-false);
  ins--rts-and-drop(be, 0);

end runtime-primitive;
