Module:    IDVM
Synopsis:  Macros for a threaded code implementation IDVM 
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// These macros implement IDVM as a threaded code interpreter.
///// This implementation relies upon a Dylan implementation which
///// performs tail call optimization

/// The implementation may be further optimized for Dylan implementations
/// which can guarantee to reuse a #rest sequence which is the final argument 
/// apply in a tail call position, if that argument corresponds to a #rest 
/// parameter, and if the callee can guarantee dynamic extent. Macros to
/// implement such an optimization are provided here, but commented out. 
/// Once we have a Dylan implementation which handles #rest appropriately
/// we can decide how to maintain the macros in the code organization.





///// Start with any implementation dependent macros
/////


// Macro to define method having a congruent signature with an idvm-instruction 


define macro idvm-function-definer 

  { define idvm-function ?:name ?:body end }
    => { define constant ?name 
           = method (?=vm-code :: <simple-object-vector>, 
                     ?=ip      :: <integer>, 
                     ?=result  :: <object>,
                     ?=vars    :: <simple-object-vector>)
               ?body
             end }

  /*  // This version is for implementations that can optimize
      // apply so that the #rest sequence is guaranteed to be ID?
      // after the call. It could be much more efficient.
  { define idvm-function ?:name ?:body end }
    => { define constant ?name 
           = method (?=vm-code :: <simple-object-vector>, 
                     ?=ip      :: <integer>, 
                     ?=result  :: <object>,
                     ?=vars    :: <simple-object-vector>)
               ?body
             end }
  */

end macro;



// call-idvm-function
// Used to call an IDVM instruction, passing appropriate arguments according
// to the implementation. For this particular implementation, this is the basis of
// the threading mechanism.

define macro call-idvm-function

  { call-idvm-function 
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?vars:expression) }
    => { ?func(?vm-code, ?ip, ?result, ?vars) }

  /*  // This version is for implementations that can optimize
      // apply so that the #rest sequence is guaranteed to be ID?
      // after the call. It could be much more efficient.

  { call-idvm-function 
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?vars:expression) }
    => { apply(?func, ?vm-code, ?ip, ?result, ?vars) }

  */

end macro;



// start-idvm-loop
// Starts an IDVM control loop (which is simply a call to an IDVM function
// for a threaded code implementation)

define macro start-idvm-loop
  { start-idvm-loop
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?vars:expression) }
    => { call-idvm-function(?func, ?vm-code, ?ip, ?result, ?vars) }
end macro;



// start-idvm-loop-building-locals
// Starts an IDVM control loop for which the locals vector has already been 
// created

define macro start-idvm-loop-building-locals

  { start-idvm-loop-building-locals 
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?locals:*) }
    => { ?func(?vm-code, ?ip, ?result, vector(?locals)) }

  /*  // This version is for implementations that can optimize
      // apply so that the #rest sequence is guaranteed to be ID?
      // after the call. It could be much more efficient.

  { start-idvm-loop-building-locals 
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?locals:*) }
    => { ?func(?vm-code, ?ip, ?result, ?locals) }

  */

end macro;





// Macros to affect the control flow of IDVM


// Thread passes control directly to the next instruction. Portable callers
// may only use thread in a tail-call position. 

define macro thread 
  { thread (#key ?result:expression = res-default, ?next-ins-offset:expression = 1) }
    => { let res-default = ?=result;
	 let $i$ = ?=ip + ?next-ins-offset;
         idvm-debug("      - next inst index: %s\n", $i$);
	 call-idvm-function(element(?=vm-code, $i$), 
                            ?=vm-code, $i$ + 1, ?result, ?=vars) }
end macro;


// Recursive-thread recursively invokes a new IDVM loop.
// For the threaded-code implementation, this is implemented identically
// to thread

define macro recursive-thread 
  { recursive-thread(?args:*) }
    => { thread(?args) }
end macro;



// Return-from-thread returns a single value from an IDVM control loop.
// Portable callers must call this in tail-call position.
//
// For the threaded code implementation, we just return the value of the body

define macro return-from-thread
  { return-from-thread(?res:expression) }
    => { ?res }
end macro;

// Return-from-thread-mv returns multiple values from an IDVM control loop.
// Portable callers must call this in tail-call position.
//
// For the threaded code implementation, we just return the values of the body

define macro return-from-thread-mv
  { return-from-thread-mv(?res:expression) }
    => { ?res }
end macro;


