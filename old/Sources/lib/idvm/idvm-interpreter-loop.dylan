Module:    IDVM
Synopsis:  Implementation of IDVM based on an interpreter loop
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// These functions and macros implement IDVM as an interpreter loop.
///// This implementation should be completely portable - but is 
///// likely to be less efficient than one based on threaded code.






///// Start with any implementation dependent macros
/////


// Macro to define method having a congruent signature with an idvm-instruction 


define macro idvm-function-definer 

  { define idvm-function ?:name ?:body end }
    => { define constant ?name 
           = method (?=vm-code          :: <simple-object-vector>, 
                     ?=ip               :: <integer>, 
                     ?=result           :: <object>,
                     ?=vars             :: <simple-object-vector>,
                     ?=return-from-idvm :: <function>)
                 => (ip :: <integer>, result :: <object>)
               ?body
             end }

end macro;



// call-idvm-function
// Used to call an IDVM instruction, passing appropriate arguments according
// to the implementation. 

define macro call-idvm-function
  { call-idvm-function 
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?vars:expression) }
    => { ?func(?vm-code, ?ip, ?result, ?vars, ?=return-from-idvm) }
end macro;



// start-idvm-loop
// Starts an IDVM control loop

define macro start-idvm-loop
  { start-idvm-loop
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?vars:expression) }
    => { idvm-interpreter-loop(?func, ?vm-code, ?ip, ?result, ?vars) }
end macro;



// start-idvm-loop-building-locals
// Starts an IDVM control loop for which the locals vector has already been 
// created

define macro start-idvm-loop-building-locals
  { start-idvm-loop-building-locals 
      (?func:expression, ?vm-code:expression, ?ip:expression, 
       ?result:expression, ?locals:*) }
    => { idvm-interpreter-loop(?func, ?vm-code, ?ip, ?result, vector(?locals)) }
end macro;





// Macros to affect the control flow of IDVM


// Thread passes control directly to the next instruction. Callers
// may only use thread in a tail-call position. 

define macro thread 
  { thread (#key ?result:expression = res-default, ?next-ins-offset:expression = 1) }
    => { let res-default = ?=result;
	 let $i$ = ?=ip + ?next-ins-offset;
         idvm-debug("      - next inst index: %s\n", $i$);
         values($i$, ?result) }
end macro;


// Recursive-thread recursively invokes a new IDVM loop.


define macro recursive-thread 
  { recursive-thread
     (#key ?result:expression = res-default, ?next-ins-offset:expression = 1) }
    => { let res-default = ?=result;
	 let $i$ = ?=ip + ?next-ins-offset;
         idvm-debug("      - next inst index: %s\n", $i$);
         idvm-interpreter-loop(element(?=vm-code,$i$),
                               ?=vm-code,
                               $i$ + 1,
                               ?result,
                               ?=vars) }
end macro;



// Return-from-thread returns a single value from an IDVM control loop.
// Portable callers must call this in tail-call position.


define macro return-from-thread
  { return-from-thread(?res:expression) }
    => { ?=return-from-idvm(?res) }
end macro;


// Return-from-thread-mv returns multiple values from an IDVM control loop.
// Portable callers must call this in tail-call position.

define macro return-from-thread-mv
  { return-from-thread-mv(?res:expression) }
    => { let (#rest vals) = ?res;
         apply(?=return-from-idvm, vals) }
end macro;



//// Finally, here's the control loop itself


define constant idvm-interpreter-loop 
  = method (func :: <function>,
            vm-code :: <simple-object-vector>, 
            ip :: <integer>, 
            result :: <object>, 
            vars :: <simple-object-vector>)
      block (return-from-idvm)
        let l-ip = ip;
        let l-result = result;
        let l-func = func;
        while (#t)
          let (n-ip, n-result) 
            = call-idvm-function(l-func, vm-code, l-ip, l-result, vars);
          l-ip := n-ip + 1;
          l-result := n-result;
          l-func := vm-code[n-ip];
        end while;
      end block;
    end method;

