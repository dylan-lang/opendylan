Module:    IDVM
Synopsis:  Infix macros for the IDVM 
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




///// Portable definitions
/////
///// The macros in this file are designed to be portably independent of the 
///// implementation of the control structure of IDVM itself. They will work
///// whether IDVM is implemented as a threaded code interpreter, or as a
///// traditional interpreter loop.



// A macro for defining an IDVM instruction function

// idvm-method-definer was originally implemented as a method definer (i.e. by
// adding a method to a GF. I've moved away from that, because we might want a 
// special meta class for IDVM functions one day in the future. (TonyM 11/6/96)

define macro idvm-method-definer 
  { define idvm-method ?:name () ?:body end }
    => { define idvm-function ?:name ?:body end }
end macro;




// A macro for printing debug messages during development, if required.

define macro idvm-debug

  { idvm-debug (?args:*) } => {};

  /* // This version for when IDVM is to print a debuuging trace during execution
  { idvm-debug (?args:*) } => { idvm-format(?args) }
  */

end macro;




//  macros to abstract away access to locals, operands, operand-encoding:

define macro locals 
  { locals(?index:expression) } 
    => { element(?=vars, ?index) }
end macro;


define macro locals-setter
  { locals-setter(?new-val:expression, ?index:expression) } 
    => { element-setter(?new-val, ?=vars, ?index) }
end macro;



define macro operand
  // This pattern should be irrelevant for a good compiler
  { operand(0) } 
    => { element(?=vm-code, ?=ip) };

  { operand(?index:expression) } 
    => { element(?=vm-code, ?index + ?=ip) };
end macro;



// macros to decode parameters


define macro hilo
  { hilo(?hi:expression, ?lo:expression) } 
    => { ?lo + ash(?hi, 16) }
end macro;


define macro get-left-of-hilo
  { get-left-of-hilo(?desc:expression) } 
    => { ash(?desc, -16) }
end macro;


define macro get-right-of-hilo
  { get-right-of-hilo(?desc:expression) } 
    => { logand(?desc, #xffff) }
end macro;



//// Macros to define the instruction set



// This just unpicks the number of arguments

define macro arg-num 
  { arg-num end }                 => {0}
  { arg-num ?arg end }            => {1}
  { arg-num ?arg, ?arg end }      => {2}
  { arg-num ?arg, ?arg, ?arg end} => {3}
  { arg-num ?arg, ?arg, ?arg, ?rest:* end }  
    => {3 + arg-num ?rest end};

  arg:
    { ?:variable } => {}
    { ?parameter:name is ?:expression } => {}

end macro;



// idvm-inst-body does the real work of unpicking arguments and setting up the control
// flow to the next instruction

define macro idvm-inst-body

  // thread - invokes next instruction setting result to body
  { idvm-inst-body thread (?arg-bindings) ?n-operands:expression; ?:body end}
      => { thread(result: begin ?arg-bindings ?body end, 
                  next-ins-offset: ?n-operands) }

  // thread-keep-result - invokes next instruction keeping old value of result
  { idvm-inst-body thread-keep-result (?arg-bindings) ?n-operands:expression; ?:body end}
      => { ?arg-bindings
           ?body;
           thread(next-ins-offset: ?n-operands) }

  // return - returns the (single value) result of evaluating the body
  { idvm-inst-body return (?arg-bindings) ?n-operands:expression; ?:body end}
      => { return-from-thread(begin ?arg-bindings ?body end) }

  // return-mv - returns the (multiple value) result of evaluating the body
  { idvm-inst-body return-mv (?arg-bindings) ?n-operands:expression; ?:body end}
      => { return-from-thread-mv(begin ?arg-bindings ?body end) }

  // no-thread permits the body to define the effect on control flow.
  // the body MUST end with either thread, return-from-thread or return-from-thread-mv
  { idvm-inst-body no-thread (?arg-bindings) ?n-operands:expression; ?:body end}
      => { ?arg-bindings ?body }

  // Not a recursive definition - so IDVM functions are currently 
  // limited to 4 parameters
  arg-bindings:
    { }
      => { } 
    { ?p0:variable ?binding0, ?p1:variable ?binding1, ?p2:variable ?binding2, ?p3:variable ?binding3 }
      => { let ?p0 = operand(0); let $operand$ = ?p0; ?binding0
           let ?p1 = operand(1); let $operand$ = ?p1; ?binding1
           let ?p2 = operand(2); let $operand$ = ?p2; ?binding2 
           let ?p3 = operand(3); let $operand$ = ?p3; ?binding3 }
    { ?p0:variable ?binding0, ?p1:variable ?binding1, ?p2:variable ?binding2 }
      => { let ?p0 = operand(0); let $operand$ = ?p0; ?binding0 
           let ?p1 = operand(1); let $operand$ = ?p1; ?binding1 
           let ?p2 = operand(2); let $operand$ = ?p2; ?binding2 }
    { ?p0:variable ?binding0, ?p1:variable ?binding1 }
      => { let ?p0 = operand(0); let $operand$ = ?p0; ?binding0 
           let ?p1 = operand(1); let $operand$ = ?p1; ?binding1 }
    { ?p0:variable ?binding0 }
      => { let ?p0 = operand(0); let $operand$ = ?p0; ?binding0 }
  
  binding0:
    { ?binding } => { ?binding }
  binding1:
    { ?binding } => { ?binding }
  binding2:
    { ?binding } => { ?binding }
  binding3:
    { ?binding } => { ?binding }

  binding:
    {  } => { $operand$; }  // a dummy usage to reduce warnings
    { is ?fn:name(?bindingA:name, ?bindingB:name) }
      => { let ?bindingA = "get-left-of-" ## ?fn($operand$);
           let ?bindingB = "get-right-of-" ## ?fn($operand$); }

end macro;


define macro actual-idvm-inst-definer

  { define actual-idvm-inst ?thread-type ?:name (?args:*) 
      ?:body 
    end }
    => { define idvm-function "idvmd-" ## ?name 
           call-idvm-function(idvm-debugger, ?=vm-code, ?=ip, ?=result, ?=vars);
           call-idvm-function("idvm-" ## ?name, ?=vm-code, ?=ip, ?=result, ?=vars);
         end;

         define idvm-function "idvm-" ## ?name 
           idvm-debug("%s: %s\n %s %s", 
                      ?=ip - 1, 
                      copy-sequence(?=vm-code, start: ?=ip - 1, end: ?=ip + arg-num ?args end),
                      ?=result,
                      map(my-debug-name, ?=vars));
           idvm-inst-body ?thread-type (?args) arg-num ?args end; ?body end;
         end;

         element(debug-to-normal-map, "idvmd-" ## ?name) := "idvm-" ## ?name;
         element(normal-to-debug-map, "idvm-" ## ?name) := "idvmd-" ## ?name }

end macro;





/// High-level instruction defining macros 


// IDVM instruction which threads with the value of its body as the new result.

define macro idvm-inst-definer
  { define idvm-inst ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst thread ?name (?args) ?body end }
end macro;



// IDVM instruction which threads without changing the value of result. 

define macro idvm-inst-keep-result-definer
  { define idvm-inst-keep-result ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst thread-keep-result ?name (?args) ?body end }
end macro;


// IDVM instruction which returns the value of its body without threading. 

define macro idvm-inst-return-definer
  { define idvm-inst-return ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst return ?name (?args) ?body end }
end macro;


// IDVM instruction which returns all values of its body without threading. 

define macro idvm-inst-return-mv-definer
  { define idvm-inst-return-mv ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst return-mv ?name (?args) ?body end }
end macro;


// IDVM instruction which leaves it to the body to describe the effect on control flow.
// The body must use either thread, return-from-thread or return-from-thread-mv in 
// tail-call position

define macro idvm-inst-no-thread-definer
  { define idvm-inst-no-thread ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst no-thread ?name (?args) ?body end }
end macro;



// Pair of IDVM instructions, one which threads with the value of its body as the new result,
// and one which returns the value of its body (named idvm-foo-returning). 

define macro idvm-insts-definer
  { define idvm-insts ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst thread ?name (?args) ?body end;
       define actual-idvm-inst return ?name ## "-returning" (?args) ?body end }
end macro;


// Pair of IDVM instructions, one which threads with the value of its body as the new result,
// and one which returns all the values of its body (named idvm-foo-returning). 

define macro idvm-insts-mv-definer
  { define idvm-insts-mv ?:name (?args:*) ?:body end }
  => { define actual-idvm-inst thread ?name (?args) ?body end;
       define actual-idvm-inst return-mv ?name ## "-returning" (?args) ?body end }
end macro;






/*
The following is used in a method-generating vm-builder to invoke vm code:

want:
(vm-code[1])(vm-code, 2, #f, #f, #f, #f, #f, #f, #f, #f, #f)
or:
(vm-code[1])(vm-code, 2, #f, vector(#f, #f, #f, #f, #f, #f, #f, #f))
source:

invoke-code(vm-code, #f, locals: #f, #f, #f, #f, #f, #f, #f, #f)
*/


/// EMULATOR HACK for invoke-code & friends
/// Note the comma after locals: in the following pattern. This should not be necessary
/// - and indeed should not be present. However, it's necessary because our infix reader
/// currently moves in mysterious ways.
///
define macro invoke-code
  { invoke-code 
      (?vm-code:expression, ?result:expression, locals:, ?locals:*) }
    => { start-idvm-loop-building-locals
           (element(?vm-code, 1), ?vm-code, 2, ?result, ?locals) }
end macro;


define macro invoke-code-processing-keys
  { invoke-code-processing-keys 
      (?vm-code:expression, ?result:expression, locals:, ?locals:*) }
    => { start-idvm-loop-building-locals
           (idvm-process-keys, ?vm-code, 1, ?result, ?locals) }
end macro;


define macro invoke-code-with-vectored-locals
  { invoke-code-with-vectored-locals 
      (?vm-code:expression, ?result:expression, locals-vector:, ?lvec:expression) }
    => { start-idvm-loop
           (element(?vm-code, 1), ?vm-code, 2, ?result, ?lvec) }
end macro;


define macro invoke-code-with-vectored-locals-processing-keys
  { invoke-code-with-vectored-locals-processing-keys 
      (?vm-code:expression, ?result:expression, locals-vector:, ?lvec:expression) }
    => { start-idvm-loop
           (idvm-process-keys, ?vm-code, 1, ?result, ?lvec) }
end macro;






/// The following for defining conditional instructions.

define macro idvm-res-gets-local-op-local-inst-definer
  { define idvm-res-gets-local-op-local-inst ?:name = ?op:expression }
    => {define idvm-inst ?name (descriptor is hilo(dst-index, src-index))
          ?op(locals(src-index), locals(dst-index))
        end }
end macro;


define macro idvm-res-gets-local-op-lit-inst-definer
  { define idvm-res-gets-local-op-lit-inst ?:name = ?op:expression }
    => {define idvm-inst ?name (local-index, literal)
          ?op(locals(local-index), literal)
        end }
end macro;


define macro idvm-res-gets-res-op-local-inst-definer
  { define idvm-res-gets-res-op-local-inst ?:name = ?op:expression }
    => {define idvm-inst ?name (local-index)
          ?op(?=result, locals(local-index))
        end }
end macro;


define macro idvm-res-gets-res-op-lit-inst-definer
  { define idvm-res-gets-res-op-lit-inst ?:name = ?op:expression }
    => {define idvm-inst ?name (literal)
          ?op(?=result, literal)
        end }
end macro;



/// The following for defining conditional branch instructions. 

define macro idvm-branch-local-op-local-inst-definer
  { define idvm-branch-local-op-local-inst ?:name = ?op:expression }
    => {define idvm-inst-no-thread ?name (descriptor is hilo(dst-index, src-index), branch-offset)
          thread(next-ins-offset: if (?op(locals(src-index), locals(dst-index)))
                                    branch-offset
                                  else 2
                                  end)
        end }
end macro;


define macro idvm-branch-local-op-lit-inst-definer
  { define idvm-branch-local-op-lit-inst ?:name = ?op:expression }
    => {define idvm-inst-no-thread ?name (local-index, literal, branch-offset)
          thread(next-ins-offset: if (?op(locals(local-index), literal))
                                    branch-offset
                                  else 3
                                  end)
        end }
end macro;


define macro idvm-branch-res-op-local-inst-definer
  { define idvm-branch-res-op-local-inst ?:name = ?op:expression }
    => {define idvm-inst-no-thread ?name (local-index, branch-offset)
          thread(next-ins-offset: if (?op(?=result, locals(local-index)))
                                    branch-offset
                                  else 2
                                  end)
        end }
end macro;


define macro idvm-branch-res-op-lit-inst-definer
  { define idvm-branch-res-op-lit-inst ?:name = ?op:expression }
    => {define idvm-inst-no-thread ?name (literal, branch-offset)
          thread(next-ins-offset: if (?op(?=result, literal))
                                    branch-offset
                                  else 2
                                  end)
        end }
end macro;



