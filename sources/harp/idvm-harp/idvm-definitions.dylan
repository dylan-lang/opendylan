module:    idvm-harp
Synopsis:  Definitions for the real IDVM instruction set
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Definitions of the IDVM instruction set. This is basically
/// simply extracted from the file which implements the VM itself.
/// I contemplated the idea of actually sharing the contents of the file,
/// and just evaluating it in the context of a different set of macros.
/// But, for now, we have a duplication.


// Returns (4)

define idvm-harp-inst-no-thread return ()            end;
define idvm-harp-inst-no-thread return-false ()      end;
define idvm-harp-inst-no-thread return-lit (literal) end;
define idvm-harp-inst-no-thread return-loc (index)   end;



// Calls (9*2) (4+18 = 22)

// 0 arg call
define idvm-harp-insts call (function)
end;

// 1 arg calls with result or local or literal as argument
define idvm-harp-insts call-res (function)
end;

define idvm-harp-insts call-loc (function,local-index)
end;

define idvm-harp-insts call-lit (function,local-index)
end;

// 2 arg call
define idvm-harp-insts call-loc-loc (function,descriptor is hilo(loc1-index,loc2-index))
end;

define idvm-harp-insts call-lit-loc (function,lit,loc-index)
end;

define idvm-harp-insts call-loc-lit (function,loc-index,lit)
end;

define idvm-harp-insts call-lit-lit (function,lit1,lit2)
end;

// N arg call
define idvm-harp-insts call-n (function,descriptor is hilo(nargs,index))
end;


// 0 arg call of result
define idvm-harp-insts rescall ()
end;

// 1 arg calls of the result with literal or local argument
define idvm-harp-insts rescall-loc (local-index)
end;

define idvm-harp-insts rescall-lit (literal)
end;

// 2 arg calls of result
define idvm-harp-insts rescall-loc-loc (descriptor is hilo(loc1-index,loc2-index))
end;

define idvm-harp-insts rescall-lit-loc (lit,loc-index)
end;

define idvm-harp-insts rescall-loc-lit (loc-index,lit)
end;

define idvm-harp-insts rescall-lit-lit (lit1,lit2)
end;

// N arg call
define idvm-harp-insts rescall-n (descriptor is hilo(nargs,index))
end;




// Control Transfer (3) (22+3=25)

define idvm-harp-inst-no-thread jump-true (jump-offset)
end;

define idvm-harp-inst-no-thread jump-false (jump-offset)
end;

define idvm-harp-inst-no-thread jump (jump-offset)
end;


// Branches (8 * 4 = 32) (25 + 32 = 57)

define idvm-harp-branch-local-op-local-inst loc-br-lt       = \<;
define idvm-harp-branch-local-op-local-inst loc-br-gt       = \>;
define idvm-harp-branch-local-op-local-inst loc-br-le       = \<=;
define idvm-harp-branch-local-op-local-inst loc-br-ge       = \>=;
define idvm-harp-branch-local-op-local-inst loc-br-eq       = \=;
define idvm-harp-branch-local-op-local-inst loc-br-ne       = \~=;
define idvm-harp-branch-local-op-local-inst loc-br-ideq     = \==;
define idvm-harp-branch-local-op-local-inst loc-br-idne     = \~==;

define idvm-harp-branch-local-op-lit-inst   lit-br-lt       = \<;
define idvm-harp-branch-local-op-lit-inst   lit-br-gt       = \>;
define idvm-harp-branch-local-op-lit-inst   lit-br-le       = \<=;
define idvm-harp-branch-local-op-lit-inst   lit-br-ge       = \>=;
define idvm-harp-branch-local-op-lit-inst   lit-br-eq       = \=;
define idvm-harp-branch-local-op-lit-inst   lit-br-ne       = \~=;
define idvm-harp-branch-local-op-lit-inst   lit-br-ideq     = \==;
define idvm-harp-branch-local-op-lit-inst   lit-br-idne     = \~==;

define idvm-harp-branch-res-op-local-inst   res-loc-br-lt   = \<;
define idvm-harp-branch-res-op-local-inst   res-loc-br-gt   = \>;
define idvm-harp-branch-res-op-local-inst   res-loc-br-le   = \<=;
define idvm-harp-branch-res-op-local-inst   res-loc-br-ge   = \>=;
define idvm-harp-branch-res-op-local-inst   res-loc-br-eq   = \=;
define idvm-harp-branch-res-op-local-inst   res-loc-br-ne   = \~=;
define idvm-harp-branch-res-op-local-inst   res-loc-br-ideq = \==;
define idvm-harp-branch-res-op-local-inst   res-loc-br-idne = \~==;

define idvm-harp-branch-res-op-lit-inst     res-lit-br-lt   = \<;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-gt   = \>;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-le   = \<=;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-ge   = \>=;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-eq   = \=;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-ne   = \~=;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-ideq = \==;
define idvm-harp-branch-res-op-lit-inst     res-lit-br-idne = \~==;


// Conditionals (8 * 4 = 32) (57 + 32 = 89)

define idvm-harp-res-gets-local-op-local-inst loc-lt       = \<;
define idvm-harp-res-gets-local-op-local-inst loc-gt       = \>;
define idvm-harp-res-gets-local-op-local-inst loc-le       = \<=;
define idvm-harp-res-gets-local-op-local-inst loc-ge       = \>=;
define idvm-harp-res-gets-local-op-local-inst loc-eq       = \=;
define idvm-harp-res-gets-local-op-local-inst loc-ne       = \~=;
define idvm-harp-res-gets-local-op-local-inst loc-ideq     = \==;
define idvm-harp-res-gets-local-op-local-inst loc-idne     = \~==;

define idvm-harp-res-gets-local-op-lit-inst   lit-lt       = \<;
define idvm-harp-res-gets-local-op-lit-inst   lit-gt       = \>;
define idvm-harp-res-gets-local-op-lit-inst   lit-le       = \<=;
define idvm-harp-res-gets-local-op-lit-inst   lit-ge       = \>=;
define idvm-harp-res-gets-local-op-lit-inst   lit-eq       = \=;
define idvm-harp-res-gets-local-op-lit-inst   lit-ne       = \~=;
define idvm-harp-res-gets-local-op-lit-inst   lit-ideq     = \==;
define idvm-harp-res-gets-local-op-lit-inst   lit-idne     = \~==;

define idvm-harp-res-gets-res-op-local-inst   res-loc-lt   = \<;
define idvm-harp-res-gets-res-op-local-inst   res-loc-gt   = \>;
define idvm-harp-res-gets-res-op-local-inst   res-loc-le   = \<=;
define idvm-harp-res-gets-res-op-local-inst   res-loc-ge   = \>=;
define idvm-harp-res-gets-res-op-local-inst   res-loc-eq   = \=;
define idvm-harp-res-gets-res-op-local-inst   res-loc-ne   = \~=;
define idvm-harp-res-gets-res-op-local-inst   res-loc-ideq = \==;
define idvm-harp-res-gets-res-op-local-inst   res-loc-idne = \~==;

define idvm-harp-res-gets-res-op-lit-inst     res-lit-lt   = \<;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-gt   = \>;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-le   = \<=;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-ge   = \>=;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-eq   = \=;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-ne   = \~=;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-ideq = \==;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-idne = \~==;


// Arithmetic (4 * 2 = 8) (89 + 8 = 97)

define idvm-harp-res-gets-local-op-local-inst loc-add      = \+;
define idvm-harp-res-gets-local-op-local-inst loc-sub      = \-;

define idvm-harp-res-gets-local-op-lit-inst   lit-add      = \+;
define idvm-harp-res-gets-local-op-lit-inst   lit-sub      = \-;

define idvm-harp-res-gets-res-op-local-inst   res-loc-add  = \+;
define idvm-harp-res-gets-res-op-local-inst   res-loc-sub  = \-;

define idvm-harp-res-gets-res-op-lit-inst     res-lit-add  = \+;
define idvm-harp-res-gets-res-op-lit-inst     res-lit-sub  = \-;


// Data Transfer (5) (97 + 5 = 102)

define idvm-harp-inst res-gets-lit (literal)
end;

define idvm-harp-inst-keep-result loc-gets-lit (local-index,literal)
end;

define idvm-harp-inst res-gets-loc (local-index)
end;

define idvm-harp-inst-keep-result loc-gets-res (local-index)
end;

define idvm-harp-inst-keep-result loc-gets-loc (descriptor is hilo(dst-index,src-index))
end;


// Value-cell instructions for Closures (3) (102 + 3 = 105)

define idvm-harp-inst-keep-result make-value-cell (index)
end;

define idvm-harp-inst-keep-result new-value-cell-res (index)
end;

define idvm-harp-inst-keep-result new-value-cell-lit (index,initial-value)
end;

define idvm-harp-inst-keep-result new-value-cell-loc (descriptor is hilo(dst-index,src-index))
end;


// Variable access via closed-over variables (env in locals(0)) (11) (105 + 11 = 116)

define idvm-harp-inst res-gets-ev  (index) end;
define idvm-harp-inst res-gets-evc (index) end;
define idvm-harp-inst res-gets-vc  (index) end;

// NB ev-gets-res is omitted because the environment closes over non-side effected variables.
// Side effected closed over variables are accessed indirectly via value cells.  Thus only
// evc-gets-res should be needed for side effecting the outer environment.
// define idvm-harp-inst-keep-result ev-gets-res  (index) locals(0)[index]       := result end;

define idvm-harp-inst-keep-result evc-gets-res (index) end;
define idvm-harp-inst-keep-result vc-gets-res  (index) end;



define idvm-harp-inst-keep-result loc-gets-ev (descriptor is hilo(dst-index,src-index))
end;

define idvm-harp-inst-keep-result loc-gets-evc (descriptor is hilo(dst-index,src-index))
end;

define idvm-harp-inst-keep-result loc-gets-vc (descriptor is hilo(dst-index,src-index))
end;


define idvm-harp-inst-keep-result evc-gets-loc (descriptor is hilo(dst-index,src-index))
end;

define idvm-harp-inst-keep-result vc-gets-loc (descriptor is hilo(dst-index,src-index))
end;



define idvm-harp-inst-keep-result evc-gets-lit (local-index,literal)
end;

define idvm-harp-inst-keep-result vc-gets-lit (local-index,literal)
end;



// Closure and block instructions (10) (116 + 10 = 126)

define idvm-harp-inst make-closure (info)
end;

define idvm-harp-inst make-closure-copying (info)
end;

define idvm-harp-inst make-closure-copying-with-specs (info,local-index)
end;

// unwind-protect-code looks like:
//
//      IDVM-unwind-protect, hilo(fcs,ccs),
//      .....
//      code for form
//      IDVM-return (or a returning inst)
//      .....
//      code for 1 cleanup form
//      IDVM-return (or a returning inst)
//      code following unwind-protect
//
// This works since a cleanup _has_ to occur within the same dynamic extent
// as the surrounding dynamic environment in which the block occurs.

define idvm-harp-inst-no-thread unwind-protect (descriptor is hilo(cleanup-offset,end-offset))
end;

define idvm-harp-inst-no-thread unwind-protect-returning (descriptor is hilo(cleanup-offset,end-offset))
end;

// handler-bind (4)

define idvm-harp-inst-no-thread handler-bind-lit (descriptor is hilo(handler-offset,end-offset), condition)
end;

define idvm-harp-inst-no-thread handler-bind-loc (descriptor is hilo(handler-offset,end-offset), cond-loc-idx)
end;

define idvm-harp-inst-no-thread handler-bind-lit-returning (handler-offset, condition)
end;

define idvm-harp-inst-no-thread handler-bind-loc-returning (descriptor is hilo(handler-offset,cond-loc-idx))
end;

// bind-exit (2)
//
define idvm-harp-inst-no-thread bind-exit (descriptor is hilo(end-offset,exit-proc-loc-idx))
end;

define idvm-harp-inst-no-thread bind-exit-returning (exit-proc-local-idx)
end;

// multiple-value-bind instruction. NB, first bound multiple value is passed as result.
// Hence mv-base is index of second multiple value. (1) (126 + 1 = 127)

define idvm-harp-inst-no-thread mv-bind (next-inst-offset,descriptor is hilo(mv-base,mv-count))
end;

define idvm-harp-inst-no-thread mv-bind-rest (next-inst-offset,descriptor is hilo(mv-base,mv-count))
end;

// keyword parameters (1) (127 + 1 = 128)
//
// Calling Convention for first idvm instruction:
//
//   next-method/#f passed in via result reg
//   environment in local 0 (#f in methods and closures with no outer environment)
//   first arg always in local 1
//   #rest follows
//   key-values follow

define idvm-harp-inst process-keys ()
end;
