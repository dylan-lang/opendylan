Module:    IDVM
Language:  infix-dylan
Synopsis:  IDVM Instruction Set 3 (23/8/94)
Author:    Eliot Miranda, Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Returns (4)

define idvm-inst-return return ()            result        end;
define idvm-inst-return return-false ()      #f            end;
define idvm-inst-return return-lit (literal) literal       end;
define idvm-inst-return return-loc (index)   locals(index) end;



// Calls (9*2) (4+18 = 22)

// 0 arg call
define idvm-insts-mv call (function)
    (function)()
end;

// 1 arg calls with result or local or literal as argument
define idvm-insts-mv call-res (function)
    (function)(result)
end;

define idvm-insts-mv call-loc (function,local-index)
    (function)(locals(local-index))
end;

define idvm-insts-mv call-lit (function,local-index)
    (function)(local-index)
end;

// 2 arg call
define idvm-insts-mv call-loc-loc (function,descriptor is hilo(loc1-index,loc2-index))
    (function)(locals(loc1-index),locals(loc2-index))
end;

define idvm-insts-mv call-lit-loc (function,lit,loc-index)
    (function)(lit,locals(loc-index))
end;

define idvm-insts-mv call-loc-lit (function,loc-index,lit)
    (function)(locals(loc-index),lit)
end;

define idvm-insts-mv call-lit-lit (function,lit1,lit2)
    (function)(lit1,lit2)
end;

// N arg call
define idvm-insts-mv call-n (function,descriptor is hilo(nargs,index))
    apply(function,copy-sequence(vars, start: index,end: index + nargs))
end;


// 0 arg call of result
define idvm-insts-mv rescall ()
    (result)()
end;

// 1 arg calls of the result with literal or local argument
define idvm-insts-mv rescall-loc (local-index)
    (result)(locals(local-index))
end;

define idvm-insts-mv rescall-lit (literal)
    (result)(literal)
end;

// 2 arg calls of result
define idvm-insts-mv rescall-loc-loc (descriptor is hilo(loc1-index,loc2-index))
    (result)(locals(loc1-index),locals(loc2-index))
end;

define idvm-insts-mv rescall-lit-loc (lit,loc-index)
    (result)(lit,locals(loc-index))
end;

define idvm-insts-mv rescall-loc-lit (loc-index,lit)
    (result)(locals(loc-index),lit)
end;

define idvm-insts-mv rescall-lit-lit (lit1,lit2)
    (result)(lit1,lit2)
end;

// N arg call
define idvm-insts-mv rescall-n (descriptor is hilo(nargs,index))
    apply(result,copy-sequence(vars, start: index, end: index + nargs))
end;



// Control Transfer (3) (22+3=25)

define idvm-inst-no-thread jump-true (jump-offset)
    thread(next-ins-offset: if (result) jump-offset else 1 end)
end;

define idvm-inst-no-thread jump-false (jump-offset)
    thread(next-ins-offset: if (result) 1 else jump-offset end)
end;

define idvm-inst-no-thread jump (jump-offset)
    thread(next-ins-offset: jump-offset)
end;


// Branches (8 * 4 = 32) (25 + 32 = 57)

define idvm-branch-local-op-local-inst loc-br-lt       = \<;
define idvm-branch-local-op-local-inst loc-br-gt       = \>;
define idvm-branch-local-op-local-inst loc-br-le       = \<=;
define idvm-branch-local-op-local-inst loc-br-ge       = \>=;
define idvm-branch-local-op-local-inst loc-br-eq       = \=;
define idvm-branch-local-op-local-inst loc-br-ne       = \~=;
define idvm-branch-local-op-local-inst loc-br-ideq     = \==;
define idvm-branch-local-op-local-inst loc-br-idne     = \~==;

define idvm-branch-local-op-lit-inst   lit-br-lt       = \<;
define idvm-branch-local-op-lit-inst   lit-br-gt       = \>;
define idvm-branch-local-op-lit-inst   lit-br-le       = \<=;
define idvm-branch-local-op-lit-inst   lit-br-ge       = \>=;
define idvm-branch-local-op-lit-inst   lit-br-eq       = \=;
define idvm-branch-local-op-lit-inst   lit-br-ne       = \~=;
define idvm-branch-local-op-lit-inst   lit-br-ideq     = \==;
define idvm-branch-local-op-lit-inst   lit-br-idne     = \~==;

define idvm-branch-res-op-local-inst   res-loc-br-lt   = \<;
define idvm-branch-res-op-local-inst   res-loc-br-gt   = \>;
define idvm-branch-res-op-local-inst   res-loc-br-le   = \<=;
define idvm-branch-res-op-local-inst   res-loc-br-ge   = \>=;
define idvm-branch-res-op-local-inst   res-loc-br-eq   = \=;
define idvm-branch-res-op-local-inst   res-loc-br-ne   = \~=;
define idvm-branch-res-op-local-inst   res-loc-br-ideq = \==;
define idvm-branch-res-op-local-inst   res-loc-br-idne = \~==;

define idvm-branch-res-op-lit-inst     res-lit-br-lt   = \<;
define idvm-branch-res-op-lit-inst     res-lit-br-gt   = \>;
define idvm-branch-res-op-lit-inst     res-lit-br-le   = \<=;
define idvm-branch-res-op-lit-inst     res-lit-br-ge   = \>=;
define idvm-branch-res-op-lit-inst     res-lit-br-eq   = \=;
define idvm-branch-res-op-lit-inst     res-lit-br-ne   = \~=;
define idvm-branch-res-op-lit-inst     res-lit-br-ideq = \==;
define idvm-branch-res-op-lit-inst     res-lit-br-idne = \~==;


// Conditionals (8 * 4 = 32) (57 + 32 = 89)

define idvm-res-gets-local-op-local-inst loc-lt       = \<;
define idvm-res-gets-local-op-local-inst loc-gt       = \>;
define idvm-res-gets-local-op-local-inst loc-le       = \<=;
define idvm-res-gets-local-op-local-inst loc-ge       = \>=;
define idvm-res-gets-local-op-local-inst loc-eq       = \=;
define idvm-res-gets-local-op-local-inst loc-ne       = \~=;
define idvm-res-gets-local-op-local-inst loc-ideq     = \==;
define idvm-res-gets-local-op-local-inst loc-idne     = \~==;

define idvm-res-gets-local-op-lit-inst   lit-lt       = \<;
define idvm-res-gets-local-op-lit-inst   lit-gt       = \>;
define idvm-res-gets-local-op-lit-inst   lit-le       = \<=;
define idvm-res-gets-local-op-lit-inst   lit-ge       = \>=;
define idvm-res-gets-local-op-lit-inst   lit-eq       = \=;
define idvm-res-gets-local-op-lit-inst   lit-ne       = \~=;
define idvm-res-gets-local-op-lit-inst   lit-ideq     = \==;
define idvm-res-gets-local-op-lit-inst   lit-idne     = \~==;

define idvm-res-gets-res-op-local-inst   res-loc-lt   = \<;
define idvm-res-gets-res-op-local-inst   res-loc-gt   = \>;
define idvm-res-gets-res-op-local-inst   res-loc-le   = \<=;
define idvm-res-gets-res-op-local-inst   res-loc-ge   = \>=;
define idvm-res-gets-res-op-local-inst   res-loc-eq   = \=;
define idvm-res-gets-res-op-local-inst   res-loc-ne   = \~=;
define idvm-res-gets-res-op-local-inst   res-loc-ideq = \==;
define idvm-res-gets-res-op-local-inst   res-loc-idne = \~==;

define idvm-res-gets-res-op-lit-inst     res-lit-lt   = \<;
define idvm-res-gets-res-op-lit-inst     res-lit-gt   = \>;
define idvm-res-gets-res-op-lit-inst     res-lit-le   = \<=;
define idvm-res-gets-res-op-lit-inst     res-lit-ge   = \>=;
define idvm-res-gets-res-op-lit-inst     res-lit-eq   = \=;
define idvm-res-gets-res-op-lit-inst     res-lit-ne   = \~=;
define idvm-res-gets-res-op-lit-inst     res-lit-ideq = \==;
define idvm-res-gets-res-op-lit-inst     res-lit-idne = \~==;


// Arithmetic (4 * 2 = 8) (89 + 8 = 97)

define idvm-res-gets-local-op-local-inst loc-add      = \+;
define idvm-res-gets-local-op-local-inst loc-sub      = \-;

define idvm-res-gets-local-op-lit-inst   lit-add      = \+;
define idvm-res-gets-local-op-lit-inst   lit-sub      = \-;

define idvm-res-gets-res-op-local-inst   res-loc-add  = \+;
define idvm-res-gets-res-op-local-inst   res-loc-sub  = \-;

define idvm-res-gets-res-op-lit-inst     res-lit-add  = \+;
define idvm-res-gets-res-op-lit-inst     res-lit-sub  = \-;


// Data Transfer (5) (97 + 5 = 102)

define idvm-inst res-gets-lit (literal)
    literal
end;

define idvm-inst-keep-result loc-gets-lit (local-index,literal)
    locals(local-index) := literal;
end;

define idvm-inst res-gets-loc (local-index)
    locals(local-index)
end;

define idvm-inst-keep-result loc-gets-res (local-index)
    locals(local-index) := result;
end;

define idvm-inst-keep-result loc-gets-loc (descriptor is hilo(dst-index,src-index))
    locals(dst-index) := locals(src-index);
end;


// Value-cell instructions for Closures (3) (102 + 3 = 105)

define idvm-inst-keep-result make-value-cell (index)
    locals(index) := make(<value-cell>, value: locals(index));
end;

define idvm-inst-keep-result new-value-cell-res (index)
    locals(index) := make(<value-cell>, value: result);
end;

define idvm-inst-keep-result new-value-cell-lit (index,initial-value)
    locals(index) := make(<value-cell>, value: initial-value);
end;

define idvm-inst-keep-result new-value-cell-loc (descriptor is hilo(dst-index,src-index))
    locals(dst-index) := make(<value-cell>, value: locals(src-index));
end;


// Variable access via closed-over variables (env in locals(0)) (11) (105 + 11 = 116)

define idvm-inst res-gets-ev  (index) locals(0)[index]       end;
define idvm-inst res-gets-evc (index) locals(0)[index].value end;
define idvm-inst res-gets-vc  (index) locals(index).value    end;

// NB ev-gets-res is omitted because the environment closes over non-side effected variables.
// Side effected closed over variables are accessed indirectly via value cells.  Thus only
// evc-gets-res should be needed for side effecting the outer environment.

// define idvm-inst-keep-result ev-gets-res  (index) locals(0)[index]    := result; end;

define idvm-inst-keep-result evc-gets-res (index) locals(0)[index].value := result; end;
define idvm-inst-keep-result vc-gets-res  (index) locals(index).value    := result; end;



define idvm-inst-keep-result loc-gets-ev (descriptor is hilo(dst-index,src-index))
    locals(dst-index) :=  locals(0)[src-index];
end;

define idvm-inst-keep-result loc-gets-evc (descriptor is hilo(dst-index,src-index))
    locals(dst-index) :=  locals(0)[src-index].value;
end;

define idvm-inst-keep-result loc-gets-vc (descriptor is hilo(dst-index,src-index))
    locals(dst-index) :=  locals(src-index).value;
end;



define idvm-inst-keep-result evc-gets-loc (descriptor is hilo(dst-index,src-index))
    locals(0)[dst-index].value := locals(src-index);
end;

define idvm-inst-keep-result vc-gets-loc (descriptor is hilo(dst-index,src-index))
    locals(dst-index).value := locals(src-index);
end;

// This one omitted for the same reason as ev-gets-res
//define idvm-inst-keep-result ev-gets-lit (local-index,literal)
//    locals(0)[local-index] := literal;
//end;

define idvm-inst-keep-result evc-gets-lit (local-index,literal)
    locals(0)[local-index].value := literal;
end;

define idvm-inst-keep-result vc-gets-lit (local-index,literal)
    locals(local-index).value := literal;
end;


// Closure and block instructions (10) (116 + 10 = 126)

define idvm-inst make-closure (info)
    build-IDVM-method(info)
end;

define idvm-inst make-closure-copying (info)
    select(info by instance?)
      <method> => info;
      <vm-method-info> =>
        build-IDVM-method(info, env: idvm-build-environment(info,vars));
    end select;
end;

define idvm-inst make-closure-copying-with-specs (info,local-index)
    select(info by instance?)
      <method> =>
        info.specializers := locals(local-index);
        info;
      <vm-method-info> =>
        info.parameter-type := locals(local-index);
        build-IDVM-method(info, env: idvm-build-environment(info,vars));
    end select;
end;

// unwind-protect-code looks like:
//
//      IDVM-unwind-protect, hilo(protected-form-size,entire-form-size),
//      .....                              ^    ^
//      code for form                      |    |  protected-form-size
//      IDVM-return (or a returning inst)  |    v
//      .....                              |  
//      code for 1 cleanup form            |  entire-form-size
//      IDVM-return (or a returning inst)  v    
//      code following unwind-protect
//
// This works since a cleanup _has_ to occur within the same dynamic extent
// as the surrounding dynamic environment in which the block occurs.

define idvm-inst-no-thread unwind-protect (descriptor is hilo(cleanup-offset,end-offset))
    thread(result: block ()
                       recursive-thread(next-ins-offset: 1)
                   cleanup
                       recursive-thread(next-ins-offset: cleanup-offset)
                   end,
           next-ins-offset: end-offset)
end;

define idvm-inst-return-mv unwind-protect-returning (descriptor is hilo(cleanup-offset,end-offset))
    block ()
        recursive-thread(next-ins-offset: 1)
    cleanup
        recursive-thread(next-ins-offset: cleanup-offset)
    end
end;

// handler-bind (4)

///////////// !@#$
///////////// NB: This does NOT implement handler-bind. Instead it implements
///////////// handler-case. The compiler doesn't use these instructions yet 
///////////// anyway. When it does, we should probably have both.
///////////// (Tony, 24 May 95)

define idvm-inst-no-thread handler-bind-lit (descriptor is hilo(handler-offset,end-offset), condition)
    thread(result: block ()
                       recursive-thread(next-ins-offset: 2)
                   exception (condition)
                       recursive-thread(next-ins-offset: handler-offset)
                   end,
           next-ins-offset: end-offset)
end;

define idvm-inst-no-thread handler-bind-loc (descriptor is hilo(handler-offset,end-offset), cond-loc-idx)
    let condition = locals(cond-loc-idx);
    thread(result: block ()
                       recursive-thread(next-ins-offset: 2)
                   exception (condition)
                       recursive-thread(next-ins-offset: handler-offset)
                   end,
           next-ins-offset: end-offset)
end;

define idvm-inst-return-mv handler-bind-lit-returning (handler-offset, condition)
    block ()
        recursive-thread(next-ins-offset: 2)
    exception (condition)
        recursive-thread(next-ins-offset: handler-offset)
    end
end;

define idvm-inst-return-mv handler-bind-loc-returning (descriptor is hilo(handler-offset,cond-loc-idx))
    let condition = locals(cond-loc-idx);
    block ()
        recursive-thread(next-ins-offset: 1)
    exception (condition)
        recursive-thread(next-ins-offset: handler-offset)
    end
end;

// bind-exit (2)
//
define idvm-inst-no-thread bind-exit (descriptor is hilo(end-offset,exit-proc-loc-idx))
    thread(result: block (return)
                       locals(exit-proc-loc-idx) := return;
                       recursive-thread(next-ins-offset: 1)
                   end,
           next-ins-offset: end-offset)
end;

define idvm-inst-return-mv bind-exit-returning (exit-proc-loc-idx)
    block (return)
        locals(exit-proc-loc-idx) := return;
        recursive-thread(next-ins-offset: 1)
    end
end;

// multiple-value-bind instruction. NB, first bound multiple value is passed as result.
// Hence mv-base is index of second multiple value. (1) (126 + 1 = 127)

define idvm-inst-no-thread mv-bind (next-inst-offset,descriptor is hilo(mv-base,mv-count))
  let mv-inst = operand(2);

  thread(result: select (mv-count)  // limited number of optimised cases, currently 1 case for 2 values
                 2 => begin
                        let (new-result, b) = recursive-thread(next-ins-offset: 2);
                        vars[mv-base] := b;
                        new-result
                      end;
          
                 otherwise =>    // slow case for arbitrary number of multiple values
                      begin
                        let (new-result, #rest mvs) = recursive-thread(next-ins-offset: 2);
                        if (mvs.size < (mv-count - 1))
                          replace-subsequence!(vars, mvs, start: mv-base, end: mv-base + mvs.size);
                          fill!(vars, #f, start: mv-base + mvs.size, end: mv-base + mv-count - 1);
                        else
                          let mvs-head = copy-sequence(mvs, end: mv-count - 1);
                          replace-subsequence!(vars, mvs-head, start: mv-base, end: mv-base + mv-count - 1);
                        end if;
                        new-result
                      end
                 end select,
         next-ins-offset: next-inst-offset)
end;

/// mv-bind-rest is similar to mv-bind, but it binds an additional 
/// value to the remaining values as a #rest sequence.

define idvm-inst-no-thread mv-bind-rest (next-inst-offset,descriptor is hilo(mv-base,mv-count))
  let mv-inst = operand(2);

  thread(result: select (mv-count)  // limited number of optimised cases, currently 1 case for 0 required values
                 0 => begin
                        let (#rest new-result) = recursive-thread(next-ins-offset: 2);
                        new-result
                      end;
          
                 otherwise =>    // slow case for arbitrary number of multiple values
                      begin
                        let (new-result, #rest mvs) = recursive-thread(next-ins-offset: 2);
                        if (mv-count == 1)
                          locals(mv-base) := mvs;
                        elseif (mvs.size < mv-count)
                          let mv-end = mv-base + mv-count - 1;
                          replace-subsequence!(vars, mvs, start: mv-base, end: mv-base + mvs.size);
                          fill!(vars, #f, start: mv-base + mvs.size, end: mv-end);
                          locals(mv-end) := #[];
                        else
                          let mv-end = mv-base + mv-count - 1;
                          let mvs-head = copy-sequence(mvs, end: mv-count - 1);
                          replace-subsequence!(vars, mvs-head, start: mv-base, end: mv-end);
                          locals(mv-end) := copy-sequence(mvs, start: mv-count - 1);
                        end if;
                        new-result
                      end
                 end select,
         next-ins-offset: next-inst-offset)
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

define idvm-inst process-keys ()
    let info      :: <vm-method-info> = vm-code[0];
    let nargs     :: <integer>        = info.arg-count;
    let kvps      :: <simple-object-vector>          = info.key-value-pairs;
    let kvps-size :: <integer>        = kvps.size;
    let rv        :: <simple-object-vector>          = locals(nargs + 1); // e.g. locals: env, a0, a1, rv
    let rest-size :: <integer>        = rv.size;

    let reason = 
        block (return)
          if (rest-size.odd?) return("odd # args") end;

          for (i from 0 below kvps-size by 2)
            locals((i / 2) + nargs + 2) := kvps[i + 1]
          end for;

          for (i from rest-size - 2 to 0 by -2)
            let keyword = rv[i];
            let j       = 0;

            if (keyword.object-class ~== <symbol>) return("non-symbol arg") end;

            while (j < kvps-size & kvps[j] ~== keyword) j := j + 2 end;

            if (j >= kvps-size)
              let called-via-next-method? = result;
              unless (info.takes-all-keys | called-via-next-method?) 
		return("illegal key") 
	      end unless;
            else
              locals((j / 2) + nargs + 2) := rv[i + 1]
            end if;
          end for;
          #f
        end block;

    if (reason) error(concatenate("invalid keys: ", reason)) end if;

    result;
end;
