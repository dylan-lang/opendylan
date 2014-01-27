module: dfmc-optimization
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define compilation-pass analyze-environments,
  visit: functions,
  mandatory?: #t;

define compilation-pass allocate-registers,
  visit: functions,
  mandatory?: #t,
  after: analyze-environments,
  check-after?: #t;

define compilation-pass eliminate-assignments,
  visit: functions,
  mandatory?: #t,
  before: analyze-calls;

define compilation-pass constant-fold,
  visit: computations,
  optimization: low,
  after: eliminate-assignments,
  before: analyze-calls;

define compilation-pass analyze-calls,
  visit: computations,
  optimization: low,
  before: single-value-propagation;

define compilation-pass try-inlining,
  visit: computations,
  optimization: low,
  after: analyze-calls,
  before: single-value-propagation,
  triggered-by: analyze-calls,
  trigger: analyze-calls,
  trigger: single-value-propagation;

define compilation-pass analyze-non-local-exits,
  disabled?: #t,
  visit: functions,
  after: try-inlining,
  triggered-by: try-inlining,
  trigger: analyze-calls;

define compilation-pass single-value-propagation,
  visit: computations,
  trigger: analyze-calls,
  before: analyze-environments;

define compilation-pass delete-useless-computations,
  visit: computations,
  optimization: low,
  after: single-value-propagation,
  before: analyze-environments;

define compilation-pass delete-useless-environments,
  visit: functions,
  optimization: low,
  after: delete-useless-computations,
  before: analyze-environments;

*/

define inline method opt-trace
    (key :: <symbol>, description :: <string>,  object)
  if (*trace-optimizations?*)
    *trace-optimizing-callback*(key, description, object)
  end;
end method;

define constant $max-reoptimization-iterations = 50;
define constant $max-optimization-iterations   = 10000;

//boolean indicating that we're active!
define variable *trace-optimizations?*      :: <boolean>            = #f;
//callback function called for trace events
define variable *trace-optimizing-callback* :: false-or(<function>) = #f;
//library filter:
//  #f                         -- trace everything
//  #t                         -- trace top level library
//  library-name (as <symbol>) -- only specific library
define variable *trace-optimizing-library*
  :: type-union(<symbol>, <boolean>) = #f;
//file filter
// #f       -- no filter
// filename -- only trace this filename
define variable *trace-optimizing-file*     :: false-or(<string>)   = #f;
//method filter
// #f                   -- no filter
// method (as <symbol>) -- filter by method name
define variable *trace-optimizing-method*   :: false-or(<symbol>)   = #f;

// HACK: SHOULD BE ELSEWHERE

define function debug-string (object) => (res :: false-or(<symbol>))
  let debug-name = object.debug-name;
  if (instance?(debug-name, <variable-name-fragment>))
    debug-name.fragment-identifier
  elseif (debug-name)
    as(<symbol>, debug-name)
  end
end function debug-string;

define sealed method really-run-compilation-passes (code)
end method;

define inline function tracing-library? (library)
  if (~library)
    #t
  else
    let current-ld = current-library-description();
    if (library == #t)
      current-top-level-library-description?(current-ld)
    else
      library == debug-name(language-definition(current-ld))
    end if
  end if
end function;

define inline function tracing-file? (file, code :: <&lambda>)
  if (~file)
    #t
  else
    let cr = model-compilation-record(code);
    let sr = compilation-record-source-record(cr);
    let lc = source-record-location(sr);
    locator-base(lc) = file
  end if
end function;

define function tracing-optimizations?
    (code :: <&lambda>) => (well? :: <boolean>)
  tracing-library?(*trace-optimizing-library*)
    & tracing-file?(*trace-optimizing-file*, code)
    & ((*trace-optimizing-callback* & #t)
         | *trace-optimizations?*
         | (*trace-optimizing-method*
              & debug-string(code) == *trace-optimizing-method*))
end function;

define sealed method really-run-compilation-passes (code :: <&lambda>)
  dynamic-bind(*trace-optimizations?* = tracing-optimizations?(code))
    unless (~code.body | lambda-optimized?(code))
      block ()
        for-all-lambdas (f in code)
          lambda-optimized?(f) := #t;
        end for-all-lambdas;
        with-simple-abort-retry-restart
            ("Abort all analysis passes and continue.",
             "Restart all analysis passes.")
          with-dependent-context ($compilation of model-creator(code))
            opt-trace(#"start-phase-for-code",
                      "static single assignment conversion",
                      code);
            for-all-lambdas (f in code)
              opt-trace(#"start-phase-for-lambda",
                        "static single assignment conversion", f);
              // make sure we've got some DFM to play with
              // elaborate-top-level-definitions(f);
              // finish pseudo-SSA conversion
              if (f == code | ~maybe-delete-function-body(f))
                eliminate-assignments(f);
              end;
            end for-all-lambdas;
            opt-trace(#"start-phase-for-code", "rename temporaries", code);
            for-all-lambdas (f in code)
              opt-trace(#"start-phase-for-lambda", "rename temporaries", f);
              if (f == code | lambda-used?(f))
                maybe-rename-temporaries-in-conditionals(f);
              end;
            end for-all-lambdas;
            opt-trace
              (#"start-phase-for-code",
               "dead code removal, constant fold, call upgrading, inlining",
               code);
            for-all-lambdas (f in code)
              if (f == code | lambda-used?(f))
                opt-trace
                  (#"start-phase-for-lambda",
                   "dead code removal, constant fold, call upgrading, inlining",
                   f);
                // Now we're ready for some fun.
                run-optimizations(f);
              end;
            end for-all-lambdas;
            iterate loop (count = 0)
              let something? = #f;
              for-all-lambdas (f in code)
                opt-trace
                  (#"start-phase-for-lambda",
                   "dead code removal, constant fold, call upgrading, inlining",
                   f);
                if (f == code | lambda-used?(f))
                  something? := something? | run-optimizations(f);
                end;
              end for-all-lambdas;
              if (something?)
                if (count < $max-reoptimization-iterations)
                  loop(count + 1)
                else
                  opt-trace(#"debug-string", "MAX REOPTIMIZATIONS REACHED", code);
                end if;
              end;
            end iterate;
            // now carry out the global stuff like environment analysis
            opt-trace
              (#"start-phase-for-code",
               "common subexpression elimination, useless environment deletion",
               code);
            for-all-lambdas (f in code)
              if (f == code | lambda-used?(f) | lambda-top-level?(f))
                opt-trace
                  (#"start-phase-for-lambda",
                   "common subexpression elimination, useless environment deletion",
                   f);
                share-common-subexpressions(f);
                delete-useless-environments(f);
              end;
            end for-all-lambdas;
            opt-trace(#"start-phase-for-code",
                      "analyze dynamic-extend, check optimized computations",
                      code);
            for-all-lambdas (f in code)
              if (f == code | lambda-used?(f) | lambda-top-level?(f))
                opt-trace(#"start-phase-for-lambda",
                          "analyze dynamic-extend, check optimized computations",
                          f);
                analyze-dynamic-extent-for(f);
                analyze-environments(f);
                check-optimized-computations(f);
              end;
            end for-all-lambdas;
            opt-trace(#"start-phase-for-code", "prune closures", code);
            for-all-lambdas (f in code)
              if (f == code | lambda-used?(f) | lambda-top-level?(f))
                opt-trace(#"start-phase-for-lambda", "prune closures", f);
                prune-closure(environment(f));
              end;
            end for-all-lambdas;
            opt-trace(#"start-phase-for-code", "constant fold closures", code);
            for-all-lambdas (f in code)
              if (f == code | lambda-used?(f) | lambda-top-level?(f))
                opt-trace(#"start-phase-for-lambda",
                          "constant fold closures",
                          f);
                constant-fold-closure(f);
              end;
            end for-all-lambdas;
            opt-trace(#"finished-phase-for-code", "finished optimizations", code);
          end with-dependent-context;
        end with-simple-abort-retry-restart;
      cleanup
        for-all-lambdas (f in code)
          optimization-queue(f) := #f;
          strip-environment(environment(f));
        end for-all-lambdas;
      end block;
    end unless;
  end dynamic-bind;
end method;

define method ensure-optimization-queue (code)
  optimization-queue(code)
    | begin
        init-optimization-queue(code);
        optimization-queue(code);
      end;
end method;

define method run-optimizations (code) => (b :: <boolean>)
  // run the computation based ones first
  let queue = ensure-optimization-queue(code);
  let something? = queue-head(queue);
  for (count from 0 below $max-optimization-iterations,
       item = something? then queue-head(queue), while: item)
    opt-trace(#"highlight-queue", "optimizations", queue);
    if (do-optimize(item))
      something? := #t;
    else
      unless (item.item-status == $queueable-item-dead)
        queue-pop(queue);
      end unless;
    end if
  finally
    if (count = $max-optimization-iterations)
      opt-trace(#"debug-string", "MAX REOPTIMIZATIONS REACHED", code);
    end if;
  end;
  if (something?) #t else #f end;
end method;

/*
     OPTIMIZE

 The entry point for optimization of computations. If an
 optimization succeeds or maybe the type of the computation changes
 we would expect the nodes which are now *potentially* subject to
 optimization to be re-scheduled for optimization by calling
 re-optimize on them.
*/

define generic optimize (item :: <computation>) => (b :: <boolean>);

// An "around" method to establish some context.

define function do-optimize (item :: <computation>) => (b :: <boolean>)
  with-parent-computation (item)
    optimize(item) & #t;
  end;
end function;

define inline method run-optimizer
    (name :: <string>, optimize :: <function>, c :: <computation>)
 => (b :: <boolean>)
  opt-trace(#"optimizing", concatenate("running optimizer on ", name), c);
  optimize(c) & #t;
end method;

define compiler-sideways method re-optimize-type-estimate (c :: <computation>)
 => ()
  let tmp = temporary(c);
  if (tmp)
    type-estimate-retract(c)
  end if;
end method;

// the default method just pops the computation
define method optimize (c :: <computation>) => (b :: <boolean>)
  run-optimizer("DELETE", delete-useless-computations, c)
    | run-optimizer("FOLD", constant-fold, c)
  // delete-useless-computations(c) | constant-fold(c)
end method optimize;

/// Here's some examples of optimize

define method optimize (c :: <bind>) => (b :: <boolean>)
end method optimize;

// We try 3 things on <function-calls>: constant fold, dispatch or inline
define method optimize (c :: <function-call>) => (b :: <boolean>)
  next-method()
    | run-optimizer("UPGRADE", analyze-calls, c)
    | run-optimizer("INLINE", try-inlining, c)
  //  | analyze-calls(c) | try-inlining(c)
end method optimize;

define method optimize (c :: <primitive-call>) => (b :: <boolean>)
  next-method()
    | run-optimizer("UPGRADE", analyze-calls, c)
  //   | analyze-calls(c)
end method optimize;

// check for case in which <end-protected-block> is only remaining
// user of the protected temporary, in which case we can remove that
// use as well.                                    gts,98feb27
define method optimize (c :: <unwind-protect>) => (b :: <boolean>)
  let temp = c.protected-end & return-temp(c.protected-end);
  if (temp & temp.used?)
    let users = temp.users;
    if (size(users) == 1)
      debug-assert(head(users) == c.protected-end);
      // gts-debug("cleanups", "Removing end-protected user of %=.\n", temp);
      remove-user!(temp, c.protected-end);
    end if;
  end if;
  next-method()
end method optimize;

