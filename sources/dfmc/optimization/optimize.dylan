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

define inline method opt-format-out (string :: <string>, #rest args)
  if (*trace-optimizations?*)
    apply(format-out, string, args)
  end if
end method;

define constant $max-reoptimization-iterations = 50;
define constant $max-optimization-iterations   = 10000;

define variable *trace-optimizations?*     = #f;
define variable *trace-optimizing-library* = #f;
define variable *trace-optimizing-file*    = #f;
define variable *trace-optimizing-method*  = #f;
define variable *dump-dfm?*                = #f;
define variable *dump-dfm-library*         = #f;
define variable *dump-dfm-file*            = #f;
define variable *dump-dfm-method*          = #f;

define function send-debug (key :: <symbol>, object)
  *dump-dfm-method* &
    *dump-dfm-method*(key, object);
end;
// HACK: SHOULD BE ELSEWHERE

define function debug-string (object)
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
    & (*trace-optimizations?*
	 | (*trace-optimizing-method*
	      & debug-string(code) == *trace-optimizing-method*))
end function;

define function dumping-dfm? 
    (code :: <&lambda>) => (well? :: <boolean>)
  tracing-library?(*dump-dfm-library*)
    & tracing-file?(*dump-dfm-file*, code)
    & (*dump-dfm?*
	 | (*dump-dfm-method*
	      & debug-string(code) == *dump-dfm-method*))
end function;

define sealed method really-run-compilation-passes (code :: <&lambda>)
  dynamic-bind
     (*trace-optimizations?* = tracing-optimizations?(code))
  unless (~code.body | lambda-optimized?(code))
    let send-debug = method(k, o) send-debug(k, pair(code, o)) end;
    block ()
      for-all-lambdas (f in code)
	lambda-optimized?(f) := #t;
      end for-all-lambdas;
      // opt-format-out("OPTIMIZING %=\n", code);
      with-simple-abort-retry-restart
	  ("Abort all analysis passes and continue.", 
	   "Restart all analysis passes.")
	with-dependent-context ($compilation of model-creator(code))
          send-debug(#"relayouted", #());
          send-debug(#"beginning", #("SSA conversion"));
	  for-all-lambdas (f in code)
	    if (f == code | ~maybe-delete-function-body(f))
	      eliminate-assignments(f);
	    end;
            send-debug(#"relayouted", #());
	  end for-all-lambdas;
          send-debug(#"beginning", #("rename temporaries"));
          if (*flow-types-through-conditionals?*)
  	    for-all-lambdas (f in code)
  	      if (f == code | lambda-used?(f))
                maybe-rename-temporaries-in-conditionals(f);
              end;
              send-debug(#"relayouted", #());
	    end for-all-lambdas;
          end;
          send-debug(#"beginning", #("run optimizations (delete, fold, upgrade, inline)"));
	  for-all-lambdas (f in code)
	    if (f == code | lambda-used?(f))
	      // Now we're ready for some fun.
	      run-optimizations(f);
              send-debug(#"relayouted", #());
	    end;
	  end for-all-lambdas;
          send-debug(#"beginning", #("(loop) run optimizations"));
	  iterate loop (count = 0)
	    let something? = #f;
	    for-all-lambdas (f in code)
	      if (f == code | lambda-used?(f))
		something? := something? | run-optimizations(f);
	      end;
	    end for-all-lambdas;
	    if (something?) 
	      if (count < $max-reoptimization-iterations) 
		loop(count + 1)
	      else
		opt-format-out("MAX REOPTIMIZATIONS FOR %= REACHED\n", code);
	      end if;
	    end;
	  end iterate;
          send-debug(#"relayouted", #());
	  // now carry out the global stuff like environment analysis
          send-debug(#"beginning", #("common subexpression elimination, useless environment deletion"));
	  for-all-lambdas (f in code)
	    if (f == code | lambda-used?(f) | lambda-top-level?(f))
	      share-common-subexpressions(f);
	      delete-useless-environments(f);
              send-debug(#"relayouted", #());
	    end;
	  end for-all-lambdas;
          send-debug(#"beginning", #("analyze dynamic-extent, environments, check optimized computations"));
	  for-all-lambdas (f in code)
	    if (f == code | lambda-used?(f) | lambda-top-level?(f))
	      analyze-dynamic-extent-for(f);
	      analyze-environments(f);
	      check-optimized-computations(f);
              send-debug(#"relayouted", #());
	    end;
	  end for-all-lambdas;
          send-debug(#"beginning", #("pruning closures"));
	  for-all-lambdas (f in code)
	    if (f == code | lambda-used?(f) | lambda-top-level?(f))
	      prune-closure(environment(f));
              send-debug(#"relayouted", #());
	    end;
	  end for-all-lambdas;
          send-debug(#"beginning", #("constant folding closures"));
	  for-all-lambdas (f in code)
	    if (f == code | lambda-used?(f) | lambda-top-level?(f))
	      constant-fold-closure(f);
              send-debug(#"relayouted", #());
	    end;
	  end for-all-lambdas;
	end with-dependent-context;
      end with-simple-abort-retry-restart;
    cleanup
      for-all-lambdas (f in code)
	optimization-queue(f) := #f;
        strip-environment(environment(f));
      end for-all-lambdas;
      send-debug(#"relayouted", #());
      //send-debug(#"highlight", 0);
      send-debug(#"beginning", #("finished"));
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
    // do-queue(method (i) opt-format-out("  ELT %=\n", i) end, queue);
    send-debug(#"highlight-queue", pair(code, map(computation-id, queue | #())));
    if (do-optimize(item))
      something? := #t;
      if (*trace-optimizations?*)
        format-out("---------\n");
        unless (instance?(item, <nop>))
          block()
            print-method-out(code);
          exception (e :: <condition>)
          end;
          format-out("---------\n");
        end unless;
      end if
    else
      unless (item.item-status == $queueable-item-dead)
        queue-pop(queue);
      end unless;
    end if
  finally
    if (count = $max-optimization-iterations)
      opt-format-out("MAX OPTIMIZATIONS FOR %= REACHED\n", code);
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
    send-debug(#"highlight", item);
    send-debug(#"relayouted", pair(item, #()));
    let res = optimize(item) & #t;
    if (res)
      //*dump-dfm-method*(#"relayouted", #());
    end;
    res
  end;
end function;

define inline method run-optimizer
    (name :: <string>, optimize :: <function>, c :: <computation>) 
 => (b :: <boolean>)
  opt-format-out("%s %= \n", name, c);
  // with-parent-computation (c)
  send-debug(#"beginning", pair(c, list(name, c.computation-id)));
    optimize(c) & #t;
  // end;
end method;

define compiler-sideways method re-optimize-type-estimate (c :: <computation>) => ()
  let tmp   = temporary(c);
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

