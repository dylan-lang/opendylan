Synopsis: compilation-pass management
Module:   dfmc-management
Author:   Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define thread variable *trace-compilation-passes* = #f;
define thread variable *reoptimize-after-changes* = #t;

define thread variable *always-check-after?* = #f;
define thread variable *always-check-before?* = #f;
*/

define sealed sideways method run-compilation-passes (code) => code;
  // if (#t)
    really-run-compilation-passes(code);
  //  else
  //  unless (lambda-optimized?(code))
  //    with-simple-abort-retry-restart
  //	("Abort all analysis passes and continue.", 
  //	 "Restart all analysis passes.")
  //      let queue = make(<compilation-queue>);
  //      for (pass in reverse(*passes*.pass-ordering))
  //	push-pass!(queue, pass);
  //      end for;
  //      with-dependent-context ($compilation of model-creator(code))
  //	  local method loop () => ();
  //		  let pass = pop-pass!(queue);
  //		  if (pass)
  //		    run-pass(code, pass, queue);
  //		    loop();
  //		  end if;
  //	  end method loop;
  //	  loop();
  //	  lambda-optimized?(code) := #t;
  //      end with-dependent-context;
  //    end with-simple-abort-retry-restart;
  //  end;
  //  end if;
  code
end method run-compilation-passes;

/* TODO: OBSOLETE?
define method run-pass
    (code, pass :: <compilation-pass>, queue :: <compilation-queue>) => code;
  with-simple-abort-retry-restart
      (format-to-string("Abort %s and continue.", pass.name),
       format-to-string("Restart %s.", pass.name))
    if (pass.print-before?)
      format-out("// before %s\n%=\n", pass.name, code);
    end if;
    if (pass.check-before? | *always-check-before?*)
      ensure-invariants(code, before: pass.name);
    end if;
    if (*trace-compilation-passes*)
      format-out("// running compilation pass %s\n", pass.name);
    end if;
    let changed? = traverse(code, pass.pass-function, pass.visiting-policy);
    if (*reoptimize-after-changes* & changed?)
      for (trigger in pass.all-triggered-passes)
        if (*trace-compilation-passes*)
          format-out("//   triggered %s\n", trigger.name);
        end if;
        push-pass!(queue, trigger);
      end for;
    end if;
    if (pass.print-after?)
      format-out("// after %s\n%=\n", pass.name, code);
    end if;
    if (pass.check-after? | *always-check-after?*)
      ensure-invariants(code, after: pass.name);
    end if;
  end with-simple-abort-retry-restart;
  code
end method run-pass;
*/


/*
/// traversal mechanisms

define method traverse
    (code :: <&lambda>, function, visit == #"top-level-forms")
 => (changed? :: <boolean>);
  function(code)
end method traverse;

define method traverse (f :: <&lambda>, function, visit == #"functions")
 => (changed? :: <boolean>);
  let changed? = #f;
  for-used-lambda (sub-f in f)
    if (traverse(sub-f, function, #"functions"))
      changed? := #t;
    end if;
  end for-used-lambda;
  if (function(f))
    changed? := #t;
  end if;
  changed?
end method traverse;

define method traverse (code :: <&lambda>, function, visit == #"computations")
 => (changed? :: <boolean>);
  local method visit-computations (f :: <&lambda>)
	  let changed? = #f;
	  for-computations (c in f)
	    if (function(c))
	      changed? := #t;
	    end if;
	  end for-computations;
          changed?
	end method visit-computations;
  traverse(code, visit-computations, #"functions")
end method traverse;


//// convenience functions for debugging

define variable *traced-passes* = #();

define method trace-pass (pass :: <compilation-pass>)
  pass.print-before? := #t;
  pass.print-after? := #t;
  pass.check-before? := #t;
  pass.check-after? := #t;
  *traced-passes* := pair(pass.name, *traced-passes*);
  values()
end method trace-pass;

define method trace-pass (name)
  trace-pass(as(<compilation-pass>, name))
end method trace-pass;

define method untrace-pass (pass :: <compilation-pass>)
  pass.print-before? := #f;
  pass.print-after? := #f;
  pass.check-before? := #f;
  pass.check-after? := #f;
  *traced-passes* := remove!(*traced-passes*, pass.name);
  values()
end method untrace-pass;

define method untrace-pass (name)
  untrace-pass(as(<compilation-pass>, name))
end method untrace-pass;

define method untrace-passes ()
  for (name in *traced-passes*)
    untrace-pass(name)
  end for;
  values();
end method untrace-passes;

define method disable-pass (pass :: <compilation-pass>)
  pass.disabled? := #t;
  values()
end method disable-pass;

define method disable-pass (name)
  disable-pass(as(<compilation-pass>, name))
end method disable-pass;

define method enable-pass (pass :: <compilation-pass>)
  pass.disabled? := #f;
  values()
end method enable-pass;

define method enable-pass (name)
  enable-pass(as(<compilation-pass>, name))
end method enable-pass;
*/
