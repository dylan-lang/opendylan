Module: dfmc-management
Author: Jonathan Bachrach, Keith Playford, and Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// BOOT HACK

define variable *boot-compilation-unit* = #f;

define method ensure-booted () => ()
  if (~*boot-compilation-unit*)
    let boot-cu = make(<compilation-unit>);
    with-simple-abort-retry-restart 
        ("Abort the initial model object boot.", 
         "Restart the initial model object boot.")
      dynamic-bind (*compilation-unit* = boot-cu)
        format-out("Booting into boot compilation unit.\n");
          dynamic-bind (*delay-class-computation* = #t)
            boot-class-world();
            run-registration-thunks();
          end;
      end;
    end;
    *boot-compilation-unit* := boot-cu;
  end;
end method;

/// DRIVER

*compilation-unit* := #f;

define method printer(code, #key message)
  if (message)
    format-out(";; %s\n", message)
  end if;
  format-out("%=\n", code);
  code
end method printer;

define method trans (form)
  with-program-conditions
    ensure-booted();
    dynamic-bind (*compilation-unit* = make(<compilation-unit>))
      let lambda = convert-top-level-form(form);
      register-top-level-lambda(*compilation-unit*, lambda);
      run-compilation-passes(lambda)
    end dynamic-bind;
  end with-program-conditions;
end method trans;

define method compute (form)
  eval(trans(form))
end method compute;

define method interpret (form)
  compile-stage(compute(form))
end method interpret;

/// time-independent evaluator

define method &eval (env :: <environment>, form)
  let lambda = convert-lambda*(env, <&lambda>, #{}, #f, list(form));
  dynamic-bind (*optimization-level* = $optimization-mandatory)
    run-compilation-passes(lambda);
  end dynamic-bind;
  constant-eval(lambda)
end method &eval;

// common case optimizations (very important)

define method &eval (env :: <environment>, form :: <name>)
  &eval(env, lookup(env, form));
end method &eval;

define method &eval (env :: <environment>, form :: <variable-name-fragment>)
  &eval(env, lookup(env, form));
end method &eval;

define method &eval (env :: <environment>, form :: <module-binding>)
  if (constant?(form) & defined?(form))
    form.binding-value-slot
  else
    next-method()
  end
end method &eval;


/// testing tools

/* define macro try
  { try ?:body end }
  => { trans(?body) }
end macro;

define macro run
  { run ?:body end }
  => { eval(trans(?body)) }
end macro;

define macro run-constant
  { run-constant ?:body end }
  => { constant-eval(trans(?body)) }
end macro; */

// TODO: having special syntax for these again would be nice

define method try (fragment)
  trans(fragment)
end method try;

define method run (fragment)
  eval(trans(fragment))
end method run;
