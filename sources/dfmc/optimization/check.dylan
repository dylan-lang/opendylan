Module:   dfmc-optimization
Author:   Keith Playford
Synopsis: Do post-optimization consistency checks
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define program-warning <calling-inline-only-function-out-of-line>
  slot condition-inline-only-function,
    required-init-keyword: inline-only-function:;
  format-string 
    "Failed to inline call to the inline-only function %s - making a "
    "local copy to call out of line";
  format-arguments inline-only-function;
end program-warning;

define method check-optimized-computations (o :: <&lambda>)
  let checker = if (lambda-initializer?(o))
                  rcurry(check-optimized-reference, #t);
                else
                  rcurry(check-optimized-reference, #f);
                end if;
  walk-lambda-references(checker, o);
end method;

define method check-optimized-reference 
    (c :: <computation>, ref, object, check-forward-refs? :: <boolean>) => ()
  //do nothing
end method;

define serious-program-warning <forward-binding-reference>
  slot condition-binding-name,
    required-init-keyword: binding-name:;
  slot condition-current-form, 
    required-init-keyword: current-form:;
  format-string    "Illegal forward reference to the binding %= from within %=.";
  format-arguments binding-name, current-form;
end serious-program-warning;

define method check-optimized-reference 
    (c :: <computation>, ref :: <binding-reference>, b :: <binding>,
     check-forward-refs? :: <boolean>) => ()
  if (check-forward-refs?)
    if(defined-after?(*current-dependent*, binding-definition(b)))
      let model = binding-model-or-hollow-object(b);
      if (~model)
        note(<forward-binding-reference>, 
          source-location: *current-dependent* & form-source-location(*current-dependent*),
          binding-name: binding-variable-name(b),
          current-form: *current-dependent*);
      end if;
    end if;
  end if;
end method;

define method check-optimized-reference 
    (c :: <computation>, ref :: <object-reference>, f :: <&function>,
     check-forward-refs? :: <boolean>) => ()
  if (model-compile-stage-only?(f) | inlined-inline-only-function?(f))
    let copy = find-inline-copy(current-compilation-record(), f);
    reference-value(ref) := copy
  end;
end method;

define method check-optimized-reference 
    (c :: <function-call>, ref :: <object-reference>, f :: <&generic-function>,
     check-forward-refs? :: <boolean>) => ()
  next-method();
  incf-dynamic-dispatch-count();
end method;

define method check-optimized-reference 
    (c :: <engine-node-call>, ref :: <object-reference>, e :: <&cache-header-engine-node>,
     check-forward-refs? :: <boolean>) => ()
  // format-out(">>> check-optimized-reference CHEN (%=) %= %= %=\n", object-class(c), c, ref, e);
  let f :: <&generic-function> = ^cache-header-engine-node-parent(e);
  if (model-compile-stage-only?(f) | inlined-inline-only-function?(f))
    let copy = find-inline-copy(current-compilation-record(), f);
    ^cache-header-engine-node-parent(e) := copy;
  end;
end method;

/*
define method check-optimized-computations (o :: <&lambda>)
  for-computations (c in o)
    check-optimized-computations(c)
  end;
end method;

define method check-optimized-computations (c :: <computation>)
end method;
*/

/*
define method check-optimized-computations (c :: <values>)
  format-out("Checking values: %=\n", c);
  let effective-users = users-through-merges(c);
  if (every?(rcurry(instance?, <extract-single-value>), effective-users))
    format-out("All users of %= are inline\n", c);
    break("Stop");
    let effective-generators 
      = generators-through-merges(effective-users.first);
  end;
end method;

define method users-through-merges 
    (c :: <computation>) => (users :: <list>)
  collecting ()
    iterate walk (c* = c.temporary.users)
      for (c in c*)
        if (instance?(c, <merge>))
          walk(c.temporary.users);
        else
          collect(c);
        end;
      end;
    end;
  end;
end method;

define method generators-through-merges 
    (c :: <extract-single-value>) => (users :: <list>)
  collecting ()
    iterate walk (c = c.computation-value.generator)
      if (instance?(c, <binary-merge>))
        walk(c.merge-left-value.generator);
        walk(c.merge-right-value.generator);
      else
        collect(c);
      end;
    end;
  end;
end method;
*/

// An inlined inline-only function is one that's called from some function
// that is itself declared inline and so is a copy that has ended up
// inlined elsewhere. In that case, we have to copy again.

define method inlined-inline-only-function? 
    (m :: <&method>) => (well? :: <boolean>)
  ~model-has-definition?(m) & lambda-top-level?(m)
end method;

define method inlined-inline-only-function? 
    (gf :: <&generic-function>) => (well? :: <boolean>)
  if (~model-has-definition?(gf))
    // signal("*** Inlined inline-only generic %= encountered.", gf);
    #t
  end;
end method;

define method check-optimized-computations (c :: <function-call>)
  let f = function-value(c);
  if (f)
    /*
    if (inlined-inline-only-function?(f))
      format-out(">>> Inlined inline only: %=\n", f);
    end;
    */
    if (model-compile-stage-only?(f) | inlined-inline-only-function?(f))
      // format-out("Doing: %=\n", f);
      if (instance?(f, <&generic-function>))
        // format-out("Going for the copy...\n");
        let copy 
          = find-inline-copy
              (form-compilation-record(*current-dependent*), f);
        // format-out("Made the copy.\n");
        simplify-call-to-call-to-object!(c, copy);
      elseif (instance?(f, <&method>))
        // format-out("Going for the method copy...\n");
        let copy 
          = find-inline-copy
              (form-compilation-record(*current-dependent*), f);
        // format-out("Made the copy.\n");
        simplify-call-to-call-to-object!(c, copy);
      else
	// break("Function in function-call is %=", f);
	#f
      end;
    end;
  end;
end method;

//// Generic function copying.

/*
define class <inline-only-copier> (<copier>) end class;

define dont-copy-object <source-location>    using <inline-only-copier>;
define dont-copy-object <fragment>           using <inline-only-copier>;
define dont-copy-object <signature-spec>     using <inline-only-copier>;
define dont-copy-object <&class>             using <inline-only-copier>;
define dont-copy-object <environment>        using <inline-only-copier>;
define dont-copy-object <computation>        using <inline-only-copier>;
define dont-copy-object <object-reference>   using <inline-only-copier>;
define dont-copy-object <binding>            using <inline-only-copier>;
// define dont-copy-object <&iep>               using <inline-only-copier>;
define dont-copy-object <&gf-iep>            using <inline-only-copier>;
define dont-copy-object <&mep>               using <inline-only-copier>;
define dont-copy-object <&engine-node>       using <inline-only-copier>;
define dont-copy-object <optimization-queue> using <inline-only-copier>;
define dont-copy-object <mapped-unbound>     using <inline-only-copier>;

define dont-copy-slots  <model-properties> using <inline-only-copier> =
  { private-model-creator => (*current-dependent* |
     error("Attempt to copy a model outside of proper compilation-context")),
    private-model-definition => #f };

define dont-copy-slots  <&generic-function> using <inline-only-copier> =
  { %generic-function-methods => #() };

define dont-copy-slots  <emitted-object> using <inline-only-copier> =
  { emitted-name      => #f,
    emitted-type-name => #f };

define dont-copy-slots  <&domain> using <inline-only-copier> =
  { domain-next => #f } ;

define method deep-copy
    (copier :: <inline-only-copier>, object :: <&iep>)
 => (copy :: <&iep>)
  // break("Copying iep");
  let copy = next-method();
  // break("Copied iep");
  copy
end method;

define method deep-copy
    (copier :: <inline-only-copier>, object :: <&mep>)
 => (copy :: <&mep>)
  next-method();
end method;

define method deep-copy
    (copier :: <inline-only-copier>, object :: <&generic-function>)
 => (copy :: <&generic-function>)
  let copy = next-method();
  %generic-function-methods(copy) 
    := maybe-do-deep-copy(copier, ^generic-function-methods-known(object));
  %generic-function-methods-initialized?(copy) 
    := #t;
  %generic-function-domains(copy)
    := maybe-do-deep-copy(copier, ^generic-function-domains(object));
  %generic-function-domains-initialized?(copy)
    := #t;
  copy
end method;



define method deep-copy
    (copier :: <inline-only-copier>, object :: <&method>)
 => (copy :: <&method>)
  let copy = next-method();
  /*
  if (copy ~== object)
    break("Copied %=", object);
    run-optimizations(copy);
  end;
  */
  copy
end method;
*/

// We want to copy a complete method so we use the standard dfm copier
// except we force the top level method to be copied, even though it
// has a definition.

define class <inline-only-copier> (<dfm-copier>) end;

define dont-copy-object <signature-spec> using <inline-only-copier>;
// define dont-copy-object <&engine-node>   using <inline-only-copier>;
define dont-copy-object <&absent-engine-node>   using <inline-only-copier>;
define dont-copy-object <unknown>        using <inline-only-copier>;
define dont-copy-object <&engine-node-ep> using <inline-only-copier>;

define method deep-copy 
    (copier :: <inline-only-copier>, object :: <&engine-node>) => (value)
  if (instance?(object, <&cache-header-engine-node>))
    let copy = next-method();
    if (copy ~== object)
      // Installs new entry point objects.
      initialize(object);
    end;
    copy
  else
    object;
  end;
end method;

/* BOLLOX: Pay no attention to this...

// If we do copy something with a definition, remove it so that it
// looks local. This will only apply to the forced generic function
// and method object copies.

define dont-copy-slots <model-properties> using <inline-only-copier> =
  { model-definition => #f };

define method walker-shallow-getters 
    (walker_ :: <inline-only-copier> , x_ :: subclass (<model-properties>))
 => (res :: <sequence>)
  next-method();
end method;

*/

define dont-copy-slots  <&generic-function> using <inline-only-copier> =
  { %generic-function-methods => #() };

define dont-copy-slots  <&lambda> using <inline-only-copier> =
  { private-lambda-heap => #f };

// TODO: This is very bad because of the number of non-modeled vectors
// in the DFM representation.
define method deep-copy
    (copier :: <inline-only-copier>, object :: <simple-object-vector>)
 => (copy :: <simple-object-vector>)
  if (model-has-definition?(object))
    object
  else
    let copy = next-method();
    copy
  end;
end method;

define method force-copy-method-into
    (table :: false-or(<table>), 
       copier :: <inline-only-copier>, m :: <&method>) 
 => (m :: <&method>)
  let copy = maybe-do-deep-copy(copier, m);
  when (table)
    element(table, m) := copy;
  end when;
  model-definition(copy) := #f;
  copy
end method;

define method force-copy-method-into
    (table :: false-or(<table>), 
       copier :: <inline-only-copier>, m :: <&lambda>) 
 => (m :: <&method>)
  let copy = next-method();
  ensure-lambda-body(m);
  // Also need to force copy this top level method's iep and mep and 
  // correct their back-pointers.
  copy.^iep          := maybe-do-deep-copy(copier, m.^iep);
  copy.^iep.function := copy;
  if (instance?(m.^mep, <&shared-entry-point>))
    copy.^mep          := m.^mep;
  else
    copy.^mep          := maybe-do-deep-copy(copier, m.^mep);
    copy.^mep.function := copy;
  end if;
  ensure-method-dfm(copy);
  walk-lambda-computations
    (method (c) item-status(c) := $queueable-item-absent; end, copy.body); 
  if (model-library(m) ~== model-library(copy))
    // force reoptimization if in another library 
    // otherwise avoid nasty reoptimization race conditions
    lambda-optimized?(copy) := #f;
  end if;
  // format-out("  !!! Really running passes on %= optimized? %=\n",
  //            copy, lambda-optimized?(copy));
  if (lambda-optimized?(copy))
    // recheck references
    check-optimized-computations(copy);
  else
    really-run-compilation-passes(copy);
  end if;
  if (~copy.environment)
    break("Bogus!!! copy of %=", m);
  end;
  copy
end method;
 
define method force-copy-domain-into
    (table :: false-or(<table>), 
       copier :: <inline-only-copier>, x :: <&domain>) 
 => (x :: <&domain>)
  let copy = maybe-do-deep-copy(copier, x);
  copy.model-definition := #f;
  copy
end method;
 
define method make-inline-copy-in
    (table :: <table>, gf :: <&generic-function>)
 => (new-gf :: <&generic-function>)
  let copier = make(<inline-only-copier>);
  // Force a copy of the generic function.
  let gf-copy = maybe-do-deep-copy(copier, gf);
  // This registration has to be done early to avoid recursively trying to
  // make a new inline copy is seen again during reoptimization.
  element(table, gf) := gf-copy;
  // bottom out copying for rescanning of copied code
  element(table, gf-copy) := gf-copy;
  gf-copy.model-definition := #f;
  // Force a copy of each method.
  %generic-function-methods(gf-copy) 
    := map(curry(force-copy-method-into, #f, copier), 
	   ^generic-function-methods-known(gf));
  %generic-function-methods-initialized?(gf-copy) 
    := #t;
  // Force a copy of each domain.
  %generic-function-domains(gf-copy) 
    := map(curry(force-copy-domain-into, #f, copier), 
	   ^generic-function-domains-known(gf));
  %generic-function-domains-initialized?(gf-copy) 
    := #t;
  gf-copy
end method;

define method make-inline-copy-in
    (table :: <table>, m :: <&method>) => (new-m :: <&method>)
  let copier = make(<inline-only-copier>);
  // Force a copy of the method.
  force-copy-method-into(table, copier, m)
end method;

define function find-inline-copy 
    (record :: <compilation-record>, f :: <&function>)
 => (local-copy :: <&function>)
  let table = compilation-record-inline-only-table(record);
  let existing = element(table, f, default: #f);
  if (existing)
    // format-out(">>> Using local copy: %=\n", existing);
    existing
  else
    // format-out("  >>> Making local copy in %=: %=\n", record, f);
    let copy = make-inline-copy-in(table, f);
    // format-out("  <<< Made local copy: %=\n", f);
    copy
  end
end function;
