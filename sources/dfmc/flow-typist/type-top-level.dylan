Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method initialize-type-system-caches (ld :: <library-description>) => ()
  // When you make a <library-description>, install typist caches.   Done
  // this way because of module lossage, i.e. typist classes aren't visible
  // in dfmc-namespace, so couldn't do it with slot initializers. Sigh.
  library-type-estimate-disjoint?-cache(ld) := 
    // size: 4000 is a heuristic to avoid excessive rehashing when compiling
    // the Dylan library.  There should be a heuristic, based perhaps on the
    // library size, or something.
    make(<pair-type-key-table>, size: 4000);
  library-dispatch-result-cache(ld) := make(<dispatch-result-cache-table>);
end;

define sideways method initialize-type-system (ld :: <library-description>) => ()
  unless (empty?(ld.library-description-compilation-records))
    with-library-context (ld)  
      without-dependency-tracking
//        reset-type-caches();
        set-up-instance?-methods-for-if-inference();
        $eq-sealed-domains := #f;
        set-up-eq-methods-for-if-inference();
        initialize-type-system-caches (ld);
        initialize-special-actions();
        ld.library-type-cache := #f;
      end;
    end;
  end;
end;


define sideways method initialize-type-system (mb :: <module-binding>) => ()
  mb.inferred-type := typist-<unknown-type>();
  mb.type-dependencies := make(<table>);
end;


//define constant run-pass = access(dfmc-management, run-pass);
//define constant pass = as(<compilation-pass>, "eliminate-assignments");


define method eliminate-assignments (f :: <&lambda>)
  for (t in f.environment.temporaries)
    if (~empty?(t.assignments) & ~cell?(t))
      cell-assigned-temporaries(t);
    end if;
  end for;
end method eliminate-assignments;

define method cell-assigned-temporaries (t :: <temporary>)
  let (make-cell-c, cell) = convert-make-cell(t.environment, t);
  insert-computation-after!
    (t.generator | t.environment.lambda.body, make-cell-c);
  for (user in t.users)
    if (user ~== make-cell-c)
      let (get-c, get-t)
        = with-parent-computation (user)
            convert-get-cell-value(user.environment, cell);
          end;
      insert-computation-before-reference!(user, get-c, t);
      replace-temporary-references!(user, t, get-t);
    end if;
  end for;
  for (assignment in t.assignments)
    assert(assignment.assigned-binding == t);
    let val-t = assignment.computation-value;
    let (set-c, set-t)
      = with-parent-computation (assignment)
          convert-set-cell-value!(assignment.environment, cell, val-t);
        end;
    insert-computation-after!(assignment, set-c);
    replace-temporary-in-users!(assignment.temporary, val-t);
    delete-computation!(assignment);
    // Track cell writes
    cell.assignments := add!(cell.assignments, set-c); 
  end for;
  t.assignments := #(); // should this happen automatically?
end method cell-assigned-temporaries;

// Constructors for celling primitives.

define method convert-make-cell 
    (env :: <lambda-lexical-environment>, t :: <temporary>)
 => (c :: <computation>, t :: <cell>);
   with-parent-computation (t.generator)
     make-with-temporary
       (env, <make-cell>, value: t, temporary-class: <cell>); 
   end;
end method convert-make-cell;

define method convert-get-cell-value 
    (env :: <lambda-lexical-environment>, cell :: <cell>)
 => (c :: <computation>, t :: <temporary>)
   make-with-temporary(env, <get-cell-value>, cell: cell);
end method convert-get-cell-value;

define method convert-set-cell-value!
    (env :: <lambda-lexical-environment>, cell :: <cell>, 
     ref :: <value-reference>)
 => (c :: <computation>, t :: <temporary>)
   make-with-temporary(env, <set-cell-value!>, cell: cell, value: ref);
end method convert-set-cell-value!;

define macro for-all-lambs
  { for-all-lambs (?:variable in ?:expression) ?:body end }
    => { do-all-lambdas(method (?variable) ?body end, ?expression) }
end macro;

define function ensure-boxed(fn)
  when (fn.body)
    for-all-lambs(l in fn)
      when (l.body)
        eliminate-assignments(l);
      end;
    end;
  end;
end;

define method type-initializer-method(l :: <&lambda>)
  if (l.body)
    with-library-context (model-library(l))
      let css = get-default-call-site-summary(l);
      type-infer-lambda(l, css);
      process-delayed-typist-notes(css);
    end;
  end;
end;

define method type-all-lambdas (outer-lambda :: <&lambda>)
  when (outer-lambda.body)
    let local-css = get-default-call-site-summary(outer-lambda);
    let res-type = local-css.result-type;
    if ( local-css.compressed? 
       & ~instance?(res-type, <unknown-type>) ) // How can this happen?
      format-out("Uncompressing and retyping %=\n", local-css);
      local-css.result-type := typist-<unknown-type>();
    end;
    type-infer-lambda(outer-lambda, local-css);
  end;      
  let env = outer-lambda.environment;
  for (sub-e in env.inners)
    type-all-lambdas(sub-e.lambda);
  end;
end method;

define method type-top-level-method(l :: <&accessor-method>)
end;

define method type-top-level-method(l :: <&lambda>)
//    format-out("Typing %=\n", ^debug-name(l));
//    *KM-FLAG* := ^debug-name(l) = "%gf-dispatch-absent";

  if (l.body)
    with-library-context (model-library(l))
      type-all-lambdas(l);
      for-all-lambs (lambda in l) process-delayed-typist-notes(lambda) end;
    end;
/*
    We would like to be able to compress summaries as soon as we can.  In
    particular, after typing a top-level method has been typed.  Previously
    this was problematic because of the treatment of module bindings.  Any
    assignment to a module binding later on might force the summary to be
    retyped.  This had to be changed for other reasons, but we still have 
    problems in this area.  First, we don't want to compress anything that
    might be needed for inlining, as otherwise it will need to be retyped.
    It is difficult to know whether an inner lambda might be inlined at this
    point in the compilation process, so we can only retract summaries for
    outer lambdas that won't be inlined.  But the typing of an inner lambda 
    for a new summary will need typing info for the variables closed over.  At
    the moment it gets this by collecting over the enclosing summaries, so
    we need to keep at least the default enclosing summary - yuk.
    <exit> nodes probably introduce similiar complications.

    We need a better way to handle this...

    unless (method-inlineable?(l) | instance?(l, <&copy-down-method>))
      for (css in l.call-site-summaries)
        unless (instance?(css, <default-call-site-summary>))
          css.compressed? := #t
        end
      end;
    end
*/
  end;
end;


define method squash-summaries-for-top-level-form(o) => ()
end;

define method squash-summaries-for-top-level-form(form :: <method-definition>) => ()
  let lambda = form-model(form);
  let definition = model-definition(lambda);
  if (definition)
    let policy = form-inline-policy(definition);
    if (policy == #"not-inline" | policy == #"default-inline")
      // Might as well purge some of the unnecessary junk
      for (css in lambda.call-site-summaries)
        if (instance?(css, <specialized-call-site-summary>))
        end
      end;
    end
  end
end;
  
define method squash-summaries-for-top-level-form(form :: <constant-method-definition>) => ()
  let lambda = lookup-model-object(form-variable-name(form));
  let definition = model-definition(lambda);
  if (definition)
    let policy = form-inline-policy(definition);
    if ((policy == #"not-inline" | policy == #"default-inline") & instance?(lambda, <&lambda>))
      // Might as well purge some of the unnecessary junk
      for (css in lambda.call-site-summaries)
        if (instance?(css, <specialized-call-site-summary>))
        end
      end;
    end
  end
end;
  

/// This is the main interface called by the compile driver. It produces an 
/// initial typing of a form (if it doesn`t have one already).
define generic type-top-level-form(form  :: <top-level-form>)
  => ();

define macro type-top-level-form-rules-definer
  // Expand a bunch of rules into methods for type-top-level-form.
  { define type-top-level-form-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-top-level-form method.
  { ?tname:name :: ?typ:expression <- ?expr:expression }
  => { define method type-top-level-form
	   (?tname :: ?typ) => ()
         ?expr
       end }
end;

// To see what sorts of trouble you have to get into, do:
// (tools:make-class-browser :root (list (find-class 'dylan::<top-level-form>)))
//
// <modified-top-level-form> -- has adjectives
// <defining-form> -- "define ... end" (might be macro w/obscure semantics)
// <variable-defining-form> -- has names and installed? flag
//   <explicitly-typed-variable-defining-form> -- has seq type expressions (form-type-expression)
//     <binding-defining-form>
//       <variable-definition>
//       <constant-definition>
//         <constant-method-definition> (also under <primitive-definition>)
//
// form-model-object -- gets model object from variable def form + name.
// form-model


define type-top-level-form-rules
  // Rules for type-top-level-form.
  form :: <top-level-form>
    // Nothing
    <- #f;
  form :: <macro-call-form>
    // Do nothing, since these are only for browsers.
    <- #f;
  form :: <top-level-init-form>
    // Non-definition top-level forms are misc inits -- get init method & type it.
    <- type-initializer-method(form-init-method(form));
  form :: <namespace-defining-form>
    // Do nothing for <module-definition>s and <library-definition>s.
    <- #f;
  form :: <function-defining-form>
    // *** Extract signature?
    <- #f;
  form :: <method-definition>
    // Assumes we've seen constant-method-definitions already
    <- if (form-model(form))
         type-top-level-method(form-model(form)); // Look at code
//         shrink-summaries(form-model(form));
       else
         error("No model for %s", form);
       end;
  form :: <constant-method-definition>
    <- begin
         let form-model = lookup-model-object(form-variable-name(form));
         if (form-model)
           type-top-level-method(form-model); // Look at code
//           shrink-summaries(form-model);
         else
           error("No model for %s", form);
         end;
       end;
  // *** Bunch of other kinds of <top-level-form>s.
end;





