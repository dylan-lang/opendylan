Module: dfmc-typist
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//gts,private-compilation-record-top-level-forms :=
//gts,   access(dfmc-common, private-compilation-record-top-level-forms);

//gts, define constant call-site-summaries-setter = access(dfmc-modeling, call-site-summaries-setter);

//define constant run-pass = access(dfmc-management, run-pass);

define constant pass = as(<compilation-pass>, "eliminate-assignments");
define function box-it(fn)
  if (fn.body)
    eliminate-assignments(fn)
  end;
end;


define function recomp!(lib)
  compile-library(lib, force-compile?: #t);
end;

define function recomp(lib)
  let ld = lookup-library-description(lib);
  update-library-definitions(ld);
  compile-library-until-models(ld);
  compile-library-to-dfm(ld);
  type-library(ld); 
end;

define method ensure-lambda(fn :: <&lambda>, lib)
  fn
end;

define method ensure-lambda(fn :: <symbol>, lib)
  value-in(fn, lib);
end;

define function type-it(fn, lib)
  with-library-context (lookup-library-description (lib))
    without-dependency-tracking
      dynamic-bind (*outside-compiler?* = #t)
        let f = ensure-lambda(fn, lib);
        box-it(f);
        let css = get-call-site-summary(f, default-arg-types(f));
        let (#rest r) = do-type-infer-lambda(f, css);
        format-out("%s\n", f);
        r;
      end;
    end;
  end;
end;
  
define function val (sym, module, #key lib = module)
  with-library-context (lookup-library-description (lib))
    value-in(sym, module, lib);
  end;
end;


define variable *summary-counts* = make(<stretchy-vector>);

define function count-summaries(lib)
  let l = lookup-library-description (lib);
  *summary-counts*.size := 0;
  with-library-context (l)
    for (comp-rec in library-description-compilation-records(l))
      format-out("%s\n", comp-rec);
      for (tlf in private-compilation-record-top-level-forms(comp-rec))
        count-form(tlf)
      end;
    end;
  end;  
  let counts = sort!(*summary-counts*, test: method(x,y) x.tail.head < y.tail.head end);
  for (lc in counts) format-out("%= has %= [%=] summaries\n", 
    lc.head, lc.tail.head, lc.tail.tail) end;
end;

define method count-form(form)
end;

define method count-form
    (form :: <binding-defining-form>) => ();
  with-dependent($compilation of form)
    if (form-init-method(form))
      count-method(form-init-method(form));
    end;
  end;
end;

define method count-form (o :: <method-defining-form>)
  count-method(form-model(o));
end;

define method count-form (form :: <constant-method-definition>)
  with-dependent($compilation of form)  
    let form-model = 
      lookup-model-object(form-variable-name(form), default: #f);
    if (form-model) count-method(form-model); end;
  end;
end;

define method count-method(m)
end;

define method count-method(l :: <&lambda>)
  let summaries = l.call-site-summaries;
  let total = 0;
  let zapped = ~(l.body);
  unless(l.body)
    regenerate-dfm-for(l)
  end;
  for-all-lambdas (f in l)
    total := total + f.call-site-summaries.size;    
  end for-all-lambdas;
  add!(*summary-counts*, pair(l, pair(total, summaries.size)));

//gts,  if (zapped) access(dfmc-conversion, retract-method-dfm)(l) end;
 /* for (css in summaries)
    for (callee in css.callees)
      unless (callee == css)
        count-method(callee.css-lambda);
      end;
    end;
  end;
*/
end;


define method count-bindings (description) => ()
  let description = lookup-library-description(description);
  let library = language-definition(description);
  let library-def = library.namespace-definition;
  for (module in defined-modules-in(library))
    let module-def = if (instance?(module, <dylan-user-module>))
		       // no explicit definition, so use the library...
		       library-def
		     else
		       module.namespace-definition
		     end;
      for (binding in module.definitions)
        let table = binding.type-dependencies;
        let count = 0; 
        let max-l = 0;
        for (l in table)
          let s = l.size;
          count := count + s;
          max-l := max(max-l, s);
        end;
        unless (count = 0)
          format-out("%= [%=, %=, %=]\n", binding, count, table.size, max-l);
        end
      end for;
  end for;
end method;





define function type-lib(lib)
  *object-type* := #f;
  *values-rest-type* := #f;
  *false-type* := #f;
  *true-type* := #f;
  *sov-type* := #f;
  *rest-variable-type* := #f;
  *singleton-cache* := make(<table>);
  *raw-singleton-cache* := make(<table>);
  
  set-up-instance?-methods-for-if-inference();
  $=-sealed-domains := #f;
  set-up-eq-methods-for-if-inference();

  dynamic-bind (*outside-compiler?* = #t)
    let l = lookup-library-description (lib);
    library-type-estimate-disjoint?-cache(l) := make(<pair-type-key-table>);
    with-library-context (l)
      for (comp-rec in library-description-compilation-records(l))
        format-out("%s\n", comp-rec);
        for (tlf in private-compilation-record-top-level-forms(comp-rec))
          type-form(tlf);
        end;
      end;
    end;
  end;
end;


define method type-form(o)
end;

define method type-form
    (form :: <binding-defining-form>) => ();
  when(form-init-method(form))
    type-method(form-init-method(form));
  end;
end;

define method type-form (o :: <method-defining-form>)
  type-method(form-model(o));
end;

define method type-form (form :: <constant-method-definition>)
  when(form-init-method(form))
    type-method(form-init-method(form));
  end;
  type-method(form-model(form));
end;

define method type-form (o :: <function-defining-form>)
end;

define method type-method(m)
end;



define variable *typist-errors* = make(<stretchy-vector>);

define variable *strange-results* = make(<stretchy-vector>);

define variable *print-results?* = #f;

define method type-method(l :: <&lambda>)
  if (l.body)
    let lib = l.model-creator.form-compilation-record.compilation-record-library | lookup-library-description (#"dylan");
    with-library-context (lib)
      let css = get-call-site-summary(l, default-arg-types(l));
      let (#rest r) = do-type-infer-lambda(l, css);
      dynamic-bind(*print-method-bodies?* = #f)
        if (*print-results?*) format-out("%s -> %s\n", l, r[0]); end;
      end;
      without-dependency-tracking
        let s = l.^function-signature;
        let t = make(<values-type>, 
                     types: ^signature-values(s),
                     rest-type: ^signature-rest-value(s));
        unless (^subtype?(r[0], t))
          if (*print-results?*) format-out("\n\n*** Wrong ***\n\n"); end;
          add!(*typist-errors*, l);
        end;
        if (^subtype?(r[0], dylan-value(#"<bottom>")))
          add!(*strange-results*, l);
        elseif 
          (~empty?(r[0].fixed-types) & 
           ^subtype?(r[0].fixed-types[0], dylan-value(#"<boolean>")) &
           ( empty?(^signature-values(s)) |
           ~^subtype?(^signature-values(s)[0], dylan-value(#"<boolean>"))))
          if (*print-results?*) format-out("\n *** STRANGE!! \n"); end;
          add!(*strange-results*, l);
        end;
      end;
      l;
    end;
  end;
end;

// gts, new, based on above.
define method check-inferred-type(l :: <&method>,  t :: <&type>) => ()
  format-out("checking %s -> %s...\n", l, t);
  without-dependency-tracking
    if (instance?(t, <&raw-type>)
          | (instance?(t, <values-type>) 
               & any?(rcurry(instance?, <&raw-type>), fixed-types(t))))
      format-out("raw type, not checking\n");
    else
      let s = l.^function-signature;
      let num-req = ^signature-number-values(s);
      let sig-t = make(<values-type>, 
		       types: copy-sequence(^signature-values(s), start: 0, end: num-req),
		       rest-type: ^signature-rest-value(s));
      if (~^subtype?(t, sig-t))
	format-out("\n\n *** inferred type %= is not a subtype of declared type %= ***\n",
		   t, sig-t);
	add!(*typist-errors*, l);
      elseif (^subtype?(t, dylan-value(#"<bottom>")))
	format-out("\n\n*** inferred type %= is a subtype of bottom *** \n\n", t);
	add!(*strange-results*, l);
      else
	format-out("ok\n");
      end;
    end if;
  end;
end method;

//gts, lookup-library-description := access(dfmc-debug, lookup-library-description);

define function clean-lib(lib)
  *typist-errors*.size := 0;
  *strange-results*.size := 0;
  clean-bindings(lib);
  let l = lookup-library-description (lib);
  with-library-context (l)
    for (comp-rec in library-description-compilation-records(l))
      for (tlf in private-compilation-record-top-level-forms(comp-rec))
        clean-form(tlf);
      end;
    end;
  end;
  if (lib == #"dylan") reset-basic-types() end;
  reset-type-caches();
end;

define method clean-form(o)
end;

//gts, binding-cached-model-object := access(dfmc-namespace, binding-cached-model-object);

define method clean-form(form :: <generic-definition>)
  with-dependent($compilation of form)  
    let gf = binding-cached-model-object(lookup-binding(form.form-variable-name));
    when (instance?(gf, <&generic-function>))
      gf.effective-method-cache := make(<call-site-summary-table>);
      gf.dispatch-result-cache := make(<call-site-summary-table>);
    end;
  end;
end;

//gts, generate-binding-defining-fragment := access(dfmc-conversion, generate-binding-defining-fragment);

define method clean-form
    (form :: <binding-defining-form>) => ();
  with-dependent($compilation of form)
    if (form-init-method(form))
      clean-method(form-init-method(form));
/*    else
      let names = form-variable-names(form);
      let init-model =
        convert-top-level-initializer
        (generate-binding-defining-fragment(form),
         debug-name: generate-variable-names-debug-name(names));
      maybe-compute-and-install-method-dfm(init-model);
      form-init-method(form) := init-model;
*/
    end;
  end;
end;

define method clean-form (o :: <method-defining-form>)
  clean-method(form-model(o));
end;

define method clean-form (form :: <constant-method-definition>)
  with-dependent($compilation of form)  
    let form-model = 
      lookup-model-object(form-variable-name(form), default: #f);
    if (form-model) clean-method(form-model); end;
  end;
end;

define method clean-method(m)
end;


define method clean-up-type-info (css :: <call-site-summary>) end;

//gts, define constant queue-front = access(dfmc-flow-graph, queue-front);
//gts, define constant queue-back = access(dfmc-flow-graph, queue-back);
//gts, define constant queue-front-setter = access(dfmc-flow-graph, queue-front-setter);
//gts, define constant queue-back-setter = access(dfmc-flow-graph, queue-back-setter);

define method clean-up-type-info (obj)
  obj.inferred-type := typist-<unknown-type>();
  where := obj.type-dependencies;
  where.queue-front := #f;
  where.queue-back := #f;
end;

define method clean-up-type-info (obj :: <module-binding>)
  obj.inferred-type := typist-<unknown-type>();
  where := obj.type-dependencies;
  remove-all-keys!(where);
end;

/*
define method clean-method(l :: <&lambda>)
  when (l.call-site-summaries.size & l.call-site-summaries.size > 0)
    css := get-call-site-summary(l, (default-arg-types(l)));
    map(clean-up-type-info, css.introduced-dependencies);
  end;
  l.call-site-summaries := make(<call-site-summary-table>); 
  for (env in l.environment.inners)
    clean-method(env.lambda);
  end;
end;
*/

define method clean-method(l :: <&lambda>)
  let summaries = l.call-site-summaries;
  l.call-site-summaries := make(<call-site-summary-table>);
  for (css in summaries)
    unless (css.compressed?)
      map(clean-up-type-info, css.introduced-dependencies);
    end;
/*    for (callee in css.callees)
      unless (callee == css)
        clean-method(callee.css-lambda);
      end;
    end;
*/
  end;
end;



define function clean (table)
  for (c in table)
     c.lambda.call-site-summaries := make(<call-site-summary-table>);
  end;
end;

define function clean-function (name, module)
  value-in(name, module).call-site-summaries := make(<call-site-summary-table>);
end;

define function box-lib(lib)
  let l = lookup-library-description (lib);
  with-library-context (l)
    for (comp-rec in library-description-compilation-records(l))
      for (tlf in private-compilation-record-top-level-forms(comp-rec))
        box-form(tlf);
      end;
    end;
  end;
end;

define method box-form(o)
end;

define method box-form
    (form :: <binding-defining-form>) => ();
  with-dependent($compilation of form)
    if (form-init-method(form))
      box-it(form-init-method(form));
    else
      let names = form-variable-names(form);
      let init-model =
        convert-top-level-initializer
        (generate-binding-defining-fragment(form),
         debug-name: generate-variable-names-debug-name(names));
      maybe-compute-and-install-method-dfm(init-model);
      form-init-method(form) := init-model;
      box-it(form-init-method(form));
    end;
  end;
end;

define method box-form (o :: <method-defining-form>)
  box-it(form-model(o));
end;

define method box-form (form :: <constant-method-definition>)
   with-dependent($compilation of form)
    if (form-init-method(form))
      box-it(form-init-method(form));
    else
      let names = form-variable-names(form);
      let init-model =
        convert-top-level-initializer
        (generate-binding-defining-fragment(form),
         debug-name: generate-variable-names-debug-name(names));
      maybe-compute-and-install-method-dfm(init-model);
      form-init-method(form) := init-model;
      box-it(form-init-method(form));
    end;
  end;
 box-it(form-model(form));
end;

*print-method-bodies?* := #t;

//disable-pass(#"allocate-registers");

//define function allocate-registers (#rest args) end;

//gts, do-with-library-description := access(dfmc-namespace, do-with-library-description);

define macro show
  { show ( ?x:body ) } 
   => { let lib = lookup-library-description (#"dylan");
        with-library-context (lib)
          with-library-description(lib)
            let (#rest result) = begin ?x end;
            *r* := result;
          end
        end }
end;

define function recomp-dylan ()
  recomp(#"dylan");
end;

//gts, lookup-named-project := access(dfmc-debug, lookup-named-project);

define function close-projects ()
  let dylan-project = lookup-named-project(#"dylan");
  let projects = as(<vector>,*all-open-projects*);
  for (proj in projects)
    unless (proj == dylan-project)
      close-project(proj);
    end;
  end;
end;

//gts, css-print := access(dfmc-debug-back-end, css-print);


define class <xxx> (<object>)
  slot foo = make(<table>);
end; 


define function clean-bindings(lib)
  let l = lookup-library-description (lib);
  with-library-context (l)
    for (comp-rec in library-description-compilation-records(l))
      for (tlf in private-compilation-record-top-level-forms(comp-rec))
        clean-binding(tlf);
      end;
    end;
  end;
end;

define method clean-binding(o)
end;

//gts, binding-cached-model-object := access(dfmc-namespace, binding-cached-model-object);

define method clean-binding(form :: <generic-definition>)
  with-dependent($compilation of form)  
    let binding = lookup-binding(form.form-variable-name);
//    binding.assignments := #();   
    binding.inferred-type := typist-<unknown-type>();
  end; 
end;

//gts, generate-binding-defining-fragment := access(dfmc-conversion, generate-binding-defining-fragment);

define method clean-binding
    (form :: <binding-defining-form>) => ();
  with-dependent($compilation of form)
    let binding = lookup-binding(form.form-variable-name);
//    binding.assignments := #();   
    binding.inferred-type := typist-<unknown-type>();
  end;
end;

define method clean-binding
    (form :: <variable-defining-form>) => ();
  with-dependent($compilation of form)
    let binding = lookup-binding(form.form-variable-name);
//    binding.assignments := #();   
    binding.inferred-type := typist-<unknown-type>();
  end;
end;

define method clean-binding
    (form :: <constant-definition>) => ();
  with-dependent($compilation of form)
    let binding = lookup-binding(form.form-variable-name);
//    binding.assignments := #();   
    binding.inferred-type := typist-<unknown-type>();
  end;
end;


define method clean-binding (form :: <constant-method-definition>)
  with-dependent($compilation of form)
    let binding = lookup-binding(form.form-variable-name);
//    binding.assignments := #();   
    binding.inferred-type := typist-<unknown-type>();
  end;
end;

define macro test-in-lib
  { test-in-lib (?lib:expression, ?x:body ) } 
   => { without-dependency-tracking
          with-library-context 
            ( project-current-compilation-context
                ( lookup-named-project(?lib )))
                    ?x
          end
        end }
end;
