Module:    DFMC-Typist
Author:    Steve Rowley
Synopsis:  Typist interface for library compilation.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// This is the main interface called by the compile driver.  Called for 
/// effect on the cache, which is stored in the library description.
define generic type-estimate-top-level-form(form  :: <top-level-form>)
  => ();

define macro type-estimate-top-level-form-rules-definer
  // Expand a bunch of rules into methods for type-estimate-top-level-form.
  { define type-estimate-top-level-form-rules ?rules end } => { ?rules }
rules:
  // Body is ;-separated rules generating ;-separated methods.
  { }            => { }
  { ?rule; ... } => { ?rule; ... }
rule:
  // Each rule generates a type-estimate-top-level-form method.
  { ?tname:name :: ?typ:expression <- ?expr:expression }
  => { define method type-estimate-top-level-form (?tname :: ?typ) => ()
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

define variable *annoying-typist-chatter?* :: <boolean> = #f;

define function typist-chatter(format-str, #rest format-args)
  // For interim debugging.
  when (*annoying-typist-chatter?*)
    format-out("\n// *** type-estimate-top-level-form: ");
    apply(format-out, format-str, format-args)
  end
end;

define type-estimate-top-level-form-rules
  // Rules for type-estimate-top-level-form.
  form :: <top-level-form> 
    // Temporary punt method so things will work in the interim.
    <- typist-chatter("NOP'd on %=\n", form);
  form :: <macro-call-form>
    // Do nothing, since these are only for browsers.
    <- #f;
  form :: <top-level-init-form>
    // Non-definition top-level forms are misc inits -- get init method & type it.
    <- type-estimate(form-init-method(form));
  form :: <namespace-defining-form>
    // Do nothing for <module-definition>s and <library-definition>s.
    <- #f;
  form :: <function-defining-form>
    // *** Extract signature?
    <- typist-chatter("function defn %=\n", form);
  form :: <method-definition>
    <- if (form-model(form))
         type-estimate(form-model(form)) // Look at code
       else
         next-method()                          // Else punt like above
       end
  // *** Bunch of other kinds of <top-level-form>s.
end;
