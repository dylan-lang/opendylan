Module:   dfmc-definitions
Synopsis: Shared code between define constant and define variable.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// TODO: SHOULD BE MOVED TO TOP-LEVEL-FORMS WHEN MERGED

define compiler-sideways sealed domain make (subclass(<form-properties>));
define compiler-sideways sealed domain initialize (<form-properties>);
define compiler-sideways sealed domain initialize-packed-slots (<form-properties>);

define dood-class <binding-defining-form>  /* abstract */
    (<explicitly-typed-variable-defining-form>)
  lazy constant slot form-bindings-spec,
    required-init-keyword: bindings-spec:;
  lazy slot form-init-expression,
    required-init-keyword: init-expression:;
end dood-class;

define method form-variable-names
    (form :: <binding-defining-form>) => (names :: <sequence>)
  let name-or-names = form-variable-name-or-names(form);
  if (instance?(name-or-names, <variable-name-fragment>))
    list(name-or-names)
  else
    name-or-names
  end;
end method;

define method form-defined-bindings
    (form :: <binding-defining-form>) => (bindings :: <sequence>)
  choose(method (binding)
	   form == untracked-binding-definition(binding, default: #f)
	 end,
	 map(untracked-lookup-binding, form.form-variable-names))
end;

define method form-variable-name
    (form :: <binding-defining-form>) => (name :: <variable-name-fragment>)
  let name-or-names = form-variable-name-or-names(form);
  if (instance?(name-or-names, <variable-name-fragment>))
    name-or-names
  else
    name-or-names.first
  end
end method;


define sideways method form-ignored? (form :: <top-level-form>)
  (form.form-top-level-installed? & form.form-ignored-internal?)
    | begin
	let parent :: false-or(<top-level-form>) = form-parent-form(form);
	parent & form-ignored?(parent)
      end
end method;


define method form-ignored-internal? (form :: <binding-defining-form>)
  ~any?(method (v) form-defines-variable?(form, v) end,
	form.form-variable-names)
end method;

define class <literal-value-binding-defining-form> (<binding-defining-form>)
end class;

define method model-variable-using-definition
    (defn :: <binding-defining-form>, model-object) => (variable-name)
  let name-or-names = form-variable-name-or-names(defn);
  if (instance?(name-or-names, <variable-name-fragment>))
    name-or-names
  else
    any?(method (name)
	   let binding = untracked-lookup-binding(name);
	   let (binding-model, computed?)
	     = untracked-binding-model-object-if-computed(binding);
	   computed? & binding-model == model-object & name
	 end,
	 name-or-names)
      | error("Can find variable for model %s", model-object);
  end;
end;

// eof
