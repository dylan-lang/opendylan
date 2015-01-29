Module:   dfmc-definitions
Synopsis: Undefined variable handling.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define dood-class <missing-definition> (<missing-variable-defining-form>) end;

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <missing-definition>) => ()
  // Do nothing.
end method;

define method form-implicitly-defined?
    (form :: <missing-definition>) => (well? :: <boolean>)
  #t
end method;

define method binding-definition-missing?
    (binding :: <module-binding>) => (well? :: <boolean>)
  let def = untracked-binding-definition(binding, default: #f);
  if (def)
    note-binding-dependency(binding, dep$defined?);
    instance?(def, <missing-definition>)
  elseif (binding-imported-into-library?(binding))
    ~binding-defined?(binding) // stripped database
  else
    install-missing-definition(binding);
    #t
  end;
end method;

define method install-missing-definition
   (binding :: <module-binding>) => ()
  let name = binding-variable-name(binding);
  let missing-def
    = make(<missing-definition>,
           source-location:
             *current-dependent* & form-source-location(*current-dependent*),
           variable-name:   name,
           adjectives:      #[]);
  add-derived-top-level-forms
    (current-compilation-record(), list(missing-def));
  add-missing-definition(name, missing-def);
  form-top-level-installed?(missing-def) := #t;
  // Note the dependency after installing in order to avoid unnecessary
  // change triggering.
  note-binding-dependency(binding, dep$defined?);
end method;
