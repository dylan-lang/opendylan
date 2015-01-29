Module: dfmc-conversion
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// The top type

define compiler-sideways method compute-form-model-object
    (form :: <top-type-definition>, name :: <variable-name-fragment>)
      => (model)
  ^make(<&top-type>,
        definition: form);
end method;

//// The bottom type

define compiler-sideways method compute-form-model-object
    (form :: <bottom-type-definition>, name :: <variable-name-fragment>)
      => (model)
  ^make(<&bottom-type>,
        definition: form);
end method;


//// Booted raw-types

define compiler-sideways method compute-form-model-object
    (form :: <raw-type-definition>, name :: <variable-name-fragment>)
      => (model)
  let supertype-name = form-supertype-name(form);
  let supertype-model =
    if (supertype-name)
      let model = ^top-level-eval(supertype-name);
      if (model)
        model
      else
        error("Undefined raw type supertype %s of %s",
              supertype-name, name);
        dylan-value(#"<raw-object>")
      end;
    else
      #f;
    end;
  ^make(<&raw-type>,
        definition: form,
        debug-name: as-lowercase(as(<string>, name)),
        supertype: supertype-model,
        descriptor-function: form-raw-type-descriptor-function(form));
end method;
