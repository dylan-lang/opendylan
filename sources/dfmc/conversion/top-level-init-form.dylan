module: dfmc-conversion
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-sideways method compute-and-install-form-model-objects
    (form :: <top-level-init-form>) => ()
  let init-model 
    = if (*interactive-compilation-layer*)
        convert-top-level-initializer-for-values(form-body(form));
      else
        convert-top-level-initializer(form-body(form));
      end;
  form-init-method(form) := init-model;
end method;

define method convert-top-level-initializer-for-values
    (form, #key debug-name = "top-level-initializer")
  let form 
    = #{ () => (values-vector) let (#rest results) = (?form); results };
  let model = convert-method-to-model(debug-name, form);
  model
end method;

// eof
