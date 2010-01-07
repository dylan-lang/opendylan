Module:   dfmc-conversion
Synopsis: Domain model operations.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define program-warning <domain-visible-to-sibling-libraries>
  slot condition-variable-name,
    init-keyword: variable-name:;
  format-string 
    "This domain on %= is visible to sibling libraries because the "
    "domain types are all based on imported classes.";
  format-arguments
    variable-name;
end program-warning;

define method check-model (d :: <&domain>) => ()
  let gf = ^domain-generic-function(d);
  // Leakage check.
  if (gf & ~single-method-generic-function?(gf)
         & ~(^generic-function-sideways?(gf) | ^domain-sideways?(d))
         & model-library(gf) ~== model-library(d)
         & all-types-known-imported?
             (model-library(d), ^domain-types(d)))
    note(<domain-visible-to-sibling-libraries>,
         source-location: model-source-location(d),
         variable-name:   model-variable-name(d));
  end;
end method;

define function ^domain-sideways? 
    (d :: <&domain>) => (well? :: <boolean>)
  form-sideways?(model-definition(d))
end function;

define function ^domain-generic-function (object :: <&domain>)
  let binding = model-variable-binding(object);
  let model = binding & binding-model-object(binding, default: #f);
  model
end function;
