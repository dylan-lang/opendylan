Module:   dfmc-conversion
Synopsis: Domain model operations.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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

define serious-program-warning <domain-types-not-valid>
  slot condition-variable-name,
    init-keyword: variable-name:;
  slot condition-problem,
    required-init-keyword: problem:;
  format-string "Domain types for %= are not valid - %s.";
  format-arguments variable-name, problem;
end serious-program-warning;

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
  if (gf)
    let (ok?, problem) = ^domain-types-match-generic?(d, gf);
    if (~ok?)
      note(<domain-types-not-valid>,
           source-location: model-source-location(d),
           variable-name:   model-variable-name(d),
           problem:         problem);
    end if;
  end if;
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

define function ^domain-types-match-generic?
    (d :: <&domain>, gf :: type-union(<&generic-function>, <&method>))
 => (ok? :: <boolean>, reason)
  block (return)
    let gf-sig = ^function-signature(gf);
    let gf-required = gf-sig.^signature-required;
    let domain-types = ^domain-types(d);

    // They have the same number of required arguments.
    if (gf-sig.^signature-number-required ~= size(domain-types))
      return(#f, "they don't have the same number of required arguments");
    end if;

    // Each of the domain's type specializers is a subtype of the
    // corresponding parameter specializer of the generic function.
    unless (every?(^subtype?, domain-types, gf-required))
      return(#f, "some of the method's required parameter specializers "
                 "aren't subtypes of their counterparts in the generic");
    end unless;

    values(#t, "valid")
  end block;
end;
