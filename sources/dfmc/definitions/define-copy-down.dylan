Module:   dfmc-definitions
Synopsis: define copy down
Author:   Mark Tillotson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define &definition copy-down-method-definer
  { define ?mods:* \copy-down-method ?:name ?signature:* }
    => do-define-copy-down-method (form, mods, name, signature) ;
  { define ?mods:* \copy-down-method ?other:* }
    => begin
         note(<malformed-define-method>,
              source-location: form.fragment-source-location) ;
         #();
       end;
end;

define function do-define-copy-down-method (fragment, mods, name, sig-fragment)
  let (options, adjectives) = parse-method-adjectives (name, mods);
  let (signature, body) = parse-method-signature(name, sig-fragment);
  ensure-next-method-binding (signature);
  let method-definition
    = apply(make, <method-definition>,
            source-location: fragment-source-location(fragment),
            variable-name:   name,
            adjectives:      adjectives,
            signature:       signature,
            body:            body,
            signature-and-body-fragment: sig-fragment,
            options);
  method-definition.form-class := #"copy-down-method" ;
  if (form-sealed?(method-definition))
    let domain-fragment
      = generate-implicit-domain-definition-fragment(method-definition);
    let expanded-forms = top-level-convert(method-definition, domain-fragment);
    pair(method-definition, expanded-forms)
  else
    list(method-definition)
  end
end;
