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

/*
define function do-define-copy-down-method (fragment, mods, name, signature)
format-out ("in do-define-copy-down-method\n");
  // let (options, adjectives) = parse-method-adjectives (name, mods);
format-out ("copy-down-method signature is %s\n", signature) ;
  // maybe should check body is empty
  // ensure-next-method-binding (signature);  // not sure this useful
//  let  args-list = macro-case (signature) { (?args:*) ?other:* } => args end;
  let  method-form
    = #{ define ?mods \method ?name ?signature end };
  let  new-fragment = method-form.as-body ;
  let  expanded-forms = top-level-convert (fragment, new-fragment) ;
format-out ("expanded forms for copy-down method %s\n", expanded-forms) ;
  let  interesting-form = first (expanded-forms) ;
  interesting-form.form-class := #"copy-down-method" ;
  expanded-forms
end;
*/
define function do-define-copy-down-method (fragment, mods, name, signature)
// format-out ("in do-define-copy-down-method\n");
  let (options, adjectives) = parse-method-adjectives (name, mods);
//  let signature-and-body = make (<sequence-fragment>,
//                                 record: signature.fragment-record,
//                                 source-position: signature.fragment-source-position,
//                                 fragments:
//                                   concatenate (signature . fragment-fragments,
//                                                #{ #f }.template-fragments)) ;
  let signature-and-body = signature ;
  let (signature, body) = parse-method-signature(name, signature-and-body);
// format-out ("copy-down-method signature is %s\n", signature) ;
  ensure-next-method-binding (signature);
  let method-definition
    = apply(make, <method-definition>,
            source-location: fragment-source-location(fragment),
            variable-name:   name,
            adjectives:      adjectives,
            signature:       signature,
            body:            body,
            signature-and-body-fragment: signature,
            options);
  method-definition.form-class  := #"copy-down-method" ;
  if (form-sealed?(method-definition))
    let domain-fragment
      = generate-implicit-domain-definition-fragment(method-definition);
    let expanded-forms = top-level-convert(method-definition, domain-fragment) ;
    pair (method-definition, expanded-forms)
  else
    list (method-definition)
  end
end;
