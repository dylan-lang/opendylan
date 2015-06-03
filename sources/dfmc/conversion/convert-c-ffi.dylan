module: dfmc-conversion
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define program-error <invalid-c-name-option-value>
  slot condition-c-name-expression, init-keyword: c-name-expression:;
  format-string "The expression %=, supplied for the c-name keyword "
                "option, does not evaluate to a string constant.";
  format-arguments c-name-expression;
end;

define program-error <invalid-import-option-value>
  slot condition-import-expression, init-keyword: import-expression:;
  format-string "The expression %=, supplied for the import keyword "
                "option, does not evaluate to a boolean constant.";
  format-arguments import-expression;
end;

define method parse-expressions (expressions-form)
  macro-case (expressions-form)
    { ?exprs:* }
      => exprs;
  exprs:
    { }
      => #();
    { ?:expression, ... }
      => pair(expression, ...);
  end macro-case
end method;

define method parse-ffi-result-type (results-form)
  macro-case (results-form)
    { } => #{ nothing :: <raw-c-void> };
    { ?results:* } => #{ ?results };
  end
end method;

define method as-string (name :: <symbol>)
  as-lowercase(as(<string>, name))
end method;

define method as-string (name :: <string>)
  name
end method;

define method as-string (name :: <variable-name-fragment>)
  as-string(name.fragment-identifier)
end method;

define method as-string (name :: <string-fragment>)
  as-string(name.fragment-value)
end method;

define method convert-%c-variable-pointer
    (env :: <environment>, context :: <value-context>, name-expr, import-expr)
  let (first, call, temp)
    = convert-primitive-call(env, context, <c-variable-pointer-call>, #f, #());
  let name = ^top-level-eval(name-expr);
  unless(instance?(name, <string>))
    note(<invalid-c-name-option-value>,
         source-location: fragment-source-location(name-expr),
         c-name-expression: name-expr);
    name := "";
//    name := "dummy_variable_name";
  end unless;
  let import = ^top-level-eval(import-expr, on-failure: #"error");
  unless(instance?(import, <boolean>))
    note(<invalid-import-option-value>,
         source-location: fragment-source-location(import-expr),
         import-expression: import-expr);
    import := #f;
  end unless;
  call.c-variable := make(<&c-variable>, name: name, import: import);
  values(first, call, temp)
end method;

define &converter %c-variable-pointer
  { %c-variable-pointer (?name:expression, ?import:expression) }
    => convert-%c-variable-pointer(env, context, name, import)
end &converter;

define method convert-%c-call-function
    (env :: <environment>, context :: <value-context>,
     name, signature, arguments, modifiers)
  let sig-spec = parse-primitive-signature(name, signature);
  let (ffi-signature, signature) = make-ffi-signature(sig-spec);
  let function
    = make(<&c-function>,
           c-function-name: name & as-string(name),
           c-signature: ffi-signature,
           signature: signature,
           c-modifiers: as-string(modifiers));
  convert-primitive-call(env, context, <primitive-call>, function, arguments);
end method;

define &converter %call-c-function
  { %call-c-function (?name:expression, #key ?c-modifiers:expression = "")
        (?parameters:*) => (?results:*)
      (?arguments:*) end }
  => begin
       let results = parse-ffi-result-type(results);
       convert-%c-call-function (env, context, name,
                                 #{ (?parameters) => (?results) },
                                 parse-expressions(arguments),
                                 as-string(c-modifiers));
     end
end &converter;

define method convert-%c-call-function-indirect
    (env :: <environment>, context :: <value-context>,
     signature, arguments, modifiers)
  let sig-spec = parse-primitive-signature(#f, signature);
  let (ffi-signature, signature) = make-ffi-signature(sig-spec);
  let function
     = make(<&c-function>,
            c-function-name: #f,
            signature: signature,
            c-signature: ffi-signature,
            value: #f,
            c-modifiers: as-string(modifiers));
  convert-primitive-call(env, context, <primitive-indirect-call>, function, arguments);
end method;

define &converter %call-c-function-indirect
  { %call-c-function-indirect (#key ?c-modifiers:expression = "")
        (?fparam:*, ?parameters:*) => (?results:*)
      (?arguments:*) end }
  => begin
       let results = parse-ffi-result-type(results);
       // We peel off and ignore the indirect function pointer parameter spec,
       // but leave the argument, leaving the contract the back end expects.
       convert-%c-call-function-indirect
         (env, context,
          #{ (?parameters) => (?results) },
          parse-expressions(arguments),
          c-modifiers);
     end
end &converter;

define method convert-%c-callable-function
    (env :: <environment>, context :: <value-context>,
     name, form, modifiers, other-name, export)
  let (signature-spec, body) = parse-method-signature(name, form);
  let signature
    = compute-signature(#f, signature-spec);
  let body
    = as-body(body);
  let model
    = compute-method-explicitly
        (<&c-callable-function>, #f, #f, signature-spec, body,
         c-function-name: ^top-level-eval(name),
         c-modifiers: ^top-level-eval(modifiers),
         alternate-name: as-string(other-name),
         export: export.fragment-value);
  ^function-signature(model) := signature;
  convert-lambda-into*(env, model, body);
  convert-object-reference(env, context, model.^iep)
end method;

define &converter %c-callable-function
  { %c-callable-function (?name:expression,
                          #key ?c-modifiers:expression = "",
                               ?other-name:expression = "not-given",
                               ?export:expression = #f)
       ?method-form:*
    end }
  =>
  convert-%c-callable-function
    (env, context, name, method-form, c-modifiers, other-name, export);
end &converter;

define method convert-%objc-msgsend
    (env :: <environment>, context :: <value-context>,
     signature, arguments, modifiers)
  let sig-spec = parse-primitive-signature(name, signature);
  let (ffi-signature, signature) = make-ffi-signature(sig-spec);
  let modifiers = as-string(modifiers);
  let function
    = make(<&objc-msgsend>,
           c-function-name: format-to-string("objc_msgSend", modifiers),
           c-signature: ffi-signature,
           signature: signature,
           c-modifiers: modifiers);
  convert-primitive-call(env, context, <primitive-call>, function, arguments);
end method;

define &converter %objc-msgsend
  { %objc-msgsend (?target:expression, ?selector:expression, #key ?c-modifiers:expression = "")
        (?parameters:*) => (?results:*)
      (?arguments:*) end }
  => begin
       let results = parse-ffi-result-type(results);
       convert-%objc-msgsend (env, context,
                              #{ (target :: <raw-machine-word>, selector :: <raw-machine-word>, ?parameters) => (?results) },
                              pair(target, pair(selector, parse-expressions(arguments))),
                              as-string(c-modifiers));
     end
end &converter;



define compiler-sideways method compute-and-install-form-model-objects
    (form :: <raw-aggregate-definition>) => ()
  let variable-name = form-variable-name(form);
  let model = ^make(raw-aggregate-model-class(form),
                    definition: form,
                    debug-name: mapped-model(as-lowercase(as(<string>, variable-name))),
                    members: mapped-model(map(compute-raw-aggregate-member,form-members(form))),
                    options: mapped-model(map(^top-level-eval, form.form-options)));
  define-model-object(variable-name, model);
end method;

define method raw-aggregate-model-class (form :: <raw-struct-definition>)
 => (model-class)
  <&raw-struct-type>
end;

define method raw-aggregate-model-class (form :: <raw-union-definition>)
 => (model-class)
  <&raw-union-type>
end;

// special method for <&c-callable-function>

define method convert-lambda-into*
    (env :: <environment>, f :: <&c-callable-function>, the-body,
     #key multiple-values? = #f)
  let sig-spec = f.signature-spec;
  let (ffi-signature, signature) = make-ffi-signature(sig-spec);
  f.c-signature := ffi-signature;
  next-method(env, f, the-body, multiple-values?: #f);
end method convert-lambda-into*;

// Signature parsing.

define program-warning <next-method-in-define-primitive>
  constant slot condition-primitive-name,
    required-init-keyword: primitive-name:;
  format-string
    "#next specified in the parameter list of the primitive %= "
    "- ignoring";
  format-arguments
    primitive-name;
end program-warning;

define program-warning <key-variable-in-define-primitive>
  constant slot condition-primitive-name,
    required-init-keyword: primitive-name:;
  format-string
    "#key specified in the parameter list of the primitive %= "
    "- ignoring";
  format-arguments
    primitive-name;
end program-warning;

define program-warning <all-keys-in-define-primitive>
  constant slot condition-primitive-name,
    required-init-keyword: primitive-name:;
  format-string
    "#all-keys specified in the parameter list of the primitive %= "
    "- ignoring";
  format-arguments
    primitive-name;
end program-warning;

define method parse-primitive-signature (name, sig-fragment) => (sig-spec)
  let sig-spec
    = parse-signature-as
        (<primitive-signature-spec>, sig-fragment);
  verify-primitive-signature-spec(name, sig-spec, sig-fragment);
  sig-spec
end method;

define method verify-primitive-signature-spec (name, sig :: <primitive-signature-spec>, sig-fragment)
  if (spec-argument-next-variable-spec(sig))
    note(<next-method-in-define-primitive>,
         source-location: fragment-source-location(sig-fragment),
         primitive-name:  name);
  end;
  if (spec-argument-key?(sig))
    note(<key-variable-in-define-primitive>,
         source-location: fragment-source-location(sig-fragment),
         primitive-name:  name);
  end;
  if (spec-argument-all-keys?(sig))
    note(<all-keys-in-define-primitive>,
         source-location: fragment-source-location(sig-fragment),
         primitive-name:  name);
  end;
end method;

define method make-ffi-signature (sig-spec :: <signature-spec>)
 => (ffi-signature, sig)
  let ffi-signature = compute-signature(#f, sig-spec);
  let signature
    = compute-signature-using-types
        (sig-spec,
         shallow-copy(ffi-signature.^signature-required),
         shallow-copy(ffi-signature.^signature-values),
         #f, #[], #[]);
  let required  = signature.^signature-required;
  let req-specs = sig-spec.spec-argument-required-variable-specs;
  for (i from 0 below ^signature-number-required(signature),
       type in required)
    if (instance?(type, <&raw-struct-type>))
      // in dylan they are all <raw-pointer>s.
      required[i] := ^top-level-eval(#{<raw-pointer>});
      req-specs[i]
        := make(<required-variable-spec>,
                type-expression: #{<raw-pointer>},
                variable-name: req-specs[i].spec-variable-name)
    end;
  end;
  values(ffi-signature, signature);
end;
