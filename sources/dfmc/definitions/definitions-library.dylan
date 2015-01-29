module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-definitions
  use dylan;
  use dfmc-common;
  use dfmc-namespace;
  use dfmc-conditions;
  use dfmc-reader;
  use dfmc-macro-expander;
  export dfmc-definitions;
end library;

define module dfmc-definitions
  use dylan;
  use dfmc-common;
  use dfmc-imports;
  use dfmc-namespace;
  use dfmc-conditions;
  use dfmc-reader;
  use dfmc-macro-expander;

  export // utilities
    as-body,
    as-name,
    parse-signature-as,
    parse-method-signature,
    parse-value-bindings,
    choose-instances,
    lookup-compile-stage-function,
    lookup-optimizer-function,
    method-fragment?
    ;

  export // options
    option-definer,
    parse-options;

  export // adjectives
    property-definer,
    parse-property-adjectives;

  export // boot
    <expander-defining-form>,
    <&definition>,
      form-expander,
    <&macro-definition>,
    <&converter-definition>,
    <&definition-definition>,
    <top-type-definition>,
    <bottom-type-definition>,
    <raw-type-definition>,
      form-supertype-name,
      form-raw-type-descriptor-function,
    <raw-aggregate-definition>,
     <raw-struct-definition>,
     <raw-union-definition>,
      form-members,
      form-options,
    booted-module?,
    do-define-evaluator-override,
    do-define-optimizer-override, &optimizer-function-definer,
    do-define-core-instance,
      booted-constant-definitions,
    do-define-core-unadorned-definition,
    do-define-core-library,
    do-define-core-module,
    do-define-top-type,
    do-define-bottom-type,
    do-define-raw-type,
    do-define-core-primitive,
    do-define-core-converter,
    do-define-core-macro,
    do-define-core-definition;

  export
    top-level-convert-forms,
    install-top-level-form;

  export <library-definition>,
         <module-definition>,
         <namespace-defining-form>,
    form-namespace-name,
    form-create-clauses,
    form-export-clauses,
    form-use-clauses;

  export <binding-defining-form>,
         <literal-value-binding-defining-form>,
         <constant-definition>,
         <literal-value-constant-definition>,
         <booted-constant-definition>,
         <literal-value-booted-constant-definition>,
         <constant-method-definition>,
         <variable-definition>,
         <literal-value-variable-definition>,
    form-bindings-spec,
    form-init-expression;

  export <macro-definition>,
    form-macro-object,
    form-macro-rules,
    macro-definition-word,
    macro-fragment?,
    do-compile-macro;

  export <function-defining-form>;

  export <function-definition>;

  export <method-defining-form>,
    form-complete?;

  export <signature-spec>,
    spec-argument-required-variable-specs,
    spec-argument-number-required,
    spec-argument-next-variable-spec,
    spec-argument-rest-variable-spec,
    spec-argument-rest?,
    spec-argument-key-variable-specs,
    spec-argument-key?,
    spec-argument-all-keys?,
    spec-argument-optionals?,
    spec-argument-number-keys,

    spec-value-required-variable-specs,
    spec-value-number-required,
    spec-value-rest-variable-spec,
    spec-value-rest?,

    <variable-specs>
    ;

  export <variable-spec>,
    spec-variable-typed?,
    spec-variable-name,
    spec-type-expression,
    <required-variable-spec>,
    <next-variable-spec>,
    <rest-variable-spec>,
    <key-variable-spec>,
      spec-keyword-expression,
      spec-default-expression;

  export <generic-definition>,
    form-sideways?,
    form-options,
    form-signature,
    form-parameters-have-dynamic-extent?;

  export <method-definition>,
    method-definition?,
    form-upgrade?,
    form-sideways?,
    form-class,
    form-signature,
    form-signature-and-body-fragment,
    form-handled-by-make-when-dynamic?,
    ensure-next-method-binding;

  export <domain-definition>,
    domain-definition?,
    form-domain-type-expressions;

  export <class-definition>,
    form-superclass-expressions,
    form-slot-specs,
    form-inherited-slot-specs,
    form-keyword-specs,
    form-metaclass-spec,
      spec-metaclass-name,
      spec-metaclass-initargs,
    form-abstract?, form-concrete?,
    form-primary?, form-free?,
    form-declared-sealed?, form-sealed?, form-compiler-open?, form-sealed-if-private?,
    form-model-object
    ;

  export <slot-definition>,
    form-getter-definition,
    form-setter-definition,
    spec-getter,
    spec-setter,
    spec-type-expression,
    spec-init-supplied?,
    spec-init-expression?,
    spec-init-value?,
    spec-init-expression,
    spec-allocation,
      spec-virtual?,
      spec-repeated?,
    spec-constant?,
    spec-sealed?,
    spec-atomic?,
    spec-volatile?,
    spec-init-keyword,
    spec-init-keyword-required?;

  export <repeated-slot-definition>,
    spec-size-getter,
    spec-size-init-keyword,
    spec-size-init-supplied?,
    spec-size-init-expression;

  export <slot-initial-value-spec>, <slot-keyword-initialization-spec>,
    spec-init-expression,
    spec-init-supplied?,
    spec-init-expression?,
    spec-init-value?,
    spec-init-keyword,
    spec-init-keyword-required?;

  export <inherited-slot-spec>;
  export <init-arg-spec>;

  export <primitive-definition>,
    form-primitive-value,
    form-primitive-stateless?,
    form-primitive-side-effecting?,
    form-primitive-dynamic-extent?,
    <primitive-signature-spec>;

  export <shared-symbols-definition>,
    form-shared-symbols;

  export <referenced-variable>,
    referenced-variable-name,
    referenced-variable-module;

  export <missing-definition>,
    binding-definition-missing?,
    install-missing-definition;

  export
    \with-dependent-retraction, do-with-dependent-retraction,

    binding-local-references?,
    retract-top-level-form-models,
    retract-compilation-record-models,
    retract-form-model-objects,
    retract-compilation-record;

  export
    binding-local-referers,
    form-referenced-macro-variables,
    form-referenced-binding-variables,
    choose-name-dependencies;

  export // To allow using <name-dependency> as a variable in browsers
    <name-dependency>,
    dependency-name,
    dependency-module;

  export
    binding-method-definitions,
    binding-domain-definitions,
    binding-direct-subclass-definitions;

  export
    *heap-statistics?*;

  export // hygiene for emulator
    find-property, parse-many-property-adjectives,
    <unrecognized-properties>, <contradictory-properties>,
    <property>, <property-value>,
    property-keyword, property-value-value, property-value-syntax,
    <option>;

end module;
