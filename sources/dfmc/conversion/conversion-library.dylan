module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-conversion
  use dylan;
  use common-dylan, import: { transcendentals };
  use generic-arithmetic;
  use big-integers;
  use dfmc-core;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-typist;
  use dfmc-flow-graph;
  export dfmc-conversion;
end library;

define module dfmc-conversion
  use dylan;
  use generic-arithmetic,
    prefix: "generic/";
  use transcendentals;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-typist;
  use dfmc-flow-graph;
  export
    $ignore, $single, $all-rest,
    convert,
    convert-type-expression,
    convert-value-reference,
    convert-object-reference,
    convert-object-reference-1,
    make-object-reference,
    convert-method-reference,
    convert-reference,
    convert-values,
    convert-global-reference,
    convert-dylan-reference,
    make-global-reference,
    make-dylan-reference,
    make-value-reference,
    convert-error-call,
    convert-1,
    convert-top-level-initializer;

  export // utilities
    make-with-temporary*,
    fast-constant-value?,
    constant-value,
    function-value,
    call-effective-function,
    extractable-constant-value?,
    extract-constant,
    pad-multiple-values,
    temporary-value-context,
    <value-context>,
    <ignore-value-context>,
    <single-value-context>,
    <multiple-value-context>,
    match-values-with-temporary,
    match-values-with-context,
    bind-local-variable,
    do-convert;

  export
    <argument-sequence>,
    maybe-vector-element-references,
    ^function-key-type*,
    ^function-value-type*,
    ^function-required-type*,
    ^function-rest-value-type*;

  export
    &top-level-eval,
    ^top-level-eval,
    ^top-level-eval-sequence,
    ^top-level-eval-type;

  export
    check-model,
    <heap-deferred-all-classes-model>,
    finish-class-models,
    finish-generic-function-models;

  export // generic functions
    ^generic-function-explicitly-defined-methods,
    ^generic-function-explicitly-defined-domains,
    ^generic-function-domains-known,
    ^generic-function-methods-known;

  export // methods
    ^method-generic-function,
    method-inlineable?,
    maybe-compute-and-install-method-dfm,
    compute-and-install-method-dfm,
    retract-method-dfm,
    ensure-optimized-method-model,
    ensure-method-model,
    ensure-method-dfm,
    ensure-method-dfm-or-heap,
    empty-method?;

  export // classes
    ^ensure-slots-initialized,
    ^ensure-class-complete,
    accessor-method-dispatch-arg,
    get-method-slot-descriptor,
    slot-offset-fixed-in-class?,
    ^slot-fixed-offset,
    slot-guaranteed-initialized-in-class?,
    do-instance-slot-values, \for-instance-slot-value;

  export
    \for-layout-fixed-slot-value, do-layout-fixed-slot-values,
    \for-layout-repeated-slot-value, do-layout-repeated-slot-values,
    fixed-slot-primitive-fixup-info,
    repeated-slot-primitive-fixup-info;
  export
    define-compiler-metaclass;

  export
    trace-macro, untrace-macro;

  export
    copy-down-body;

  export
    *strip-enabled?*;

end module;
