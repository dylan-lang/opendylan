module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-common
  use functional-dylan;
  use collections;
  use walker;
  use io;
  use system;
  use dood;
  use source-records;
  use ppml;
  export dfmc-common;
  export dfmc-imports;
end library;

define module dfmc-imports
  use dylan-extensions,
    import: {<hash-state>, string-hash,
	     <abstract-integer>,
	     <double-integer>, 
	       %double-integer-low,
	       %double-integer-high,
	     <machine-word>,
	     debug-name, debug-name-setter,
	     // converter support
	     &definition-definer,
	     &converter-definer,
	     &macro-definer,
	     \macro-case,
	     \macro-template,
	     $end-count-<object>,
	     pack-tristate,  unpack-tristate,
	     pack-quadstate, unpack-quadstate,
	     pack-boolean,   unpack-boolean,
	     initialize-packed-slots,
	     packed-slots-definer,
	     <ordered-object-table>,
	     <ordered-object-set> },
    export: all;
  use functional-extensions, export: all;
  use threads, export: all;
  use collectors, export: all;
  use set, export: all;
  use byte-vector, export: all;
  use plists, export: all;
  use walker, export: all;
  use locators, export: all;
  use streams, export: all;
  use format, export: all;
  use print, export: all;
  use standard-io, export: all;
  use format-out, export: all;
  use operating-system, rename: {load-library => os/load-library}, export: all;
  use date, export: all;
  use file-system, export: all;
  use dood, export: all;
  use source-records, export: all;
  use ppml, export: all;
end module;

define module dfmc-common
  use functional-dylan;
  use dfmc-imports;

  export 
    \rare-slots-definer,
      \rare-slot-definer, // TODO: Remove when macro references are tracked
    \property-delegation-definer, 
    \property-delegation-getters-definer, 
    \property-delegation-setters-definer, 
    \symbolic-class-definer,
    \symbolic-class-aux-definer,
    \symbolic-accessors-definer,

    \coagulate-name,

    \debug-out, *debug-out*,

    \sealed-constructor-definer;

  export
    <named-object>, 
    <name>,
    name, name-setter,
    named?,
    coerce-name;
  
  export
    <numbered-object>,
    id, id-setter,
    make-next-id;
    
  export
    <referenced-object>,
    users, users-setter,
    used?, used-once?, 
    add-user!, remove-user!, 
    references;

  export
    <emitted-object>,
    emitted-name,  emitted-name-setter;

  export
    mapped-model, immutable-model,
    standard-model-object,
    find-model-properties-in,
    <model-properties>,
    private-model-definition, model-definition, model-definition-setter,
    model-has-definition?, model-source-location,
    model-compile-stage-only?,
    model-variable-name, model-variable-using-definition,
    private-model-creator, model-creator, 
    model-compilation-record,
    model-downloaded?, model-interactive?,
    model-original-library,
    model-library, maybe-model-library;

  export
    thread-property-definer;

  export
    eval, constant-eval,
    &eval, &constant-eval,
    compile-stage,
    run-stage;

  export
    <dood-dfmc-object>;

  export
    <compilation-context>,
    compiled-to-definitions?,
      compiled-to-definitions?-setter,
    compilation-from-definitions-started?,
      compilation-from-definitions-started?-setter,
    compilation-definitions-inconsistent?,
      compilation-definitions-inconsistent?-setter,
      \with-inconsistent-definitions,
    compilation-timings,
      compilation-timings-setter,
    compilation-context-records,
      compilation-context-records-setter;
  export
    <variable-name>,
    make-variable-name-fragment,
    resolve-qualified-variable-name-module,

    current-library-description, 
      current-library-description?,
    current-top-level-library-description, 
      current-top-level-library-description?,
    current-library-in-context?,
    current-back-end,
    current-back-end-name,
    current-compilation-mode,
    current-processor-name, current-os-name,
    compiling-dylan-library?,

    library-description-personal?,

    run-compilation-passes,

    word-size,

    *optimization-level*,

    $optimization-mandatory,
    $optimization-low,
    $optimization-medium,
    $optimization-high,
    $optimization-default;

    // \compilation-pass-definer,
    // define-compilation-pass!,
    // optimization-default-hack, visit-default-hack,

    // <compilation-pass>,
    // lookup-pass,
    // pass-function, pass-function-setter,
    // optimization-level, optimization-level-setter,
    // visiting-policy, visiting-policy-setter,
    // all-triggered-passes,
    // disabled?, disabled?-setter,
    // print-before?, print-before?-setter,
    // print-after?, print-after?-setter,
    // check-before?, check-before?-setter,
    // check-after?, check-after?-setter,
    // include-back-ends, include-back-ends-setter,
    // exclude-back-ends, exclude-back-ends-setter,

    // *passes*,
    // pass-ordering,

    // <compilation-queue>,
    // push-pass!,
    // pop-pass!,

    // topological-sort

  export
    describe,
    describe*;

  export
    <compilation-record>,
      compilation-record-heap-referenced-objects,
        compilation-record-heap-referenced-objects-setter,			     
      compilation-record-approximate-model-heap-size,
        compilation-record-approximate-model-heap-size-setter,			     
      compilation-record-data-size,
        compilation-record-data-size-setter,			     
      compilation-record-code-size,
        compilation-record-code-size-setter,			     
    <interactive-compilation-record>,
    <library-compilation-record>,
      <compilation-record-sequence>, 
      <compilation-record-vector>,
      compilation-record-original-library,
        compilation-record-original-library-setter,
      compilation-record-downloaded?, compilation-record-interactive?,
      compilation-record-library,
      compilation-record-module, compilation-record-module-setter,
      compilation-record-source-record,
      compilation-record-source-line-count, 
        compilation-record-source-line-count-setter,
      compilation-record-preceeding-line-count,
        compilation-record-preceeding-line-count-setter,
      compilation-record-dispatch-decisions,
      compilation-record-dispatch-decisions-setter,
      compilation-record-sequence-number,
        compilation-record-sequence-number-setter,
      compilation-record-dependency-table,
        compilation-record-dependency-table-setter,
      compilation-record-top-level-forms, 
        compilation-record-top-level-forms-setter,
      compilation-record-definitions-installed?,
        compilation-record-definitions-installed?-setter,
      compilation-record-expanded-top-level-forms,
      compilation-record-model-properties,
        compilation-record-model-properties-setter,
      compilation-record-inline-only-table,
        compilation-record-inline-only-table-setter,
      compilation-record-back-end-data,
        compilation-record-back-end-data-setter,
      compilation-record-name,
      compilation-record-transaction-id,
        compilation-record-transaction-id-setter,
      add-derived-top-level-forms,
      clear-compilation-record-caches,
      retract-compilation-record-heap,
      compilation-record-model-heap, 
        compilation-record-model-heap-setter,
      compilation-record-needs-linking?, 
        compilation-record-needs-linking?-setter;

  export source-record-compilation-record;

  export
    \with-form-creation,
       *last-form-sequence-number*,
    \with-boot-form-creation, do-with-boot-form-creation,
    \with-dependent, \without-dependency-tracking, do-with-dependent,
    *current-dependent*,
     $no-dependent,
    *interactive-compilation-layer*,
    compilation-record-of,
    current-compilation-record,
    remove-dependent-program-conditions,
    *current-stage*,
     $top-level-processing,
     $compilation,
    $top-level-processing-mask,
    $compilation-mask,
    note-name-dependency,
     note-name-dependency-of,
     dep$name-syntax,
     dep$name-macro-ref,
     dep$name-binding,
     dep$name-binding-ref,
    note-binding-dependency,
     note-binding-dependency-of,
     dep$active-definition,
     dep$defined?,
     dep$modifying-definitions,
     dep$model-object,
     dep$modifying-models,
    note-adding-definition,
    note-adding-modifying-definition,
    note-removing-definition,
    note-removing-modifying-definition,
    note-removing-model-object,
    note-removing-modifying-models,
    note-changing-binding,
    defined-after?,
    defined-before?,

    make-dependency-condition,
    dependency-stage-match?,
    dep$all,
    dep$count,
    stage$0,
    stage$0-mask,
    stage$1,
    stage$1-mask,
    stage$count,
    stage$all,

    <form-properties>,
      form-properties, form-properties-setter, 
      form-stripped?, form-stripped?-setter, 
      form-properties-in-context,
      make-default-form-properties,
      merge-form-properties!,
    <installable-form-properties>,
      form-top-level-installable?,
      form-top-level-installed?, form-top-level-installed?-setter,
    <top-level-form>, <top-level-form-sequence>,
      form-source-location,
      form-compilation-record,
      form-library, 
      form-original-library,
      form-downloaded?, form-interactive?,
      form-define-word,
      form-parent-form, form-parent-form-setter,
      form-sequence-number,
      form-init-method, form-init-method-setter,
      form-system-init-method, form-system-init-method-setter,
      form-dependencies, form-dependencies-setter,
      form-referenced-variables, form-referenced-variables-setter,
      form-macro-word-class,
      form-top-level-methods,
      install-top-level-forms,
      retract-top-level-form,
      retract-body-fragments,
      compute-and-install-form-dfm,
    <top-level-init-form>,
      form-body,
      form-body-setter,

    <macro-call-form>,
      form-define-word,
      form-derived-forms, form-derived-forms-setter,

    <modified-top-level-form>,
      form-adjectives,

    <defining-form>,

    <variable-defining-form>,
       $end-count-<variable-defining-form>, // hygiene
       form-variable-name-or-names,
       form-variable-names,
       form-variable-name,
       form-inline-policy,
       form-compile-stage-only?,
       form-implicitly-defined?,
       form-thread?, form-locked?, form-atomic?,
       form-models-installed?, form-models-installed?-setter,
       maybe-compute-and-install-form-model-objects,
       compute-and-install-form-model-objects,
       compute-form-model-object,
       finish-installing-form-model-objects,

    <missing-variable-defining-form>,
       $end-count-<missing-variable-defining-form>, // hygiene

    <modifying-form>,
       $end-count-<modifying-form>, // hygiene
       form-model, form-model-setter,
       shadowable-form-model, shadowable-form-model-setter,

    <explicitly-typed-variable-defining-form>,
       form-type-expressions,
       form-type-expression,

    define-parsed-library,
    define-parsed-module;

  // strip incremental slots
  export
    strip-incremental-slots;

  // Incremental/loose-mode / binding.

  export
    form-evaluation-tried-and-failed?, 
      form-evaluation-tried-and-failed?-setter,
    form-dynamic?,
    form-binding-guaranteed-initialized?,
    compute-form-hollow-object;

  // Interactive.

  export
    form-incremental?,
    form-redefinition?;

  // GTS Debugging
  export
    gts-debug, *gts-debug*;
end module;
