Module: dfmc-debug
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define metering-set *top-level-metering*
  functions
    canonicalize-project-sources     in projects-implementation,
     install-parsed-sources           in dfmc-management,
    compile-library-from-definitions in dfmc-management,
     ensure-library-models-computed   in dfmc-management,
     ensure-library-models-finished   in dfmc-management,
     ensure-library-models-checked    in dfmc-management,
     ensure-library-dfm-computed      in dfmc-management,
     ensure-library-bindings-checked  in dfmc-management,
     ensure-library-type-estimated    in dfmc-management,
      type-estimate                    in dfmc-typist,
     ensure-library-optimized         in dfmc-management,
     ensure-library-heaps-computed    in dfmc-management,
     ensure-library-records-linked    in dfmc-management,
     ensure-library-glue-linked       in dfmc-management;
end metering-set;

define metering-set *dependency-metering*
  include *top-level-metering*;
  functions
    add-definition in dfmc-namespace,
      note-changing-definition in dfmc-definitions,
      definer-references-in in dfmc-namespace,
      trigger-binding-dependents in dfmc-definitions,
        match-binding-dependencies in dfmc-definitions,
        lookup-form-binding-dependencies in dfmc-definitions,
      retract-dependent-stages in dfmc-definitions;
end metering-set;

define metering-set *dfm-metering*
  include *top-level-metering*;
  functions
    dylan-make in-package clos;
  functions
    get-token in dfmc-reader,
      internal-get-token in dfmc-reader,
      do-process-token in dfmc-reader,
        do-process-token in-package dylan,
      classify-word-in in dfmc-reader,
      extract-string in dfmc-reader,
      make-identifier in dfmc-reader,
    run-parser in parser-run-time,
    //  get-next-lexeme in-package parsergen,
    //  call-parser-action in-package parsergen,
    //  is-all-same-reduction in-package parsergen,
    // match-variable-constraint in dfmc-macro-expander,
    // match-expression-constraint in dfmc-macro-expander,
    parse-template-fragments-as in dfmc-macro-expander,
    parse-constraint in dfmc-macro-expander,
    parse-bounded-constraint in dfmc-macro-expander,
    match-expression-constraint in dfmc-macro-expander;
    // parse-template-fragments-as in dfmc-macro-expander,
    // read-top-level-fragment in dfmc-reader;
  functions
    convert-lambda-into* in dfmc-conversion,
    upgrade-to-multiple-values in dfmc-conversion,
    convert-using-definition in dfmc-conversion,
    convert-object-reference in dfmc-conversion,
    convert-reference in dfmc-conversion,
    convert-function-call in dfmc-conversion,
    convert-body in dfmc-conversion,
    convert-body-mv in dfmc-conversion;
  functions
    make-with-temporary in dfmc-flow-graph;
end metering-set;

define metering-set *parser-metering*
  include *top-level-metering*;
  functions
    dylan-make in-package clos;
  functions
    get-token in dfmc-reader,
      internal-get-token in dfmc-reader,
      do-process-token in dfmc-reader,
        do-process-token in-package dylan,
      classify-word-in in dfmc-reader,
      extract-string in dfmc-reader,
      make-identifier in dfmc-reader,
    run-parser in parser-run-time,
      get-next-lexeme in-package parsergen,
      call-parser-action in-package parsergen,
      is-all-same-reduction in-package parsergen,
    // match-variable-constraint in dfmc-macro-expander,
    // match-expression-constraint in dfmc-macro-expander,
    parse-constraint in dfmc-macro-expander,
    parse-template-fragments-as in dfmc-macro-expander,
    read-top-level-fragment in dfmc-reader;
  functions
    compute-library-definitions in dfmc-management,
    compute-source-record-top-level-forms in dfmc-management,
    install-top-level-forms in dfmc-management;
  functions
    top-level-convert-forms in dfmc-definitions,
      booted-init-sequence in dfmc-definitions,
      booted-definition-sequence in dfmc-definitions,
      booted-source-sequence in dfmc-definitions,
    top-level-convert in dfmc-definitions,
    as-body in dfmc-definitions,
    as-name in dfmc-definitions;
  functions
    parse-property-adjectives in dfmc-definitions,
    parse-options in dfmc-definitions,
    do-define-library in dfmc-definitions,
    do-define-module in dfmc-definitions,
    do-define-macro in dfmc-definitions,
    do-define-class in dfmc-definitions,
      parse-class-clauses in dfmc-definitions,
      parse-slot-clause in dfmc-definitions,
      parse-inherited-slot-clause in dfmc-definitions,
      parse-keyword-clause in dfmc-definitions,
      parse-metaclass-clause in dfmc-definitions,
    do-define-generic in dfmc-definitions,
      parse-signature-as in dfmc-definitions,
    do-define-method in dfmc-definitions,
    do-define-domain in dfmc-definitions,
    do-define-constant in dfmc-definitions,
    do-define-variable in dfmc-definitions,
    do-define-primitive in dfmc-definitions;
  functions
    update-imports in dfmc-namespace,
    lookup-name in dfmc-namespace;
end metering-set;

define metering-set *optimizer-metering*
  include *top-level-metering*;
  functions
    run-compilation-passes      in dfmc-common,
    allocate-registers          in dfmc-optimization,
    analyze-environments        in dfmc-optimization,
    eliminate-assignments       in dfmc-optimization,
    constant-fold               in dfmc-optimization,
    delete-useless-environments in dfmc-optimization,
    delete-useless-computations in dfmc-optimization,
    check-function-call         in dfmc-optimization,
    try-inlining                in dfmc-optimization,
    single-value-propagation    in dfmc-optimization,
    analyze-non-local-exits     in dfmc-optimization,
    analyze-calls               in dfmc-optimization;
end metering-set;

define metering-set *top-level+typist-metering*
  include *top-level-metering*;
  modules dfmc-typist;
end metering-set;

define metering-set *top-level+optimizer+typist-metering*
  include *optimizer-metering*;
  modules dfmc-typist;
end metering-set;
