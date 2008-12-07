module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-optimization
  use dylan;
  use dfmc-core;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-typist;
  use dfmc-conversion;
  use dfmc-back-end;
  export dfmc-optimization;
end library;

define module dfmc-optimization
  use dylan;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-reader;
  use dfmc-macro-expander;
  use dfmc-typist;
  use dfmc-conversion;
  use dfmc-back-end;
  export
    really-run-compilation-passes,
    <optimization-note>,

    // assignment.dylan
    eliminate-assignments,

    // constant-folding.dylan
    // constant?,

    // cse.dylan
    share-common-subexpressions,

    // dead.dylan
    delete-useless-computations,

    // entry-points.dylan
    analyze-calls,
    maybe-upgrade-call,

    // inlining.dylan
    *inlining?*,
    try-inlining,
    inline-call,

    // multiple-values.dylan
    single-value-propagation,

    // non-local-exit.dylan
    analyze-non-local-exits,

    // tail-call.dylan
    tail-position?,

    // for call statistics
    incf-static-dispatch-count,
    incf-dynamic-dispatch-count,

    *warn-about-bogus-upgrades*,
    *colorize-bogus-upgrades*,

    *profile-all-calls?*,
    *partial-dispatch?*,

    *trace-optimizations?*,
    *trace-optimizing-method*,
    *trace-optimizing-library*,
    *trace-optimizing-file*,
    *dump-dfm?*,
    *dump-dfm-method*,
    *dump-dfm-library*,
    *dump-dfm-file*,
    *call-upgrading?*;

  export <run-time-type-error>,
    <run-time-result-type-error>,
    <non-sequence-last-argument-in-apply>,
    <attempt-to-instantiate-abstract-class>,
    <ambiguous-copy-down-method>,
    <unknown-copy-down-method-domain>,
    <missing-copy-down-method>,
    <non-function-in-call>,
    <unknown-keyword-in-call>,
    <argument-count-mismatch-in-call>,
    <too-few-arguments-in-call>,
    <too-many-arguments-in-call>,
    <unbalanced-keyword-arguments-in-call>,
    <non-keywords-in-call>,
    <argument-type-mismatch-in-call>,
    <values-argument-type-mismatch-in-call>,
    <bogus-upgrade-possible>,
    <unrecognized-keyword-arguments-in-call>,
    <too-many-arguments-in-apply-call>,
    <argument-type-mismatch-in-apply-call>,
    <bogus-apply-upgrade-possible>,
    <no-applicable-methods-in-call>,
    <calling-inline-only-function-out-of-line>;

  export
    best-function-key?,
    best-function-rest?,
    best-function-optionals?,
    best-function-all-keys?,
    best-function-number-keys,
    best-function-number-required;
end module;
