Module:    Dylan-User
Author:    Andy Sizer
Synopsis:  Library and module definitions for the typist.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///
/// The Typist -- A type-inference module for the DFM.
///

define library dfmc-typist
  use functional-dylan;
  use metering;
  use file-source-records;
  use dfmc-reader;
  use dfmc-flow-graph;
  use dfmc-modeling;
  use dfmc-core;

//  use projects;
//  use registry-projects;
  export dfmc-typist;
end library;

define module dfmc-typist
  use functional-dylan;
  use metering;
  use dfmc-core;
  use dfmc-imports;
  use dfmc-flow-graph;
  use dfmc-modeling;

  export 
    *trace-typist?*, *trace-typist?*-setter,
    call-values,
    lookup-type,
    type-infer-lambda,
    do-type-infer-lambda,
    arguments-guaranteed-joint?,
    constant-value?,
    dispatched-method,
    optimized?, optimized?-setter,
    arguments-potentially-joint?,
    guaranteed-method-precedes?,
    guaranteed-joint?,
    guaranteed-disjoint?,
    $methods-unordered, $method1-precedes, $method2-precedes,
    default-arg-types,
    get-argument-types,
    type-computations,
    type-call-in-all-summaries,
    update-type-computations,
    all-applicable-methods-guaranteed-known?,
    guaranteed-method-relationship,
    next-method-list,
    get-computation-record,
    input-types,
    ensure-boxed,
    type-initializer-method,
    type-top-level-method,
    type-top-level-form,
    called-function,
    candidate-summaries,
    squash-summaries-for-top-level-form,
    typist-walk-computations,
    initialize-special-actions;        // gts, merge

  export <constant-folded>,
         <method-constant-folded>,
         <generic-constant-folded>,
         <successfully-dispatched-call>,
         <successful-dispatch>,
         <illegal-call-record>;

  export
    <run-time-type-error>, 
    <run-time-result-type-error>,
    <possible-run-time-type-error>,
    <non-sequence-last-argument-in-apply>,
    <non-function-in-call>,
    <incompatible-call>,
      <unknown-keyword-in-call>,
      <argument-count-mismatch-in-call>,
        <too-few-arguments-in-call>,
        <too-many-arguments-in-call>,
        <too-many-arguments-in-apply-call>,
      <unbalanced-keyword-arguments-in-call>,
      <non-keywords-in-call>,
      <argument-types-mismatch-in-call>,
      <argument-type-mismatch-in-apply-call>,
      <values-argument-types-mismatch-in-call>,
      <unrecognized-keyword-arguments-in-call>,
      <no-applicable-methods-in-call>;

  export
    <computation-record>;
end module;









