Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-reader
  use dylan;
  use parser-run-time;
  use big-integers;
  use io;
  use dfmc-common;
  use dfmc-conditions;
  use source-records;
  export dfmc-reader;
end library;

define module dfmc-reader
  use dylan;
  use dylan-extensions,
    import: { vector-element, vector-element-setter, \without-bounds-checks,
              make-symbol };
  use pprint;
  use parser-run-time,
    rename: { <lexer> => <parser-lexer> };
  use big-integers,
    prefix: "generic";
  use dfmc-common;
  use dfmc-imports;
  use dfmc-conditions;
  use source-records;

  //// Token classes used externally.

  export
    $define-body-word-only-token,
    $define-list-word-only-token,
    $begin-word-only-token,
    $function-word-only-token,
    $local-declaration-word-only-token,
      $local-methods-word-only-token,

    $define-macro-body-word-only-token,
    $macro-case-begin-word-only-token,

    $unreserved-name-token;

  //// Source reading interface.

  export
    read-top-level-fragment,
      source-lines-read,
    re-read-fragments,
      $start-token-constraint,
      $start-name-constraint,
      $start-expression-constraint,
      $start-variable-constraint,
      $start-body-constraint,
      $start-case-body-constraint,
      $start-property-list-constraint,
      $start-fragment-constraint,
      $end-constraint;

  //// Reading conditions.

  export
    <reader-error>,
      <invalid-token>,
        <integer-too-large>,
        <character-code-too-large>,
        <ratios-not-supported>,
      <invalid-end-of-input>,
      <parser-error>,
      <manual-parser-error>,
      parser-error-handler;

  //// Fragment value creation, and the extra "literal" unbound value.

  export
    as-fragment-value, <mapped-unbound>, $unbound;

  //// Special source location indicating nothingness...

  export
    $nowhere;

  //// Dynamic source location contexts.

  export
    <compiler-range-source-location>,
    source-location-record,
    record-position-as-location, source-location-source-position,
    \with-parent-source-location, do-with-parent-source-location,
    \with-parent-fragment, do-with-parent-fragment,
    parent-source-location,
    range-source-offset-greater-than?,
    source-location-start-offset-setter, source-location-end-offset-setter;

  //// Program fragment interface.

  export
    \with-fragment-info, *fragment-context*,
    classify-word-in, classify-expansion-word-in,
    merge-token-classes, definer-token-class?,
    classify-dylan-name, dylan-variable-name,
    <fragment>,
      fragment-source-location,
      fragment-source-position,
        fragment-source-position-setter, // HACK: FOR DOOD
      fragment-record,
        fragment-record-setter,          // HACK: FOR DOOD
      <elementary-fragment>,
        <punctuation-fragment>,
          <dot-fragment>,
          <separator-fragment>,
            <comma-fragment>,
            <semicolon-fragment>,
          <colon-colon-fragment>,
          <equal-greater-fragment>,
          <lparen-fragment>, <rparen-fragment>,
          <lbracket-fragment>, <rbracket-fragment>,
          <lbrace-fragment>, <rbrace-fragment>,
          <hash-lparen-fragment>,
          <hash-lbracket-fragment>,
          <hash-word-fragment>,
            <hash-next-fragment>,
            <hash-rest-fragment>,
            <hash-key-fragment>,
            <hash-all-keys-fragment>,
      <sequence-fragment>, fragment-fragments,
      <nested-fragment>, nested-fragment?,
        fragment-left-delimiter,
        fragment-nested-fragments,
        fragment-right-delimiter,
        <parens-fragment>,
        <brackets-fragment>,
        <braces-fragment>,
      <name-fragment>, fragment-name, fragment-name-string,
        same-name-when-local?,
      <escaped-name-fragment>,
      <operator-fragment>, fragment-name,
        <binary-operator-fragment>,
        <unary-operator-fragment>,
          <unary-and-binary-operator-fragment>,
        <equal-fragment>,
      <expression-fragment>,
        <variable-name-fragment>,
          fragment-identifier,
          fragment-module,
          fragment-context,
          make-variable-name-fragment-in-module,
            make-variable-name-like,
            splice-name-hygienically,
            suffix-name-hygienically,
        <literal-fragment>,
        <literal-constant-fragment>, fragment-value,
          <elementary-literal-fragment>,
            <number-fragment>,
              <abstract-integer-fragment>,
                <integer-fragment>,
                <big-integer-fragment>,
              <float-fragment>,
            <symbol-fragment>,
              <symbol-syntax-symbol-fragment>,
              <keyword-syntax-symbol-fragment>,
            <character-fragment>,
            <string-fragment>,
            <boolean-fragment>,
              <true-fragment>,
              <false-fragment>,
          <list-fragment>, fragment-elements,
            <improper-list-fragment>,
            <proper-list-fragment>,
          <vector-fragment>,
        <macro-call-fragment>, fragment-macro, fragment-argument,
          <definition-fragment>, fragment-modifiers, fragment-define-word,
            <body-definition-fragment>,
              fragment-body-fragment, fragment-end-word,
            <list-definition-fragment>,
              fragment-list-fragment,
            <macro-body-definition-fragment>,
              fragment-macro-body-fragment, fragment-end-word,
          <statement-fragment>, fragment-body-fragment,
          <function-macro-fragment>, fragment-body-fragment,
          <local-declaration-call-fragment>,
            fragment-declaration-fragment,
            fragment-body-fragment,
              <local-declaration-fragment>, fragment-list-fragment,
        <function-call-fragment>, fragment-function, fragment-arguments,
        <body-fragment>, body-fragment, fragment-constituents,
        <macro-definition-fragment>;

  export
    <end-of-modifiers-marker>, end-of-modifiers-marker?;

  export
    <fragment-copier>;

  export
    <variable-name-table>,
      \with-new-hygiene-context, do-with-new-hygiene-context,
    make-unique-local-variable-name-fragment;

  export
    <query-fragment>,
    <query-query-fragment>,
    <query-equal-fragment>,
    <hash-lbrace-fragment>,
    <escaped-substitution-fragment>,
      fragment-escaped-fragment,
    <constrained-name-fragment>, fragment-constraint,
    <pattern-variable-fragment>, fragment-name, fragment-constraint,
    <spliced-pattern-variable-fragment>,
      fragment-pattern-variable, fragment-prefix, fragment-suffix,
    <sequence-pattern-variable-fragment>,
      fragment-name, fragment-constraint, fragment-separator,
    <ellipsis-fragment>,
    <unhygienic-name-fragment>,
    <template-call-fragment>,
      fragment-template,
      <template-aux-rule-call-fragment>,
        fragment-rule-name,
      <template-macro-call-fragment>;

  export
    <template>, template-fragments;

  //// Fragment source location utilities.

  export
    spanning;

  export
    as-fragment, parsed-literal;

  export
    present-fragments;

end module;
