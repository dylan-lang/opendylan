Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dfmc-macro-expander
  use functional-dylan;
  use dfmc-common;
  use dfmc-conditions;
  use dfmc-reader;
  export dfmc-macro-expander;
end library;

define module dfmc-macro-expander
  use functional-dylan;
  use dfmc-imports,
    export: { &definition-definer,
              &converter-definer,
              &macro-definer,
              \macro-case,
              \macro-template };
  use dfmc-common;
  use dfmc-conditions;
  use dfmc-reader, export: all;

  // Macro descriptors and their operations.

  export
    <macro-descriptor>,
      macro-word-in-variable-name,
      macro-expander-function,
      macro-referenced-names,
      expand-macro-call,
      <simple-macro-descriptor>,
      <suffixed-macro-descriptor>,
        suffixed-name?;

  // Parsing utils.

  export
    as-expression,
      parse-template-fragments-as;

  // Inermediate macro rep.

  export
    <rewrite-rule>, rule-pattern, rule-template,
    <rewrite-rule-set>, rule-set-rewrite-rules,
      <aux-rewrite-rule-set>, rule-set-name,

    <pattern-match>,
      <binding-match>,
        <simple-match>,
        <sequence-match>,
      <structure-match>,
        <nested-match>,
          <paren-match>,
          <bracket-match>,
          <brace-match>,
        <variable-match>,
        <property-list-match>,
          <rest-match>,
          <key-match>,
          <key-sequence-match>,
        <splicing-match>,

    <simple-element-substitution>,
    <as-symbol-substitution>,
    <as-string-substitution>,
    <splicing-substitution>,
    <simple-sequence-substitution>,
    <macro-call-substitution>,
    <aux-rule-call-substitution>;

  // Pattern compilation.

  export
    <rewrite-rule-expander>,
      expander-referenced-names,
      generate-expander-function;

  // Template/fragment construction back-end.

  export
    make-in-expansion,
    make-template,
      substitute-as-string,
      substitute-as-symbol,
      substitute-spliced-as-name,
      substitute-spliced-as-string,
      substitute-spliced-as-symbol,
      substitute-sequence,
      substitute-sequence-separated,
      maybe-substitute-separator,
    make-literal-fragment,
    make-name-fragment,
      make-escaped-name-fragment,
      make-unhygienic-name-fragment,
      make-binary-operator-fragment,
      make-unary-operator-fragment,
        make-unary-and-binary-operator-fragment,
    make-constrained-name-fragment,
    make-equal-fragment,
    make-equal-greater-fragment,
    make-colon-colon-fragment,
    make-dot-fragment,
    make-comma-fragment,
    make-semicolon-fragment,
    make-lparen-fragment, make-rparen-fragment,
    make-lbracket-fragment, make-rbracket-fragment,
    make-lbrace-fragment, make-rbrace-fragment,
    make-hash-lparen-fragment,
    make-hash-lbracket-fragment,
    make-hash-lbrace-fragment,
    make-hash-next-fragment,
    make-hash-rest-fragment,
    make-hash-key-fragment,
    make-hash-all-keys-fragment,
    make-query-fragment,
    make-query-query-fragment,
    make-query-equal-fragment,
    make-hash-lbrace-fragment,

    make-parens-fragment,
    make-brackets-fragment,
    make-braces-fragment;

  // Pattern matching back-end.
 
  export
    <fragment-list>,

    macro-main-rule-match-error,
    macro-aux-rule-match-error,
   
    match-body-part, match-body-part-strict,
    match-list-part, match-list-part-strict,

    match-name,
      match-otherwise,
      match-spliced-name,
    match-operator,
      match-equal-greater,
    match-hash-rest,
    match-hash-next,
    match-hash-key,
    match-hash-all-keys,
    match-dot,
    match-colon-colon,
    match-literal,

    match-token-constraint,
    match-name-constraint,
    match-expression-constraint,
    match-variable-constraint,
    match-body-constraint,
      match-bounded-body-constraint,
      match-bounded-body-constraint-no-backtracking,
    match-case-body-constraint,
      match-bounded-case-body-constraint,
    match-macro-constraint,
      expand-for-macro-constraint, // to be implemented above
    match-symbol-constraint,

    match-property-list,
    match-parens, match-brackets, match-braces,
    match-variable,

    match-end-of-modifiers,

    split-at-kept-semicolon;

  // Expansion auxiliary function.

  export
    as-fragment-tokens,
    call-as-fragment-tokens,
    export-fragment-tokens,
    import-to-template;

  // Pattern/template compilation entry points.

  export
    \with-native-template-evaluation, do-with-native-template-evaluation,
    \with-expansion-module, do-with-expansion-module,
    \with-expansion-source-location, do-with-expansion-source-location,
    \with-expansion-source-form, do-with-expansion-source-form,
    default-in-expansion,
    compile-macro-template,
    compile-define-macro-rules,
    compile-macro-case-rules,

    compile-macro-template-to-code,
    pattern-variable-name;

  // Old API.

  export
    \handling-parse-errors, do-handling-parse-errors;

  // Debugging.

  export
    \with-macroexpansion-output, do-with-macroexpansion-output,
    *expansion-stream*, *trace-stream*,
    compiling-for-macroexpansion?;

end module;
