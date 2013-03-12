Module: dylan-user
Author: Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library java-parser
  use common-dylan;
  use system;
  use io;
  use parser-run-time;
  use big-integers;
  export java-parser;
end library java-parser;

define module java-parser
  use common-dylan;
  use byte-vector;
  use transcendentals;
  use locators;
  use streams;
  use big-integers, prefix: "generic-",
    rename: { <integer> => <abstract-integer> };
  use parser-run-time;

  export
    <grammar-object>,
    run-java-parser;

  // grammar
  export
    <literal>,
      literal-value,
        <boolean-literal>, // literal-value = #t or #f
            <false-literal>,
            <true-literal>,
        <character-literal>, // literal-value = <integer>
        <float-literal>,     // literal-value = <double-float>
          float-literal-mantissa,
          float-literal-scale,
          float-literal-exponent,
            <double-float-literal>,
            <single-float-literal>,
        <integer-literal>,  // literal-value = <abstract-integer>
          integer-literal-radix,
            <int-literal>,
            <long-literal>,
        <null-literal>,  // literal-value = #f
        <string-literal>; // literal-value = <byte-string> or <unicode-string>

  export
    <type-descriptor>,
      type-name, // <name> or <primitive-type>
      type-numdims,
      <primitive-type>,
        primitive-type-symbol, // #"boolean", #"byte", #"float", ...
      <reference-type>;

  export
    <name>, <names>,
      name-identifiers,
      <identifier>,
        identifier-name,
      <qualified-name>;

  export
    <compilation-unit>, <compilation-units>,
      compilation-unit-package,
      compilation-unit-imports,
      compilation-unit-types;

  export <import>, <imports>,
    import-name,
      <type-import>, // import type-name;
      <package-import>; // import package-name.*;

  export <declaration>,
    declaration-modifiers;

  export <type-declaration>, <type-declarations>, // (<declaration>)
    type-declaration-name,
    type-declaration-declarations,
    <class-declaration>,
      class-super,
      class-interfaces,
    <interface-declaration>,
      interface-extends;

  export
    <body-declaration>, <body-declarations>,
      declaration-modifiers,
      declaration-body,
      <field-declaration>,
        field-declarators,
        field-type,
      <static-initializer>,
      <abstract-method-declaration>,
        method-name,
        method-return-type, // #f means VOID (except in constructors)
        method-parameters,
        method-throws,
        <method-declaration>, // non empty body
          <constructor-declaration>;

  export
    <formal-parameter>, <formal-parameters>,
      parameter-name,
      parameter-type;

  export
    <variable-declarator>, <variable-declarators>,
      variable-declarator-name,
      variable-declarator-numdims,
      variable-declarator-init; // false or <variable-initializer>

  export
    <variable-initializer>, <variable-initializers>,
      <array-initializer>,
        array-inits,
      <expression>,
        <literal>,
        <name>,
        <array-access>,
          array-access-value,
          array-access-index,
        <field-access>,
          field-access-value,
          field-access-field,
        <cast-expression>,
          cast-type,
          cast-value,
        <binary-expression>,
          expression-argument-1,
          expression-argument-2,
          expression-operator,
          <assignment>,
        <unary-expression>,
          expression-argument,
          expression-operator,
          <pre-expression>,
          <post-expression>,
        <if-expression>,
          if-expression-condition,
          if-expression-true-value,
          if-expression-false-value,
        <instanceof-expression>,
          instanceof-type,
          instanceof-value,
        <method-call>,
          method-call-args,
          method-call-name,
          method-call-class,
          <named-method-call>,
          <constructor-call>,
        <new-array-expression>,
          new-array-type,
          new-array-dims,
        <new-class-expression>,
          new-class-type,
          new-class-args,
        <this-token>,
        <super-token>;

  export
    <operator>,
      operator-symbol,
      <assignment-operator>, // includes ++ and --
        assignment-operator;

  export
    <block-statement>, <block-statements>,
      <local-variable-declaration>,
        local-variable-declarators,
        local-variable-type,
      <statement>, <statements>,
        <block>,
          block-statements,
        <empty-statement>,
        <switch-statement>,
          switch-statement-value,
          switch-statement-cases,
        <do-statement>,
          do-statement-condition,
          do-statement-body,
        <break-statement>,
          break-statement-label,
        <continue-statement>,
          continue-statement-label,
        <return-statement>,
          return-statement-value,
        <synchronized-statement>,
          synchronized-statement-condition,
          synchronized-statement-body,
        <throw-statement>,
          throw-statement-value,
        <try-statement>,
          try-statement-body,
          try-statement-catches,
          try-statement-finally,
        <labeled-statement>,
          labeled-statement-label,
          labeled-statement-statement,
        <if-statement>,
          if-statement-condition,
          if-statement-true-value,
          if-statement-false-value,
        <while-statement>,
          while-statement-condition,
          while-statement-body,
        <for-statement>,
          for-statement-init, // statement, statements, or local var
          for-statement-update,
          for-statement-condition,
          for-statement-body,
        <statement-expression>,
          // See details of these under <expression>
          <constructor-call>,
          <method-call>,
          <assignment>,
          <pre-expression>,
          <post-expression>,
          <new-class-expression>;

  export
    <switch-case>, <switch-cases>,
      switch-case-labels, // expressions or #f
      switch-case-body;

  export
    <catch>, <catches>,
      catch-parameter,
      catch-body;

  export
    <modifiers>, // subtype of <integer>
      modifiers-symbols, // mask => sequence of symbols
      $public-modifier,
      $protected-modifier,
      $private-modifier,
      $static-modifier,
      $abstract-modifier,
      $final-modifier,
      $native-modifier,
      $synchronized-modifier,
      $transient-modifier,
      $volatile-modifier;

end module;

