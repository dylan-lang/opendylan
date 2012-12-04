Module: dylan-user
Author: Toby Weinberg
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library c-lexer
  use common-dylan;
  use collections;
  use io;
  use system;
  use t-lists;
  use generic-arithmetic;
  use big-integers;
  export c-lexer-utilities;
  export c-lexer;
  export cpp;
  export c-lexer-test-interface;
end library;


// Test Interface module -- exports internal stuff for unit testing
define module c-lexer-test-interface
  create test-pre-lexer;
  create test-lexer;
  create test-ansi-cpp-evaluator;
  // create test-cpp-unlex;
  create with-emulator, with-dfmc; // export them so the compiler will shut-up
end module;

define module c-lexer-utilities
  create
    print-separated-sequence, print-comma-separated-sequence;
  create source-name; // , current-token removed this
                      // only needed for debugging I think
end module;

define module c-lexer-utilities-internal
  use common-dylan, exclude: { <union>, clex-digit?, format-to-string };
  use streams;
  use standard-io;
  use print;
  use format;
  use format-out;
  use t-lists;
  // re-export stuff from 'c-lexer-utilities' module
  use c-lexer-utilities, export: all;
end module;

// Lexer interface module
define module C-lexer
  use streams,
    export: {read-element, unread-element, inner-stream-setter, read-to-end};
  create
    <C-lexer>,
      <ansi-C-lexer>, <Microsoft-C-lexer>, <C++-lexer>,
    <pre-lexer>, <numbered-string-stream>,
    read-to-end-of-inner-stream,
    read-include-filename,
    current-line, lex-from-string,
    <associativity-type>;
  create  // functions on tokens
    lexer-string, lexer-string-setter, source-line, source-line-setter,
    parser-tag, dylan-value, copy-token, copy-token-list,
    quoted-string, constant-value, precedence, associativity,
    internal-lexer-string-value, internal-lexer-string-value-setter;
  create    //  token classes
    <token-list>,
    <token>,
      <white-space>, <macro-terminator>, <space>, <empty-token>,
      <new-line>, <pound>, <pound-pound>, <identifier>, <punctuation>,
      <open-parenthesis>, <close-parenthesis>, <open-bracket>,
      <close-bracket>, <open-brace>, <close-brace>, <comma>, <semi-colon>,
      <ellipsis>, <eoi>, <literal-token>, <character-literal>,
      <string-literal>, <include-filename>, <ordinary-filename>,
      <standard-filename>, <integer-literal>, <octal-integer-literal>,
      <decimal-integer-literal>, <hexadecimal-integer-literal>, <float-literal>,
      <symbol-token>, <unary-operator>, <binary-operator>, <dot-star>,
      <dot>, <and-and>, <and-assign>, <and>, <multiply-assign>, <star>,
      <plus-assign>, <plus>, <unary-plus>, <plus-plus>, <minus-assign>,
      <minus>, <unary-minus>, <minus-minus>, <arrow>, <tilde>, <not-equal>,
      <not>, <divide-assign>, <divide>, <remainder-assign>, <remainder>,
      <left-shift-assign>, <left-shift>, <less-than-or-equal>, <less-than>,
      <right-shift-assign>, <right-shift>, <greater-than-or-equal>,
      <greater-than>, <carat-assign>, <carat>, <or-assign>, <or>, <or-or>,
      <equal-equal>, <equal>, <question>, <colon>, <colon-colon>,
      <reserved-word>, <identifier-or-typedef-name>,
        <__unaligned>,
        <linkage-specifier>,
          <__cdecl>, <_cdecl>, <__stdcall>, <_inline>, <__inline>,
          <__fastcall>,
        <Microsoft-attribute>,
          <__asm>,  <__based>,
        <storage-class>,
          <Microsoft-storage-class>,
            <__declspec>,
            <ansi-storage-class>,
              <auto>, <register>, <static>, <extern>, <typedef>,
        <type-specifier>,
          <Microsoft-type-specifier>,
            <__int8>, <__int16>, <__int32>, <__int64>,
            <ansi-type-specifier>,
              <void>, <char>, <short>, <int>, <long>, <C-float-type>,
              <double>, <signed>, <unsigned>,
        <type-qualifier>,
          <Microsoft-type-qualifier>,
            <__unaligned>,
            <ansi-type-qualifier>,
              <const>, <volatile>,
        <struct-or-union>,
          <struct>, <union>, <enum>,
        <break>, <case>, <continue>, <default>, <do>, <else>, <for>, <goto>,
        <if>, <return>, <sizeof>, <switch>, <while>, <typedef-name>;
  create $eoi-token;
// These should move to c-lexer utilities as should all of the stuff in
// characters.dylan.
  create $largest-identifier-char, $smallest-identifier-char,
    $identifier-char-range, $identifier-char-range-squared,
    clex-digit?, clex-alpha-not-underscore?, clex-alphanumeric-not-underscore?,
    clex-integer-to-string;
end module;

define module C-lexer-internal
  use common-dylan, exclude: { format-to-string }; //  exclude: {<union>, close, clex-digit?}
  use streams, export: {read-element, unread-element};
  use standard-io;
  use print;
  use format;
  use format-out;
  use t-lists;
  use generic-arithmetic, prefix: "stupid-";
  use c-lexer-utilities;
  use C-lexer, export: all;
  use c-lexer-test-interface;
  use C-lexer;
end module C-lexer-internal;

// cpp interface
define module cpp
  use streams,
    export: {read-element, unread-element};
  create
    <C-identifier-table>, // probably should be in c-lexer-utilities module
    <cpp-stream>,
      macro-definitions, inner-stream-stack, skip-stack,
      *cpp-include-path*, add-cpp-include-path!, cpp-handle-pragma,
    cpp-handle-include-entry, cpp-handle-include-exit,
      <ANSI-cpp-stream>, <Microsoft-cpp-stream>, <C++-cpp-stream>,
      <pragma-token>, pragma-body, pragma-body-setter;
end module cpp;

define module cpp-internal
  use common-dylan, exclude: { <union>, format-to-string };
  use table-extensions, exclude: { table };
  use streams, export: {read-element, unread-element};
  use standard-io;
  use locators;
  use file-system;
  use print;
  use format;
  use format-out;
  use t-lists;
  use c-lexer-utilities;
  use cpp,
    export: {<cpp-stream>};
  use C-lexer;
  use c-lexer-test-interface;
end module cpp-internal;

