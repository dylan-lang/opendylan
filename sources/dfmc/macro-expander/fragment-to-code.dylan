Module:    dfmc-macro-expander
Synopsis:  Generate code to reconstruct a given fragment.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method generate-constructor (f :: <literal-fragment>)
  let value = fragment-value(f);
  #{ make-literal-fragment(?value) }
end method;

define method generate-constructor (f :: <name-fragment>)
  let name = fragment-name(f);
  #{ make-name-fragment(?name) }
end method;

define method generate-constructor (f :: <escaped-name-fragment>)
  let name = fragment-name(f);
  #{ make-escaped-name-fragment(?name) }
end method;

define method generate-constructor (f :: <unhygienic-name-fragment>)
  let name = fragment-name(fragment-name(f));
  #{ make-unhygienic-name-fragment(?name) }
end method;

define method generate-constructor (f :: <binary-operator-fragment>)
  let name = fragment-name(f);
  #{ make-binary-operator-fragment(?name) }
end method;

define method generate-constructor (f :: <unary-operator-fragment>)
  let name = fragment-name(f);
  #{ make-unary-operator-fragment(?name) }
end method;

define method generate-constructor (f :: <unary-and-binary-operator-fragment>)
  let name = fragment-name(f);
  #{ make-unary-and-binary-operator-fragment(?name) }
end method;

define method generate-constructor (f :: <constrained-name-fragment>)
  let symbol = fragment-name(f);
  let constraint = fragment-constraint(f);
  #{ make-constrained-name-fragment(?symbol, ?constraint) }
end method;

define method generate-constructor (f :: <escaped-substitution-fragment>)
  generate-constructor(fragment-escaped-fragment(f));
end method;

//// Punctuation.

define macro punctuation-generator-definer
  { define punctuation-generator "<" ## ?:name ## ">"; }
    => { define method generate-constructor (f :: "<" ## ?name ## ">")
           #{ "make-" ## ?name() }
         end method; }
end macro;

define punctuation-generator <equal-fragment>;

define punctuation-generator <equal-greater-fragment>;
define punctuation-generator <colon-colon-fragment>;
define punctuation-generator <dot-fragment>;
define punctuation-generator <comma-fragment>;
define punctuation-generator <semicolon-fragment>;
define punctuation-generator <lparen-fragment>;
define punctuation-generator <rparen-fragment>;
define punctuation-generator <lbracket-fragment>;
define punctuation-generator <rbracket-fragment>;
define punctuation-generator <lbrace-fragment>;
define punctuation-generator <rbrace-fragment>;

define punctuation-generator <hash-lparen-fragment>;
define punctuation-generator <hash-lbracket-fragment>;

define punctuation-generator <hash-next-fragment>;
define punctuation-generator <hash-rest-fragment>;
define punctuation-generator <hash-key-fragment>;
define punctuation-generator <hash-all-keys-fragment>;

define punctuation-generator <query-fragment>;
define punctuation-generator <query-query-fragment>;
define punctuation-generator <query-equal-fragment>;
define punctuation-generator <ellipsis-fragment>;

define punctuation-generator <hash-lbrace-fragment>;

// eof
