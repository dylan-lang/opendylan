Module:    dfmc-macro-expander
Synopsis:  Generate a function to reconstruct a given fragment.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method generate-constructor-function (f :: <literal-fragment>)
  let value = fragment-value(f);
  match-method make-literal-fragment(value) end
end method;

define method generate-constructor-function (f :: <name-fragment>)
  let name = fragment-name(f);
  match-method make-name-fragment(name) end
end method;

define method generate-constructor-function (f :: <escaped-name-fragment>)
  let name = fragment-name(f);
  match-method make-escaped-name-fragment(name) end
end method;

define method generate-constructor-function (f :: <unhygienic-name-fragment>)
  let name = fragment-name(fragment-name(f));
  match-method make-unhygienic-name-fragment(name) end
end method;

define method generate-constructor-function (f :: <binary-operator-fragment>)
  let name = fragment-name(f);
  match-method make-binary-operator-fragment(name) end
end method;

define method generate-constructor-function (f :: <unary-operator-fragment>)
  let name = fragment-name(f);
  match-method make-unary-operator-fragment(name) end
end method;

define method generate-constructor-function 
    (f :: <unary-and-binary-operator-fragment>)
  let name = fragment-name(f);
  match-method make-unary-and-binary-operator-fragment(name) end
end method;

define method generate-constructor-function (f :: <constrained-name-fragment>)
  let symbol = fragment-name(f);
  let constraint = fragment-constraint(f);
  match-method make-constrained-name-fragment(symbol, constraint) end
end method;

define method generate-constructor-function 
    (f :: <escaped-substitution-fragment>)
  generate-constructor-function(fragment-escaped-fragment(f));
end method;

//// Punctuation.

define macro punctuation-generator-function-definer
  { define punctuation-generator-function "<" ## ?:name ## ">" }
    => { define method generate-constructor-function 
             (f :: "<" ## ?name ## ">")
           match-method "make-" ## ?name() end
         end method; }
end macro;

define punctuation-generator-function <equal-fragment>;

define punctuation-generator-function <equal-greater-fragment>;
define punctuation-generator-function <colon-colon-fragment>;
define punctuation-generator-function <dot-fragment>;
define punctuation-generator-function <comma-fragment>;
define punctuation-generator-function <semicolon-fragment>;
define punctuation-generator-function <lparen-fragment>;
define punctuation-generator-function <rparen-fragment>;
define punctuation-generator-function <lbracket-fragment>;
define punctuation-generator-function <rbracket-fragment>;
define punctuation-generator-function <lbrace-fragment>;
define punctuation-generator-function <rbrace-fragment>;

define punctuation-generator-function <hash-lparen-fragment>;
define punctuation-generator-function <hash-lbracket-fragment>;

define punctuation-generator-function <hash-next-fragment>;
define punctuation-generator-function <hash-rest-fragment>;
define punctuation-generator-function <hash-key-fragment>;
define punctuation-generator-function <hash-all-keys-fragment>;

define punctuation-generator-function <query-fragment>;
define punctuation-generator-function <query-query-fragment>;
define punctuation-generator-function <query-equal-fragment>;
define punctuation-generator-function <ellipsis-fragment>;

define punctuation-generator-function <hash-lbrace-fragment>;

// eof
