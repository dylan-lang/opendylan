Module:    dfmc-macro-expander
Synopsis:  Hacks to convert emulator flavour templates to compiler
           template description objects. Bootstrapping only.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro token-converter-definer
  { define token-converter ?:name => ?class:name, ?stuff; }
    => { define method convert-template-token (type == ?#"name", value)
           make(?class, record: #f, source-position: #f, ?stuff)
         end method; }
  { define token-converter ?:token => ?class:name, ?stuff; }
    => { define method convert-template-token (type == ?token, value)
           make(?class, record: #f, source-position: #f, ?stuff)
         end method; }
end macro;

define token-converter <symbol> => <variable-name-fragment>, name: value;
define token-converter <ellipsis> => <variable-name-fragment>, name: value;

define token-converter <defining-word> => <variable-name-fragment>, name: value;
define token-converter <fragment-define-word> => <variable-name-fragment>, name: value;
define token-converter <fragment-define-bindings-word> => <variable-name-fragment>, name: value;

define token-converter <literal> => <literal-fragment>, value: value;
define token-converter <string> => <literal-fragment>, value: value;
define token-converter <keyword> => <literal-fragment>, value: value;

define token-converter <implies> => <equal-greater-fragment>;

define token-converter <binds> => <equal-fragment>;
define token-converter <var-singleton-sep> 
  => <binary-operator-fragment>, name: #"==";
define token-converter <minus> 
  => <unary-and-binary-operator-fragment>, name: #"-";
define token-converter <becomes> => <binary-operator-fragment>, name: #":=";
define token-converter <binary-operator> => <binary-operator-fragment>, name: value;
define token-converter <unary-operator> => <unary-operator-fragment>, name: value;
define token-converter <not> => <unary-operator-fragment>, name: #"~";

define token-converter <dot> => <dot-fragment>;
define token-converter <var-sep> => <comma-fragment>;
define token-converter <var-type-sep> => <colon-colon-fragment>;
define token-converter <statement-sep> => <semicolon-fragment>;

define token-converter <lbracket> => <lparen-fragment>;
define token-converter <rbracket> => <rparen-fragment>;
define token-converter <lsbracket> => <lbracket-fragment>;
define token-converter <rsbracket> => <rbracket-fragment>;
define token-converter <lcbracket> => <lbrace-fragment>;
define token-converter <rcbracket> => <rbrace-fragment>;

define token-converter <list-open> => <hash-lparen-fragment>;
define token-converter <vector-open> => <hash-lbracket-fragment>;

define token-converter &next: => <hash-next-fragment>;
define token-converter &rest: => <hash-rest-fragment>;
define token-converter &key: => <hash-key-fragment>;
define token-converter &all-keys: => <hash-all-keys-fragment>;

define token-converter parsed-literal: => <literal-fragment>, value: value;
  

define method convert-template (contents)
  if (empty?(contents))
    #()
  else
    let type = contents.first;
    if (keyword?(type) 
          & ~member?(type, #(&next:, &rest:, &key:, &all-keys:,
                             parsed-literal:)))
      let value 
        = make(<variable-name-fragment>, 
	       record: #f,
               source-position: #f,
               name: as(<symbol>, as(<string>, type)));
      pair(value,
           convert-template(contents.tail.tail));
    elseif (object-class(type) == <symbol>)
      let value = contents.second;
      pair(convert-template-token(type, value),
           convert-template(contents.tail.tail));
    else
      pair(convert-template-substitution(type),
           convert-template(contents.tail));
    end;
  end;
end method;

//// Substitution conversion.

define macro hack-import-definer
  { define hack-import ?names end }
    => { ?names }
names:
  { }
    => { }
  { ?:name, ... }
    => { define constant "old/" ## ?name 
           = access(infix-reader, ?name); ... }
end macro;

define hack-import 
  <unconstrained-pattern-variable>, name,
  <sequence-pattern-variable>, separator,
  <spliced-pattern-variable>, pattern, before, after,
  <string-pattern-variable-pattern>,
  <symbol-pattern-variable-pattern>,
  <parsed-fragment>, token-value,
  <expression-substitution>, expression,
end;

define method convert-template-substitution
    (subst :: old/<unconstrained-pattern-variable>)
  make(<simple-element-substitution>, 
       variable-name: make-old-expression(subst.old/name))
end method;

define method convert-template-substitution
    (subst :: old/<expression-substitution>)
  make(<simple-element-substitution>, 
       variable-name: make-old-expression(subst.old/expression))
end method;

define method convert-template-substitution
    (subst :: old/<string-pattern-variable-pattern>)
  make(<as-string-substitution>,
       variable-name: make-old-expression(subst.old/name.old/name))
end method;

define method convert-template-substitution
    (subst :: old/<symbol-pattern-variable-pattern>)
  make(<as-symbol-substitution>,
       variable-name: make-old-expression(subst.old/name.old/name))
end method;

define method convert-template-substitution
    (subst :: old/<sequence-pattern-variable>)
  let old-sep = subst.old/separator;
  make(<simple-sequence-substitution>, 
       variable-name: make-old-expression(subst.old/name),
       separator:     old-sep & apply(convert-template-token, old-sep));
end method;

define method convert-template-substitution
    (subst :: old/<spliced-pattern-variable>)
  make(<splicing-substitution>, 
       variable-name: #f,
       name-substitution: 
         convert-template-substitution(subst.old/pattern),
       prefix: subst.old/before | "",
       suffix: subst.old/after | "");
end method;

//// Hax.

// For some reason this seems to be needed when compiling macros loaded
// from a database.  I don't understand it, but it works. -gz, 4/12/97
define method fragment-name (f :: old/<parsed-fragment>)
  old/token-value(f)
end method;

define constant reparse
  = access(infix-reader, reparse);
define constant fragments
  = access(infix-reader, fragments);
define constant as-fragment 
  = access(infix-reader, as-fragment);
define constant <template-closure> 
  = access(infix-reader, <template-closure>);

define method as-fragment (c :: <template-closure>)
  c
end method;

define method fragments (c :: <template-closure>) c end;

// eof
