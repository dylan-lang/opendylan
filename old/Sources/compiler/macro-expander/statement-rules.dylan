Module:    infix-reader
Synopsis:  Statement-rule rules and patterns
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Statement rules.

define class <statement-rule> (<main-rule>)
end class;

define method statement-rule (#key name, pattern, template)
  make(<statement-rule>, name: name, pattern: pattern, template: template);
end class;

define method register-macro-word 
    (m :: <template-macro>, rule :: <statement-rule>)
  register-infix-macro-word(m.word, #"<fragment-begin-word>");
  m.word;
end method;

define method compile-main-rule (rule :: <statement-rule>, m)
  let main-p = rule.pattern;
  let body-p = main-p.pattern;
  let body-fragments = body-p.fragments;
  compile-pattern-fragments(body-fragments, #f, m);
end method;

// Statement patterns.

define pattern <statement-rule-pattern> (<main-rule-pattern>)
  pattern slots pattern;
end pattern;

define method match-main 
    (p :: <statement-rule-pattern>, f :: <fragment>, e, fail)
  match-body(p.pattern.fragments, f.fragments, e, fail);
end method;

// eof
