Module:    infix-reader
Synopsis:  Function-rule rules and patterns
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Function rules.

define class <function-rule> (<main-rule>)
end class;

define method function-rule (#key name, pattern, template)
  make(<function-rule>, name: name, pattern: pattern, template: template);
end method;

define method register-macro-word 
    (m :: <template-macro>, rule :: <function-rule>)
  // no need for a special classification?
  m.word;
end method;

define method compile-main-rule (rule :: <function-rule>, m)
  let main-p = rule.pattern;
  let body-p = main-p.pattern;
  let body-fragments = body-p.fragments;
  compile-pattern-fragments(body-fragments, #f, m);
end method;

// Function patterns.

define pattern <function-rule-pattern> (<main-rule-pattern>)
  pattern slots pattern;
end pattern;;

define method match-main 
    (p :: <function-rule-pattern>, f :: <fragment>, e, fail)
  match-body(p.pattern.fragments, f.fragments, e, fail);
end method;

// eof
