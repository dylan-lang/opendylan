Module:    infix-reader
Synopsis:  Local-declaration-rule rules and patterns
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Local declaration / function rule disambiguation.

define method function-or-local-declaration-rule 
    (#key name, pattern, template)
  if (instance?(pattern, <local-declaration-rule-pattern>))
    local-declaration-rule(name: name, pattern: pattern, template: template)
  else
    function-rule(name: name, pattern: pattern, template: template)
  end
end method;

define method function-or-local-declaration-rule-pattern (#key name, pattern)
  let body-fragments = pattern.fragments;
  // Look for a ?:body at the end end of the main rule pattern.
  if (body-fragments.size >= 2
        & instance?(body-fragments.last, <constrained-pattern-variable>)
        & body-fragments.last.type == #"body")
    let (before, after) 
      = split(body-fragments, curry(match-token, *semicolon*));
    local-declaration-rule-pattern
      (name: name, 
       pattern: 
         sequence-fragment(fragments: before),
       body-pattern:
         after.second);
  // Look for a single bracketed pattern.
  elseif (body-fragments.size = 1 
            & instance?(body-fragments.first, <bracketed-fragment>))
    function-rule-pattern
      (name: name, 
       pattern: sequence-fragment(fragments: body-fragments.first.fragments))
  else
    reader-error
      (#f, "Unrecognized main rule pattern shape in ~s, ~s.", name, pattern);
  end;
end method;

//// Local declaration rules.

define class <local-declaration-rule> (<main-rule>)
end class;

define method local-declaration-rule 
    (#key name, pattern, body-pattern, template)
  make(<local-declaration-rule>, 
       name: name, 
       pattern: pattern, 
       template: template);
end method;

define method register-macro-word 
    (m :: <template-macro>, rule :: <local-declaration-rule>)
  register-infix-macro-word(m.word, #"<fragment-local-declaration-word>");
  m.word;
end method;

define method compile-main-rule (rule :: <local-declaration-rule>, m)
  let main-p = rule.pattern;
  let body-p = main-p.pattern;
  let body-fragments = body-p.fragments;
  compile-pattern-fragments(body-fragments, #f, m);
end method;

// Function patterns.

define pattern <local-declaration-rule-pattern> (<main-rule-pattern>)
  pattern slots pattern, body-pattern;
end pattern;

define method match-main 
    (p :: <local-declaration-rule-pattern>, f :: <fragment>, e, fail)
  let (before, after) 
    = split(f.fragments, curry(match-token, *semicolon*));
  match-body(p.pattern.fragments, before, e, fail);
  match-body(p.body-pattern.list, after.tail, e, fail);
end method;

// eof
