Module:    infix-reader
Synopsis:  Basic rule classes, patterns, and protocols
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Rules.

// All rules have a pattern and a template.

define abstract class <rule> (<object>)
  slot pattern,
    required-init-keyword: pattern:;
  slot template,
    required-init-keyword: template:;
end class;

define method compile-rule (r :: <rule>)
  let bound = collect-variables(r.pattern);
  let referenced = collect-variables(r.template);
end method;

//// Main rules.

define abstract class <main-rule> (<rule>)
  slot name,
    init-keyword: name:;
end class;

// Protocol.

define generic register-macro-word (mac, rule);
define generic compile-main-rule (rule, mac);
define generic match-call (rule, fragment);

// Defaults.

define method match-call (r :: <main-rule>, f :: <fragment>)
  block (return)
    let e = make(<env>);
    let fail = method (fail-p, fail-f, #key cause) 
                 signal(make(<pattern-match-failure>,
                             fragment: f,
                             pattern:  r.pattern,
                             cause:    cause));
               end;
    match-main(r.pattern, f, e, fail);
    return(e);
  end;
end method;

//// Main rule patterns.

define abstract pattern <main-rule-pattern> (<pattern-object>)
  pattern slots name;
end class;

define generic match-main (pattern, fragment, e, fail);

//// Auxilliary rules.

define class <aux-rule> (<rule>)
end class;

define method aux-rule (#key pattern, template)
  make(<aux-rule>, pattern: pattern, template: template);
end method;

define generic match-aux-rule (main, aux-rule, fragment);

define method compile-aux-rule (rule :: <aux-rule>, rule-name, m)
  compile-pattern-fragments(rule.pattern.fragments, rule-name, m);
end method;

define method match-aux-rule (main, r :: <aux-rule>, f)
  block (return)
    let e = make(<env>);
    let fail = method (fail-p, fail-f, #key cause) 
                 signal(make(<pattern-match-failure>,
                             fragment: sequence-fragment(fragments: f),
                             pattern:  r.pattern,
                             cause:    cause));
               end;
    match-aux(r.pattern, f, e, fail);
    return(e);
  end;
end method;

define method match-aux (p, f, e, fail)
  match-body(p.fragments, f, e, fail);
end method;

// eof
