Module: infix-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Compilation.

define method compile-procedural-pattern
    (f* :: <list>) => (free-names)
  let state = make(<ptc-state>);
  for (token in f*)
    compile-pattern-fragment(token, state)
  end;
  values(map(name, state.pattern-variables),
         map(name, state.sequence-pattern-variables))
end method;

define method compile-procedural-pattern
    (p :: <sequence-fragment>) => (free-names)
  compile-procedural-pattern(p.fragments)
end method;

define method compile-procedural-pattern
    (p :: <statement-rule-pattern>) => (free-names)
  compile-procedural-pattern(p.pattern)
end method;

define method compile-procedural-pattern
    (p :: <define-rule-pattern>) => (free-names)
  compile-procedural-pattern
    (concatenate(p.modifiers-pattern.fragments, p.pattern.fragments));
end method;

define method compile-procedural-pattern
    (p :: <function-rule-pattern>) => (free-names)
  compile-procedural-pattern(p.pattern)
end method;

define method compile-procedural-pattern
    (p :: <local-declaration-rule-pattern>) => (free-names)
  compile-procedural-pattern(pair(p.body-pattern, p.pattern.fragments))
end method;

define method compile-pattern-fragment 
    (token, state) => ()
end method;

define method compile-pattern-fragment 
    (token :: <pattern-variable>, state) => ()
  state.pattern-variables 
    := add-new!(state.pattern-variables, token,
                test: same-pattern-variable?);
end method;

define method compile-pattern-fragment 
    (token :: <sequence-pattern-variable>, state) => ()
  state.sequence-pattern-variables 
    := add-new!(state.sequence-pattern-variables, token,
                test: same-pattern-variable?);
end method;

define method compile-pattern-fragment 
    (token :: <bracketed-fragment>, state) => ()
  for (fragment in token.fragments)
    compile-pattern-fragment(fragment, state);
  end;
end method;

define method compile-pattern-fragment 
    (token :: <sbracketed-fragment>, state) => ()
  for (fragment in token.fragments)
    compile-pattern-fragment(fragment, state);
  end;
end method;

define method compile-pattern-fragment 
    (token :: <cbracketed-fragment>, state) => ()
  for (fragment in token.fragments)
    compile-pattern-fragment(fragment, state);
  end;
end method;

define method compile-pattern-fragment 
    (token :: <property-list-pattern>, state) => ()
  if (token.rest?) 
    compile-pattern-fragment(token.rest-pattern, state);
  end;
  if (token.key?) 
    for (p in token.key-patterns)
      compile-pattern-fragment(p.head, state);
    end;
  end;
end method;

define method compile-pattern-fragment 
    (token :: <variable-pattern>, state) => ()
  compile-pattern-fragment(token.name, state);
  compile-pattern-fragment(token.type, state);
end method;

//// Matching.

// Hack!!! This is a hack used to distinguish the case of a macro versus
// a macro-case. The natural way to structure this is to place a common
// superclass about <template-macro> and <macro-rules> but we do not
// do that now because it requires restructuring classes that are
// fasl-dumped, essentially changing out fasl format and provoking
// much aggravation...

define class <macro-rules> (<template-macro>)
end class;

define method process-macro-rules (#rest stuff)
  apply(make, <macro-rules>, word: #"macro-case", stuff)
end method;

define method process-main-rule-set (macro-name == #"macro-case", main-rules)
end method;

define method compile-main-rule (rule :: <aux-rule>, m)
  compile-aux-rule(rule, #f, m)
end method;

define method do-rules (f, m)
  for (rule in m.main-rules) f(rule) end;
  for (rule-set in m.aux-rule-sets) 
    for (rule in rule-set.aux-rules) f(rule) end;
  end;
end method;

define method install-procedural-templates (m, #rest templates)
  let cursor = 0;
  do-rules(method (rule) 
             if (~instance?(rule.template, <template>))
               let names-and-function = templates[cursor];
               rule.template 
                 := make-procedural-template
                      (names-and-function.head, names-and-function.tail);
               cursor := cursor + 1;
             end
           end,
           m);
  m
end method;

define method uninstall-procedural-templates (m)
  do-rules(method (rule) 
             if (~instance?(rule.template, <template>))
               rule.template := #f;
             end
           end,
           m);
  m
end method;

define method run-macro-rules (m, f)
  apply-aux-rule-set
    (m, #f, make(<aux-rule-set>, name: #f, aux-rules: m.main-rules), 
     as-fragment(f).fragments);
end method;

// eof
