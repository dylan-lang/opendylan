Module: infix-reader
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Words to reclassify.

define method reclassify-using-value? (class)
  member?
    (class, #(#"<symbol>", #"<defining-word>", #"<fragment-begin-word>",
              #"<fragment-define-word>", #"<fragment-define-bindings-word>",
              #"<details-begin-word>", #"<expr-begin-word>",
              #"<simple-begin-word>", #"<details-intermediate-word>",
              #"<expr-intermediate-word>", #"<simple-intermediate-word>",
              #"<method-defining-word>", #"<generic-function-defining-word>",
              #"<seal-word>", parsed-name:))
end method;

define method reclassify-using-class? (class)
  member?(class, #(method:, let:, local:, generic:))
end method;

//// Hacked global macro environment

define constant $macros = make(<object-table>);

define method lookup-macro (macro-word, #key default)
  element($macros, macro-word, default: default)
end method;

define method lookup-expander (macro-word, #key default)
  let m = element($macros, macro-word, default: #f);
  if (m)
    compute-expander(m);
  else
    default;
  end;
end method;
      
//// Definition processing

define variable *last-rules* = #f;

define method 
    process-infix-macro-definition (name, #rest args, #key main-rules)
  let m = apply(make, <template-macro>, word: name, args);
  $macros[name] := m;
  *last-rules* := main-rules;
  values(m, register-macro-word(m, main-rules.head));
end method;

define method macro-word (rule :: <main-rule>)
  rule.name
end method;

define method macro-word (rule :: <define-rule>)
  concatenate-symbols(rule.name, #"-definer")
end method;

define method macro-word-in-variable-name
    (compiled-macro, variable-name) => (word, word-class)
  let (suffix, word-class)
    = macro-suffix-and-word-class(compiled-macro);
  if (empty?(suffix))
    values(variable-name, word-class)
  else
    if (~definer-name?(variable-name))
      values(#f, word-class)
    else
      let variable-name-string = as(<string>, variable-name);
      values(as(<symbol>,
                copy-sequence
                  (variable-name-string, 
                   end: variable-name-string.size - $definer-suffix-size)),
             word-class)
    end
  end
end method;

define method macro-suffix-and-word-class (m :: <template-macro>)
  macro-suffix-and-word-class(m.main-rules.first)
end method;

define method macro-suffix-and-word-class (rule :: <function-rule>)
  values("", #"<fragment-function-word>");
end method;

define method macro-suffix-and-word-class (rule :: <local-declaration-rule>)
  values("", #"<fragment-local-declaration-word>")
end method;

define method macro-suffix-and-word-class (rule :: <statement-rule>)
  values("", #"<fragment-begin-word>");
end method;

define method macro-suffix-and-word-class (rule :: <define-rule>)
  values("-definer", #"<fragment-define-word>");
end method;

define method macro-suffix-and-word-class (rule :: <define-bindings-rule>)
  values("-definer", #"<fragment-define-bindings-word>");
end method;

define method 
    process-compiler-macro-definition (name, #rest args, #key main-rules)
  let m = apply(make, <template-macro>, word: name, args);
  values(m, macro-word(main-rules.head));
end method;

// Called during run-time re-registration.

define method register-macro (m :: <template-macro>)
  let word = register-macro-word(m, m.main-rules.head);
  $macros[word] := m;
  word
end method;

//// Call in hooks

define method process-infix-macro-call (m, f :: <fragment>)
  let expander = compute-expander(m);
  expander(f);
end method;

// HACK!!! dodgy modifiers handling

define method process-infix-macro-call (m, f :: <vector>)
  let modifiers = sequence-fragment(fragments: fragmentize-sequence(f.first));
  let body-fragment = f.second;
  process-infix-macro-call
    (m, modified-fragment(modifiers: modifiers, fragment: body-fragment));
end method;

define method expand-template-macro 
    (m :: <template-macro>, f :: <vector>, 
       #rest options, #key constraint = body-constraint:, parser = run-parser)
  let modifiers = sequence-fragment(fragments: fragmentize-sequence(f.first));
  let body-fragment = f.second;
  apply(expand-template-macro, m,
        modified-fragment(modifiers: modifiers, fragment: body-fragment),
        options);
end method;

// HACK!!! dodgy function rule stuff

define method process-infix-macro-call (m, f :: <list>)
  process-infix-macro-call
    (m, sequence-fragment(fragments: fragmentize-list(f)));
end method;

define method process-infix-setter-macro-call (m, val, f)
  f.fragments := pair(most-specific-parsed-fragment(val), 
                      pair(*comma*, f.fragments));
  process-infix-macro-call(m, f);
end method;

// Obsolete?

define method process-infix-macro-call (m, f)
  process-infix-macro-call(m, list(f))
end method;

// Part of the above hack - convert prefix code fragments into a parsed
// fragment form.

define method fragmentize-list (forms :: <list>)
  let fragments = #();
  for (cursor = reverse(forms) then cursor.tail, until: empty?(cursor))
    fragments := pair(most-specific-parsed-fragment(cursor.head), fragments);
    unless (empty?(cursor.tail))
      fragments := pair(*comma*, fragments);
    end;
  end;
  fragments
end method;

define method fragmentize-sequence (forms :: <list>)
  map(most-specific-parsed-fragment, forms)
end method;

define method fragmentize-variable (form :: <symbol>)
  list(most-specific-parsed-fragment(form))
end method;

define method fragmentize-variable (form :: <list>)
  list(most-specific-parsed-fragment(form.first),
       *colon-colon*,
       most-specific-parsed-fragment(form.second))
end method;

define method fragmentize-case-body (forms :: <list>)
  let fragments = #();
  for (cursor = reverse(forms) then cursor.tail, until: empty?(cursor))
    fragments := concatenate(fragmentize-case(cursor.head), fragments);
    unless (empty?(cursor.tail))
      fragments := pair(*semicolon*, fragments);
    end;
  end;
  fragments
end method;

define method fragmentize-case (one-case :: <list>)
  let tests = if (one-case.first == #"otherwise")
                list(literal-fragment
                       (object: #"otherwise",
                        token-class: otherwise:, 
                        token-value: #"otherwise"))
              else
                fragmentize-list(one-case.first)
              end;
  let action = make(<parsed-fragment>,
                    token-class: parsed-body:,
                    token-value: one-case.second);
  concatenate(tests, list(*implies*, action))
end method;

define method most-specific-parsed-fragment (f :: <symbol>)
  if (keyword?(f))
    literal-fragment(object: f, token-class: #"<keyword>", token-value: f)
  else
    literal-fragment(object: f, token-class: #"<symbol>", token-value: f)
  end
end method;

define method most-specific-parsed-fragment (f)
  parsed-fragment(token-class: parsed-expression:, token-value: f)
end method;

// eof
