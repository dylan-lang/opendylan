Module:    infix-reader
Synopsis:  Macro objects
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Abstract class of all macro objects

define abstract class <macro> (<object>) end class;

define generic compute-expander (m :: <macro>);

//// Superclass of all template macros

define class <template-macro> (<object>)
  slot word,
    required-init-keyword: word:;
  slot main-rules,
    required-init-keyword: main-rules:;
  slot aux-rule-sets,
    init-value:   #(),
    init-keyword: aux-rule-sets:;
  slot aux-rule-set-table,
    init-function: curry(make, <object-table>);
end class;

define method initialize (m :: <template-macro>, #key)
  next-method();
  process-main-rule-set(m.word, m.main-rules);
  process-aux-rule-sets(m);
  compile-patterns(m);
  m;
end method;

// Sets.

define class <aux-rule-set> (<object>)
  slot name,
    required-init-keyword: name:;
  slot aux-rules,
    required-init-keyword: aux-rules:;
end class;

define method aux-rule-set (#key name, aux-rules)
  make(<aux-rule-set>, name: name, aux-rules: aux-rules);
end method;

// All main rules patterns must be of the same basic type and have the
// same name trigger as that declared after define macro.

define method process-main-rule-set (macro-name, main-rules)
  let pattern-type = object-class(main-rules.head.pattern);
  for (rule in main-rules.tail)
    unless (object-class(rule.pattern) == pattern-type)
      error("The main rule patterns of ~s, ~s are of inconsistent types",
            macro-name, main-rules);
    end;
  end;
  let rule-name = process-macro-name(main-rules.head, macro-name);
  for (rule in main-rules)
    process-main-rule(rule, rule-name, macro-name, main-rules);
  end;
end method;

define method process-macro-name (rule, macro-name)
  macro-name
end method;

define method process-main-rule (rule, rule-name, macro-name, main-rules)
  unless (rule.name == rule-name)
    error("The main rule patterns of ~s, ~s contain names inconsitent "
          "with its binding (e.g. ~s)",
           macro-name, main-rules, rule.name);
  end;
end method;

// Check for duplication and stash into the table.

define method process-aux-rule-sets (m :: <template-macro>)
  let duplicated = #();
  for (rule-set in m.aux-rule-sets)
    if (element(m.aux-rule-set-table, rule-set.name, default: #f))
      duplicated := pair(rule-set.name, duplicated);
    end;
    element(m.aux-rule-set-table, rule-set.name) := rule-set;
  end;
  if (~empty?(duplicated))
    error("Some auxilliary rule names are used more than once in ~s - ~s",
          m.word, duplicated);
  end;
end method;

// Determine the signpost words for each constrained pattern variable

define method compile-patterns (m :: <template-macro>)
  for (rule in m.main-rules)
    compile-main-rule(rule, m);
  end;
  for (set in m.aux-rule-sets)
    let rule-name = set.name;
    for (rule in set.aux-rules)
      compile-aux-rule(rule, rule-name, m);
    end;
  end;
end method;

define method compile-pattern-fragments (f*, rule-name, m)
  for (cursor = f* then cursor.tail, until empty?(cursor))
    let token = cursor.head;
    if (instance?(token, <constrained-pattern-variable>)
          & (token.type == #"body" | token.type == #"case-body")
          & ~empty?(cursor.tail))
      compile-pattern-variable(token, cursor.second, rule-name, m, #());
    elseif (instance?(token, <bracketed-fragment>))
      compile-pattern-fragments(token.fragments, rule-name, m);
    end;
  end;
end method;

// Compile-pattern-variable takes a variable and the following token.
// If the following token is a word, then it is registered as a 
// parse terminator. If the token is a rewritten pattern variable,
// the lead token of each of the patterns of that aux-rule are
// similarly compiled.

define method compile-pattern-variable (v, p, rule-name, m, seen)
end method;

define method compile-pattern-variable 
    (v, p :: <literal-fragment>, rule-name, m, seen) 
  register-parse-terminator(v, p);
end method;

define method compile-pattern-variable 
    (v, p :: <pattern-variable>, rule-name, m, seen) 
  let aux-rules = 
    element(m.aux-rule-set-table,
            if (ellipsis-pattern?(p))
              rule-name
            else
              p.name
            end,
            default: #f);
  if (aux-rules)
    compile-pattern-variable(v, aux-rules, rule-name, m, seen);
  end;
end method;

define method compile-pattern-variable
    (v, p :: <aux-rule-set>, rule-name, m, seen)
  if (~member?(p.name, seen))
    let seen = pair(p.name, seen);
    for (rule in p.aux-rules)
      let f* = rule.pattern.fragments;
      if (~empty?(f*))
        compile-pattern-variable(v, f*.head, rule-name, m, seen);
      end;
    end;
  end;
end method;

define method compute-expander (m :: <template-macro>)
  method (f :: <fragment>)
    expand-template-macro(m, f);
  end;
end method;

// Do an expansion

define method expand-template-macro 
    (m :: <template-macro>, f :: <fragment>, 
       #key constraint = body-constraint:, parser = run-parser)
  block (return)
    let (#rest vals) = apply-main-rule-set(m, m.main-rules, f);
    if (parser)
      return(reparse(vals.first, constraint: constraint, parser: parser));
    else
      apply(return, vals)
    end;
  exception (c :: <match-failure>)
    construct-invalid-syntax-error(m, f, c);
  end;
end method;

define class <main-rule-set-match-failure> (<match-failure>)
  slot match-failures,
    required-init-keyword: match-failures:;
end class;

define method apply-main-rule-set (m, main-rule-set, f)
  block (return)
    let failures = #();
    for (rule in main-rule-set)
      let env
        = block ()
            trace-main-rule-call(m.word, rule, f);
            match-call(rule, f);
          exception (c :: <match-failure>)
            failures := pair(c, failures);
            trace-main-rule-fail(m.word, rule);
            #f;
          end;
      if (env)
        apply-aux-rules!(m, rule, env);
        let (#rest vals) = apply-template(rule.template, env);
        trace-main-rule-return(m.word, rule, first(vals, default: #f));
        apply(return, vals);
      end;
    end;
    // No rewrites of the auxilliary rule set matched
    signal(make(<main-rule-set-match-failure>, match-failures: failures));
  end;
end method;

define method apply-aux-rules! (m, rule, env, #key self)
  for (b in env.bindings)
    let name = b.head;
    let rule-set = 
      element(m.aux-rule-set-table, 
              if (ellipsis-name?(name))
                self
              else
                name
              end,
              default: #f);
    if (rule-set)
      let val = b.tail;
      if (bound-sequence?(val))
        b.tail
          := map-as(<vector>, method (fragment)
                             apply-aux-rule-set(m, rule, rule-set, fragment);
                           end,
                           bound-sequence(val));
      else
        b.tail := apply-aux-rule-set(m, rule, rule-set, b.binding-value);
      end;
    end;
  end;
end method;

define class <aux-rule-set-match-failure> (<match-failure>)
  slot match-failures,
    required-init-keyword: match-failures:;
end class;

define method apply-aux-rule-set (m, main-rule, aux-rule-set, f)
  block (return)
    let failures = #();
    for (aux-rule in aux-rule-set.aux-rules)
      block (try-next-rule)
        // We only try another auxilliary rule when matching fails.
        // If matching succeeds we are committed - we don't backtrack
        // on failures in auxilliary rules subsequently applied to the 
        // match. For that behaviour wrap the block around the the
        // apply-aux-rules! and return as well. 
       let aux-env = 
          block ()
            trace-aux-rule-call(aux-rule-set.name, aux-rule, f);
            match-aux-rule(main-rule, aux-rule, f);
          exception (c :: <match-failure>)
            failures := pair(c, failures);
            trace-aux-rule-fail(aux-rule-set.name, aux-rule);
            try-next-rule();
          end;
        apply-aux-rules!(m, main-rule, aux-env, self: aux-rule-set.name);
        let (#rest vals) = apply-template(aux-rule.template, aux-env);
        trace-aux-rule-return
          (aux-rule-set.name, aux-rule, first(vals, default: #f));
        apply(return, vals)
      end;
    end;
    // No rewrites of the auxilliary rule set matched
    signal(make(<aux-rule-set-match-failure>, match-failures: failures));
  end;
end method;

define method make-procedural-template (names, function)
  method (env)
    apply(function, 
          map(method (name)
                let binding = lookup-raw(env, name);
                externalise-fragment(binding.tail)
              end,
              names))
  end
end method;

define method apply-template (template :: <function>, env)
  template(env)
end method;

define method apply-template (template :: <template>, env)
  template-closure(template, env);
end method;

//// Error message construction

define method construct-invalid-syntax-error (m, f, c)
  let string = "Invalid syntax for ~a";
  let args = list(m.word);
  for (subc in c.match-failures)
    let (substring, subargs) = invalid-syntax-error-component(subc);
    string := concatenate(string, substring);
    args := concatenate(args, subargs);
  end;
  apply(match-error, string, args);
end method;

define method construct-invalid-syntax-error-string (macro-name, c)
  let string = "Invalid syntax in ~a:";
  let args = list(macro-name);
  for (subc in c.match-failures)
    let (substring, subargs) = invalid-syntax-error-component(subc);
    string := concatenate(string, substring);
    args := concatenate(args, subargs);
  end;
  apply(format, #f, string, args);
end method;

define method invalid-syntax-error-component (c :: <pattern-match-failure>)
  if (c.cause)
    values("~&  - mismatch between \"~s\" and \"~s\"~%      Cause: ~a.", 
           list(c.fragment, c.pattern, c.cause));
  else
    values("~&  - mismatch between \"~s\" and \"~s\"",
           list(c.fragment, c.pattern));
  end;
end method;

define method invalid-syntax-error-component (c :: <aux-rule-set-match-failure>)
  let string = "";
  let args = #();
  for (subc in c.match-failures)
    let (substring, subargs) = invalid-syntax-error-component(subc);
    string := concatenate(string, substring);
    args := concatenate(args, subargs);
  end;
  values(string, args);
end method;

// eof
