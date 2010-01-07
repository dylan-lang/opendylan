Module:    dfmc-macro-expander
Synopsis:  Generate and compose functions to perform and bind the results of
           a pattern match.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro matcher-method
  { matcher-method ?:body end } 
    => { method 
             (?=env :: <match-environment>, ?=_f*_) => (failure, _f*_)
           ?body
         end }
end macro;

define method generate-rewrite-rule-expander-function 
    (exp :: <rewrite-rule-expander>) => (code :: <method>)
  let rule-env 
    = expander-aux-rule-set-env(exp);
  let aux-methods-code 
    = map(curry(generate-aux-rule-set-definition-function, exp, rule-env), 
          expander-aux-rule-sets(exp));
  let main-code
    = generate-main-rule-set-function
        (exp, rule-env, expander-main-rule-set(exp));
  let intermediate-words
    = expander-intermediate-words(exp);
  /*
  if (empty?(aux-methods-code))
    #{ let _intermediate-words_ = ?intermediate-words; 
       ?main-code }
  else
    #{ let _intermediate-words_ = ?intermediate-words; 
       local ??aux-methods-code, ...; 
       ?main-code }
  end;
  */
  main-code
end method;

define thread variable *intermediate-words* = #();

define method generate-main-rule-set-function (exp, rule-env, set)
  let main 
    = generate-rules-function(exp, rule-env, #f, rule-set-rewrite-rules(set));
  method (_f*_)
    dynamic-bind (*intermediate-words* = expander-intermediate-words(exp))
      main(_f*_);
    end;
  end;
end method;

define method generate-aux-rule-set-definition-function (exp, rule-env, set)
  let rules-code 
    = generate-rules-function
        (exp, rule-env, set, rule-set-rewrite-rules(set));
  lookup-aux-rule-function
    (rule-env, rule-set-rewriter-env-index(set)) := rules-code;
  rules-code
end method;

define method generate-rules-function (exp, rule-env, set, rules)
  if (empty?(rules))
    generate-match-failure-function(exp, set, rules);
  else
    generate-rule-function(exp, rule-env, set, rules.head, rules.tail);
  end;
end method;

define method generate-match-failure-function 
    (exp, set :: <aux-rewrite-rule-set>, rules)
  let name = expander-name(exp);
  let set-name = rule-set-name(set);
  method (_f*_)
    macro-aux-rule-match-error(_f*_, name, set-name);
  end
end method;

define method generate-match-failure-function
    (exp, set , rules)
  let name = expander-name(exp);
  method (_f*_)
    macro-main-rule-match-error(_f*_, name);
  end;
end method;

define method generate-rule-function (exp, rule-env, set, rule, more-rules)
  let pattern = rule-pattern(rule);
  let template-code = rule-compiled-template(rule);
  let binding-matches = compute-binding-matches(pattern);
  let bound-names = compute-bound-variable-names(pattern);
  let bound-names-size = size(bound-names);
  let match-code = generate-body-pattern-function(pattern);
  let more-match-code 
    = generate-rules-function(exp, rule-env, set, more-rules);
  let rewrite-code 
    = generate-bound-name-aux-rule-rewrites-function
        (exp, rule-env, set, binding-matches);
  method (_f*_)
    // format-out("Trying main rule\n");
    let env = make(<simple-object-vector>, size: bound-names-size);
    // format-out("Bound names: %=\n", bound-names);
    let failure = match-code(env, _f*_);
    // format-out("Matched: failure = %=, env = %=\n", failure, env);
    if (failure)
      more-match-code(_f*_);
    else
      call-list-with(rewrite-code, env);
      // format-out("After rewriting: env = %=\n", env);
      template-code(env);
    end;
  end;
end method;

define method generate-bound-name-aux-rule-rewrites-function
    (exp, rule-env, set, binding-matches)
  collecting ()
    for (match in binding-matches)
      let aux-set 
        = element(expander-aux-rule-set-table(exp), match-symbol-name(match),
                  default: #f);
      if (aux-set)
        let binding-index = match-env-index(match);
        let aux-rule-index = rule-set-rewriter-env-index(aux-set);
        // TODO: PERFORMANCE: Optimize out this as-fragment-tokens when
        // it isn't necessary, i.e. when rewriting a constrained thing
        // we can infer stuff about it. E.g. a ?:name that's rewritten
        // in an aux rule.
        if (instance?(match, <key-sequence-match>))
          collect
            (method (env)
               let rewrite
                 = lookup-aux-rule-function(rule-env, aux-rule-index);
               lookup-match(env, binding-index)
                 := map(compose(rewrite, as-fragment-tokens),
                        lookup-match(env, binding-index));
             end);
        else
          collect
            (method (env)
               let rewrite
                 = lookup-aux-rule-function(rule-env, aux-rule-index);
               lookup-match(env, binding-index)
                 := rewrite(as-fragment-tokens
                             (lookup-match(env, binding-index)));
             end);
        end if;
      else
        let index = match-env-index(match);
        if (instance?(match, <key-sequence-match>))
          collect
            (method (env)
               lookup-match(env, index)
                 := map(export-fragment-tokens, lookup-match(env, index));
             end);
        else
          collect
            (method (env)
               lookup-match(env, index)
                 := export-fragment-tokens(lookup-match(env, index));
             end);
        end if;
      end if;
    end for;
  end collecting;
end method;

define method generate-body-pattern-function (m* :: <list>)
  let (parts, n-parts) = split-at-semicolons(m*);
  generate-structure-parts-function
    (parts, n-parts, 
     match-body-part, match-body-part-strict, 
     generate-list-pattern-function);
end method;

/*
define method generate-nested-list-pattern-function (m*)
  generate-list-pattern-function(m*);
end method;
*/

define method generate-list-pattern-function (m* :: <list>)
  let (parts, n-parts) = split-at-commas(m*);
  generate-structure-parts-function
    (parts, n-parts, 
     match-list-part, match-list-part-strict, 
     generate-pattern-elements-function);
end method;

define method generate-structure-parts-function 
    (parts, n-parts, matcher, strict-matcher, submatcher)
  if (n-parts = 1)
    submatcher(parts.first);
  else
    let actual-matcher 
      = if (n-parts = 2) matcher else strict-matcher end;
    let before-code = submatcher(parts.first);
    let after-code 
      = generate-structure-parts-function
          (parts.tail, n-parts - 1, matcher, strict-matcher, submatcher);
    matcher-method
      let (failure, _f*-after_, _f*-before_) = actual-matcher(_f*_);
      failure
        | begin
            let (failure, _f*_) = before-code(env, _f*-before_);
            failure
              | after-code(env, _f*-after_)
          end
    end;
  end;
end method;

define method generate-pattern-elements-function (m* :: <list>)
  if (empty?(m*))
    generate-empty-function();
  else
    generate-pattern-element-function(m*.head, m*.tail);
  end;
end method;

define method generate-pattern-element-function
    (m :: type-union(<fragment>, <pattern-match>), m*)
  let m-code = generate-function(m);
  let m*-code = generate-pattern-elements-function(m*);
  matcher-method
    let (failure, _f*_) = m-code(env, _f*_);
    failure | m*-code(env, _f*_);
  end;
end method;

define method generate-pattern-element-function
    (m :: <name-fragment>, m*)
  if (fragment-name(m) ~== #"otherwise")
    next-method();
  else
    let m* 
      = if (instance?(m*.head, <equal-greater-fragment>))
          m*.tail
        else
          m*
        end;
    let m*-code = generate-pattern-elements-function(m*);
    matcher-method
      let (failure, _f*_) = match-otherwise(_f*_); 
      failure | m*-code(env, _f*_);
    end 
  end;
end method;

define method generate-pattern-element-function
    (m :: <simple-match>, m*)
  let constraint = match-constraint(m);
  if (~wildcard-constraint?(constraint))
    next-method();
  elseif (empty?(m*))
    let index = match-env-index(m);
    matcher-method
      lookup-match(env, index) := _f*_;
      values(#f, #());
    end;
  else
    let m*-code = generate-pattern-elements-function(m*);
    let index = match-env-index(m);
    matcher-method
      local method _wildcard-loop_ (_collector_, _f*_)
        let value = _collector_;
        // format-out("Try: %=\n", value);
        let failure = m*-code(env, _f*_);
        if (failure)
          if (empty?(_f*_))
            values(#t, #());
          else
            // Skip ahead to the next plausible match.
            _wildcard-loop_
               (concatenate(_collector_, list(_f*_.head)), _f*_.tail);
          end;
        else
          lookup-match(env, index) := value;
          values(#f, #());
        end;
      end;
      _wildcard-loop_(#(), _f*_);
    end;
  end;
end method;

define method generate-pattern-element-function (m :: <variable-match>, m*)
  let name-code = generate-function(match-variable-name-pattern(m));
  let type-code = generate-function(match-type-expression-pattern(m));
  let m*-code = generate-pattern-elements-function(m*);
  matcher-method
    let (failure, _after-f*_, _name-f*_, _type-f*_) = match-variable(_f*_);
    failure 
      | begin
          let (failure, _f*_) = name-code(env, _name-f*_);
          failure 
            | begin
                let (failure, _f*_) = type-code(env, _type-f*_);
                failure 
                  | m*-code(env, _after-f*_);
              end
        end
  end;
end method;

define method generate-pattern-element-function (m :: <nested-match>, m*)
  let nested-matcher = generate-nested-matcher-function(m);
  let nested-code 
    = generate-nested-pattern-elements-function(match-nested-pattern(m));
  let m*-code = generate-pattern-elements-function(m*);
  matcher-method
    let (failure, _after-f*_, _nested-f*_) = nested-matcher(_f*_);
    if (failure) 
      values(failure, #())
    else
      let failure = nested-code(env, _nested-f*_);
      if (failure) 
        values(failure, #())
      else
        m*-code(env, _after-f*_)
      end;
    end;
  end;
end method;

define method generate-nested-pattern-elements-function (m*)
  generate-body-pattern-function(m*);
end method;

define method generate-nested-matcher-function (m :: <paren-match>)
  match-parens 
end method;

define method generate-nested-matcher-function (m :: <bracket-match>)
  match-brackets 
end method;

define method generate-nested-matcher-function (m :: <brace-match>)
  match-braces
end method;

define method generate-function (m :: <name-fragment>)
  let name = fragment-name(m);
  matcher-method match-name(_f*_, name) end;
end method;

define method generate-function (m :: <operator-fragment>)
  let name = fragment-name(m);
  matcher-method match-operator(_f*_, name) end;
end method;

define method generate-function (m :: <end-of-modifiers-marker>)
  matcher-method match-end-of-modifiers(_f*_) end;
end method;

define method generate-function (m :: <equal-greater-fragment>)
  matcher-method match-equal-greater(_f*_) end;
end method;

define method generate-function (m :: <hash-next-fragment>)
  matcher-method match-hash-next(_f*_) end;
end method;

define method generate-function (m :: <hash-rest-fragment>)
  matcher-method match-hash-rest(_f*_) end;
end method;

define method generate-function (m :: <hash-key-fragment>)
  matcher-method match-hash-key(_f*_) end;
end method;

define method generate-function (m :: <hash-all-keys-fragment>)
  matcher-method match-hash-all-keys(_f*_) end;
end method;

define method generate-function (m :: <dot-fragment>)
  matcher-method match-dot(_f*_) end;
end method;

define method generate-function (m :: <colon-colon-fragment>)
  matcher-method match-colon-colon(_f*_) end;
end method;

define method generate-function (m :: <literal-fragment>)
  let value = fragment-value(m);
  matcher-method match-literal(_f*_, value) end;
end method;

define method generate-function (m :: <escaped-substitution-fragment>)
  generate-function(fragment-escaped-fragment(m));
end method;

define method generate-function (m :: <simple-match>)
  let index = match-env-index(m);
  let constraint = match-constraint(m);
  if (wildcard-constraint?(constraint))
    matcher-method
      let value = _f*_;
      lookup-match(env, index) := value;
      values(#f, #());
    end;
  elseif (bounded-constraint?(constraint))
    let matcher = generate-constraint-function(match-constraint(m));
    matcher-method
      let (failure, _f*_, value) 
        = matcher(_f*_, lookup-intermediate-words(env));
      lookup-match(env, index) := value;
      values(failure, _f*_);
    end;
  else
    let matcher = generate-constraint-function(match-constraint(m));
    matcher-method
      let (failure, _f*_, value) = matcher(_f*_);
      lookup-match(env, index) := value;
      values(failure, _f*_);
    end;
  end;
end method;

define method generate-constraint-function (constraint == #"token")
  match-token-constraint
end method;

define method generate-constraint-function (constraint == #"name")
  match-name-constraint
end method;

define method generate-constraint-function (constraint == #"expression")
  match-expression-constraint
end method;

define method generate-constraint-function (constraint == #"variable")
  match-variable-constraint
end method;

define method generate-constraint-function (constraint == #"body")
  match-bounded-body-constraint
end method;

define method generate-constraint-function (constraint == #"case-body")
  match-bounded-case-body-constraint
end method;

define method generate-constraint-function (constraint == #"macro")
  match-macro-constraint
end method;

define method generate-function (m :: <splicing-match>)
  let index = match-env-index(match-nested-pattern(m));
  let prefix = match-prefix(m);
  let suffix = match-suffix(m);
  matcher-method
    let (failure, _f*_, name) 
       = match-spliced-name(_f*_, prefix, suffix);
    lookup-match(env, index) := name;
    values(failure, _f*_);
  end;
end method;

define method generate-function (m :: <property-list-match>)
  let rest = match-rest-pattern(m);
  let (rest-index, rest-constraint)
    = if (rest)
        values(match-env-index(rest), match-constraint(rest))
      else
        values(#f, #f);
      end;
  let keys = match-key-patterns(m);
  collecting (key-indexes, key-specs)
    for (key in keys)
      let key-index = match-env-index(key);
      collect-into(key-indexes, key-index);
      let key-symbol = match-symbol-name(key);
      let key-constraint = match-constraint-spec(key);
      let key-default 
        = generate-key-default-function(match-default-expression(key));
      collect-into(key-specs, key-symbol);
      collect-into(key-specs, key-constraint);
      collect-into(key-specs, key-default);
    end;
    let key-indexes = collected(key-indexes);
    let key-specs = collected(key-specs);
    // break("Property list gen");
    matcher-method
      let (failure, _f*_, _rest_, #rest _keys_)
         = apply(match-property-list,
                 _f*_, rest-constraint, key-specs);
      // break("Property list match");
      failure 
        | begin
            if (rest-index)
              lookup-match(env, rest-index) := _rest_;
            end;
            for (index in key-indexes, value in _keys_)
              lookup-match(env, index) := value;
            end;
            values(#f, #());
          end;
    end;
  end;
end method;

define method generate-key-default-function (expression == #f)
  #f
end method;

define method generate-key-default-function (expression)
  /*
  let generator = generate-template-elements-function(list(expression));
  // A default can't contain substitutions, so no environment is needed.
  method () generator(#[]) end;
  */
  // We have to make a copy of the default so that the definitions database
  // doesn't contain cross-db references from the expansion to the
  // definition.
  method () default-in-expansion(expression) end;
end method;

// EXTENSION:

define method generate-constraint-function (constraint == #"symbol")
  match-symbol-constraint
end method;

define method generate-empty-function ()
  let success = generate-success-function();
  matcher-method
    if (empty?(_f*_))
      values(#f, #())
    else
      values(#t, #())
    end
  end;
end method;

define method generate-success-function ()
  matcher-method values(#f, #()) end;
end method;

//// Stubs

define function lookup-intermediate-words (env)
  *intermediate-words*
end function;

define function lookup-match (env, index) 
  element(env, index);
end function;

define function lookup-match-setter (value, env, index) 
  element(env, index) := value;
end function;

define function lookup-aux-rule-function (env, index) 
  element(env, index);
end function;

define function lookup-aux-rule-function-setter (value, env, index) 
  element(env, index) := value;
end function;

define function call-list-with (l, arg)
  for (f in l) f(arg) end;
end function;

define function call-list-with-collecting (funlist, arg)
  collecting ()
    for (f in funlist) collect(f(arg)) end;
  end;
end function;
