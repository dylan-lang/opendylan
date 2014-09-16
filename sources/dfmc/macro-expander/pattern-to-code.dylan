Module:    dfmc-macro-expander
Synopsis:  Generate and compose code to perform and bind the results of
           a pattern match.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method compile-macro-case-rules (input, exp)
  let code = compile-rewrite-rule-expander(exp);
  #{ let _f*_ :: <fragment-list> = as-fragment-tokens(?input);
     ?code }
end method;

define method compile-define-macro-rules (input, exp)
  let code = compile-rewrite-rule-expander(exp);
  #{ let _f*_ :: <fragment-list> = call-as-fragment-tokens(?input);
     ?code }
end method;

define method compile-rewrite-rule-expander (exp)
  let aux-methods-code 
    = map(curry(compile-aux-rule-set-definition, exp), 
          expander-aux-rule-sets(exp));
  let main-code
    = compile-main-rule-set(exp, expander-main-rule-set(exp));
  let intermediate-words
    = expander-intermediate-words(exp);
  if (empty?(aux-methods-code))
    #{ let _intermediate-words_ = ?intermediate-words; 
       ?main-code }
  else
    #{ let _intermediate-words_ = ?intermediate-words; 
       local ??aux-methods-code, ...; 
       ?main-code }
  end;
end method;

define method compile-main-rule-set (exp, set)
  compile-rules(exp, #f, rule-set-rewrite-rules(set));
end method;

define method compile-aux-rule-set-definition (exp, set)
  let rules-code = compile-rules(exp, set, rule-set-rewrite-rules(set));
  let name = rule-set-rewriter-variable-name(set);
  #{ method ?name (_f*_ :: <fragment-list>) ?rules-code end }
end method;

define method compile-rules (exp, set, rules)
  if (empty?(rules))
    generate-match-failure(exp, set, rules);
  else
    compile-rule(exp, set, rules.head, rules.tail);
  end;
end method;

define method generate-match-failure 
    (exp, set :: <aux-rewrite-rule-set>, rules)
  let exp-name = expander-name(exp);
  let name = if (exp-name) as(<symbol>, exp-name) else #"macro-case" end;
  let set-name = rule-set-name(set);
  #{ macro-aux-rule-match-error
       (_f*_, dylan-variable-name(?name), ?set-name); }
end method;

define method generate-match-failure 
    (exp, set , rules)
  let exp-name = expander-name(exp);
  let name = if (exp-name) as(<symbol>, exp-name) else #"macro-case" end;
  #{ macro-main-rule-match-error
       (_f*_, dylan-variable-name(?name)) }
end method;

define method compile-rule (exp, set, rule, more-rules)
  let pattern = rule-pattern(rule);
  let template-code = rule-compiled-template(rule);
  let binding-matches = compute-binding-matches(pattern);
  let bound-names = compute-bound-variable-names(pattern);
  let match-code = compile-body-pattern(bound-names, pattern);
  let more-match-code = compile-rules(exp, set, more-rules);
  let rewrite-code 
    = generate-bound-name-aux-rule-rewrites(exp, set, binding-matches);
  let matched-code 
    = if (empty?(rewrite-code))
        template-code
      else
        #{ ??rewrite-code; ...; ?template-code }
      end;
  #{ let (failure, ??bound-names, ...) = begin ?match-code end;
     if (~failure) 
       ?matched-code
     else 
       ?more-match-code
     end; };
end method;

define method generate-bound-name-aux-rule-rewrites 
    (exp, set, binding-matches)
  collecting ()
    for (match in binding-matches)
      let aux-set 
        = element(expander-aux-rule-set-table(exp), match-symbol-name(match),
                  default: #f);
      if (aux-set)
        let bound-name = match-variable-name(match);
        let aux-name = rule-set-rewriter-variable-name(aux-set);
        // TODO: PERFORMANCE: Optimize out this as-fragment-tokens when
        // it isn't necessary, i.e. when rewriting a constrained thing
        // we can infer stuff about it. E.g. a ?:name that's rewritten
        // in an aux rule.
        if (instance?(match, <key-sequence-match>))
          collect(#{ let ?bound-name :: <list>
                       = map(compose(?aux-name, as-fragment-tokens),
                             ?bound-name) });
        else
          collect(#{ let ?bound-name 
                       = ?aux-name(as-fragment-tokens(?bound-name)) });
        end;
        // If it's a self call, bind ellipsis for the RHS.
        if (aux-set == set)
          collect(#{ let \... = ?bound-name });
        end;
      else
        let bound-name = match-variable-name(match);
        if (instance?(match, <key-sequence-match>))
          collect(#{ let ?bound-name :: <list>
                       = map(export-fragment-tokens, ?bound-name) });
        else
          collect(#{ let ?bound-name = export-fragment-tokens(?bound-name) });
        end;
      end;
    end;
  end;
end method;

define method compile-body-pattern (e, m* :: <list>)
  let (parts, n-parts) = split-at-semicolons(m*);
  compile-body-parts(e, parts, n-parts);
end method;

define method compile-body-parts (e, parts, n-parts)
  if (n-parts = 1)
    compile-list-pattern(e, parts.first);
  else
    let matcher 
      = if (n-parts = 2) 
          #{ match-body-part }
        else
          #{ match-body-part-strict }
        end;
    let before-code = compile-nested-list-pattern(parts.first);
    let after-code = compile-body-parts(e, parts.tail, n-parts - 1);
    #{ let (failure, _f*-after_ , _f*-before_) = ?matcher(_f*_);
       failure
         | begin
             let _f*_ = _f*-before_;
             ?before-code;
             failure
               | begin 
                   let _f*_ = _f*-after_;
                   ?after-code
                 end
           end };
  end;
end method;

define method compile-nested-list-pattern (m*)
  let e = compute-bound-variable-names(m*);
  let names = env-bound-names(e);
  let code = compile-list-pattern(e, m*);
  #{ let (failure, ??names, ...) = begin ?code end }
end method;

define method compile-list-pattern (e, m* :: <list>)
  let (parts, n-parts) = split-at-commas(m*);
  compile-list-parts(e, parts, n-parts);
end method;

define method compile-list-parts (e, parts, n-parts)
  if (n-parts = 1)
    compile-pattern-elements(e, parts.first);
  else
    let matcher 
      = if (n-parts = 2) 
          #{ match-list-part }
        else
          #{ match-list-part-strict }
        end;
    let before-code = compile-nested-pattern-elements(parts.first);
    let after-code = compile-list-parts(e, parts.tail, n-parts - 1);
    #{ let (failure, _f*-after_, _f*-before_) = ?matcher(_f*_);
       failure
         | begin
             let _f*_ = _f*-before_;
             ?before-code;
             failure
               | begin 
                   let _f*_ = _f*-after_;
                   ?after-code
                 end
           end };
  end;
end method;

define method compile-pattern-elements (e, m* :: <list>)
  if (empty?(m*))
    generate-empty-code(e);
  else
    compile-pattern-element(e, m*.head, m*.tail);
  end;
end method;

define method compile-pattern-element 
    (e, m :: type-union(<fragment>, <pattern-match>), m*)
  let m-code = generate-code(m);
  let m*-code = compile-pattern-elements(e, m*);
  #{ ?m-code; failure | begin ?m*-code end }
end method;

define method compile-pattern-element 
    (e, m :: <name-fragment>, m*)
  if (fragment-name(m) ~== #"otherwise")
    next-method();
  else
    let m* 
      = if (instance?(m*.head, <equal-greater-fragment>))
          m*.tail
        else
          m*
        end;
    let m*-code = compile-pattern-elements(e, m*);
    #{ let (failure, _f*_) = match-otherwise(_f*_); 
       failure | begin ?m*-code end }
  end;
end method;

define method compile-pattern-element 
    (e, m :: <simple-match>, m*)
  let constraint = match-constraint(m);
  if (~wildcard-constraint?(constraint))
    next-method();
  elseif (empty?(m*))
    let success-code = generate-success-code(e);
    let name = match-variable-name(m);
    #{ let ?name = _f*_; ?success-code }
  else
    let m*-code = compile-pattern-elements(e, m*);
    let name = match-variable-name(m);
    let bound-names = env-bound-names(e);
    #{ local method _wildcard-loop_ 
           (_collector_ :: <list>, _f*_ :: <fragment-list>)
         let ?name = _collector_;
         // format-out("Try: %=\n", ?name);
         let (failure, ??bound-names, ...) = begin ?m*-code end;
         if (failure)
           if (empty?(_f*_))
             #t
           else
             // Skip ahead to the next plausible match.
             _wildcard-loop_
                (concatenate!(_collector_, list(_f*_.head)), _f*_.tail);
           end;
         else
           values(failure, ??bound-names, ...);
         end;
       end;
       _wildcard-loop_(#(), _f*_) }
  end;
end method;

define method compile-pattern-element (e, m :: <variable-match>, m*)
  let name-code = generate-code(match-variable-name-pattern(m));
  let type-code = generate-code(match-type-expression-pattern(m));
  let m*-code = compile-pattern-elements(e, m*);
  #{ let (failure, _after-f*_, _name-f*_, _type-f*_) = match-variable(_f*_);
     failure 
       | begin
           let _f*_ = _name-f*_;
           ?name-code;
           failure 
             | begin
                 let _f*_ = _type-f*_;
                 ?type-code;
                  failure 
                    | begin
                        let _f*_ = _after-f*_;
                        ?m*-code
                      end
               end
          end }
end method;

define method compile-pattern-element (e, m :: <nested-match>, m*)
  let nested-matcher = generate-nested-matcher-code(m);
  let nested-code 
    = compile-nested-pattern-elements(match-nested-pattern(m));
  let m*-code = compile-pattern-elements(e, m*);
  #{ let (failure, _after-f*_, _nested-f*_) = ?nested-matcher(_f*_);
     failure
       | begin
           let _f*_ = _nested-f*_;
           ?nested-code;
           failure
             | begin
                 let _f*_ = _after-f*_;
                 ?m*-code
               end
         end }
end method;

define method compile-nested-pattern-elements (m*)
  let e = compute-bound-variable-names(m*);
  let names = env-bound-names(e);
  let code = compile-body-pattern(e, m*);
  #{ let (failure, ??names, ...) = begin ?code end }
end method;

define method generate-nested-matcher-code (m :: <paren-match>)
  #{ match-parens }
end method;

define method generate-nested-matcher-code (m :: <bracket-match>)
  #{ match-brackets }
end method;

define method generate-nested-matcher-code (m :: <brace-match>)
  #{ match-braces }
end method;

define method generate-code (m :: <name-fragment>)
  let name = fragment-name(m);
  #{ let (failure, _f*_) = match-name(_f*_, ?name) }
end method;

define method generate-code (m :: <operator-fragment>)
  let name = fragment-name(m);
  #{ let (failure, _f*_) = match-operator(_f*_, ?name) }
end method;

define method generate-code (m :: <end-of-modifiers-marker>)
  #{ let (failure, _f*_) = match-end-of-modifiers(_f*_) }
end method;

define method generate-code (m :: <equal-greater-fragment>)
  #{ let (failure, _f*_) = match-equal-greater(_f*_) }
end method;

define method generate-code (m :: <hash-next-fragment>)
  #{ let (failure, _f*_) = match-hash-next(_f*_) }
end method;

define method generate-code (m :: <hash-rest-fragment>)
  #{ let (failure, _f*_) = match-hash-rest(_f*_) }
end method;

define method generate-code (m :: <hash-key-fragment>)
  #{ let (failure, _f*_) = match-hash-key(_f*_) }
end method;

define method generate-code (m :: <hash-all-keys-fragment>)
  #{ let (failure, _f*_) = match-hash-all-keys(_f*_) }
end method;

define method generate-code (m :: <dot-fragment>)
  #{ let (failure, _f*_) = match-dot(_f*_) }
end method;

define method generate-code (m :: <colon-colon-fragment>)
  #{ let (failure, _f*_) = match-colon-colon(_f*_) }
end method;

define method generate-code (m :: <literal-fragment>)
  let value = fragment-value(m);
  #{ let (failure, _f*_) = match-literal(_f*_, ?value) }
end method;

define method generate-code (m :: <escaped-substitution-fragment>)
  generate-code(fragment-escaped-fragment(m));
end method;

define method generate-code (m :: <simple-match>)
  let name = match-variable-name(m);
  let constraint = match-constraint(m);
  if (wildcard-constraint?(constraint))
    #{ let ?name = _f*_ }
  elseif (bounded-constraint?(constraint))
    let matcher = generate-constraint-function-code(match-constraint(m));
    #{ let (failure, _f*_, ?name) = ?matcher(_f*_, _intermediate-words_) };
  else
    let matcher = generate-constraint-function-code(match-constraint(m));
    #{ let (failure, _f*_, ?name) = ?matcher(_f*_) };
  end;
end method;

define method generate-constraint-function-code (constraint == #"token")
  #{ match-token-constraint }
end method;

define method generate-constraint-function-code (constraint == #"name")
  #{ match-name-constraint }
end method;

define method generate-constraint-function-code (constraint == #"expression")
  #{ match-expression-constraint }
end method;

define method generate-constraint-function-code (constraint == #"variable")
  #{ match-variable-constraint }
end method;

define method generate-constraint-function-code (constraint == #"body")
  #{ match-bounded-body-constraint }
end method;

define method generate-constraint-function-code (constraint == #"body!")
  #{ match-bounded-body-constraint-no-backtracking }
end method;

define method generate-constraint-function-code (constraint == #"case-body")
  #{ match-bounded-case-body-constraint }
end method;

define method generate-constraint-function-code (constraint == #"macro")
  #{ match-macro-constraint }
end method;

define method generate-code (m :: <splicing-match>)
  let name = match-variable-name(match-nested-pattern(m));
  let prefix = match-prefix(m);
  let suffix = match-suffix(m);
  #{ let (failure, _f*_, ?name) 
       = match-spliced-name(_f*_, ?prefix, ?suffix) }
end method;

define method generate-code (m :: <property-list-match>)
  let rest = match-rest-pattern(m);
  let (rest-name, rest-constraint)
    = if (rest)
        values(match-variable-name(rest), match-constraint(rest))
      else
        values(#{ _rest_ }, #f);
      end;
  let keys = match-key-patterns(m);
  collecting (key-names, key-specs)
    for (key in keys)
      let key-name = match-variable-name(key);
      collect-into(key-names, key-name);
      let key-symbol = match-symbol-name(key);
      let key-constraint = match-constraint-spec(key);
      let key-default 
        = generate-key-default-thunk(match-default-expression(key));
      collect-into
        (key-specs, #{ ?key-symbol, ?key-constraint, ?key-default });
    end;
    let key-names = collected(key-names);
    let key-specs = collected(key-specs);
    // break();
    #{ let (failure, _f*_, ?rest-name, ??key-names, ...)
         = match-property-list
             (_f*_, ?rest-constraint, ??key-specs, ...) }
  end;
end method;

// TODO: CORRECTNESS: Make sure this can handle the construction of all
// expressions.

define method generate-key-default-thunk (expression == #f)
  #f
end method;

define method generate-key-default-thunk (expression)
  let gen-code = compile-template-elements(list(expression));
  #{ method () ?gen-code end }
end method;

define method match-constraint-spec (key :: <key-match>)
  match-constraint(key)
end method;

define method match-constraint-spec (key :: <key-sequence-match>)
  list(match-constraint(key))
end method;

// EXTENSION:

define method generate-constraint-function-code (constraint == #"symbol")
  #{ match-symbol-constraint }
end method;

define method generate-empty-code (e)
  let success = generate-success-code(e);
  let bound-names = env-bound-names(e);
  #{ if (empty?(_f*_)) 
       ?success
     else
       #t
     end }
end method;

define method generate-success-code (e)
  let bound-names = env-bound-names(e);
  #{ values(#f, ??bound-names, ...) }
end method;

define method env-bound-names (e)
  e
end method;
