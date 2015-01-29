Module:    dfmc-macro-expander
Synopsis:  Objects describing complete expanders.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <fragment-expander> (<object>) end;

define class <rewrite-rule-expander> (<fragment-expander>)
  constant slot expander-name = #f,
    init-keyword: name:;
  constant slot expander-module = #f,
    init-keyword: module:;
  constant slot expander-adjectives = #(),
    init-keyword: adjectives:;
  constant slot expander-main-rule-set,
    required-init-keyword: main-rule-set:;
  constant slot expander-aux-rule-sets,
    required-init-keyword: aux-rule-sets:;
  constant slot expander-aux-rule-set-table = make(<table>);
  slot expander-aux-rule-set-env :: <simple-object-vector>;
  slot expander-intermediate-words = #();
  slot expander-referenced-names = #();
end class;

define method initialize (exp :: <rewrite-rule-expander>, #key)
  next-method();
  let table = expander-aux-rule-set-table(exp);
  for (set in expander-aux-rule-sets(exp), env-index from 0)
    rule-set-rewriter-env-index(set) := env-index;
    table[rule-set-name(set)] := set;
  finally
    expander-aux-rule-set-env(exp)
      := make(<simple-object-vector>, size: env-index);
  end;
  check-pattern-variables(exp);
  compute-and-install-intermediate-words(exp);
end method;

define program-warning <unconstrained-pattern-variable>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  format-string
    "The pattern variable %s is unconstrained and not rewritten by an "
    "auxiliary rule - using %s:*.";
  format-arguments
    pattern-variable-name, pattern-variable-name again;
end program-warning;

define method check-pattern-variables (exp :: <rewrite-rule-expander>)
  local method check-match (m)
    let constraint = match-constraint(m);
    if (~constraint)
      let name = match-symbol-name(m);
      let aux
        = element(expander-aux-rule-set-table(exp), name, default: #f);
      match-constraint(m) := #"*";
      if (~aux)
        note(<unconstrained-pattern-variable>,
             source-location:       match-source-location(m),
             pattern-variable-name: name);
      end;
    end;
  end method;
  do-binding-matches(check-match, exp);
end method;

define method compute-and-install-intermediate-words
    (exp :: <rewrite-rule-expander>) => ()
  let seen = make(<table>);
  local method process-body-match-words (f, f-tail)
    if (~empty?(f-tail))
      process-body-match-terminator(exp, f-tail.head, seen);
    end;
  end method;
  do-body-match-tails(process-body-match-words, exp);
  // format-out("Intermediate words: %=\n", expander-intermediate-words(exp));
end method;

define method process-body-match-terminator
    (exp :: <rewrite-rule-expander>, f, seen) => ()
end method;

define method process-body-match-terminator
    (exp :: <rewrite-rule-expander>, f :: <name-fragment>, seen) => ()
  expander-intermediate-words(exp)
    := pair(fragment-name(f), expander-intermediate-words(exp));
end method;

define method process-body-match-terminator
    (exp :: <rewrite-rule-expander>, f :: <simple-match>, seen) => ()
  let key = match-symbol-name(f);
  if (~element(seen, key, default: #f))
    element(seen, key) := #t;
    let aux-rewrite
      = element(expander-aux-rule-set-table(exp), key, default: #f);
    if (aux-rewrite)
      for (rule in rule-set-rewrite-rules(aux-rewrite))
        let pattern = rule-pattern(rule);
        if (~empty?(pattern))
          process-body-match-terminator(exp, pattern.head, seen);
        end;
      end;
    end;
  end;
end method;

define method do-binding-matches
    (f :: <function>, exp :: <rewrite-rule-expander>) => ()
  do-binding-matches(f, expander-main-rule-set(exp));
  for (set in expander-aux-rule-sets(exp))
    do-binding-matches(f, set);
  end;
end method;

define method do-body-match-tails
    (f :: <function>, exp :: <rewrite-rule-expander>) => ()
  do-body-match-tails(f, expander-main-rule-set(exp));
  for (set in expander-aux-rule-sets(exp))
    do-body-match-tails(f, set);
  end;
end method;

define method compile-rewrite-rule-templates!
    (exp :: <rewrite-rule-expander>)
  compile-rule-set-templates!(exp, expander-main-rule-set(exp));
  for (aux-set in expander-aux-rule-sets(exp))
    compile-rule-set-templates!(exp, aux-set);
  end;
end method;

define serious-program-warning <unbound-template-pattern-variable>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  format-string
    "The pattern variable %s is referenced in the template but not bound "
    "on the left hand side of the rule.";
  format-arguments
    pattern-variable-name;
end serious-program-warning;

define serious-program-warning <mismatched-template-pattern-variable>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  format-string
    "The pattern variable %s referenced in the template is "
    "bound in an incompatible way on the left hand side of the rule.";
  format-arguments
    pattern-variable-name;
end serious-program-warning;

define method compile-rule-set-templates!
    (exp :: <rewrite-rule-expander>, set :: <rewrite-rule-set>)
  for (rule in rule-set-rewrite-rules(set))
    compile-rule-template!(exp, rule, rule-template(rule));
    rule-compiled-template(rule)
      := compile-macro-template(rule-template(rule));
  end;
end method;

define method compile-rule-template!
    (exp :: <rewrite-rule-expander>, rule :: <rewrite-rule>, template)
 => ()
  local method record-referenced-name (name)
    expander-referenced-names(exp)
      := pair(name, expander-referenced-names(exp));
  end method;
  compute-template-references
    (template, record-referenced-name);
  do-template-substitutions
    (method (subst :: <substitution>)
       if (instance?(subst, <variable-substitution>))
         let name = element-variable-name(subst);
         // let index = lookup-variable-name-env-index(rule, name);
         let (index, match?)
           = lookup-substitution-variable-index(rule, subst);
         if (index)
           if (match?)
             element-env-index(subst) := index;
           else
             note(<mismatched-template-pattern-variable>,
                  // source-location: fragment-source-location(name),
                  source-location: element-source-location(subst),
                  pattern-variable-name: name);
           end;
         else
           note(<unbound-template-pattern-variable>,
                // source-location: fragment-source-location(name),
                source-location: element-source-location(subst),
                pattern-variable-name: name);
         end;
       elseif (instance?(subst, <aux-rule-call-substitution>))
         let aux-set
           = element(expander-aux-rule-set-table(exp),
                     fragment-name(element-rule-name(subst)),
                     default: #f);
         let aux-rule-index = rule-set-rewriter-env-index(aux-set);
         element-aux-rule-env(subst) := expander-aux-rule-set-env(exp);
         element-aux-rule-index(subst) := aux-rule-index;
         compile-rule-template!(exp, rule, element-template(subst));
         element-compiled-template(subst)
           := compile-macro-template(element-template(subst));
       else
         compile-rule-template!(exp, rule, element-template(subst));
         element-compiled-template(subst)
           := compile-macro-template(element-template(subst));
       end;
     end,
     template);
end method;

define function compile-macro-template (template) => (template)
  // break("Compiling template");
  generate-template-function(template)
end function;

define thread variable *expansion-stream* = #f;

define function traced-expander?
    (exp :: <rewrite-rule-expander>) => (well? :: <boolean>)
  member?(#"traced", expander-adjectives(exp))
end function;

define function generate-expander-function
    (exp :: <rewrite-rule-expander>) => (expander-function :: <function>)
  compile-rewrite-rule-templates!(exp);
  let traced? = traced-expander?(exp);
  let name = expander-name(exp);
  let f = if (dude-expander?(exp))
            generate-dude-expander-function(exp);
          else
            generate-rewrite-rule-expander-function(exp);
          end;
  let module = expander-module(exp);
  method (env, form)
    with-expansion-module (module)
      with-new-hygiene-context (#"unknown")
        if (traced?)
          format-out("%s > %s\n", name, form);
        end;
        let expansion = f(call-as-fragment-tokens(form));
        if (traced?)
          format-out("%s < %s\n", name, expansion);
        end;
        if (*expansion-stream*)
          format(*expansion-stream*, "\n%s", expansion);
        end;
        expansion
      end
    end
  end;
end function;

define macro with-macroexpansion-output
  { with-macroexpansion-output (?args:*) ?:body end }
    => { do-with-macroexpansion-output(method () ?body end, ?args) }
end macro;

define open generic do-with-macroexpansion-output (thunk :: <method>, #key);

define method do-with-macroexpansion-output
    (thunk :: <method>, #key expansion-stream)
  dynamic-bind (*expansion-stream* = expansion-stream)
    thunk()
  end;
end method;

define method compiling-for-macroexpansion? () => (well? :: <boolean>)
  (*expansion-stream*) & #t
end method;
