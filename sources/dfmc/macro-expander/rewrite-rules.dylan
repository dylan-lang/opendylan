Module:    dfmc-macro-expander
Synopsis:  Objects describing rewrite rules.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <rewrite-rule> (<object>)
  constant slot rule-pattern,
    required-init-keyword: pattern:;
  constant slot rule-template = #f,
    init-keyword: template:;
  slot rule-compiled-template = #f,
    init-keyword: template-code:;
end class;

define program-warning <name-used-already-in-pattern>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  format-string
    "The pattern variable name %s has already been used in this pattern - "
    "the fragment matching the first occurrence will be substituted into "
    "the template.";
  format-arguments
    pattern-variable-name;
end program-warning;

define serious-program-warning <invalid-constraint>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  slot condition-pattern-variable-constraint,
    required-init-keyword: pattern-variable-constraint:;
  format-string
    "The pattern variable name %s has an invalid constraint %s - "
    "using %s:*.";
  format-arguments
    pattern-variable-name, pattern-variable-constraint, 
    pattern-variable-name again;
end serious-program-warning;

define method initialize (rule :: <rewrite-rule>, #key)
  next-method();
  // Install the environment offsets of the pattern bindings, and check
  // for duplicated names.
  let count = -1;
  // TODO: Put back name table
  // let table = make(<variable-name-table>);
  let table = make(<object-table>);
  do-binding-matches
    (method (match) 
       // TODO: Put back name table
       // let name = match-variable-name(match);
       let name = match-symbol-name(match);
       let previous-match = element(table, name, default: #f);
       if (previous-match)
         let var = match-variable-name(match);
         note(<name-used-already-in-pattern>,
              source-location: fragment-source-location(var),
              pattern-variable-name: var);
       else
         element(table, name) := match;
       end;
       let constraint = match-constraint(match);
       if (~valid-match-constraint?(constraint))
         let var = match-variable-name(match);
         note(<invalid-constraint>,
              source-location: fragment-source-location(var),
              pattern-variable-name: var,
              pattern-variable-constraint: constraint);
         // Hack it to be a wildcard.
         match-constraint(match) := #"*";
       end;
       match-env-index(match) := count := count + 1 
     end,
     rule-pattern(rule));
end method;

/*
define method lookup-variable-name-env-index 
    (rule :: <rewrite-rule>, name) 
 => (index :: false-or(<integer>))
  block (return)
    do-binding-matches
      (method (match)
         // if (same-name-when-local?(name, match-variable-name(match)))
         if (fragment-name(name) == fragment-name(match-variable-name(match)))
           return(match-env-index(match));
         end
       end,
       rule-pattern(rule));
    // break("Missing template var");
    #f
  end;
end method;
*/

define method lookup-substitution-variable-index 
    (rule :: <rewrite-rule>, subst :: <substitution>)
 => (index :: false-or(<integer>), match? :: <boolean>)
  block (return)
    do-binding-matches
      (method (match :: <binding-match>)
         if (fragment-name(element-variable-name(subst))
               == fragment-name(match-variable-name(match)))
           return(match-env-index(match), 
                  matching-binding-and-substitution?(subst, match));
         end
       end,
       rule-pattern(rule));
    // break("Missing template var");
    values(#f, #f)
  end;
end method;

// This attempts to ensure that ? and ?? substitutions don't hook up with
// one another. The absence of duplicate binding names in a pattern 
// should have been verified earlier.

define method matching-binding-and-substitution?
    (subst :: <sequence-substitution>, match :: <binding-match>)
 => (well? :: <boolean>)
  instance?(match, <sequence-match>)
end method;

define method matching-binding-and-substitution?
    (subst :: <substitution>, match :: <binding-match>) => (well? :: <boolean>)
  ~instance?(match, <sequence-match>)
end method;

define class <rewrite-rule-set> (<object>)
  constant slot rule-set-rewrite-rules,
    required-init-keyword: rewrite-rules:;
end class;

define class <aux-rewrite-rule-set> (<rewrite-rule-set>)
  constant slot rule-set-name,
    required-init-keyword: name:;
  constant slot rule-set-variable-name,
    required-init-keyword: variable-name:;
  constant slot rule-set-rewriter-variable-name,
    required-init-keyword: rewriter-variable-name:;
  slot rule-set-rewriter-env-index :: <integer> = 0;
end class;

define method initialize (set :: <aux-rewrite-rule-set>, #key)
  next-method();
  // Deal with ellipsis.
  for (rule in rule-set-rewrite-rules(set))
    do-binding-matches
      (method (match :: <binding-match>) 
         if (match-symbol-name(match) == #"...")
           match-symbol-name(match) := rule-set-name(set);
           match-variable-name(match) := rule-set-variable-name(set);
         end;
       end,
       rule-pattern(rule));
    do-template-substitutions
      (method (subst :: <substitution>) 
         if (instance?(subst, <variable-substitution>)
               & fragment-name(element-variable-name(subst)) == #"...")
           element-variable-name(subst) := rule-set-variable-name(set);
         end;
       end,
       rule-template(rule));
  end;
end method;

define method do-binding-matches 
    (f :: <function>, set :: <rewrite-rule-set>) => ()
  for (rule in rule-set-rewrite-rules(set))
    do-binding-matches(f, rule);
  end;
end method;

define method do-body-match-tails 
    (f :: <function>, set :: <rewrite-rule-set>) => ()
  for (rule in rule-set-rewrite-rules(set))
    do-body-match-tails(f, rule);
  end;
end method;

define method do-binding-matches
    (f :: <function>, rule :: <rewrite-rule>) => ()
  do-binding-matches(f, rule-pattern(rule));
end method;

define method do-body-match-tails 
    (f :: <function>, rule :: <rewrite-rule>) => ()
  do-body-match-tails(f, rule-pattern(rule));
end method;

// eof
