Module:    infix-reader
Synopsis:  Define-rule rules and patterns
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Define rules.

define class <define-rule> (<main-rule>)
end class;

define method define-rule (#key name, pattern, template)
  make(<define-rule>, name: name, pattern: pattern, template: template);
end method;

define method register-macro-word 
    (m :: <template-macro>, rule :: <define-rule>)
  register-infix-macro-word(rule.name, #"<fragment-define-word>");
  concatenate-symbols(rule.name, #"-definer");
end method;

define method process-main-rule
    (rule :: <define-rule>, rule-name, macro-name, main-rules)
  let rule-pattern = rule.pattern;
  // If the modifiers pattern part hasn't already been determined,
  // work out where it ends by searching for the macro word.
  if (~rule-pattern.modifiers-pattern)
    let main-pattern = rule-pattern.pattern;
    let (before, after) // after is inclusive of the match
      = split(main-pattern.fragments, rcurry(name=, rule-name));
    if (after)
      rule.name
        := rule-name;
      rule-pattern.modifiers-pattern 
        := sequence-fragment(fragments: before);
      rule-pattern.pattern 
        := sequence-fragment(fragments: after.tail);
    else
      error("Some main rule patterns of ~s do not contain a word consistent "
            "with its binding (e.g. ~s)",
            macro-name, main-pattern);
    end if;
  end if;
end method;

define method name= (p, symbol) 
  #f
end method;

define method name= (p :: <literal-fragment>, symbol)
  p.object == symbol
end method;

define method compile-main-rule (rule :: <define-rule>, m)
  let main-p = rule.pattern;
  let body-p = main-p.pattern;
  let body-fragments = body-p.fragments;
  compile-pattern-fragments(body-fragments, #f, m);
end method;

// Define patterns.

define pattern <define-rule-pattern> (<main-rule-pattern>)
  pattern slots modifiers-pattern, pattern;
end pattern;

define method match-main 
    (p :: <define-rule-pattern>, f :: <fragment>, e, fail)
  match-main(p, modified-fragment
                  (modifiers: sequence-fragment(fragments: #()), 
                   fragment: f),
             e, fail)
end method;

define method match-main 
    (p :: <define-rule-pattern>, f :: <modified-fragment>, e, fail)
  if (p.modifiers-pattern)
    if (~instance?(p.modifiers-pattern, <sequence-fragment>))
      p.modifiers-pattern 
        := sequence-fragment(fragments: list(p.modifiers-pattern));
    end;
    match-body
      (p.modifiers-pattern.fragments, f.modifiers.fragments, e, 
       method (p, f, #key cause)
         fail(p, f, cause: "modifiers don't match")
       end);
  else
    if (~empty?(f.modifiers.fragments))
      fail(p, f, cause: "unexpected modifiers supplied");
    end
  end;
  match-body(p.pattern.fragments, f.fragment.fragments, e, fail);
end method;

//// Define bindings rules.

define class <define-bindings-rule> (<define-rule>)
end class;

define method define-bindings-rule (#key name, pattern, template)
  make(<define-bindings-rule>, 
       name: name, pattern: pattern, template: template);
end method;

define method register-macro-word 
    (m :: <template-macro>, rule :: <define-bindings-rule>)
  register-infix-macro-word(rule.name, #"<fragment-define-bindings-word>");
  concatenate-symbols(rule.name, #"-definer");
end method;

// Compilation and matching/rule-class as for <define-rule>.

define method process-macro-name (rule :: <define-rule>, macro-name)
  let prefix = definer-name?(macro-name);
  unless (prefix)
    error("The main rule patterns of ~s indicate a defining macro, "
          "but its binding does not end in ~a",
          macro-name, $definer-suffix-symbol);
  end;
  prefix
end method;

define constant $definer-suffix-symbol 
  = #"-definer";
define constant $definer-suffix-size 
  = size(as(<string>, $definer-suffix-symbol));

define method definer-name? (name :: <symbol>)
  let name-string = as(<string>, name);
  // Strict comparison because #"-definer" isn't a valid definer name
  if (name-string.size > $definer-suffix-size)
    let size-diff = name-string.size - $definer-suffix-size;
    let name-tail = as(<symbol>, copy-sequence(name-string, start: size-diff));
    if (name-tail == $definer-suffix-symbol)
      as(<symbol>, copy-sequence(name-string, end: size-diff));
    else
      #f
    end
  else
    #f
  end
end method;

// eof
