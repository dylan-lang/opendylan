Module:   dfmc-definitions
Synopsis: The macro definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Macro definitions.

// Macro definition objects.

define dood-class <macro-definition> (<expander-defining-form>)
  lazy constant slot form-macro-rules, required-init-keyword: macro-rules:;
end;

define method form-define-word
    (form :: <macro-definition>) => (word :: <symbol>)
  #"macro"
end method;

// Browser support

define method macro-definition-word (form :: <expander-defining-form>)
  let word = form.form-macro-word;
  let word-class = form.form-macro-word-class;
  let macro-kind =
    select (word-class)
      $function-word-only-token          => #"function";
      $local-declaration-word-only-token => #"declaration";
      $begin-word-only-token             => #"statement";
      $define-body-word-only-token       => #"define";
      $define-list-word-only-token       => #"define";
      otherwise                          => #"special";
    end select;
  values(word, macro-kind);
end method;

//// The real macro converter.

// Install for callback by the constraint matching code
define sideways method expand-for-macro-constraint
    (call :: <macro-call-fragment>) => (expansion)
  expand-for-macro-constraint-using-definition
    (macro-definition(fragment-macro(call)), call);
end method;

define method expand-for-macro-constraint-using-definition
    (definition :: <expander-defining-form>, call :: <macro-call-fragment>)
 => (expansion)
  let expander = form-expander(definition);
  as-fragment-tokens(expander(#f, call));
end method;

define &definition macro-definer
  { define ?mods:* \macro ?:name ?rules:* end }
    => with-native-template-evaluation
         do-define-macro(form, mods, name, rules);
       end;
end &definition;

define function do-compile-macro 
    (name, adjectives, rules, #key definition-context = fragment-module(name))
  let (main-rule-set, aux-rule-sets) = parse-macro-rules(name, rules);
  let compiled-main = compile-rule-set-spec(main-rule-set);
  let compiled-aux = map(compile-rule-set-spec, aux-rule-sets);
  let compiled-exp 
    = make(<rewrite-rule-expander>,
           name: name,
           module: definition-context,
           adjectives: adjectives,
           main-rule-set: compiled-main,
           aux-rule-sets: compiled-aux);
  let expander 
    = block ()
        generate-expander-function(compiled-exp);
     /*
      exception (e :: <error>)
        signal("Macro function generation failed for %=.", name);
        method (env, form)
          signal("Stub expansion for %=.", name);
          #{ make(<deque>) }
        end;
     */
     end;
  let compiled-macro
    = make-macro-descriptor-matching
        (main-rule-set.spec-rule-specs.first.spec-pattern-spec,
         referenced-names:
           expander-referenced-names(compiled-exp),
         expander-function: 
           method (#rest r)
             // signal("Expanding %=!", name);
             apply(expander, r);
           end);
  local method rule-macro-expander (env, fragment)
	  expand-macro-call(compiled-macro, fragment)
	end;
  values(compiled-macro, rule-macro-expander)
end;
  
define method do-define-macro (fragment :: <fragment>, mods, name, rules)
  // Need to at least the pre-processing/analysis here, right now to
  // determine the word involved.
  let (initargs, adjectives) = parse-macro-adjectives(name, mods);
  let (compiled-macro, expander) 
    = do-compile-macro(name, adjectives, rules);
  let definition =
    apply(make, <macro-definition>, 
	  source-location: fragment-source-location(fragment),
	  variable-name:   name,
	  adjectives:      adjectives,
	  macro-rules:     rules,
	  macro-object:    compiled-macro,
	  expander:        expander,
          initargs);
  install-top-level-form(definition);
  list(definition)
end method;

define method dood-reinitialize 
    (dood :: <dood>, form :: <macro-definition>) => ()
  with-dood-context (dood-root(dood))
    with-dependent-context ($top-level-processing of form)
      // format-out("REINITIALIZING %s\n", form);
      let (compiled-macro, expander) 
	= do-compile-macro
	    (form.form-variable-name, form.form-adjectives, 
	       form.form-macro-rules);
      form.form-macro-object := compiled-macro;
      form.form-expander := expander;
    end;
  end;
end method;

define abstract class <pattern-spec> (<object>)
  constant slot spec-elements,
    required-init-keyword: elements:;
end class;

define class <aux-pattern-spec> (<pattern-spec>) end;

define abstract class <main-pattern-spec> (<pattern-spec>) end;

define class <define-body-pattern-spec> (<main-pattern-spec>) end;

define method make-macro-descriptor-matching 
    (spec :: <define-body-pattern-spec>, #rest initargs)
  apply(make, <suffixed-macro-descriptor>, 
        word-class: $define-body-word-only-token,
        suffix:     "-definer",
        initargs)
end method;

define class <define-list-pattern-spec> (<main-pattern-spec>) end;

define method make-macro-descriptor-matching 
    (spec :: <define-list-pattern-spec>, #rest initargs)
  apply(make, <suffixed-macro-descriptor>, 
        word-class: $define-list-word-only-token,
        suffix:     "-definer",
        initargs)
end method;

define class <statement-pattern-spec> (<main-pattern-spec>)
  constant slot spec-word, 
    required-init-keyword: word:;
end class;

ignore(spec-word);

define method make-macro-descriptor-matching 
    (spec :: <statement-pattern-spec>, #rest initargs)
  apply(make, <simple-macro-descriptor>, 
        word-class: $begin-word-only-token,
        initargs)
end method;

define class <function-pattern-spec> (<main-pattern-spec>)
  constant slot spec-word, 
    required-init-keyword: word:;
end class;

define method make-macro-descriptor-matching 
    (spec :: <function-pattern-spec>, #rest initargs)
  apply(make, <simple-macro-descriptor>, 
        word-class: $function-word-only-token,
        initargs)
end method;

define class <local-declaration-pattern-spec> (<main-pattern-spec>)
  constant slot spec-word, 
    required-init-keyword: word:;
end class;

define method make-macro-descriptor-matching 
    (spec :: <local-declaration-pattern-spec>, #rest initargs)
  apply(make, <simple-macro-descriptor>, 
        word-class: $local-declaration-word-only-token,
        initargs)
end method;

define class <reference-pattern-spec> (<main-pattern-spec>)
  constant slot spec-word, 
    required-init-keyword: word:;
end class;

define method make-macro-descriptor-matching 
    (spec :: <reference-pattern-spec>, #rest initargs)
  apply(make, <simple-macro-descriptor>, 
        word-class: $unreserved-name-token,
        initargs)
end method;

define abstract class <template-spec> (<object>) end;

define class <pattern-template-spec> (<template-spec>)
  constant slot spec-elements,
    required-init-keyword: elements:;
end class;

define class <procedural-template-spec> (<template-spec>)
  constant slot spec-expression,
    required-init-keyword: expression:;
end class;

define class <rule-spec> (<object>)
  constant slot spec-pattern-spec,
    required-init-keyword: pattern-spec:;
  constant slot spec-template-spec,
    required-init-keyword: template-spec:;
end class;

define abstract class <rule-set-spec> (<object>)
  constant slot spec-rule-specs,
    required-init-keyword: rule-specs:;
end class;

define class <main-rule-set-spec> (<rule-set-spec>) end;

define class <aux-rule-set-spec> (<rule-set-spec>) 
  constant slot spec-name,
    required-init-keyword: name:;
end class;

define method parse-macro-rules 
    (name, f) => (main-rule-set, aux-rule-sets)
  let (main-rule-set, aux-rules-f) = parse-macro-main-rule-set(name, f);
  collecting (aux-rule-sets)
    iterate walk (input-f = aux-rules-f)
      macro-case (input-f)
        { } => #t;
        { ?stuff:* }
          => begin
               let (next-set, remaining-f) 
                 = parse-macro-aux-rule-set(name, stuff);
               collect-into(aux-rule-sets, next-set);
               walk(remaining-f);
             end;
      end;
    end;
    let aux-rule-sets = collected(aux-rule-sets);
    values(main-rule-set, aux-rule-sets);
  end;
end method;

define serious-program-warning <missing-main-rules> (<manual-parser-error>)
  slot condition-macro-name,
    required-init-keyword: macro-name:;
  format-string
    "Invalid main rule set in the definition of the macro %s.";
  format-arguments
    macro-name;
end serious-program-warning;

define serious-program-warning <inconsistent-main-rules>
    (<manual-parser-error>)
  slot condition-macro-name,
    required-init-keyword: macro-name:;
  format-string
    "Inconsistent main rule shapes in the definition of the macro %s.";
  format-arguments
    macro-name;
end serious-program-warning;

define method main-rule-pattern-specs-consistent? 
    (specs :: <list>) => (well? :: <boolean>)
  let first-class = specs.first.spec-pattern-spec.object-class;
  every?(method (spec)
           instance?(spec-pattern-spec(spec), first-class)
         end, 
         specs.tail)
end method;

define method parse-macro-main-rule-set (name, rules-frag) => (set, remains)
  collecting (rule-specs)
    macro-case (rules-frag)
      { ?rules } 
        => begin
             let specs = collected(rule-specs);
             if (empty?(specs) 
                   | ~instance?
                        (specs.first.spec-pattern-spec, <main-pattern-spec>))
               note(<missing-main-rules>,
                    source-location: 
                      fragment-source-location(rules-frag)
                        | fragment-source-location(name),
                    macro-name: name);
               // Doesn't return.
             elseif (~main-rule-pattern-specs-consistent?(specs))
               note(<inconsistent-main-rules>,
                    source-location: 
                      fragment-source-location(rules-frag)
                        | fragment-source-location(name),
                    macro-name: name);
               // Doesn't return.
             else
               values(make(<main-rule-set-spec>, rule-specs: specs), rules);
             end;
           end;
    rules:
      { { ?lhs:* } => { ?rhs:* }; ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  lhs,
                     template-spec: make(<pattern-template-spec>,
                                         elements: rhs)));
             ...
           end;
      { { ?lhs:* } => { ?rhs:* } ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  lhs,
                     template-spec: make(<pattern-template-spec>, 
                                         elements: rhs)));
             ...
           end;
      { { ?lhs:* } =>  ?rhs:expression; ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  lhs,
                     template-spec: make(<procedural-template-spec>, 
                                         expression: rhs)));
             ...
           end;
      { { ?lhs:* } => ?rhs:expression ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  lhs,
                     template-spec: make(<procedural-template-spec>,
                                         expression: rhs)));
             ...
           end;

      { ?other:* }
        => other;
    lhs:
      { \define ?stuff:* \end }
        => make(<define-body-pattern-spec>, 
                elements: extract-define-word(name, stuff));
      { \define ?stuff:* }
        => make(<define-list-pattern-spec>, 
                elements: extract-define-word(name, stuff));
      { ?word:name ?stuff:* \end }
        => begin
             check-macro-word(name, word);
             make(<statement-pattern-spec>, word: word, elements: stuff);
           end;
      { ?word:name (?stuff:*) }
        => begin
             check-macro-word(name, word);
             make(<function-pattern-spec>, word: word, elements: stuff);
           end;
      { ?word:name }
        => begin
             check-macro-word(name, word);
             make(<reference-pattern-spec>, 
                  word: word, 
                  elements: make(<sequence-fragment>, fragments: #()));
           end;
      { ?word:name ?stuff:* }
        => begin
             check-macro-word(name, word);
             make(<local-declaration-pattern-spec>, 
                  word: word, elements: stuff);
           end;
      { ?stuff:* }
        => make(<aux-pattern-spec>, elements: stuff);
    end;
  end;
end method;

define serious-program-warning <inconsistent-macro-word> 
    (<manual-parser-error>)
  slot condition-macro-name,
    required-init-keyword: macro-name:;
  slot condition-macro-word,
    required-init-keyword: macro-word:;
  format-string
    "This main rule pattern of the macro %s starts with %s, which does not "
    "match the macro name.";
  format-arguments
    macro-name, macro-word;
end serious-program-warning;

define method check-macro-word 
    (name :: <variable-name-fragment>, word :: <variable-name-fragment>)
 => ()
  if (fragment-name(name) ~== fragment-name(word))
    note(<inconsistent-macro-word>,
         source-location: 
           fragment-source-location(word)
             | fragment-source-location(name),
         macro-name: name,
         macro-word: word);
  end;
end method;

define serious-program-warning <invalid-definer-macro-name> 
    (<manual-parser-error>)
  slot condition-macro-name,
    required-init-keyword: macro-name:;
  format-string
    "The macro %s has a main rule pattern like a defining macro, but its name "
    "does not end in \"-definer\".";
  format-arguments
    macro-name;
end serious-program-warning;

define serious-program-warning <inconsistent-define-word> 
    (<manual-parser-error>)
  slot condition-macro-name,
    required-init-keyword: macro-name:;
  format-string
    "This main rule pattern of the defining macro %s does not match the macro "
    "name.";
  format-arguments
    macro-name;
end serious-program-warning;

define method extract-define-word 
    (name :: <variable-name-fragment>, pattern) => (new-pattern)
  let define-word = suffixed-name?(fragment-name(name), "-definer");
  if (~define-word)
    note(<invalid-definer-macro-name>,
         source-location: 
           fragment-source-location(pattern)
             | fragment-source-location(name),
         macro-name:      name);
  end;
  let f* = fragment-fragments(pattern);
  let modifiers-pattern = #();
  block (return)
    for (f*-cursor = f* then f*-cursor.tail, until: empty?(f*-cursor))
      let f = f*-cursor.head;
      if (instance?(f, <variable-name-fragment>)
            & fragment-name(f) == define-word)
        // We've found the define word.
        return(make(<sequence-fragment>, 
                    fragments:
                      concatenate!
                        (reverse!(modifiers-pattern),
                                  pair(make(<end-of-modifiers-marker>), 
                                       f*-cursor.tail))));
      else
        modifiers-pattern := pair(f, modifiers-pattern);
      end;
    finally
      // Malformed pattern.
      note(<inconsistent-define-word>,
           source-location: 
              fragment-source-location(pattern)
                | fragment-source-location(name),
           macro-name:      name);
      // Doesn't return.
    end;
  end;
end method;

define method parse-macro-aux-rule-set (name, rules) => (set, remains)
  macro-case (rules)
    { ?set-name:symbol ?more:* }
      => begin
           let (specs, remains) = parse-macro-rule-set(name, more);
           values(make(<aux-rule-set-spec>, 
                       name:       set-name, 
                       rule-specs: specs),
                  remains);
         end;
  end;           
end method;

define serious-program-warning <malformed-aux-rules> (<manual-parser-error>)
  slot condition-rule-name,
    required-init-keyword: rule-name:;
  format-string
    "Invalid auxiliary rule set %s in macro definition.";
  format-arguments
    rule-name;
end serious-program-warning;

define method parse-macro-rule-set (name, rules-frag) => (set, remains)
  collecting (rule-specs)
    macro-case (rules-frag)
      { ?rules } 
        => begin
             let specs = collected(rule-specs);
             if (empty?(specs))
               note(<malformed-aux-rules>,
                    source-location: 
                      fragment-source-location(rules-frag)
                        | fragment-source-location(name),
                    rule-name:      name);
               // Doesn't return.
             else
               values(specs, rules);
             end;
           end;
    rules:
      { { ?lhs:* } => { ?rhs:* }; ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  make(<aux-pattern-spec>, elements: lhs),
                     template-spec: make(<pattern-template-spec>, elements: rhs)));
             ...
           end;
      { { ?lhs:* } => { ?rhs:* } ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  make(<aux-pattern-spec>, elements: lhs),
                     template-spec: make(<pattern-template-spec>, elements: rhs)));
             ...
           end;
      { { ?lhs:* } => ?rhs:expression; ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  make(<aux-pattern-spec>, elements: lhs),
                     template-spec: make(<procedural-template-spec>,
                                         expression: rhs)));
             ...
           end;
      { { ?lhs:* } => ?rhs:expression ... }
        => begin
             collect-first-into
               (rule-specs, 
                make(<rule-spec>,
                     pattern-spec:  make(<aux-pattern-spec>, elements: lhs),
                     template-spec: make(<procedural-template-spec>, 
                                         expression: rhs)));
             ...
           end;

      { ?other:* }
        => other;
    end;
  end;
end method;

//// Compilation of pattern specs to their internal representation.

define method compile-rule-set-spec (set :: <rule-set-spec>)
  make(<rewrite-rule-set>,
       rewrite-rules: map(compile-rule-spec, spec-rule-specs(set)));
end method;

// TODO: Remove this symbol/keyword gyration due to emulator humbug.

define method compile-rule-set-spec (set :: <aux-rule-set-spec>)
  let name = as(<symbol>, as(<string>, fragment-value(spec-name(set))));
  make(<aux-rewrite-rule-set>,
       rewrite-rules: map(compile-rule-spec, spec-rule-specs(set)),
       name:          name,
       variable-name: 
         pattern-variable-name(make-variable-name-fragment(name)),
       rewriter-variable-name: 
         pattern-variable-name
           (make-variable-name-fragment
              (as(<symbol>, concatenate(as(<string>, name), "-rewriter")))));
end method;

define method compile-rule-spec (rule :: <rule-spec>)
  let pattern = compile-pattern-spec(spec-pattern-spec(rule));
  let template = spec-template-spec(rule);
  if (instance?(template, <procedural-template-spec>))
    make(<rewrite-rule>, 
         pattern: pattern, template-code: spec-expression(template));
  else
    let template = compile-template-spec(spec-template-spec(rule));
    make(<rewrite-rule>, 
         pattern: pattern, template: template);
  end;
end method;

define method compile-pattern-spec (spec :: <pattern-spec>)
  compile-pattern-spec-elements(fragment-fragments(spec-elements(spec)));
end method;

define method compile-pattern-spec-elements (f* :: <list>)
  if (empty?(f*))
    #()
  else
    compile-pattern-spec-element(f*.head, f*.tail);
  end;
end method;

define method compile-pattern-spec-element (f :: <fragment>, f*)
  pair(compile-one-pattern-spec-element(f),
       compile-pattern-spec-elements(f*));
end method;

define method compile-pattern-spec-element 
    (f :: <spliced-pattern-variable-fragment>, f*)
  let sep = f*.head;
  if (instance?(sep, <colon-colon-fragment>))
    let type = f*.tail.head;
    if (instance?(type, <spliced-pattern-variable-fragment>))
      // A variable pattern.
      pair(make(<variable-match>,
                source-location: fragment-source-location(f),
                variable-name-pattern:   
                  compile-one-pattern-spec-element(f),
                type-expression-pattern: 
                  compile-one-pattern-spec-element(type)),
           compile-pattern-spec-elements(f*.tail.tail));
    else
      pair(compile-one-pattern-spec-element(f),
           compile-pattern-spec-elements(f*));
    end;
  else
    pair(compile-one-pattern-spec-element(f),
         compile-pattern-spec-elements(f*));
  end;
end method;

define method compile-pattern-spec-element 
    (f :: <hash-word-fragment>, f*)
  let (pattern-part, next-part) = split-at-kept-semicolon(f*);
  let next-part = next-part | #();
  macro-case (pair(f, pattern-part))
    { ?properties:* }
      => pair(properties, 
              compile-pattern-spec-elements(next-part));
  properties:
    { \#rest ?rest:*, ?hash-key-opt:* }
      => make(<property-list-match>,
              source-location: fragment-source-location(f),
              rest-pattern:
                compile-rest-pattern-spec-element(rest),
              key-patterns: hash-key-opt);
    { ?hash-key-opt:* }
      => make(<property-list-match>,
              key-patterns: hash-key-opt);
  hash-key-opt:
    { }
      => #();
    { \#key, \#all-keys }
      => #(); // TODO: CORRECTNESS: Tag all-keys.
    { \#key ?keys:* }
      => keys;
  keys:
    { }
      => #();
    { \#all-keys }
      => #(); // TODO: CORRECTNESS: Tag all-keys.
    { ?key:*, ... }
      => pair(key, ...);
  key:
    { ?var:* = ?default:expression }
      => compile-key-pattern-spec-element(var, default);
    { ?var:* }
      => compile-key-pattern-spec-element(var, #f);
  end;
end method;

define method compile-rest-pattern-spec-element 
    (f :: <pattern-variable-fragment>)
  compile-one-pattern-spec-element(f);
end method;

define method compile-rest-pattern-spec-element 
    (f :: <spliced-pattern-variable-fragment>)
  compile-one-pattern-spec-element(f);
end method;

define method compile-key-pattern-spec-element 
    (f :: <spliced-pattern-variable-fragment>, default)
  compile-key-pattern-name
    (<key-match>, fragment-pattern-variable(f), default);
end method;

define method compile-key-pattern-spec-element 
    (f :: <sequence-pattern-variable-fragment>, default)
  compile-key-pattern-name
    (<key-sequence-match>, fragment-name(f), default);
end method;

define method compile-key-pattern-name
    (class :: <class>, f :: <constrained-name-fragment>, default)
  make(class, 
       source-location: fragment-source-location(f),
       symbol-name:     fragment-name(f),
       variable-name:   pattern-variable-name(f),
       constraint:      fragment-constraint(f),
       default-expression: default);
end method;

define method compile-key-pattern-name
    (class :: <class>, f :: <variable-name-fragment>, default)
  make(class, 
       source-location: fragment-source-location(f),
       symbol-name:   as(<symbol>, fragment-name(f)),
       variable-name: pattern-variable-name(f),
       constraint:    #f,
       default-expression: default);
end method;

define method compile-one-pattern-spec-element (f :: <fragment>)
  f
end method;

define method compile-one-pattern-spec-element (f :: <parens-fragment>)
  make(<paren-match>, 
       source-location: fragment-source-location(f),
       nested-pattern: 
         compile-pattern-spec-elements(fragment-nested-fragments(f)));
end method;

define method compile-one-pattern-spec-element (f :: <brackets-fragment>)
  make(<bracket-match>,
       source-location: fragment-source-location(f),
       nested-pattern: 
         compile-pattern-spec-elements(fragment-nested-fragments(f)));
end method;

define method compile-one-pattern-spec-element (f :: <braces-fragment>)
  make(<brace-match>,
       source-location: fragment-source-location(f),
       nested-pattern: 
         compile-pattern-spec-elements(fragment-nested-fragments(f)));
end method;

define method compile-one-pattern-spec-element 
    (f :: <pattern-variable-fragment>)
  make(<simple-match>, 
       source-location: fragment-source-location(f),
       symbol-name:   as(<symbol>, fragment-name(f)),
       variable-name: fragment-name(f),
       constraint:    fragment-constraint(f));
end method;

define method compile-one-pattern-spec-element 
    (f :: <spliced-pattern-variable-fragment>)
  let prefix = fragment-prefix(f);
  let suffix = fragment-suffix(f);
  let var = fragment-pattern-variable(f);
  if (prefix | suffix)
    make(<splicing-match>, 
         source-location: fragment-source-location(f),
         nested-pattern: compile-name-pattern-spec(var),
         prefix: prefix,
         suffix: suffix);
  else
    compile-name-pattern-spec(var);
  end;
end method;

define method compile-one-pattern-spec-element
    (f :: <ellipsis-fragment>)
  make(<simple-match>, 
       source-location: fragment-source-location(f),
       symbol-name:   #"...",
       variable-name: 
         pattern-variable-name(make-variable-name-fragment(#"...")),
       constraint:    #"*");
end method;

define method compile-name-pattern-spec 
    (f :: <constrained-name-fragment>)
  make(<simple-match>, 
       source-location: fragment-source-location(f),
       symbol-name:     fragment-name(f),
       variable-name:   pattern-variable-name(f),
       constraint:      fragment-constraint(f));
end method;

define method compile-name-pattern-spec 
    (f :: <variable-name-fragment>)
  make(<simple-match>, 
       source-location: fragment-source-location(f),
       symbol-name:   as(<symbol>, fragment-name(f)),
       variable-name: pattern-variable-name(f),
       constraint:    #f);
end method;

define serious-program-warning <coercing-match-not-supported>
  slot condition-match-name,
    required-init-keyword: match-name:;
  format-string
    "Coercing matches are not supported - "
    "using the simple pattern variable name %s.";
  format-arguments match-name;
end serious-program-warning;

define method compile-name-pattern-spec
    (f :: <literal-fragment>)
  let match-name = as(<symbol>, fragment-value(f));
  note(<coercing-match-not-supported>,
       source-location:  fragment-source-location(f),
       match-name:       match-name);
  make(<simple-match>,
       source-location: fragment-source-location(f),
       symbol-name:     match-name,
       variable-name:   pattern-variable-name
                          (make-variable-name-fragment(match-name)),
       constraint:      #f);
end method;

// Common error cases.

define serious-program-warning <missing-query>
  slot condition-constrained-name,
    required-init-keyword: constrained-name:;
  slot condition-constraint,
    required-init-keyword: constraint:;
  format-string "Constrained name without a leading query - "
                "using ?%s:%s.";
  format-arguments constrained-name, constraint;
end serious-program-warning;

define method compile-one-pattern-spec-element 
    (f :: <constrained-name-fragment>)
  note(<missing-query>, 
       source-location:  fragment-source-location(f),
       constrained-name: fragment-name(f),
       constraint:       fragment-constraint(f));
  compile-key-pattern-name(<simple-match>, f, #f);
end method;

define serious-program-warning <query-equal-in-pattern>
  slot condition-name,
    required-init-keyword: name:;
  format-string "Unexpected ?=%s in macro pattern - "
                "using the unadorned name %s.";
  format-arguments name, name again;
end serious-program-warning;

define method compile-one-pattern-spec-element 
    (f :: <unhygienic-name-fragment>)
  note(<query-equal-in-pattern>,
       source-location:  fragment-source-location(f),
       name:             fragment-name(f));
  compile-one-pattern-spec-element(fragment-name(f));
end method;

define method compile-rest-pattern-spec-element 
    (f :: <constrained-name-fragment>)
  // Gives us default constrained name correction as above.
  next-method();
end method;

define method compile-rest-pattern-spec-element (f :: <fragment>)
  parser-error-handler(#f, f, #());
end method;

define method compile-key-pattern-spec-element (f :: <fragment>, default)
  parser-error-handler(#f, f, #());
end method;

/*
define method compile-one-pattern-spec-element 
    (f :: <pattern-variable-fragment>)
  make(<simple-match>, 
       symbol-name:   as(<symbol>, fragment-name(f)),
       variable-name: fragment-name(f),
       constraint:    fragment-constraint(f));
end method;
*/

//// Compilation of template specs to their internal representation.

define method compile-template-spec (spec :: <procedural-template-spec>)
end method;

define method compile-template-spec (spec :: <pattern-template-spec>)
  compile-template-spec-elements(fragment-fragments(spec-elements(spec)));
end method;

define method compile-template-spec-elements (f* :: <list>)
  if (empty?(f*))
    #()
  else
    compile-template-spec-element(f*.head, f*.tail);
  end;
end method;

define method compile-template-spec-element (f :: <fragment>, f*)
  pair(compile-one-template-spec-element(f),
       compile-template-spec-elements(f*));
end method;

define serious-program-warning <missing-ellipsis>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  format-string 
    "Sequence substitution ??%s not followed by a valid separator or "
    "ellipsis - using ?%s ...";
  format-arguments pattern-variable-name;
end serious-program-warning;

define method compile-template-spec-element 
    (f :: <sequence-pattern-variable-fragment>, f*)
  let separator = f*.head;
  if (instance?(separator, <separator-fragment>)
        & instance?(f*.tail.head, <ellipsis-fragment>))
    let var = pattern-variable-name(fragment-name(f));
    pair(make(<simple-sequence-substitution>, 
              source-location: fragment-source-location(f),
              variable-name: var,
              separator:     separator),
         compile-template-spec-elements(f*.tail.tail));
  else
    let var = pattern-variable-name(fragment-name(f));
    let f*-remains 
      = if (~instance?(separator, <ellipsis-fragment>))
          note(<missing-ellipsis>,
               source-location: fragment-source-location(f),
               pattern-variable-name: var);
          f*
        else
          f*.tail
        end;
    pair(make(<simple-sequence-substitution>, 
              source-location: fragment-source-location(f),
              variable-name: var,
              separator:     #f),
         compile-template-spec-elements(f*-remains));
  end;
end method;

define method compile-one-template-spec-element (f :: <fragment>)
  f
end method;

define method compile-one-template-spec-element 
    (f :: <spliced-pattern-variable-fragment>)
  let prefix = fragment-prefix(f);
  let suffix = fragment-suffix(f);
  let var = fragment-pattern-variable(f);
  if (prefix | suffix)
    make(<splicing-substitution>, 
         source-location: fragment-source-location(f),
         variable-name: #f,
         name-substitution: compile-name-template-spec(var),
         prefix: prefix | "",
         suffix: suffix | "");
  else
    compile-name-template-spec(var);
  end;
end method;

define method compile-one-template-spec-element
    (f :: <ellipsis-fragment>)
  make(<simple-element-substitution>, 
       source-location: fragment-source-location(f),
       variable-name: 
         pattern-variable-name(make-variable-name-fragment(#"...")));
end method;

define method compile-one-template-spec-element
    (f :: <template-macro-call-fragment>)
  make(<macro-call-substitution>, 
       source-location: fragment-source-location(f),
       template: 
         compile-template-spec-elements
           (fragment-nested-fragments(fragment-template(f))));
end method;

define method compile-one-template-spec-element
    (f :: <template-aux-rule-call-fragment>)
  make(<aux-rule-call-substitution>, 
       source-location: fragment-source-location(f),
       rule-name: fragment-rule-name(f),
       template: 
         compile-template-spec-elements
           (fragment-nested-fragments(fragment-template(f))));
end method;

define method compile-name-template-spec 
    (f :: <variable-name-fragment>)
  make(<simple-element-substitution>, 
       source-location: fragment-source-location(f),
       variable-name: pattern-variable-name(f));
end method;

define method compile-name-template-spec 
    (f :: <symbol-fragment>)
  make(<as-symbol-substitution>, 
       source-location: fragment-source-location(f),
       variable-name: 
         pattern-variable-name
           (make-variable-name-fragment(fragment-value(f))));
end method;

define method compile-name-template-spec 
    (f :: <string-fragment>)
  make(<as-string-substitution>, 
       source-location: fragment-source-location(f),
       variable-name: 
         pattern-variable-name
           (make-variable-name-fragment(as(<symbol>, fragment-value(f)))));
end method;

define program-warning <constrained-substitution>
  slot condition-pattern-variable-name,
    required-init-keyword: pattern-variable-name:;
  format-string
    "The template substitution %s has a constraint - ignoring.";
  format-arguments
    pattern-variable-name;
end program-warning;

define method compile-name-template-spec 
    (f :: <constrained-name-fragment>)
  note(<constrained-substitution>,
       source-location: fragment-source-location(f),
       pattern-variable-name: fragment-name(f));
  make(<simple-element-substitution>, 
       source-location: fragment-source-location(f),
       variable-name: pattern-variable-name(f));
end method;

define method compile-one-template-spec-element (f :: <nested-fragment>)
  make(f.object-class, 
       left-delimiter: fragment-left-delimiter(f),
       nested-fragments: 
         compile-template-spec-elements(fragment-nested-fragments(f)),
       right-delimiter: fragment-right-delimiter(f));
end method;

//// Adjectives.

// For testing...
define property <macro-dude-property> => dude?: = #f
  value dude = #t;
end property;

define property <macro-traced-property> => traced?: = #f
  value traced = #t;
end property;

define constant macro-adjectives
  = list(<macro-dude-property>, <macro-traced-property>);

define method parse-macro-adjectives 
    (name, adjectives-form) => (initargs, adjectives)
  parse-property-adjectives(macro-adjectives, adjectives-form, name)
end method;

//// Pseudo-macro "macro"

define &macro \macro
  { \macro end } => #{ }
end &macro;

//// Utility.

define function macro-fragment? (fragment) => (well? :: <boolean>)
  instance?(fragment, <macro-call-fragment>) 
    & (lookup-binding(fragment-macro(fragment)) == dylan-binding(#"macro"))
end function;

// eof
