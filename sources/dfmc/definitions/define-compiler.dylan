Module:   dfmc-definitions
Synopsis: Syntax definitions used in the compiler.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Definitions, converters, and macros.

define &macro &converter-definer
  { define ?mods:* \&converter ?:name ?rules:* end }
    => do-define-&converter(form, mods, name, rules);
end &macro;

define method do-define-&converter 
    (fragment :: <fragment>, mods, name, rules)
  let module = definitions-module();
  with-expansion-module (module)
    do-define-some-kind-of-an-expander
      (fragment, mods, name, 
       #{ env, context, form }, rules, 
       #{ ?name ## "-converter" }, 
       #{ do-define-core-converter });
  end;
end method;

define &macro &definition-definer
  { define ?mods:* \&definition ?:name ?rules:* end }
    => do-define-&definition(form, mods, name, rules);
end &macro;

define method do-define-&definition 
    (fragment :: <fragment>, mods, name, rules)
  let module = definitions-module();
  with-expansion-module (module)
    do-define-some-kind-of-an-expander
      (fragment, mods, name, 
       #{ env, form }, rules, 
       #{ ?name ## "-definition" }, 
       #{ do-define-core-definition });
  end;
end method;

define &macro &macro-definer
  { define ?mods:* \&macro ?:name ?rules:* end }
    => do-define-&macro(form, mods, name, rules);
end &macro;

define method do-define-&macro 
    (fragment :: <fragment>, mods, name, rules)
  let module = definitions-module();
  with-expansion-module (module)
    do-define-some-kind-of-an-expander
      (fragment, mods, name, 
       #{ env, form }, rules,
       #{ ?name ## "-expander" }, 
       #{ do-define-core-macro });
  end;
end method;

define method do-define-some-kind-of-an-expander
    (fragment :: <fragment>, mods, name, parameters, 
       rules, expander-name, installer-name) 
 => (expansion)
  let module = definitions-module();
  with-expansion-module (module)
    let (main-rule-set, aux-rule-sets) = parse-macro-rules(name, rules);
    // Need to at least the pre-processing/analysis here, right now to
    // determine the word involved.
    let compiled-main = compile-rule-set-spec(main-rule-set);
    let compiled-aux = map(compile-rule-set-spec, aux-rule-sets);
    let compiled-exp 
      = make(<rewrite-rule-expander>,
	     name: name,
	     module: fragment-module(name),
	     main-rule-set: compiled-main,
	     aux-rule-sets: compiled-aux);
    let input = #{ form };
    let expander-code
      = compile-define-macro-rules(input, compiled-exp);
    let descriptor-generator
      = make-macro-descriptor-generator-matching
          (main-rule-set.spec-rule-specs.first.spec-pattern-spec,
           name, expander-name);
    #{ define method ?expander-name (?parameters)
	 ?expander-code
       end;
       ?installer-name
         (?#"name", #f, #f, ?descriptor-generator, ?expander-name); }
    end with-expansion-module;
end method;

// TODO: Allow the special case word categrory for macro definers to be
// declared in the definition somewhere instead of using these name
// spotting hacks.

define method make-macro-descriptor-generator-matching 
    (spec :: <define-body-pattern-spec>, name, expander-name)
  let word-class
    = macro-case (name)
        { macro-definer }       => #{ $define-macro-body-word-only-token }
        { &macro-definer }      => #{ $define-macro-body-word-only-token }
        { &converter-definer }  => #{ $define-macro-body-word-only-token }
        { &definition-definer } => #{ $define-macro-body-word-only-token }
        { ?other:* }            => #{ $define-body-word-only-token }
     end;
  #{ make(<suffixed-macro-descriptor>,
          word-class: ?word-class,
          suffix:     "-definer",
          expander-function: ?expander-name) }
end method;

define method make-macro-descriptor-generator-matching 
    (spec :: <define-list-pattern-spec>, name, expander-name)
  #{ make(<suffixed-macro-descriptor>, 
          word-class: $define-list-word-only-token,
          suffix:     "-definer",
          expander-function: ?expander-name) }
end method;

define method make-macro-descriptor-generator-matching 
    (spec :: <statement-pattern-spec>, name, expander-name)
  let word-class
    = macro-case (name)
        { \macro-case } => #{ $macro-case-begin-word-only-token }
        { \macro }      => #{ $macro-case-begin-word-only-token }
        { ?other:* }    => #{ $begin-word-only-token }
     end;
  #{ make(<simple-macro-descriptor>, 
          word-class: ?word-class,
          expander-function: ?expander-name) }
end method;

define method make-macro-descriptor-generator-matching 
    (spec :: <function-pattern-spec>, name, expander-name)
  #{ make(<simple-macro-descriptor>, 
          word-class: $function-word-only-token,
          expander-function: ?expander-name) }
end method;

define method make-macro-descriptor-generator-matching 
    (spec :: <local-declaration-pattern-spec>, name, expander-name)
  let word-class
    = macro-case (name)
        { \local }   => #{ $local-methods-word-only-token }
        { ?other:* } => #{ $local-declaration-word-only-token }
     end;
  #{ make(<simple-macro-descriptor>, 
          word-class:        ?word-class,
          expander-function: ?expander-name) }
end method;

//// Macro case.

define &macro \macro-case
  { \macro-case (?input:expression) ?rules:* end }
    => do-expand-macro-case(form, input, rules);
end &macro;

define method do-expand-macro-case (fragment :: <fragment>, input, rules)
  // let module = fragment-module(fragment-macro(fragment));
  let module = macro-expander-module();
  let aux-rule-sets = parse-macro-case-rules(rules);
  // Need to at least the pre-processing/analysis here, right now to
  // determine the word involved.
  with-expansion-module (module)
    let compiled-main = compile-rule-set-spec(aux-rule-sets.head);
    let compiled-aux = map(compile-rule-set-spec, aux-rule-sets.tail);
    let compiled-exp 
      = make(<rewrite-rule-expander>,
	     module: module,
	     main-rule-set: compiled-main,
	     aux-rule-sets: compiled-aux);
    let expander-code
      = compile-macro-case-rules(input, compiled-exp);
    // break("Done macro case");
    expander-code
  end;
end method;

define method parse-macro-case-rules 
    (f) => (main-rule-set, aux-rule-sets)
  let aux-rules-f = #{ macro-case-main-aux-rule-set: ?f };
  collecting (aux-rule-sets)
    iterate walk (input-f = aux-rules-f)
      macro-case (input-f)
        { } => #t;
        { ?stuff:* }
          => begin
               let (next-set, remaining-f) 
                 = parse-macro-aux-rule-set(#f, stuff);
               collect-into(aux-rule-sets, next-set);
               walk(remaining-f);
             end;
      end;
    end;
    let aux-rule-sets = collected(aux-rule-sets);
    aux-rule-sets
  end;
end method;

define serious-program-warning 
    <unexpected-procedural-template> (<manual-parser-error>)
  format-string
    "Invalid use of the internal compiler macro syntax #{ } outside "
    "the compiler.";
end serious-program-warning;

define &macro \macro-template
  { \macro-template ?stuff:* end }
    => do-expand-macro-template(form, stuff);
end &macro;

define method do-expand-macro-template (fragment :: <fragment>, stuff)
  let module = macro-expander-module();
  if (module)
    let template-code
      = with-expansion-module (module)
	  let analysed-template 
            = compile-template-spec-elements(fragment-fragments(stuff));
          compile-macro-template-to-code(analysed-template);
        end;
    template-code
  else
    note(<unexpected-procedural-template>,
         source-location: fragment-source-location(fragment));
    // Doesn't return.
  end;
end method;

define method macro-expander-module ()
  /*
  let ld = lookup-library-description("dfmc-macro-expander");
  lookup-module-in(ld.language-definition, #"dfmc-macro-expander");
  */
  lookup-module(#"dfmc-macro-expander", default: #f);
end method;

define method definitions-module ()
  lookup-module(#"dfmc-conversion", default: #f)
    | lookup-module(#"dfmc-definitions");
end method;
