Module:    dfmc-macro-expander
Synopsis:  Hacks to convert emulator-style patterns/macros to compiler
           style descriptions. Bootstrapping only.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro hack-import-definer
  { define hack-import ?names end }
    => { ?names }
names:
  { }
    => { }
  { ?:name, ... }
    => { define constant "old/" ## ?name 
           = access(infix-reader, ?name); ... }
end macro;

define hack-import
  <fragment>, fragment-tokens,
  <parsed-fragment>, 
  <bracketed-fragment>, <sbracketed-fragment>, <cbracketed-fragment>,
  <unconstrained-pattern-variable>, name, wildcard?,
  <constrained-pattern-variable>, type,
  <variable-pattern>,
  <property-list-pattern>, rest-pattern, key-patterns, rest?, key?, all-keys?,
  <spliced-pattern-variable>, pattern, before, after,
  <main-rule>, name, pattern, template,
  <define-rule>, 
  <define-bindings-rule>,
  <function-rule>,
  <local-declaration-rule>, body-pattern,
  *semicolon*,
  <template>
end;

define method make-old-expression (name)
  make(old/<parsed-fragment>, 
       token-class: parsed-expression:, 
       token-value: name)
end method;

define method make-old-expression (temp :: old/<template>)
  make-old-expression(list(#"syntax-template", temp));
end method;

define function make-old-name (name)
  make(old/<parsed-fragment>, 
       token-class: parsed-name:, 
       token-value: name)
end function;

define method convert-pattern-match (f :: old/<fragment>)
  let tokens = old/fragment-tokens(f);
  convert-template-token(tokens.first, tokens.second);
end method;

define method convert-pattern-match (f :: old/<bracketed-fragment>)
  make(<paren-match>, nested-pattern: convert-pattern(f.old/fragments))
end method;

define method convert-pattern-match (f :: old/<sbracketed-fragment>)
  make(<bracket-match>, nested-pattern: convert-pattern(f.old/fragments))
end method;

define method convert-pattern-match (f :: old/<cbracketed-fragment>)
  make(<brace-match>, nested-pattern: convert-pattern(f.old/fragments))
end method;

define method convert-pattern-match 
    (f :: old/<unconstrained-pattern-variable>)
  make(<simple-match>, 
       symbol-name: f.old/name,
       variable-name: make-old-name(f.old/name),
       constraint:    #"*");
end method;

define method convert-pattern-match 
    (f :: old/<constrained-pattern-variable>)
  make(<simple-match>, 
       symbol-name: f.old/name,
       variable-name: make-old-name(f.old/name),
       constraint:    f.old/type);
end method;

define method convert-pattern-match 
    (f :: old/<variable-pattern>)
  make(<variable-match>, 
       variable-name-pattern: convert-pattern-match(f.old/name),
       type-expression-pattern: convert-pattern-match(f.old/type));
end method;

define method convert-pattern-match
    (f :: old/<property-list-pattern>)
  make(<property-list-match>,
       rest-pattern: 
         f.old/rest? & convert-pattern-match(f.old/rest-pattern),
       key-patterns:
         if (f.old/key?)
           map(convert-key-pattern-match, f.old/key-patterns)
         else
           #()
         end);
end method;

// Handle quoted constants.

define method strip-quote (expr) expr end;

define method strip-quote (expr :: <pair>)
  if (expr.first == #"quote")
    expr.second 
  else
    expr
  end
end method;

define function convert-key-pattern-match (key-match)
  let f = first(key-match);
  let default = second(key-match, default: not-found());
  if (found?(default))
    default := make(<literal-fragment>, 
		    record: #f,
                    source-position: #f,
                    value: strip-quote(default));
  else
    default := #f;
  end;
  make(<key-match>, 
       symbol-name:   f.old/name,
       variable-name: make-old-name(f.old/name),
       constraint:    if (instance?(f, old/<constrained-pattern-variable>))
                        f.old/type
                      else
                        #"*"
                      end,
       default-expression: default);
end function;

define method convert-pattern-match 
    (f :: old/<spliced-pattern-variable>)
  make(<splicing-match>, 
       nested-pattern: convert-pattern-match(f.old/pattern),
       prefix: f.old/before | "",
       suffix: f.old/after | "");
end method;

define method convert-pattern (contents)
  if (empty?(contents))
    #()
  else
    let type = contents.first;
    if (keyword?(type))
      let value
        = make(<variable-name-fragment>, 
	       record: #f,
               source-position: #f,
               name: as(<symbol>, as(<string>, type)));
      pair(value,
           convert-pattern(contents.tail.tail));
    elseif (object-class(type) == <symbol>)
      let value = contents.second;
      pair(convert-template-token(type, value),
           convert-pattern(contents.tail.tail));
    else
      pair(convert-pattern-match(type),
           convert-pattern(contents.tail));
    end;
  end;
end method;

define method compile-compiler-pattern (x)
  let m* = convert-pattern(x);
  let bound-names = compute-bound-variable-names(m*);
  access(infix-reader, reparse)
    (compile-pattern-elements(bound-names, m*));
end method;

define method compile-compiler-fragment-case 
    (#key name, main-rules, aux-rule-sets)
  let name-fragment 
    = name & make(<variable-name-fragment>, 
	          record: #f,
                  source-position: #f,
                  name: name);
  let converted-rules 
    = make(<rewrite-rule-set>, rewrite-rules: map(convert-rule, main-rules));
  let converted-aux
    = map(convert-aux-rule-set, aux-rule-sets);
  let converted-expander
    = make(<rewrite-rule-expander>,
           name:          name-fragment,
           main-rule-set: converted-rules,
           aux-rule-sets: converted-aux);
  access(infix-reader, reparse)
    (compile-rewrite-rule-expander(converted-expander));
end method;

define method compile-compiler-macro-call-case
    (#key name, main-rules, aux-rule-sets)
  compile-compiler-fragment-case
    (name: name, main-rules: main-rules, aux-rule-sets: aux-rule-sets);
end method;

define hack-import
  <statement-rule>,
  <rule>, pattern, template,
  <aux-rule>, pattern, template, fragments,
  <aux-rule-set>, name, aux-rules
end;

// Just ensure the template is in code form ready to processing
// in the expansion - don't try anything slick.

define method convert-rule (rule :: old/<rule>)
  let pattern = convert-pattern(rule.old/pattern.old/fragments);
  let template-code = make-old-expression(rule.old/template);
  make(<rewrite-rule>, pattern: pattern, template-code: template-code);
end method;

define method convert-rule (rule :: old/<main-rule>)
  let pattern = convert-pattern(rule.old/pattern.old/pattern.old/fragments);
  let template-code = make-old-expression(rule.old/template);
  make(<rewrite-rule>, pattern: pattern, template-code: template-code);
end method;

define method convert-rule (rule :: old/<define-rule>)
  let pattern = convert-pattern(rule.old/pattern.old/pattern.old/fragments);
  let template-code = make-old-expression(rule.old/template);
  make(<rewrite-rule>, pattern: pattern, template-code: template-code);
end method;

define method convert-rule (rule :: old/<local-declaration-rule>)
  let pattern 
    = convert-pattern
        (concatenate
          (rule.old/pattern.old/pattern.old/fragments,
           list(old/*semicolon*),
           list(rule.old/pattern.old/body-pattern)));
  let template-code = make-old-expression(rule.old/template);
  make(<rewrite-rule>, pattern: pattern, template-code: template-code);
end method;

define method convert-aux-rule-set (set :: old/<aux-rule-set>)
  make(<aux-rewrite-rule-set>,
       name: set.old/name,
       variable-name: make-old-name(set.old/name),
       rewriter-variable-name: 
         make-old-name
           (as(<symbol>, 
            concatenate(as(<string>, set.old/name), "-rewriter"))),
       rewrite-rules: map(convert-rule, set.old/aux-rules));
end method;

define method macro-word-in-variable-name
    (compiled-macro, variable-name) => (word, word-class)
  let (word, word-class) 
    = access(infix-reader, macro-word-in-variable-name)
        (compiled-macro, variable-name);
  values(word, translate-word-class(word-class));
end method;

define method translate-word-class (word-class)
  #"$unreserved-name-token"
end method;

define method translate-word-class (word-class == #"<fragment-define-word>")
  #"$define-body-word-only-token"
end method;

define method translate-word-class 
    (word-class == #"<fragment-define-bindings-word>")
  #"$define-list-word-only-token"
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<define-rule>, name == #"macro-definer")
  values(rule.old/name, #"$define-macro-body-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<define-rule>, name == #"&macro-definer")
  values(rule.old/name, #"$define-macro-body-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<define-rule>, name == #"&converter-definer")
  values(rule.old/name, #"$define-macro-body-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<define-rule>, name == #"&definition-definer")
  values(rule.old/name, #"$define-macro-body-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<statement-rule>, name == #"macro-case")
  values(rule.old/name, #"$macro-case-begin-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<statement-rule>, name)
  values(rule.old/name, #"$begin-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<local-declaration-rule>, name)
  values(rule.old/name, #"$local-declaration-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<local-declaration-rule>, name == #"local")
  values(rule.old/name, #"$local-methods-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<function-rule>, name)
  values(rule.old/name, #"$function-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<define-rule>, name)
  values(rule.old/name, #"$define-body-word-only-token");
end method;

define method compiler-macro-word-in-variable-name 
   (rule :: old/<define-bindings-rule>, name)
  values(rule.old/name, #"$define-list-word-only-token");
end method;

define method macro-word-in-variable-name (v :: <vector>, var)
  values(v[1], v[2]);
end method;

// eof
