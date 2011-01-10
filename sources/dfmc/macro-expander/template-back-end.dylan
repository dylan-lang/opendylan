Module:    dfmc-macro-expander
Synopsis:  The template "back-end" functions, calls to which are generated
           in order to construct a new fragment when evaluating a template.
Author:    Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Expansion context.

define thread variable *expansion-module* = #f;

define macro with-expansion-module
  { with-expansion-module (?mod:expression) ?:body end }
    => { do-with-expansion-module(?mod, method () ?body end) }
end macro;

define inline function do-with-expansion-module (mod, body-thunk)
  dynamic-bind (*expansion-module* = mod)
    body-thunk();
  end;
end function;

//// Template objects.

define constant $the-empty-template = make(<template>, fragments: #());

define /* inline */ function make-template (#rest elements)
  if (empty?(elements) | empty-template-elements?(elements))
    // format-out("Folding: %=\n", elements);
    $the-empty-template
  else
    // TODO: PERFORMANCE: Remove this list coercion, and any dependence
    // on the fragments being a list.
    make(<template>, fragments: as(<list>, elements));
  end;
end function;

//// Of substitutions.

define inline method import-to-template 
    (value) => (f :: <fragment>)
  make-literal-fragment(value);
end method;

define inline method import-to-template 
    (f :: <fragment>) => (f :: <fragment>)
  f
end method;

define inline method import-to-template 
    (f :: <template>) => (f :: <template>)
  f
end method;

/*
define inline method fragment-name-symbol 
    (name :: <variable-name-fragment>) => (name-symbol :: <symbol>)
  fragment-name(name);
end method;
*/

//// Coercing substitution utilities.

define constant <fragment-or-template> = type-union(<fragment>, <template>);

define method stringify 
    (f :: <fragment-or-template>) => (string :: <byte-string>)
  format-to-string("%s", f);
end method;

define method stringify 
    (f :: <variable-name-fragment>) => (string :: <byte-string>)
  fragment-name-string(f)
end method;

define inline method as-name 
    (f :: <fragment-or-template>) => (name :: <variable-name-fragment>)
  macro-case (f)
    { ?:name } => name
  end;
end method;

//// Simple coercing substitutions.

define method substitute-as-string (f :: <fragment-or-template>)
  make-in-expansion(<string-fragment>, value: stringify(f));
end method;

define method substitute-as-symbol (f :: <fragment-or-template>)
  make-in-expansion
    (<symbol-fragment>, value: as(<symbol>, stringify(f)));
end method;

//// Splicing substitutions.

define serious-program-warning
    <macro-splicing-substitution-constraint-error> (<macro-match-error>)
  slot condition-fragment,
    required-init-keyword: fragment:;
  format-string 
    "Attempt to perform a splicing substitution on the non-identifier %s.";
  format-arguments fragment;
end serious-program-warning;

// As an identifier.

define inline method substitute-spliced-as-name 
    (prefix :: <string>, name :: <variable-name-fragment>, suffix :: <string>)
  splice-name-hygienically(name, prefix, suffix);
end method;

define method substitute-spliced-as-name 
    (prefix :: <string>, f :: <fragment>, suffix :: <string>)
  note(<macro-splicing-substitution-constraint-error>,
       source-location: fragment-source-location(f),
       macro-name: #"template",
       fragment: f);
end method;

define method substitute-spliced-as-name 
    (prefix :: <string>, f :: <template>, suffix :: <string>)
  substitute-spliced-as-name(prefix, as-name(f), suffix);
end method;

// As a string.

define method substitute-spliced-as-string 
    (prefix :: <string>, name :: <variable-name-fragment>, suffix :: <string>)
  make-in-expansion
    (<string-fragment>,
       value: as-fragment-value
                (concatenate(prefix, fragment-name-string(name), suffix)))
end method;

define method substitute-spliced-as-string 
    (prefix :: <string>, f :: <fragment>, suffix :: <string>)
  note(<macro-splicing-substitution-constraint-error>,
       source-location: fragment-source-location(f),
       macro-name: #"template",
       fragment: f);
end method;

define method substitute-spliced-as-string 
    (prefix :: <string>, f :: <template>, suffix :: <string>)
  substitute-spliced-as-string(prefix, as-name(f), suffix);
end method;

// As a symbol.

define method substitute-spliced-as-symbol 
    (prefix :: <string>, name :: <variable-name-fragment>, suffix :: <string>)
  make-in-expansion
    (<symbol-fragment>,
       value: as-fragment-value
                (as(<symbol>,
                    concatenate(prefix, fragment-name-string(name), suffix))))
end method;

define method substitute-spliced-as-symbol 
    (prefix :: <string>, f :: <fragment>, suffix :: <string>)
  note(<macro-splicing-substitution-constraint-error>,
       source-location: fragment-source-location(f),
       macro-name: #"template",
       fragment: f);
end method;

define method substitute-spliced-as-symbol 
    (prefix :: <string>, f :: <template>, suffix :: <string>)
  substitute-spliced-as-symbol(prefix, as-name(f), suffix);
end method;

//// ?? sequence substitutions.

define method substitute-sequence (seq :: <sequence>)
  map-as(<list>, import-to-template, seq)
end method;

define method substitute-sequence (seq :: <sequence-fragment>)
  substitute-sequence(fragment-fragments(seq));
end method;

define method substitute-sequence-separated 
    (seq :: <sequence>, sep :: <separator-fragment>)
  collecting ()
    for (frag in seq, first = #t then #f)
      if (~first) collect(sep) end;
      collect(import-to-template(frag));
    end;
  end;
end method;

define method substitute-sequence-separated 
    (seq :: <sequence-fragment>, sep :: <separator-fragment>)
  substitute-sequence-separated(fragment-fragments(seq), sep);
end method;

// TODO: CORRECTNESS: Be careful about templates of templates that are
// really empty but may appear not to be.

define method maybe-substitute-separator 
    (sep :: <separator-fragment>, subst :: <list>)
  if (empty?(subst)) subst else pair(sep, subst) end;
end method;

define method maybe-substitute-separator 
    (sep :: <separator-fragment>, subst :: <sequence-fragment>)
  maybe-substitute-separator(sep, fragment-fragments(subst));
end method;

define method maybe-substitute-separator 
    (sep :: <separator-fragment>, subst :: <template>)
  maybe-substitute-separator(sep, template-fragments(subst));
end method;

define method maybe-substitute-separator 
    (sep :: <separator-fragment>, subst)
  list(sep, subst);
end method;

define function empty-template-elements? 
    (elements :: <sequence>) => (empty? :: <boolean>)
  every?(empty-template-element?, elements);
end function;

define method empty-template-element? (e) => (empty? :: <boolean>)
  #f 
end method;

define method empty-template-element? 
    (e :: <sequence>) => (empty? :: <boolean>)
  empty?(e);
end method;

define method empty-template-element? 
    (e :: <sequence-fragment>) => (empty? :: <boolean>)
  empty?(fragment-fragments(e));
end method;

define method empty-template-element? 
    (e :: <template>) => (empty? :: <boolean>)
  empty?(template-fragments(e));
end method;

//// Of other fragments.

define thread variable *expansion-source-location* = #f;

// TODO: This sucks. It should all be computed at macro compilation
// time.

define thread variable *expansion-fragment-cache* = #f;
define constant $shared-expansion-fragment-cache :: <object-table>
  = make(<object-table>);

define inline function make-in-expansion-caching (key, class, #rest initargs)
  if (*expansion-fragment-cache*)
    cached-expansion-fragment(key)
      | (cached-expansion-fragment(key)
           := apply(make-in-expansion, class, initargs))
  else
    apply(make-in-expansion, class, initargs)
  end;
end function;

define inline method cached-expansion-fragment 
    (key :: <object>) => (f :: false-or(<fragment>))
  let cached = element(*expansion-fragment-cache*, key, default: #f);
  /*
  if (cached)
    format-out("Cache win: %s\n", key);
  end;
  */
  cached
end method;

define inline method cached-expansion-fragment-setter
    (f :: <fragment>, key :: <object>) => (f :: <fragment>)
  element(*expansion-fragment-cache*, key) := f
end method;

define inline function do-with-expansion-source-location 
    (rec, pos, f :: <function>)
  dynamic-bind (*expansion-source-location* = pair(rec, pos))
    let use-shared? = ~(*expansion-fragment-cache*);
    if (use-shared?)
      dynamic-bind (*expansion-fragment-cache* 
                      = $shared-expansion-fragment-cache)
        block ()
          f();
        cleanup
          remove-all-keys!(*expansion-fragment-cache*);
        end;
      end;
    else
      dynamic-bind (*expansion-fragment-cache* = make(<object-table>))
        f();
      end;
    end;
  end;
end function;

define macro with-expansion-source-location
  { with-expansion-source-location (?rec:expression, ?pos:expression)
     ?:body
    end }
    => { do-with-expansion-source-location(?rec, ?pos, method () ?body end) }
end macro;

define inline function do-with-expansion-source-form 
    (form, f :: <function>)
  let loc = form-source-location(form);
  with-expansion-source-location 
      (loc & form-compilation-record(form),
         loc & source-location-source-position(loc))
    f();
  end;
end function;

define macro with-expansion-source-form
  { with-expansion-source-form (?form:expression)
     ?:body
    end }
    => { do-with-expansion-source-form(?form, method () ?body end) }
end macro;

// For template elements.

define inline function make-in-expansion (class, #rest initargs)
  let exp = *expansion-source-location*;
  apply(make, class, record: exp & exp.head, source-position: exp & exp.tail,
	initargs)
end function;

// For #key pattern defaults from the LHS.

define function default-in-expansion 
    (default-expression :: <fragment>) => (copy :: <fragment>)
  deep-copy(make(<default-fragment-copier>), default-expression)
end function;

define class <default-fragment-copier> (<fragment-copier>) end;

define dont-copy-slots <fragment> using <default-fragment-copier> =
  { fragment-record          
      => begin
           let exp = *expansion-source-location*;
           exp & exp.head
         end,
    fragment-source-position 
      => begin
           let exp = *expansion-source-location*;
           exp & exp.tail
         end };

// This forces variable names in the default that were parsed simple to
// become special when substituted.

define method do-deep-copy
    (copier :: <default-fragment-copier>, f :: <variable-name-fragment>)
 => (copy :: <variable-name-fragment>)
  make-name-fragment(fragment-name(f));
end method;

define method make-literal-fragment (value :: <object>)
  make-in-expansion(<elementary-literal-fragment>, value: value);
end method;

define method make-literal-fragment (value :: <integer>)
  make-in-expansion(<integer-fragment>, value: value);
end method;

define method make-literal-fragment (value :: <string>)
  make-in-expansion(<string-fragment>, value: value);
end method;

define method make-literal-fragment (value :: <symbol>)
  make-in-expansion(<symbol-syntax-symbol-fragment>, value: value);
end method;

define method make-literal-fragment (value :: <boolean>)
  if (value)
    make-in-expansion(<true-fragment>, value: value);
  else
    make-in-expansion(<false-fragment>, value: value);
  end;
end method;

define method make-literal-fragment (value :: <character>)
  make-in-expansion(<elementary-literal-fragment>, value: value);
end method;

// TODO: CORRECTNESS: Decide what do do about elements/values and
// the stubbing that's going on here.

define method make-literal-fragment (value == #())
  make-in-expansion
    (<list-fragment>, value: value, elements: #());
end method;

define method make-literal-fragment (value :: <list>)
  make-in-expansion
    (<list-fragment>, value: value, elements: #());
end method;

define method make-literal-fragment (value :: <vector>)
  make-in-expansion
    (<vector-fragment>, value: value, elements: #());
end method;

define function make-name-fragment (value :: <symbol>)
  make-in-expansion-caching
    (value, <variable-name-fragment>, 
     name: value, 
     // TODO: CORRECTNESS: This is not the right context.
     context: *expansion-module*,
     kind: classify-expansion-word-in(*expansion-module*, value));
end function;

define function make-escaped-name-fragment (value :: <symbol>)
  // The default kind is unreserved.
  make-in-expansion
    (<escaped-name-fragment>, 
     name: value, 
     context: *expansion-module*);
end function;

define function make-unhygienic-name-fragment (value :: <symbol>)
  make-in-expansion
    (<variable-name-fragment>, 
     name: value, 
     context: calling-module(),
     origin:  #f, // indicates top level
     kind: classify-expansion-word-in(*expansion-module*, value));
end function;

define function calling-module () => (module)
  let dependent = *current-dependent*;
  let dependent-cr
    = if (instance?(dependent, <top-level-form>))
        form-compilation-record(dependent)
      else
        dependent
      end;
  compilation-record-module(dependent-cr);
end function;

// TODO: CORRECTNESS: Make sure this is handling binary/unary
// mixtures appropriately.

define function make-binary-operator-fragment (value :: <symbol>)
  make-in-expansion
    (<binary-operator-fragment>, name: value);
end function;

define function make-unary-operator-fragment (value :: <symbol>)
  make-in-expansion
    (<unary-operator-fragment>, name: value);
end function;

define function make-unary-and-binary-operator-fragment (value :: <symbol>)
  make-in-expansion
    (<unary-and-binary-operator-fragment>, name: value);
end function;

define function make-constrained-name-fragment 
    (symbol :: <symbol>, constraint :: <symbol>)
  make-in-expansion
    (<constrained-name-fragment>,
     context: *expansion-module*,
     name: symbol, 
     constraint: constraint);
end function;

define macro punctuation-constructor-definer
  { define punctuation-constructor "<" ## ?:name ## ">"; }
    => { define function "make-" ## ?name ()
           make-in-expansion("<" ## ?name ## ">");
         end function; }
end macro;

define punctuation-constructor <equal-fragment>;

define punctuation-constructor <equal-greater-fragment>;
define punctuation-constructor <colon-colon-fragment>;
define punctuation-constructor <dot-fragment>;
define punctuation-constructor <comma-fragment>;
define punctuation-constructor <semicolon-fragment>;
define punctuation-constructor <lparen-fragment>;
define punctuation-constructor <rparen-fragment>;
define punctuation-constructor <lbracket-fragment>;
define punctuation-constructor <rbracket-fragment>;
define punctuation-constructor <lbrace-fragment>;
define punctuation-constructor <rbrace-fragment>;

define punctuation-constructor <hash-lparen-fragment>;
define punctuation-constructor <hash-lbracket-fragment>;
define punctuation-constructor <hash-lbrace-fragment>;

define punctuation-constructor <hash-next-fragment>;
define punctuation-constructor <hash-rest-fragment>;
define punctuation-constructor <hash-key-fragment>;
define punctuation-constructor <hash-all-keys-fragment>;

define punctuation-constructor <query-fragment>;
define punctuation-constructor <query-query-fragment>;
define punctuation-constructor <query-equal-fragment>;
define punctuation-constructor <ellipsis-fragment>;

// TODO: PERFORMANCE: Flatten at compile time instead.

define function make-parens-fragment (nested)
  make-in-expansion
    (<parens-fragment>,
     left-delimiter: make-in-expansion(<lparen-fragment>),
     nested-fragments: nested,
     right-delimiter: make-in-expansion(<rparen-fragment>));
end function;

define function make-brackets-fragment (nested)
  make-in-expansion
    (<brackets-fragment>,
     left-delimiter: make-in-expansion(<lbracket-fragment>),
     nested-fragments: nested,
     right-delimiter: make-in-expansion(<rbracket-fragment>));
end function;

define function make-braces-fragment (nested)
  make-in-expansion
    (<braces-fragment>,
     left-delimiter: make-in-expansion(<lbrace-fragment>),
     nested-fragments: nested,
     right-delimiter: make-in-expansion(<rbrace-fragment>));
end function;

//// Fragment import

// To a macro-case.

define generic as-fragment-tokens 
    (object :: <object>) => (tokens :: <fragment-list>);

define inline method as-fragment-tokens 
    (f) => (tokens :: <fragment-list>)
  // Try to import it.
  list(import-to-template(f));
end method;

define inline method as-fragment-tokens 
    (f :: <fragment>) => (tokens :: <fragment-list>)
  list(f);
end method;

define method as-fragment-tokens 
    (f :: <template>) => (tokens :: <fragment-list>)
  let (failure, parsed-f)
    = parse-template-fragments-as
        ($start-fragment-constraint, template-fragments(f));
  if (failure)
    error("Template %= wouldn't parse as a fragment: %s.", f, failure);
  end;
  parsed-f
end method;

define inline method as-fragment-tokens 
    (f :: <sequence-fragment>) => (tokens :: <fragment-list>)
  fragment-fragments(f);
end method;

define inline method as-fragment-tokens 
    (f :: <sequence>) => (tokens :: <fragment-list>)
  f
end method;

define inline method as-fragment-tokens 
    (f :: <elementary-fragment>) => (tokens :: <fragment-list>)
  list(f);
end method;

define inline method as-fragment-tokens 
    (f :: <literal-constant-fragment>) => (tokens :: <fragment-list>)
  list(f);
end method;

define inline method as-fragment-tokens 
    (f :: <nested-fragment>) => (tokens :: <fragment-list>)
  list(f);
end method;

define inline method as-fragment-tokens 
    (f :: <macro-call-fragment>) => (tokens :: <fragment-list>)
  list(f);
end method;

define inline method as-fragment-tokens 
    (f :: <body-fragment>) => (tokens :: <fragment-list>)
  list(f);
end method;

// To a definition matcher.

define generic call-as-fragment-tokens 
    (object) => (tokens :: <fragment-list>);

define inline method fragment-end-of-modifiers-marker 
    (f :: <fragment>) => (marker :: <end-of-modifiers-marker>)
  make(<end-of-modifiers-marker>)
end method;

define method call-as-fragment-tokens 
    (f :: <body-definition-fragment>) => (tokens :: <fragment-list>)
  concatenate
    (f.fragment-modifiers, 
     list(f.fragment-end-of-modifiers-marker), 
     f.fragment-body-fragment)
end method;

define method call-as-fragment-tokens 
    (f :: <macro-body-definition-fragment>) => (tokens :: <fragment-list>)
  concatenate
    (f.fragment-modifiers, 
     list(f.fragment-end-of-modifiers-marker), 
     f.fragment-macro-body-fragment)
end method;

define method call-as-fragment-tokens 
    (f :: <list-definition-fragment>) => (tokens :: <fragment-list>)
  concatenate
    (f.fragment-modifiers, 
     list(f.fragment-end-of-modifiers-marker), 
     f.fragment-list-fragment)
end method;

define method call-as-fragment-tokens 
    (f :: <statement-fragment>) => (tokens :: <fragment-list>)
  fragment-body-fragment(f);
end method;

define method call-as-fragment-tokens 
    (f :: <local-declaration-call-fragment>) => (tokens :: <fragment-list>)
  concatenate
    (fragment-list-fragment(fragment-declaration-fragment(f)),
     list(make-in-expansion(<semicolon-fragment>)),
     list(fragment-body-fragment(f)));
end method;

define inline method call-as-fragment-tokens 
    (f :: <function-macro-fragment>) => (tokens :: <fragment-list>)
  fragment-body-fragment(f);
end method;

// For reference macros.
define inline method call-as-fragment-tokens 
    (f :: <variable-name-fragment>) => (tokens :: <fragment-list>)
  #()
end method;

// Out to user code.

define generic export-fragment-tokens (f*);

define method export-fragment-tokens (f* :: <list>)
  if (empty?(f*))
    make(<sequence-fragment>, 
         fragments: f*, record: #f, source-position: #f);
  elseif (empty?(f*.tail))
    f*.head
  else
    make(<sequence-fragment>, 
         fragments: f*, record: #f, source-position: #f);
  end;
end method;

define inline method export-fragment-tokens (f :: <fragment>)
  f
end method;
