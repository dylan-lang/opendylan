Module:   dfmc-definitions
Synopsis: The shared-symbols definition processor.
Author:   Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Shared-symbols definitions.

// Shared-symbols definition objects.

define class <shared-symbols-definition> (<variable-defining-form>)
  constant slot form-shared-symbols :: <sequence>,
    required-init-keyword: shared-symbols:;
end class;

define method form-define-word
    (form :: <shared-symbols-definition>) => (word :: <symbol>)
  #"shared-symbols"
end method;

// Conversion to a definition object.

define &definition shared-symbols-definer
  { define ?mods:* shared-symbols ?:name ?shared-symbols:* end }
    => do-define-shared-symbols(form, mods, name, shared-symbols);
end &definition;

define function do-define-shared-symbols
    (form, mods, name, shared-symbols-form) => (forms)
  let (options, adjectives) = parse-shared-symbols-adjectives(mods);
  let shared-symbols = parse-shared-symbols(shared-symbols-form);
  let tlf 
    = apply(make, <shared-symbols-definition>, 
            adjectives:       adjectives,
            variable-name:    name,
	    source-location:  fragment-source-location(form),
	    shared-symbols:   as(<vector>, shared-symbols),
	    options);
  register-constant-variable-name(fragment-identifier(name));
  let initializer-definitions
    = generate-initializer-definition-forms(tlf);
   add(initializer-definitions, tlf)
end function;

define function parse-shared-symbols (shared-symbols-form)
  macro-case (shared-symbols-form)
    { ?shared-symbols:* } 
      => shared-symbols;
  shared-symbols:
    { }
      => #();
    { ?:symbol, ... }
      => pair(fragment-value(symbol), ...);
  end macro-case
end function;

//// Shared-symbols adjective parsing.

define function parse-shared-symbols-adjectives (adjectives-form) => (initargs)
  values(#[], #[])
end function;

define method generate-initializer-definition-forms
    (form :: <shared-symbols-definition>) => (new-forms :: <sequence>)
  let forms
    = collecting ()
        for (symbol in form-shared-symbols(form))
          let code  = #{ %resolve-symbol(?symbol) };
          let forms = top-level-convert(form, code);
          for (form in forms)
            collect(form);
          end for;
        end for;
      end collecting;
  forms
end method;

// eof


