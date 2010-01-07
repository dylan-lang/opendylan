Module:   dfmc-definitions
Synopsis: The generic function definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Generic function definitions.

define class <generic-definition> (<function-defining-form>)
end class;

define leaf packed-slots form-properties (<generic-definition>, <function-defining-form>)
  field slot form-sealing = $form-sealing/sealed, field-size: 2,
    init-keyword: sealing:;
  boolean slot form-sideways? = #f, // vs. upwards (maybe!)
    init-keyword: sideways?:;  
  boolean slot form-parameters-have-dynamic-extent? = #f,
    init-keyword: dynamic-extent:;
end packed-slots;

define method form-define-word
    (form :: <generic-definition>) => (word :: <symbol>)
  #"generic"
end method;

define method form-options (form :: <generic-definition>)
  #()
end method;

define method form-implicitly-defined? (form :: <generic-definition>) => (val)
  let parent = form.form-parent-form;
  // most commonly parent == #f, so check that first for speed even though
  // it's redundant with the other checks.
  parent & (instance?(parent, <method-definition>)
	      | instance?(parent, <slot-definition>))
end method;

define method form-compile-stage-only? 
    (form :: <generic-definition>) => (well? :: <boolean>)
  (form-inline-policy(form) == #"inline-only" & ~form-dynamic?(form))
    // The following case implements inheritance of the inline-only 
    // property by an implicitly defined generic if all its methods
    // are inline-only (and there is at least one method).
    | (form-implicitly-defined?(form) 
         & ~form-dynamic?(form)
         & begin
	     let binding = form-variable-binding(form);
             let modifying = untracked-binding-modifying-definitions(binding);
             // You can get empty modifiers for an implictly-defined generic
             // only if a virtual slot declaration was the generator.
             ~empty?(modifying)
                & every?(method (mod-form) 
                           if (instance?(mod-form, <method-definition>))
                             form-inline-policy(mod-form) == #"inline-only"
                           else
                             #t
                           end
                         end,
                         modifying)
           end)
end method;

// Conversion to a definition object.

define &definition generic-definer
  { define ?mods:* \generic ?:name ?signature:* }
    => do-define-generic(form, mods, name, signature);
end &definition;

define method do-define-generic (fragment, mods, name, signature)
  let (initargs, adjectives) = parse-generic-adjectives(name, mods);
  list(apply(make, <generic-definition>,
             source-location: fragment-source-location(fragment),
             variable-name:   name,
             adjectives:      adjectives,
             signature:       parse-generic-signature(name, signature),
             initargs));
end method;

// Modifier parsing.

define property <generic-sealed-property> => sealing: = $form-sealing/sealed
  value sealed        = $form-sealing/sealed;
  value open          = $form-sealing/open;
  value compiler-open = $form-sealing/compiler-open;
  value dynamic       = $form-sealing/dynamic;
end property;

define property <generic-sideways-property> => sideways?: = #f
  value sideways = #t;
end property;

//define property <generic-sealed-property-default-open> => sealing: = $form-sealing/open
//  value sealed        = $form-sealing/sealed;
//  value open          = $form-sealing/open;
//  value compiler-open = $form-sealing/compiler-open;
//  value dynamic       = $form-sealing/dynamic;
//end property;

//define property <generic-sideways-property-default-open> => sideways?: = #t
//  value sideways = #t;
//end property;

define property <generic-dynamic-extent-property> => dynamic-extent: = #f
  value dynamic-extent = #t;
end property;

define property <generic-inline-property> 
    => inline-policy: = #"default-inline"
  value inline-only    = #"inline-only";
end property;

define constant generic-adjectives-default-sealed
  = list(<generic-sealed-property>,
         <generic-sideways-property>,
         <generic-dynamic-extent-property>,
         <generic-inline-property>);

//define constant generic-adjectives-default-open
//  = list(<generic-sealed-property-default-open>, 
//         <generic-sideways-property-default-open>,
//         <generic-dynamic-extent-property>,
//         <generic-inline-property>); 

define function generic-adjectives ()
//  let mode = current-compilation-mode();
//  if (mode  == #"default-open") generic-adjectives-default-open
//  else generic-adjectives-default-sealed end
  generic-adjectives-default-sealed
end function;

define method parse-generic-adjectives 
    (name, adjectives-form) => (initargs, adjectives)
  parse-property-adjectives(generic-adjectives(), adjectives-form, name)
end method;

// Signature parsing.

define program-warning <next-method-in-define-generic>
  constant slot condition-generic-name,
    required-init-keyword: generic-name:;
  format-string
    "#next specified in the parameter list of the generic function %= "
    "- ignoring";
  format-arguments
    generic-name;
end program-warning;

define program-warning <non-empty-options-in-define-generic>
  constant slot condition-generic-name,
    required-init-keyword: generic-name:;
  format-string
    "Unexpected code after the signature of the generic "
    "function %= - ignoring";
  format-arguments
    generic-name;
end program-warning;

define method parse-generic-signature (name, sig-fragment) => (sig-spec)
  let (sig-spec, remains)
    = parse-signature-as
        (<generic-signature-spec>, sig-fragment);
  // Check that there's nothing after the signature, and warn if there
  // is.
  macro-case (remains)
    { } 
      => #t;
    { ?other:* } 
      => note(<non-empty-options-in-define-generic>,
              source-location: fragment-source-location(name),
              generic-name:    name);
  end;              
  verify-generic-signature-spec(name, sig-spec, sig-fragment);
  sig-spec
end method;

// TODO: Check that defaults weren't specified in the keyword arguments.

define method verify-generic-signature-spec (name, sig :: <generic-signature-spec>, sig-fragment)
  if (spec-argument-next-variable-spec(sig))
    note(<next-method-in-define-generic>,
         generic-name:     name,
         source-location:  fragment-source-location(sig-fragment));
  end;
end method;
