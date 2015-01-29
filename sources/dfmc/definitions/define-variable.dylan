Module:   dfmc-definitions
Synopsis: The variable definition processor.
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Variable definitions.

// Variable definition objects.

define class <variable-definition> (<binding-defining-form>)
end class;

define leaf packed-slots form-properties
    (<variable-definition>, <variable-defining-form>)
  boolean slot form-thread? = #f, init-keyword:  thread?:;
  boolean slot form-locked? = #f, init-keyword:  locked?:;
  boolean slot form-atomic? = #f, init-keyword:  atomic?:;
end packed-slots;

define program-warning <variable-declared-both-thread-and-locked>
  format-string
    "Variable definition specifies the mutually exclusive properties "
    "\"thread\" and \"locked\"."
end program-warning;

define method initialize (form :: <variable-definition>, #key)
  next-method();
  if (form-thread?(form) & form-locked?(form))
    note(<variable-declared-both-thread-and-locked>,
         source-location: form-source-location(form));
  end;
end method;

define class <literal-value-variable-definition>
    (<variable-definition>, <literal-value-binding-defining-form>)
end;

define method form-define-word
    (form :: <variable-definition>) => (word :: <symbol>)
  #"variable"
end method;

define method constant?
    (binding :: <variable-definition>) => (value :: <boolean>)
  #f
end method;

// Conversion to a definition object.

define program-warning <malformed-define-variable>
  format-string "Invalid syntax for define variable.";
end;

define &definition variable-definer
  { define ?mods:* variable ?:variable = ?:expression }
    => do-define-variable(form, mods, variable, expression);
  { define ?mods:* variable (?bindings:*) = ?:expression }
    => do-define-variable(form, mods, bindings, expression);
  { define ?mods:* variable ?stuff:* }
    => begin
         note(<malformed-define-variable>,
               source-location: fragment-source-location(form));
         #();
       end;
end &definition;

define method do-define-variable (fragment, mods, bindings, init)
  let (initargs, adjectives) = parse-variable-adjectives(bindings, mods);
  let bindings-spec  = parse-value-bindings(bindings);
  let required-specs = spec-value-required-variable-specs(bindings-spec);
  let variable-names = bound-variable-names(bindings-spec);
  list
    (apply(make,
             if (size(required-specs) = 1 &
                  ~spec-value-rest?(bindings-spec) &
                  instance?(init, <literal-constant-fragment>) &
                  instance?(spec-type-expression(first(required-specs)),
                            <variable-name-fragment>))
                <literal-value-variable-definition>
              else
                <variable-definition>
              end,
            source-location:  fragment-source-location(fragment),
            variable-name:    if (size(variable-names) == 1)
                                variable-names.first
                              else
                                variable-names
                              end,
            type-expressions: bound-type-expressions(bindings-spec),
            adjectives:       adjectives,
            bindings-spec:    bindings-spec,
            init-expression:  init,
            initargs));
end method;

// Modifier parsing.

define property <variable-thread-property> => thread?: = #f
  value thread = #t;
end property;

define property <variable-locked-property> => locked?: = #f
  value locked = #t;
end property;

define property <variable-atomic-property> => atomic?: = #f
  value atomic = #t;
end property;

define constant variable-adjectives
  = list(<variable-thread-property>,
         <variable-locked-property>,
         <variable-atomic-property>);

define function parse-variable-adjectives
    (bindings, mods) => (initargs, adjectives)
  parse-property-adjectives(variable-adjectives, mods, bindings);
end function;
