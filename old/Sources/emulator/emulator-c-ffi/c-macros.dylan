Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Define C-type-alias

// Defining an alias just defines a new binding for an existing wrapper
// class with space for documentation.

define macro C-type-alias-definer
  { define C-type-alias ?alias:name = ?type:expression ?options }
    => { both-stages
           define constant ?alias = ?type;
         end }
end macro;

//// Define C-discriminated-type

// Defining an discriminated type actually just defines a subclass of 
// canonical type? The local canonical type? The compiler must be able
// to work out what this superclass is at compile time, otherwise we're
// screwed.

// Local-canonical-type doesn't move between base types?

define macro C-discriminated-type-definer
  { define C-discriminated-type ?name ?spec end }
    => { both-stages
           define C-type-wrapper-class ?name ?spec end;
         end;
         define C-type-methods ?name ?spec end; }
end macro;

/* Macro system bug
define macro C-type-wrapper-class-definer

  { define C-type-wrapper-class ?name (?supers) as ?expression ?options end }
    => { define class ?name (?supers, abstract-supertype(?expression)) end;
         copy-wrapper-properties(?name, ?expression);
       }

  { define C-type-wrapper-class ?name as ?expression ?options end }
    => { define C-type-wrapper-class ?name () as ?expression ?options end }

end macro;
*/

define macro C-type-wrapper-class-definer

  { define C-type-wrapper-class ?name (?supers) as ?expression ?options end }
    => { define class ?name (canonical-type(?expression), ?supers) end;
         copy-wrapper-properties(?name, ?expression);
       }

  { define C-type-wrapper-class ?name as ?expression ?options end }
    => { define class ?name (canonical-type(?expression)) end;
         copy-wrapper-properties(?name, ?expression);
       }
end macro;

define macro c-type-methods-definer
  { define c-type-methods ?name ?supers as ?expression ?options }
    => { define method class-for-map (class == ?name) ?options end }
  { define c-type-methods ?name ?supers as ?expression }
    => { values() }
options:
  { #key ?map }
    => { ?map }
end macro;

//// Define C-subtype (supercedes define C-discriminated-type)

define macro C-subtype-definer
  { define C-subtype ?name (?supers) ?spec end }
    => { both-stages
           define C-subtype-wrapper-class ?name (?supers) ?spec end;
         end;
         define C-subtype-methods ?name (?supers) (?spec) end; }
end macro;

define macro C-subtype-wrapper-class-definer
  { define C-subtype-wrapper-class ?name (?supers) ?options end }
    => { define class ?name (?supers) end;
         copy-supers-properties(?name, ?supers)
       }
end macro;

define macro c-subtype-methods-definer
  { define c-subtype-methods ?name (?supers) 
      (#rest ?options, #key ?map, #all-keys)
    end }
    => { define method class-for-map (class == ?name) ?map end }
  { define c-subtype-methods ?name (?supers) ()
    end }
    => { values() }
end macro;

//// Define C-struct

define macro c-struct-definer
  { define c-struct ?name ?fields end }
    => { both-stages
           define c-struct-wrapper ?name ?fields end;
         end;
         define c-struct-methods ?name (?fields) (?fields) end; }
end macro;

define macro c-struct-wrapper-definer
  { define c-struct-wrapper ?name ?fields end }
    => { define class ?name (<C-struct>) end }
end macro;

define macro c-struct-methods-definer
  { define c-struct-methods ?name () () end }
    => { initialize-C-struct(?name, list()) }
  { define c-struct-methods ?name (?fields) (?generics) end }
    => { ?generics;
         initialize-C-struct(?name, list(?fields)) }
fields:
  { ?field; ... }
    => { ?field, ... }
  { }
    => { }
field:
  { slot ?name :: ?expression, ?options }
    => { make(<field-descriptor>, 
              getter:  ?name,
              setter:  ?name ## \-setter,
              type:    ?expression,
              ?options) }
  { #rest ?keys }
    => { }
generics:
  { ?gen; ... }
    => { ?gen; ... }
  { }
    => { }
gen:
  { slot ?name ?stuff }
    => { define generic ?name (obj);
         define generic ?name ## \-setter (val, obj) }
  { #rest ?keys }
    => { #f }
end macro;

//// Define C-union

define macro c-union-definer
  { define c-union ?name ?fields end }
    => { both-stages
           define c-union-wrapper ?name ?fields end;
         end;
         define c-union-methods ?name (?fields) (?fields) end; }
end macro;

define macro c-union-wrapper-definer
  { define c-union-wrapper ?name ?fields end }
    => { define class ?name (<C-union>) end }
end macro;

define macro c-union-methods-definer
  { define c-union-methods ?name (?fields) (?generics) end }
    => { ?generics;
         initialize-C-union(?name, list(?fields)) }
fields:
  { ?field; ... }
    => { ?field, ... }
  { }
    => { }
field:
  { slot ?name :: ?expression, ?options }
    => { make(<field-descriptor>, 
              getter:  ?name,
              setter:  ?name ## \-setter,
              type:    ?expression,
              ?options) }
  { #rest ?keys }
    => { }
generics:
  { ?gen; ... }
    => { ?gen; ... }
  { }
    => { }
gen:
  { slot ?name ?stuff }
    => { define generic ?name (obj);
         define generic ?name ## \-setter (val, obj) }
  { #rest ?keys }
    => { #f }
end macro;

//// Define C-function

define macro C-function-definer
  { define C-function ?dylan-name:name ?spec end }
    => { compile-stage
           generate-C-call-out(?#"dylan-name", list(?spec))
         end }
spec:
  { ?stuff; ... }
    => { ?stuff, ... }
  { }
    => { }
stuff:
  { ?style parameter ?expression, ?options }
    => { make(<arg-descriptor>,
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?style,
              ?options) }

  { ?style parameter ?name :: ?expression, ?options }
    => { make(<arg-descriptor>,
              name:            ?#"name",
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?style,
              ?options) }

  { result ?expression, ?options }
    => { make(<result-descriptor>,
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?options) }

  { result ?name :: ?expression, ?options }
    => { make(<result-descriptor>,
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?options) }

  { #rest ?keys }
    => { list(?keys) }
style:
  { output }
    => { by-reference: #t }
  { input output }
    => { by-reference: #t }
  {  }
    => { by-reference: #f }
end macro;

define macro C-callable-wrapper-definer
  { define C-callable-wrapper ?dylan-name:name of ?dylan-function:name
      ?spec
    end }
    => { compile-stage
           generate-C-call-in(?#"dylan-name", #"dylan-function", list(?spec))
         end }
spec:
  { ?stuff; ... }
    => { ?stuff, ... }
  { }
    => { }
stuff:
  { ?style parameter ?expression, ?options }
    => { make(<arg-descriptor>,
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?style,
              ?options) }

  { ?style parameter ?name :: ?expression, ?options }
    => { make(<arg-descriptor>,
              name:            ?#"name",
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?style,
              ?options) }

  { result ?expression, ?options }
    => { make(<result-descriptor>,
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?options) }

  { result ?name :: ?expression, ?options }
    => { make(<result-descriptor>,
              type:            ?expression,
              type-expression: quote(?expression), // HACK!!!
              ?options) }

  { #rest ?keys }
    => { list(?keys) }
style:
  { output }
    => { by-reference: #t }
  { input output }
    => { by-reference: #t }
  {  }
    => { by-reference: #f }
end macro;

define macro compile-stage
  { compile-stage ?body end }
    => { at-compile-stage(?body); values() }
end macro;

define macro both-stages
  { both-stages ?body end }
    => { at-compile-stage(begin ?body; #f end);
         ?body }
end macro;

//// Linking information

define macro C-link-requirements-definer
  { define C-link-requirements ?stuff end }
    => { cl-function(ffi(read-foreign-modules))(?stuff) }
end macro;

// eof
