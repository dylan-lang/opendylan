Module:   dfmc-common
Synopsis: Modeled objects both mapped and unmapped common properties
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Modeled objects.

// Library is maintained locally rather than by searching up through the
// definition so that we have that information for all objects, including
// those that weren't explicitly defined.

define compiler-open generic model-definition (object) => (res);
define compiler-open generic model-definition-setter (def, object);

define function model-has-definition? (object) => (has? :: <boolean>)
  model-definition(object) & #t
// TODO: try this to avoid loading definition:
//  model-variable-name(object) & #t
end;

define function model-source-location (object) => (loc)
  let def = model-definition(object);
  when (def)
    def.form-source-location
      | begin
	  let parent = def.form-parent-form;
	  parent & parent.form-source-location
	end;
  end;
end;


define function model-compile-stage-only? (object) => (compile-stage-only?)
  let def = model-definition(object);
  def & form-compile-stage-only?(def);
end;

define abstract compiler-open dood-class <model-properties> 
    (<dood-mapped-object>, <emitted-object>)
  lazy slot model-definition = #f,
    init-keyword: definition:;
  lazy constant slot model-creator = *current-dependent* |
    error("Attempt to create a model outside of proper compilation-context");
end dood-class;

define function model-compilation-record
    (m) => (cr :: false-or(<compilation-record>))
  compilation-record-of(model-creator(m));
end function;

define function model-original-library (m) => (ld)
  let cr = model-compilation-record(m);
  debug-assert(cr, "%s created outside of proper compilation context", m);
  compilation-record-original-library(cr)
end;

define method model-creator-library (c :: <compilation-context>)
  c
end method;

define method model-creator-library (c)
  compilation-record-library(compilation-record-of(c));
end;

define function model-variable-name (m)
  let form = model-definition(m);
  form & model-variable-using-definition(form, m)
end;

define function model-library (m) => (ld)
  model-creator-library(model-creator(m));
end;

define method maybe-model-library (m :: <model-properties>) => (ld)
  model-library(m)
end;

define method maybe-model-library (m) => (ld)
  maybe-read-only-model-properties(m) &
    model-library(m)
end;

define function model-downloaded? (m) => (well? :: <boolean>)
  compilation-record-downloaded?(model-compilation-record(m))
end function;

define function model-interactive? (m) => (well? :: <boolean>)
  compilation-record-interactive?(model-compilation-record(m))
end function;

// The following protocol handles mapped objects that are "interned" in the 
// run time, but not necessarily where the compiler is currently running.

define compiler-open generic standard-model-object (object) => (standard);

define method standard-model-object (object) => (object)
  object
end method;

define compiler-open generic find-model-properties-in (ld, model, settable?, #key create?)
 => (properties :: false-or(<model-properties>));

define inline function maybe-read-only-model-properties (model)
  let model = standard-model-object(model);
  find-model-properties-in
    (current-library-description(), model, #f, create?: #f)
end function;

define inline function read-only-model-properties (model)
  let model = standard-model-object(model);
  find-model-properties-in(current-library-description(), model, #f)
end function;

define inline function settable-model-properties (model)
  let model = standard-model-object(model);
  find-model-properties-in(current-library-description(), model, #t)
end function;

// Ensures that model has model-properties, creating them if it doesn't,
// so that new model properties are created in the right context.
define compiler-open generic mapped-model (model) => model;

define method mapped-model (object :: <model-properties>) => model;
  // Only mergable <model-properties> are types, which are automatically
  // immutable
  immutable-model(object)
end method;

define method mapped-model (object) => model;
  read-only-model-properties(object);
  object
end method;

define method mapped-model (object :: <integer>) => model;
  object
end method;

define method mapped-model (object :: <byte-character>) => model;
  object
end method;

define method mapped-model (object :: <pair>) => model;
  read-only-model-properties(object);
  mapped-model(object.head);
  mapped-model(object.tail);
  object
end method;

define method mapped-model (object :: <simple-object-vector>) => model;
  read-only-model-properties(object);
  do(mapped-model, object);
  object
end method;

define method mapped-model (object :: <byte-string>) => model;
  immutable-model(object);
end method;

define method emitted-name (object)
  read-only-model-properties(object).emitted-name
end method;

define method emitted-name-setter (name, object)
  settable-model-properties(object).emitted-name := name
end method;

define method model-definition (object) => (def)
  read-only-model-properties(object).model-definition
end method;

define method model-definition-setter (defn, object)
  settable-model-properties(object).model-definition := defn
end method;

define compiler-open generic model-creator (object);

define method model-creator (object)
  read-only-model-properties(object).model-creator
end method;
