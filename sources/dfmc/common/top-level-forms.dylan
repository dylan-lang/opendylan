Module:   dfmc-common
Synopsis: The symbolic top-level-form/definition framework
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Used by browser API
define abstract compiler-open class <variable-name> (<object>) end;

//// TODO: Work out this variable name forward-reference soup. Perhaps we
//// should move the AST node definitions forward, out of the reader.

define constant <variable-name-fragment> = <variable-name>; /* <fragment> */
define constant <type-expression> = <object>; /* <fragment> */

//// Top level forms.

define thread variable *interactive-compilation-layer* = #f;

define macro form-properties-definer
  { define ?adjectives:* form-properties ?:name (?supers:*) ?slots end }
   => { define form-properties-aux (?adjectives) ?name (?supers)
          (?slots) (?slots) (?slots) end; }
slots:
  { } => { }
  { ?slot:*; ...} => { ?slot; ...}
slot:
  { ?mods:* slot ?:name :: ?type:expression = ?init:expression, ?rest:* }
    => { ?mods slot ?name :: ?type = ?init, ?rest }
  { ?mods:* slot ?:name = ?init:expression, ?rest:* }
    => { ?mods slot ?name :: <object> = ?init, ?rest }
end macro;

define macro form-properties-aux-definer
  {define form-properties-aux (?adjectives:*) ?:name (?supers:*)
          (?slots:*) (?cslots) (?mergers) end }
    => { define ?adjectives dood-class ?name (?supers) ?cslots end;
         define form-properties-accessors ?name ?slots end;
         define method form-properties-class 
             (form :: ?name) => (c :: singleton(?name))
           ?name
         end method;
         define method merge-form-properties!
             (old :: ?name, new :: ?name, #next next-method)
           next-method();
           ?mergers;
         end method; }
cslots:
  { } => { }
  { ?cslot:*; ...} => { ?cslot; ...}
cslot:
  { ?mods:* slot ?slot-name :: ?type:expression = ?init:expression, ?rest:* }
    => { ?mods slot ?slot-name :: ?type = ?init, ?rest }
mergers:
  { } => { }
  { ?merger; ... } => { ?merger; ... }
merger:
  { ?mods:* slot ?slot-name :: ?type:expression = ?init:expression, ?rest:* }
    => { old.?slot-name := new.?slot-name }
slot-name: { ?:name } => { "shadowable-" ## ?name }
end macro;

define macro form-properties-accessors-definer
  { define form-properties-accessors ?class:name end } => { }
  { define form-properties-accessors ?class:name
      ?mods:* slot ?accessor:name :: ?type:expression = ?default:expression,
          ?rest:*;
      ?more:*
    end }
    => { define method ?accessor (object :: ?class) => (val :: ?type)
           let p = form-properties-in-context
                      (current-library-description(), object, #f);
           if (p) "shadowable-" ## ?accessor(p) else ?default end
         end method;
         define method ?accessor ## "-setter" (value, object :: ?class)
           let p = form-properties-in-context
                      (current-library-description(), object, #t);
           "shadowable-" ## ?accessor ## "-setter" (value, p)
         end method;
         define form-properties-accessors ?class ?more end }
end macro;


// All modifiable slots of top level forms must be defined in subclasses
// of <form-properties> via define form-properties.  A slot is modifiable
// if its contents can change after the end of the top-level processing
// transaction that created the slot.

define abstract class <form-properties> (<dood-mapped-object>) end;

define generic form-top-level-installable?
    (form :: <form-properties>) => (installable?);

// Default
define method form-top-level-installable? (form :: <form-properties>) => (no)
  #f
end method;

define generic form-properties-class (form :: <form-properties>) => c :: subclass(<form-properties>);

define function make-default-form-properties
    (form :: <form-properties>) => (p :: <form-properties>);
  make(form-properties-class(form))
end function;

define generic merge-form-properties! (old :: <form-properties>,
                                      new :: <form-properties>);

// Base case.
define method merge-form-properties! (old :: <form-properties>,
                                      new :: <form-properties>)
end method;



define form-properties <top-level-form-properties> (<form-properties>)
  lazy slot form-init-method = #f;
  lazy slot form-system-init-method = #f;
  slot form-properties-flags :: <integer> = 0;
end;

define packed-slots form-properties-flags (<top-level-form-properties>, <object>)
  boolean slot form-evaluation-tried-and-failed? = #f;
  boolean slot form-stripped? = #f;
end packed-slots;

define method initialize
    (x :: <top-level-form-properties>, #rest all-keys, #key, #all-keys)
  next-method();
  apply(initialize-packed-slots, x, all-keys)
end method;

define compiler-open generic form-source-location (object);

define compiler-open dood-class <top-level-form> /* abstract */ 
    (<top-level-form-properties>)
  constant slot form-compilation-record :: <compilation-record>
    = current-compilation-record();
  lazy constant slot form-source-location,
    required-init-keyword: source-location:;
  lazy slot form-parent-form :: false-or(<top-level-form>) = #f,
    init-keyword: parent-form:;
  // sequence number used to establish the ordering of forms - given two
  // forms with the same parent, the one with the smaller sequence number
  // comes first.  Note that the numbers are not guaranteed to be
  // sequential or start with 0.  In addition, derived forms created after
  // the initial top-level conversion are given sequence number of -1,
  // since they are assumed to be basically order independent wrt other
  // forms with the same parent.
  constant slot form-sequence-number :: <integer> = next-form-sequence-number();
  lazy slot form-dependencies = #f;
  lazy slot form-referenced-variables = #f;
end;

define constant <top-level-form-sequence> = <sequence>; 
// TODO: NEED REAL LIMITED SEQUENCE STORED IN SLOT 
//  = limited(<sequence>, of: <top-level-form>);

// HACK: SHOULD TIGHTEN TO <TOP-LEVEL-FORM> AFTER FIXING PACKED-SLOTS
define compiler-open generic form-properties-in-context
    (context, form :: <form-properties>, create?) 
 => (p :: false-or(<form-properties>));

define generic form-parent-form
    (form :: <top-level-form>) => (parent :: false-or(<top-level-form>));

define generic form-adjectives
    (form :: <top-level-form>) => (adjectives :: false-or(<sequence>));

define method form-adjectives 
    (form :: <top-level-form>) => (adjectives :: false-or(<sequence>));
  #f
end method;

define compiler-open generic form-define-word
  (form :: <top-level-form>) => (word :: false-or(<symbol>));

define method form-define-word 
    (form :: <top-level-form>) => (word :: false-or(<symbol>));
  #f
end method;

define generic form-library
    (form :: <top-level-form>) => (library-description);

define generic form-original-library
    (form :: <top-level-form>) => (false-or-library-description);

define compiler-open generic form-macro-word-class
    (form :: <top-level-form>) => (class);

define method form-macro-word-class 
    (form :: <top-level-form>) => (class)
  #f
end;

// We can get to the library via the compilation record.

define method form-library
    (form :: <top-level-form>) => (library-description)
  compilation-record-library(form-compilation-record(form))
end method;

define method form-original-library
    (form :: <top-level-form>) => (library-description)
  compilation-record-original-library(form-compilation-record(form))
end method;

define inline function form-downloaded?
    (form :: <top-level-form>) => (well? :: <boolean>)
  compilation-record-downloaded?(form-compilation-record(form))
end function;

define inline function form-interactive?
    (form :: <top-level-form>) => (well? :: <boolean>)
  compilation-record-interactive?(form-compilation-record(form))
end function;

// Return the form's top level methods. This is used as the starting
// point for code generation/optimisation.

define compiler-open generic form-top-level-methods
    (form :: <top-level-form>) => (method-models);

// The default is just the init method, if there is one.

define method form-top-level-methods
    (form :: <top-level-form>) => (method-models)
  let init = form-init-method(form);
  let so-far = if (init) list(init) else #() end;
  let system-init = form-system-init-method(form);
  let so-far = if (system-init) pair(system-init, so-far) else so-far end;
  so-far
end method;

define compiler-open generic install-top-level-forms
    (form :: <top-level-form-sequence>) => ();

// Totally undo the handling of the form.

define compiler-open generic retract-top-level-form (form :: <top-level-form>) => ();

// Sequencing/dependencies

// Current form or compilation record being processed
define thread variable *current-dependent* = #f;

define constant $no-dependent = #"no-dependent";

// The current compilation stage taking place
define thread variable *current-stage* = #f;

define macro with-dependent
  { with-dependent (?stage:expression of ?dependent:expression) ?:body end }
    => { do-with-dependent(?stage, ?dependent, method () ?body end) }
end macro;

define inline function do-with-dependent (stage, dependent, f)
  if (*current-stage* == stage & *current-dependent* == dependent)
    f()
  else
    debug-assert(~*interactive-compilation-layer*
                   | ~compilation-record-downloaded?
                        (compilation-record-of(dependent)),
                 // Trying to compile something that's already been downloaded?
                 "Changing interactive context to downloaded dependent?");
    debug-assert(current-library-description?
                   (compilation-record-library
                      (compilation-record-of(dependent))),
                 "New dependent %s is not in current context %s!",
                 dependent, current-library-description());
    dynamic-bind (*current-stage* = stage,
                  *current-dependent* = dependent)
      f()
    end;
  end if;
end function;

define macro without-dependency-tracking
  { without-dependency-tracking ?:body end }
    => { dynamic-bind (*current-stage* = #"testing",
                       *current-dependent* = $no-dependent)
          ?body
         end }
end macro;

define compiler-open generic remove-dependent-program-conditions (dependent, stages);

// Dependency tracking

// Name dependencies
define constant dep$name-syntax           = ash(1, 0);
define constant dep$name-macro-ref        = ash(1, 1);
define constant dep$name-binding          = ash(1, 2);
// This is the same as dep$name-binding, but indicates that the dependency
// should count as a "reference" for warning/browsing purposes.
define constant dep$name-binding-ref      = ash(1, 3);
// Binding dependencies.
define constant dep$active-definition     = ash(1, 4);
define constant dep$defined?              = ash(1, 5);
define constant dep$modifying-definitions = ash(1, 6);
define constant dep$model-object          = ash(1, 7);
define constant dep$modifying-models      = ash(1, 8);
define constant dep$count                 = 9;

define constant dep$all = ash(1, dep$count) - 1;

// Compilation stages.  For now (and maybe forever) everything from
// models on is one stage.
define constant stage$0                   = 0 * dep$count;
define constant stage$1                   = 1 * dep$count;
define constant stage$count               = 2;

// TODO: The folding version works around a bug in the compiler that
// prevents it folding away the inlined representation of ash that ends
// up in the body of make-dependency-condition (not to mention the
// debug-assert's which defeat it anyway).
define inline function make-dependency-condition
    (stage :: <integer>, kind :: <integer>) => (condition :: <integer>);
  debug-assert(stage == stage$0 | stage == stage$1, "Invalid stage %s", stage);
  debug-assert(0 <= kind & kind <= dep$all, "Invalid kind %s", kind);
  ash(kind, stage);
end function;

define macro folding-make-dependency-condition
  { folding-make-dependency-condition(?stage:expression, ?kind:expression) }
     => { ash(?kind, ?stage) }
end macro;

define constant stage$0-mask
  = folding-make-dependency-condition(stage$0, dep$all);
define constant stage$1-mask
  = folding-make-dependency-condition(stage$1, dep$all);
define constant stage$all :: <integer> = logior(stage$0-mask, stage$1-mask);

define constant $top-level-processing      = stage$0;
define constant $top-level-processing-mask = stage$0-mask;
define constant $compilation               = stage$1;
define constant $compilation-mask          = stage$1-mask;

define function dependency-stage-match? (stage, stage-mask)
  logand(make-dependency-condition(stage, dep$all), stage-mask) ~== 0
end function;

define compiler-open generic note-binding-dependency-of 
  (dependent, condition, binding);
define compiler-open generic note-name-dependency-of
  (dependent, condition, binding, name, module);

// These functions retract all dependents on the thing being changed, so
// that nothing depends on it any more.  For instance note-adding-definition
// retracts everything which depended on 'binding' being undefined, note-
// removing-definition retracts everything which depended on 'binding' having
// 'definition' as its definition, etc.
define compiler-open generic note-adding-definition (binding, definition);
define compiler-open generic note-removing-definition (binding, definition);
define compiler-open generic note-adding-modifying-definition (binding, definition);
define compiler-open generic note-removing-modifying-definition (binding, definition);
define compiler-open generic note-removing-model-object (binding);
define compiler-open generic note-removing-modifying-models (binding);
define compiler-open generic note-changing-binding (binding, name, module, new-binding);

define function note-binding-dependency (binding, kind)
  let dependent = *current-dependent*;
  unless (dependent == $no-dependent)
    let stage = *current-stage*;
    unless (dependent & stage)
      error("Unknown dependent for binding dependency on %s", binding.name);
    end;
    note-binding-dependency-of(dependent,
                               make-dependency-condition(stage, kind),
                               binding)
  end;
end function;

define function note-name-dependency (binding, kind, word, module)
  let dependent = *current-dependent*;
  unless (dependent == $no-dependent)
    let stage = *current-stage*;
    unless (dependent & stage)
      error("Unknown dependent for binding dependency on %s", binding.name);
    end;
    note-name-dependency-of(dependent,
                            make-dependency-condition(stage, kind),
                            binding, word, module)
  end;
end function;

define thread variable *last-form-sequence-number* = #f;

define macro with-form-creation
  { with-form-creation ?:body end }
    => { dynamic-bind (*last-form-sequence-number* = -1)
           ?body
         end;
       }
end macro;

// Assumes body doesn't do anything fancy, just makes a form.
define macro with-boot-form-creation
 { with-boot-form-creation (?seq:expression of ?parent:expression) ?:body end }
    => { do-with-boot-form-creation(?seq, ?parent, method () ?body end) }
end macro;

define inline function do-with-boot-form-creation (seq, parent, fn)
  dynamic-bind (*last-form-sequence-number* = seq - 1,
                *current-dependent* = parent,
                *current-stage* = $top-level-processing)
    fn()
  end;
end function;

define function next-form-sequence-number () => sequence-number;
  if (*current-stage* == $top-level-processing)
    *last-form-sequence-number* := *last-form-sequence-number* + 1
  else
    -1
  end
end function;

define function current-compilation-record ()
  compilation-record-of(*current-dependent*)
end;

define method compilation-record-of (cr :: <compilation-record>)
  cr
end method;

define method compilation-record-of (form :: <top-level-form>)
  form-compilation-record(form)
end method;

define method compilation-record-of (none == #"no-dependent")
  #f
end method;

// True if form is defined after base
define generic defined-after? (base, form) => form-after-base?;

// True if form is defined before base
define generic defined-before? (base, form) => form-before-base?;

define inline function library-defined-after? (base-lib, other-lib)
  // one of the arguments should be the current dependent, so
  // should be in the current library.  This makes things simpler...
  if (*interactive-compilation-layer*)
    debug-assert(~other-lib | ~base-lib);
    ~other-lib
  else
    debug-assert(current-library-in-context?(other-lib) |
                   current-library-in-context?(base-lib));
    current-library-in-context?(other-lib)
  end;
end function;

define inline method defined-after? (cr1 :: <compilation-record>,
                                     cr2 :: <compilation-record>) => after?;
  let lib1 = cr1.compilation-record-original-library;
  let lib2 = cr2.compilation-record-original-library;
  if (lib1 == lib2)
    let n1 :: <integer> = cr1.compilation-record-sequence-number;
    let n2 :: <integer> = cr2.compilation-record-sequence-number;
    n1 < n2
  else
    library-defined-after?(lib1, lib2)
  end;
end method;

define inline method defined-before? (cr1 :: <compilation-record>,
                                      cr2 :: <compilation-record>) => before?;
  defined-after?(cr2, cr1)
end;

define method defined-after? (cr :: <compilation-record>,
                              form :: <top-level-form>) => after?;
  let form-cr :: <compilation-record> = form.form-compilation-record;
  defined-after?(cr, form-cr)
end;

define method defined-after? (form :: <top-level-form>,
                              cr :: <compilation-record>) => after?;
  defined-before?(cr, form)
end;

define method defined-before? (cr :: <compilation-record>,
                               form :: <top-level-form>) => before?;
  let form-cr :: <compilation-record> = form.form-compilation-record;
  defined-before?(cr, form-cr)
end;

define method defined-before? (form :: <top-level-form>,
                               cr :: <compilation-record>) => before?;
  defined-after?(cr, form)
end;

define method defined-after? (base :: <top-level-form>,
                              form :: <top-level-form>) => after?;
  let base-cr = base.form-compilation-record;
  let form-cr = form.form-compilation-record;
  let base-lib = base-cr.compilation-record-original-library;
  let form-lib = form-cr.compilation-record-original-library;
  if (base-lib ~== form-lib)
    library-defined-after?(base-lib, form-lib)
  elseif (base-cr == form-cr)
    local method after?(base, form)
            let base-parent = base.form-parent-form;
            let form-parent = form.form-parent-form;
            if (base-parent == form-parent)
              base.form-sequence-number < form.form-sequence-number
            elseif (base == form-parent)
              #t
            elseif (base-parent == #f)
              after?(base, form-parent)
            else
              after?(base-parent, form)
            end
          end method;
    after?(base, form)
  else
    base-cr.compilation-record-sequence-number
      < form-cr.compilation-record-sequence-number
  end
end method;

define method defined-before? (base :: <top-level-form>,
                               form :: <top-level-form>) => before?;
  defined-after?(form, base)
end method;

// All non-definition top-level forms are classed as miscellaneous 
// initialisations.

define compiler-open generic retract-body-fragments (x);

define method retract-body-fragments (x)
end method;

define compiler-open generic form-body (form) => (body);
define compiler-open generic form-body-setter (value, form);


define dood-class <top-level-init-form> (<top-level-form>)
  lazy slot form-body,
    required-init-keyword: body:;
end;

define method retract-body-fragments (x :: <top-level-init-form>)
  next-method();
  form-body(x) := #f;
end method;

define method strip-incremental-slots (x :: <top-level-init-form>)
  next-method();
  retract-body-fragments(x);
end method;

// A top-level form which is a macro call.
// This exists only for browsers and is otherwise ignored by the compiler

define dood-class <macro-call-form> (<top-level-form>)
  // form-derived-forms must be lazy to prevent circularities in proxies
  // for booted compiler forms.
  lazy slot form-derived-forms :: <top-level-form-sequence>;
  constant slot form-define-word :: false-or(<symbol>),
    required-init-keyword: define-word:;
end;

define method form-top-level-methods 
    (form :: <macro-call-form>) => (method-models)
  #()
end method;

// A modified top-level-form has space for adjectives.

define /* abstract */ dood-class <modified-top-level-form> (<top-level-form>)
  lazy constant slot form-adjectives :: false-or(<sequence>),
    required-init-keyword: adjectives:;
end dood-class;

// A defining-form is something of the syntactic form "define ... end".
// It may be a macro, and so we may not know its semantics or whether
// it actually defines, or is associated with, a variable in any way. We
// can browse its adjectives, at least.

define /* abstract */ compiler-open class <defining-form> 
    (<modified-top-level-form>) 
end class;

define compiler-open form-properties <installable-form-properties> 
    (<top-level-form-properties>)
end;

define packed-slots form-properties-flags
    (<installable-form-properties>, <top-level-form-properties>)
  boolean slot form-top-level-installed? = #f;
end packed-slots;

define inline method shadowable-form-top-level-installed? 
    (form :: <installable-form-properties>) => (res :: <boolean>)
  // HACK: REALLY WANT THIS TO WORK WITH A TRANSPARENT ACCESSOR
  let offset :: <integer>
    // = packed-slots-end-count(<top-level-form-properties>);
    = $end-count-<top-level-form-properties>;
  logbit?(offset, shadowable-form-properties-flags(form))
end method;

// Special case... Override the automatically generated definition above,
// because need to default to #t, not #f, even though want the slot
// initialized to #f.
define method form-top-level-installed? (form :: <top-level-form>)
 => (well? :: <boolean>);
  ~form-top-level-installable?(form) |
    begin
      let p = form-properties-in-context
               (current-library-description(), form, #f);
      if (p)
        shadowable-form-top-level-installed?(p)
      else
        #t
      end
    end
end method;

define method form-top-level-installable? (form :: <installable-form-properties>) => (yes)
  #t
end method;


// A variable-defining form defines zero or more variables and installs
// their models in bindings.
define form-properties <variable-defining-form-properties>
    (<installable-form-properties>)
end;

define inline function pack-installed? (x) => (z :: <integer>)
  select (x)
    #f            => 0;
    #"processing" => 1;
    #t            => 2;
  end select;
end function;

define inline function unpack-installed? (x :: <integer>) => (res)
  select (x)
    0 => #f            ;
    1 => #"processing" ;
    2 => #t            ;
  end select;
end function;

define packed-slots form-properties-flags
    (<variable-defining-form-properties>, <installable-form-properties>)
  eval slot form-models-installed? = #f, field-size: 2,
    pack-function: pack-installed?, unpack-function: unpack-installed?;
end packed-slots;

/* abstract */
define compiler-open dood-class <variable-defining-form>
    (<defining-form>, <variable-defining-form-properties>)
  lazy constant slot form-variable-name-or-names 
                       :: type-union(<sequence>, <variable-name-fragment>),
    required-init-keyword: variable-name:;
  slot form-properties :: <integer> = 0;
end dood-class;

define inline function pack-inline-policy 
    (policy :: <symbol>) => (encoding :: <integer>)
  select (policy)
    #"default-inline" => 0;
    #"inline"         => 1;
    #"inline-only"    => 2;
    #"may-inline"     => 3;
    #"not-inline"     => 4;
  end select;
end function;

define inline function unpack-inline-policy
    (encoding :: <integer>) => (policy :: <symbol>)
  select (encoding)
    0 => #"default-inline";
    1 => #"inline"        ;
    2 => #"inline-only"   ;
    3 => #"may-inline"    ;
    4 => #"not-inline"    ;
  end select;
end function;

/*
define generic form-inline-policy 
    (form :: <top-level-form>) => (policy :: <symbol>);

define method form-inline-policy 
    (form :: <top-level-form>) => (policy :: <symbol>)
  #"default-inline"
end method;
*/

define packed-slots form-properties
    (<variable-defining-form>, <variable-defining-form-properties>)
  eval slot form-inline-policy = #"default-inline", 
    field-size: 3,
    pack-function: pack-inline-policy, 
    unpack-function: unpack-inline-policy,
    init-keyword: inline-policy:;
end packed-slots;

define compiler-open generic form-variable-names
    (form :: <variable-defining-form>) => (names :: <sequence>);

// This method must be overridden by multiple-binding forms
define method form-variable-names
    (form :: <variable-defining-form>) => (names :: <list>);
  let name-or-names = form-variable-name-or-names(form);
  debug-assert(instance?(name-or-names, <variable-name-fragment>));
  list(name-or-names);
end method;

// A convenience for the case where a form defines a single variable.

define compiler-open generic form-variable-name
    (form :: <variable-defining-form>) => (name :: <variable-name-fragment>);

// This method must be overridden by multiple-binding forms
define method form-variable-name
    (form :: <variable-defining-form>) => (name :: <variable-name-fragment>)
  form-variable-name-or-names(form)
end method;

define compiler-open generic model-variable-using-definition
  (form :: <variable-defining-form>, model-object) => (variable-name);

// This method must be overridden by multiple-binding forms
define method model-variable-using-definition
    (form :: <variable-defining-form>, model-object) => (variable-name)
  form-variable-name(form)
end;

/* abstract */
define compiler-open class <missing-variable-defining-form>
    (<variable-defining-form>)
end class;

define packed-slots form-properties 
    (<missing-variable-defining-form>, <variable-defining-form>)
end packed-slots;

define compiler-open generic form-compile-stage-only?
    (form :: <top-level-form>) => (compile-stage-only?);

define method form-compile-stage-only?
    (form :: <top-level-form>) => (compile-stage-only?)
  #f
end method;

define compiler-open generic form-implicitly-defined?
    (form :: <top-level-form>) => (implicit?);

define method form-implicitly-defined?
    (form :: <top-level-form>) => (implicit?)
  #f
end method;

// Queries as to whether the variable definition defines normal module
// variables or special variables of some kind to do with threads. 
// Default is normal.

define compiler-open generic form-thread? 
    (form :: <variable-defining-form>) => (result :: <boolean>);

define method form-thread? 
    (form :: <variable-defining-form>) => (result :: <boolean>)
  #f
end method;

define compiler-open generic form-locked? 
    (form :: <variable-defining-form>) => (result :: <boolean>);

define method form-locked? 
    (form :: <variable-defining-form>) => (result :: <boolean>)
  #f
end method;

define compiler-open generic form-atomic? 
    (form :: <variable-defining-form>) => (result :: <boolean>);

define method form-atomic? 
    (form :: <variable-defining-form>) => (result :: <boolean>)
  #f
end method;

// A modifying form is a modifying definition of its variable(s).
// Since it's not a primary definition, it must store its model somewhere
// other than the binding...
define form-properties <modifying-form-properties>
    (<variable-defining-form-properties>)
  lazy slot form-model = #f;
end;

define compiler-open generic form-model (form);
define compiler-open generic form-model-setter (model, form);

define /* abstract */ compiler-open class <modifying-form>
    (<variable-defining-form>, <modifying-form-properties>)
end;

define packed-slots form-properties (<modifying-form>, <variable-defining-form>)
end packed-slots;

// A typed-definition is a definition-form with space for an
// explicitly-declared type.

define /* abstract */ compiler-open dood-class 
    <explicitly-typed-variable-defining-form> (<variable-defining-form>)
  lazy constant slot form-type-expressions :: <sequence>,
    required-init-keyword: type-expressions:;
end dood-class;

define generic form-type-expressions
    (form :: <variable-defining-form>) => (expressions :: <sequence>);

define generic form-type-expression
    (form :: <variable-defining-form>, name :: <variable-name-fragment>) 
      => (expression :: <type-expression>);

// Namespace functions.

// The following functions are used during the boot to generate
// definitions for distinguished core libraries and modules.

define compiler-open generic define-parsed-library
    (name, #key source-location, use-clauses, create-clauses, export-clauses)
      => (library-definition, library);
define compiler-open generic define-parsed-module
    (name, #key source-location, use-clauses, create-clauses, export-clauses)
      => (module-definition, module);

//// Model computation.

// Variable-defining forms implement this method to compute and install
// the model objects for the variables they defined. A default method
// on compute-and-install-form-model-objects exists which calls 
// compute-form-model-object on each variable name and then stores the
// result if this is more convenient.

define compiler-open generic maybe-compute-and-install-form-model-objects
    (form :: <top-level-form>) => ();

define compiler-open generic finish-installing-form-model-objects
    (form :: <top-level-form>) => ();

define compiler-open generic compute-and-install-form-model-objects
    (form :: <top-level-form>) => ();

define compiler-open generic compute-form-model-object
    (form :: <variable-defining-form>, name :: <variable-name-fragment>)
      => (model-object);

// Code computation.

define compiler-open generic compute-and-install-form-dfm
    (form :: <variable-defining-form>) => ();  

define method compute-and-install-form-dfm
    (form :: <variable-defining-form>) => ()
  // Nothing needs doing by default.
end method;

//// Incremental/loose compilation.

// In the context of the current compilation, should the form in question
// be compiled statically or expanded to code to create the corresponding
// object dynamically?

define compiler-open generic form-dynamic? 
    (form :: <top-level-form>) => (loose? :: <boolean>);

define compiler-open generic form-binding-guaranteed-initialized?
    (form :: <variable-defining-form>) => (always? :: <boolean>);

define method form-binding-guaranteed-initialized?
    (form :: <variable-defining-form>) => (always? :: <boolean>)
  #f
end method;

// Make a "hollow" model object for static binding that will be filled
// in at run time.

define compiler-open generic compute-form-hollow-object
    (form :: <top-level-form>) => (hollow-model);

//// Interactive compilation.

define compiler-open generic form-incremental?
    (form :: <top-level-form>) => (well? :: <boolean>);

define compiler-open generic form-redefinition?
    (form :: <top-level-form>) => (well? :: <boolean>);
