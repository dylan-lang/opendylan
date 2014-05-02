Module: dfmc-definitions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// The boot framework.

// The boot-record records the set of things that must be inserted into
// a Dylan world at the very start. Some things are core definitions
// such as converters and macros, and these are booted at the definition
// level. The rest are expressed as source to be fed to the compiler. 

define class <booted-set> (<object>)
  constant slot booted-set-keys :: <table> 
    = make(<table>);
  constant slot booted-set-order :: <stretchy-vector>
    = make(<stretchy-vector>);
end class;

define method register-thunk-in (set :: <booted-set>, thunk) => ()
  // If it exists already, place where it was before. Otherwise append
  let key-table = booted-set-keys(set);
  let order = booted-set-order(set);
  let key = thunk-name(thunk);
  let posn = element(key-table, key, default: #f);
  if (posn)
    // signal("Redefining thunk for %=.", key);
    order[posn] := thunk;
  else
    element(key-table, key) := size(order);
    add!(order, thunk);
    thunk
  end;
end method;

define class <boot-record> (<object>)
  constant slot booted-namespace-thunks :: <booted-set> = make(<booted-set>);
  constant slot booted-expander-thunks :: <booted-set> = make(<booted-set>);
  constant slot booted-definition-thunks :: <booted-set> = make(<booted-set>);
  constant slot booted-source-thunks :: <booted-set> = make(<booted-set>);
  slot booted-constant-names :: <deque> = make(<deque>);
end class;

define abstract class <boot-thunk> (<object>)
  constant slot thunk-name,
    required-init-keyword: name:;
end class;

define class <definition-thunk> (<boot-thunk>)
  constant slot thunk-definition-constructor,
    required-init-keyword: definition-constructor:;
end class;

define method thunk-definition (thunk :: <definition-thunk>)
  thunk-definition-constructor(thunk)()
end method;

define class <source-thunk> (<boot-thunk>)
  constant slot thunk-source-constructor,
    required-init-keyword: source-constructor:;
end class;

define constant *boot-record* = make(<boot-record>);

define method register-definition-thunk (booted-thunks, thunk) => ()
  let record = *boot-record*;
  register-thunk-in(record.booted-thunks, thunk);
end method;

define method register-source-thunk (thunk) => ()
  let record = *boot-record*;
  register-thunk-in(record.booted-source-thunks, thunk);
end method;

define function find-definition-thunk (booted-thunks, name) => (thunk)
  let record = *boot-record*;
  let pos = element(record.booted-thunks.booted-set-keys, name, default: #f);
  pos & element(record.booted-thunks.booted-set-order, pos)
end function;

define method register-constant-variable-name (name) => ()
  let record = *boot-record*;
  booted-constant-names(record)
    := add-new!(booted-constant-names(record), name);
end method;

define method booted-constant-definitions () => (definitions)
  map(dylan-definition, booted-constant-names(*boot-record*));
end method;

define method booted-source-sequence () => (sources)
  let record = *boot-record*;
  map-as(<vector>, 
         method (thunk) thunk-source-constructor(thunk)() end, 
         record.booted-source-thunks.booted-set-order)
end method;

define method booted-definition-sequence () => (definitions)
  let record = *boot-record*;
  local method definitions-of (thunks)
          map-as(<list>, thunk-definition, thunks.booted-set-order)
        end;
  local method installed-definitions-of (thunks)
	  map-as(<list>, method (thunk)
			   let form = thunk-definition(thunk);
			   install-top-level-form(form);
			   form
			 end,
		 thunks.booted-set-order)
	end;
  concatenate!(definitions-of(record.booted-namespace-thunks),
               installed-definitions-of(record.booted-expander-thunks),
               definitions-of(record.booted-definition-thunks))
end method;

//// Booted definitions

//// Database support

// The class of forms which are always defined in the compiler image rather
// than being stored in a database.
// Not every form which is booted via the boot record is a
// <compiler-booted-form>, only ones which contain stuff that can't be stored
// in a database.  For example namespaces and booted constant definitions
// are stored in the database in the ordinary way.
define class <compiler-booted-form> (<top-level-form>)
  constant slot booted-form-boot-id :: <symbol>,
    required-init-keyword: boot-id:;
end class;

define class <dood-booted-form-proxy> (<dood-proxy>)
  constant slot dood-proxy-booted-form-boot-id :: <symbol>,
    required-init-keyword: boot-id:;
  constant slot dood-proxy-booted-form-parent :: <top-level-form>,
    required-init-keyword: parent:;
  constant slot dood-proxy-booted-form-sequence-number :: <integer>,
    required-init-keyword: sequence-number:;
end class;

define method dood-make-booted-form-proxy
    (dood :: <dood>, form :: <compiler-booted-form>) => (proxy)
  make(<dood-booted-form-proxy>,
       boot-id: form.booted-form-boot-id,
       parent: form.form-parent-form,
       sequence-number: form.form-sequence-number);
end method;

define method dood-disk-object
    (dood :: <dood>, form :: <compiler-booted-form>)
 => (proxy :: <dood-booted-form-proxy>)
  assert(dylan-library-library-description?(dood.dood-root));
  dood-as-proxy(dood, form, dood-make-booted-form-proxy)
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-booted-form-proxy>) => (object)
  dood-restore-compiler-booted-proxy(dood, proxy, booted-definition-thunks)
end method;

define function dood-restore-compiler-booted-proxy
    (dood :: <dood>, proxy :: <dood-booted-form-proxy>, booted-thunks)
  let thunk = find-definition-thunk(booted-thunks,
				    proxy.dood-proxy-booted-form-boot-id);
  assert(thunk, "Dylan database/compiler version mismatch");
  let sequence-number = proxy.dood-proxy-booted-form-sequence-number;
  let parent = proxy.dood-proxy-booted-form-parent;
  with-dood-context (dood-root(dood))
    with-boot-form-creation (sequence-number of parent)
      let form = thunk-definition(thunk);
      form.form-parent-form := parent;
      // TODO: Really should preserve who-calls info in the DB.
      // For now, at least ensure dependency tracking knows the dep info is gone
      form.form-stripped? := #t;
      // form.form-dependencies := proxy....
      // form.form-referenced-variables := proxy....
      form
    end with-boot-form-creation;
  end with-dood-context;
end function;

// Boot definitions

define method do-define-core-instance (boot-name, class-name, value)
  local method boot-instance ()
    define-booted-constant
      (make-variable-name-fragment(boot-name),
       make-variable-name-fragment(class-name),
       value);
  end;
  register-constant-variable-name(boot-name);
  register-definition-thunk
    (booted-definition-thunks,
     make(<definition-thunk>,
          name: boot-name,
          definition-constructor: boot-instance));
end method;

define method do-define-core-library (boot-name, name, clauses)
  local method boot-library ()
    apply(define-parsed-library, name, clauses);
  end;
  register-definition-thunk
    (booted-namespace-thunks,
     make(<definition-thunk>, 
          name: boot-name,
          definition-constructor: boot-library));
  local method boot-library-model ()
    let library    = dylan-library();
    let definition = namespace-definition(library);
    generate-initializer-source-with-namespace(definition, library)
  end;
  register-source-thunk
    (make(<source-thunk>, 
          name: boot-name,
          source-constructor: boot-library-model));
end method;

define method do-define-core-module (boot-name, name, clauses)
  local method boot-module ()
    apply(define-parsed-module, name, clauses)
  end;
  register-definition-thunk
    (booted-namespace-thunks,
     make(<definition-thunk>, 
          name: boot-name,
          definition-constructor: boot-module));
  local method boot-module-model ()
    let module     = lookup-module-in(dylan-library(), name);
    let definition = namespace-definition(module);
    generate-initializer-source-with-namespace(definition, module)
  end;
  register-source-thunk
    (make(<source-thunk>, 
          name: boot-name,
          source-constructor: boot-module-model));
end method;

define dood-class <expander-defining-form> (<variable-defining-form>) /* abstract */
  weak slot form-macro-object,
    required-init-keyword: macro-object:;
  weak slot form-expander,
    required-init-keyword: expander:;
  // set by initialize method.
  lazy slot form-macro-word;
  lazy slot form-macro-word-class;
end;

define method form-compile-stage-only?
    (form :: <expander-defining-form>) => (compile-stage-only?)
  #t
end method;


define method initialize (definition :: <expander-defining-form>, #key)
  next-method();
  let compiled-macro = form-macro-object(definition);
  let (word, word-class) 
    = macro-word-in-variable-name
        (compiled-macro, form-variable-name(definition).fragment-identifier);
  definition.form-macro-word := word;
  definition.form-macro-word-class := word-class;
end method;

define /* abstract */ class <&definition>
    (<expander-defining-form>, <compiler-booted-form>)
end;

define class <dood-booted-expander-form-proxy> (<dood-booted-form-proxy>) end;

define method dood-make-booted-form-proxy
    (dood :: <dood>, form :: <&definition>) => (proxy)    
  make(<dood-booted-expander-form-proxy>,
       boot-id: form.booted-form-boot-id,
       parent: form.form-parent-form,
       sequence-number: form.form-sequence-number);
end method;

define method dood-restore-proxy
    (dood :: <dood>, proxy :: <dood-booted-expander-form-proxy>) => (object)
  dood-restore-compiler-booted-proxy(dood, proxy, booted-expander-thunks)
end method;

define method do-define-core-entity 
    (name, klass, word, word-class, macro-object, expander) => ()
  local method definition-constructor ()
	  make(klass,
	       boot-id:          name,
	       source-location:  #f,
	       adjectives:       #(),
	       variable-name:    make-variable-name-fragment(name),
	       macro-object:     macro-object,
	       expander:         expander)
        end;
  register-definition-thunk
    (booted-expander-thunks,
     make(<definition-thunk>, 
          name: name, 
          definition-constructor: definition-constructor));
end method;

define class <&macro-definition> (<&definition>) end;

define method initialize (definition :: <&macro-definition>, #key)
  next-method();
  // TODO: If it may use procedural templates, ensure that the result
  // is a parsed body in the correct hygiene context. This is a hygiene
  // workaround.
  form-expander(definition) := compose(as-body, form-expander(definition));
end method;

define method do-define-core-macro 
    (name, word, word-class, macro-object, expander) => ()
  do-define-core-entity
    (name, <&macro-definition>, word, word-class,
       macro-object, expander);
end method;

define class <&converter-definition> (<&definition>) end;

define method do-define-core-converter 
    (name, word, word-class, macro-object, expander) => ()
  do-define-core-entity
    (name, <&converter-definition>, word, word-class, macro-object, expander);
end method;

define class <&definition-definition> (<&definition>) end;

define method do-define-core-definition 
    (name, word, word-class, macro-object, expander) => ()
  do-define-core-entity
    (name, <&definition-definition>, 
       word, word-class, macro-object, expander);
end method;

define constant *evaluator-overrides* = make(<table>);

define method do-define-evaluator-override (name, function) => ()
  *evaluator-overrides*[name] := function;
end;

// Open so can map iep -> function in modeling.
define compiler-open generic lookup-compile-stage-function (model-object)
 => (function :: false-or(<function>));

define method lookup-compile-stage-function (accessor)
 => (function :: false-or(<function>))
  lookup-override-function(*evaluator-overrides*, accessor)
end method;

define method booted-module? (module :: <dylan-user-module>)
  #f
end;

define method booted-module? (module :: <module>)
  let cr = form-compilation-record(namespace-definition(module));
  let ld = compilation-record-library(cr);
  dylan-library-library-description?(ld)
    & cr == ld.compilation-context-records[0]
end;


define method lookup-override-function
    (table :: <object-table>, binding :: <module-binding>)
 => (fn :: false-or(<function>))
  booted-module?(binding.binding-home)
    & element(table, binding.binding-identifier, default: #f)
end;

define method lookup-override-function
    (table :: <object-table>, model :: <object>)
 => (fn :: false-or(<function>))
  when (model)
    let binding = model-variable-binding(model);
    binding & lookup-override-function(table, binding);
  end;
end;

//// Booted primitives

define method maybe-blat-model-value (defn :: <variable-defining-form>)
  let binding = form-variable-binding(defn);
  binding & untracked-retract-binding-model-object(binding)
end method;

define method do-define-core-primitive (name, adjectives, value, signature)
  let initargs = parse-primitive-adjectives(name, adjectives);
  let variable = make-variable-name-fragment(name);
  local method primitive-constructor ()
          let definition =
	    apply(make, <primitive-definition>,
		  boot-id: name,
		  source-location: #f,
		  variable-name:   variable,
		  adjectives:      adjectives,
		  signature:       signature,
		  value:           value,
		  initargs);
          maybe-blat-model-value(definition);
          definition
	end;
  register-definition-thunk
    (booted-definition-thunks,
     make(<definition-thunk>,
	  name: name,
	  definition-constructor: primitive-constructor));
end method;

//// The top type

define /* abstract */ class <virtual-type-definition> (<variable-defining-form>)
end class;

define method form-compile-stage-only?
    (form :: <virtual-type-definition>) => (compile-stage-only?)
  #t
end method;

define class <top-type-definition> (<virtual-type-definition>)
end class;

define function do-define-top-type (name)
  local method definition-constructor ()
    make(<top-type-definition>,
         source-location:  #f,
         variable-name:    make-variable-name-fragment(name),
         adjectives:       #());
  end;
  register-definition-thunk
    (booted-definition-thunks,
     make(<definition-thunk>, name: name, 
          definition-constructor: definition-constructor));
end;

//// The bottom type

define class <bottom-type-definition> (<variable-defining-form>)
end class;

define function do-define-bottom-type (name)
  local method definition-constructor ()
    make(<bottom-type-definition>,
         source-location:  #f,
         variable-name:   make-variable-name-fragment(name),
         adjectives:       #());
  end;
  register-definition-thunk
    (booted-definition-thunks,
     make(<definition-thunk>, name: name, 
          definition-constructor: definition-constructor));
end;


//// Booted raw-types

define class <raw-type-definition>
    (<virtual-type-definition>, <compiler-booted-form>)
  constant slot form-supertype-name, required-init-keyword: supertype-name:;
  constant slot form-raw-type-descriptor-function :: <function>,
    required-init-keyword: descriptor-function:;
end class;

define function do-define-raw-type (name, supertype-name, descriptor-function)
  local method definition-constructor ()
    let definition =
      make(<raw-type-definition>,
	   boot-id:             name,
	   source-location:     #f,
	   variable-name:       make-variable-name-fragment(name),
	   supertype-name:      supertype-name &
				  make-variable-name-fragment(supertype-name),
	   descriptor-function: descriptor-function,
	   adjectives:          #());
    maybe-blat-model-value(definition);
    definition
  end;
  register-definition-thunk
    (booted-definition-thunks,
     make(<definition-thunk>, name: name, 
          definition-constructor: definition-constructor));
end;

//// Booted classes.

define method do-define-core-unadorned-definition (name, constructor :: <function>) => ()
  register-source-thunk
    (make(<source-thunk>, name: name, source-constructor: constructor));
end method;

//// for raw structs
// it's not really booted, but it's closer to anything her than anything else

define /* abstract */ class <raw-aggregate-definition> (<virtual-type-definition>)
  constant slot form-members, required-init-keyword: members:;
  constant slot form-options, required-init-keyword: options:;
end;  



define class <raw-struct-definition> (<raw-aggregate-definition>)
end class;


define &definition raw-struct-type-definer
  { define raw-struct-type ?:name ?root-name:name (?options:*)
      ?members:*
  end}
  => do-define-raw-aggregate
       (<raw-struct-definition>, name, root-name, members, options);
members:
  { } => #();
  { ?member:*; ...} => pair(member, ...);
member:
  { member ?:name } => list(#"member", name);
  { bitfield-member ?:name ?width:expression } 
    => list(#"bitfield-member", name, width);
  { array-member ?:name ?length:expression }
    => list(#"array-member", name, length);
options:
  { } => #();
  { ?option:expression, ... } => pair(option, ...);
end;


define function do-define-raw-aggregate
    (class, name, root-name, member-fragments, options)
  list(make(class,
	    source-location: fragment-source-location(root-name),
	    variable-name: name,
	    adjectives: #(),
	    members: member-fragments,
	    options: options));
end;


define class <raw-union-definition> (<raw-aggregate-definition>)
end class;


define &definition raw-union-type-definer
  { define raw-union-type ?:name ?root-name:name (?options:*)
      ?members:*
  end}
  => do-define-raw-aggregate
       (<raw-union-definition>, name, root-name, members, options);
members:
  { } => #();
  { ?member:*; ...} => pair(member, ...);
member:
  { member ?:name } => list(#"member", name);
  { array-member ?:name ?length:expression }
    => list(#"array-member", name, length);
options:
  { } => #();
  { ?option:expression, ... } => pair(option, ...);
end;
