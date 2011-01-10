Module:  dfmc-definitions
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *debug-dependencies* = #f;
define variable *trace-dependencies* = #f;
define variable *trace-retraction* = #f;

// A dependency stage and kind
define constant <dep-condition> = limited(<integer>, min: 0, max: stage$all);

// A set of dependency conditions
define constant <dep-conditions> = <dep-condition>;

// A set of dependency kinds
define constant <kind-mask> = limited(<integer>, min: 0, max: dep$all);

// A set of dependency stages
// Represented as set of conditions corresponding to dep$all in all the
// included stages.
define constant <stage-mask> = <dep-conditions>;

define class <referenced-variable> (<variable-name>, <dood-dfmc-object>)
  constant slot referenced-variable-name :: <symbol>,
    required-init-keyword: name:;
  constant slot referenced-variable-module :: <module>,
    required-init-keyword: module:;
end class;

define sealed domain make (subclass(<referenced-variable>));
define sealed domain initialize (<referenced-variable>);

define constant dependency-name   = referenced-variable-name;
define constant dependency-module = referenced-variable-module;

define class <name-dependency> (<referenced-variable>)
  slot name-dependency-conditions :: <dep-conditions>,
    required-init-keyword: conditions:;
end class;

define sealed domain make (subclass(<name-dependency>));
define sealed domain initialize (<name-dependency>);

define constant <qualified-entry>
  //  name dep or list of them.  Later could have other qualified deps.
  = type-union(<name-dependency>, <list>);

define class <binding-dependency> (<dood-dfmc-object>)
  slot binding-dependency-conditions :: <dep-conditions> = 0,
    init-keyword: conditions:;
  slot binding-qualified-dependencies :: <qualified-entry> = #(),
    init-keyword: qualified:;
end class;

define sealed domain make (subclass(<binding-dependency>));
define sealed domain initialize (<binding-dependency>);

define dood-class <form-binding-dependency> (<binding-dependency>)
  constant slot dependency-binding :: <module-binding>,
    required-init-keyword: binding:;
  lazy slot binding-dependency-next :: false-or(<form-binding-dependency>),
    required-init-keyword: next:;
end;

define sideways method note-binding-dependency-of
    (cr :: <compilation-record>,
     condition :: <dep-condition>,
     binding :: <module-binding>)
  unless (current-library-stripable?())
    // Don't bother recording dependencies on primary definitions in
    // other libraries - if they are tightly bound, any change will cause
    // full recompilation.  If they are loosely bound, we will ignore any
    // dependencies...
    let kind = dependency-conditions-kind(condition);
    unless ((kind == dep$active-definition | kind == dep$defined?) &
	      binding-imported-into-library?(binding))
      let entry = compilation-record-binding-dependencies(cr, binding);
      let new = add-binding-dependency(entry, condition);
      if (new) compilation-record-dependency-table(cr)[binding] := new end;
    end;
  end;
end method;

define sideways method note-name-dependency-of
    (cr :: <compilation-record>,
     condition :: <dep-condition>,
     binding :: <module-binding>,
     name :: <symbol>,
     module :: <module>)
  unless (current-library-stripable?())
    let entry = compilation-record-binding-dependencies(cr, binding);
    let new = add-name-dependency(entry, name, module, condition);
    if (new) compilation-record-dependency-table(cr)[binding] := new end;
  end;
end method;

define sideways method note-binding-dependency-of
    (form :: <top-level-form>,
     condition :: <dep-condition>,
     binding :: <module-binding>)
  unless (current-library-stripable?() | form.form-stripped?)
    // Don't bother recording dependencies on primary definitions in
    // other libraries - if they are tightly bound, any change will cause
    // full recompilation.  If they are loosely bound, we will ignore any
    // dependencies...
    let kind = dependency-conditions-kind(condition);
    unless ((kind == dep$active-definition | kind == dep$defined?) &
	      binding-imported-into-library?(binding))
      let entry = form-binding-dependencies(form, binding);
      add-binding-dependency(entry, condition);
    end;
  end;
end method;

define sideways method note-name-dependency-of
    (form :: <top-level-form>,
     condition :: <dep-condition>,
     binding :: <module-binding>,
     name :: <symbol>,
     module :: <module>)
  unless (form.form-stripped? |
	    (current-library-stripable?() &
	       (dep-conditions-stages(condition, dep$refs) == 0)))
    let entry = form-binding-dependencies(form, binding);
    add-name-dependency(entry, name, module, condition);
  end;
end method;

/*
define sideways method old-strip-incremental-slots (form :: <top-level-form>)
  iterate loop 
       (last = #f, 
	dep :: false-or(<form-binding-dependency>) 
	  = form.form-dependencies)
    if (dep)
      let next       = dep.binding-dependency-next;
      let entry      = dep.binding-qualified-dependencies;
      let refs-entry = matching-name-entry(entry, dep$refs);
      if (refs-entry == #())
	let binding = dep.dependency-binding;
	unregister-binding-dependent(binding, form);
	if (last)
	  last.binding-dependency-next := next
	else
	  form.form-dependencies := next
	end;
	loop(last, next)
      else
	dep.binding-qualified-dependencies := refs-entry;
	loop(dep, next)
      end if;
    end if;
  end iterate;
end method;
*/

define inline method form-referenced-binding-variables-from
    (references :: <simple-object-vector>) => (res :: <integer>)
  1
end method;

define inline method form-referenced-binding-variables-below
    (references :: <simple-object-vector>) => (res :: <integer>)
  references[0]
end method;

define inline method form-referenced-macro-variables-from
    (references :: <simple-object-vector>) => (res :: <integer>)
  references[0]
end method;

define inline method form-referenced-macro-variables-below
    (references :: <simple-object-vector>) => (res :: <integer>)
  size(references)
end method;

define sideways method strip-incremental-slots // dependencies-as-referenced-variables 
    (form :: <top-level-form>)
  unless (form-stripped?(form))
    let macro-references :: <list>   = #();
    let binding-references :: <list> = #();
    iterate loop 
	 (last = #f, 
	  dep :: false-or(<form-binding-dependency>) = form.form-dependencies)
      local method add-to-references 
                (refs :: <list>, dep :: <referenced-variable>)
              let name   = dependency-name(dep);
              let module = dependency-module(dep);
              iterate find-module (elt :: <list> = refs)
                case 
                  empty?(elt) =>
                    add(add(refs, name), module);
                  head(elt) == module =>
                    tail(elt) := add(tail(elt), name);
                    refs;
                  otherwise =>
                    find-module(tail(elt));
                end case;
              end iterate;
            end method,
            method collect-reference (dep :: <name-dependency>)
	      if (dependency-matching-kinds?(dep, dep$name-macro-ref))
                macro-references
                  := add-to-references(macro-references, dep)
	      else
                binding-references
                  := add-to-references(binding-references, dep)
	      end if
	    end method;
      if (dep)
	let next       = dep.binding-dependency-next;
	let entry      = dep.binding-qualified-dependencies;
	let refs-entry = matching-name-entry(entry, dep$refs);
	if (refs-entry == #())
	  let binding = dep.dependency-binding;
	  unregister-binding-dependent(binding, form);
	  if (last)
	    last.binding-dependency-next := next
	  else
	    form.form-dependencies := next
	  end;
	  loop(last, next)
	else
	  dep.binding-qualified-dependencies := refs-entry;
	  if (instance?(refs-entry, <list>))
	    for (e in refs-entry)
	      collect-reference(e)
	    end for;
	  else
	    collect-reference(refs-entry)
	  end if;
	  loop(dep, next)
	end if;
      end if;
    end iterate;
    form.form-referenced-variables 
      := concatenate(vector(size(binding-references)),
                     as(<vector>, binding-references),
                     as(<vector>, macro-references));
    form.form-dependencies         := #f;
    form.form-stripped?            := #t;
  end unless;
end method;

define inline method form-referenced-variables-between
    (references :: <simple-object-vector>, from :: <integer>, below :: <integer>)
 => (refs :: false-or(<list>))
  collecting ()
    iterate search
	(i :: <integer> = from, module :: false-or(<module>) = #f)
      when (i < below)
	let elt = references[i];
	if (instance?(elt, <module>))
	  search(i + 1, elt)
	else
	  collect(make(<referenced-variable>, name: elt, module: module));
	  search(i + 1, module)
	end if
      end when
    end iterate
  end collecting;
end method;

define method form-referenced-binding-variables 
    (form) => (refs :: false-or(<list>))
  let references = form-referenced-variables(form);
  references &
    form-referenced-variables-between
      (references,
       form-referenced-binding-variables-from(references),
       form-referenced-binding-variables-below(references));
end method;

define method form-referenced-macro-variables 
    (form) => (refs :: false-or(<list>))
  let references = form-referenced-variables(form);
  references &
    form-referenced-variables-between
      (references,
       form-referenced-macro-variables-from(references),
       form-referenced-macro-variables-below(references));
end method;

define variable *heap-statistics?* = #f;

define sideways method strip-incremental-slots (cr :: <compilation-record>)
  // compilation-record-dependency-table(cr) := make(<table>);
  let table = cr.compilation-record-dependency-table;
  unless (empty?(table))
    for (entry keyed-by binding in table)
      unregister-binding-dependent(binding, cr);
    end;
    cr.compilation-record-dependency-table := make(<table>);
  end;
  unless (*heap-statistics?*)
    if (compilation-record-model-heap(cr))
      strip-incremental-slots(compilation-record-model-heap(cr));
      compilation-record-model-heap(cr) := #f;
    end;
  end unless;
end method;


define method add-binding-dependency (entry :: <qualified-entry>,
				      condition :: <dep-condition>)
  make(<binding-dependency>, conditions: condition, qualified: entry);
end;

define method add-binding-dependency (entry :: <binding-dependency>,
				      condition :: <dep-condition>)
  entry.binding-dependency-conditions
    := add-dependency-condition(entry.binding-dependency-conditions,
				condition);
  #f
end method;

define method add-name-dependency (entry :: <qualified-entry>,
				   name :: <symbol>,
				   module :: <module>,
				   condition :: <dep-condition>)
  local method new (name, module, condition)
                make(<name-dependency>,
                     name: name, module: module, conditions: condition)
               end;
  local method add-if (dep :: <name-dependency>)
                if (dep.dependency-name == name &
                      dep.dependency-module == module)
                  let conditions = dep.name-dependency-conditions;
		  dep.name-dependency-conditions
		    := add-dependency-condition(conditions, condition);
		  #t
                end;
              end;
  if (entry == #())
    new(name, module, condition);
  elseif (instance?(entry, <list>))
    unless (any?(add-if, entry)) pair(new(name, module, condition), entry) end;
  else
    unless (add-if(entry)) list(new(name, module, condition), entry) end;
  end
end method;

define method add-name-dependency (entry :: <binding-dependency>,
				   name :: <symbol>,
				   module :: <module>,
				   condition :: <dep-condition>)
  let deps = entry.binding-qualified-dependencies;
  let new = add-name-dependency(deps, name, module, condition);
  if (new) entry.binding-qualified-dependencies := new end;
  #f
end method;

define function compilation-record-binding-dependencies
    (cr :: <compilation-record>,
     binding :: <module-binding>)
  let table = compilation-record-dependency-table(cr);
  let table-entry = element(table, binding, default: #f);
  if (table-entry == #f)
    register-binding-dependent(binding, cr);
    #()
  else
    table-entry
  end;
end function;

define function lookup-form-binding-dependencies (form, binding)
  block (return)
    for (dep = form.form-dependencies then dep.binding-dependency-next,
	 while: dep)
      if (dep.dependency-binding == binding) return(dep) end;
    end for;
    #f
  end
end function;

define function form-binding-dependencies 
    (form, binding) => (result :: <form-binding-dependency>)
  lookup-form-binding-dependencies(form, binding)
    | begin
	let new = make(<form-binding-dependency>,
		       binding: binding,
		       next: form.form-dependencies);
	register-binding-dependent(binding, form);
	form.form-dependencies := new;
	new
      end;
end function;

// dependent retraction

define macro with-dependent-retraction
  { with-dependent-retraction ?:body end }
    => { do-with-dependent-retraction(method () ?body end) }
end macro;

define thread variable *dependents-being-retracted* = #f;

define inline function do-with-dependent-retraction (fn)
  if (*dependents-being-retracted*)
    fn()
  else
    dynamic-bind (*dependents-being-retracted* = make(<table>))
      fn()
    end
  end
end function;

define function stage-being-retracted? (dependent, mask :: <stage-mask>)
  let table = *dependents-being-retracted*;
  table & logand(element(table, dependent, default: 0), mask) ~== 0
end function;

define function stage-being-retracted?-setter (flag, dependent, mask :: <stage-mask>)
  let table = *dependents-being-retracted*;
  let old = element(table, dependent, default: 0);
  if (flag)
    table[dependent] := logior(old, mask);
  else
    let new = logand(old, lognot(mask));
    if (new == 0) remove-key!(table, dependent);
    else table[dependent] := new end;
  end;
end function;

// retract stages
// Make it as if the record was never processed.
define function retract-compilation-record (cr :: <compilation-record>)
  retract-record-top-level-processing(cr);
end function;

// Make it as if the form was never processed.  Must be a method due to
// package screws.
define sideways method retract-top-level-form (form :: <top-level-form>) => ()
  retract-form-top-level-processing(form)
end method;

define method retract-dependent-stages (cr :: <compilation-record>,
					stages :: <stage-mask>)
  if (*debug-dependencies*)
    if (*current-dependent* &
	cr == current-compilation-record() &
	dependency-stage-match?(*current-stage*, stages))
      error("Retracting %s of %s\n", dependency-conditions-name(stages), cr);
    end;
  end;
  if (*trace-dependencies*)
    format-out("Retracting %s of %s\n", dependency-conditions-name(stages), cr);
  end if;
  if (logand(stages, $compilation-mask) ~== 0)
    retract-record-compilation(cr);
  end;
  if (logand(stages, $top-level-processing-mask) ~== 0)
    retract-record-top-level-processing(cr);
  end;
  debug-assert(stage$count == 2);
end method;

define method retract-dependent-stages (form :: <top-level-form>,
					stages :: <stage-mask>)
  if (*debug-dependencies* | *trace-dependencies*)
    if (form == *current-dependent* &
	dependency-stage-match?(*current-stage*, stages))
      format-out(" ** Retracting current dependent %s/%s\n", dependency-conditions-name(stages), form);
    elseif (*trace-dependencies*)
      format-out("Retracting %s of %s\n", dependency-conditions-name(stages), form);
    end;
  end;
  if (logand(stages, $compilation-mask) ~== 0)
    retract-form-compilation(form);
  end if;
  if (logand(stages, $top-level-processing-mask) ~== 0)
    retract-form-top-level-processing(form);
  end if;
  debug-assert(stage$count == 2);
end method;

define function retract-compilation-record-models (cr :: <compilation-record>)
  clear-dependent-model-properties(cr);
  remove-dependent-program-conditions(cr, $compilation-mask);
  retract-compilation-record-dependencies(cr, $compilation-mask);
  clear-compilation-record-caches(cr);
  retract-compilation-record-heap(cr);
end function;

// Record compilation means heaping and linking.
// TODO: CORRECTNESS: now that emitting is incremental, should this
// undo emission?
define function retract-record-compilation (cr :: <compilation-record>)
  if (cr.compilation-record-model-heap)
    if (*trace-retraction*)
      format-out("Retracting COMP %s due to %s\n", cr, *trace-retraction*);
      *trace-retraction* := #t;
    end;
    retract-compilation-record-models(cr);
  elseif (*debug-dependencies*)
    // make sure retracting dependencies would be a noop
    for (entry keyed-by binding in cr.compilation-record-dependency-table)
      let new-entry = remove-dependency-stages(entry, $compilation-mask);
      debug-assert(new-entry == entry);
    end;
    debug-assert(begin // Make sure retracting conditions would be a noop
		   let q = element(cr.compilation-record-library.library-conditions-table,
				   cr, default: not-found());
		   not-found?(q)
		     | every?(method (c)
				c.condition-compilation-stage ~== $compilation
			      end, q)
		 end);
  end;
end function;

define function retract-record-top-level-processing (cr :: <compilation-record>)
  retract-record-compilation(cr);
  if (cr.compilation-record-top-level-forms & // not already retracted...
	~stage-being-retracted?(cr, $top-level-processing-mask))
    if (*trace-retraction*)
      format-out("Retracting TLP %s due to %s\n", cr, *trace-retraction*);
      *trace-retraction* := #t;
    end;
    stage-being-retracted?(cr, $top-level-processing-mask) := #t;
    let ld = cr.compilation-record-library;
    for (form :: <top-level-form> in cr.compilation-record-top-level-forms,
	 while: cr.compilation-record-top-level-forms)
      retract-form-compilation(form);
      if (form-top-level-installable?(form) & form-top-level-installed?(form) &
	    ~stage-being-retracted?(form, $top-level-processing-mask))
	stage-being-retracted?(form, $top-level-processing-mask) := #t;
	ld.compiled-to-definitions? := #f;
	cr.compilation-record-definitions-installed? := #f;
	// TODO: CORRECTNESS: this doesn't flush implicitly defined forms,
	// since they all get flushed at the end.  But what if we abort before
	// then?  Maybe need with-inconsistent-definitions around whole thing,
	// except would be nice to leave compilation retraction out of it.
	// Better yet if flushing implicit definitions wasn't so expensive,
	// so we could just go ahead and do it here...
	with-inconsistent-definitions (ld)
	  uninstall-top-level-form(form);
	  remove-dependent-program-conditions(form, $top-level-processing-mask);
	  while (form.form-dependencies)
	    let dep = form.form-dependencies;
	    unregister-binding-dependent(dep.dependency-binding, form);
	    form.form-dependencies := dep.binding-dependency-next;
	  end;
	end with-inconsistent-definitions;
	stage-being-retracted?(form, $top-level-processing-mask) := #f;
      end if;
    end for;
    with-inconsistent-definitions (ld)
      remove-dependent-program-conditions(cr, $top-level-processing-mask);
      let table = cr.compilation-record-dependency-table;
      for (entry keyed-by binding in table)
	unregister-binding-dependent(binding, cr);
      end;
      remove-all-keys!(table);
      // Force recomputation of module, even if no forms got uninstalled.
      ld.compiled-to-definitions? := #f;
      cr.compilation-record-definitions-installed? := #f;
      cr.compilation-record-module := #f;
      cr.compilation-record-top-level-forms := #f;
    end with-inconsistent-definitions;
    stage-being-retracted?(cr, $top-level-processing-mask) := #f;
  end if;
end function;

define function retract-form-top-level-processing (form :: <top-level-form>)
  if (*debug-dependencies*)
    debug-assert(~(instance?(form, <variable-defining-form>)
		     & form-models-installed?(form)),
		 "Trying to retract installed definition!");
  end;
  if (*trace-dependencies*)
    format-out("  installed? = %s, being-retracted? = %s\n",
	       form-top-level-installable?(form) & form-top-level-installed?(form),
	       stage-being-retracted?(form, $top-level-processing-mask));
  end;
  retract-form-compilation(form);
  if (form-top-level-installable?(form) & form-top-level-installed?(form) &
	~stage-being-retracted?(form, $top-level-processing-mask))
    if (*trace-retraction*)
      format-out("Retracting TLP %s due to %s\n", form, *trace-retraction*);
      *trace-retraction* := #t;
    end;
    stage-being-retracted?(form, $top-level-processing-mask) := #t;
    let cr = form.form-compilation-record;
    let ld = form.form-library;
    ld.compiled-to-definitions? := #f;
    cr.compilation-record-definitions-installed? := #f;
    with-inconsistent-definitions (ld)
      // TODO: should retract derived forms?
      uninstall-top-level-form(form);
      remove-dependent-program-conditions(form, $top-level-processing-mask);
      while (form.form-dependencies)
	let dep = form.form-dependencies;
	unregister-binding-dependent(dep.dependency-binding, form);
	form.form-dependencies := dep.binding-dependency-next;
      end;
      if (form-implicitly-defined?(form))
	// Implicitly defined forms only exist when installed...
	let cr = form.form-compilation-record;
	// TODO: store derived forms in a separate place so don't have to
	// potentially copy a whole top-level forms vector just to remove one!
	cr.compilation-record-top-level-forms
	  := remove!(cr.compilation-record-top-level-forms, form);
      end;
    end with-inconsistent-definitions;
    stage-being-retracted?(form, $top-level-processing-mask) := #f;
  end;
end function;

define function retract-form-compilation (form :: <top-level-form>)
  if (~stage-being-retracted?(form, $compilation-mask))
    stage-being-retracted?(form, $compilation-mask) := #t;
    if (*trace-retraction*)
      format-out("Retracting COMP %s due to %s\n", form, *trace-retraction*);
      *trace-retraction* := #t;
    end;
    // Record "compilation" (i.e. heaping & linking) is not incremental,
    // so have to flush it when change anything...
    retract-record-compilation(form.form-compilation-record);
    // ** NEEDS TO RETRACT TYPE ESTIMATES of removed models, because
    // weak tables are not implemented yet!
    retract-top-level-form-models(form);
    stage-being-retracted?(form, $compilation-mask) := #f;
  end;
end function;

define function retract-top-level-form-models
    (form :: <top-level-form>)
  clear-dependent-model-properties(form);
  form-init-method(form) := #f;
  form-system-init-method(form) := #f;
  retract-form-model-objects(form);
  retract-form-dependencies(form, $compilation-mask);
  retract-form-dispatch-decisions(form);
  remove-dependent-program-conditions(form, $compilation-mask);
end function;

define function retract-form-dispatch-decisions (form :: <top-level-form>)
  // TODO: Need to bring this under dependency tracking.  For now this
  // means that dispatch decision recording is only meaninful in a full
  // compilation.
  form.form-compilation-record.compilation-record-dispatch-decisions := #();
end;

define compiler-open generic retract-form-model-objects (form :: <top-level-form>) => ();

define method retract-form-model-objects (form :: <top-level-form>) => ()
end method;

define method retract-form-model-objects (form :: <variable-defining-form>) => ()
  if (form.form-models-installed?)
    // There should be no retraction going on in interactive libraries..
    debug-assert(~*interactive-compilation-layer*);
    let ld :: <project-library-description> = form.form-original-library;
    ld.library-description-models-change-count :=
      ld.library-description-models-change-count + 1;
    form.form-models-installed? := #f;
  end;
  uninstall-form-models(form);
end;

define method uninstall-form-models (form :: <variable-defining-form>)
  for (binding in form-defined-bindings(form))
    retract-binding-model-object(binding);
  end for;
end method;


////////////////////////////////////////////////////////////////////////////
//
// Triggering.

// Dependencies which only affect stuff after the definition.
// ~defined-after?(*current-dependent*, definition)
define constant dep$befores = dep$name-syntax + dep$name-macro-ref;

define method all-binding-conditions
    (entry :: <list>) => (cc :: <dep-conditions>)
  for (deps = entry then deps.tail,
       cc = 0 then logior(cc, deps.head.name-dependency-conditions),
       until: deps == #())
  finally
    cc
  end;
end method;

define method all-binding-conditions
    (entry :: <name-dependency>) => (cc :: <dep-conditions>)
  entry.name-dependency-conditions
end method;

define method all-binding-conditions
    (entry :: <binding-dependency>) => (cc :: <dep-conditions>)
  logior(entry.binding-dependency-conditions,
	 all-binding-conditions(entry.binding-qualified-dependencies))
end method;

define method all-name-conditions
    (entry :: <list>, name :: <symbol>, module :: <module>)
 => (cc :: <dep-conditions>)
  for (deps = entry then deps.tail,
       cc = 0 then begin
		     let dep :: <name-dependency> = deps.head;
		     if (dep.dependency-name == name
			   & dep.dependency-module == module)
		       logior(cc, dep.name-dependency-conditions)
		     else
		       cc
		     end
		   end,
       until: deps == #())
  finally
    cc
  end;
end method;

define method all-name-conditions
    (entry :: <name-dependency>, name :: <symbol>, module :: <module>)
 => (cc :: <dep-conditions>)
  if (entry.dependency-name == name & entry.dependency-module == module)
    entry.name-dependency-conditions
  else
    0
  end;
end method;

define method all-name-conditions
    (entry :: <binding-dependency>, name :: <symbol>, module :: <module>)
 => (cc :: <dep-conditions>)
  all-name-conditions(entry.binding-qualified-dependencies, name, module)
end method;

define function match-binding-dependencies
    (entry, kinds :: <kind-mask>) => (stages :: <stage-mask>);
  dep-conditions-stages(all-binding-conditions(entry), kinds)  
end;

define function match-name-dependencies
    (entry, kinds :: <kind-mask>, name :: <symbol>, module :: <module>)
 => (stages :: <stage-mask>);
  dep-conditions-stages(all-name-conditions(entry, name, module), kinds)
end;

define inline function dependency-matching-kinds? 
    (dep :: <name-dependency>, kinds :: <kind-mask>) => (res :: <boolean>)
  let conditions = dep.name-dependency-conditions;
  let kind = dependency-conditions-kind(conditions);
  logand(kind, kinds) ~== 0
end function;

define inline function matching-name-entry (entry, kinds :: <kind-mask>)
  local method win? (dep :: <name-dependency>) 
          dependency-matching-kinds?(dep, kinds)
        end method;
  if (instance?(entry, <list>))
    let entry = choose(win?, entry);
    if (entry.tail == #()) entry.head else entry end
  elseif (win?(entry))
    entry
  else
    #()
  end
end function;
  

// FOR BROWSERS
define function choose-name-dependencies (form :: <top-level-form>,
					  kinds :: <kind-mask>)
  local method loop (dep :: false-or(<form-binding-dependency>), refs)
	  if (dep)
	    let entry = dep.binding-qualified-dependencies;
	    let match = matching-name-entry(entry, kinds);
	    loop(dep.binding-dependency-next,
		 if (instance?(match, <list>))
		   concatenate(match, refs)
		 else
		   pair(match, refs)
		 end)
	  else
	    refs
	  end;
	end method;
  let deps = loop(form.form-dependencies, #());
  deps
end function;

define method dependent-stages (cr :: <compilation-record>,
				binding :: <module-binding>,
				mask :: <kind-mask>,
				def) => (stages :: <stage-mask>)
  let aft-mask = def & logand(mask, lognot(dep$befores));
  let mask = if (aft-mask & aft-mask ~== mask & ~defined-before?(cr, def))
	       aft-mask
	     else
	       mask
	     end;
  unless (mask == 0)
    let table = cr.compilation-record-dependency-table;
    let entry = element(table, binding, default: #f);
    entry & match-binding-dependencies(entry, mask)
  end | 0
end method;

define method dependent-name-stages (cr :: <compilation-record>,
				     binding :: <module-binding>,
				     mask :: <kind-mask>,
				     name :: <symbol>,
				     module :: <module>,
				     def) => (stages :: <stage-mask>)
  let aft-mask = def & logand(mask, lognot(dep$befores));
  let mask = if (aft-mask & aft-mask ~== mask & ~defined-before?(cr, def))
	       aft-mask
	     else
	       mask
	     end;
  unless (mask == 0)
    let table = cr.compilation-record-dependency-table;
    let entry = element(table, binding, default: #f);
    entry & match-name-dependencies(entry, mask, name, module)
  end | 0
end method;

define method dependent-stages (form :: <top-level-form>,
				binding :: <module-binding>,
				mask :: <kind-mask>,
				def) => (stages :: <stage-mask>)
  if (form-stripped?(form) & mask == dep$refs)
    // compressed dependencies as compressed referenced-variables
    // with module name ... module name ... see strip-incremental-slots
    block (return)
      let vars = form-referenced-variables(form) | #[];
      let from = form-referenced-binding-variables-from(vars);
      iterate search (i :: <integer> = from, module = #f)
        when (i < size(vars))
          let elt = vars[i];
	  if (instance?(elt, <module>))
	    search(i + 1, elt)
	  elseif (module) // MAKE SURE
	    when (untracked-lookup-binding-in(module, elt))
	      return(dep$name-binding-ref); // HACK: IS THIS RIGHT?
	    end when;
	  end if;
        end when;
      end iterate;
      0
    end block;
  else
    // TODO: should precompute this unless def is actually in cr.
    let aft-mask = def & logand(mask, lognot(dep$befores));
    let mask = if (aft-mask & aft-mask ~== mask & ~defined-before?(form, def))
		 aft-mask
	       else
		 mask
	       end;
    unless (mask == 0)
      let entry = lookup-form-binding-dependencies(form, binding);
      entry & match-binding-dependencies(entry, mask)
    end | 0
  end if
end method;

define method dependent-name-stages (form :: <top-level-form>,
				     binding :: <module-binding>,
				     mask :: <kind-mask>,
				     name :: <symbol>,
				     module :: <module>,
				     def) => (stages :: <stage-mask>)
  // TODO: should precompute this unless def is actually in cr.
  let aft-mask = def & logand(mask, lognot(dep$befores));
  let mask = if (aft-mask & aft-mask ~== mask & ~defined-before?(form, def))
	       aft-mask
	     else
	       mask
	     end;
  unless (mask == 0)
    let entry = lookup-form-binding-dependencies(form, binding);
    entry & match-name-dependencies(entry, mask, name, module)
  end | 0
end method;

define sideways method note-adding-definition
    (binding :: <module-binding>, definition :: <top-level-form>)
  note-changing-definition(binding, #f, definition)
end method;

define sideways method note-removing-definition
    (binding :: <module-binding>, definition :: <top-level-form>)
  note-changing-definition(binding, definition, #f)
end method;

define sideways method note-adding-modifying-definition
    (binding :: <module-binding>, definition :: <top-level-form>)
  if (*trace-dependencies*) format-out("Adding %s\n", definition) end;
  trigger-binding-dependents(binding, dep$modifying-definitions, definition)
end;

define sideways method note-removing-modifying-definition
    (binding :: <module-binding>, definition :: <top-level-form>)
  if (*trace-dependencies*) format-out("Removing %s\n", definition) end;
  trigger-binding-dependents(binding, dep$modifying-definitions, definition)
end;

define sideways method note-removing-modifying-models (binding :: <module-binding>)
  if (*trace-dependencies*)
    format-out("Removing %s modifying model\n", binding)
  end;
  trigger-binding-dependents(binding, dep$modifying-models, #f);
end;

define sideways method note-removing-model-object (binding :: <module-binding>)
  if (*trace-dependencies*)
    format-out("Removing %s model object\n", binding)
  end;
  trigger-binding-dependents(binding, dep$model-object, #f);
end;

// TODO: when add definition, any macro/syntax refs before it remain
// valid, since they continue to be undefined.  Similarly when remove
// a definition, any macro/syntax refs preceding it continue to be
// valid, since they came up undefined even before.
// For now we just invalidate everything though...
define function note-changing-definition (binding :: <module-binding>,
					  old :: false-or(<top-level-form>),
					  new :: false-or(<top-level-form>))
  unless (*interactive-compilation-layer*)
    if (*trace-dependencies*)
      format-out("CHANGING %s from %s to %s\n", binding, old, new);
    end;
    let mask :: <kind-mask> = dep$active-definition;
    let mask :: <kind-mask> =
      if (~old == ~new)
	mask
      else
	mask + dep$defined?
      end if;
    let old-macro-class = old & form-macro-word-class(old);
    let old-macro = old-macro-class & old;
    let new-macro-class = new & form-macro-word-class(new);
    let new-macro = new-macro-class & new;
    let start = #f;
    if (old-macro ~== new-macro)
      mask := mask + dep$name-macro-ref;
      start := if (old-macro
		     & (~new-macro | defined-after?(old-macro, new-macro)))
		 old-macro
	       else
		 new-macro
	       end;
      let old-def? = old-macro & definer-token-class?(old-macro-class) & #t;
      let new-def? = new-macro & definer-token-class?(new-macro-class) & #t;
      if ((old-macro & ~old-def?) ~== (new-macro & ~new-def?))
	mask := mask + dep$name-syntax;
      end;
      if (old-def? ~== new-def?)
	let refs = definer-references(binding);
	for (word-binding in refs)
	  trigger-binding-dependents(word-binding, dep$name-syntax, start);
	end;
      end;
    end;
    trigger-binding-dependents(binding, mask, start);
    if (*trace-dependencies*)
      format-out("END CHANGING %s\n", binding);
    end;
  end;
end function;


define sideways method note-changing-binding
    (old-binding :: <module-binding>,
     name :: <symbol>,
     module :: <module>,
     new-binding :: false-or(<module-binding>))
  if (*trace-dependencies*)
    format-out("CHANGING %s in %s from %s to %s\n",
	       name, module, old-binding, new-binding);
  end;
  let mask :: <dep-condition> = dep$name-binding + dep$name-binding-ref;
  let old-definition = untracked-binding-definition(old-binding, default: #f);
  let old-macro-class = old-definition & form-macro-word-class(old-definition);
  let old-macro = old-macro-class & old-definition;
  let new-definition = new-binding & untracked-binding-definition(new-binding, default: #f);
  let new-macro-class = new-definition & form-macro-word-class(new-definition);
  let new-macro = new-macro-class & new-definition;
  let start = #f;
  if (old-macro ~== new-macro)
    mask := mask + dep$name-macro-ref;
    start := if (old-macro &
		   (~new-macro | defined-after?(old-macro, new-macro)))
	       old-macro
	     else
	       new-macro
	     end;
    let old-def? = old-macro & definer-token-class?(old-macro-class) & #t;
    let new-def? = new-macro & definer-token-class?(new-macro-class) & #t;
    if ((old-macro & ~old-def?) ~== (new-macro & ~new-def?))
      mask := mask + dep$name-syntax;
    end;
    if (old-def? ~== new-def?)
      let word = name-definer-word(name);
      if (word)
	let word-binding = untracked-lookup-binding-in(module, word);
	trigger-name-dependents(word-binding, dep$name-syntax, start,
				name, module);
      end;
    end;
  end;
  trigger-name-dependents(old-binding, mask, start, name, module);
end method;
  

define constant dep$refs = dep$name-macro-ref + dep$name-binding-ref;

define function binding-local-references? (binding :: <module-binding>)
  local method ref? (dep)
	  dependent-stages(dep, binding, dep$refs, #f) ~== 0
	end;
  any?(ref?, binding.binding-local-dependents)
end function;

// For browsers.  They only want top level forms, not source records...
define function binding-local-referers (binding :: <module-binding>)
  local method ref?(dependent)
	  instance?(dependent, <top-level-form>)
	    & dependent-stages(dependent, binding, dep$refs, #f) ~== 0
	end;
  choose(ref?, binding-local-dependents(binding))
end function;

define function trigger-binding-dependents (binding :: <module-binding>,
					    mask :: <kind-mask>,
					    def :: false-or(<top-level-form>))
 unless (*interactive-compilation-layer*)
  if (*trace-dependencies*)
    format-out("  TRIGGER %s %s\n", binding, dependency-conditions-name(mask));
  end;
  local method trigger (dep)
	 UNLESS (dep == #()) // I don't know why this keeps happening?
	  let stages = dependent-stages(dep, binding, mask, def);
	  unless (stages == 0)
	    if (*trace-retraction*) *trace-retraction* := binding end;
	    retract-dependent-stages(dep, stages);
	    if (*trace-retraction*) *trace-retraction* := #t end;
	  end;
	 END;
	end method;
  do(trigger, binding-local-dependents(binding))
 end;
end function;

define function trigger-name-dependents (binding :: <module-binding>,
					 mask :: <kind-mask>,
					 def :: false-or(<top-level-form>),
					 name :: <symbol>,
					 module :: <module>)
 unless (*interactive-compilation-layer*)
  if (*trace-dependencies*)
    format-out("  TRIGGER NAME %s in %s %s\n", name, module, dependency-conditions-name(mask));
  end;
  local method trigger (dep)
         UNLESS (dep == #()) // This keeps happening for some reason
	  let stages = dependent-name-stages(dep, binding, mask, name, module, def);
	  unless (stages == 0)
	    if (*trace-retraction*) *trace-retraction* := binding end;
	    retract-dependent-stages(dep, stages);
	    if (*trace-retraction*) *trace-retraction* := #t end;
	  end;
	 end;
	end method;
    do(trigger, binding-local-dependents(binding))
 end;
end function;



define function retract-compilation-record-dependencies
    (cr :: <compilation-record>, stages :: <stage-mask>)
  let table = cr.compilation-record-dependency-table;
  for (entry keyed-by binding in table)
    let new-entry = remove-dependency-stages(entry, stages);
    if (new-entry == #())
      unregister-binding-dependent(binding, cr);
      remove-key!(table, binding);
    elseif (new-entry ~== entry)
      table[binding] := new-entry;
    end;
  end for;
end function;

define function retract-form-dependencies
    (form :: <top-level-form>, stages :: <stage-mask>)
  local method loop (last, dep)
	  if (dep)
	    let new-dep = remove-dependency-stages(dep, stages);
	    let next = dep.binding-dependency-next;
	    if (new-dep == #())
              unregister-binding-dependent(dep.dependency-binding, form);
	      if (last) last.binding-dependency-next := next
	      else form.form-dependencies := next end;
	      loop(last, next);
	    else
	      unless (new-dep == dep)
		dep.binding-dependency-conditions := 0;
		dep.binding-qualified-dependencies := new-dep;
	      end;
	      loop(dep, next);
	    end;
	  end;
	end method;
  loop(#f, form.form-dependencies);
end function;

define method remove-dependency-stages (dep :: <binding-dependency>,
					stages :: <stage-mask>)
  let cc = dep.binding-dependency-conditions;
  let new-cc = remove-dependency-conditions-stages(cc, stages);
  let deps = dep.binding-qualified-dependencies;
  let new-deps = remove-name-dependency-stages(deps, stages);
  if (new-cc == 0)
    new-deps
  else
    dep.binding-dependency-conditions := new-cc;
    dep.binding-qualified-dependencies := new-deps;
    dep
  end
end method;

define method remove-dependency-stages (dep :: <qualified-entry>,
					stages :: <stage-mask>)
  remove-name-dependency-stages(dep, stages)
end method;

define method remove-name-dependency-stages (entry :: <list>,
					     stages :: <stage-mask>)
  local method loop(last, dd, all)
	  if (dd == #()) all
	  else
	    let dep :: <name-dependency> = dd.head;
	    let cc = dep.name-dependency-conditions;
	    let new = remove-dependency-conditions-stages(cc, stages);
	    if (new == 0)
	      loop(last, dd.tail, if (last)
				    last.tail := dd.tail;
				    all
				  else
				    dd.tail
				  end)
	    else
	      dep.name-dependency-conditions := new;
	      loop(dd, dd.tail, all)
	    end;
	  end;
	end method;
  let new = loop(#f, entry, entry);
  if (new.tail == #()) new.head else new end
end method;

define method remove-name-dependency-stages (dep :: <name-dependency>,
					     stages :: <stage-mask>)
  let cc = dep.name-dependency-conditions;
  let new = remove-dependency-conditions-stages(cc, stages);
  if (new == 0)
    #()
  else
    dep.name-dependency-conditions := new;
    dep
  end
end method;

// Returns conditions with all stage conditions removed.
define inline function remove-dependency-conditions-stages
    (conditions :: <dep-conditions>, stages :: <stage-mask>)
 => (conditions :: <dep-conditions>)
  logand(conditions, lognot(stages));
end function;

define inline function add-dependency-condition
    (conditions :: <dep-conditions>, condition :: <dep-condition>)
 => (conditions :: <dep-conditions>)
  logior(conditions, condition)
end function;

define inline function dependency-conditions-kind
    (condition :: <dep-conditions>) => (kind :: <kind-mask>)
  debug-assert(stage$count == 2);
  logand(dep$all, logior(ash(condition, - stage$0),
			 ash(condition, - stage$1)))
end function;


define function dep-conditions-stages
    (conditions :: <dep-conditions>, kinds :: <kind-mask>) => (stages :: <stage-mask>);
  debug-assert(stage$count == 2);
  logior(if (logand(conditions, make-dependency-condition(stage$0, kinds)) == 0) 0
	 else stage$0-mask end,
	 if (logand(conditions, make-dependency-condition(stage$1, kinds)) == 0) 0
	 else stage$1-mask end)
end function;


// DEBUG only
define function dependency-conditions-name (conditions :: <dep-conditions>)
  let str = #f;
  local method add-stage(stage, name)
	  let cc = ash(conditions, - stage);
	  local method add-mask (mask, stage, name)
		  unless (logand(cc, mask) == 0)
		    let name = name | format-to-string("x0%x", logand(cc, mask));
		    let full-name = concatenate(stage, "/", name);
		    str := if (str) concatenate(str, "+", full-name) else full-name end;
		    cc := logand(cc, lognot(mask));
		  end;
		end;
	  if (logand(cc, dep$all) == dep$all)
	    str := if (str) concatenate(str, "+", stage, "/*")
		   else concatenate(stage, "/*") end;
	  else
	    add-mask(dep$name-syntax, name, "Syntax");
	    add-mask(dep$name-macro-ref, name, "Macro");
	    add-mask(dep$name-binding, name, "Binding");
	    add-mask(dep$name-binding-ref, name, "BindingRef");
	    add-mask(dep$active-definition, name, "Definition");
	    add-mask(dep$defined?, name, "Defined?");
	    add-mask(dep$modifying-definitions, name, "ModifyingDefs");
	    add-mask(dep$model-object, name, "Model");
	    add-mask(dep$modifying-models, name, "ModifyingModels");
	    add-mask(dep$all, name, #f);
	  end;
	  conditions := logand(conditions,
			       lognot(make-dependency-condition(stage, dep$all)));
	end method;
  add-stage($compilation, "Comp");
  add-stage($top-level-processing, "TLP");
  unless (conditions == 0)
    let name = format-to-string("x0%x", conditions);
    str := if (str) concatenate(str, "+", name) else name end;
  end;
  str | "0"
end;

// DEBUG only
define method print-object (d :: <name-dependency>, s :: <stream>) => ()
  format(s, "{NameDep %= in %s : %s}",
	 d.dependency-name, d.dependency-module,
	 dependency-conditions-name(d.name-dependency-conditions));
end method;

// DEBUG only
define method print-object (d :: <binding-dependency>, s :: <stream>) => ()
  format(s, "{Binding dependency %s + %s}",
	 dependency-conditions-name(d.binding-dependency-conditions),
	 d.binding-qualified-dependencies);
end;
// DEBUG only
define method print-object (d :: <form-binding-dependency>, s :: <stream>) => ()
  format(s, "{Form %s:%s + %s}",
	 d.dependency-binding,
	 dependency-conditions-name(d.binding-dependency-conditions),
	 d.binding-qualified-dependencies);
end;

// DEBUG only
define method as (c :: subclass(<sequence>), d :: <form-binding-dependency>)
 => (object :: <sequence>)
  for (l = #() then pair(dep, l),
       dep = d then dep.binding-dependency-next,
       while: dep)
  finally as(c, reverse!(l))
  end;
end method;
