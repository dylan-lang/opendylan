module: dfmc-namespace
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Setting this to #f turns on special handling of system libraries.
define variable *shadow-system-libraries?* = #f;


define method verify-target-library
    (target, ld :: <project-library-description>)
// Verify that ld does correspond to a library in the target, including
// checking that neither the database nor the runtime has been modified in
// such a way as to invalidate the correspondence.  Presumably this means
// storing some kind of a timestamp in both the runtime and the database,
// and also some flag in the runtime to say whether interactive execution
// has taken place.
// This may need to return some kind of runtime id of the library which we
// would store and then pass to the downloader.
end method;

define macro interactive-class-mapping-definer
  { define interactive-class-mapping end } => { }
  { define interactive-class-mapping
      ?original:expression => ?interactive:expression;
      ?rest:*
    end }
    => { define method interactive-class-for
	     (original :: ?original) => (c :: singleton(?interactive))
	   ?interactive
	 end;
         define interactive-class-mapping ?rest end; }
end macro;

define class <interactive-library-description> (<library-description>)
  inherited slot compiled-to-definitions? = #t;
  constant slot interactive-library-target,
    required-init-keyword: target:;
  constant slot interactive-library-os-name :: <symbol>,
    required-init-keyword: os:;
  constant slot interactive-library-processor-name :: <symbol>,
    required-init-keyword: processor:;
  constant slot interactive-library-major-version :: <integer>,
    required-init-keyword: major-version:;
  constant slot interactive-library-minor-version :: <integer>,
    required-init-keyword: minor-version:;
  constant slot interactive-library-project-library :: <project-library-description>,
    required-init-keyword: project-library:;
  constant slot interactive-form-properties :: <table> = make(<table>);
  constant slot library-combined-back-end-data,
    required-init-keyword: back-end-data:;
  // This slot is here for the benefit of unshadowed library definitions.
  // If we ever retract interactive library definitions, this table must
  // be retracted along with the definition.
  constant slot used-library-table :: <ordered-object-table>,
    required-init-keyword: used-libraries:;
  slot all-library-descriptions :: <list> = #();
end;

define class <dylan-interactive-library-description>
    (<dylan-library-description>, <interactive-library-description>)
end;

// change count never changes, since we don't do recompilation.
define method library-description-change-count
    (ild :: <interactive-library-description>) => (count :: singleton(0))
  0
end method;

define method library-description-os-name
    (ild :: <interactive-library-description>) => (name :: <symbol>)
  ild.interactive-library-os-name
end method;

define method library-description-processor-name 
    (ild :: <interactive-library-description>) => (name :: <symbol>)
  ild.interactive-library-processor-name
end method;

define method library-description-major-version
    (ild :: <interactive-library-description>) => (vers :: <integer>)
  ild.interactive-library-major-version
end method;

define method library-description-minor-version
    (ild :: <interactive-library-description>) => (vers :: <integer>)
  ild.interactive-library-minor-version
end method;

define method library-description-library-pack
    (ild :: <interactive-library-description>) => (vers :: <integer>)
  ild.interactive-library-project-library.library-description-library-pack
end method;

define method library-description-build-location
    (ild :: <interactive-library-description>) => (build-location)
  ild.interactive-library-project-library.library-description-build-location;
end method;

define method library-description-build-settings
    (ild :: <interactive-library-description>) => (settings :: <sequence>)
  ild.interactive-library-project-library.library-description-build-settings;
end method;

define method library-description-project
    (ild :: <interactive-library-description>) => (project)
  ild.interactive-library-project-library.library-description-project;
end method;

// TODO: Debugging only.
define method compiled-to-definitions?-setter
    (value, ild :: <interactive-library-description>)
  let il = *interactive-compilation-layer*;
  error("Bug! Who's compiling an interactive library description?");
end method;

define method compilation-from-definitions-started?
    (ild :: <interactive-library-description>)
  let il = *interactive-compilation-layer*;
  if (~il)
    next-method();
  else
    debug-assert(current-library-description?(ild));
    il.compilation-from-definitions-started?
  end;
end method;

define method compilation-from-definitions-started?-setter
    (value :: <boolean>, ild :: <interactive-library-description>)
  // Can only start compiling current library!
  debug-assert(current-library-description?(ild));
  let il = *interactive-compilation-layer*;
  if (il)
    il.compilation-from-definitions-started? := value;
  else
    next-method()
  end;
end method;

define method library-description-compilation-mode
    (il :: <interactive-library-description>)
  #"interactive"
end;

define method library-description-stripped?
    (il :: <interactive-library-description>) => (yes :: singleton(#t))
  #t
end method;

define method library-conditions-table
    (ild :: <interactive-library-description>) => (table :: <table>)
  let il = *interactive-compilation-layer*;
  if (il & il.interactive-layer-base == ild)
    il.conditions-layer
  else
    next-method()
  end
end method;

define method library-type-cache
    (ild :: <interactive-library-description>) => (cache)
  let il = *interactive-compilation-layer*;
  if (il & il.interactive-layer-base == ild)
    il.library-type-cache
  else
    next-method()
  end
end method;

define method library-type-estimate-disjoint?-cache
    (ild :: <interactive-library-description>) => (cache)
  let il = *interactive-compilation-layer*;
  if (il & il.interactive-layer-base == ild)
    il.library-type-estimate-disjoint?-cache
  else
    next-method()
  end
end method;

define method library-type-estimate-cons-cache
    (ild :: <interactive-library-description>) => (cache)
  let il = *interactive-compilation-layer*;
  if (il & il.interactive-layer-base == ild)
    il.library-type-estimate-cons-cache
  else
    next-method()
  end
end method;

define method library-type-estimate-dispatch-cache
    (ild :: <interactive-library-description>) => (cache)
  let il = *interactive-compilation-layer*;
  if (il & il.interactive-layer-base == ild)
    il.library-type-estimate-dispatch-cache
  else
    next-method()
  end
end method;

define interactive-class-mapping
   <project-library-description>
     => <interactive-library-description>;
   <dylan-project-library-description>
     => <dylan-interactive-library-description>;
end;


define function project-library-interactive-context
    (ld :: <project-library-description>, target)
 => (ild :: false-or(<interactive-library-description>))
  any?(method (p) if (p.head == target) p.tail end end,
       ld.project-library-interactive-contexts)
end;

define function project-library-interactive-context-setter
    (ild :: false-or(<interactive-library-description>),
     ld :: <project-library-description>,
     target)
  ld.project-library-interactive-contexts
    := if (ild)
	 debug-assert(~project-library-interactive-context(ld, target),
		      "Context already set.  Re-entrant call to establish-execution-context?");
	 add!(ld.project-library-interactive-contexts, pair(target, ild))
       else
	 // TODO: Maybe should leave something behind if any interactive
	 // execution has taken place, if the runtime doesn't record that.
	 remove!(ld.project-library-interactive-contexts, target,
		 test: method(p, target) p.head == target end)
       end;
  ild
end function;

define function lookup-interactive-context
    (target, ld :: <project-library-description>,
     #key default = unsupplied(), force-shadow?)
  project-library-interactive-context(ld, target)
    | if (supplied?(default))
	default
      else
	verify-target-library(target, ld);
	make-interactive-context(ld, target, force-shadow?: force-shadow?)
      end;
end function;

define function detach-interactive-namespaces
    (ld :: <project-library-description>)
  for (target+ild in ld.project-library-interactive-contexts)
    let ild = target+ild.tail;
    detach-interactive-library(ild, ~ild.library-forms-dynamic?)
  end;
end function;

define function interactive-namespaces-detached?
    (ld :: <project-library-description>)
 every?(method(target+ild) interactive-library-detached?(target+ild.tail) end,
	ld.project-library-interactive-contexts)
end;
  
define function models-in-interactive-use?
    (ld :: <project-library-description>)
  ~ld.library-forms-dynamic? & ~empty?(ld.project-library-interactive-contexts)
end function;


define method library-description-in-context
    (cx :: <interactive-library-description>,
     ld :: <interactive-library-description>)
 => (ld :: <interactive-library-description>)
  ld
end method;

define method library-description-in-context
    (cx :: <interactive-library-description>,
     ld :: <project-library-description>)
 => (ld :: <interactive-library-description>)
  if (ld == cx.interactive-library-project-library) // skip target search
    cx
  else
    let target = cx.interactive-library-target;
    let ild = project-library-interactive-context(ld, target);
    debug-assert(ild, "Used interactive context not precomputed?");
    ild
  end
end method;

define method library-description-in-context
    (cx :: <interactive-library-description>, ld == #f)
 => (ld :: <interactive-library-description>)
  debug-assert(*interactive-compilation-layer*);
  *interactive-compilation-layer*.interactive-layer-base
end method;

define method initial-form-properties (form :: <modifying-form>, models?)
  let model = models? & form.shadowable-form-model;
  if (model)
    let p = make-default-form-properties(form);
    p.shadowable-form-model := model;
    p
  end
end;

define method initial-form-properties (form :: <top-level-form>, models?)
  #f
end method;

define method form-properties-in-context
    (ild :: <interactive-library-description>, form :: <top-level-form>, create?)
 => (p :: false-or(<form-properties>))
  debug-assert(current-library-description?(ild));
  local method lookup (form, create?)
	  if (form-interactive?(form))
	    form
	  else
	    let home-ld = form.form-library;
	    let table = home-ld.interactive-form-properties;
	    element(table, form, default: #f)
	      | if (~home-ld.interactive-library-detached?)
		  let props = initial-form-properties(form, ~ild.library-forms-dynamic?);
		  props & (table[form] := props)
		end
	      | if (create?)
		  table[form] := make-default-form-properties(form);
		end
	  end;
	end method;
  let il = *interactive-compilation-layer*;
  if (~il)
    lookup(form, create?)
  else
    debug-assert(il.interactive-layer-base == ild);
    if (~form-downloaded?(form))
      form
    else
      element(il.form-properties-layer, form, default: #f) |
	begin
	  let p = lookup(form, #f);
	  if (create?) // iff settable
	    // BREAK("setting properties for %s from %s\n", form, form-ld);
	    il.form-properties-layer[form]
	      := if (p) shadow-form-properties(p)
		 else make-default-form-properties(form) end;
	  else
	    p
	  end
	end
    end
  end
end method;

define method form-properties-in-context
    (ild :: <interactive-library-description>,
     form :: <form-properties>,
     create?) => (p :: <form-properties>)
  ignore(create?);
  form
end method;

define method compute-cached-form-model-in
    (ild :: <interactive-library-description>,
     form :: <modifying-form>)
  debug-assert(~library-forms-dynamic?(ild));
  if (ild.interactive-library-detached?)
    #f
  else
    let project-model
      = with-dood-context (ild.interactive-library-project-library)
          untracked-ensure-form-model(form)
        end;
    form.form-model := project-model
  end;
end method;


define method binding-properties-in-context
    (ild :: <interactive-library-description>, b :: <module-binding>, create?)
  // Note that this can be called with ild being anything between the current
  // library and the home library of the binding - see imported-binding-
  // delegated-property and untracked-binding-modifying-definitions.
  debug-assert(valid-binding-home-library-in?(ild, b), "Bogus binding: %s", b);
  local method bare-binding-properties (b :: <module-binding>)
	  make(interactive-class-for(b), dependents: #[])
	end;
  local method lookup (b :: <module-binding>, create?)
	  let lib = ild.language-definition;
	  if (instance?(lib, <interactive-library>))
	    let table = lib.interactive-binding-properties;
	    element(table, b, default: #f)
	      | if (~lib.interactive-library-detached?)
		  let props = initial-binding-properties(b, ~ild.library-forms-dynamic?, #f);
		  props & (table[b] := props)
		end
	      | if (create?)
		  table[b] := bare-binding-properties(b);
		end;
	  else
	    // TODO: Unfortunately, there are cache slots in imported bindings
	    // which it's ok to set, but we can't tell which slot is going to
	    // be set at this point so we can't test for this.
	    // debug-assert(~create?,
	    //              "Changing interactive binding properties in unshadowed library!");
	    binding-properties-in-context
	      (ild.interactive-library-project-library, b, create?);
	  end;
	end method;
  let il = *interactive-compilation-layer*;
  if (~il)
    lookup(b, create?);
  else
    element(il.binding-properties-layer, b, default: #f) |
      begin
	let p = lookup(b, #f);
	if (create?) // iff settable
	  debug-assert(il.interactive-layer-base == ild,
		       "Modifying inherited properties!");
	  il.binding-properties-layer[b]
	    := if (p) shadow-binding-properties(p)
	       else bare-binding-properties(b) end
	else
	  p
	end
      end
  end
end method;

define method compute-cached-binding-model-object-in
    (ild :: <interactive-library-description>,
     binding :: <imported-module-binding>,
     definition :: <variable-defining-form>) => (model);
  let canonical = binding-canonical-binding(binding);
  with-dependent-context ($compilation of definition)
    let home-ld = namespace-library-description(binding-home(canonical));
    compute-cached-binding-model-object-in(home-ld, canonical, definition)
  end;
end method;					 

define method compute-cached-binding-model-object-in
    (ild :: <interactive-library-description>,
     binding :: <canonical-module-binding>,
     definition :: <variable-defining-form>) => (model);
  debug-assert(definition.form-library == ild, "Mismatched home lib");
  // We only do this for tightly compiled libraries.
  debug-assert(~library-forms-dynamic?(ild));
  if (ild.interactive-library-detached?)
    $binding-model-not-computed
  else
    let project-model
      = with-dood-context (ild.interactive-library-project-library)
	  debug-assert(definition == binding.binding-active-definition,
		       "Unexpected definition difference");
	  untracked-binding-model-object(binding, #f);
        end;
    binding.binding-cached-model-object := project-model
  end;
end method;

define method library-dynamically-bound-in?
    (ld :: <interactive-library-description>,
     uld :: <interactive-library-description>)
  let bindings = ld.all-inter-library-bindings;
  #"loose" == if (interactive-library-shadowed?(ld))
		bindings[uld]
	      else
		bindings[uld.interactive-library-project-library]
	      end;
end method;


define open generic active-lexical-variables
    (runtime-context) => (variables-and-values :: <sequence>);

// TODO: remove this fake method once somebody actually implements above.
define method active-lexical-variables
    (runtime-context) => (variables-and-values :: <sequence>);
  #()
end method;

define constant <outer-lexical-environment> = <variable-name-table>;

define function make-outer-lexical-environment (runtime-context) =>
    (env :: <outer-lexical-environment>)
  let table = make(<outer-lexical-environment>);
  let vars-and-vals = as(<list>, active-lexical-variables(runtime-context));
  for (plist = vars-and-vals then plist.tail.tail, until: plist == #())
    let var = dylan-variable-name(plist.first);
    if (element(table, var, default: table) == table)
      table[var] := plist.second;
    end;
  end;
  table
end;


define class <interactive-layer> (<compilation-context>)
  constant slot interactive-layer-base :: <interactive-library-description>,
    required-init-keyword: base:;
  constant slot interactive-layer-lexical-environment :: <outer-lexical-environment>,
    required-init-keyword: lexical-environment:;
  constant slot binding-properties-layer :: <table> = make(<table>);
  constant slot form-properties-layer :: <table> = make(<table>);
  constant slot mapped-model-properties-layer :: <table> = make(<table>);
  constant slot conditions-layer :: <table> = make(<table>);
  // Use these exact names so initialize-typist-library-caches works.
  slot library-type-cache = #f;
  slot library-type-estimate-disjoint?-cache = #f;
  slot library-type-estimate-cons-cache = #f;
  slot library-type-estimate-dispatch-cache = #f;
end;

define function outer-lexical-environment ()
 => (maybe-env :: false-or(<outer-lexical-environment>))
  let il = *interactive-compilation-layer*;
  if (il)
    // See make-outer-lexical-environment
    il.interactive-layer-lexical-environment;
  end;
end;

// This is just here so the progress-line in ensure-definitions-installed
// doesn't err out...
define method library-description-project (il :: <interactive-layer>)
  #f
end method;

define macro with-interactive-layer
 { with-interactive-layer (?layer:variable = ?library-context:expression
			     in ?runtime-context:expression)
    ?:body
   end }
    => 
    { do-with-interactive-layer(?library-context, ?runtime-context,
				method(?layer) ?body end) }
end macro;

define function do-with-interactive-layer
    (ild :: <interactive-library-description>,
     runtime-context,
     fn :: <method>)
  debug-assert(~*interactive-compilation-layer*,
	       "Can't nest interactive compiles");
  debug-assert(ild.language-definition);
  // TODO: Should shadow if isn't!
  debug-assert(ild.interactive-library-shadowed?,
	       "Can't interact in unshadowed library!");
  with-library-context (ild)
    let layer = make(<interactive-layer>,
		     base: ild,
		     lexical-environment:
		       make-outer-lexical-environment(runtime-context));
    initialize-typist-library-caches(layer);
    dynamic-bind (*interactive-compilation-layer* = layer)
      fn(layer)
    end;
  end;
end function;

define method find-model-properties-in
    (ld :: <interactive-library-description>, model, settable?, 
     #key create? = #t)
 => (p :: false-or(<mapped-model-properties>))
  let il = *interactive-compilation-layer*;
  if (il)
    let shadow-properties = element(il.mapped-model-properties-layer,
				    model,
				    default: #f);
    if (shadow-properties)
      shadow-properties
    else
      debug-assert(il.interactive-layer-base == ld);
      let base-properties 
        = any?(method (ld)
                 lookup-owned-model-properties-in(ld, model)
               end, 
               all-library-descriptions(ld));
      if (base-properties & settable?)
	il.mapped-model-properties-layer[model]
	  := shadow-model-properties(base-properties);
      elseif (base-properties)
	// just looking up some value.
	base-properties
      elseif (create?)
	// If current dependent hasn't been downloaded yet, it's ok to create
	// model, but don't put it in the library cache yet.
	// But if current dependent has been downloaded ???? Well, we're
	// disallowing that so far.
	let m = new-mapped-model(model);
	debug-assert(~compilation-record-downloaded?(model-compilation-record(m)));
	m
      end;
    end;
  else
    next-method()
  end;
end;

define inline function shadow-properties (original)
  shallow-copy-instance(original);
end function;


define method shadow-model-properties (p :: <model-properties>)
//  DEBUG-ASSERT(object-class(p) == access(dfmc-common,<mapped-model-properties>));
  shadow-properties(p)
end method;

// TODO: this whole thing needs to be atomic.
define function merge-interactive-layer (layer :: <interactive-layer>, tid)
  debug-assert(layer.compiled-to-definitions? &
		 layer.compilation-from-definitions-started?,
	       "Layer not compiled?");
  let ld = layer.interactive-layer-base;
  debug-assert(layer == *interactive-compilation-layer* &
		 current-library-description?(ld), "Wrong context");
  // Make lookups use the basic library, not the layer
  dynamic-bind (*interactive-compilation-layer* = #f)

    // Merge mapped model properties.  Since nobody actually points to model
    // properties, can just replace them.
    let properties = ld.library-owned-model-properties;
    for (prop keyed-by model in layer.mapped-model-properties-layer)
      let table = if (model-downloaded?(prop))
		    debug-assert(element(prop.model-library.library-owned-model-properties,
					 model, default: #f),
				 "downloaded model doesn't have properties in owner");
		    prop.model-library.library-owned-model-properties
		  else
		    properties
		  end;
      table[model] := prop;
    end;

    // Merge compilation records
    let cr* = layer.compilation-context-records;
    let cr-table = ld.cached-library-description-record-table;
    ld.cached-library-description-record-table := #f; // in case abort
    for (cr in cr*, index from size(ld.compilation-context-records) by 1)
      cr.compilation-record-transaction-id := tid;
      cr.compilation-record-original-library := ld;
      cr.compilation-record-sequence-number := index;
      if (cr-table) cr-table[cr.compilation-record-source-record] := cr end;
    end;
    // TODO: Change to a different type so can do this more efficiently.
    ld.compilation-context-records
      := concatenate(cr*, ld.compilation-context-records);
    ld.cached-library-description-record-table := cr-table;
    // At this point every form/cr is marked as downloaded.

    // Merge binding properties
    let binding-props = ld.language-definition.interactive-binding-properties;
    for (prop keyed-by b in layer.binding-properties-layer)
      debug-assert(valid-binding-home-library-in?(ld, b));
      // Since nobody actually points to binding properties, just replace 'em.
      binding-props[b] := prop
    end;

    // Merge form properties
    for (prop keyed-by form in layer.form-properties-layer)
      // form has been previously downloaded, but changed model or something...
      if (form-interactive?(form))
	merge-form-properties!(form, prop)
      else
	let table = form.form-library.interactive-form-properties;
	let old-prop = element(table, form, default: #f);
	if (old-prop)
	  merge-form-properties!(old-prop, prop)
	else
	  table[form] := prop
	end;
      end;
    end for;

    // Merge conditions
    // TODO: would we ever need to remove conditions?
    let conditions = ld.library-conditions-table;
    for (new-q keyed-by key in layer.conditions-layer)
      let q = element(conditions, key, default: not-found());
      let lkey = if (key == #f) tid else key end;
      conditions[lkey] := if (found?(q)) concatenate!(q, new-q) else new-q end;
    end;
  end;
end function;


// TODO: Don't need to copy libraries which we won't allow modifying by
// interactive execution, if there is such a thing...  More generally,
// don't need to copy libraries until either try to execute something
// in them, or try to modify the underlying project library description.

define method make-interactive-context (ld :: <project-library-description>,
					target,
					#key force-shadow? = #f)
  debug-assert(compiled-to-definitions?(ld));
  let ild = make(interactive-class-for(ld),
		 project-library: ld,
		 used-libraries: used-libraries-for-target(ld, target),
		 target: target,
		 // TODO: these should come from the target, or at least match it
		 os: ld.library-description-os-name,
		 processor: ld.library-description-processor-name,
		 major-version: ld.library-description-major-version,
		 minor-version: ld.library-description-minor-version,
		 back-end-data: ld.library-combined-back-end-data);
  // TODO: Should we copy language-definition-change-count?
  ild.compilation-context-records := ld.compilation-context-records;
  ild.library-forms-dynamic? := ld.library-forms-dynamic?;
  // TODO: Is this only necessary because all objects aren't being
  // recomputed?
  ild.library-owned-model-properties 
    := shallow-copy(ld.library-owned-model-properties);
  project-library-interactive-context(ld, target) := ild;
  make-interactive-language-definition(ild, force-shadow?: force-shadow?);
  ild.all-library-descriptions
    := map(method (ld) project-library-interactive-context(ld, target) end,
	   ld.all-library-descriptions);
  ild.library-description-dylan-library
    := project-library-interactive-context
         (ld.library-description-dylan-library, target);
  ild
end;


define method make-interactive-context (ld :: <dylan-project-library-description>,
					target, #key force-shadow?)
  let ild = next-method();
  if (ild.interactive-library-shadowed?)
    install-dylan-boot-constants(ild);
  end;
  ild
end method;

define method close-library-description
    (ild :: <interactive-library-description>)
  let ld = ild.interactive-library-project-library;
  let target = ild.interactive-library-target;
  project-library-interactive-context(ld, target) := #f;
  ild.language-definition := #f;
  #f
end method;



define abstract class <interactive-namespace> (<namespace>)
  constant slot interactive-namespace-project-namespace :: <namespace>,
    required-init-keyword: original:;
end;

define class <interactive-library> (<full-library>, <interactive-namespace>)
  constant slot interactive-binding-properties = make(<table>);
  // True if all properties from base library have been copied.
  slot interactive-library-detached? :: <boolean> = #f;
end;

define function interactive-library-shadowed?
    (ild :: <interactive-library-description>)
  instance?(ild.language-definition, <interactive-library>);
end;

define method interactive-library-detached? (lib :: <library>)
 => (nope :: singleton(#f))
  #f
end;

define method interactive-library-detached?
    (ld :: <interactive-library-description>) => (well? :: <boolean>)
  ld.language-definition.interactive-library-detached?
end method;

define class <interactive-boot-library> (<full-boot-library>, <interactive-library>) end;

define class <interactive-module> (<full-module>, <interactive-namespace>) end;

define class <interactive-dylan-user-module> (<full-dylan-user-module>, <interactive-module>) end;

define interactive-class-mapping
  <library> => <interactive-library>;
  <boot-library> => <interactive-boot-library>;
  <module> => <interactive-module>;
  <dylan-user-module> => <interactive-dylan-user-module>;
end;

define class <interactive-canonical-module-binding-properties>
    (<canonical-module-binding-properties>)
  slot shadowable-binding-previous-definition /* :: false-or(<top-level-form>) */ = #f,
    init-keyword: previous-definition:;
  slot interactive-binding-project-dependents,
    required-init-keyword: dependents:;
  slot interactive-binding-local-dependents :: <sequence> = #();
end;

define class <interactive-imported-module-binding-properties>
    (<imported-module-binding-properties>)
  slot shadowable-binding-previous-definition /* :: false-or(<top-level-form>) */ = #f,
    init-keyword: previous-definition:;
  slot interactive-binding-project-dependents,
    required-init-keyword: dependents:;
  slot interactive-binding-local-dependents :: <list> = #();
end;

define method binding-local-dependents-in-context
    (ld :: <interactive-library-description>, b :: <module-binding>)
 => (deps :: <sequence>)
  let p = ld.interactive-library-shadowed?
             & binding-properties-in-context(ld, b, #f);
  let ideps = if (p) p.interactive-binding-local-dependents else #() end;
  let pdeps = if (p)
		dood-maybe-force-address-proxy
		  (p.interactive-binding-project-dependents)
	      end | b.shadowable-binding-local-dependents;
  if (empty?(pdeps))
    ideps
  elseif (empty?(ideps))
    pdeps
  else
    p.interactive-binding-project-dependents := #[];
    p.interactive-binding-local-dependents
      := concatenate!(ideps,
		      if (instance?(pdeps, <list>)) copy-sequence(pdeps)
		      else as(<list>, pdeps) end);
  end
end method;

define method register-binding-dependent-in-context
    (ld :: <interactive-library-description>, b :: <module-binding>, dep)
  debug-assert(ld.interactive-library-shadowed?,
	       "Registering dependencies in an unshadowed library?");
  let p = binding-properties-in-context(ld, b, #t);
  p.interactive-binding-local-dependents
    := pair(dep, as(<list>, p.interactive-binding-local-dependents));
end method;

define method unregister-binding-dependent-in-context
    (ld :: <interactive-library-description>, b :: <module-binding>, dep)
  // This method should never get called because we don't do triggering
  // in interactive, so we should never retract anything.
  debug-assert(#f, "Unregistering dependencies in an interactive compile??");
/*
  debug-assert(ld.interactive-library-shadowed?,
	       "Unregistering dependencies in an unshadowed library?");
  let p = binding-properties-in-context(ld, b, #t);
  p.interactive-binding-local-dependents
    := remove!(as(<list>, p.interactive-binding-local-dependents), dep);
*/
end method;

define interactive-class-mapping
  <canonical-module-binding>
    => <interactive-canonical-module-binding-properties>;
  <imported-module-binding>
    => <interactive-imported-module-binding-properties>;
end;

define method shadow-form-properties (p :: <form-properties>)
  shadow-properties(p)
end method;

// Don't shadow an entire form, just the interesting stuff.
define method shadow-form-properties (form :: <top-level-form>)
  let p = make-default-form-properties(form);
  merge-form-properties!(p, form);
  p
end method;

define method shadow-binding-properties (p ::  <interactive-canonical-module-binding-properties>)
  debug-assert(object-class(p) == <interactive-canonical-module-binding-properties>);
  shadow-properties(p)
end method;

define method shadow-binding-properties (p ::  <interactive-imported-module-binding-properties>)
  debug-assert(object-class(p) == <interactive-imported-module-binding-properties>);
  shadow-properties(p)
end method;

/*
// This tries to make it so interactively created bindings are also installed
// in the project library, so they will be eq.  However, this won't work in the
// face of module redefining anyway, so don't bother, and besides it makes
// it much more difficult to do binding-interactive?
define method new-binding-in (im :: <interactive-module>, name :: <symbol>)
  let m = im.interactive-namespace-project-namespace;
  let new-binding = lookup-name(m, name, default: #f)
                     | new-binding-in(m, name);
  define-name(im, name, new-binding);
  new-binding
end method;
*/

define method binding-interactive? (binding :: <module-binding>)
  instance?(binding.binding-home, <interactive-module>)
end method;


define method initial-binding-properties
    (b :: <canonical-module-binding>, models?, dependents?)
  let p = b.canonical-binding-properties;
  if (p)
    let def = p.shadowable-binding-active-definition;
    let mdefs = p.shadowable-binding-local-modifying-definitions;
    // Would be nice if could find out whether empty without loading it,
    // then could store the dood proxy.
    let deps = dependents? & as(<vector>, b.shadowable-binding-local-dependents);
    if (def | ~empty?(mdefs) | (deps & ~empty?(deps)))
      if (models? & def & ~def.form-interactive?)
	make(<interactive-canonical-module-binding-properties>,
	     definition: def,
	     modifying-definitions: copy-sequence(mdefs),
	     dependents: deps,
	     cached-model: p.shadowable-binding-cached-model-object)
      else
	make(<interactive-canonical-module-binding-properties>,
	     definition: def,
	     dependents: deps,
	     modifying-definitions: copy-sequence(mdefs))
      end
    end
  end
end;

define method initial-binding-properties
    (b :: <imported-module-binding>, models?, dependents?)
  let mdefs = b.shadowable-binding-local-modifying-definitions;
  // Would be nice if could find out whether empty without loading it,
  // then could store the dood proxy.
  let deps = dependents? & as(<vector>, b.shadowable-binding-local-dependents);
  if (~empty?(mdefs) | (deps & ~empty?(deps)))
    make(<interactive-imported-module-binding-properties>,
	 dependents: deps,
	 modifying-definitions: copy-sequence(mdefs))
  end
end;


define function detach-interactive-library (ild, models?)
  let ilib = ild.language-definition;
  unless (ilib.interactive-library-detached?)
    let proj-ld = ild.interactive-library-project-library;
    let form-props-table = ild.interactive-form-properties;
    local method detach-forms-of (b :: <module-binding-properties>)
	    do(method (form :: <top-level-form>)
		 debug-assert(form.form-original-library == proj-ld);
		 unless (element(form-props-table, form, default: #f))
		   let props = initial-form-properties(form, models?);
		   if (props) form-props-table[form] := props end;
		 end;
	       end method,
	       b.shadowable-binding-local-modifying-definitions);
	  end;
    let binding-props-table = ilib.interactive-binding-properties;
    local method detach-binding (b :: <module-binding>)
	    let props = element(binding-props-table, b, default: #f);
	    if (props)
	      unless (props.interactive-binding-project-dependents)
		let deps = b.private-shadowable-binding-local-dependents;
		props.interactive-binding-project-dependents
		  := if (dood-lazy-value?(deps)) deps else as(<vector>, deps) end
	      end;
	    else
	      let props = initial-binding-properties(b, models?, #t);
	      if (props) binding-props-table[b] := props end;
	    end;
	  end;
    do-imported-bindings
      (ilib, method (b)
	       if (models?) detach-forms-of(b) end;
	       detach-binding(b);
	     end);
    for (lb in ilib.namespace-local-bindings)
      if (defined?(lb))
	let module = lb.library-binding-value;
	for (b in module.namespace-local-bindings)
	  if (models?)
	    let bp = b.canonical-binding-properties;
	    if (bp) detach-forms-of(bp) end;
	  end;
	  detach-binding(b);
	end;
      end;
    end;
    ilib.interactive-library-detached? := #t;
  end;
end function;

define method make-interactive-language-definition
    (ild :: <interactive-library-description>, #key force-shadow?)
  let ld = ild.interactive-library-project-library;
  let original :: <full-library> = ld.language-definition;
  if (force-shadow? |
      *shadow-system-libraries?* |
      ld.library-description-personal? |
      // Force shadow if any used library shadowed (perhaps by being
      // explicitly force-shadowed previously).
      any?(method (uld :: <project-library-description>)
	     let ui = project-library-interactive-context
	                (uld, ild.interactive-library-target);
	     ui & interactive-library-shadowed?(ui)
	   end,
	   ld.all-used-library-descriptions))
    debug-assert(empty?(library-deleted-modules(original)));
    let ilib = make(interactive-class-for(original),
		    original: original,
		    used-libraries: ild.used-library-table,
		    // This is needed in case it's a boot-library...
		    description: ld);
    ild.language-definition := ilib;
    copy-namespace-slots(ilib);
    let new-dups = ilib.library-duplicate-definitions;
    for (dups keyed-by binding in original.library-duplicate-definitions)
      new-dups[binding] := copy-sequence(dups);
    end;
    let new-ignores = ilib.library-ignored-definitions;
    for (ignores keyed-by binding in original.library-ignored-definitions)
      new-ignores[binding] := copy-sequence(ignores);
    end;
    // Not clear how we will handle *new* imported bindings, and if
    // can match them up with new imported bindings created in the
    // project...
    let itables = ilib.imported-bindings-tables;
    for (original-table keyed-by key in original.imported-bindings-tables)
      let table = make(<imported-bindings-table>);
      copy-table-into(table, original-table);
      itables[key] := table;
    end;

    // These are only used in note-changing-definition to trigger
    // dependencies.  Since we don't do dependency triggering in interactive,
    // we don't really need these.. Although it shouldn't be a big
    // deal since there aren't many definer macros in any library anyhow.
    /*
      let new-definers = ilib.library-definer-references;
      for (refs keyed-by b in original.library-definer-references)
	new-definers[b] := copy-sequence(refs);
      end;
    */
    let new-definitions = ilib.namespace-local-bindings;
    for (lb keyed-by key in original.namespace-local-bindings)
      let imodule = defined?(lb) &
	             make-interactive-module(ild, lb.library-binding-value);
      debug-assert(lb.library-binding-home == original);
      new-definitions[key] := make(<library-binding>,
				    name: lb.name,
				    home: ilib,
				    exported?: lb.exported?,
				    value: imodule);
    end;
    // repopulate imported-name-cache
    do(method(imported-name)
	   lookup-name(ilib, imported-name, default: $name-not-imported)
       end,
       original.imported-name-cache.key-sequence);
    ilib
  else
    ild.language-definition := original;
  end;
end method;

define function used-libraries-for-target
    (ld :: <project-library-description>, target)
 => (table :: <ordered-object-table>)
  let table = make(<ordered-object-table>);
  for (ul keyed-by name in ld.used-library-table)
    let ild = lookup-interactive-context(target,
					 ul.used-library-description,
					 force-shadow?: #f);
    table[name] := make(<used-library>,
			description: ild,
			model-change-count: ild.language-definition-change-count,
			major-version: ul.used-library-major-version,
			minor-version: ul.used-library-minor-version,
			binding: ul.used-library-binding,
			count: 0);
  end;
  table
end;

define method make-interactive-module 
    (ild :: <interactive-library-description>, original :: <full-module>)
  let ilib = ild.language-definition;
  debug-assert(original.namespace-original-library
		 == ild.interactive-library-project-library);
  let space = make(interactive-class-for(original),
		   original: original,
		   library: ilib);
  copy-namespace-slots(space);
  copy-table-into(space.namespace-local-bindings,
		  original.namespace-local-bindings);
  copy-table-into(space.imported-name-cache,
		  original.imported-name-cache);
  copy-table-into(space.module-definer-bindings,
		  original.module-definer-bindings);
  space
end method;

define function copy-namespace-slots (space :: <interactive-namespace>)
  let original :: <full-namespace> = space.interactive-namespace-project-namespace;
  space.debug-name := original.debug-name;
  space.namespace-definition := original.namespace-definition;
  space.namespace-model := original.namespace-model;
  space.use-clauses := original.use-clauses;
  space.create-clauses := original.create-clauses;
  space.export-clauses := original.export-clauses;
  space.emitted-name := original.emitted-name; // Is this right?
  copy-table-into(space.exported-names, original.exported-names);
  copy-table-into(space.created-names, original.created-names);
end;

//// Interactor bindings.

// These correspond to the pseudo-bindings made available in the interactor
// for interactor history variables and local variables visible in the
// current stack frame.

define class <interactor-binding> (<named-object>, <binding>)
  // This slot contains whatever we get from the runtime manager - we
  // just hand it back later.
  constant slot binding-interactor-id,
    required-init-keyword: interactor-id:;
end class;           

// eof
