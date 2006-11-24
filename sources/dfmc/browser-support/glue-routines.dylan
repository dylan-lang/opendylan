module: dfmc-browser-support
Synopsis: Browser-specific routines
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define macro with-context
  { with-context (?context:expression) ?:body end }
    => { dfmc-do-with-library-context(method () ?body end, ?context) }
end macro;

define macro with-form-context
  { with-form-context (?form:expression) ?:body end }
    => { dfmc-do-with-library-context
	  (method () ?body end, ?form.source-form-context) }
end macro;				      

define macro with-no-context
  { with-no-context ?:body end }
    => { dynamic-bind (dfmc-*library-description* = #f) ?body end }
end macro;

////
////                 COMPILATION-CONTEXT-SOURCES
////
define sealed class <compiler-source-record-sequence> (<sequence>)
  constant slot source-sequence-records :: dfmc-<compilation-record-vector>,
    required-init-keyword: records:;
end class;

define method type-for-copy (seq :: <compiler-source-record-sequence>)
 => (type :: singleton(<simple-object-vector>))
  <simple-object-vector>
end;

define sealed method size (seq :: <compiler-source-record-sequence>)
  size(seq.source-sequence-records)
end method;

define sealed method element (seq :: <compiler-source-record-sequence>,
			      index :: <integer>, 
			      #key default = unsupplied())
 => (res :: dfmc-<source-record>)
  if (supplied?(default))
    let cr = element(seq.source-sequence-records, index, default: not-found());
    if (found?(cr)) cr.dfmc-compilation-record-source-record else default end
  else
    seq.source-sequence-records[index].dfmc-compilation-record-source-record
  end
end method;

define sealed method forward-iteration-protocol
    (seq :: <compiler-source-record-sequence>)
 => (i, s, n :: <function>, f? :: <function>,k :: <function>, 
     e :: <function>, es :: <function>, c :: <function>);
  local method next (seq, i) => res; i + 1 end;
  local method finished? (seq, i, limit) => res; i == limit end;
  local method key (seq, i) => res; i end;
  local method elt (seq, i) => res;
	  let cr = seq.source-sequence-records[i];
	  cr.dfmc-compilation-record-source-record
	end;
  local method elt-setter (val, seq, i) => res; seq[i] := val end;
  local method copy (seq, i) => res; i end;
  values(0, seq.source-sequence-records.size,
	 next, finished?, key, elt, elt-setter, copy)
end method;

define function compilation-context-sources (cc :: dfmc-<library-description>)
 => (sr* :: <compiler-source-record-sequence>);
  make(<compiler-source-record-sequence>,
       records: as(dfmc-<compilation-record-vector>, 
		   dfmc-library-description-compilation-records(cc)))
end function;

define function compilation-context-version (cc :: dfmc-<library-description>)
 => (major-version, minor-version, time-stamp)
  values(cc.dfmc-library-description-major-version,
	 cc.dfmc-library-description-minor-version,
	 as(<machine-word>, cc.dfmc-library-description-change-count))
end function;

define function compilation-context-compiler-settings
    (ld :: dfmc-<library-description>)
  list(operating-system: ld.dfmc-library-description-os-name,
       processor: ld.dfmc-library-description-processor-name,
	   back-end: ld.dfmc-library-description-compiler-back-end-name,
       mode: ld.dfmc-library-description-compilation-mode,
       build-location: ld.dfmc-library-description-build-location,
       library-pack: ld.dfmc-library-description-library-pack)
end function;

define function compilation-context-compiler-settings-setter
    (settings, ld :: dfmc-<library-description>)
  local method setter (ld, #key operating-system = unsupplied(),
		                processor = unsupplied(),
                        back-end = unsupplied(),
		                mode = unsupplied(),
		                build-location = unsupplied(),
		                library-pack = unsupplied())
	  if (supplied?(operating-system))
	    ld.dfmc-library-description-os-name := operating-system
	  end;
	  if (supplied?(processor))
	    ld.dfmc-library-description-processor-name := processor
	  end;
      if (supplied?(back-end))
        ld.dfmc-library-description-compiler-back-end-name := back-end
	  end;
      if (supplied?(mode))
	    ld.dfmc-library-description-compilation-mode := mode
	  end;
	  if (supplied?(build-location))
	    ld.dfmc-library-description-build-location := build-location
	  end;
	  if (supplied?(library-pack))
	    ld.dfmc-library-description-library-pack := library-pack
	  end;
	end;
  apply(setter, ld, settings)
end function;


define class <unresolved-variable> (<variable>)
  constant slot unresolved-variable-name :: <symbol>,
    required-init-keyword: name:;
  constant slot unresolved-variable-module-name :: <symbol>,
    required-init-keyword: module:;
end class;

define method variable-name (var :: dfmc-<variable-name-fragment>)
  let name = var.dfmc-fragment-identifier;
  let module = var.dfmc-fragment-context;
  values(name, if (module) module.dfmc-namespace-name else #"internal" end)
end method;

define method variable-name (var :: dfmc-<name-dependency>)
  let name = var.dfmc-dependency-name;
  let module = var.dfmc-dependency-module;
  values(name, module.dfmc-namespace-name)
end method;

  // This isn't quite right - it's supposed to return the module name as it's
// known in the library the variable was referenced from, but that's gone, all
// we have is the home name of the module in the library the module was
// defined in.
define method variable-name (var :: dfmc-<module-binding>)
  let name = var.dfmc-binding-identifier;
  let module = var.dfmc-binding-home;
  values(name, module.dfmc-namespace-name)
end method;

define method variable-name (var :: <unresolved-variable>)
  values(var.unresolved-variable-name, var.unresolved-variable-module-name)
end method;

define function make-variable (name, module-name)
  make(<unresolved-variable>,
       name: as(<symbol>,name),
       module: as(<symbol>, module-name))
end function;

define function dylan-variable (name, #key module = #"dylan")
  make-variable(name, module)
end function;

// Since a binding is a kind of <variable>, we can just return the
// binding as the home variable.  Do that consistently, so can have
// a more efficient same-variable?.
define method variable-home (context :: dfmc-<library-description>,
			     var :: dfmc-<module-binding>)
 => home :: dfmc-<module-binding>;
  dfmc-local-binding-in(context.dfmc-language-definition, var);
end method;

define method variable-home (context :: dfmc-<library-description>,
			     var :: dfmc-<variable-name-fragment>)
 => home :: dfmc-<module-binding>;
  with-context (context)
    dfmc-untracked-lookup-binding(var)
  end
end method;

define method variable-home (context :: dfmc-<library-description>,
			     var :: dfmc-<referenced-variable>)
 => home :: dfmc-<module-binding>;
  with-context (context)
    let name = var.dfmc-referenced-variable-name;
    let module = var.dfmc-referenced-variable-module;
    dfmc-untracked-lookup-binding-in(module, name)
  end
end method;

define method variable-home (context :: dfmc-<library-description>,
			     var :: <unresolved-variable>)
 => home :: dfmc-<module-binding>;
  with-context (context)
    let (name, module-name) = variable-name(var);
    let library = context.dfmc-language-definition;
    let module = dfmc-lookup-module-in(library, module-name);
    dfmc-untracked-lookup-binding-in(module, name)
  end
end method;  

define function same-variable? (context :: dfmc-<library-description>,
				v1 :: <variable>, v2 :: <variable>)
  variable-home(context, v1) == variable-home(context, v2)
end function;

define function same-variable-name? (v1 :: <variable>, v2 :: <variable>)
  let (v1-n, v1-m) = variable-name(v1);
  let (v2-n, v2-m) = variable-name(v2);
  v1-n == v2-n & v1-m == v2-m
end function;

define function project-library-definition (context :: dfmc-<library-description>)
  let library = context.dfmc-language-definition;
  library & library.dfmc-namespace-definition
end function;

define function do-namespace-names (space :: dfmc-<namespace>,
				    function :: <function>,
				    inherited?, internal?, local-bindings?)
  let local-bindings = space.dfmc-namespace-local-bindings;
  if (internal?)
    for (local-binding keyed-by name in local-bindings)
      let kind = if (dfmc-exported-name?(space, name)) #"exported"
		 else #"internal" end;
      if (local-bindings?)
	function(name, kind, local-binding: local-binding)
      else
	function(name, kind)
      end;
    end
  else
    for (name in space.dfmc-exported-names)
      if (local-bindings?)
	function(name, #"exported", local-binding: local-bindings[name])
      else
	function(name, #"exported")
      end;
    end;
  end;
  if (inherited?)
    dfmc-do-imported-names(function, space, internal?: internal?);
  end if;
end function;

define function do-library-modules (context :: dfmc-<library-description>,
				    function :: <function>,
				    #key inherited?, internal?)
  let library = context.dfmc-language-definition;
  debug-assert(library);
  let action = method (name, kind)
		 with-no-context function(name, kind) end
               end;
  do-namespace-names(library, action, inherited?, internal?, #f);
end function;

define function find-module-definition (context :: dfmc-<library-description>,
					name) => (definition, kind)
  let library = context.dfmc-language-definition;
  debug-assert(library);
  let name = as(<symbol>, name);
  let module = dfmc-lookup-module-in(library, name, default: #f);
  let defined? = dfmc-defined-name?(library, name);
  if (module)
    values(module.dfmc-namespace-definition,
	   if (defined?) #"defined" else #"inherited" end)
  end;
end function;

define inline function name-exported-from? (space :: dfmc-<namespace>,
					    name :: <symbol>)
  dfmc-lookup-name(space, name, default: #f, exported?: #t) & #t
end function;

define function module-exported? (context :: dfmc-<library-description>,
				  name)
  name-exported-from?(context.dfmc-language-definition, as(<symbol>, name))
end function;

define function do-module-variables (context :: dfmc-<library-description>,
				     definition :: <module-definition>,
				     function :: <function>,
				     #key inherited?, internal?)
  with-context (context) // needed for fragment-module
    let module-name = definition.dfmc-form-namespace-name;
    let module = dfmc-lookup-module-in(context.dfmc-language-definition,
				       module-name);
    let active-definition = module & module.dfmc-namespace-definition;
    if (definition == active-definition)
      local method variable-function (name, kind, #key local-binding)
	      if (local-binding)
		// ignore local bindings without any properties...
		if (kind == #"exported" |
		      dfmc-canonical-binding-properties(local-binding))
		  with-no-context function(local-binding, kind) end;
		end;
	      else
		let variable = make-variable(name, module-name);
	        with-no-context function(variable, kind) end;
	      end;
	    end method;
      do-namespace-names(module, variable-function, inherited?, internal?, #t);
    end if;
  end with-context;
end function;

define method variable-exported? (context :: dfmc-<library-description>,
				  var :: dfmc-<module-binding>)
  dfmc-exported-name?(var.dfmc-binding-home, var.dfmc-binding-identifier)
end method;

define method variable-exported? (context :: dfmc-<library-description>,
				  var :: dfmc-<variable-name-fragment>)
  with-context (context) // needed for fragment-module.
    name-exported-from?(var.dfmc-fragment-module, var.dfmc-fragment-identifier)
  end
end method;

define method variable-exported? (context :: dfmc-<library-description>,
				  var :: dfmc-<name-dependency>)
  with-context (context)
    name-exported-from?(var.dfmc-dependency-module, var.dfmc-dependency-name)
  end
end method;

define method variable-exported? (context :: dfmc-<library-description>,
				  var :: <unresolved-variable>)
  with-context (context)
    let library = context.dfmc-language-definition;
    let module-name = var.unresolved-variable-module-name;
    let module = dfmc-lookup-module-in(library, module-name);
    name-exported-from?(module, var.unresolved-variable-name)
  end;
end method;

// #t means something not representable in the API.
define constant <type-expression> 
  = type-union(<variable>, singleton(#t), <source-locator>);

define generic externalize-type (type) => (res :: false-or(<type-expression>));

define method externalize-type (type) => (res :: singleton(#t))
  #t
end method;

define method externalize-type (type == #f) => (res :: singleton(#f))
  #f
end method;

define method externalize-type (type :: <variable>) => (res :: <variable>)
  type
end method;

define method externalize-type 
    (type :: dfmc-<fragment>) => (res :: <type-expression>)
  if (instance?(type, <variable>))
    // Hack to work around the fact that <fragment> actually precedes 
    // <variable> in the CPL of a name fragment.
    type
  else
    let loc = dfmc-fragment-source-location(type);
    loc | #t
  end;
end method;

define function source-locator-positions (loc :: <source-locator>)
  values(loc.dfmc-source-location-start-character,
         loc.dfmc-source-location-end-character)
end function;

define function source-locator-lines (loc :: <source-locator>)
  values(loc.dfmc-source-location-start-line,
         loc.dfmc-source-location-end-line)
end function;


define generic source-form-defined-variables
    (form :: <source-form>) => (variables :: <sequence>);

define method source-form-defined-variables (form :: <source-form>)
 => (variables :: <empty-list>)
  #()
end method;

define method source-form-defined-variables (form :: dfmc-<variable-defining-form>)
 => (variables :: <sequence>)
  with-form-context (form)
    form.dfmc-form-variable-names;
  end;
end method;

define method source-form-defined-variables (macro-form :: <macro-form>)
 => (variables :: <sequence>)
  with-form-context (macro-form)
    let variables = #();
    for (form in macro-form.macro-form-expanded-forms)
      for (var in form.source-form-defined-variables)
	variables := add-new!(variables, var);
      end;
    end;
    reverse!(variables)
  end;
end method;

define generic source-form-variable-type
    (form :: <source-form>, variable :: <variable>)
      => (type :: false-or(<type-expression>));

define function explicit-variable-type
    (form :: dfmc-<explicitly-typed-variable-defining-form>,
     variable :: <variable>)
 => (type :: false-or(<type-expression>), found? :: <boolean>)
  with-form-context (form)
    let index = find-key(form.dfmc-form-variable-names,
			 method (v) same-variable-name?(variable, v) end);
    if (index)
      let type = form.dfmc-form-type-expressions[index];
      values(externalize-type(type), #t)
    else
      values(#f, #f)
    end;
  end;
end function;

define method source-form-variable-type (form :: dfmc-<variable-defining-form>,
                                         variable :: <variable>)
 => (type :: singleton(#f))
  // Strictly speaking should err if variable is not defined by the form,
  // but it's not worth checking
  #f;
end method;

define method source-form-variable-type (form :: dfmc-<explicitly-typed-variable-defining-form>,
                                         variable :: <variable>)
 => (type :: false-or(<type-expression>))
  // Strictly speaking should err if not found?, but since the base case
  // doesn't err, we won't either...
  with-form-context (form)
    let (type, found?) = explicit-variable-type(form, variable);
    found? & type
  end;
end method;

define method source-form-variable-type (macro-form :: <macro-form>,
					 variable :: <variable>)
 => (type :: false-or(<type-expression>))
  with-form-context (macro-form)
    block (return)
      for (form in macro-form.macro-form-expanded-forms)
	if (instance?(form, dfmc-<explicitly-typed-variable-defining-form>))
	  let (type, found?) = explicit-variable-type(form, variable);
	  if (found?) return(type) end;
	end if;
      end for;
    end block;
  end;
end method;

// we are not exporting it since this is an internal error in the env
// no assert since we want to continue
define class <invalid-canonical-source-record-error> (<simple-error>) end;

define function canonical-compilation-record (context :: dfmc-<library-description>, sr)
 => (cr :: false-or(dfmc-<compilation-record>))
  let cr = dfmc-source-record-compilation-record(context, sr, default: #f);
  unless(cr)
    signal(make(<invalid-canonical-source-record-error>,
		format-string: "Invalid canonical source record %s passed to the compiler",
		format-arguments: list(sr.source-record-name)))
  end;
  cr
end;
  

define function source-record-top-level-forms (context :: dfmc-<library-description>, sr)
  with-context (context)
    let cr = canonical-compilation-record(context, sr);
    let all-forms = cr & cr.dfmc-compilation-record-top-level-forms;
    choose(method (form)
	     ~instance?(form, dfmc-<missing-variable-defining-form>) &
	       form.dfmc-form-parent-form == #f
	   end,
	   all-forms | #())
  end;
end function;

define function source-record-dispatch-decisions (context :: dfmc-<library-description>, sr) => (dds :: false-or(<vector>))
  with-context (context)
    let cr = dfmc-source-record-compilation-record(context, sr, default: #f);
    let dds = cr & cr.dfmc-compilation-record-dispatch-decisions;
    // Do not return dispatch decisions if they're still in the process
    // of being computed (i.e. before they are turned into a vector).
    instance?(dds, <vector>) & dds
  end;
end;

define function macro-form-expanded-forms (macro-form :: <macro-form>)
  with-form-context (macro-form)
    choose(method(form) form.dfmc-form-parent-form == macro-form end,
	   macro-form.dfmc-form-derived-forms)
  end;
end function;

define function module-definition-name (form :: <module-definition>)
  dfmc-form-namespace-name(form)
end function;

define function library-definition-name (form :: <library-definition>)
  dfmc-form-namespace-name(form)
end function;

define function module-definition-used-modules (form :: <module-definition>)
  map(dfmc-used-name, form.dfmc-form-use-clauses)
end function;

define function library-definition-used-libraries (form :: <library-definition>)
  map(dfmc-used-name, form.dfmc-form-use-clauses)
end function;

define function class-definition-superclass-types (form :: <class-definition>)
  with-form-context (form)
    map(externalize-type, dfmc-form-superclass-expressions(form))
  end;
end function;

define function class-definition-init-keywords (form :: <class-definition>)
  with-form-context (form)
    map(dfmc-spec-init-keyword, form.dfmc-form-keyword-specs)
  end;
end function;

define function keyword-spec (form :: <class-definition>,
			      keyword :: <symbol>)
  block (return)
    for (spec in form.dfmc-form-keyword-specs)
      if (keyword == spec.dfmc-spec-keyword-expression) return(spec) end;
    end for;
  end block;
end function;

define function class-definition-init-keyword-required?
  (form :: <class-definition>, init-keyword)
  with-form-context (form)
    let spec = keyword-spec(form, init-keyword);
    spec.dfmc-spec-init-keyword-required?
  end;
end function;

define function class-definition-init-keyword-init-kind
  (form :: <class-definition>, init-keyword)
  with-form-context (form)
    let spec = keyword-spec(form, init-keyword);
    slot-definition-init-kind(spec)
  end;
end function;

define function class-definition-init-keyword-type
  (form :: <class-definition>, init-keyword)
  with-form-context (form)
    let spec = keyword-spec(form, init-keyword);
    externalize-type(spec.dfmc-spec-type-expression)
  end;
end function;

define function functional-parameters (form :: <functional-definition>)
  with-form-context (form)
    let sig = form.dfmc-form-signature;
    let reqs = sig.dfmc-spec-argument-required-variable-specs;
    let next = sig.dfmc-spec-argument-next-variable-spec;
    let rest? = sig.dfmc-spec-argument-rest?;
    let rest = rest? & sig.dfmc-spec-argument-rest-variable-spec;
    let key? = sig.dfmc-spec-argument-key?;
    let keys = key? & sig.dfmc-spec-argument-key-variable-specs;
    let req-vals = sig.dfmc-spec-value-required-variable-specs;
    let rest-val = sig.dfmc-spec-value-rest-variable-spec;
    local method var (spec)
	    if (spec) spec.dfmc-spec-variable-name end
	  end;
    local method vars (specs)
	    if (specs) map(var, specs) end
	  end;
    values(vars(reqs), var(rest), var(next), vars(keys),
	   vars(req-vals),var(rest-val))
  end;
end function;

define function functional-keys (form :: <functional-definition>)
  with-form-context (form)
    let sig = form.dfmc-form-signature;
    let key? = sig.dfmc-spec-argument-key?;
    let all-keys? = sig.dfmc-spec-argument-all-keys?;
    let keys = key? & sig.dfmc-spec-argument-key-variable-specs;
    local method key (spec)
	    if (spec)
	      let key = spec.dfmc-spec-keyword-expression;
	      if (instance?(key, <symbol>))
		key
	      else
		// What the key name is defaulted, it gets stored as a literal
		key.dfmc-fragment-value
	      end
	    end;
	  end;
    values(if (keys) map(key, keys) end, all-keys?)
  end;
end function;

define function functional-parameter-types (form :: <functional-definition>)
  with-form-context (form)
    let sig = form.dfmc-form-signature;
    let reqs = sig.dfmc-spec-argument-required-variable-specs;
    let next = sig.dfmc-spec-argument-next-variable-spec;
    let rest? = sig.dfmc-spec-argument-rest?;
    let rest = rest? & sig.dfmc-spec-argument-rest-variable-spec;
    let key? = sig.dfmc-spec-argument-key?;
    let keys = key? & sig.dfmc-spec-argument-key-variable-specs;
    let req-vals = sig.dfmc-spec-value-required-variable-specs;
    let rest-val = sig.dfmc-spec-value-rest-variable-spec;
    local method var (spec)
	    spec & externalize-type(spec.dfmc-spec-type-expression)
	  end;
    local method vars (specs)
	    specs & map(var, specs)
	  end;
    values (vars(reqs), var(rest), var(next), vars(keys),
	    vars(req-vals), var(rest-val))
  end;
end function;

define function generic-definition-options (form :: <generic-definition>)
  with-form-context (form)
    dfmc-form-options(form)
  end;
end function;

define function domain-definition-domain-types (form :: <domain-definition>)
  with-form-context (form)
    map(externalize-type, form.dfmc-form-domain-type-expressions)
  end;
end function;

define function slot-definition-init-kind (form :: <slot-definition>)
  with-form-context (form)
    if (form.dfmc-spec-init-supplied?)
      if (form.dfmc-spec-init-expression?)
	#"init-expression"		// #"="?
      elseif (form.dfmc-spec-init-value?)
	#"init-value"
      else
	#"init-function"
      end
    elseif (form.dfmc-spec-init-keyword-required?)
      #"init-keyword"
    end
  end;
end function;

define function slot-definition-keyword (form :: <slot-definition>)
  with-form-context (form)
    values(form.dfmc-spec-init-keyword, form.dfmc-spec-init-keyword-required?)
  end;
end function;

define function slot-definition-class-definition (form :: <slot-definition>)
  with-form-context (form)
    form.dfmc-form-parent-form
  end;
end function;

define function slot-definition-type (form :: <slot-definition>)
  with-form-context (form)
    externalize-type(form.dfmc-spec-type-expression)
  end;
end function;


/***********************************************************************/
//  Global derived info

define function variable-active-definition
    (context :: dfmc-<library-description>, var :: <variable>)
  with-context (context)
    let binding = variable-home(context, var);
    let def = dfmc-untracked-binding-definition(binding, default: #f);
    def & ~instance?(def, dfmc-<missing-variable-defining-form>) & def
  end
end function;

define function variable-active-method-definitions
    (context :: dfmc-<library-description>, var :: <variable>)
  with-context (context)
   let binding = variable-home(context, var);
   dfmc-choose-instances(<method-definition>,
			 dfmc-untracked-binding-modifying-definitions(binding))
  end
end function;

define function variable-all-definitions
    (context :: dfmc-<library-description>, var :: <variable>)
  with-context (context)
    let binding = variable-home(context, var);
    concatenate(dfmc-untracked-binding-all-definitions(binding),
		choose(method (form)
			 instance?(form, <method-definition>) |
			   instance?(form, <domain-definition>)
		       end,
		       dfmc-untracked-binding-all-modifying-definitions(binding)))
  end
end function;

define function source-form-browsing-context
    (context :: dfmc-<library-description>, form :: <source-form>)
  with-context (context)
    dfmc-form-library(form)
  end;
end function;

define function class-binding-in-context
    (context :: dfmc-<library-description>, form :: <class-definition>)
  let var = dfmc-form-variable-name(form);
  unless (dfmc-form-ignored?(form))
    dfmc-form-variable-binding(form)
  end;
end function;

define method class-direct-subclass-definitions
    (context :: dfmc-<library-description>, form :: <source-form>)
  // Someday, might know about define constant x = make(<class>), but not now
  #f
end method;

define method class-direct-subclass-definitions
    (context :: dfmc-<library-description>, form :: <class-definition>)
  with-context (context)
    let binding = class-binding-in-context(context, form);
    if (binding)
      dfmc-choose-instances(<class-definition>,
			    dfmc-untracked-binding-modifying-definitions(binding))
    end
  end
end method;

define method class-all-superclass-definitions
    (context :: dfmc-<library-description>, form :: <source-form>)
  #f
end method;

define method class-all-superclass-definitions
    (context :: dfmc-<library-description>, form :: <class-definition>)
  with-context (context)
    let binding = class-binding-in-context(context, form);
    if (binding)
      let (c, c?) = dfmc-untracked-binding-model-object-if-computed(binding);
      if (c?)
	let defs = map(dfmc-model-definition, dfmc-^all-superclasses(c));
	if (~member?(#f, defs))
	  defs
	end if;
      end if;
    end if;
  end with-context;
end method;

define method class-all-slot-definitions
    (context :: dfmc-<library-description>, form :: <source-form>)
  #f
end method;

define method class-all-slot-definitions
    (context :: dfmc-<library-description>, form :: <class-definition>)
  with-context (context)
    let binding = class-binding-in-context(context, form);
    if (binding)
      let (c, c?) = dfmc-untracked-binding-model-object-if-computed(binding);
      if (c?)
	let sds = dfmc-^slot-descriptors(c);
	if (sds)
	  let defs = map(dfmc-model-definition,  sds);
	  if (~member?(#f, defs))
	    defs
	  end;
	end;
      end;
    end;
  end with-context
end method;

define method class-direct-method-definitions
    (context :: dfmc-<library-description>, form :: <source-form>)
  #f
end method;

define method direct-method-on? (form :: <source-form>, binding)
  #f
end method;

define method direct-method-on? (form :: <method-definition>, binding)
  let sig = dfmc-form-signature(form);
  any?(method (vspec)
	 let type = dfmc-spec-type-expression(vspec);
	 instance?(type, dfmc-<variable-name-fragment>) &
	   dfmc-untracked-lookup-binding(type) == binding
       end method,
       sig.dfmc-spec-argument-required-variable-specs)
end method;

define method class-direct-method-definitions
    (context :: dfmc-<library-description>, form :: <class-definition>)
  // TODO: should the compiler pre-compute these?
  with-context (context)
    let binding = class-binding-in-context(context, form);
    if (binding)
      reduce(method (methods, def)
	       if (direct-method-on?(def, binding))
		 pair(def, methods)
	       else
		 methods
	       end
	     end method,
	     #(),
	     dfmc-binding-local-referers(binding))
    end;
  end with-context;
end method;


// These next two return a lot of internal info.  Could probably narrow it down
// to refs directly referenced from source by only selecting names in the
// module of the form...
define method source-form-referenced-variables
    (context :: dfmc-<library-description>, form :: <source-form>, #key kind)
  with-context (context)
    dfmc-form-referenced-binding-variables(form)
      | dfmc-choose-name-dependencies(form, dfmc-dep$name-binding-ref);
  end;
end method;

define method source-form-referenced-macros
    (context :: dfmc-<library-description>, form :: <source-form>)
  with-context (context)
    dfmc-form-referenced-macro-variables(form)
      | dfmc-choose-name-dependencies(form, dfmc-dep$name-macro-ref);
  end;
end method;

define method variable-referencing-forms
    (context :: dfmc-<library-description>, var :: <variable>, #key kind)
  with-context (context)
    let binding = variable-home(context, var);
    dfmc-binding-local-referers(binding);
  end;
end method;


/// Warnings
define function program-note-message (note :: <program-note>)
  apply(format-to-string,
	condition-format-string(note),
	condition-format-arguments(note))
end;

define function program-note-creator (note :: <program-note>)
 => (creator :: type-union(<source-form>, dfmc-<source-record>, singleton(#f)));
  let creator = dfmc-condition-program-note-creator(note);
  if (instance?(creator, dfmc-<compilation-record>))
    dfmc-compilation-record-source-record(creator)
  else
    creator
  end
end function;

define function program-note-< (n1 :: <program-note>, n2 :: <program-note>)
  let c1 = n1.dfmc-condition-program-note-creator;
  let c2 = n2.dfmc-condition-program-note-creator;
  ~c1 | (c2 & dfmc-defined-after?(c1, c2))
end;

define inline function choose-notes
    (cc :: dfmc-<library-description>, test :: <function>)
 => (notes :: <vector>)
  let notes = #();
  for (q :: <deque> keyed-by dependent in cc.dfmc-library-conditions-table)
    when (test(dependent))
      notes := concatenate!(as(<list>, q), notes);
    end;
  end;
  sort!(as(<vector>, notes), test: program-note-<, stable: #t);
end;

define method compilation-context-notes (cc :: dfmc-<library-description>)
  with-context (cc)
    choose-notes(cc, method (dep) #t end)
  end;
end method;

define method execution-transaction-notes
    (cc :: dfmc-<interactive-library-description>, transaction-id)
  with-context (cc)
    choose-notes(cc, method (dep)
		       dep == transaction-id
			 | ((instance?(dep, dfmc-<compilation-record>)
			       | instance?(dep, dfmc-<top-level-form>))
			      & dfmc-compilation-record-transaction-id
			          (dfmc-compilation-record-of(dep)) == transaction-id)
		     end);
  end;
end method;

define function source-record-notes
    (cc :: dfmc-<library-description>, sr)
  let cr = dfmc-source-record-compilation-record(cc, sr);
  with-context (cc)
    choose-notes(cc, method (dep)
		       dep == cr
			 | (instance?(dep, dfmc-<top-level-form>)
			      & dep.dfmc-form-compilation-record == cr)
		     end)
  end;
end function;

define function source-form-notes
    (context :: dfmc-<library-description>, form :: <source-form>)
  with-context (context)
    element(context.dfmc-library-conditions-table, form, default: #())
  end
end function;

///// 
////   Project compilation
/////
define dfmc-program-error <library-pack-not-installed>
  slot condition-project, init-keyword: project:;
  slot condition-library-pack, init-keyword: library-pack:;
  format-string "Library Pack %d must be installed to use %s";
  format-arguments library-pack, project;
end dfmc-program-error <library-pack-not-installed>;

define function open-compilation-context
    (project, #key database-location, profile-location, build-settings, read-only?, 
     load-namespace? = #t)
  let (ld, database-already-exists?)
    = dfmc-make-library-description(project,
				    database-location: database-location,
				    profile-location: profile-location,
				    build-settings: build-settings,
				    read-only?: read-only?,
				    load-namespace?: load-namespace?);
  ld
end function;

define function compilation-context-project
    (context :: dfmc-<library-description>)
  dfmc-library-description-project(context)
end function;

define function compilation-context-database-location
    (context :: dfmc-<project-library-description>)
  dfmc-library-description-database-location(context)
end function;

// define function compilation-context-profile-location
//     (context :: dfmc-<project-library-description>)
//   dfmc-library-description-profile-location(context)
// end function;

define function close-compilation-context
    (context :: dfmc-<project-library-description>)
  dfmc-close-library-description(context)
end function;

define function save-compilation-context
    (context :: dfmc-<project-library-description>, #key flush?)
  with-context (context)
    dfmc-save-definition-database(context, flush?: flush?)
  end;
end function;

define method compile-project-definitions
    (context :: dfmc-<project-library-description>, #rest keys,
     #key compile-all?, compile-if-built?, build-settings = #(), strip? = #f,
     #all-keys)
 => (did-it? :: <boolean>)
  // other keys: skip-link?, start-at, skip-emit?, harp-output? = unsupplied(),
  // force-link?, form?, force-emit?, assembler-output?, save?, flush?,
  apply(dfmc-compile-library-from-definitions, context, keys)
end method;

define function used-compilation-contexts
    (context :: dfmc-<library-description>) => (contexts :: <sequence>)
  with-context (context) // ensures language-definition
    dfmc-library-description-used-descriptions(context)
  end;
end function;

// Returns all compilation-contexts used by context, plus context itself,
// sorted so that a library preceeds all the libraries it uses.  context
// itself is first in the list.  The return value is explicitly a list so
// you can use .tail to get just the used contexts.
define function all-known-compilation-contexts
    (context :: dfmc-<library-description>) => (contexts :: <list>)
  with-context (context) // ensures language-definition
    dfmc-all-library-descriptions(context)
  end;
end function;

define function compilation-context-built?
    (context :: dfmc-<library-description>)
  dfmc-library-description-built?(context)
end function;

define function dylan-library-compilation-context?
    (context :: dfmc-<library-description>)
  dfmc-dylan-library-library-description?(context)
end function;


//////
/////  Interactive execution
/////

define function establish-execution-context
    (ld :: dfmc-<project-library-description>, debug-target,
     #key allow-interaction? = #t)
 => (ild :: dfmc-<interactive-library-description>)
  dfmc-lookup-interactive-context(debug-target, ld,
				  force-shadow?: allow-interaction?)
end function;

define function find-execution-context
    (ld :: dfmc-<project-library-description>, debug-target)
 => (ild :: false-or(dfmc-<interactive-library-description>))
  dfmc-lookup-interactive-context(debug-target, ld, default: #f);
end function;


define function release-execution-context
    (ild :: dfmc-<interactive-library-description>)
  dfmc-close-library-description(ild);
end function;

define function condition-database-name(c :: <dood-opening-warning>)
  => (name)
  dfmc-dood-name(dfmc-dood-failed-dood(c))
end;
