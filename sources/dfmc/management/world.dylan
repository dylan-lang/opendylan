module: dfmc-management
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method dood-dfmc-initial-segments
    (class :: subclass(<dfmc-dood>)) => (segments, default-segment);
  let segments
    = vector(make(<dood-typed-segment>, 
		  name: "namespace",
		  type: type-union(<library-description>, 
				   <namespace>, 
				   <module-binding>)),
	     make(<dood-typed-segment>, 
		  name: "definitions",
		  type: type-union(<top-level-form>)),
	     make(<dood-typed-segment>, 
		  name: "models",
		  type: type-union(<model-properties>)),
	     make(<dood-typed-segment>, 
		  name: "code",
		  type: type-union(<lexical-environment>, 
				   <computation>, 
				   <value-reference>, 
				   <body-fragment>)),
	     make(<dood-typed-segment>,
		  name: "debug-info",
		  type: type-union(<lambda-compiled-data>)));
  values(segments, segments[0])
end method dood-dfmc-initial-segments;

// Dump conditions to log file

define function dump-conditions-for(description :: <library-description>)
 => (warning-count, serious-warning-count, error-count);
  with-build-area-output (stream = description,
			  base: as-lowercase(as(<string>,
				library-description-emit-name(description))),
			  type: "log")
    conditions-for(description, stream)
  end
end;

define function conditions-for (description :: <library-description>,
				stream :: <stream>,
				#key test :: <function> = method (c) #t end,
				      summary? = #t,
				      print-conditions? = #t)
 => (warning-count, serious-warning-count, error-count, ignored-count);
  let cond-tab = description.library-conditions-table;
  let (ignored-count, error-count, serious-warning-count, warning-count)
    = values(0,0,0,0);
  let print-context = #f;

  local method dump (key)
	  for (condition in element(cond-tab, key, default: #[]))
	    if (test(condition))
	      when (print-conditions?)
		if (print-context) 
		  format(stream, "//\n// Conditions for %s:\n//\n\n", print-context);
                  print-context := #f;
	        end;
	        format(stream, "%=\n", condition);
              end when;
	      case
		instance?(condition, <serious-program-warning>) =>
		  serious-warning-count := serious-warning-count + 1;
		instance?(condition, <warning>) =>
		  warning-count := warning-count + 1;
		instance?(condition, <error>) =>
		  error-count := error-count + 1;
	      end;
	    else
	      ignored-count := ignored-count + 1
	    end;
          end for;
     end method;

  print-context := "orphans";
  dump(#f);

  // TODO: At the moment conditions with no associated library do not get
  // retracted.  As a temporary measure we zap them here.
  remove-key!(cond-tab, #f); 

  print-context := description;
  dump(description);

  for (cr in description.library-description-compilation-records)
    print-context := cr;
    dump(cr);
    if (cr.compilation-record-top-level-forms)
      for (tlf in cr.compilation-record-top-level-forms) dump(tlf) end
    end
  end for;

  when (summary?)
    let ignor-msg = if (ignored-count == 0)
		      ""
		    else
		      format-to-string("%d ignored warnings, ", ignored-count)
		    end;
    let error-msg = if (error-count == 0)
		      ""
		    else
		      format-to-string(" and %d errors", error-count)
		    end;
    progress-line("There were %s%d warnings, %d serious warnings%s.",
		  ignor-msg, warning-count, serious-warning-count, error-msg);
  end;

  values(warning-count, serious-warning-count, error-count, ignored-count)
end;


define class <abort-compilation> (<simple-condition>)
  constant slot abort-compilation-warnings-count :: <integer>,
    required-init-keyword: warnings:;
  constant slot abort-compilation-serious-warnings-count :: <integer>,
    required-init-keyword: serious-warnings:;
  constant slot abort-compilation-errors-count :: <integer>,
    required-init-keyword: errors:;
end;

// HACK: THIS SHOULD HAPPEN AUTOMATICALLY
ignore(abort-compilation-errors-count);
ignore(abort-compilation-serious-warnings-count);
ignore(abort-compilation-warnings-count);

//// Main driver.

define method ensure-library-compiled (description :: <project-library-description>,
				       #rest flags,
				       #key skip-heaping?,
				       abort-on-all-warnings?, 
				       abort-on-serious-warnings?,
				       #all-keys)
 => (warning-count, serious-warning-count, error-count, data-size, code-size);
  let (warning-count, serious-warning-count, error-count) = values(0,0,0);
  verify-library-before-compile(description);
  with-stage-progress("Computing data models for", $models-stage-time)
    ensure-library-models-computed(description);
    ensure-library-models-finished(description);
    ensure-library-models-checked(description);
  end;
  with-stage-progress("Computing code models for", $dfm-stage-time)
    ensure-library-dfm-computed(description)
  end;
  with-stage-progress("Checking bindings in", $bindings-check-stage-time)
    ensure-library-bindings-checked(description)
  end;
  with-stage-progress("Performing type analysis of", $typist-stage-time)
    ensure-library-type-estimated(description)
  end;
  with-stage-progress("Optimizing", $optimize-stage-time)  
    ensure-library-optimized(description)
  end;

  // TO DO: temporary hack - always write the conditions.log file
  // to clear obsolete files
  unless (#f & empty?(description.library-conditions-table))
    progress-line("Dumping conditions to %s.log",
		  as-lowercase(as(<string>,
				  library-description-emit-name(description))));
    let (warnings, serious-warnings, errors) = 
      dump-conditions-for(description);
    warning-count := warnings;
    serious-warning-count := serious-warnings;
    error-count := errors;
    if(abort-on-all-warnings?) abort-on-serious-warnings? := #t end;
    if(skip-heaping? |
	 (abort-on-all-warnings? & (warning-count > 0))
	 |
	 (abort-on-serious-warnings? & (serious-warning-count > 0)))
      progress-line("Aborting compilation");
      library-progress-text(description, 
			    "There were %d warnings, %d serious warnings and %d errors.",
			    warning-count, serious-warning-count, error-count);

      signal(make(<abort-compilation>,
	     warnings: warning-count,
	     serious-warnings: serious-warning-count,
	     errors: error-count))
    end;

  end;

  if(skip-heaping?)
    signal(make(<abort-compilation>,
	   warnings: 0,
	   serious-warnings: 0,
	   errors: 0))
  end;

  let (data-size, code-size)
    = with-stage-progress("Generating code for", $heaping-stage-time)
	ensure-library-heaps-computed(description, flags)
      end;
  values(warning-count, serious-warning-count, error-count, data-size, code-size);
end method;

// This method can be overriden for testing by specializing on
// <library-description>.
define method verify-library-before-compile (description :: <object>)
end method;

define open generic note-definitions-updated(ld);

// Note that turning this on effectively disables incremental compilation.
define variable *retract-models-after-compilation?* = #f;

define open generic compile-library-from-definitions 
  (description, #key, #all-keys);

define variable *retract-types-after-compilation?* = #t;

define method compile-library-from-definitions 
    (description :: <project-library-description>,
     #rest flags, #key compile-if-built?, compile-all?,
                       abort-on-all-warnings?, abort-on-serious-warnings?,
                       skip-link?, strip?, save?, flush?, stats?, gc?, gc-stats?,
     #all-keys)
 => (did-it? :: <boolean>)
  let strip? = strip? & *strip-enabled?*;
  debug-assert(description.compiled-to-definitions?, "not compiled to definitions?");
  if (~compile-if-built? & description.library-description-built?)
    progress-line("Library %s is up to date.", description);
    #f
  else
    with-program-conditions
      with-ramp-allocation(all?: gc-stats?)
        with-top-level-library-description (description)
	  with-library-context (description)
             debug-assert(begin
			    verify-used-libraries(description);
			    compiled-to-definitions?(description)
			  end,
			  "Dev env didn't ensure used libs for compilation");
	    if (description.models-in-interactive-use?)
	      detach-interactive-namespaces(description);
	    end;
            // TODO: need to verify used libraries
            if (description.library-description-stripped?)
	      // Can't just compile stripped library, since info needed from
	      // definitions may have been stripped. 
	      timing-compilation-phase("Recomputing full definitions" of description)
	        retract-library-parsing(description);
	        compute-library-definitions(description);
	      end;
            elseif (description.models-in-interactive-use?)
              // Compiling an unstripped tightly-compiled library.
              // Can't compile incrementally, since that might modify existing
              // models, which are being pointed to directly from interactive
              // contexts.  Full recompilation is OK though, since that will
              // detach old models without modifying them (knock wood) and then
              // make all new ones.
              retract-library-compilation(description);
            end;
	    if (description.library-description-compilation-aborted?)
	      // TODO: reload from disk database...
	      retract-library-compilation(description);
	    end;
            if (compile-all?)
	      retract-library-compilation(description)
	    end;
            if (description.library-references-retracted-models?)
	      retract-library-compilation(description);
	    end;
            debug-assert(~any?(library-references-retracted-models?,
			       description.all-used-library-descriptions),
		         "Out of date used libraries");
	    block ()
	      description.library-description-compilation-aborted? := #t;
	      if (strip?)
	        description.library-description-stripped? := #"pending";
	      end;
	      let (warning-count, serious-warning-count, error-count, 
		   data-size, code-size) = 
	        apply(ensure-library-compiled, description, flags);
	      
	      unless (skip-link?)
	        with-stage-progress("Linking object files for", $linking-stage-time)
		  ensure-library-glue-linked(description, flags);
	          record-library-build(description);
	        end;
	      end;
              description.library-description-compilation-aborted? := #f;
  
	      if (strip?)
	        ensure-library-stripped(description);
	      end;
              if (save?)
	        with-stage-progress("Saving database for", $save-db-stage-time)
		  timing-compilation-phase("Saving database" of description)
		    with-walk-progress   (progress-line("  Committed %=.", count))
		      ensure-database-saved
		        (description, flush?: flush?, stats?: stats?)
		    end with-walk-progress;
		  end;
	        end;
              end if;
              dump-timings-for(description);
	      // memory stats
	      // mark-garbage();
	      progress-line("  Data %d bytes.", data-size);
	      progress-line("  Code %d bytes.", code-size);
	      let (total-size, free-size) = room();
	      // progress-line(" ====");
	      progress-line(" Heap Allocated %= Total %= Free %=",
			    total-size - free-size, total-size, free-size);
	      // Useful for performance tuning; must be done before retraction.
	      debug-out(#"internal",
		        " Size of type cache = %d\n"
		        " Size of cons cache = %d\n"
		        " Size of disjoint cache = %d\n"
		        " Size of dispatch cache = %d gfs/%d entries\n",
		        size(library-type-cache(description)),
		        size(library-type-estimate-cons-cache(description)),
		        size(library-type-estimate-disjoint?-cache(description)),
		        size(library-type-estimate-dispatch-cache(description)),
		        reduce(method (n, t) (t & (n + t.size)) | n end, 0,
			       library-type-estimate-dispatch-cache(description))
			  );
              progress-line("There were %d warnings, %d serious warnings and %d errors.",
			    warning-count, serious-warning-count, error-count);
              progress-report-text("There were %d warnings, %d serious warnings and %d errors.",
				   warning-count, serious-warning-count, error-count);

	    cleanup
 	      // Clear out cache slots in imported bindings
 	      retract-library-imported-bindings(description);
 	      // TODO: need a way to retract type info when retract models,
 	      // but for now do this to avoid memory leaks.
              if (*retract-types-after-compilation?*)
   	        map(initialize-typist-library-caches,
		    all-library-descriptions(description))
              end if;
	      if (*retract-models-after-compilation?*)
	        retract-models-after-compilation(description);
	      end;
	    end block;
          end with-library-context;
        end with-top-level-library-description;
      end with-ramp-allocation;
    end with-program-conditions;
    if (gc-stats?)
      print-gc-statistics(description)
    end;
    if (gc? & collect-garbage?(gc?))
      signal
	(make(<garbage-collection>,
	      info: as-lowercase(as(<string>,
				    library-description-emit-name(description)))))
    end if;
    #t
  end if;
end method;

/// STRIPPING

define function ensure-library-stripped (ld :: <project-library-description>)
  unless (ld.library-description-stripped? == #t)
    timing-compilation-phase("Stripping" of ld)
      with-program-conditions
	with-library-context (ld)
	  if (ld.compilation-from-definitions-started?)
	    // Have to recompute heaps so can find all owned models.
	    // Have to do it up front so can retract any dependencies it 
	    // generates, or models it recomputes, or any other side-effects.
	    maybe-recompute-library-heaps(ld);
	  end;
	  strip-incremental-slots(ld);
	end;
      end;
    end;
  end;
end function;

define method maybe-recompute-library-heaps
    (ld :: <project-library-description>)
  unless (ld.library-description-combined-record)
    for (cr in compilation-context-records(ld))
      unless (cr.compilation-record-model-heap)
	compute-and-install-compilation-record-heap(cr, skip-emit?: #t);
      end;
    end;
  end;
end method;

define sideways method strip-incremental-slots
    (ld :: <project-library-description>)
  // TODO: this should be without-dependency-tracking
  let library = language-definition(ld);
  // once we start stripping, no longer suitable for incremental recompile
  ld.library-description-stripped? := #"pending";
  ld.library-external-model-cache := make(<table>);
  when (ld.library-description-combined-record)
    strip-incremental-slots(ld.library-description-combined-record);
  end;
  for (cr in library-description-compilation-records(ld))
    strip-incremental-slots(cr);
    compiling-forms ($compilation of form in cr)
      unless (form-ignored?(form)) strip-incremental-slots(form) end;
    end;
  end for;
  strip-library-model-properties(ld);
  let library-def = namespace-definition(library);
  when (library-def)
    for (module in defined-modules-in(library))
      let module-def = module.namespace-definition | library-def;
      with-dependent ($compilation of module-def)
	strip-incremental-slots(module)
      end;
    end for;
  end;
  ld.library-description-stripped? := #t;
end method;

define sideways method strip-incremental-slots (heap :: <model-heap>)
  do(strip-incremental-slots, heap.heap-defined-object-sequence);
  do(strip-incremental-slots, heap.heap-root-system-init-code);
  do(strip-incremental-slots, heap.heap-root-init-code);
end method;

/*
define method string-size (s :: <byte-string>)
  if (size(s) <= 6)
    0
  else
    format-out("%=\n", s);
    round/(size(s), 4) + 2
  end if;
end method;

define method string-size (s)
  0
end method;

define method emitted-name-size (ld :: <compilation-context>)
  let total = 0;
  with-program-conditions
    with-library-context (ld)
      let library = language-definition(ld);
      let library-def = namespace-definition(library);
      for (module in defined-modules-in(library))
	let module-def
	  = if (instance?(module, <dylan-user-module>))
	      // no explicit definition, so use the library...
	      library-def
	    else
	      module.namespace-definition
	    end;
	with-dependent ($compilation of module-def)
	  for (binding in module.namespace-local-bindings)
            total := total + string-size(emitted-name(binding));
	  end for;
	end;
      end for;
      with-dependent ($compilation of library-def)
        do-imported-bindings(library,
			     method (binding)
			       total
				 := total + string-size(emitted-name(binding))
			     end);
      end;
      for (cr in library-description-compilation-records(ld))
        let heap = compilation-record-model-heap(cr);
        for (literal in heap.heap-defined-object-sequence)
          total := total + string-size(emitted-name(literal));
        end for;
      end for;
    end;
  end;
  total * 4
end method;
*/

/// ENSURE EXPORTED ONLY

define method slow-instance? (object, class-name :: <byte-string>)
  let class = object-class(object);
  debug-name(class) = class-name
end method;

define compiler-sideways method dood-disk-object-default
    (dood :: <dfmc-namespace-dood>, object) => (object)
  // format-out("DDOD %s\n", debug-name(object-class(object)));
  if (slow-instance?(object, "<name-dependency>")
	| slow-instance?(object, "<binding-dependency>")
	| slow-instance?(object, "<form-binding-dependency>")
	| slow-instance?(object, "<&iep>")
	| slow-instance?(object, "<&mep>")
	| slow-instance?(object, "<&lambda-xep>")
	| slow-instance?(object, "<&generic-function-xep>")
	| slow-instance?(object, "<stripped-compiled-lambda>")
	| slow-instance?(object, "<fully-compiled-lambda>"))
    #f
  else
    if (slow-instance?(object, "<compilation-record>"))
      compilation-record-back-end-data(object) := #f;
      compilation-record-heap-referenced-objects(object) := #f;
      compilation-record-top-level-forms(object) := #f;
      compilation-record-dispatch-decisions(object) := #();
      remove-all-keys!(compilation-record-dependency-table(object));
    elseif (slow-instance?(object, "<canonical-module-binding>"))
      shadowable-binding-local-dependents(object) := #[];
      binding-local-modifying-definitions(object) := #();
      retract-modifying-models(object);
    elseif (slow-instance?(object, "<imported-module-binding>"))
      shadowable-binding-local-dependents(object) := #[];
      binding-local-modifying-definitions(object) := #();
      retract-modifying-models(object);

      retract-imported-binding(object);
    end if;
    object
  end if
end method;

define compiler-sideways method dood-disk-object
    (dood :: <dfmc-namespace-dood>, object :: <model-properties>) => (object)
  // format-out("DDOM %s\n", debug-name(object-class(object)));
  if (slow-instance?(object, "<&module>") | slow-instance?(object, "<&library>"))
    next-method();
  else
    binding-model-not-computed-proxy(dood)
      | (binding-model-not-computed-proxy(dood)
	   := dood-disk-object(dood, $binding-model-not-computed))
  end if
end method;

define compiler-sideways method dood-disk-object
    (dood :: <dfmc-namespace-dood>, object :: <top-level-form>) => (object)
  // format-out("DDOF %s\n", debug-name(object-class(object)));
  let res = 
  if (slow-instance?(object, "<module-definition>")
	| slow-instance?(object, "<library-definition>")
	| slow-instance?(object, "<macro-definition>")
	| (slow-instance?(object, "<literal-value-constant-definition>")
	     & without-dependency-tracking 
		 let var = form-variable-name(object);
	         let bnd = lookup-binding(var);
		 let mod = binding-model-object(bnd, default: #f);
		 slow-instance?(mod, "<&module>")
		   | slow-instance?(mod, "<&library>")
 	       end without-dependency-tracking))
    next-method();
  else
    #f
  end if;
  // format-out("DDOF %= => %=\n", object, res);
  res
end method;

define sideways method ensure-export-only (ld :: <library-description>)
  strip-incremental-slots(ld);
  remove-all-keys!(library-conditions-table(ld));
  library-description-combined-record(ld) := #f;
  let library = language-definition(ld);

  without-dependency-tracking
    let library-bindings = namespace-local-bindings(library);
    let visible-bindings = make(<object-table>);
    let queue            = make(<deque>);
    // establish visibility sets
    let visible-bindings = make(<object-set>);
    local method visible-binding? (binding :: <module-binding>) => (well?)
	    member?(binding, visible-bindings)
	  end method,
	  method make-visible-binding (binding :: <module-binding>) => (well?)
	    add!(visible-bindings, binding);
	  end method,
	  method macro-binding?
	      (binding :: <module-binding>) => (well? :: false-or(<top-level-form>))
	    let def = untracked-binding-definition(binding, default: #f);
	    instance?(def, <macro-definition>) & def
	  end method,
	  method scan-macro-references 
	      (queue :: <object-deque>, binding :: <module-binding>, def)
	    format-out(" SCANNING MACRO %=\n", def);
	    let object           = form-macro-object(def);
	    let referenced-names = macro-referenced-names(object);
	    for (name in referenced-names)
	      let binding = untracked-lookup-binding(name); // TODO: want binding here
	      unless (visible-binding?(binding))
		format-out("   QUEUEING MREF %=\n", binding);
		push-last(queue, binding); 
	      end unless;
	    end for;
	  end method,
	  method export-binding 
	      (queue :: <object-deque>, binding :: <module-binding>)
	    let macro-def = macro-binding?(binding);
	    if (macro-def)
	      scan-macro-references(queue, binding, macro-def)
	    end if;
	    make-visible-binding(binding);
	  end method,
	  method maybe-export-binding 
	      (queue :: <object-deque>, binding :: <module-binding>)
	    format-out("  CONSIDERING %= DEF? %= IMP? %=\n", 
		       binding, defined?(binding), binding-imported-into-library?(binding));
	    if (defined?(binding) & ~binding-imported-into-library?(binding))
	      format-out("  VISIBLE %=\n", binding);
	      export-binding(queue, binding)
	    end if;
	  end method;
    // trace reachable bindings
    for (library-binding keyed-by module-name in library-bindings)
      let object = library-binding-value(library-binding);
      format-out("CLEANING %=\n", object);
      let local-bindings = namespace-local-bindings(object);
      for (binding in local-bindings)
	maybe-export-binding(queue, binding);
      end for;
      while (~empty?(queue))
	maybe-export-binding(queue, pop(queue));
      end while;
    end for;
    // kill unreachable bindings
    for (library-binding keyed-by module-name in library-bindings)
      let object         = library-binding-value(library-binding);
      let local-bindings = namespace-local-bindings(object);
      format-out("KILLING IN %=\n", object);
      for (name in key-sequence(local-bindings))
	unless (visible-binding?(local-bindings[name]))
	  format-out("  KILLING %=\n", name);
	  remove-key!(local-bindings, name);
	end unless;
      end for;
      remove-all-keys!(imported-name-cache(object));
    end for; 
  end without-dependency-tracking;
  imported-bindings-tables(library) := make(<dood-lazy-table>);
  remove-all-keys!(library-definer-references(library));
end method;

//// Retraction

define method retract-compilation-record-order (cr1, cr2)
  // TODO: retract order-dependent interdependents only.
  retract-compilation-record(cr1);
  retract-compilation-record(cr2);
end method;

define method retract-models-after-compilation (ld :: <library-description>)
  for (ld in all-library-descriptions(ld))
    unless (dylan-library-library-description?(ld))
      retract-library-compilation(ld);
    end unless;
  end for;
end method;

// Retract everything after from models on, forcing a full recompile
// next time around.
define sideways method retract-library-compilation
    (ld :: <project-library-description>)
  if (ld.compilation-from-definitions-started?)
    progress-line("Retracting compilation of %s", ld.library-description-project);
    with-library-context (ld)
     with-dependent-retraction
      let count = ld.library-description-models-change-count;
      initialize-typist-library-caches(ld);
      retract-library-models(ld);
      retract-compilation-timings(ld);
      retract-library-imported-bindings(ld);
      clear-library-model-properties(ld);
      ld.library-description-models-change-count := count + 1;
      ld.compilation-from-definitions-started? := #f;
      if (compiling-dylan-library?())
	install-dylan-boot-constants(ld);
      end;
     end;
    end;
    progress-line("Done.");
  end;
end method;

define method retract-library-models (ld :: <project-library-description>)
  retract-library-copiers(ld);
  remove-dependent-program-conditions(ld, $compilation-mask);
  let ccr = ld.library-description-combined-record;
  when (ccr)
    retract-compilation-record-models(ccr);
    ld.library-description-combined-record := #f;
  end;
  for (cr in ld.library-description-compilation-records)
    retract-compilation-record-models(cr);
    let forms = cr.compilation-record-top-level-forms;
    if (forms)
      do(retract-top-level-form-models, forms);
    end;
  end;
end method;

define method retract-library-models (ld :: <dylan-project-library-description>)
  next-method();
  remove-all-keys!(library-description-dylan-value-cache(ld));
end method;

//// Batch-mode condition handling.

/* TODO: OBSOLETE?
define generic handle-batch-condition (condition :: <condition>) => ();

define method handle-batch-condition (condition :: <error>) => ()
  // Unexpected error.
end method;

define method handle-batch-condition (condition :: <program-error>) => ()
  // Try a skip and continue.
end method;

define method handle-batch-condition (condition :: <program-restart>) => ()
  // If we've got this, there is no sensible restart.
end method;
*/

// Incremental condition handling

define sideways method remove-dependent-program-conditions
    (ld :: <library-description>, stages)
  let cond-tab = ld.library-conditions-table;
  remove-program-conditions-from!(cond-tab, ld, stages);
end method;

define sideways method remove-dependent-program-conditions
    (cr :: <compilation-record>, stages)
  let cond-tab = cr.compilation-record-library.library-conditions-table;
  remove-program-conditions-from!(cond-tab, cr, stages);
end method;

define sideways method remove-dependent-program-conditions
    (form :: <top-level-form>, stages)
  let cond-tab = form.form-library.library-conditions-table;
  remove-program-conditions-from!(cond-tab, form, stages);
end method;




// eof
