Module: dfmc-debug
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Catch anything that doesn't have a more specific method.
define constant <library-designator> = <object>;

define method compile-library-to-definitions (ld :: <library-description>) => ()
  update-library-definitions(ld);
end method;

ignore(compile-library-to-definitions);	// For use in an interactor/emulator ...

define method lookup-library-description (library-key :: <library-designator>)
  project-current-compilation-context(lookup-named-project(library-key))
end method;

define method lookup-library-description (library-key :: <library-description>)
  library-key
end method;

define method lookup-library-description (library-key :: <project>)
  project-current-compilation-context(library-key)
end;

//// alternate entry points for debugging


define function recompile-library (library-key)
  compile-library(library-key, force-parse?: #t, force-recompile?: #t);
end function;

define method update-library-definitions (library-key :: <library-designator>)
  update-library-definitions(lookup-library-description(library-key));
end method;

define method compile-library-to-models (library-key :: <library-designator>)
  compile-library-to-models(lookup-library-description(library-key));
end method;

define method check-library-models (library-key :: <library-designator>)
  check-library-models(lookup-library-description(library-key));
end method;

define method compile-library-to-dfm (library-key :: <library-designator>)
  compile-library-to-dfm(lookup-library-description(library-key));
end method;

define method type-estimate-library (library-key :: <library-designator>)
  type-estimate-library(lookup-library-description(library-key));
end method;

define method interpret-project (library-key :: <library-designator>)
  let ld = lookup-library-description(library-key);
  interpret-library(ld);
end method;

define method interpret-library (description :: <library-description>, #key trace?)
  // Run the runtime-execution over the forms of this library.
  with-program-conditions
    with-library-context (description)
      ensure-library-interpreted(description, trace?: trace?, results?: #f);
    end
  end
end method;
 
define method optimize-library (library-key :: <library-designator>)
  optimize-library(lookup-library-description(library-key));
end method;

define method heap-library (library-key :: <library-designator>, #rest flags) => ()
  apply(heap-library, lookup-library-description(library-key), flags);
end method;

define method save-library
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  apply(save-definition-database, description, flags);
end method;

define method save-library-namespace
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  apply(save-namespace-database, description, flags);
end method;

define method report-library-database-statistics
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  with-program-conditions
    apply(report-definition-database-statistics, description, flags);
  end;
end method;

define method report-recursive-library-database-statistics
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  with-program-conditions
    apply(report-recursive-definition-database-statistics, description, flags);
  end;
end method;

define method report-diff-library-database-statistics
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  with-program-conditions
    apply(report-diff-definition-database-statistics, description, flags);
  end;
end method;

define method report-library-heap-statistics
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  with-program-conditions
    apply(heap-stats, description, flags);
  end;
end method;

define method report-recursive-library-heap-statistics
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  with-program-conditions
    apply(all-heap-stats, description, flags);
  end;
end method;

define method report-diff-library-heap-statistics
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  with-program-conditions
    apply(diff-heap-stats, description, flags);
  end;
end method;

define method save-library-export-only
    (library-key :: <library-designator>, #rest flags) => ()
  let description = lookup-library-description(library-key);
  apply(save-definition-database, description, export-only?: #t, flags);
end method;

ignore(save-library-export-only);	// For use in an interactor/emulator ...

define method compilation-records (library-key :: <library-designator>)
  let description = lookup-library-description(library-key);
  library-description-compilation-records(description)
end method;

ignore(compilation-records);	// For use in an interactor/emulator ...

define method emit-library (library-key :: <library-designator>,
			    #rest flags)
  apply(emit-library, lookup-library-description(library-key), flags);
end method;

define method link-library (library-key :: <library-designator>,
			    #rest flags)
  apply(link-library, lookup-library-description(library-key), flags);
end method;

define method link-glue (library-key :: <library-designator>, #rest keys)
  apply(link-glue,lookup-library-description(library-key), keys);
end method;

define sideways method compile-library-from-definitions
    (library-key :: <library-designator>, #rest keys, #key, #all-keys)
  apply(compile-library-from-definitions,
	lookup-library-description(library-key),
	keys)
end method;



define function try (template, //:: <template-closure>, 
                     #key compiler = compile-library)
 => (ld :: <library-description> , #rest form :: <top-level-form>)
  // Call compile-template & extract the resulting DFM code for template.
  // Giving compiler: compile-library-until-optimized is reasonable, too.
  // Compile library, extract compilation records & init forms.
  let (ld, sr) = compile-template(template, compiler: compiler);
  // Return library and top-level-forms of source template.
  let cr = source-record-compilation-record(ld, sr);
  apply(values, ld, compilation-record-top-level-forms(cr))
end;

ignore(try);			// For use in an interactor/emulator ...

////
////    DRIVERS
////

// Compilation all the way up to definitions.

define method compile-library-until-definitions (library-id) => ()
  update-library-definitions(library-id)
end method;

// Compilation all the way up to models.

define method compile-library-until-models (library-id) => ()
  compile-library-until-definitions(library-id);
  compile-library-to-models(library-id);
end method;

// Compilation all the way up to dfm.

define method compile-library-until-dfm (library-id) => ()
  compile-library-until-models(library-id);
  compile-library-to-dfm(library-id);
end method;

// Compilation all the way up to type inference

define function compile-library-until-type-estimated (library-id) => ()
  compile-library-until-dfm(library-id);
  type-estimate-library(library-id)
end;

// Compilation all the way up to optimisation.

define method compile-library-until-optimized (library-id) => ()
  compile-library-until-type-estimated(library-id);
  optimize-library(library-id);
end method;


// Compilation all the way up to "heaping"(!)

define method compile-library-until-heaped (library-id, #rest flags) => ()
  compile-library-until-optimized(library-id);
  apply(heap-library, library-id, flags);
end method;

// Compilation all the way up to linking.

define method compile-library-until-linked (library-id, #rest flags) => ()
  apply(compile-library-until-heaped, library-id, flags);
  apply(link-library, library-id, flags);
end method;

// Compilation all the way up to glueing.

define method compile-library-until-glued (library-id, #rest flags) => ()
  apply(compile-library-until-linked, library-id, flags);
  link-glue(library-id);
end method;

ignore(compile-library-until-glued);	// For use in an interactor/emulator ...


define method update-library-definitions (ld :: <library-description>) => ()
  parse-project-sources(ld);
end method;

define method compile-library-to-models (description :: <library-description>) => ()
  with-program-conditions
    with-library-context (description)
      ensure-library-models-computed(description);
      ensure-library-models-finished(description);
      ensure-library-models-checked(description);
    end with-library-context;
  end with-program-conditions;
end method;

define method check-library-models (description :: <library-description>)
  with-program-conditions
    with-library-context (description)
      ensure-library-models-checked(description);
    end;
  end;
end method;

define method compile-library-to-dfm (description :: <library-description>)
  with-program-conditions
    with-library-context (description)
      ensure-library-dfm-computed(description);
      ensure-library-bindings-checked(description);
    end with-library-context;
  end with-program-conditions;
end method;

define method type-estimate-library (description :: <library-description>)
  // Run the typist over the forms of this library.
  with-program-conditions
    with-library-context (description)
      ensure-library-type-estimated(description);
    end
  end
end method;

define method optimize-library (description :: <library-description>)
  with-program-conditions
    with-library-context (description)
      ensure-library-optimized(description);
    end;
  end;
end method;

define method heap-library (description :: <library-description>,
			    #rest flags) => ()
  with-program-conditions
    with-library-context (description)
      ensure-library-heaps-computed(description, flags);
    end;
  end;
end method;

define method emit-library (description :: <library-description>,
			    #rest flags)
  with-program-conditions
    with-library-context (description)
      for (cr in library-description-compilation-records(description))
        with-dependent ($compilation of cr)
	  // As soon as we start linking, last build becomes invalid, so clear it
	  description.library-description-built? := #f;
	  let sr = cr.compilation-record-source-record;
	  progress-line("  Emitting %s", sr);
	  apply(emit-all, current-back-end(), cr, flags);
        end;
      end;
    end;
  end;
end method;

define method link-library (ld :: <library-description>,
			    #rest flags)
  with-program-conditions
    with-library-context (ld)
      progress-line("Linking library: %s.", ld);
        // As soon as we start linking, last build becomes invalid, so clear it.
        ld.library-description-built? := #f;
        block ()
          apply(emit-library-records, current-back-end(), ld, flags);
	  //---*** andrewa: this can never have worked because
	  //---*** copy-extra-records isn't imported.
	  // apply(copy-extra-records, ld, flags);
        afterwards 
          progress-line("Linking done.")
        end;
    end;
  end;
end method;

define method link-glue (description :: <library-description>, #rest keys)
  with-program-conditions
    with-library-context (description)
      ensure-library-glue-linked(description, keys)
    end;
  end;
end method;

define method emit-source-record (name :: <string>, library-key :: <library-designator>, #rest flags)
  let description = lookup-library-description(library-key);
  let project = lookup-named-project(library-key);
  with-library-context (description)
    for (cr in library-description-compilation-records(description))
      if (cr.compilation-record-name = name)
	// As soon as we start linking, last build becomes invalid, so clear it
      timing-compilation-phase ("Emitting" of description)
	description.library-description-built? := #f;
	let sr = cr.compilation-record-source-record;
	progress-line("  Emitting %s", sr);
	with-dependent ($compilation of cr)
	  apply(emit-all, current-back-end(), cr, flags);
        end;
        end;
      end if;
    end for;
  end;
end method;

// still some use for this hack

define method link-source-record (name :: <string>, library-key :: <library-designator>, #rest flags)
  let description = lookup-library-description(library-key);
  let project = lookup-named-project(library-key);
  with-library-context (description)
    for (cr in library-description-compilation-records(description))
      if (cr.compilation-record-name = name)
      timing-compilation-phase ("Linking" of description)
	with-dependent ($compilation of cr)
	  let sr = cr.compilation-record-source-record;
	  progress-line("  Linking and Emitting %s", sr);
	  apply(emit-library-records, current-back-end(), description, cr: cr, flags);
        end;
        end;
      end if;
    end for;
  end;
end method;

define method compile-source-record (name :: <string>, library-key :: <library-designator>, #rest flags)
  let retract? = *retract-dfm?*;
  block()
    *retract-dfm?* := #f;
    close-project(library-key);
    compile-library(library-key, force-compile?: #t, skip-emit?: #t, skip-link?: #t, strip?: #f);
    apply(emit-source-record, name, library-key, flags);
    apply(link-source-record, name, library-key, flags);
  cleanup
    *retract-dfm?* := retract?;
  end block;
end method;

define method recompile-source-record (name :: <string>, library-key :: <library-designator>, #rest flags)
  apply(emit-source-record, name, library-key, flags);
  apply(link-source-record, name, library-key, flags);
end method;


define method emit-source-records(library :: <symbol>, #rest sources)
  if (sources.empty?)
    emit-library(library);
  else
    map(method(source :: <string>)
	    emit-source-record(source, library);
	end method,
	sources);
  end if;
end method emit-source-records;

ignore(emit-source-records);	// For use in an interactor/emulator ...

define method link-source-records(library :: <symbol>, #rest sources)
  if (sources.empty?)
    link-library(library);
  else
    map(method(source :: <string>)
	    link-source-record(source, library);
	end method,
	sources);
  end if;
end method link-source-records;

ignore(link-source-records);	// For use in an interactor/emulator ...

// Namespace lookup debugging functions

define function module-in (module, #rest library-opt)
  let library = if (empty?(library-opt)) module else library-opt.first end;
  let ld = lookup-library-description(as(<symbol>, library));
  values(lookup-module-in(ld.language-definition, as(<symbol>, module)), ld)
end function;

define function binding-in (name, #rest module-info)
  let (module, ld) = apply(module-in, module-info);
  with-library-context (ld)
    untracked-lookup-binding-in(module, as(<symbol>, name))
  end;
end function;

define function definition-in (#rest binding-info)
  let (module, ld) = apply(module-in, copy-sequence(binding-info, start: 1));
  with-library-context (ld)
    untracked-binding-definition(apply(binding-in, binding-info));
  end with-library-context;
end function;

define function value-in (#rest binding-info)
  let (module, ld) = apply(module-in, copy-sequence(binding-info, start: 1));
  with-library-context (ld)
    untracked-binding-model-object-if-computed
      (apply(binding-in, binding-info));
  end with-library-context;
end function;



// Interactive stuff

define function execute-string (string, #key module = "internal",
				             library = "dylan",
				             target = #"fake-target",
				             context = #"fake-context",
				             skip-link? = #f,
                                             harp-output? = #t,
                                             dfm-output? = #f,
				             assembler-output? = #f,
				             trace? = #f,
				             interpret? = #f)
  let sr = make(<string-template-source-record>, // Kludge!!!
		contents: as(<byte-vector>, string),
		module: as(<symbol>, module),
		name: "Test");
  let ld = lookup-library-description(library);
  let ild = lookup-interactive-context(target, ld);
  execute-source(ild, context, list(sr),
		 skip-link?: skip-link?,
		 harp-output?: harp-output?,
		 dfm-output?: dfm-output?,
		 assembler-output?: assembler-output?,
		 trace?: trace?,
		 interpret?: interpret?)
end function;

ignore(execute-string);		// For use in an interactor/emulator ...

define function string-complete? (string, #key module = "internal",
				          library = "dylan",
				          target = #"fake-target",
				          context = #"fake-context")
 => (well? :: <boolean>, conditions :: <sequence>)
  let sr = make(<string-template-source-record>, // Kludge!!!
		contents: as(<byte-vector>, string),
		module: as(<symbol>, module),
		name: "Test");
  let ld = lookup-library-description(library);
  let ild = lookup-interactive-context(target, ld);
  source-complete?(ild, context, list(sr));
end function;

define method show-warnings
    (library-id, #key recursive?, personal?, summary?, nonits?,
                      stream = *standard-output*)
  let test = if (nonits?)
	       method (c) ~instance?(c, <binding-defined-but-not-used>) end
	     else
	       method (c) #t end
	     end;
  local method count-warnings (ld)
	  conditions-for(ld, stream, test: test,
			 print-conditions?: #f, summary?: #f)
	end;
  local method print-warnings (ld)
	  conditions-for(ld, stream, test: test,
			 print-conditions?: #t, summary?: #f)
	end;
  let ld = lookup-library-description(library-id);
  if (recursive? | personal?)
    let (wrn, ser, err, ign) = values(0, 0, 0, 0);
    for (uld in ld.all-library-descriptions)
      when (~personal? | uld == ld | uld.library-description-personal?)
	let (w, s, e, i) = count-warnings(uld);
	ign := ign + i;
	wrn := wrn + w;
	ser := ser + s;
	err := err + e;
	unless ((w + s + e) == 0)
	  unless (summary?)
	    format(stream, "/" "/\n/" "/ *** Warnings for library %s ***\n/" "/\n\n",
		   uld.library-description-emit-name);
	    print-warnings(uld);
	  end;
	  format(stream, "Library %s: %s\n",
		 uld.library-description-emit-name,
		 warnings-summary-message(w, s, e, 0));
	end unless;
      end when;
    end for;
    format(stream, "Total: there were %s",
	   warnings-summary-message(wrn, ser, err, ign));
  else
    conditions-for(ld, stream,
		   test: test,
		   print-conditions?: ~summary?,
		   summary?: #t);
  end
end;

define function warnings-summary-message (warnings, serious, errors, nits)
  let nit-msg = if (nits == 0)
		  ""
		else
		  format-to-string("%d ignored warnings, ", nits)
		end;
  let err-msg = if (errors == 0)
		  ""
		else
		  format-to-string(" and %d errors", errors);
		end;
  format-to-string("%s%d warnings, %d serious warnings%s.",
		   nit-msg, warnings, serious, err-msg)
end function;


// Misc utilities

define macro with-c
  { with-c (?ld:expression) ?:body end }
    => { with-library-context (lookup-library-description(?ld))
	   without-dependency-tracking
	     ?body
           end;
         end; }
end macro;

define function delete-dood (ld)
  let read? = read-databases?();
  let write? = write-databases?();
  let ld = block ()
	     use-databases?() := #f;
	     lookup-library-description(ld);
	   cleanup
	     read-databases?() := read?;
	     write-databases?() := write?;
	   end;
  let path = ld.library-description-database-location;
  if (path)
    close-project(ld.library-description-project);
    delete-file(path);
  end;
end function;

ignore(delete-dood);		// For use in an interactor/emulator ...

