Module:    dfmc-shell
Synopsis:  Batch mode compilation handling
Author:    Roman Budzianowski, Andy Armstrong, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *trace-asm?* = #f;


define command-argument-spec
  flags: #("binding", "default-binding"),
  keyword: #"default-binding",
  value?: #t
end;

define command-flag force-compile;

define command-flag force;

define command-flag clean;

define command-argument project;

define command-argument boolean;

define command-flag force-parse;

define command-flag ignore-warnings;

define command-flag seal;

define command-flag combine-dlls;

define command-argument-spec
  flags: #("abort-on-serious-warnings", "abort"),
  keyword: #"abort-on-serious-warnings"
end;

define command-argument-spec
  flags: #("abort-on-any-warnings", "abort-on-any"),
  keyword: #"abort-on-any-warnings"
end;

define command-flag export-only;

define command-flag save;

define command-flag flush;

define command-flag harp;

define command-flag dfm;

define command-flag nodebug;

define command-flag report;

define command-argument library;

define command-argument module;

define command-argument filename;

define command-argument formname;

define command-argument-spec
  flags: #("m"),
  keyword: #"mode",
  value?: #t
end;

define command-argument watchpoint;

define command-flag assemble;

define command-flag gnu;

define command-flag microsoft;

define command-flag nolink;

define command-flag nocompile;

define command-flag dll;

define command-flag exe;

define command-flag force;

define command-argument-spec
  flags: #("recursive", "rec"),
  keyword: #"recursive"
end;

define command-flag not-recursive;

define command-flag personal;

define command-flag summary;

define command-flag nonits;

define command-flag skip-emit;

define command-flag skip-link;

/// User-level commands

define inline function assemble? (assemble) => (assemble?)
  assemble | *trace-asm?* | unsupplied()
end function assemble?;

define dylan-shell-command 
  open (project)
  description "opens a project"
  documentation
  "Usage: OPEN project\n"
  "\n"
  "Opens a project either by library name or filename. Once a project\n"
  "is opened, then compilation of any libraries that use the library\n"
  "defined by the project will use this opened definition.\n"
  hidden? #f
  ensure-project-open(context, project)
end;

define dylan-shell-command
  remove-build-products (project, not-recursive)
  description "removes all build products including compiler databases"
  documentation
  "Usage: REMOVE-BUILD-PRODUCTS [/not-recursive] project\n"
  "\n"
  "Removes all build products including compiler databases\n"
  hidden? #f
  with-project-location-handler (context)
    let project = ensure-project-open(context, project);
    if (project)
      project-remove-build-products(project, recursive?: ~not-recursive);
    else
      user-message(context, "Project % not found", project);
    end;
  end;
end;

define dylan-shell-command 
  import (project)
  description "converts a .LID file into a project"
  documentation
  "Usage: IMPORT lid-file\n"
  "\n"
  "Creates a project file for a given Library Interchange Definition file.\n"
  hidden? #f
  let locator = as(<file-locator>, project);
  let pathname = merge-locators(locator, working-directory());
  if (file-exists?(pathname))
    let project = import-lid-project(pathname);
    if (project)
      user-message(context, "Imported %s as %s\n",
		   pathname, project-location(project))
    else
      user-message(context, "Failed to import %s\n", pathname)
    end
  else
    user-message(context, "Failed to import %s: file does not exist", pathname)
  end
end;

define dylan-shell-command 
  close (project)
  description "closes a project"
  documentation
  "Usage: CLOSE project\n"
  "\n"
  "The close command removes all knowledge of a project from the\n"
  "batch compiler, such that the next time a library is compiled against\n"
  "this library, the compiler will search for it again. This is\n"
  "primarily useful to switch between different versions of a library\n"
  "(say the debug and release versions).\n"
  hidden? #f
  if(close-project(project))
    user-message(context, "Project %s closed\n", project);
  else
    user-message(context, "Failed to close project %s\n", project);
  end;
end;

define dylan-shell-command 
  close-all ()
  description "closes all open projects"
  documentation
  "Usage: CLOSE-ALL\n"
  "\n"
  "This effectively restarts the compiler, in that all knowledge\n"
  "of any opened projects is removed. This can be useful if you\n"
  "wish to compile a different version of a project that you have\n"
  "previously compiled in a session.\n"
  hidden? #f
  close-all-projects();
end;

define dylan-shell-command
  Build (project, clean, not-recursive, save, dfm, harp, combine-dlls, nodebug, nolink, nocompile, dll, exe, microsoft, gnu, abort-on-serious-warnings, default-binding)
  description "builds a project"
  documentation
  "Usage: BUILD {options} project\n"
  "\n"
  "This both compiles and links a project and its subprojects. By\n"
  "default this is an incremental operation so that the compiler\n"
  "and linker only do the smallest amount of work necessary.\n"
  "\n"
  "The following options are provided:\n"
  "\n"
  "  /not-recursive - don't process the subprojects of this project\n"
  "  /clean         - force a clean build of the project\n"
  "  /save          - save the compiler database\n"
  "\n"
  "  /nolink        - don't link the project\n"
  "  /dll           - force the project to be linked as a DLL\n"
  "  /exe           - force the project to be linked as an executable\n"
  "\n"
  "  /microsoft     - link using the Microsoft linker\n"
  "  /gnu           - link using the GNU linker\n"

  hidden? #f
  with-compiler-transaction
  with-project-location-handler (context)
    let project = ensure-project-open(context, project);
    let compile? = ~ nocompile;
    let link? = ~ nolink;
    let linker = case
		   microsoft => #"microsoft";
		   gnu       => #"gnu";
		   otherwise => unsupplied();
		 end;
    if (project)
      if (save) write-databases?() := #t end if;
      let binding = default-binding & as(<symbol>, default-binding);
      block(exit)
	// validate inputs
	if(binding & binding ~== #"tight" & binding ~== #"loose")
	  user-message(context, "Incorrect value '%s' for default-binding flag\n"
			 "Has to be 'loose' or 'tight'", binding);
	  exit()
	end;
	let aborted?
	  =
	  if (compile?)
	    if (not-recursive)
	      compile-library
		(project,
		 save?: save, 
		 default-binding: binding,
		 force-compile?: clean, force-parse?: clean, dfm-output?: dfm,
		 harp-output?: harp, debug-info?: ~ nodebug,
		 abort-on-all-warnings?:     #f,
		 abort-on-serious-warnings?: abort-on-serious-warnings,
		 assembler-output?: assemble?(#f))
	    else
	      update-libraries
		(project, save?: save, force?: clean,
		 default-binding: binding, dfm-output?: dfm,
		 harp-output?: harp, debug-info?: ~ nodebug,
		 abort-on-all-warnings?:     #f,
		 abort-on-serious-warnings?: abort-on-serious-warnings,
		 assembler-output?: assemble?(#f))
	    end;
	  end;
	unless (aborted?)
	  if (link?)
	    link-library-with-options
	      (project, 
	       linker: linker,
	       dll?: dll, exe?: exe, combine?: combine-dlls,
	       force?: clean, not-recursive?: not-recursive)
	  end;
	end
      end
    end
  end
  end with-compiler-transaction;
end;


/// Internal commands

define dylan-shell-command
  parse (project)
  description "parses a project"
  documentation
  "Usage: PARSE project\n"
  "\n"
  "Parses the project, producing an in-memory model of the code.\n"
  hidden? #t
  with-compiler-transaction
  with-project-location-handler (context)
    let project = ensure-project-open(context, project);
    project & load-library(project.project-registered-name)
  end
  end
end;

// TODO this needs to call project-compiled? which is currently
// in environment/dfmc/projects, but should be in project manager
// so we can call it...

define dylan-shell-command
  Link (project, gnu, microsoft, dll, exe, force, not-recursive, combine-dlls)
  description "links compiled object files"
  documentation
  "Usage: LINK {options} project\n"
  "\n"
  "This links a project and its subprojects, producing either DLLs\n"
  "or executables and storing them in the project's bin directory.\n"
  "\n"
  "The following options are provided:\n"
  "\n"
  "  /dll           - link the project as a DLL\n"
  "  /exe           - link the project as an executable\n"
  "  /force         - force link the project and its subprojects\n"
  "  /not-recursive - only link this project, not its subprojects\n"
  "  /combine-dlls  - link all DLLs into one executable\n"
  "\n"
  "  /microsoft     - link using the Microsoft linker\n"
  "  /gnu           - link using the GNU linker\n"
  hidden? #t
  let project = ensure-project-open(context, project);
  let linker = case
		 microsoft => #"microsoft";
		 gnu       => #"gnu";
		 otherwise => unsupplied();
	       end;
  if (project)
    link-library-with-options(project,
			      linker: linker,
			      dll?: dll, exe?: exe, combine?: combine-dlls,
			      force?: force, not-recursive?: not-recursive)
  end
end;

define dylan-shell-command 
  collect-garbage (report)
  description "performs a garbage collection"
  documentation
  "Perform a full garbage collection.\n"
  "\n"
  "The following options are provided:\n"
  "\n"
  "  /report    - display statistics of all live objects on the heap\n"
  hidden? #t
  let result = collect-garbage(print-stats?: report);
  user-message(context, "%s\n", result)
end;

define dylan-shell-command 
  room ()
  description "shows the heap space allocated"
  documentation
  "Shows the heap space allocated.\n"
  hidden? #t
  let result = room();
  user-message(context, "%s\n", result);
end;

define command-flag allocation;
define command-flag off;

define dylan-shell-command 
  profile (allocation, off)
  description "Activates compilation profiling"
  documentation
  "Activates compilation profiling.\n"
  "\n"
  "The following options are provided:\n"
  "\n"
  "  /allocation    - profile heap allocation\n"
  "  /off           - disable profiling\n"
  hidden? #t
  case
    allocation => *dfmc-profile-allocation?* := #t;
    off => *dfmc-profile-allocation?* := #f;
    otherwise => user-message(context, "No profiling activated -- please supply an option\n");
  end case;
end;


define command-argument class-name;
define command-flag count;

define dylan-shell-command 
  break (class-name, module, library, count)
  description "Activates breakpoints on object allocation"
  documentation
  "Activates breakpoints on object allocation.\n"
  "Usage: break {/count} class-name module library\n"
  "\n"
  "The following options are provided:\n"
  "\n"
  "  /count:n           - break on nth instance\n"
  hidden? #t
  let class-name = as(<symbol>, class-name);
  let module = as(<symbol>, module);
  let library = as(<symbol>, library);
  let class = variable-value(class-name, module, library);
  let count =
    if (count)
      string-to-integer(count)
    end if;
  set-class-breakpoint(class, count: count);
end;

define dylan-shell-command 
  clear (class-name, module, library)
  description "Clears breakpoints on object allocation"
  documentation
  "Clears breakpoints on object allocation.\n"
  "Usage: clear class-name module library\n"
  hidden? #t
  let class-name = as(<symbol>, class-name);
  let module = as(<symbol>, module);
  let library = as(<symbol>, library);
  let class = variable-value(class-name, module, library);
  clear-class-breakpoint(class);
end;

define dylan-shell-command 
  clear-all ()
  description "Clears all breakpoints on object allocation"
  documentation
  "Clears all breakpoints on object allocation.\n"
  "Usage: clear-all\n"
  hidden? #t
  clear-class-breakpoints();
end;

define dylan-shell-command 
  breaks ()
  description "Display breakpoints on object allocation"
  documentation
  "Display breakpoints on object allocation.\n"
  "Usage: breaks\n"
  hidden? #t
  display-class-breakpoints();
end;


define dylan-shell-command
  compile-library (library, force-compile, force-parse, assemble, dfm, harp, nodebug, save, flush, abort-on-serious-warnings, abort-on-any-warnings, skip-emit, skip-link)
  description "compiles a library without its used libraries"
  documentation
  "Compile a Dylan library without updating compilation of used libraries\n"
  "\n"
  "Options:  force-compile force-parse dfm harp nodebug assemble abort-on-serious-warnings abort-on-any-warnings skip-emit skip-link\n"
  "\n"
  "Example:  compile-library -force-parse test\n"
  hidden? #t
  with-compiler-transaction
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    if (project)
      if (save) 
	write-databases?() := #t;
	force-parse := #t	// To save we need to rebuild everything
      end;
      compile-library
	(project, 
	 force-compile?: force-compile, force-parse?: force-parse,
	 abort-on-all-warnings?: abort-on-any-warnings, 
	 abort-on-serious-warnings?: abort-on-serious-warnings, 
	 assembler-output?: assemble?(assemble), dfm-output?: dfm,
	 harp-output?: harp, debug-info?: ~ nodebug,
	 save?: save, flush?: flush, 
	 skip-emit?: skip-emit, skip-link?: skip-link);
    end if;
  end with-project-location-handler
  end with-compiler-transaction
end;

define dylan-shell-command 
  recursive-statistics (library, force)
  description "shows recursive database statistics"
  documentation
  "Show recursive database statistics.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & report-recursive-library-database-statistics(library, force?: force);
  end with-project-location-handler;
end;

define dylan-shell-command 
  statistics (library, force)
  description "shows database statistics"
  documentation
  "Show database statistics.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & report-library-database-statistics(library, force?: force);
  end with-project-location-handler;
end;

define dylan-shell-command 
  diff-statistics (library)
  description "shows differential database statistics"
  documentation
  "Show differential database statistics.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & report-diff-library-database-statistics(library);
  end with-project-location-handler;
end;

define dylan-shell-command 
  enable-stripping ()
  description "enables stripping in tight mode"
  documentation
  "enables stripping in tight mode.\n"
  hidden? #t
  *strip-enabled?* := #t;
end;

define dylan-shell-command 
  disable-stripping ()
  description "disables stripping in tight mode"
  documentation
  "disables stripping in tight mode.\n"
  hidden? #t
  *strip-enabled?* := #f;
end;

define macro with-interpreter
  { with-interpreter (?context:expression, ?project:name = ?library:name) ?:body end }
    => { with-compiler-transaction
	   with-project-location-handler (?context)
	     let saved-strip-enabled?        = *strip-enabled?*;
	     let saved-inlining?             = *inlining?*;
	     let saved-call-upgrading?       = *call-upgrading?*;
	     let old-show-compiler-messages? = show-compiler-messages?();
	     block ()
	       let ?project = ensure-project-open(?context, ?library);
	       if (?project)
		 *strip-enabled?*          := #f;
		 *inlining?*               := #f;
		 *call-upgrading?*         := #f;
		 internal-reporting()      := #f;
		 show-compiler-messages?() := #f;
		 ?body
	       end if
	     cleanup
	       internal-reporting()      := #t;
	       show-compiler-messages?() := old-show-compiler-messages?;
	       *call-upgrading?*         := saved-call-upgrading?;
	       *strip-enabled?*          := saved-strip-enabled?;
	       *inlining?*               := saved-inlining?;
	     end block;
	   end with-project-location-handler
	 end with-compiler-transaction }
end macro;

define method interpret-library-command 
    (library, force, save, #key context = *top-level-loop*)
  with-interpreter (context, project = library)
    compile-library
      (project, force-parse?: force, save?: save,
       progress?: #f, skip-emit?: #t, skip-link?: #t, skip-heaping?: #t);
    interpret-project(project)
  end with-interpreter;
end method;

define dylan-shell-command
  interpret-library (library, force-compile, force-parse, dfm, save, flush)
  description "Interprets a library"
  documentation
  "Interprets a Dylan library\n"
  "\n"
  "Example:  interpret-library play\n"
  hidden? #t
  interpret-library-command(library, force-parse, save, context: context)
end;

define command-argument input-string;

define method interpret-string-command 
    (library, input-string, #key context = *top-level-loop*)
  with-interpreter (context, project = library)
    let tid =
      execute-string(input-string, module: library, library: library,
		     skip-link?: #t, harp-output?: #f, interpret?: #t);
    for (result in interpreter-transaction-value(tid) | #())
      format-out("%=\n", result);
    end for;
  end with-interpreter;
end method;

define dylan-shell-command
  interpret (library, input-string)
  description "Interprets a string in library"
  documentation
  "Interprets a string in a library\n"
  "\n"
  "Example:  interpret play \"1 + 1\"\n"
  hidden? #t
  interpret-string-command(library, input-string, context: context)
end;

define method interpret-top-level-command 
    (library, #key context = *top-level-loop*)
  with-interpreter (context, project = library)
    let handler (<serious-condition>) = condition-handler;
    iterate loop ()
      block ()
	format(*standard-output*, "==> ");
	iterate read-input (input-string = read-line(*standard-input*))
	  unless (input-string = ":exit")
	    if (string-complete?(input-string, module: library, library: library))
	      let tid =
		execute-string(input-string, module: library, library: library,
			       skip-link?: #t, harp-output?: #f, interpret?: #t);
	      for (result in interpreter-transaction-value(tid) | #())
		format-out("%=\n", result);
	      end for;
	      loop()
	    else 
	      read-input(concatenate(input-string, "\n", read-line(*standard-input*)));
	    end if;
	  end unless;
        end iterate;
      exception (<command-loop-continuation>)
	loop()
      exception (<abort>)
	loop()
      end block;
    end iterate;
  end with-interpreter;
end method;

define dylan-shell-command
  interpret-top-level (library, input-string)
  description "Starts an Interpreter loop in library"
  documentation
  "Interprets strings in a library\n"
  "\n"
  "Example:  interpret-top-level play\n"
  hidden? #t
  interpret-top-level-command(library, context: context)
end;

define dylan-shell-command 
  enable-heap-statistics ()
  description "enables future heap statistics"
  documentation
  "enables future heap statistics.\n"
  hidden? #t
  *heap-record-back-pointers?* := #t;
  *heap-statistics?* := #t;
end;

define dylan-shell-command 
  disable-heap-statistics ()
  description "disables future heap statistics"
  documentation
  "disables future heap statistics.\n"
  hidden? #t
  *heap-record-back-pointers?* := #f;
  *heap-statistics?* := #f;
end;

define dylan-shell-command 
  enable-inlining ()
  description "enables inlining"
  documentation
  "enables inlining.\n"
  hidden? #t
  *inlining?* := #t;
end;

define dylan-shell-command 
  disable-inlining ()
  description "disables inlining"
  documentation
  "disables inlining.\n"
  hidden? #t
  *inlining?* := #f;
end;

define dylan-shell-command 
  enable-call-upgrading ()
  description "enables call upgrading"
  documentation
  "enables call upgrading.\n"
  hidden? #t
  *call-upgrading?* := #t;
end;

define dylan-shell-command 
  disable-call-upgrading ()
  description "disables call upgrading"
  documentation
  "disables call upgrading.\n"
  hidden? #t
  *call-upgrading?* := #f;
end;

define dylan-shell-command 
  heap-statistics (library)
  description "shows heap statistics"
  documentation
  "Show heap statistics.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & report-library-heap-statistics(library);
  end with-project-location-handler;
end;

define dylan-shell-command 
  recursive-heap-statistics (library)
  description "shows heap statistics"
  documentation
  "Show heap statistics.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & report-recursive-library-heap-statistics(library);
  end with-project-location-handler;
end;

define dylan-shell-command 
  diff-heap-statistics (library)
  description "shows heap statistics"
  documentation
  "Show heap statistics.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & report-diff-library-heap-statistics(library);
  end with-project-location-handler;
end;

define dylan-shell-command 
  watchpoint-class ()
  description "shows database watchpoint class"
  documentation
  "Show database watchpoint class.\n"
  hidden? #t
  dood-watchpoint-class()
end;

define dylan-shell-command 
  set-watchpoint-class (watchpoint)
  description "sets database watchpoint class"
  documentation
  "Sets database watchpoint class.\n"
  hidden? #t
  dood-watchpoint-class() := watchpoint;
end;

define dylan-shell-command 
  watchpoint-dood ()
  description "shows database watchpoint dood"
  documentation
  "Show database watchpoint dood.\n"
  hidden? #t
  dood-watchpoint-dood()
end;

define dylan-shell-command 
  set-watchpoint-dood (watchpoint)
  description "sets database watchpoint dood"
  documentation
  "Sets database watchpoint dood.\n"
  hidden? #t
  dood-watchpoint-dood() := watchpoint;
end;

define command-argument buffer-size;

define dylan-shell-command 
  set-dood-buffer-size (buffer-size)
  description "sets dood buffer size"
  documentation
  "Sets dood buffer size\n."
  hidden? #t
  dood-buffer-size() := string-to-integer(buffer-size);
end;

define dylan-shell-command 
  dood-buffer-size ()
  description "gets dood buffer size"
  documentation
  "Gets dood buffer size\n."
  hidden? #t
  user-message(context, "%d\n", dood-buffer-size());
end;

define command-argument number-of-buffers;

define dylan-shell-command 
  set-dood-number-of-buffers (number-of-buffers)
  description "sets dood buffer size"
  documentation
  "Sets dood buffer size\n."
  hidden? #t
  dood-number-of-buffers() := string-to-integer(number-of-buffers);
end;

define dylan-shell-command 
  dood-number-of-buffers ()
  description "Gets dood buffer size"
  documentation
  "Gets dood buffer size\n."
  hidden? #t
  user-message(context, "%d\n", dood-number-of-buffers());
end;

define command-argument-spec
  flags:   #("lambda"),
  keyword: #"lambda",
  value?:  #t
end;

define command-argument-spec
  flags:   #("in-library"),
  keyword: #"in-library",
  value?:  #t
end;

define command-argument-spec
  flags:   #("in-file"),
  keyword: #"in-file",
  value?:  #t
end;

define command-flag in-top-library;

define dylan-shell-command 
  trace-optimizations (lambda, in-library, in-top-library, in-file)
  description "configures optimization tracing"
  documentation
  "Usage: trace-optimizations [/lambda lambda] [/in-library library] [/in-top-library] [/in-file file]\n"
  "\n"
  "  /lambda         - traces optimizations only for this lambda.\n"
  "  /in-library     - traces optimizations only for this library.\n"
  "  /in-top-library - traces optimizations only for the top-library.\n"
  "  /in-file        - traces optimizations only for this file.\n"
  hidden? #t
  if (lambda)
    *trace-optimizing-method* := as(<symbol>, lambda);
    *trace-optimizations?*    := #f;
  else 
    *trace-optimizations?*    := #t;
  end if;
  if (in-library)
    *trace-optimizing-library* := as(<symbol>, in-library);
  elseif (in-top-library)
    *trace-optimizing-library* := #t;
  end if;
  if (in-file)
    *trace-optimizing-file* := in-file;
  end if;
end;

define dylan-shell-command 
  untrace-optimizations ()
  description "untraces optimization"
  documentation
  "untraces optimization.\n"
  hidden? #t
  *trace-optimizations?*     := #f;
  *trace-optimizing-method*  := #f;
  *trace-optimizing-library* := #f;
  *trace-optimizing-file*    := #f;
end;

define dylan-shell-command 
  dump-dfm (lambda, in-library, in-top-library, in-file)
  description "configures dfm dumping"
  documentation
  "Usage: dump-dfm [/lambda lambda] [/in-library library] [/in-top-library] [/in-file file]\n"
  "\n"
  "  /lambda         - dumps dfm only for this lambda.\n"
  "  /in-library     - dumps dfm only for this library.\n"
  "  /in-top-library - dumps dfm only for the top-library.\n"
  "  /in-file        - dumps dfm only for this file.\n"
  hidden? #t
  if (lambda)
    *dump-dfm-method* := as(<symbol>, lambda);
    *dump-dfm?*       := #f;
  else 
    *dump-dfm?*       := #t;
  end if;
  if (in-library)
    *dump-dfm-library* := as(<symbol>, in-library);
  elseif (in-top-library)
    *dump-dfm-library* := #t;
  end if;
  when (in-file)
    *dump-dfm-file* := in-file;
  end when;
end;

define dylan-shell-command 
  undump-dfm ()
  description "turns off dfm dumping"
  documentation
  "turns off dfm dumping.\n"
  hidden? #t
  *dump-dfm-file*    := #f;
  *dump-dfm-library* := #f;
  *dump-dfm-method*  := #f;
  *dump-dfm?*        := #f;
end;

define dylan-shell-command 
  save (library, export-only, flush)
  description "saves a compilation database"
  documentation
  "Save Database for a Dylan Library\n"
  "\n"
  "Options:  export-only\n"
  "\n"
  "Example:  save -export-only test\n"
  hidden? #t
  if (write-databases?())
    save-library(library, export-only?: export-only, flush?: flush)
  else
    user-message(context,
		 "Cannot save as database writing is currently switched off.\n"
		 "To switch it back on do: set-write-databases? true.\n"
		 "Note that you will then have to recompile the library.\n")
  end if;
end;

define dylan-shell-command 
  save-namespace (library, flush)
  description "saves a compilation namespace database"
  documentation
  "Save Namespace Database for a Dylan Library\n"
  "\n"
  "Options:  flush\n"
  "\n"
  "Example:  save -flush test\n"
  hidden? #t
  if (write-databases?())
    save-library-namespace(library, flush?: flush)
  else
    user-message(context,
		 "Cannot save as database writing is currently switched off.\n"
		 "To switch it back on do: set-write-databases? true.\n"
		 "Note that you will then have to recompile the library.\n")
  end if;
end;

define dylan-shell-command 
  flush (library)
  description "flushes a library's database"
  documentation
  "Flush database for a Dylan library.\n"
  hidden? #t
  let saved-write? = write-databases?();
  block ()
    write-databases?() := #f;
    save-library (library, flush?: #t);
  cleanup
    write-databases?() := saved-write?;
  end block;
end;

define dylan-shell-command 
  build-locations ()
  description "displays build locations"
  documentation
  "Display build locations.\n"
  hidden? #t
  user-message(context, "System roots: %s\n", system-registry-path());
  let personal = user-registry-path();
  if(personal)
    user-message(context, "Personal roots: %s\n", personal)
  end if;
end;

define dylan-shell-command 
  find-library (library)
  description "finds location of a dylan library"
  documentation
  "Find location of a Dylan library.\n"
  hidden? #t
  let project = lookup-named-project(library);
  user-message(context, "%s\n", project);
end;

define dylan-shell-command
  all-open-projects ()
  description "shows all open projects"
  documentation
  "Show all open projects.\n"
  hidden? #t
  for (p in *all-open-projects*)
    user-message(context, "  %s\n", p);
  end for;
end;


define command-argument registry-path;

define dylan-shell-command 
  registries (registry-path)
  description "displays or sets available registries"
  documentation
  "Display or sets available registries.\n"
  hidden? #t
  let (processor, os) = default-platform-info(*default-project-class*);
  when (registry-path)
    os/environment-variable("FUNCTIONAL_DEVELOPER_USER_REGISTRIES") := registry-path;
  end;
  let registries = find-registries(processor, os);
  for(r in registries)
    user-message(context, "%s\n", r);
  end for;
end;

define dylan-shell-command
  update-libraries (library, force, save, flush, dfm, harp, nodebug, abort-on-any-warnings, abort-on-serious-warnings, default-binding)
  description "compiles a project"
  documentation
  "Compile a Dylan application (with dependency checking)\n"
  "\n"
  "Options:  force\n"
  "Options:  save\n"
  "Options:  flush\n"
  "Options:  dfm\n"
  "Options:  harp\n"
  "Options:  nodebug\n"
  "Options:  abort-on-serious-warnings\n"
  "Options:  abort-on-all-warnings\n"
  "Options:  default-binding [tight | loose]\n"
  "\n"
  "Example:  update-libraries -force test\n"
  hidden? #t
  with-compiler-transaction
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    if (project)
      if (save) write-databases?() := #t end if;
      let binding = default-binding & as(<symbol>, default-binding);
      if(binding & binding ~== #"tight" & binding ~== #"loose")
	user-message(context, "Incorrect value '%s' for default-binding flag\n"
		       "Has to be 'loose' or 'tight'", binding);
      else
	update-libraries(project, force?: force, save?: save, flush?: flush, 
			 default-binding: binding, dfm-output?: dfm,
			 harp-output?: harp, debug-info?: ~ nodebug,
			 abort-on-all-warnings?: abort-on-any-warnings, 
			 abort-on-serious-warnings?: abort-on-serious-warnings,
			 assembler-output?: assemble?(#f));
      end if;
    end if;
  end with-project-location-handler;
  end with-compiler-transaction;
end;

define dylan-shell-command
  run-command-on-sources (command, library)
  description "Runs shell command on every source file in a library"
  documentation
  "Runs shell program 'command' on every source file in 'library'\n"
  "\n"
  "Example:  run-command-on-sources dir my-library\n"
  "    would run dir on every source file involved in the compilation of \n"
  "    the library named my-library\n"
  "Example:  run-command-on-sources c:\\users\\gts\\doit.bat play\n"
  hidden? #t
  run-command-on-sources(command, library);
end;

define dylan-shell-command
  run-command-on-source-directories (command, library)
  description "Runs shell command on every directory in a library"
  documentation
  "Runs shell 'command' on every directory which contains \n"
  "source files used to compile 'library'\n"
  "\n"
  "Example:  run-command-on-source-directories hope-sync my-library\n"
  hidden? #t
  run-command-on-source-directories(command, library);
end;

define dylan-shell-command
  unify (filename)
  description "Creates new library and lid files for groups of projects"
  documentation
  "Creates new library and lid files as per config info in 'filename'\n"
  hidden? #t
  unify-project(config: as(<string>, filename))
end;

define dylan-shell-command
  warnings (project, recursive, personal, summary, nonits)
  description "Show warnings for a project"
  documentation
  "Usage: warnings [/rec[ursive] ] [/personal] [/summary] [/nonits] project\n"
  "\n"
  "  /recursive - show warnings for all used projects as well.\n"
  "  /personal - like /recursive, but ignore system projects.\n"
  "  /summary - don't actually show warnings, just show total count.\n"
  "  /nonits - ignore \"defined but not referenced\" warnings.\n"
  hidden? #t
  show-warnings(project,
		recursive?: recursive,
		personal?: personal,
		summary?: summary,
		nonits?: nonits)
end;

define dylan-shell-command
  tags (project, filename, recursive, personal)
  description "Make a tags file for a project"
  documentation
  "Usage: TAGS [ /rec[ursive] ] [ /personal ] project tags-file-name\n"
  "\n"
  "Creates a tags file for definitions in the project.  If /recursive\n"
  "is specified, includes all subprojects.  If /personal is specified,\n"
  "/recursive is implied, but only personal subprojects are included.\n"
  hidden? #t
  make-tags(project, as(<string>, filename),
	    recursive?: recursive,
	    personal?: personal)
end;
