Module:    dw
Synopsis:  Batch mode compilation handling
Author:    Roman Budzianowski, Andy Armstrong, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $compiler-error-return-code = -1;

define variable *current-library-context* = #f;
define variable *current-file-context* = #f;


define command-argument-spec
  flags: #("read-only", "ro"),
  keyword: #"read-only"
end;

define command-flag off;

define command-flag warnings;

define command-argument subsystem;

define command-argument-spec
  flags: #("editable", "rw"),
  keyword: #"editable"
end;

define command-argument-spec
  flags: #("database", "db"),
  keyword: #"database"
end;

define command-argument-spec
  flags: #("to-file"),
  keyword: #"to-file",
  value?: #t
end;

/// User-level commands

define imported-shell-command help;

define imported-shell-command help-all;

define imported-shell-command exit;

define imported-shell-command parse;

define imported-shell-command open;

define imported-shell-command import;

define imported-shell-command close;

define imported-shell-command remove-build-products;

define imported-shell-command close-all;

define imported-shell-command Link;

define imported-shell-command Build;

define imported-shell-command run-command-on-sources;

define imported-shell-command run-command-on-source-directories;

define imported-shell-command unify;

define imported-shell-command tags;

define imported-shell-command warnings;

/// Internal commands

define command-argument file;

define shell-command
  print(file, mode, library, force-compile, force-parse, dfm, harp, assemble)
  description "show command flags"
  documentation
  "Show command flags.\n"
  hidden? #t
  format-out("file %s ", file);
  format-out("mode %s ", mode);
  format-out("library %s ", library);
  format-out("force-compile %s ", force-compile);
  format-out("force-parse %s ", force-parse);
  format-out("dfm %s ", dfm);
  format-out("harp %s ", harp);
  format-out("assemble %s ", assemble);
  // format-out("report %s ", report);
  format-out("\n");
end;

/***
define imported-shell-command all-open-projects;
***/

define shell-command
  all-open-projects (editable)
  description "shows all open projects"
  documentation
  "Show all open projects.\n"
  hidden? #t
  for (p in *all-open-projects*)
    if((editable & ~p.project-read-only?) | ~editable)
	user-message(context, "  %s\n", p);
    end;
  end;
end;

define imported-shell-command collect-garbage;

define imported-shell-command room;

define imported-shell-command profile;
define imported-shell-command break;
define imported-shell-command breaks;
define imported-shell-command clear;
define imported-shell-command clear-all;

define imported-shell-command compile-library;

define shell-command 
  recompile(library)
  description "recompiles a library"
  documentation
  "Recompile a Dylan Library.\n"
  hidden? #t
  with-project-location-handler (context)
    let project = ensure-project-open(context, library);
    project & recompile-library(project)
  end
end;

define imported-shell-command dood-number-of-buffers;

define imported-shell-command set-dood-number-of-buffers;

define imported-shell-command dood-buffer-size;

define imported-shell-command set-dood-buffer-size;

define imported-shell-command watchpoint-class;

define imported-shell-command set-watchpoint-class;

define imported-shell-command watchpoint-dood;

define imported-shell-command set-watchpoint-dood;

define imported-shell-command heap-statistics;

define imported-shell-command recursive-heap-statistics;

define imported-shell-command enable-heap-statistics;

define imported-shell-command disable-heap-statistics;

define imported-shell-command statistics;

define imported-shell-command recursive-statistics;

define imported-shell-command diff-statistics;

define imported-shell-command save;

define imported-shell-command save-namespace;

define imported-shell-command dump-dfm;

define imported-shell-command undump-dfm;

define imported-shell-command trace-optimizations;

define imported-shell-command untrace-optimizations;

define imported-shell-command enable-stripping;

define imported-shell-command disable-stripping;
 
define imported-shell-command interpret-library;

define imported-shell-command interpret-top-level;

define imported-shell-command interpret;

define imported-shell-command enable-inlining;

define imported-shell-command disable-inlining;
 
define command-flag full;
define command-flag by-library;
define command-flag hits-only;
define command-flag app-results-only;

define command-argument-spec
  flags: #("profile-base"),
  keyword: #"profile-base",
  value?: #t
end;

define command-argument-spec
  flags: #("library-name"),
  keyword: #"library-name",
  value?: #t
end;

define shell-command
  dispatch-statistics(profile-base, library-name, full, by-library, hits-only, app-results-only)
  description "prints out dispatching statistics"
  documentation
  "prints out dispatching statistics\n"
  "\n"
  hidden? #t
  let stats = make-dispatch-statistics(#f);
  collect-dispatch-statistics(%current-library(), stats);
  print-dispatch-statistics
    (stats, library: library-name, profile-base: profile-base, 
     full?: full, by-library?: by-library, 
     hits-only?: hits-only, app-results-only?: app-results-only);
end;

define shell-command
  enable-partial-dispatch()
  description "enables partial dispatch"
  documentation
  "enables partial dispatch\n"
  "\n"
  hidden? #t
  partial-dispatch?(0) := #t;
  call-site-caches-enabled?() := #t;
end;

define shell-command
  disable-partial-dispatch()
  description "disables partial dispatch"
  documentation
  "disables partial dispatch\n"
  "\n"
  hidden? #t
  partial-dispatch?(0) := #f;
end;


define shell-command
  enable-partial-dispatch-megamorphic-punt?()
  description "enables partial dispatch megamorphic punt"
  documentation
  "enables partial dispatch megamorphic punt\n"
  "\n"
  hidden? #t
  partial-dispatch-megamorphic-punt?() := #t;
  partial-dispatch?(0) := #t;
  call-site-caches-enabled?() := #t;
end;

define shell-command
  disable-partial-dispatch-megamorphic-punt?()
  description "disables partial dispatch megamorphic punt"
  documentation
  "disables partial dispatch megamorphic punt\n"
  "\n"
  hidden? #t
  partial-dispatch-megamorphic-punt?() := #f;
end;

define shell-command
  enable-partial-dispatch-sharing?()
  description "enables partial dispatch sharing"
  documentation
  "enables partial dispatch sharing\n"
  "\n"
  hidden? #t
  sharing-partial-dispatch-cache-headers?() := #t;
  partial-dispatch?(0) := #t;
  call-site-caches-enabled?() := #t;
end;

define shell-command
  disable-partial-dispatch-sharing?()
  description "disables partial dispatch sharing"
  documentation
  "disables partial dispatch sharing\n"
  "\n"
  hidden? #t
  sharing-partial-dispatch-cache-headers?() := #f;
end;




define shell-command
  enable-call-site-caches()
  description "enables call site caches"
  documentation
  "enables call site caches\n"
  "\n"
  hidden? #t
  call-site-caches-enabled?() := #t;
end;

define shell-command
  disable-call-site-caches()
  description "disables call site caches"
  documentation
  "disables call site caches\n"
  "\n"
  hidden? #t
  call-site-caches-enabled?() := #f;
end;


define shell-command
  enable-partial-dispatch-only()
  description "enables partial dispatch exclusively"
  documentation
  "enables partial dispatch exclusively\n"
  "\n"
  hidden? #t
  enable-partial-dispatch-only(%current-library())
end;


define shell-command
  enable-call-site-caches-only()
  description "enables call-site caches exclusively"
  documentation
  "enables call-site caches exclusively\n"
  "\n"
  hidden? #t
  enable-call-site-caches-only(%current-library());
end;


define shell-command
  enable-generic-caches-only()
  description "enables generic caches exclusively"
  documentation
  "enables generic caches exclusively\n"
  "\n"
  hidden? #t
  enable-generic-caches-only(%current-library());
end;


define shell-command
  enable-all-terminal-engine-node-profiling()
  description "enables profiling of all terminal engine nodes"
  documentation
  "enables profiling of all terminal engine nodes\n"
  "\n"
  hidden? #t
  profile-all-terminal-engine-nodes?() := #t;
end;

define shell-command
  disable-all-terminal-engine-node-profiling()
  description "disables profiling of all terminal engine nodes"
  documentation
  "disables profiling of all terminal engine nodes\n"
  "\n"
  hidden? #t
  profile-all-terminal-engine-nodes?() := #f;
end;


define shell-command
  enable-profiling-all-calls()
  description "enables generating code for profiling all calls"
  documentation
  "enables generating code for profiling all calls\n"
  "\n"
  hidden? #t
  *profile-all-calls?* := #t;
end;

define shell-command
  disable-profiling-all-calls()
  description "disables generating code for profiling all calls"
  documentation
  "disables generating code for profiling all calls\n"
  "\n"
  hidden? #t
  *profile-all-calls?* := #f;
end;


define shell-command
  enable-dispatch-profiling()
  description "enables dispatch profiling in runtime"
  documentation
  "enables dispatch profiling in runtime\n"
  "\n"
  hidden? #t
  *dispatch-profiling-enabled?* := #t;
end;

define shell-command
  disable-dispatch-profiling()
  description "disables dispatch profiling in runtime"
  documentation
  "disables dispatch profiling in runtime\n"
  "\n"
  hidden? #t
  *dispatch-profiling-enabled?* := #f;
end;

define shell-command
  decache-generics()
  description "decaches all generics"
  documentation
  "decaches all generics\n"
  "\n"
  hidden? #t
  decache-all-generics(%current-library());
end;

define shell-command
  clear-dispatch-profiling()
  description "clear dispatch profiling counters"
  documentation
  "clear dispatch profiling counters\n"
  "\n"
  hidden? #t
  clear-dispatch-profiling(%current-library());
end;


define shell-command
  load-namespace(library)
  description "loads/parses all library definitions"
  documentation
  "loads/parses all library definitions\n"
  "\n"
  hidden? #t
  with-compiler-transaction
  let project = lookup-named-project(library);
  if(project)
    load-namespace(library)
  end
  end
end;

define shell-command
  project-save(library, database, to-file)
  description "saves a project file and its database"
  documentation
  "Save Project File and Database for a Dylan Library\n"
  "\n"
  "Options:  database (saves also database)\n"
  "          to-file <filename> (saves project to a file <filename>\n"
  "\n"
  "Example:  project-save -database test\n"
  hidden? #f
  let save-state = write-databases?();
  let project = lookup-named-project(library, create?: #f);
  if(project)
    block()
      write-databases?() := #t;
      let save-db = database & write-databases?();
      if(save-db)
	user-message(context, "saving database to %s ...", project.project-database-location);
      end;
      save-project(project, save-db?: save-db, to-file: to-file)
    cleanup
      write-databases?() := save-state;
    end;
  else
    user-message(context, "Project %s not open");
  end;
end;

define imported-shell-command flush;

define shell-command 
  debugging(off, subsystem)
  description "turns debugging output on or off for a subsystem"
  documentation
  "Turn debugging output on or off for a subsystem.\n"
  hidden? #t
  if(subsystem)
    if(~off)
      *debug-out* := pair(as(<symbol>, subsystem), *debug-out*)
    else
      *debug-out* := remove(*debug-out*, as(<symbol>, subsystem))
    end;
  end;
  if(~empty?(*debug-out*))
    user-message(context, "Debugging subsystems: %s", *debug-out*)
  end;
end;

define command-argument personal-directory;

define function do-set-personal-root (personal-directory)
  let locator = as(<directory-locator>, personal-directory);
  local method subdirectory (#rest subdirectories)
	  let subdirectory = apply(subdirectory-locator, locator, subdirectories);
	  as(<string>, subdirectory)
	end method subdirectory;
  os/environment-variable("OPEN_DYLAN_USER_ROOT")       := personal-directory;
  os/environment-variable("OPEN_DYLAN_USER_BUILD")      := subdirectory("build");
  os/environment-variable("OPEN_DYLAN_USER_INSTALL")    := personal-directory;
  os/environment-variable("OPEN_DYLAN_USER_SOURCES")    := subdirectory("sources");
  os/environment-variable("OPEN_DYLAN_USER_REGISTRIES") := subdirectory("sources", 
										  "registry");
end function;

define shell-command
  set-personal-root(personal-directory)
  description "sets personal environment variables from root"
  documentation
  "sets personal environment variables from root parameter\n"
  "\n"
  hidden? #t
  do-set-personal-root(personal-directory);
end;

define shell-command
  personal-root()
  description "prints personal root directory"
  documentation
  "prints personal root directory\n"
  "\n"
  hidden? #t
  let personal-directory
    = os/environment-variable("OPEN_DYLAN_USER_ROOT");
  format-out("OPEN_DYLAN_USER_ROOT = %=\n", personal-directory);
end;

define command-argument system-directory;

define function do-set-system-root (system-directory)
  let locator = as(<directory-locator>, system-directory);
  local method subdirectory (#rest subdirectories)
	  let subdirectory = apply(subdirectory-locator, locator, subdirectories);
	  as(<string>, subdirectory)
	end method subdirectory;
  os/environment-variable("OPEN_DYLAN_RELEASE_ROOT")       := system-directory;
  os/environment-variable("OPEN_DYLAN_RELEASE_BUILD")      := subdirectory("build");
  os/environment-variable("OPEN_DYLAN_RELEASE_INSTALL")    := system-directory;
  os/environment-variable("OPEN_DYLAN_RELEASE_SOURCES")    := subdirectory("sources");
  os/environment-variable("OPEN_DYLAN_RELEASE_REGISTRIES") := subdirectory("sources",
										     "registry");
end function;

define shell-command
  set-system-root(system-directory)
  description "sets system environment variables from root"
  documentation
  "sets system environment variables from root parameter\n"
  "\n"
  hidden? #t
  do-set-system-root(system-directory);
end;

define shell-command
  system-root()
  description "prints system root directory"
  documentation
  "prints system root directory\n"
  "\n"
  hidden? #t
  let system-directory
    = os/environment-variable("OPEN_DYLAN_RELEASE_ROOT");
  format-out("OPEN_DYLAN_RELEASE_ROOT = %=\n", system-directory);
end;

define shell-command
  set-root(personal-directory)
  description "sets system and personal environment variables from root"
  documentation
  "sets system and personal environment variables from root parameter\n"
  "\n"
  hidden? #t
  do-set-system-root(personal-directory);
  do-set-personal-root(personal-directory);
end;

define command-argument target-platform;

define function do-set-target-platform (target-platform)
  let system-root = os/environment-variable("OPEN_DYLAN_RELEASE_ROOT");
  let user-root = os/environment-variable("OPEN_DYLAN_USER_ROOT");
  if (system-root & user-root)
    local method subdirectory (directory, #rest subdirectories)
	    let subdirectory = apply(subdirectory-locator, 
				     as(<directory-locator>, directory), subdirectories);
	    as(<string>, subdirectory)
	  end method subdirectory;
    os/environment-variable("OPEN_DYLAN_RELEASE_INSTALL")
      := subdirectory(system-root, target-platform);
    os/environment-variable("OPEN_DYLAN_USER_INSTALL")
      := subdirectory(user-root, target-platform);
    os/environment-variable("OPEN_DYLAN_USER_BUILD")
      := subdirectory(user-root, target-platform, "build");
    close-all-projects();	//---*** NOTE: THIS IS OVERKILL, I think!
    target-platform-name() := target-platform;
    #t
  elseif (user-root)
    format-out("OPEN_DYLAN_RELEASE_ROOT must be set to change platforms.\n");
    #f
  else
    format-out("OPEN_DYLAN_USER_ROOT must be set to change platforms.\n");
    #f
  end
end function do-set-target-platform;

define shell-command
  set-target-platform(target-platform)
  description "sets the target platform"
  documentation "sets the target platform\n"
                "\n"
  hidden? #t
  do-set-target-platform(target-platform);
end;

define shell-command
  target-platform()
  description "prints the target platform"
  documentation "prints the target platform\n"
                "\n"
  hidden? #t
  format-out("OPEN_DYLAN_PLATFORM_NAME = %=\n", target-platform-name());
end;


// debugging roman

define function %print-canonical-sources(context, library)
  let project = library & lookup-named-project(library);
  if(project)
    let sr* = project-canonical-source-records(project);
    do(method(sr)
	   let mod? = source-record-modified?(sr);
	   let rem? = source-record-removed?(sr);
	   user-message(context, "%s id: %s %s\n", 
			sr.source-record-name,
			source-record-as-id(sr, sr.source-record-location),
			if(rem?) 
			  "removed" 
			elseif(mod?) 
			  "modified"
			else 
			  "current" 
			end
			  )
       end,
       sr*)
  end;
end;

define shell-command
  canonical-sources(library)
  description "lists canonical sources"
  documentation
  "List canonical sources.\n"
  hidden? #t
  %print-canonical-sources(context, library);
end;
					       
// alternate entry points for debugging  Nosa

define inline function assemble? (assemble) => (assemble?)
  assemble | *trace-asm?* | unsupplied()
end function assemble?;

define shell-command 
  compile-to-heap(library)
  description "compiles a library to heap"
  documentation
  "Compile a Dylan Library to heap.\n"
  hidden? #t
  compile-library(library, force-compile?: #t, skip-emit?: #t, skip-link?: #t);
end;

define shell-command 
  compile-to-models(library)
  description "compiles a library to models"
  documentation
  "Compile a Dylan Library to models.\n"
  hidden? #t
  compile-library-to-models(library);
end;

define shell-command 
  heap(library, dfm, harp, assemble)
  description "heaps & links a library"
  documentation
  "Heap & link a Dylan Library\n"
  "\n"
  "Options:  dfm, harp assemble\n"
  "\n"
  "Example:  heap -harp -assembler test\n"
  hidden? #t
  heap-library
    (library, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble));
end;

define shell-command 
  emit(library, dfm, harp, assemble)
  description "generates code for a library"
  documentation
  "Code Generate a Dylan Library\n"
  "\n"
  "Options:  harp assemble\n"
  "\n"
  "Example:  emit -dfm -harp -assemble test\n"
  hidden? #t
  emit-library
    (library, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble));
end;

define shell-command 
  link-library(library, dfm, harp, assemble, gnu, microsoft, dll, force, not-recursive)
  description "links a library"
  documentation
  "Link a Dylan Library\n"
  "\n"
  "Options:  harp assemble\n"
  "\n"
  "Example:  link-library -dfm -harp -assemble test\n"
  hidden? #t
  debug/link-library(library, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble));
end;

define shell-command 
  link-glue(library, harp, assemble)
  description "links gluefile for a library"
  documentation
  "Link gluefile for a Dylan Library\n"
  "\n"
  "Options:  harp assemble\n"
  "\n"
  "Example:  link-glue -harp -assemble test\n"
  hidden? #t
  link-glue
    (library, harp-output?: harp, assembler-output?: assemble?(assemble));
end;

define shell-command 
  type-estimate(library)
  description "types a library"
  documentation
  "Type a Dylan Library.\n"
  hidden? #t
  type-estimate-library(library);
end;

define shell-command 
  optimize(library)
  description "optimizes a library"
  documentation
  "Optimize a Dylan Library.\n"
  hidden? #t
  optimize-library(library);
end;

define shell-command 
  update(library)
  description "parses a library"
  documentation
  "Parse a Dylan library.\n"
  hidden? #t
  update-library-definitions(library);
end;

define shell-command 
  in(library)
  description "selects a library"
  documentation
  "Select a Dylan Library.\n"
  hidden? #t
  *current-library-context* := library;
end;

define shell-command 
  in-file(filename)
  description "selects a dylan file in a library"
  documentation
  "Select a Dylan file in a Dylan Library.\n"
  hidden? #t
  *current-file-context* := filename;
end;

define shell-command 
  compile-file(filename, force-compile, force-parse, dfm, harp, assemble)
  description "compiles a dylan file"
  documentation
  "Compile a Dylan file\n"
  "\n"
  "Options:  force-compile force-parse dfm harp assemble\n"
  "\n"
  "Example:  compile-file -force-parse -dfm -harp -assemble testfile\n"
  hidden? #t
  if (*current-library-context*)
    compile-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, force-compile?: force-compile, force-parse?: force-parse, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble));
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command 
  recompile-file(filename, dfm, harp, assemble)
  description "recompiles a dylan file"
  documentation
  "Recompile a Dylan file.\n"
  hidden? #t
  if (*current-library-context*)
    recompile-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble));
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command 
  emit-file(filename, dfm, harp, assemble)
  description "generates code for a dylan file"
  documentation
  "Code Generate a Dylan file\n"
  "\n"
  "Options:  harp assemble\n"
  "\n"
  "Example:  emit-file -harp -assemble testfile\n"
  hidden? #t
  if (*current-library-context*)
    emit-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble));
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command 
  link-file(filename, dfm, harp, assemble)
  description "links a dylan file"
  documentation
  "Link a Dylan file\n"
  "\n"
  "Options:  dfm harp assemble\n"
  "\n"
  "Example:  link-file -dfm -harp -assemble testfile\n"
  hidden? #t
  if (*current-library-context*)
    link-source-record
      (as-lowercase(as(<string>, filename)), *current-library-context*, dfm-output?: dfm, harp-output?: harp, assembler-ouptut?: assemble?(assemble));
  else
    format-out("\nSet current-library using:  in <library>   before attempting this command\n");
  end if;
end;

define shell-command 
  emit-form(formname, dfm, harp, assemble)
  description "generates code for a dylan form"
  documentation
  "Code Generate a Dylan form\n"
  "\n"
  "Options:  dfm harp assemble\n"
  "\n"
  "Example:  emit-form -dfm -harp -assemble testform\n"
  hidden? #t
  if (*current-library-context* & *current-file-context*)
    emit-source-record
      (as-lowercase(as(<string>, *current-file-context*)), *current-library-context*, dfm-output?: dfm, harp-output?: harp, assembler-output?: assemble?(assemble), form?: as-lowercase(as(<string>, formname)));
  else
    format-out("\nSet current-library using:  in <library>         before attempting this command");
    format-out("\nSet current-file    using:  in-file <filename>   before attempting this command\n");
  end if;
end;

define shell-command
  report(library)
  description "enables reporting"
  documentation
  "Enable Reporting\n"
  hidden? #t
  load-library(library);
  user-message(context, "Reporting...\n")
end;

define shell-command
  load(library)
  description "loads a dylan library"
  documentation
  "Load a Dylan Library\n"
  "\n"
  "Example:  load test\n"
  hidden? #t
  load-library(library);
end;


define imported-shell-command build-locations;

define imported-shell-command find-library;

define function project-info (context, project, #key warnings?)
  user-message(context, "%s\n", project);
  user-message(context, "Build location: %s\n", project.project-build-location);
  user-message(context, "Database location: %s\n", project.project-database-location);
  if (project.project-top-level?)
    user-message(context, "Top level project\n");
  end;

  if (project.project-owners)
    for (owner in project.project-owners, sep = "owner projects:" then ",")
      user-message(context, "%s %s", sep, owner.project-name);
    finally
      when (sep = ",") user-message(context, "\n") end;
    end;
  end;

  let sep = "Direct users:";
  for (parent in *all-open-projects*)
    when (member?(project, parent.directly-used-projects))
      user-message(context, "%s %s", sep, parent.project-name);
      sep := ",";
    end;
  end;
  when (sep = ",") user-message(context, "\n") end;

  for (proj in project.directly-used-projects, sep = "Used projects:" then ",")
    user-message(context, "%s %s", sep, proj.project-name)
  finally
    when (sep = ",") user-message(context, "\n") end;
  end;

  let (compilation-context, in-memory?, current?, saved?) =
    ensure-project-database(project);
  if(compilation-context)
    user-message(context, "Database is in memory (project has been parsed before), describes %s sources, is %s\n",
		 if(current?) "current" else "older" end,
		 if(saved?) "saved" else "not saved" end)
  else
    user-message(context, "There is no database (project has never been parsed)\n");
  end;
  // hack, since we don't have access to the sink stream of the command loop
  warnings? & project-dump-conditions(project, *standard-output*);
end;

define shell-command
  project-info (library, warnings, recursive, personal)
  description "Display information about a dylan library"
  documentation
  "Display information about a dylan library or libraries.\n"
  hidden? #t
  with-compiler-transaction
    let project = lookup-named-project(library);
    project-info(context, project,
		 warnings?: warnings);
    when (recursive | personal)
      for (project in all-used-projects(project))
	when (~personal | project-personal-library?(project))
	  project-info(context, project,
		       warnings?: warnings)
	end;
      end;
    end;
  end
end;

define shell-command
  project-set (read-only, editable, library)
  description "Set attributes for a user project"
  documentation
  "Set attributes for a user project\n"
  "\n"
  "Options: read-only editable\n"
  "Example: set-project -read-only test\n"
  hidden? #t
  let project = lookup-named-project(library);
  project.project-read-only? := read-only;
  user-message(context, "%s\n", project);
end;

define imported-shell-command registries;

define imported-shell-command update-libraries;

define shell-command
  set-write-databases?(boolean)
  description "enables database saving"
  documentation
  "Makes a database writable.\n"
  hidden? #t
  write-databases?() :=  boolean = "#t" | boolean = "true";
end;

define shell-command
  set-read-databases?(boolean)
  description "enabled database reading"
  documentation
  "Makes a database readable.\n"
  hidden? #t
  read-databases?() :=  boolean = "#t" | boolean = "true";
end;

define shell-command
  write-databases?()
  description "determines if database is writable"
  documentation
  "Determines if database is writable.\n"
  hidden? #t
  user-message(context, "%s\n", write-databases?());
end;

define shell-command
  read-databases?()
  description "determines if a database is readable"
  documentation
  "Determines if a database is readable.\n"
  hidden? #t
  user-message(context, "%s\n", read-databases?());
end;

define imported-shell-command trace-optimizations;

define imported-shell-command untrace-optimizations;

define shell-command
  trace-harp ()
  description "enables tracing of code generator"
  documentation
  "Trace Code Generator.\n"
  hidden? #t
  *trace-harp?* := #t;
end;

define shell-command
  untrace-harp ()
  description "disables tracing of code generator"
  documentation
  "Turn off Trace for Code Generator.\n"
  hidden? #t
  *trace-harp?* := #f;
end;

define shell-command
  trace-assembler ()
  description "enables tracing of code generator"
  documentation
  "Trace Machine Code Generator.\n"
  hidden? #t
  *trace-asm?* := #t;
end;

define shell-command
  untrace-assembler ()
  description "disables tracing of machine code generator"
  documentation
  "Turn off Trace for Machine Code Generator.\n"
  hidden? #t
  *trace-asm?* := #f;
end;

define shell-command
  enter-debugger ()
  description "enters debugger"
  documentation
  "Enter Debugger.\n"
  hidden? #t
  break("Entering debugger");
end;

// Recovery Protocol

define imported-shell-command continue;

define imported-shell-command debug;

define imported-shell-command abort;

define imported-shell-command restarts;

/// The entry points

define variable *initializing?* :: <boolean> = #t;

define sideways method default-handler (condition :: <error>)
  if (*initializing?*)
    display-condition(condition, prefix: "Initialization error");
    os/exit-application($compiler-error-return-code)
  else
    next-method()
  end
end method default-handler;

define function dw()
  *initializing?* := #f;
  block()
    apply(run-dw, os/application-arguments());
  exception(<end-of-stream-error>)
    format-out("Done\n");
  exception(<end-of-loop>)
    format-out("Done\n");
  end block;
end;

// main function of the compiler driver
define function run-dw(#rest command-args)
  // if no command arguments start the command loop
  if (command-args.size > 0)
    apply(run-batch-compile, command-args);
  else
    let handler (<serious-condition>) = condition-handler;
    let handler (<project-fatal-error>)
      = method (condition :: <project-fatal-error>, handler :: <function>)
	  display-condition(condition, prefix: "Fatal error");
	  signal(make(<command-loop-continuation>))
      end;
    let handler (<keyboard-interrupt>) =
      method(condition :: <keyboard-interrupt>, next-handler)
	  cerror("Continue execution",
		 "Keyboard Interrupt(Control-c) caught");
      end method;
    run-command-loop(*top-level-loop*);
  end;
end;

// define sideways method partial-dispatch?-setter(v, x) end;

// block ()
//   decache-all-generics(%current-library());
// exception (<error>)
// end block;

// partial-dispatch?(#"yo") := #t;

show-internal-compiler-messages?() := #t;

dw();
