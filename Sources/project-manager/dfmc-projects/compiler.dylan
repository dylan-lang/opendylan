Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//---*** TODO: ensure the build etc directories exist.

/// Useful constants

//---*** We really should get this available to us...
define constant <context>             = <object>;
define constant <interactive-context> = <context>;

define constant $link-makefile = "dylanmakefile.mkf";

define constant <database-warning>
  = type-union(<database-corruption-warning>,
	       <database-version-warning>,
	       <database-user-version-warning>);

// Compiler protocols:
//
//   open-compilation-context
//   install-project-sources
//   parse-project-sources
//   compile-project-definitions
//
// Note that we specify a <dfmc-build-target> for the project.

define thread variable *within-compiler-lock?* :: <boolean> = #f;

define constant $compiler-lock = make(<recursive-lock>, name: "compiler-lock");

define constant $pm-lock = make(<recursive-lock>, name: "project-manager-lock");

define inline function do-with-compiler-lock
    (function :: <function>)
  if (*within-compiler-lock?*)
    function()
  else
    with-lock ($compiler-lock, timeout: 10.0)
      dynamic-bind (*within-compiler-lock?* = #t)
        function()
      end
    end
  end
end function do-with-compiler-lock;

define macro with-compiler-lock
  { with-compiler-lock ()
      ?:body
    end }
 => {  do-with-compiler-lock
         (method ()
	    ?body 
	  end) }
end macro with-compiler-lock;

define macro with-compiler-transaction
  { with-compiler-transaction (?target:expression)
      ?:body
    end }
 => {  with-compiler-lock ()
         with-workspace-build (?target.target-workspace, ?target)
	   update-target-compiler-settings(?target);
	   ?body
	 end
       end }
end macro with-compiler-transaction;

define method open-target-compiler-database
    (target :: <dfmc-build-target>)
 => (context :: <context>)
  with-compiler-transaction (target)
    let handler <database-warning> = curry(target-database-warning, target);
    let read-only? = target.target-read-only?;
    let load-namespace? = #t;
    let context
      = open-compilation-context
          (project,
	   database-location: target.target-database-location,
	   profile-location:  target.target-profile-location,
	   build-settings:    target.target-dfmc-build-settings,
	   read-only?:        read-only?,
	   load-namespace?:   load-namespace?);
    if (~read-only? | context.project-library-definition)
      target.target-compilation-context := context
    else
      close-compilation-context(context);
      project-serious-warning
	(project, "System project %s has no library definition",
	 target.target-project.project-title)
    end
  end
end method open-target-compiler-database;

define function reset-target-database
    (target :: <dfmc-build-target>) => ()
  let context = target.target-compilation-context;
  let (major-version, minor-version) = compilation-context-version(context);
  install-project-sources(context, #(), major-version, minor-version);
end function reset-target-database;

define method target-database-warning
    (target :: <dfmc-build-target>, warning :: <database-warning>,
     next-handler :: <function>)
 => ()
  let message
    = select (warning by instance?)
	<database-corruption-warning> =>
	  "Discarding corrupted compiler database: %s";
	<database-version-warning>, <database-user-version-warning> =>
	  "Discarding incompatible compiler database: %s";
      end;
  let database-name = as(<string>, warning.condition-database-name);
  project-warning(target.target-project, message, database-name);
  next-handler()
end method target-database-warning;

define method load-target-namespace
    (target :: <dfmc-build-target>) => ()
  let project = target.target-project;
  let context = target.target-compilation-context;
  let records = target.target-source-records;
  let major-version = project.project-major-version;
  let minor-version = project.project-minor-version;
  install-project-sources(context, records, major-version, minor-version)
end method load-target-namespace;

define method parse-target-library
    (target :: <dfmc-build-target>) => ()
  let workspace = target.target-project.project-workspace;
  with-compiler-transaction (target)
    let context = target.target-compilation-context;
    let parsed? = parse-project-sources(context);
    debug-message("Parse-project-sources returned %s", parsed?);
  end
end method parse-target-library;

define method targets-to-recompile
    (target :: <dfmc-build-target>)
 => (targets :: <sequence>)
  //---*** How should this really be written to take into account
  //---*** loose vs tight binding, dependency tracking et al.
  let targets :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let rebuild? :: <boolean> = #f;
  do-build-targets
    (method (target :: <dfmc-build-target>)
       if (rebuild?
	     | (target.target-source-records
		  ~= target.target-canonical-source-records))
	 add!(targets, target);
	 rebuild? := #t;
       end
     end,
     target);
  targets
end method targets-to-recompile;

define method compile-target-library
    (target :: <dfmc-build-target>, build :: <build-request>) => ()
  let project   = target.target-project;
  let workspace = project.project-workspace;
  with-compiler-transaction (target)
    block ()
      let context          = target.target-compilation-context;
      let settings         = target.target-dfmc-build-settings;
      let clean?           = build.build-clean?;
      let save?            = build.build-save?;
      let abort-reason     = build.build-abort-reason;
      let compilation-mode = build.build-compilation-mode;
      let strip?           = #f;
      let default-binding  = #f;
      let status
	= compile-project-definitions
	    (context,
	     build-settings:             settings,
	     strip?:                     strip?,
	     compile-all?:               clean?,
	     compile-if-built?:          clean?,
	     abort-on-all-warnings?:     abort-reason == #"warnings",
	     abort-on-serious-warnings?: abort-reason == #"serious-warnings",
	     default-binding:            default-binding,
	     save?:                      save?);
      unless (status)
	debug-message("Compile-project-definitions for project %s returned #f",
		      project.project-name)
      end
    exception (error :: <source-record-error>)
      resignal-build-warning(project, error, abort?: #t)
    end
  end
end method compile-target-library;

define method target-dfmc-build-settings
    (target :: <dfmc-build-target>) => (settings :: <list>)
  let location = target.target-project.project-file-location;
  let build-settings :: <list> = #();
  local
    method add-setting (key :: <symbol>, value :: <object>) => ()
      build-settings := pair(key, pair(value, build-settings))
    end method add-setting,

    method maybe-add-setting 
	(key :: <symbol>, value) => ()
      value & add-setting(key, value)
    end method maybe-add-setting,

    method full-path
	(file-name :: <string>) => (path :: <file-locator>)
      merge-locators(as(<file-locator>, file-name), location)
    end method full-path,

    method maybe-add-sources
	(key :: <symbol>, value) => ()
      value & add-setting(key, map(full-path, value))
    end method maybe-add-sources;

  maybe-add-sources(#"c-source-files", #"c");
  maybe-add-sources(#"c-object-files", #"obj");
  maybe-add-sources(#"rc-files",       #"rc");

  maybe-add-setting(#"c-header-files", #"h");
  maybe-add-setting(#"c-libraries",    #"lib");

  maybe-add-setting(#"linker-options", target.target-linker-options);
  maybe-add-setting(#"executable",     target.target-filename);
  maybe-add-setting(#"base-address",   target.target-computed-base-address);

  build-settings
end method target-dfmc-build-settings;

define method save-target-database
    (target :: <dfmc-build-target>, build :: <build-request>) => ()
  let context = target.target-compilation-context;
  with-walk-progress (build-message(target, "Walked %d", count))
    let flush? :: <boolean> = #f;
    save-compilation-context(context, flush?: flush?)
  end
end method save-target-database;

define method generate-link-makefile
    (target :: <dfmc-build-target>, build :: <build-request>) => ()
  let build-directory = target.target-build-directory;
  let location
    = make(<file-locator>,
	   directory: build-directory,
	   name:      $link-makefile);
  with-open-file (stream = location, direction: #"output")
    write-link-makefile(stream, target)
  end
end method generate-link-makefile;

define method write-link-makefile
    (stream :: <stream>, target :: <dfmc-build-target>) => ()
  let context = target.target-compilation-context;
  let build-directory = target.target-build-directory;
  let info :: <pair> = #();
  do-target-subtargets
    (method (subtarget :: <dfmc-build-target>)
       let subtarget-build-directory = subtarget.target-build-directory;
       let name = subtarget.project-library-name;
       let relative-path
	 = if (~subtarget-build-directory)
	     #"system"
	   else
	     as(<string>, 
		relative-locator(subtarget-build-directory, build-directory))
	   end;
       info := concatenate!(list(name, "dummy", relative-path), info)
     end,
     target);

  write-comment(stream, "This build file is generated, please don't edit");
  let project = target.target-project;
  write-lid-library-info(stream, project, files?: #f);
  write-lid-linker-info
    (stream, project,
     base-address-string: compute-base-address(target, target.target-base-address));
  write-list-value(stream, #"files", context.compilation-context-object-names);
  write-list-value(stream, #"used-projects", info);
  // Write out the C libraries for all subprojects so that user projects
  // can be linked without the user explicitly adding them all.
  write-source-files
    (stream, project, #"all-c-libraries", type: #"lib",
     relative?: #f, subprojects?: #t)
end method write-link-makefile;

define method targets-to-relink
    (target :: <dfmc-build-target>)
 => (targets :: <sequence>)
  let targets :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  do-build-targets
    (method (target :: <dfmc-build-target>)
       //---*** How should we determine this?
       add!(targets, target);
     end,
     target);
  targets
end method targets-to-relink;

define method link-target-executable
    (target :: <dfmc-build-target>, build :: <build-request>) => ()
  let project         = target.target-project;
  let workspace       = project.project-workspace;
  let build-directory = target.target-build-directory;
  let clean?          = build.build-clean?;
  let unify?          = build.build-unify?;
  let exports?        = build.build-exports?;
  let subprojects?    = build.build-subprojects?;
  let type            = build.build-target-type | target.target-type;
  let linker          = build.build-linker | target.target-linker;
  let build-options
    = concatenate(case
		    ~subprojects? => #["link"];
		    clean?        => #["force"];
		    otherwise     => #[];
		  end,
		  case
		    exports?      => #["exports"];
		    type == "dll" =>
		      case
			unify?    => #["unify-all", "dll"];
			otherwise => #["dll"];
		      end;
		    otherwise     =>
		      case
			unify?    => #["unify-all", "exe"];
			otherwise => #["exe"];
		      end;
		  end);
  build-system(build-options,
	       directory:          as(<string>, build-directory),
	       linker:             linker,
               // progress-callback:  progress-callback,
	       project-build-info: curry(project-build-info, workspace))
end method link-target-executable;

define method project-build-info
    (workspace :: <dfmc-project-workspace>, library-title :: <string>)
 => (found? :: <boolean>, personal? :: <boolean>, build-location)
  let library-name = as(<symbol>, library-title);
  let project = find-open-project(workspace, library-name);
  if (project)
    //---*** How do we find the right target?
    values(#t,
	   ~project.project-read-only?,
	   target.target-build-directory)
  end
end method project-build-info;

define method remove-target-build-products
    (target :: <dfmc-build-target>, #key subprojects? :: <boolean> = #t)
 => ()
  if (subprojects?)
    do(remove-target-database, all-used-targets(target, system?: #f));
  end;
  remove-target-database(target);
  // remove compiler & link products
  if (makefile-exists?(project))
    build-system(case
		   subprojects? => #["clean-all"];
		   otherwise    => #["clean"];
		 end,
		 directory: as(<string>, target.target-build-directory),
		 progress-callback: always(#f));
  end
end method remove-target-build-products;

define function update-target-compiler-settings
    (target :: <dfmc-build-target>) => ()
  let context = target.target-compilation-context;
  context.compilation-context-compiler-settings
    := if (target.target-read-only?)
	 list(#"build-location",   target.target-build-directory)
       else
	 list(#"build-location",   target.target-build-directory,
	      #"mode",             target.target-compilation-mode,
	      #"processor",        target.target-processor,
	      #"operating-system", target.target-operating-system)
       end
end function update-target-compiler-settings;


/// Compiler callbacks

// These are the compiler protocols that the compiler expects to be
// implemented by the project manager.
//
//  // Return the context for the used library of this project
//  define open generic used-library-context
//    (context, used-library-dylan-name, #key canonicalize?) => used-c;
//
//  // Note that the definitions are now up-to-date.
//  define open generic note-definitions-updated(context);
//
//  // Return the version of a library/project
//  define open generic project-library-version(project) => 
//      (major-ver, minor-ver);
//
//  // Return inter library binding
//  define open generic project-inter-library-binding(project, used-project) =>
//      (binding :: one-of(#"tight", #"loose"));

define sideways method used-library-context 
    (context :: <context>, used-library-name :: <symbol>, 
     #key canonicalize? :: <boolean> = #f)
 => (subcontext :: <context>)
  let workspace = context.compilation-context-workspace;
  let target = workspace.workspace-build-target;
  let used-project = find-project(workspace, used-library-name);
  let build = target.target-build-state;
  if (canonicalize? & ~build-canonicalized-project(build, used-project))
    refresh-project-sources(used-project, clean?: #t);
    build-canonicalized-project(build, used-project) := #t
  else
    refresh-project-sources(used-project)
  end;
  add-new!(used-project.project-owners, project);
  project.project-compilation-context
end method used-library-context;

define method project-library-version
    (project :: <dfmc-project>)
 => (major-version :: <integer>, minor-version :: <integer>)
  values(project.project-major-version, project.project-minor-version)
end method project-library-version;

define method project-inter-library-binding
    (project ::  <dfmc-project>, used-project :: <dfmc-project>)
 => (mode :: one-of(#"tight", #"loose"))
  let binding = project-dynamic-environment(#"default-binding");
  let default-binding = binding & as(<symbol>, binding);
  let loose-bindings = project.project-library-loose-bindings;
  let tight-bindings = project.project-library-tight-bindings;
  let used-name = used-project.project-library-name;
  case
    member?(used-name, loose-bindings) => #"loose";
    member?(used-name, tight-bindings) => #"tight";
    otherwise =>
      default-binding | *default-inter-library-binding*;
  end
end method project-inter-library-binding;

define sideways method note-definitions-updated
    (context :: <context>) => ()
  let target = context.compilation-context-target;
  target.target-database-state  := #"up-to-date";
  target.target-database-saved? := #f;
end method note-definitions-updated;

/*---*** andrewa: this needs implementing...
define method project-record-id-source-record 
    (project :: <project>, id) => (record :: <source-record>)
  %project-record-id-source-record (project, id)
end method project-record-id-source-record;

define function %project-record-id-source-record
    (project :: <project>, id, #key create? = #t)
 => (record :: <source-record>)
  let table = project.project-source-record-table;
  let str = as(<string>, id);
  element(table, str, default: #f) |
    (create? & (table[str] := id-as-source-record(project-source-record-class(project), 
						  project-compiler-source-location(project),
						  id)))
end function %project-record-id-source-record;

define method project-source-record-id 
    (project :: <project>, record :: <source-record>) => (id)
  source-record-as-id(sr, project-compiler-source-location(project));
end method project-source-record-id;

// Must return a name suitable for use as the base of a file name,
// and unique within project.
define method project-source-record-name 
    (project :: <project>, record :: <source-record>)
 => (name :: false-or(<string>))
  let name
    = source-record-relative-name(sr, project-compiler-source-location(project));
  if (name & locator-directory(as(<file-locator>, name)))
    let base = source-record-name(sr);
    let cx = project.project-current-compilation-context;
    let index = position(cx.compilation-context-sources, sr);
    concatenate(integer-to-string(index), "=", base)
  else
    name
  end;
end method project-source-record-name;
*/


/// Base addresses

/// All Functional Developer DLLs that may be used by user libraries reside above
/// this address.  Consequently, it's an upper bound for the base for user
/// libraries including our private libraries (e.g., the compiler and environment)

define constant $top-for-user-libraries = #x64000;

/// Presume that user libraries fit into 32 pages (128K)
define constant $allowance-for-user-libraries = #x20;

define method library-base
    (#key base-address = #f, #all-keys) => (base :: false-or(<machine-word>))
  base-address
end method library-base;

define method compute-base-address
    (target :: <dfmc-build-target>, explicit-base :: false-or(<machine-word>))
 => (computed-base :: <string>)
end method compute-base-address;

define method target-computed-base-address
    (target :: <dfmc-build-target>) => (address :: false-or(<machine-word>))
  let base-address-string
    = target.target-base-address-string
        | begin
	    let baseless-libraries = 1;
	    let top = $top-for-user-libraries;
	    let projects = all-used-projects(project);
	    for (p in projects)
	      let base = apply(library-base, project-build-settings(p));
	      if (base)
		let base :: <machine-word> = base;
		top := min(top,
			   as(<integer>, machine-word-unsigned-shift-right(base, 12)))
	      else
		baseless-libraries := baseless-libraries + 1
	      end
	    end;
	    let machine-word :: <machine-word> = 
	      as(<machine-word>, top - baseless-libraries * $allowance-for-user-libraries);
	    machine-word-unsigned-shift-left(machine-word, 12)
	  end begin;
  machine-word-to-string(base-address-string, prefix: "0x")
end method target-computed-base-address;
