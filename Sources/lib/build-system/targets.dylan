Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *targets* = make(<string-table>);

define variable *simple-targets* = make(<string-table>);

define constant $no-target = "default";


define class <no-more-targets>(<condition>)
end class;

// Take appropriate actions on all input targets

define method do-targets() => ()
  block()
    while (#t)
      target-function()();
    end while;
  exception(<no-more-targets>)
  end block;
end method;

// Get the action for specified target

define method target-function() => (command :: <function>)
  let target = next-target();

  let f =
    element(*targets*,
	    target,
	    default: #f);
  if (f)
    f
  else
    other-targets
  end if; 

end method target-function;

// Managing list of targets

define method more-targets() => ()

  *build*.more-targets? := #t;
  shift-targets()

end method;

define method shift-targets() => ()
  *build*.target := *build*.target + 1
end method;

define method next-target() => (next-target :: <string>)

  block()
    *build*.targets[*build*.target]
  exception(<error>)
    $no-target
  end block;

end method;

define method first-target?() => (first? :: <boolean>)

  *build*.target = 0;

end method;

define method simple-target?(target :: <string>) => (simple? :: <boolean>)

  element(*simple-targets*, as-lowercase(target), default: #f);

end method;

// Notify clients of start of a new build phase

define method next-phase()=> (phase-notifier :: <build-notifier>)
  *build*.build-phase := *build*.build-phase + 1;
  $phase-notifier
end method;

define method compute-number-of-required-phases(min-required :: <integer>) => ()
  *build*.build-phases :=
    min-required + *build*.c-src-objs.size + *build*.rc-objs.size
end method;

// The Targets

define method force-target() => ()

  *build*.force? := #t;
  more-targets();

end method;

define method echo-target() => ()

  *build*.dylanecho? := #t;
  more-targets();

end method;

define method default-target() => ()

  if (*build*.more-targets?)
    signal(make(<no-more-targets>));
  else
    interactive-build();
  end if;

end method;

define method separate-dll-target() => ()

  build-dll();
  more-targets();

end method;

define method dll-target() => ()
  *build*.exports-only? := #f;
  if (*unify-all?*)
    unify-all-dll-target();
  else
    select (default-dll-policy())
      #"single-dlls", #"separate-dlls" => separate-dll-target();
      otherwise => unify-dll-target();
    end select;
  end if;
end method;

define method build-dll() => ()
  local method main()
	  case
	    *build*.backup? => backup();
	    *build*.restore? => restore();
	    *build*.force? =>
	      check-foreign-files();
	      link-dll();
	    otherwise => maybe-link-dll();
	  end case;
	end method;

  if (*build*.link?)
    check-foreign-files();
    link-dll();
  else
    install-libraries(target: "separate-dll");
    main();
  end if;

end method;

define method maybe-link-dll() => ()
  let up-to-date? = check-build-dependencies(*build*.dll, *build*.libfile);
  if (up-to-date?)
    if (*build*.top-level?)
      echo(#f, "%s is up-to-date", *build*.dll);
    end if;
    install-dll(*build*);
  else
    compute-number-of-required-phases(3);
    echo-to-client($stage-notifier, "Linking %s", *build*.dylanlib);
    link-dll();
  end if;
end method;

define method ensure-gc-complete() => ()
  /* Gross hack to ensure we're not in the middle of a GC */
  primitive-mps-park();  
  primitive-mps-release();
end method;

define method link-dll() => ()

  ensure-gc-complete();
  do-link-dll(*build*.linker);
  install-dll(*build*);

end method;

define open generic do-link-dll(linker :: <linker>) => ();

define method do-link-dll(linker :: <linker>) => ();
  let library :: <symbol> = *build*.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let import-fixups = library-filename-with-extensions(library, "imp0", "o");
  let dylan-library? = library.dylan-library?;

  unless (dylan-library?) write-build-import-fixups(*build*) end;

  echo(next-phase(), "Linking %s", *build*.dll);

  execute-shell-commands(#f, #f,
			 "linkdll",
			 "$(dllname)", library-file-name(library),
			 "$(mangled-dllname)", mangled-library,
			 "$(full-dll-name)", *build*.dll,
			 "$(objects)",
			 concatenate(if (dylan-library?) #()
				     else list(import-fixups) end,
				     *build*.c-objs,
				     *build*.c-src-objs,
				     *build*.rc-objs),
			 "$(c-libs)",
			 concatenate(substitute-environment-variables(*build*.c-libs),
				     *build*.rtlibs),
			 "$(base)", *build*.base-address,
			 "$(linkopts)", substitute-environment-variables(*build*.linkopts),
			 "$(image-version)", *build*.image-version);
  uninstall(import-fixups);

end method;

define method uninstall
    (file :: <file-locator>) => ()

  echo?("Deleting file %s", file);

  if (file-exists?(file))
    delete-build-file(file);
  end if;

end method;

define method install-libraries(#key target :: <string> = "dll") => ()

  for (library :: <symbol> in *build*.libs.key-sequence)
    maybe-install(library, target);
  end for;

end method;

define open generic exports-file-extension(linker :: <linker>)
 => (extension :: <string>);

define method exports-file-extension(linker :: <linker>)
 => (extension :: <string>)
  "lib"
end method;

define open generic dll-file-extension(linker :: <linker>)
 => (extension :: <string>);

define method dll-file-extension(linker :: <linker>)
 => (extension :: <string>)
  "dll"
end method;

define open generic exe-file-extension(linker :: <linker>)
 => (extension :: <string>);

define method exe-file-extension(linker :: <linker>)
 => (extension :: <string>)
  "exe"
end method;


define method lib-target()

  *build*.exports-only? := #t;
  *exports* := add(*exports*,
		   file-in-directory(*build*.build-directory, *build*.dylanapp,
				     extension: "exp"));
  if (*unify-all?*)
    unify-all-dll-target();
  else
    select (default-dll-policy())
      #"single-dlls", #"separate-dlls" => separate-dll-target();
      otherwise => unify-dll-target();
    end select;
  end if;
end method;

define open generic create-archive(linker :: <linker>) => ();

define method create-archive(linker :: <linker>) => ()
end method;

define method archive-target()

  create-archive(*linker*);
  signal(make(<no-more-targets>));

end method;


/////
///// DLL UNIFICATION
/////

define open generic do-dll-unification(linker :: <linker>, all?, exe?)
 => (build :: <build>);

define method do-dll-unification(linker :: <linker>, all?, exe?)
 => (build :: <build>)

  let builds :: <stretchy-vector> = do-builds-target();
  let libraries :: <object-table> = make-build-libraries(builds);
  let (exported-builds :: <list>,
       imported-libraries :: <string-table>, 
       imported-builds :: <list>, 
       imported-group-builds :: <table>,
       winning-build :: <build>)
    = process-builds-for-unification(builds, all?, exe?);
  let current-directory = working-directory();
  let winning-build-library = winning-build.dylanlib;
  let executable :: <file-locator> =
    if (exe?) winning-build.app
    else
      if (winning-build.exports-only?) winning-build.libfile
      else winning-build.dll
      end;
    end;
  let exports-only? :: <boolean> = *build*.exports-only?;

  with-link-stream (stream = winning-build, direction: #"output")

    for (build :: <build> in imported-builds)
      unless (build.system-build? | build.build-up-to-date? | *build*.link?)
	let library :: <symbol> = build.dylanlib;
	maybe-install(library, "unify-dll", libraries: libraries);
      end unless;
      apply!(link-object, stream, build.libfile);
    end for;

    for (build :: <build> in imported-group-builds.key-sequence)
      unless (build.system-build? | build.build-up-to-date? | *build*.link?)
	let library :: <symbol> = build.dylanlib;
	maybe-install(library, "unify-dll", libraries: libraries);
      end unless;
      apply!(link-object, stream, build.libfile);
    end for;

    unless (*build*.link?)
      
      for (build :: false-or(<build>) in imported-group-builds)
	if (build & ~build.build-up-to-date?)
	  let library :: <symbol> = build.dylanlib;
	  echo?("Spotted shadow of system project %s", library);
	  maybe-install(library, "unify-dll", libraries: libraries);
	end if;
      end for;

    end unless;

    // Pulls in the object-files of this sub-project
    local method localize-link (build :: <build>,
				fixups :: <pair>,
				#key echo? = #t, first?) => ()
	    let library :: <symbol> = build.dylanlib;
	    let system? :: <boolean> = build.system-build?;
	    if (echo?)
	      echo(#f, "Unifying %s library %s into %s",
		   if (system?) "system" else "personal" end,
		   library, executable);
	    end if;
	    let directory :: <directory-locator> = build.%build-directory;
	    apply!(link-objects, stream, build, directory);
	    unless (*linker*.static-linking? | exports-only?)
	      read-build-imports(build, #f,
				 directory: directory,
				 imported-libraries: imported-libraries);
	      unless (library.dylan-library?)
		write-import-fixups(fixups.head, fixups.tail, directory, first?);
	      end;
	    end unless;
	    unless (system?)
	      install-dylanmakefile(build, directory: directory);
	    end unless;
	  end method;
    
    
    unless (winning-build.build-up-to-date?)
      
      with-import-fixups(winning-build)
	
	// ensure this is the winning build
	localize-link(winning-build, fixups,
		      echo?: ~ exported-builds.empty?, first?: #t);
	
	for (build :: <build> in exported-builds)
	  localize-link(build, fixups);
	end for;

      end with-import-fixups;
      
    end unless;

    // Include runtime support libraries
    if (all? | winning-build-library.dylan-library?)
      apply!(link-runtime-libraries, stream, *linker*);
    else
      apply!(link-object, stream, "dylan-support.lib");
    end if;
    unless (*linker*.static-linking? | *linker*.fake-imports.empty?)
      write-fake-imports(winning-build);
      apply!(link-object, stream, library-filename-with-extensions(winning-build-library, "imp", "o"));
    end unless;
    
    unless (*linker*.static-linking? | winning-build-library.dylan-library?)
      apply!(link-object, stream, library-filename-with-extensions(winning-build-library, "imp0", "o"));
    end;
    
  end with-link-stream;
  
  winning-build

end method;

define method process-builds-for-unification
    (builds :: <stretchy-vector>, all? :: <boolean>, exe? :: <boolean>,
     #key check? :: <boolean> = #t)
 => (exported-builds :: <list>,
     imported-libraries :: <string-table>,
     imported-builds :: <list>, 
     imported-group-builds :: <table>,
     winning-build :: <build>)
  let exported-builds :: <list> = #();
  let imported-libraries :: <string-table> = make(<string-table>);
  let imported-builds :: <list> = #();
  let imported-group-builds :: <table> = make(<table>);

  let *build*-dylanlib = *build*.dylanlib;
  let current-directory = working-directory();
  let (target-project, target-project-group) = merged-project-libraries(*build*-dylanlib);
  let *winning-build* :: <build> 
    = if (target-project = *build*-dylanlib) *build*
      else *builds-cache*[target-project] end if;
  let winning-build-directory :: <directory-locator> 
    = if (*winning-build*.system-build?) current-directory
      else *winning-build*.build-directory
      end if;
  let executable :: <string>
    = if (exe?) 
	*winning-build*.app.locator-name
      else
	if (*winning-build*.exports-only?) *winning-build*.libfile.locator-name
	else *winning-build*.dll.locator-name
	end;
      end;
  let absolute-executable
    = file-in-directory(winning-build-directory, executable);
  let absolute-libfile
    = file-in-directory(winning-build-directory, *winning-build*.libfile.locator-name);
  let up-to-date? :: <boolean> = ~ (*build*.force? | *build*.link?);
  
  local method dependency-checking (build :: <build>, winning-build :: <build>) => ()
	  unless (~check? | build.system-build? | build.build-up-to-date?.supplied?)
	    let directory :: <directory-locator> = build.build-directory;
	    let up-to-date? :: <boolean>
	      = with-build-directory(directory)
		  dynamic-bind (*build* = build)
		    let (absolute-executable :: <file-locator>,
			 absolute-libfile :: <file-locator>,
			 install? :: <boolean>) =
		      case
			winning-build == *winning-build* =>
			  values(absolute-executable, absolute-libfile, #f);
			winning-build == build =>
			  values(build.dll, build.libfile, #t);
			otherwise =>
			  let build-directory = winning-build.build-directory;
			  let dll-name = winning-build.dll.locator-name;
			  let lib-name = winning-build.libfile.locator-name;
			  let dll
			    = if (winning-build.system-build?)
				file-in-directory(*dll-installation*, dll-name);
			      else
				file-in-directory(build-directory, dll-name);
			      end if;
			  let libfile
			    = if (winning-build.system-build?)
				file-in-directory(*lib-installation*, lib-name);
			      else
				file-in-directory(build-directory, lib-name);
			      end if;
			  values(dll, libfile, #f);
		      end case;
		    let up-to-date? :: <boolean> 
		      = check-build-dependencies(absolute-executable, absolute-libfile,
						 up-to-date?: up-to-date?);
		    echo?("dependency-checking %=: %= %= %=",
			  build.dylanlib, up-to-date?, absolute-executable, absolute-libfile);
		    if (up-to-date? & install?)
		      // Reinstall build-products into personal installation area
		      install-dll(build);
		    end if;
		    up-to-date?
		  end dynamic-bind;
		end with-build-directory;
	    build.build-up-to-date? := up-to-date?;
	    unless (winning-build == build)
	      if (winning-build.build-up-to-date?)
		winning-build.build-up-to-date? := up-to-date?;
	      end if;
	    end unless;
	  end unless;
        end method;

  dependency-checking(*winning-build*, *winning-build*);

  for (build :: <build> in builds)
    unless (build == *winning-build*)

    let library :: <symbol> = build.dylanlib;
    let executable :: <string> = build.dylanapp;
    let (parent-library, project-group) = merged-project-libraries(library);

    if (all?)
      if (~ linkable?(*linker*, library))
	dependency-checking(build, build);
	imported-builds := add(imported-builds, build);
	imported-libraries[executable] := executable;
      else
	dependency-checking(build, *winning-build*);
	exported-builds := add(exported-builds, build);
      end if;
    else
      if (project-group.empty?)
	echo?("maybe-install no group %= of %=", library, *build*-dylanlib);
	dependency-checking(build, build);
	imported-builds := add(imported-builds, build);
	imported-libraries[executable] := executable;
      else
	if (target-project == parent-library)
	  dependency-checking(build, *winning-build*);
	  exported-builds := add(exported-builds, build);
	else
	  let new-build :: <build> = *builds-cache*[parent-library];
	  let new-library :: <symbol> = new-build.dylanlib;
	  let new-executable :: <string> = new-build.dylanapp;

          // Check the winning-build before all other members
	  dependency-checking(new-build, new-build);
	  dependency-checking(build, new-build);

	  unless (element(imported-libraries, new-executable, default: #f))
	    echo?("maybe-install used group %= of %= (executable %=)",
		  parent-library, *build*-dylanlib, new-executable);
	    imported-group-builds[new-build] := #f;
	    imported-libraries[new-executable] := new-executable;
	  end unless;
	  imported-libraries[executable] := new-executable;
	  case
	    ~new-build.system-build? => #f;
	    build.system-build?      => #f;
	    otherwise                => imported-group-builds[new-build] := build;
	  end case;
	end if;
      end if;
    end if;

    end unless;
  end for;
  if (target-project-group.size > exported-builds.size + 1)
    for (library :: <symbol> in target-project-group)
      unless (member?(library, exported-builds,
		      test: method(library :: <symbol>, build :: <build>)
				library = build.dylanlib
			    end))
	build-error("Failure to locate library %= of dll-group %=",
		    library, target-project)
      end unless;
    end for;
  end if;

  echo?("Process-Builds-For-Unification:\n\n"
	"  exported-builds: %=\n\n"
	"  imported-libraries: %=\n\n"
	"  imported-builds: %=\n\n"
	"  imported-group-builds: %=\n\n",
	map(dylanlib, exported-builds),
	imported-libraries.key-sequence,
	map(dylanlib, imported-builds),
	map(dylanlib, imported-group-builds.key-sequence));

  values(exported-builds,
	 imported-libraries, imported-builds, imported-group-builds,
	 *winning-build*);
end method;

// Emit link-entries for target unified build to link-script or command-line.
// Each object is either emitted to stream or added to a list depending on
// what the specializable target object is.
// DLL Unification on Win32 will use the link stream emitter because of command
// line limits, and Microsoft linker's convenient use of link scripts.
// For unification on Linux, however, the command-line can be used directly so
// just accumulate all the objects in a <list> for later direct insertion in
// linker command-line. 

define generic link-object(stream, object) => (stream);

define method link-object(stream :: <stream>, object)
 => (stream :: <stream>)
  format(stream, "%s\n", object);
  stream
end method;

define method link-object(stream :: <list>, object)
 => (stream :: <list>)
  pair(format-to-string("%s", object), stream)
end method;

define generic link-objects
    (stream, build :: <build>, directory :: <directory-locator>) => (stream);

define method link-objects
    (stream :: <stream>, build :: <build>, directory :: <directory-locator>)
 => (stream :: <stream>)
  echo?("link-objects %= of %=", build.dylanapp, *build*.dylanapp);
  local method add-objects
	    (objects :: <sequence>, directory :: false-or(<directory-locator>))
	  for (object in objects)
	    format(stream, "%s\n",
		   if (directory)
		     merge-locators(as(<file-locator>, object), directory)
		   else
		     object
		   end)
	  end for;
	end method;
  add-objects(build.objs, directory);
  add-objects(build.c-objs, directory);
  add-objects(build.c-src-objs, directory);
  add-objects(build.rc-objs, directory);
  add-objects(substitute-environment-variables(build.c-libs), #f);
  add-objects(substitute-environment-variables(build.linkopts), #f);
  stream
end method;

define method link-objects
    (stream :: <list>, build :: <build>, directory :: <directory-locator>)
 => (stream :: <list>)
  let *stream* :: <list> = stream;
  echo?("link-objects %= of %=", build.dylanapp, *build*.dylanapp);
  local method add-objects
	    (objects :: <sequence>, directory :: false-or(<directory-locator>))
	  for (object in objects)
	    *stream* :=
	      pair(format-to-string
		     ("%s",
		      if (directory)
			merge-locators(as(<file-locator>, object), directory)
		      else
			object
		      end),
		   *stream*);
	  end for;
	end method;
  add-objects(build.objs, directory);
  add-objects(build.c-objs, directory);
  add-objects(build.c-src-objs, directory);
  add-objects(build.rc-objs, directory);
  add-objects(substitute-environment-variables(build.c-libs), #f);
  add-objects(substitute-environment-variables(build.linkopts), #f);
  /*
  format-out("Setting link-stream to %=\n",
	     reduce(method(s :: <string>, arg) concatenate(s, format-to-string("%s", arg)) end, "", *stream*));
  */
  *stream*
end method;

define open generic link-runtime-libraries(stream, linker :: <linker>) => (stream);

define method link-runtime-libraries(stream :: <stream>, linker :: <linker>)
 => (stream :: <stream>)
  // Hack in the runtime libraries
  format(stream, "%s\n", "mincrt.lib");
  format(stream, "%s\n", "pentium-run-time.lib");
  format(stream, "%s\n", "mmdw.lib");
  format(stream, "%s\n", "mpsplinth.lib");
  stream
end method;

// Support static linking in the build-system.
// The default is to do dynamic linking of shared libraries.

define open generic static-linking?(linker :: <linker>) => (static? :: <boolean>);

define method static-linking?(linker :: <linker>) => (static? :: <boolean>)
  #f
end method;

define method do-unify-target(#key all?, exe?) => (unify-target :: <function>)

  method()

  if (~ exe? & *linker*.static-linking?)
    // For static linking, only exes are expected to be linked; there are no shared libraries
  else

  let maybe-unify :: <function> = if (exe?) maybe-unify-exe else maybe-unify-dll end;

  echo?("Unify-target: library %=", *build*.dylanlib);

  let build :: <build> = do-dll-unification(*linker*, all?, exe?);

  maybe-unify(build, all?);

  end if;

  more-targets();

  end method;
end method;

define method unify-all-target()
  *unify-all?* := #t;
  more-targets();
end method;

define constant unify-dll-target = do-unify-target();
define constant unify-exe-target = do-unify-target(exe?: #t);
define constant unify-all-dll-target = do-unify-target(all?: #t);
define constant unify-all-exe-target = do-unify-target(all?: #t, exe?: #t);


define method %build-directory (build :: <build>) => (directory :: <directory-locator>)
  let library :: <symbol> = build.dylanlib;
  let system? :: <boolean> = build.system-build?;
  if (system?)
    let directory = subdirectory-locator($system-build, library-file-name(library));
    unless (file-exists?(directory))
      build-error("Need compiler products of system sub-project %= in order to unify %=",
		  library, *build*.dylanlib);
    end unless;
    directory
  else
    build.build-directory;
  end if;
end method;

define method resolve-superprojects
    (parent-library :: <symbol>, project-group :: <sequence>, imported? :: <boolean>)
 => (supers :: <stretchy-vector>)
  dynamic-bind (*build-cache* = make(<stretchy-vector>))
    echo?("Resolving super-projects for %=", parent-library);
    local method resolve-superproject
	      (library :: <symbol>) => ()
	    let seen? = element(*used-project-cache*, library, default: #f);
	    unless (seen?)
	      *used-project-cache*[library] := #t;

	      let build? = element(*builds-cache*, library, default: #f);

	      if (build?)
		echo?("Spotted build of system project in builds-cache %s", library);
		build-library(library, build?.build-directory, "builds", 
			      system?: build?.system-build?);
	      else
		// Build Area precedence rule
		let (directory :: false-or(<directory-locator>), system?, error?)
		  = search-for-build-directory($dylanmakefile, library);
		case
		  error? =>
		    if (imported?)
		      // Assume it's a system project
		      build-library(library, #f, "builds", system?: #t);
		    else
		      build-warning("Link deferred! Need compiler products of super-project %="
				    " in order to unify sub-project %=",
				    library, *build*.dylanlib);
		    end if;
		  system? =>
		    build-library(library, #f, "builds", system?: #t);
		  otherwise =>
		    build-library(library, directory, "builds");
		end case;
	      end if;
	    end unless;
	  end;
    resolve-superproject(parent-library);
    do(resolve-superproject, project-group);
    *build-cache*
  end dynamic-bind;
end method;

// Compute all-used-builds of current build

define method do-builds-target() => (build-cache :: <stretchy-vector>)
  dynamic-bind (*used-project-cache* = make(<object-table>),
		*build-cache* = make(<stretchy-vector>))

    builds-target(top-level?: #t);

    // check for unresolved super-projects, adding incrementally
    local method check-builds
	      (builds :: <stretchy-vector>, build-cache :: <stretchy-vector>, 
	       target-parent-library :: <symbol>, target-project-group :: <sequence>)
	   => (build-cache :: <stretchy-vector>)
	    let build-cache :: <stretchy-vector> = build-cache;
	    for (build :: <build> in builds)
	      let library :: <symbol> = build.dylanlib;
	      let (parent-library, project-group) = merged-project-libraries(library);
	      let seen? = element(*used-project-cache*, parent-library, default: #f);
	      let imported? = target-parent-library ~== parent-library;
	      unless (seen?)
		let unseen = resolve-superprojects(parent-library, project-group, imported?);
		let outstanding :: <stretchy-vector> = make(<stretchy-vector>);
		let outstanding-libraries :: <list> = #();
		let outstanding-count :: <integer> = 0;
		for (build :: <build> in unseen)
		  let library :: <symbol> = build.dylanlib;
		  let (parent-library, new-project-group) = merged-project-libraries(library);
		  if (parent-library ~== library
			& ~member?(parent-library, outstanding-libraries))
		    outstanding[outstanding-count] := build;
		    outstanding-count := outstanding-count + 1;
		    echo?("%s introduces implicitly imported superproject %s",
			  library, parent-library);
		    outstanding-libraries := add(outstanding-libraries, parent-library);
		  end if;
		end for;
		build-cache := concatenate(build-cache, unseen);
		if (~empty?(outstanding))
		  build-cache := check-builds(outstanding, build-cache, target-parent-library, target-project-group);
		end if;
	      end unless;
	    end for;
	  build-cache
	  end method;

    let (parent-library, project-group) = merged-project-libraries(*build*.dylanlib);
    check-builds(*build-cache*, *build-cache*, parent-library, project-group);

  end dynamic-bind;
end method;

define method builds-target(#key top-level?) => ()
  install-libraries(target: "builds");
  cache-build(*build*);
  *used-project-cache*[*build*.dylanlib] := #t;
  unless (top-level?)
    more-targets();
  end unless;
end method;

define method cache-build(build :: <build>) => ()
  let library :: <symbol> = build.dylanlib;

  *build-cache*[*build-cache*.size] := build;
  *builds-cache*[library] := build;
end method;

// Compute all-used-directories of current build

define method make-build-libraries
    (builds :: <stretchy-vector>) => (libraries :: <object-table>)
  let libraries :: <object-table> = make(<object-table>);
  for (build :: <build> in builds)
    let dylanlib :: <symbol> = build.dylanlib;
    libraries[dylanlib] := build.build-directory;
  end for;
  libraries
end method;


define method maybe-unify-dll(build :: <build>, all?) => ()
  if (build.build-up-to-date?)
    if (*build*.top-level?)
      echo(#f, "%s is up-to-date", build.dll);
    end if;
    install-dll(build);
  else
    compute-number-of-required-phases(3);
    echo-to-client($stage-notifier, "Linking %s", build.dylanlib);
    unify-dll(build);
  end if;
end method;

define method unify-dll(build :: <build>) => ()

  ensure-gc-complete();
  do-unify-dll(build, build.linker);
  install-dll(build);

end method;

define open generic do-unify-dll(build :: <build>, linker :: <linker>) => ();

define method do-unify-dll(build :: <build>, linker :: <linker>) => ();
  let library :: <symbol> = build.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));

  if (build.exports-only?)
    echo(next-phase(), "Linking %s", build.libfile);
    execute-shell-commands(#f, #f,
			   "linklib",
			   "$(full-lib-name)", build.libfile,
			   "$(objects)",
			   format-to-string("@unify-%s.link", build.dylanapp));
  else
    echo(next-phase(), "Linking %s", build.dll);
    execute-shell-commands(#f, #f,
			   "unifydll",
			   "$(dllname)", build.dylanapp,
			   "$(mangled-dllname)", mangled-library,
			   "$(full-dll-name)", build.dll,
			   "$(base)", build.base-address,
			   "$(image-version)", build.image-version,
			   "$(optimization)",
			   if (library.dylan-library?) "" else "/OPT:REF" end);
    unless (*linker*.fake-imports.empty?)
      uninstall(library-filename-with-extensions(library, "imp", "o"));
    end unless;
    uninstall(library-filename-with-extensions(library, "imp0", "o"));
  end if;
end method;

define method maybe-unify-exe(build :: <build>, all?) => ()
  if (build.build-up-to-date?)
    echo(#f, "%s is up-to-date", build.app);
    install-exe(build);
  else
    compute-number-of-required-phases(2);
    echo-to-client($stage-notifier, "Linking %s", build.dylanlib);
    unify-exe(build, all?);
  end if;
end method;

define method unify-exe(build :: <build>, all?) => ()

  ensure-gc-complete();
  echo(next-phase(), "Linking %s", build.app);
  do-unify-exe(build, build.linker, all?);
  install-exe(build);

end method;

define open generic do-unify-exe(build :: <build>, linker :: <linker>, all?) => ();

define method do-unify-exe(build :: <build>, linker :: <linker>, all?) => ();
  let library :: <symbol> = build.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let entry-point :: <string> =
    concatenate(mangled-library, if (all?) "Exe0" else "Exe" end);

  if (all?)
    if (*exports*.empty?)
      execute-shell-commands(#f, #f,
			     "linklib",
			     "$(full-lib-name)", build.libfile,
			     "$(objects)", "_glue.obj");
    end if;
  end if;

  // Don't use the base address from the "make" file as it's only for DLLs
  execute-shell-commands(#f, #f,
			 "unifyexe",
			 "$(exename)", build.dylanapp,
			 "$(entry-point)", entry-point,
			 "$(full-exe-name)", build.app,
			 "$(image-version)", build.image-version,
			 "$(exports)",
			 if (all?)
			   if (*exports*.empty?)
			     filename-with-extension(build.dylanapp, "exp");
			   else
			     *exports*
			   end;
			 else ""
			 end);
  unless (*linker*.fake-imports.empty?)
    uninstall(library-filename-with-extensions(library, "imp", "o"));
  end unless;
  uninstall(library-filename-with-extensions(library, "imp0", "o"));
end method;


define method maybe-install
    (library :: <symbol>, target :: <string>, #key libraries) => ()

  let cache = *used-project-cache*;
  let build-libs = libraries | *build*.libs;
  let build-location = build-libs[library];

  echo?("Processing library %s", library);
  let used? = element(cache, library, default: #f);
  let processed? = used?;

  if (processed?)
    echo?("Library %s has been looked at already", library);
  else

    echo?("Checking dependent library %s", library);

    if (cache)
      cache[library] := #t;
    end;

    let dotlib = filename-with-extension(library-file-name(library),
					 exports-file-extension(*build*.linker));
    // let lib-file = file-in-directory(*lib-installation*, dotlib);
    
    if (*build*.system-build?)
      let (found?, personal?, project-build-location) = *project-build-info*(library-file-name(library));
      echo?("project-build-info for %= ... found? %= personal? %=  project-build-location %=",
	    library, found?, personal?, project-build-location);
      if (found?)
	if (personal?)
	  build-location := project-build-location;
	else
	  build-location := "system";
	end if;
      else
	// Build Area precedence rule
	let (directory :: false-or(<directory-locator>), system?, error?)
	  = search-for-build-directory($dylanmakefile, library);
	case
	  error?    => build-location := "system";
	  system?   => build-location := "system";
	  otherwise => build-location := directory;
	end case;
      end if;
    end if;

    if (build-location = "system")
      if (target = "builds")
	build-library(library, #f, target, system?: #t);
      else
        echo?("This is a system library")
      end if;
    else
      if (file-exists?(build-location))
	build-library(library, build-location, target);
      else
	echo(#f, "WARNING: Build area %s for library %s does not exist",
	     build-location, library);
	/*
	if (~ file-exists?(lib-file)
	      & ~ file-exists?(file-in-directory($system-lib, dotlib)))
	  build-error("%s does not appear in any Installation Area",
		      dotlib);
	end if;
        */
      end if;
    end if;

  end if;

end method;


define function note-library-modified()
  // record in the cache
  *used-project-cache*[*build*.dylanlib] := #"new";
end;

define method build-library
    (library :: <symbol>, build-location :: false-or(<directory-locator>), target :: <string>,
     #key system?)
 => ()
  build-system-internal(vector(target),
			toplevel?: #f,
			directory: build-location,
			library: library,
			system?: system?);
end method;

define open generic linkable?
    (linker :: <linker>, library :: <symbol>) => (linkable? :: <boolean>);

define method linkable?
    (linker :: <linker>, library :: <symbol>) => (linkable? :: <boolean>)
  #t
end method;

define open generic check-build-dependencies-target 
    (linker :: <linker>, target :: <file-locator>)
 => (target :: <file-locator>);

define method check-build-dependencies-target 
    (linker :: <linker>, target :: <file-locator>)
 => (target :: <file-locator>)
  target
end method;

define method check-build-dependencies
    (target :: <file-locator>, exports-file :: <file-locator>,
     #key up-to-date? = #t)
 => (up-to-date? :: <boolean>)

  let target = check-build-dependencies-target(*build*.linker, target);

  echo?("Checking Dependencies for %s", target);

  // echo-to-client(next-phase(), "Checking Dependencies for %s", target);

  check-foreign-files();

  if (up-to-date?)
    ~ (~ *build*.exports-only?
	 & linkable?(*build*.linker, *build*.dylanlib)
	 & newer-file?(exports-file, target, error?: #f))
    & newest-file?(exports-file, *build*.objs)
    & newest-file?(exports-file, *build*.c-objs)
    & newest-file?(exports-file, *build*.c-src-objs)
    & newest-file?(exports-file, *build*.rc-objs);
  end if;

end method;

define method check-foreign-files() => ()

  for (obj-file in *build*.c-src-objs)
    obj(obj-file);
  end for;

  for (res-file in *build*.rc-objs)
    res(res-file);
  end for;

end method;

define method newest-file?(file :: <file-locator>, files :: <list>) => (most-recent? :: <boolean>)

  block(return)
    for (dependent-file in files)
      if (newer-file?(dependent-file, file))
	echo?("%s is newer than %s", dependent-file, file);
	return(#f);
      else
	echo?("%s is newer than %s", file, dependent-file);
      end if;
    end for;
    #t
  end block;

end method;

define method newer-file?(dependent-file :: <file-locator>, target-file :: <file-locator>,
			  #key error? = #t) => (newer-than? :: <boolean>)

  if (~ file-exists?(dependent-file))
    if (error?)
      build-error("%s not found; Current directory is %s",
		  dependent-file,
		  working-directory());
    end if;
  elseif (~ file-exists?(target-file))
    #t;
  else
    file-property(dependent-file, #"write-date")
      > file-property(target-file, #"write-date");
  end if;

end method;

define method obj(file :: <file-locator>)

  let c-file = new-filename-extension(file, "c");

  if (*build*.force?)
    c-compile(*linker*, c-file);
  elseif (newer-file?(c-file, file))
    c-compile(*linker*, c-file);
  else
    if (*build*.top-level?)
      echo(next-phase(), "%s is up-to-date", file);
    end if;
  end if;

end method;

define open generic c-compile(linker :: <linker>, file :: <file-locator>) => ();

define method c-compile(linker :: <linker>, file :: <file-locator>) => ()

  echo(next-phase(), "Compiling C file %s", file);
  execute-shell-command(#f, #f,
			build-environment-variable("ccompile"),
			file);

end method;

define method res(file :: <file-locator>)

  let rc-file = new-filename-extension(file, "rc");
  let file = linker-resource-object-file(*linker*, file);

  if (*build*.force?)
    rc-compile(rc-file);
  elseif (newer-file?(rc-file, file))
    rc-compile(rc-file);
  else
    if (*build*.top-level?)
      echo(next-phase(), "%s is up-to-date", file);
    end if;
  end if;

end method;

define method rc-compile (file :: <file-locator>) => ()

  let basename = target-library(file);

  echo(next-phase(), "Compiling Resource file %s", file);
  execute-shell-commands(#f, #f,
			 "rccompile",
			 "$(resource)", basename);

end method;

define variable *dll-installation* :: false-or(<directory-locator>) = #f;

define method set-dll-installation () => (installation :: <directory-locator>)
  *dll-installation* :=
    if ($personal-bin)
      $personal-bin
    else
      let directory = subdirectory-locator(locator-directory(working-directory()), "bin");
      ensure-directories-exist(directory);
      directory
    end if;
end method;

define variable *lib-installation* :: false-or(<directory-locator>) = #f;

define method set-lib-installation() => (installation :: <directory-locator>)
  *lib-installation* :=
    if ($personal-lib)
      $personal-lib
    else
      let directory = subdirectory-locator(locator-directory(working-directory()), "lib");
      ensure-directories-exist(directory);
      directory
    end if;
end method;

define variable *release-installation* = #f;

define method set-release-installation () => (installation :: <directory-locator>)
  let directory
    = if ($personal-install)
	subdirectory-locator($personal-install, "release")
      else
	subdirectory-locator(locator-directory(working-directory()), "release")
      end if;
  ensure-directories-exist(directory);
  *release-installation* := directory
end method;

define method install-dll(build :: <build>) => ()

  do-install-dll(build, build.linker);
  let libfile = build.libfile;
  unless (build.dll = libfile)

    let destination = file-in-directory(*lib-installation*, libfile.locator-name);

    if (newer-file?(libfile, destination, error?: #f))
      echo(next-phase(), "Installing %s in %s", libfile, *lib-installation*);
      copy-build-file(libfile, destination,
		      if-exists: #"replace");
      unless ((build == *build*) | build.system-build?)
	copy-build-file(libfile,
			file-in-directory(build.build-directory,
					  as(<string>, libfile)),
			if-exists: #"replace");
	uninstall(libfile);
      end unless;
      // Install *this* makefile
      install-dylanmakefile(*build*);
      echo-to-shell("\n");
    end if;

  end unless;

end method;

define method install-dylanmakefile 
    (build :: <build>, #key directory :: false-or(<directory-locator>))
 => ()
  let dylanmakefile :: <file-locator> =
    if (directory)
      make(<file-locator>,
	   directory: directory,
	   name:      $dylanmakefile)
    else
      as(<file-locator>, $dylanmakefile)
    end if;

  copy-build-file(dylanmakefile,
		  make(<file-locator>,
		       directory: *lib-installation*,
		       name:      as-lowercase(as(<string>, build.dylanlib)),
		       extension: $makefile-extension),
		  if-exists: #"replace");
end method;

define open generic do-install-dll(build :: <build>, linker :: <linker>) => ();

define method do-install-dll(build :: <build>, linker :: <linker>) => ()
  let library = build.dylanlib;

  unless (~ linkable?(linker, library) | build.exports-only?)
    let dll = build.dll;
    let destination = file-in-directory(*dll-installation*, dll.locator-name);

    if (newer-file?(build.libfile, destination, error?: #f))
      note-library-modified();
      echo(next-phase(), "Installing %s in %s", dll, *dll-installation*);
    copy-build-file(dll, destination,
		    if-exists: #"replace");
    unless ((build == *build*) | build.system-build?)
      copy-build-file(dll,
		      file-in-directory(build.build-directory, dll.locator-name),
		      if-exists: #"replace");
      uninstall(dll);
    end unless;
  end if;

  end unless;
end method;

define method install-target() => ()

  install-libraries(target: "separate-dll");
  more-targets();

end method;

define method separate-exe-target() => ()

  build-app();
  more-targets();

end method;

define method exe-target() => ()
  *build*.exports-only? := #f;
  if (*unify-all?*)
    unify-all-exe-target();
  else
    select (default-dll-policy())
      #"single-dlls", #"separate-dlls" => separate-exe-target();
      otherwise => unify-exe-target();
    end select;
  end if;
end method;

define method build-app() => ()

  local method main()
	  case
	    *build*.force? =>
	      check-foreign-files();
	      link-exe();
	    otherwise =>
	      let up-to-date? =	check-build-dependencies(*build*.app, *build*.libfile);
	      if (up-to-date?)
		echo(#f, "%s is up-to-date", *build*.app);
		install-exe(*build*);
	      else
		compute-number-of-required-phases(2);
		echo-to-client($stage-notifier, "Linking %s", *build*.dylanlib);
		link-exe();
	      end if;
	  end case;
	end method;

  if (*build*.link?)
    check-foreign-files();
    link-exe();
  else
    install-libraries(target: "separate-dll");
    main();
  end if;

end method;

define method link-exe() => ()

  ensure-gc-complete();
  echo(next-phase(), "Linking %s", *build*.app);
  do-link-exe(*build*.linker);
  install-exe(*build*);

end method;

define open generic do-link-exe(linker :: <linker>) => ();

define method do-link-exe(linker :: <linker>) => ();
  let library :: <symbol> = *build*.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let import-fixups = library-filename-with-extensions(library, "imp0", "o");
  let dylan-library? = library.dylan-library?;

  unless (dylan-library?) write-build-import-fixups(*build*) end;

  // Don't use the base address from the "make" file as it's only for DLLs
  execute-shell-commands(#f, #f,
			 "linkexe",
			 "$(exename)", library,
			 "$(mangled-exename)", mangled-library,
			 "$(full-exe-name)", *build*.app,
			 "$(objects)",
			 concatenate(if (dylan-library?) #()
				     else list(import-fixups) end,
				     *build*.c-objs,
				     *build*.c-src-objs,
				     *build*.rc-objs),
			 "$(c-libs)",
			 concatenate(substitute-environment-variables(*build*.c-libs),
				     *build*.rtlibs),
			 "$(linkopts)", substitute-environment-variables(*build*.linkopts),
			 "$(image-version)", *build*.image-version);
  uninstall(import-fixups);
end method;

define method install-exe(build :: <build>) => ()
  let destination = file-in-directory(*dll-installation*, build.app.locator-name);

  if (newer-file?(build.app, destination, error?: #f))
    uninstall(destination);
    echo(next-phase(), "Installing %s in %s", build.app, *dll-installation*);
    copy-build-file(build.app, destination);
    unless ((build == *build*) | build.system-build?)
      copy-build-file(build.app,
		      file-in-directory(build.build-directory, build.app),
		      if-exists: #"replace");
      uninstall(build.app);
    end unless;
  end if;
end method;

define method install-app-target() => ()

  build-app();
  more-targets();

end method;

define method release-target() => ()
  select (default-dll-policy())
    #"single-dlls", #"separate-dlls" => do-release();
    otherwise => do-release-with-unification();
  end select;
  more-targets();
end method;

define method do-release() => ()

  let builds :: <stretchy-vector> = do-builds-target();
  let system-bin = as(<string>, $system-bin);
  let redistributable-bin = as(<string>, $redistributable-bin);
  let personal-bin = as(<string>, *dll-installation*);
  let release-bin = as(<string>, *release-installation*);

  *build*.build-phases := builds.size;
  echo($stage-notifier,
       "Making release for %s in %s", *build*.dylanlib,
       release-bin);

  local method build-file(build :: <build>, dll?)
	  let dll = if (dll?) build.dll else build.app end;
	  let system? = build.system-build?;

	  if (system?)
	    // Override Bin with an existing Redistributable directory
	    let system-file = file-in-directory(redistributable-bin, dll);
	    if (file-exists?(system-file)) system-file
	    else file-in-directory(system-bin, dll)
	    end
	  else file-in-directory(personal-bin, dll)
	  end;
	end method;

  local method copy-release-file(build :: <build>,
				 #key dll? = #t, error? = #t)
	  let build-file = build-file(build, dll?);
	  let destination =
	    if (dll?) file-in-directory(release-bin, build.dll);
	    else file-in-directory(release-bin, build.app);
	    end;

	  if (file-exists?(build-file))
	    echo(next-phase(), "Installing %s", build-file);
	    copy-build-file(build-file, destination,
			    if-exists: #"replace");
	  else
	    if (error?)
	      build-error("Required release component %s not found", build-file);
	    end;
	  end;
	end method;

  for (build :: <build> in builds)
    unless (build == *build*)
      copy-release-file(build);
    end;
  end for;

  copy-release-file(*build*, error?: #f);
  copy-release-file(*build*, dll?: #f);

end method;


define method do-release-with-unification() => ()
  let builds :: <stretchy-vector> = do-builds-target();

  let (exported-builds :: <list>,
       imported-libraries :: <string-table>, 
       imported-builds :: <list>, 
       imported-group-builds :: <table>,
       winning-build :: <build>) =
    process-builds-for-unification(builds, #f, #f, check?: #f);

  *build*.build-phases := imported-builds.size + imported-group-builds.size + 1;
  echo($stage-notifier,
       "Making release for %s in %s", *build*.dylanlib,
       as(<string>, *release-installation*));

  local method build-file(build :: <build>, dll?, system?)
	  let dll = if (dll?) build.dll else build.app end;
	  let system? =
	    if (supplied?(system?)) system?
	    else build.system-build? end;

	  if (system?)
	    // Override Bin with an existing Redistributable directory
	    let system-file = file-in-directory($redistributable-bin, dll);
	    if (file-exists?(system-file)) system-file
	    else file-in-directory($system-bin, dll)
	    end
	  else file-in-directory(*dll-installation*, dll)
	  end;
	end method;

  local method copy-release-file(build :: <build>,
				 #key dll? = #t,
				      error? = #t,
				      system? = unsupplied())
	  let build-file = build-file(build, dll?, system?);
	  let destination =
	    if (dll?) file-in-directory(*release-installation*, build.dll);
	    else file-in-directory(*release-installation*, build.app);
	    end;

	  if (file-exists?(build-file))
	    echo(next-phase(), "Installing %s", build-file);
	    copy-build-file(build-file, destination,
			    if-exists: #"replace");
	  else
	    if (error?)
	      build-error("Required release component %s not found", build-file);
	    end;
	  end;
	end method;

  for (build :: <build> in imported-builds)
    copy-release-file(build);
  end for;

  for (build :: <build> in imported-group-builds.key-sequence)
    let build? = imported-group-builds[build];
    if (build?)
      copy-release-file(build, system?: #f);
    else
      copy-release-file(build);
    end;
  end for;


  copy-release-file(winning-build, error?: #f);
  copy-release-file(winning-build, dll?: #f, error?: #f);

end method;


// Take a shortcut to the Linker
// Bypass all dependency checking

define method link-target() => ()

  *build*.link? := #t;
  more-targets();

end method;

define method clean-target() => ()

  clean();
  more-targets();

end method;

define method clean-build(build :: <build>) => ()

  let personal-bin = *dll-installation*;
  let personal-lib = *lib-installation*;

  unless (build.system-build?)
    with-build-directory(build.build-directory)
      uninstall(filename-with-extension(build.dylanapp, "exp"));
      uninstall(build.libfile);
      uninstall(build.dll);
      uninstall(build.app);
      map(uninstall, build.objs);
      map(uninstall, build.c-src-objs);
      map(uninstall, build.rc-objs);
      let bare-objects = map(target-library, build.objs);
      for (object :: <string> in bare-objects)
	uninstall(filename-with-extension(object, "import"));
	uninstall(filename-with-extension(object, "el"));
	uninstall(filename-with-extension(object, "def"));
      end for;
      uninstall(filename-with-extension(build.dylanapp, "link"));
      uninstall(filename-with-extension(format-to-string("unify-%s", build.dylanapp),
					"link"));
      uninstall(as(<file-locator>, $dylanmakefile));
      uninstall(as(<file-locator>, "_SRV"));
    end with-build-directory;
  end unless;

  uninstall(file-in-directory(personal-lib, build.libfile));
  uninstall(file-in-directory(personal-bin, build.dll));
  uninstall(file-in-directory(personal-bin, build.app));
  uninstall(file-in-directory(personal-lib, library-file-name(build.dylanlib),
			      extension: $makefile-extension));

end method;

define method clean() => ()
  clean-build(*build*);
end method;

define method clean-all-target() => ()

  let builds :: <stretchy-vector> = do-builds-target();
  for (build :: <build> in builds)
    clean-build(build);
  end for;
  more-targets();

end method;

define method target-library
    (target :: <file-locator>) => (library :: <string>)
  target.locator-base
end method target-library;

/*---*** andrewa: everything should be using locators now...
define method target-library
    (target :: <string>) => (library :: <symbol>)
  target-library(as(<file-locator>, target))
end method target-library;
*/

define method run-target() => ()

  execute-shell-command
    (#t, #f,
     file-in-directory(*dll-installation*, *build*.app));
  more-targets();

end method;

define method obj-target() => ()

  obj(as(<file-locator>, next-target()));
  more-targets();

end method;

define method res-target() => ()

  res(as(<file-locator>, next-target()));
  more-targets();

end method;

define method sublib-target() => ()

  let target = next-target();
  let library = target-library(target);

  // run at top-level
  build-system-internal(#["lib"],
			directory: as(<directory-locator>,
				      format-to-string("..\\%s\\", library)));

  more-targets();

end method;

define method subdll-target() => ()

  let target = next-target();
  let library = target-library(target);

  // run at top-level
  build-system-internal(#["dll"],
			directory: as(<directory-locator>,
				      format-to-string("..\\%s\\", library)));

  more-targets();

end method;

define method subexe-target() => ()

  let target = next-target();
  let library = target-library(target);

  // run at top-level
  build-system-internal(#["exe"],
			directory: as(<directory-locator>,
				      format-to-string("..\\%s\\", library)));

  more-targets();

end method;

define method backup-target() => ()

  backup();
  more-targets();

end method;

define method backup() => ()

  let app = *build*.app;
  let dll = *build*.dll;
  let lib = *build*.libfile;
  let personal-bin = *dll-installation*;
  let personal-lib = *lib-installation*;

  let bin-backup = subdirectory-locator(personal-bin, "backup");
  let lib-backup = subdirectory-locator(personal-lib, "backup");

  ensure-directories-exist(bin-backup);
  ensure-directories-exist(lib-backup);

  let app-loc = file-in-directory(personal-bin, app);
  if (file-exists?(app-loc))
    echo(#f, "Backing up %s into %s",
	 app,
	 bin-backup);
    copy-build-file(app-loc,
		    file-in-directory(bin-backup, app),
		    if-exists: #"replace");
  end if;

  let dll-loc = file-in-directory(personal-bin, dll);
  if (file-exists?(dll-loc))
    echo(#f, "Backing up %s into %s",
	 dll,
	 bin-backup);
    copy-build-file(dll-loc,
		    file-in-directory(bin-backup, dll),
		    if-exists: #"replace");
  end if;

  let lib-loc = file-in-directory(personal-lib, lib);
  if (file-exists?(lib-loc))
    echo(#f, "Backing up %s into %s",
	 lib,
	 lib-backup);
    copy-build-file(lib-loc,
		    file-in-directory(lib-backup, lib),
		    if-exists: #"replace");
  end if;

end method;

define method restore-target() => ()

  restore();
  more-targets();

end method;

define method restore() => ()

  let app = *build*.app;
  let dll = *build*.dll;
  let lib = *build*.libfile;
  let personal-bin = *dll-installation*;
  let personal-lib = *lib-installation*;

  let bin-backup = subdirectory-locator(personal-bin, "backup");
  let lib-backup = subdirectory-locator(personal-lib, "backup");
  let backup-app = file-in-directory(bin-backup, app);
  let backup-dll = file-in-directory(bin-backup, dll);
  let backup-lib = file-in-directory(lib-backup, lib);

  if (file-exists?(backup-app))
    echo(#f, "Restoring %s into %s",
	 app,
	 personal-bin);
    copy-build-file(backup-app,
		    file-in-directory(personal-bin, app),
		    if-exists: #"replace");
  end if;

  if (file-exists?(backup-dll))
    echo(#f, "Restoring %s into %s",
	 dll,
	 personal-bin);
    copy-build-file(backup-dll,
		    file-in-directory(personal-bin, dll),
		    if-exists: #"replace");
  end if;

  if (file-exists?(backup-lib))
    echo(#f, "Restoring %s into %s",
	 lib,
	 personal-lib);
    copy-build-file(backup-lib,
		    file-in-directory(personal-lib, lib),
		    if-exists: #"replace");
  end if;

end method;

define method backup-all-target() => ()

  *build*.backup? := #t;
  build-system-internal(#["separate-dll"],
			toplevel?: #f,
			directory: working-directory());
  more-targets();

end method;

define method restore-all-target() => ()

  *build*.restore? := #t;
  build-system-internal(#["separate-dll"],
			toplevel?: #f,
			directory: working-directory());
  more-targets();

end method;


// Linker Targets -- these dynamically specialize the Build System's behaviour

define method microsoft-target() => ()
  *linker* := make-linker($microsoft-linker);
  initialize-linker();
  more-targets();
end method;

define method gnu-target() => ()
  if ($gnu-linker)
    // if gnu-linker DLLs are currently loaded, just switch linkers
    *linker* := make-linker($gnu-linker);
    initialize-linker();
    more-targets();
  else
    build-error("GNU Linker not loaded into Build System");
  end if;
end method;

define method ccl-target() => ()
  if ($ccl-linker)
    *linker* := make-linker($ccl-linker);
    initialize-linker();
    more-targets();
  else
    build-error("C Compiler & Linker not loaded into Build System");
  end if;
end method;

define method elf-target() => ()
  if ($elf-linker)
    *linker* := make-linker($elf-linker);
    $override-default-dll-policy := #"separate-dlls";
    initialize-linker();
    more-targets();
  else
    build-error("Elf Linker not loaded into Build System");
  end if;
end method;

define method microsoft-linker-target() => ()
  default-linker() := #"microsoft";
  more-targets();
end method;

define method gnu-linker-target() => ()
  default-linker() := #"gnu";
  more-targets();
end method;

define method separate-dlls-target() => ()
  default-dll-policy() := #"separate-dlls";
  more-targets();
end method;

define method combined-dlls-target() => ()
  default-dll-policy() := #"combined-dlls";
  more-targets();
end method;

define method combined-runtime-dlls-target() => ()
  default-dll-policy() := #"combined-runtime-dlls";
  more-targets();
end method;

define method other-targets() => ()

  let target = next-target();
  case
    subsequence-position(target, ".dll") => subdll-target();
    subsequence-position(target, ".exe") => subexe-target();
    subsequence-position(target, ".lib") => sublib-target();
    subsequence-position(target, ".obj") => obj-target();
    subsequence-position(target, ".res") => res-target();
    (*build*.interactive? & first-target?()) =>
      execute-shell-command(#t, #t,
			    *command-line*);
      signal(make(<no-more-targets>));
    otherwise =>
      illegal-target(target);
  end case;

end method;

define method illegal-target(target :: <string>) => ()

  print-usage();
  build-error("No such target %s", target);

end method;

define method print-usage() => ()

  echo(#f, "VALID TARGETS: %=",
       *targets*.key-sequence);

end method;

define method help-target() => ()
  format-out("\nDYLAN BUILD SYSTEM"
             "\n"
             "\n  AVAILABLE TARGETS AND THEIR ACTIONS:"
             "\n  -----------------------------------"
             "\n"
             "\n  build microsoft-linker        Make the microsoft linker the Windows default"
             "\n  build gnu-linker              Make the gnu linker the Windows default"
             "\n  build microsoft               Use the microsoft linker in this build session"
             "\n  build gnu                     Use the gnu linker in this build session"
             "\n"
             "\n  build separate-dlls           Make one-dylan-library-per-DLL the Windows default"
             "\n  build combined-dlls           Make several-dylan-libraries-per-DLL the Windows default"
             "\n  build combined-runtime-dlls   Make several-dylan-libraries-per-DLL-for-runtime-libraries-only the Windows default"
             "\n"
             "\n  build install         builds and installs dependent LIBs and DLLs"
             "\n"
             "\n  build dll             builds and installs toplevel DLL"
             "\n  build *.dll           builds and installs specified DLL"
             "\n"
             "\n  build exe             builds an EXE"
             "\n  build app             builds an EXE"
             "\n  build *.exe           builds specified EXE"
             "\n  build install-app     builds and installs an EXE"
             "\n  build run             runs an EXE"
             "\n"
             "\n  build exports         builds and installs toplevel exports"
             "\n  build lib             builds and installs toplevel exports"
             "\n  build *.lib           builds and installs specified exports"
             "\n"
             "\n  build clean           Deletes build products"
             "\n  build clean-all       Deletes build and compiler products"
             "\n"
             "\n  build *.obj           Compile C file"
             "\n  build *.res           Compile Resource file"
             "\n");
  format-out("\n  build help            Displays information about build"
             "\n"
             "\n  build force target    forces linking of all dependent DLLs and EXEs"
             "\n  build force install   re-install all dependent dlls"
             "\n"
             "\n  build link dll        A shortcut to the Linker(no dependency checks)"
             "\n  build link app        A shortcut to the Linker(no dependency checks)"
             "\n"
             "\n  build unify-all       Generates a single .exe or .dll that includes all sub-libraries"
             "\n"
             "\n  build unify-exe       Generates configured groups of .dlls and .exe"
             "\n  build unify           Generates configured groups of .dlls and .exe"
             "\n  build unify-dll       Generates configured groups of .dlls"
             "\n  build combined-exe    Generates configured groups of .dlls and .exe"
             "\n  build combined-dll    Generates configured groups of .dlls"
             "\n"
             "\n  build separate-dll    Generates individual DLLs for all dylan libraries"
             "\n  build separate-exe    Generates individual DLLs and EXEs for all dylan libraries"
             "\n"
             "\n  build archive lib     Generates archive libraries from .import files"
             "\n"
             "\n  build echo            activate build echos(for debugging)"
             "\n"
             "\n  build backup          backup toplevel builds in personal-bin\\backup"
             "\n  build backup-all      backup all dependent builds in personal-bin\\backup"
             "\n  build restore         restore toplevel builds from personal-bin\\backup"
             "\n  build restore-all     restore all dependent builds from personal-bin\\backup"
             "\n"
             "\n  You can specify any number of targets to build"
             "\n"
             "\n"
             "\n");
  format-out("\n  WHAT YOU MUST DO FIRST:"
             "\n  ----------------------"
             "\n"
             "\n    SET UP YOUR ENVIRONMENT"
             "\n      C:> U:\\dylan\\admin\\builds\\set-build-variables -p C:\\personal-dylan -s C:\\Progra~1\\Functi~1\\Dylan"
             "\n"
             "\n    BE IN THE RIGHT DIRECTORY"
             "\n      C:> cd C:\\personal-dylan\\build\\library"
             "\n"
             "\n"
             "\n  EXAMPLES:"
             "\n  --------"
             "\n"
             "\n  TO BUILD AN EXECUTABLE, THIS IS SUFFICIENT"
             "\n    C:library> build app"
             "\n"
             "\n  TO BUILD A DLL WITHOUT DEPENDENCY CHECKING"
             "\n  (same as typing link /incremental:no /dll to shell)"
             "\n    C:library> build link dll"
             "\n"
             "\n  TO FORCE A COMPLETE REBUILD OF EVERYTHING AN APPLICATION DEPENDS ON"
             "\n    C:library> build force app"
             "\n"
             "\n  TO FORCE A COMPLETE REBUILD OF EVERYTHING A DLL DEPENDS ON"
             "\n    C:library> build force dll"
             "\n"
             "\n  TO RE-BUILD A COLLECTION OF DLLS"
             "\n  (you don't have to change directory to do this)"
             "\n    C:library> build link lib1.dll lib2.dll lib3.dll lib4.dll lib5.dll lib6.dll"
             "\n"
             "\n  TO RUN AN EXECUTABLE"
             "\n  (same as typing executable to shell)"
             "\n    C:library> build run"
             "\n"
             "\n"
             "\n");
  format-out("\n  LINKER SUPPORT"
	     "\n  --------------"
             "\n"
	     "\n  By default, the build system will use the MicroSoft Linker but also supports"
	     "\n  the GNU Linker"
             "\n"
	     "\n  To use the GNU Linker"
	     "\n  - use the 'gnu' target of build"
             "\n"
	     "\n  You can use the 'microsoft' target to switch back to the MicroSoft Linker"
	     "\n  in the same build session"
             "\n"
	     "\n  EXAMPLE:   pentium-dw> build dylan.dll gnu app"
             "\n"
	     "\n  This will build the dylan library using the MicroSoft Linker, and the rest of "
	     "\n  the world using the GNU Linker"
             "\n"
             "\n  You can use Linkers interchangeably in this way"
             "\n"
             "\n"
             "\n");
  more-targets();
end method;

define method cd-target() => ()
  shift-targets();
  let target = next-target();
  if (file-exists?(format-to-string("%s\\dylanmakefile", target)))
    change-directory(target);
    *linker* := make-linker(as(<symbol>, *linker*.linker-name));
    *build* := read-buildfile();
  else
    build-error("Cannot change directory to an Invalid Build directory");
  end if;
  signal(make(<no-more-targets>));
end method;

define method exit-target() => ()
  signal(make(<exit-interactive>));
end method;

define method dump-target() => ()

  local method dump-objects(build :: <build>)
	  unless (build.system-build?)
	    for (object in build.objs)
	      execute-shell-commands(#t, #t,
				     "dumpobjects",
				     "$(object)", object);
	    end for;
	  end unless;
	end method;
  if (*build*.link?)
    dump-objects(*build*);
  else
    let builds :: <stretchy-vector> = do-builds-target();
    for (build :: <build> in builds)
      dump-objects(build);
    end for;
  end if;
  more-targets();

end method;

// Different linkers in conjunction with the compiler backend, may want to use a separate 
// main unit to avoid link-time duplicate symbol definitions for unified library links.
// If they elect to do this, there is a separate main object for each library; only the 
// toplevel library's copy is included in the unified link.

define open generic build-main-object?(linker :: <linker>) => (main? :: <boolean>);

define method build-main-object?(linker :: <linker>) => (main? :: <boolean>)
  #f
end method;

define method build-main-object(linker :: <linker>) => (main :: <file-locator>)
  object-filename-from-base-filename(linker, "_main")
end method;

// These targets are for assembling ELF assembler files
// on Linux, that have been cross-compiled from a Windows
// Platform

define method assembler-file-location
    (object :: <file-locator>) => (file-location :: <file-locator>)
  new-filename-extension(object, "s")
end method assembler-file-location;

define method assemble-object
    (object :: <file-locator>)
  let asm-file = assembler-file-location(object);
  if (*build*.link? | *build*.force? | newer-file?(asm-file, object))
    // format-out("\nAssembling %= in %=\n", asm-file, build.build-directory);
    execute-shell-commands
      (#t, #f,
       "assemble",
       "$(object)", object,
       "$(source)", asm-file);
  end if;
end method;

define method assemble-objects(build :: <build>)
  with-build-directory(build.build-directory)
    for (object :: <file-locator> in build.objs)
      assemble-object(object);
    end for;
    if (build-main-object?(*linker*))
      assemble-object(build-main-object(*linker*));
    end if;
  end;
end method;

define method assemble-target() => ()

  assemble-objects(*build*);
  more-targets();

end method;

define method assemble-all-target() => ()

  let builds :: <stretchy-vector> = do-builds-target();
  for (build :: <build> in builds)
    assemble-objects(build);
  end for;
  more-targets();

end method;


define macro &build-target-definer
  { define &build-target ?:name }
    => { 
	let target = as-lowercase(?"name");
	*targets*[target] := ?name ## "-target";
       }
end macro;

define macro &simple-build-target-definer
  { define &simple-build-target ?:name }
    => { 
	let target = as-lowercase(?"name");
	*targets*[target] := ?name ## "-target";
	*simple-targets*[target] := #t;
       }
end macro;

define macro &build-target-alias-definer
  { define &build-target-alias ?:name ?target:name}
    => { 
	let target = as-lowercase(?"name");
	*targets*[target] := ?target ## "-target";
       }
end macro;

define macro &simple-build-target-alias-definer
  { define &simple-build-target-alias ?:name ?target:name}
    => { 
	let target = as-lowercase(?"name");
	*targets*[target] := ?target ## "-target";
	*simple-targets*[target] := #t;
       }
end macro;

define &build-target default;

define &build-target force;
define &build-target link;
define &build-target echo;

define &build-target lib;
define &build-target-alias exports lib;
define &build-target archive;

define &build-target unify-dll;
define &build-target-alias combined-dll unify-dll;
define &build-target unify-exe;
define &build-target-alias combined-exe unify-exe;
define &build-target-alias unify unify-exe;
define &build-target unify-all;
define &build-target builds;

define &build-target dll;
define &build-target exe;
define &build-target-alias app exe;
define &build-target separate-dll;
define &build-target separate-exe;

// ELF Linker shared-objects are dll aliases on Linux
define &build-target-alias so dll;

define &build-target install;
define &build-target install-app;

define &build-target release;

define &build-target clean;
define &build-target clean-all;

define &build-target run;

define &build-target backup;
define &build-target restore;
define &build-target backup-all;
define &build-target restore-all;

define &simple-build-target microsoft;
define &simple-build-target gnu;
define &simple-build-target ccl;
define &simple-build-target elf;

define &simple-build-target microsoft-linker;
define &simple-build-target gnu-linker;

define &simple-build-target separate-dlls;
define &simple-build-target combined-dlls;
define &simple-build-target combined-runtime-dlls;
define &simple-build-target-alias single-dlls separate-dlls;
define &simple-build-target-alias multiple-dlls combined-dlls;

define &build-target dump;

define &build-target assemble;
define &build-target assemble-all;

define &simple-build-target help;

define macro define-interactive-target
  { define-interactive-target(?:name) }
    =>
  {
   *targets*[as-lowercase(?"name")] := ?name ## "-target"
  }
end macro;

define method install-interactive-targets() => ()
  define-interactive-target(cd);
  define-interactive-target(exit);
end method;

