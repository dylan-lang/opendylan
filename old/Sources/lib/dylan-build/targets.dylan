Module:    dylan-build
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Person-to-blame: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable *targets* = make(<string-table>);

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

// The Targets

define method force-target() => ()

  *build*.force? := #t;
  more-targets();

end method;

define method test-target() => ()

  *build*.test? := #t;
  echo-target()

end method;

define method echo-target() => ()

  *build*.dylanecho? := #t;
  more-targets();

end method;

define method default-target() => ()

  if (*build*.more-targets?)
    signal(make(<no-more-targets>));
  else
    help-target();
  end if;

end method;

define method dll-target() => ()

  build-dll();
  more-targets();

end method;

define method build-dll() => ()
  local method main()
	  if (*build*.build-dll?)
	    echo("Relinking %=, dependent library changed\n", *build*.dll);
	    check-foreign-files();
	    link-dll();
	  else
	    let up-to-date? = check-dependencies(*build*.dll);
	    if (up-to-date?)
	      echo("%= is up-to-date\n", *build*.dll);
	      if(*build*.force?) link-dll() else install-dll() end;
	    else
	      link-dll();
	    end if;	
	  end if;
	end method;

  install-libraries();
  main();

end method;

define method link-dll() => ()

  echo("Linking DLL %s\n", *build*.dll);
  ~*build*.test? &
    execute-shell-command(#f,
			  format-to-string(build-environment-variable("linkdll"), *build*.dll),
			  concatenate("@", filename-with-extension(as(<string>, *build*.dylanlib), "link")),
			  *build*.c-objs,
			  *build*.c-src-objs,
			  *build*.rc-objs,
			  *build*.c-libs,
			  *build*.rtlibs,
			  *build*.base,
			  *build*.linkopts);
  //echo("Linked DLL %s\n", *build*.dll);
  uninstall(concatenate($personal-lib, *build*.libfile));
  uninstall(concatenate($personal-bin, *build*.dll));
  install-dll();

end method;

define method uninstall(file :: <string>) => ()

  echo?("Deleting file %s\n", file);

  if (file-exists?(file))
    ~*build*.test? & delete-build-file(file);
  end if;

end method;

define method install-libraries() => ()

  map(maybe-install-dependent, *build*.used-libs.key-sequence);

end method;

define method maybe-install-dependent(library :: <symbol>) => ()

  let cache = *used-project-cache*;
  cache & element(cache, library, default: #f)
    & begin
	if(cache[library] = #"new")
	  note-library-modified()
	end if;
	#t
      end
    | begin
	echo?("Checking dependent library %s\n", library);

	if(cache)
	  cache[library] := #t;
	end;

	let build-location = *build*.used-libs[library];

	if(build-location = "system")
	  echo?("This is a system library\n")
	else
	  if (file-exists?(build-location))
	    maybe-build-library(library, build-location);
	  else
	    build-error("Build area for library %s does not exist", library);
	  end if;
	end if;
      end;
end method;


define function note-library-modified()
  // record in the cache
  *used-project-cache*[*build*.dylanlib] := #"new";
  // report to the higher up library
  // dont do that anymore
  //*build*.build-dll? := #t;
  //*build*.build-exe? := #t;
end;

define method maybe-build-library(library :: <symbol>, build-location :: <string>)
 => ()
  let (dll?, exe?) = 
  build-system-internal(#["dll"],
			toplevel?: #f,
			directory: build-location);
  // just call note-library-changed() ?
  *build*.build-dll? := dll?;
  *build*.build-exe? := exe?;
  dbg-msg("Checked library %s, setting build-dll for %s to %=\n", 
	  library, *build*.dylanlib, dll?);
  echo("\n");

end method;

define method check-dependencies(target :: <string>) => (up-to-date? :: <boolean>)

  echo?("Checking Dependencies for %s\n", target);

  check-foreign-files();

  newest-file?(target, *build*.objs)
  & newest-file?(target, *build*.c-objs)
  & newest-file?(target, *build*.c-src-objs)
  & newest-file?(target, *build*.rc-objs);

end method;

define method check-foreign-files() => ()

  for (obj-file in *build*.c-src-objs)
    obj(obj-file);
  end for;

  for (res-file in *build*.rc-objs)
    res(res-file);
  end for;

end method;

define method newest-file?(file :: <string>, files :: <list>) => (most-recent? :: <boolean>)

  block(return)
    for (dependent-file in files)
      if (newer-file?(dependent-file, file))
	echo?("%s is newer than %s\n", dependent-file, file);
	return(#f);
      else
	echo?("%s is newer than %s\n", file, dependent-file);
      end if;
    end for;
    #t
  end block;

end method;

define method newer-file?(dependent-file :: <string>, target-file :: <string>) => (newer-than? :: <boolean>)

  block(return)
    if (~ file-exists?(dependent-file))
      if(*build*.test?)
	return(#t)
      else
	build-error("%s not found; Current directory is %s",
		    dependent-file,
		    working-directory());
      end if;
    end if;

    if (~ file-exists?(target-file))
      #t;
    else
      file-property(dependent-file, #"write-date")
	> file-property(target-file, #"write-date");
    end if;
  end block;
end method;

define method obj(file :: <string>)

  let c-file =
    filename-with-extension(filename-without-extension(file), "c");

  if (*build*.force?)
    c-compile(c-file);
  elseif (newer-file?(c-file, file))
    c-compile(c-file);
  else
    echo("%= is up-to-date\n", file);
  end if;

end method;

define method c-compile(file :: <string>) => ()

  echo("Compiling C file %s\n", file);
  ~*build*.test? &
    execute-shell-command(#f,
			  build-environment-variable("ccompile"),
			  file);

end method;

define method res(file :: <string>)

  let rc-file =
    filename-with-extension(filename-without-extension(file), "rc");

  if (*build*.force?)
    rc-compile(rc-file);
  elseif (newer-file?(rc-file, file))
    rc-compile(rc-file);
  else
    echo("%= is up-to-date\n", file);
  end if;

end method;

define method rc-compile(file :: <string>) => ()

  echo("Compiling Resource file %s\n", file);
  ~*build*.test? &
    execute-shell-command(#f,
			  build-environment-variable("rccompile"),
			  file);

end method;

define method install-dll() => ()
  let destination = concatenate($personal-lib, *build*.libfile);
  if (newer-file?(*build*.libfile, destination))
    echo("Installing %s in %s\n", *build*.libfile, $personal-lib);
    note-library-modified();
    ~*build*.test? &
      copy-build-file(*build*.libfile, destination,
		      if-exists: #"replace");
  end if;
  let destination = concatenate($personal-bin, *build*.dll);
  if (newer-file?(*build*.dll, destination))
    echo("Installing %s in %s\n", *build*.dll, $personal-bin);
    note-library-modified();
    ~*build*.test? &
      copy-build-file(*build*.dll, destination,
		      if-exists: #"replace");
  end if;

end method;

define method install-target() => ()

  install-libraries();
  more-targets();

end method;

define method app-target() => ()

  build-app();
  more-targets();

end method;

define method exe-target() => ()

  app-target();

end method;

  
define method build-app() => ()

  local method main()
	  if (*build*.build-exe?)
	    echo("Relinking %=, dependent library changed\n", *build*.dylanapp);
	    check-foreign-files();
	    link-exe();
	  else
	    let up-to-date? = check-dependencies(*build*.app);
	    if (up-to-date?)
	      echo("%= is up-to-date\n", *build*.dylanapp);
	      if(*build*.force?) link-exe() else install-exe() end;
	    else
	      link-exe();
	    end if;	
	  end if;
	end method;

    install-libraries();
    main();

end method;

define method link-exe() => ()

  echo("Linking EXE %s\n", *build*.dylanapp);
  ~*build*.test? &
    execute-shell-command(#f,
			  format-to-string(build-environment-variable("linkexe"), *build*.app),
			  concatenate("@", filename-with-extension(as(<string>, *build*.dylanlib), "link")),
			  *build*.c-objs,
			  *build*.c-src-objs,
			  *build*.rc-objs,
			  *build*.c-libs,
			  *build*.rtlibs);
  //echo("Linked EXE %s\n", *build*.dylanapp);
  uninstall(concatenate($personal-lib, *build*.libfile));
  install-exe();

end method;

define method install-exe() => ()
  let destination = override-locator($personal-lib, name: *build*.libfile);
  unless (file-exists?(destination))
    echo("Installing %s in %s\n", *build*.libfile, $personal-lib);
    ~*build*.test? & copy-build-file(*build*.libfile, destination);
  end unless;
end method;

define method install-app-target() => ()

  build-app();
  let destination = override-locator($personal-bin, name: *build*.app);
  uninstall(destination);
  echo("Installing %s in %s\n", *build*.app, $personal-bin);
  ~*build*.test? & copy-build-file(*build*.app, destination);
  more-targets();

end method;

define method clean-target() => ()

  clean();
  more-targets();

end method;

define method clean() => ()

  uninstall(*build*.libfile);
  uninstall(filename-with-extension(*build*.dylanapp, "exp"));
  uninstall(*build*.dll);
  uninstall(*build*.app);
  map(uninstall, *build*.c-src-objs);
  map(uninstall, *build*.rc-objs);

end method;

define method clean-all-target() => ()

  clean();
  map(uninstall, *build*.objs);
  uninstall(format-to-string("%s.link", *build*.dylanapp));
  uninstall($dylanmakefile);
  more-targets();

end method;

define method run-target() => ()

  unless (*build*.force?)
    build-app();
  end unless;
  ~*build*.test? & execute-shell-command(#t,
					 *build*.app);
  more-targets();

end method;

define method obj-target() => ()

  obj(next-target());
  more-targets();

end method;

define method res-target() => ()

  res(next-target());
  more-targets();

end method;

define method backup-target() => ()

  backup(*build*.dylanapp);
  more-targets();

end method;

define method backup(library :: <string>) => ()

  let bin-backup = subdirectory-locator($personal-bin, #("backup"));
  let lib-backup = subdirectory-locator($personal-lib, #("backup"));

  ensure-directories-exist(bin-backup);
  ensure-directories-exist(lib-backup);

  let app-loc = override-locator($personal-bin, name: *build*.app);
  if (file-exists?(app-loc))
    echo("Backing up %s into %s\n",
	 *build*.app,
	 bin-backup);
    ~*build*.test? & copy-build-file(app-loc,
				     bin-backup,
				     if-exists: #"replace");
  end if;

  let dll-loc = override-locator($personal-bin, name: *build*.dll);
  if (file-exists?(dll-loc))
    echo("Backing up %s into %s\n",
	 *build*.dll,
	 bin-backup);
    ~*build*.test? & copy-build-file(dll-loc,
				     bin-backup,
				     if-exists: #"replace");
  end if;

  let lib-loc = override-locator($personal-lib, name: *build*.libfile);
  if (file-exists?(lib-loc))
    echo("Backing up %s into %s\n",
	 *build*.libfile,
	 lib-backup);
    ~*build*.test? & copy-build-file(lib-loc,
				     lib-backup,
				     if-exists: #"replace");
  end if;

end method;

define method restore-target() => ()

  restore(*build*.dylanapp);
  more-targets();

end method;

define method restore(library :: <string>) => ()

  let bin-backup = subdirectory-locator($personal-bin, #("backup"));
  let lib-backup = subdirectory-locator($personal-lib, #("backup"));
  let backup-app = override-locator(bin-backup, name: *build*.app);
  let backup-dll = override-locator(bin-backup, name: *build*.dll);
  let backup-lib = override-locator(lib-backup, name: *build*.libfile);

  if (file-exists?(backup-app))
    echo("Restoring %s into %s\n",
	 *build*.app,
	 $personal-bin);
    ~*build*.test? & copy-build-file(backup-app, 
				     override-locator($personal-bin, name: *build*.app),
				     if-exists: #"replace");
  end if;

  if (file-exists?(backup-dll))
    echo("Restoring %s into %s\n",
	 *build*.dll,
	 $personal-bin);
    ~*build*.test? & copy-build-file(backup-dll, 
				     override-locator($personal-bin, name: *build*.dll),
				     if-exists: #"replace");
  end if;

  if (file-exists?(backup-lib))
    echo("Restoring %s into %s\n",
	 *build*.libfile,
	 $personal-lib);
    ~*build*.test? & copy-build-file(backup-lib, 
				     override-locator($personal-lib, name: *build*.libfile),
				     if-exists: #"replace");
  end if;

end method;

define method backup-all-target() => ()

  backup(*build*.app);
  map(backup,	*build*.used-libs.key-sequence);
  more-targets();

end method;

define method restore-all-target() => ()

  restore(*build*.dylanapp);
  map(restore, *build*.used-libs.key-sequence);
  more-targets();

end method;

define method other-targets() => ()

  let target = next-target();
  if (subsequence-position(target, ".obj"))
    obj-target();
  elseif (subsequence-position(target, ".res"))
    res-target();
  else
    illegal-target(target);
  end if;

end method;

define method illegal-target(target :: <string>) => ()

  print-usage();
  build-error("No such target %s", target);

end method;

define method print-usage() => ()

  echo("VALID TARGETS: %=\n",
       *targets*.key-sequence);

end method;

define method help-target() => ()
  format-out("\nDYLAN BUILD SYSTEM"
             "\n"
             "\n  AVAILABLE TARGETS AND THEIR ACTIONS:"
             "\n  -----------------------------------"
             "\n"
             "\n  build                 builds and installs dependent LIBs and DLLs"
             "\n  build install         builds and installs dependent LIBs and DLLs"
             "\n"
             "\n  build dll             builds and installs toplevel DLL"
             "\n"
             "\n  build exe             builds an EXE"
             "\n  build app             builds an EXE"
             "\n  build install-app     builds and installs an EXE"
             "\n  build run             builds and runs an EXE"
             "\n"
             "\n  build clean           Deletes build products"
             "\n  build clean-all       Deletes build and compiler products"
             "\n"
             "\n  build *.obj           Compile C file"
             "\n  build *.res           Compile Resource file"
             "\n"
             "\n  build help            Displays information about build"
             "\n"
             "\n  build force target    forces linking of DLLs and EXEs"
             "\n  build force install   re-install all dependent dlls"
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
             "\n  WHAT YOU MUST DO FIRST:"
             "\n  ----------------------"
             "\n"
             "\n    SET UP YOUR ENVIRONMENT"
             "\n      C:> U:\\dylan\\releases\\kan\\admin\\kan-env -pr C:\\users\\user\\dylan -r U:\\dylan\\releases\\kan"
             "\n"
             "\n    BE IN THE RIGHT DIRECTORY"
             "\n      C:> cd C:\\users\\user\\dylan\\build\\x86-win32\\library"
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
             "\n    C:library> build force dll"
             "\n"
             "\n  TO FORCE A COMPLETE REBUILD OF EVERYTHING AN APPLICATION DEPENDS ON"
             "\n    C:library> build force install app"
             "\n"
             "\n  TO FORCE A COMPLETE REBUILD OF EVERYTHING A DLL DEPENDS ON"
             "\n    C:library> build force install dll"
             "\n"
             "\n  TO RE-BUILD A COLLECTION OF DLLS"
             "\n  (you don't have to change directory to do this)"
             "\n    C:library> build lib1.dll lib2.dll lib3.dll lib4.dll lib5.dll lib6.dll"
             "\n"
             "\n  TO BUILD & RUN AN EXECUTABLE"
             "\n    C:library> build run"
             "\n"
             "\n  TO RUN AN EXECUTABLE"
             "\n  (same as typing executable to shell)"
             "\n    C:library> build force run"
             "\n"
             "\n"
             "\n");
  more-targets();
end method;

define macro &build-target-definer
  { define &build-target ?:name }
    => { 
	let target = as-lowercase(?"name");
	*targets*[target] := ?name ## "-target";
       }
end macro;

define &build-target default;

define &build-target force;

define &build-target test;

define &build-target echo;

define &build-target dll;

define &build-target exe;

define &build-target app;

define &build-target install;

define &build-target install-app;

define &build-target clean;

define &build-target clean-all;

define &build-target run;

define &build-target backup;

define &build-target restore;

define &build-target backup-all;

define &build-target restore-all;

define &build-target help;
