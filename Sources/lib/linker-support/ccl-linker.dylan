Module:    ccl-linker
Synopsis:  Support for "linking" via a C compiler and linker
Author:    Gary Palter
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Set variable to indicate that the ccl-linker DLL has been loaded
$ccl-linker := #"ccl";

define class <ccl-linker> (<linker>)
end class <ccl-linker>;

define sideways method make-linker (linker == #"ccl") => (linker :: <ccl-linker>)
  make(<ccl-linker>, name: "ccl")
end method make-linker;

// define variable *include-path-initialized?* = #f;

define method check-linker-installation (linker :: <ccl-linker>) => ()
  //---*** NOTHING TO DO????
end method check-linker-installation;

define method object-filename-from-base-filename 
    (linker :: <ccl-linker>, file :: <file-locator>)
 => (object-filename :: <file-locator>)
  make(<file-locator>,
       directory: file.locator-directory,
       base:      concatenate(file.locator-base, "_dude"),
       extension: "obj")
end method object-filename-from-base-filename;

define method check-build-dependencies-target 
    (linker :: <ccl-linker>, target :: <file-locator>)
 => (target :: <file-locator>)
  if (target.locator-extension = "dll")
    filename-with-extension(target, "lib")
  else
    target
  end
end method check-build-dependencies-target;

/// Build the LIB file for a library

define method do-link-dll (linker :: <ccl-linker>) => ()
  echo(next-phase(), "Linking LIB %s", *build*.libfile);
  execute-shell-commands(#t, #f,
			 "linkdll",
			 "$(dllname)", *build*.dylanlib,
			 "$(full-lib-name)", *build*.libfile,
			 "$(objects)", concatenate(*build*.c-objs,
						   *build*.c-src-objs,
						   *build*.rc-objs))
end method do-link-dll;

define method do-install-dll (build :: <build>, linker :: <ccl-linker>) => ()
  // Nothing to do as we never actually generate DLLs ...
end method do-install-dll;

define method linkable? (linker :: <ccl-linker>, library :: <symbol>) => (linkable? :: <boolean>)
  #t				//---*** IS THIS RIGHT?
end method;


/// Link together an EXE

define method do-link-exe (linker :: <ccl-linker>) => ()
  // Don't use the base address from the "make" file as it's only for DLLs
  execute-shell-commands(#t, #f,
			 "linkexe",
			 "$(exename)", *build*.dylanlib,
			 "$(full-exe-name)", *build*.app,
			 "$(objects)", concatenate(*build*.c-objs,
						   *build*.c-src-objs,
						   *build*.rc-objs),
			 "$(c-libs)", concatenate(substitute-environment-variables(*build*.c-libs),
						  *build*.rtlibs),
			 "$(linkopts)", substitute-environment-variables(*build*.linkopts),
			 "$(image-version)", *build*.image-version)
end method do-link-exe;


/// DLL unification

define method do-dll-unification (linker :: <ccl-linker>, all?, exe?)
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
  // let winning-build-library :: <symbol> = winning-build.dylanlib;
  let executable :: <file-locator>
    = if (exe?)
	winning-build.app
      else
	winning-build.libfile
      end;
  let unified-base :: <string> = concatenate("unify-", winning-build.dylanapp);
  with-open-file (linkexe = file-in-directory(current-directory,
					      unified-base,
					      extension: "linkexe"),
		  direction: #"output")
    with-open-file (linklib = file-in-directory(current-directory,
						unified-base,
						extension: "linklib"),
		    direction: #"output")
      for (build :: <build> in imported-builds)
	unless (build.system-build? | build.build-up-to-date? | *build*.link?)
	  let library :: <symbol> = build.dylanlib;
	  maybe-install(library, "unify-dll", libraries: libraries);
	end unless;
	format(linkexe, "%s\n", build.libfile);
      end for;
      for (build :: <build> in imported-group-builds.key-sequence)
	unless (build.system-build? | build.build-up-to-date? | *build*.link?)
	  let library :: <symbol> = build.dylanlib;
	  maybe-install(library, "unify-dll", libraries: libraries);
	end unless;
	format(linkexe, "%s\n", build.libfile);
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
				  linkexe :: <stream>,
				  linklib :: <stream>,
				  #key echo? = #t, first?) => ()
	      let library :: <symbol> = build.dylanlib;
	      let system? :: <boolean> = build.system-build?;
	      if (echo?)
		echo(#f, "Unifying %s library %s into %s",
		     if (system?) "system" else "personal" end,
		     library, executable);
	      end if;
	      let directory = build.%build-directory;
	      write-script(build, linkexe, directory);
	      write-script(build, linklib, directory, objects-only?: #t);
	      unless (system?)
		install-dylanmakefile(build, directory: directory);
	      end unless;
	    end method;
      unless (winning-build.build-up-to-date?)
	// ensure this is the winning build
	localize-link(winning-build, linkexe, linklib,
		      echo?: ~ exported-builds.empty?, first?: #t);
	for (build :: <build> in exported-builds)
	  localize-link(build, linkexe, linklib);
	end for;
      end unless;
    end with-open-file;
  end with-open-file;
  winning-build
end method do-dll-unification;

define function write-script
    (build :: <build>, stream :: <stream>, directory :: <directory-locator>, #key objects-only? = #f)
 => ()
  echo?("Write-script %= of %=", build.dylanapp, *build*.dylanapp);
  local method write-lines
	    (files :: <sequence>, directory :: false-or(<directory-locator>))
	  for (file in files)
	    format(stream, "%s\n",
		   if (directory)
		     merge-locators(as(<file-locator>, file), directory)
		   else
		     file
		   end)
	  end for;
	end method;
  write-lines(build.objs, directory);
  write-lines(build.c-objs, directory);
  write-lines(build.c-src-objs, directory);
  write-lines(build.rc-objs, directory);
  unless (objects-only?)
    write-lines(#("_main_dude.obj"), directory);
    write-lines(substitute-environment-variables(build.c-libs), #f);
    write-lines(substitute-environment-variables(build.linkopts), #f);
    // Yuck! Add the run-time libraries here -- There ought to be a cleaner way!
    write-lines(#("run-time.lib", "gc.lib"), #f);
    if (any?(method (x) x = "$(guilflags)" end, build.c-libs))
      write-lines(#("Win32Main.lib"), #f);
    end;
  end;
end function write-script;

define method do-unify-dll (build :: <build>, linker :: <ccl-linker>) => ()
  echo(next-phase(), "Linking LIB %s", build.libfile);
  execute-shell-commands(#t, #f,
			 "unifydll",
			 "$(dllname)", build.dylanapp,
			 "$(full-lib-name)", build.libfile)
end method do-unify-dll;

define method do-unify-exe (build :: <build>, linker :: <ccl-linker>, all?) => ()
  // Don't use the base address from the "make" file as it's only for DLLs
  execute-shell-commands(#t, #f,
			 "unifyexe",
			 "$(exename)", build.dylanapp,
			 "$(full-exe-name)", build.app,
			 "$(image-version)", build.image-version)
end method do-unify-exe;
