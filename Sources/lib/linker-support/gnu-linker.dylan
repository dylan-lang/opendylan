Module:    gnu-linker
Synopsis:  GNU Linker Support for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Trivial utility ...
define inline-only function short-path (path :: <pathname>) => (path :: <string>)
  as(<string>, shorten-pathname(path))
end function short-path;


// Set variable to indicate that the gnu-linker DLL has been loaded

$gnu-linker := #"gnu";

define class <gnu-linker>(<dynamic-linker>)
end class;

// Link together a DLL using the gnu-linker

define method do-link-dll(linker :: <gnu-linker>) => ()
  let library :: <symbol> = *build*.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let import-fixups = library-filename-with-extensions(library, "imp0", "o");
  let dylan-library? = library.dylan-library?;

  unless (dylan-library?) write-build-import-fixups(*build*) end;

  if (unlinkable?(library))
    echo(#f, "WARNING: Cannot build dll for %= -- generating exports only", library);
    read-exports()
  elseif (*build*.exports-only?)
    read-exports()
  else

  echo(next-phase(), "Linking DLL %s", *build*.dll);

  read-imports();
  let linker-exports = read-exports();

  write-imports();
  write-exports(*build*, linker-exports);

  let objects = assemble-library(*build*);

  execute-shell-commands(#t, #f,
			 "linkdll",
			 "$(dllname)", library-file-name(library),
			 "$(mangled-dllname)", mangled-library,
			 "$(script)", short-path(search-for-file(as(<file-locator>, $link-script))),
			 "$(full-dll-name)", *build*.dll,
			 "$(dylan-imp7)",
			 new-filename-extension
			   ($dylan-imports-asm-file, "o"),
			 "$(dlllibs)", 
			 map(method (executable :: <string>)
			       if (executable.runtime-library?) ""
			       else
				 filename-with-extensions(executable, "imp", "o")
			       end if
			     end method,
			     *linker*.imports.key-sequence),
			 "$(dllobjs)",
			 concatenate(*build*.objs,
				     if (dylan-library?) #()
				     else list(import-fixups) end,
				     *build*.c-src-objs,
				     map(gnu-resource-object-file, *build*.rc-objs)),
			 "$(dylan-support)", short-path(search-for-file(as(<file-locator>, $dylan-support))),
			 "$(c-libs)", 
			 substitute-environment-variables(*build*.c-libs),
			 "$(gnulibs)",
			 as(<string>,
			    file-in-directory(gnu-installation(), "lib")),
			 "$(linkopts)",
			 substitute-environment-variables(*build*.linkopts),
			 "$(base)", *build*.base-address
			   );

  // clean up after the job is done
  uninstall(library-filename-with-extension(library, "base"));
  uninstall(library-filename-with-extension(library, "reloc"));
  uninstall(library-filename-with-extensions(library, "exp", "s"));
  map(uninstall, objects);

  end if;

end method;

// Link together a EXE using the gnu-linker

define method do-link-exe(linker :: <gnu-linker>) => ()
  let library :: <symbol> = *build*.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let import-fixups = library-filename-with-extensions(library, "imp0", "o");
  let entry-point :: <string> = concatenate(mangled-library, "Exe");
  let dylan-library? = library.dylan-library?;

  unless (dylan-library?) write-build-import-fixups(*build*) end;

  if (unlinkable?(library) | *build*.exports-only?)
    read-exports()
  else

  read-imports();
  let linker-exports = read-exports();

  write-imports();
  write-exports(*build*, linker-exports);

  let objects = assemble-library(*build*);

  execute-shell-commands(#t, #f,
			 "linkexe",
			 "$(exename)", library-file-name(library),
			 "$(entry-point)", entry-point,
			 "$(script)", short-path(search-for-file(as(<file-locator>, $link-script))),
			 "$(full-exe-name)", *build*.app,
			 "$(dylan-imp7)",
			 new-filename-extension
			   ($dylan-imports-asm-file, "o"),
			 "$(exelibs)", 
			 map(method (executable :: <string>)
			       if (executable.runtime-library?) ""
			       else
				 filename-with-extensions(executable, "imp", "o")
			       end if
			     end method,
			     *linker*.imports.key-sequence),
			 "$(exeobjs)",
			 concatenate(*build*.objs,
				     if (dylan-library?) #()
				     else list(import-fixups) end,
				     *build*.c-src-objs,
				     map(gnu-resource-object-file, *build*.rc-objs)),
			 "$(dylan-support)", short-path(search-for-file(as(<file-locator>, $dylan-support))),
			 "$(c-libs)", 
			 substitute-environment-variables(*build*.c-libs),
			 "$(gnulibs)",
			 file-in-directory(gnu-installation(), "lib"),
			 "$(subsystem)", "console",
			 "$(linkopts)",
			 substitute-environment-variables(*build*.linkopts),
			 "$(base)", *build*.base-address
			   );

  uninstall(library-filename-with-extensions(library, "exp", "s"));
  map(uninstall, objects);

  end if;
end method;

define thread variable *linker-options* :: <sequence> = #();

define method do-dll-unification(linker :: <gnu-linker>, all?, exe?)
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
  let linker-options :: <string-table> = make(<string-table>);
  let winning-build-library = winning-build.dylanlib;
  let exports-only? :: <boolean> = *build*.exports-only?;
  let executable :: <file-locator>
    = if (exe?)
	winning-build.app
      else 
	if (exports-only?)
	  winning-build.libfile
	else
	  winning-build.dll
	end;
      end;

  unless (unlinkable?(winning-build-library))

    *linker-options* := #();

    unless (*build*.link?)

      for (build :: <build> in imported-builds)
	unless (build.system-build? | build.build-up-to-date?)
	  let library :: <symbol> = build.dylanlib;
	  maybe-install(library, "unify-dll", libraries: libraries);
	end unless;
      end for;

      for (build :: <build> in imported-group-builds.key-sequence)
	unless (build.system-build? | build.build-up-to-date?)
	  let library :: <symbol> = build.dylanlib;
	  maybe-install(library, "unify-dll", libraries: libraries);
	end unless;
      end for;

      for (build :: false-or(<build>) in imported-group-builds)
	if (build & ~build.build-up-to-date?)
	  let library :: <symbol> = build.dylanlib;
	  echo?("Spotted shadow of system project %s", library);
	  maybe-install(library, "unify-dll", libraries: libraries);
	end if;
      end for;

    end unless;

    let glue-imports = make(<string-table>);

    local method localize-link(build :: <build>,
			       fixups :: <pair>,
			       #key echo? = #t, first?) => ()
	    let directory :: <directory-locator> = build.%build-directory;
	    let library :: <symbol> = build.dylanlib;
	    let system? :: <boolean> = build.system-build?;
	    if (echo?)
	      echo(#f, "Unifying %s library %s into %s",
		   if (system?) "system" else "personal" end,
		   library, executable);
	    end if;
	    unless (exports-only?)

	      with-build-directory (directory)

		let archive :: <file-locator> = filename-with-extension(build.dylanapp, "a");
		let archive-dest :: <file-locator> = file-in-directory(current-directory, archive);
		uninstall(archive);
		for (option in build.c-libs)
		  linker-options[option] := #t;
		end for;
		for (option in build.linkopts)
		  linker-options[option] := #t;
		end for;
		execute-shell-commands(#t, #f,
				       "make-archive",
				       "$(archive)", archive,
				       "$(objects)",
				       concatenate(build.objs,
						   build.c-objs,
						   build.c-src-objs,
						   map(gnu-resource-object-file, build.rc-objs)));
		unless (build == *build*)
		  rename-file(archive, archive-dest, if-exists: #"replace");
		end unless;

	      end with-build-directory;

	      read-build-imports(build, glue-imports,
				 directory: directory,
				 imported-libraries: imported-libraries);
	      unless (library.dylan-library?)
		write-import-fixups(fixups.head, fixups.tail, directory, first?);
	      end;
	      unless (system?)
		install-dylanmakefile(build, directory: directory);
	      end unless;

	    end unless;

	    read-build-exports(build, directory: directory);
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

      let linker-exports = finalize-exports(winning-build);

      unless (exports-only?)
	finalize-imports(winning-build, glue-imports);
	write-imports();
	write-fake-imports(winning-build);
	write-exports(winning-build, linker-exports);
      end unless;

      *linker-options* := linker-options.key-sequence;

    end unless;
  end unless;

  winning-build

end method;


define method do-unify-dll(build :: <build>, linker :: <gnu-linker>) => ()
  let library :: <symbol> = build.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));

  if (unlinkable?(library))
    echo(#f, "WARNING: Cannot build dll for %= -- generating exports only", library);
    read-exports()
  elseif (*build*.exports-only?)
    #f
  else

  echo(next-phase(), "Linking DLL %s", build.dll);

  let archive = filename-with-extension(build.dylanapp, "a");
  let imports-archive = filename-with-extensions(build.dylanapp, "imp", "a");
  uninstall(imports-archive);
  let objects = assemble-library(build, archive: imports-archive);
  let archives =
    map(method (library :: <symbol>)
	  library-filename-with-extension(library, "a")
	end method,
	*linker*.fake-imports.key-sequence);

  execute-shell-commands(#t, #f,
			 "linkdll",
			 "$(dllname)", library-file-name(library),
			 "$(mangled-dllname)", mangled-library,
			 "$(script)", short-path(search-for-file(as(<file-locator>, $link-script))),
			 "$(full-dll-name)", build.dll,
			 "$(dylan-imp7)",
			 new-filename-extension
			   ($dylan-imports-asm-file, "o"),
			 "$(dlllibs)", "",
			 "$(dllobjs)", 
			 make-command-line("--whole-archive",
					   imports-archive, archive, archives,
					   "--no-whole-archive"),
			 "$(dylan-support)", short-path(search-for-file(as(<file-locator>, $dylan-support))),
			 "$(c-libs)", "",
			 "$(gnulibs)",
			 file-in-directory(gnu-installation(), "lib"),
			 "$(linkopts)",
			 substitute-environment-variables(*linker-options*),
			 "$(base)", build.base-address
			   );

  // clean up after the job is done
  uninstall(library-filename-with-extension(library, "base"));
  uninstall(library-filename-with-extension(library, "reloc"));
  uninstall(library-filename-with-extensions(library, "exp", "s"));
  map(uninstall, objects);
  uninstall(imports-archive);
  uninstall(archive);
  map(uninstall, archives);

  end if;
end method;

define method do-unify-exe(build :: <build>, linker :: <gnu-linker>, all?) => ()
  let library :: <symbol> = build.dylanlib;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let entry-point :: <string> =
    concatenate(mangled-library, if (#f) "Exe0" else "Exe" end);

  let archive = filename-with-extension(build.dylanapp, "a");
  let imports-archive = filename-with-extensions(build.dylanapp, "imp", "a");
  uninstall(imports-archive);
  let objects = assemble-library(build, archive: imports-archive);
  let archives =
    map(method (library :: <symbol>)
	  library-filename-with-extension(library, "a")
	end method,
	*linker*.fake-imports.key-sequence);

  execute-shell-commands(#t, #f,
			 "linkexe",
			 "$(exename)", library-file-name(library),
			 "$(entry-point)", entry-point,
			 "$(script)", short-path(search-for-file(as(<file-locator>, $link-script))),
			 "$(full-exe-name)", build.app,
			 "$(dylan-imp7)",
			 new-filename-extension
			   ($dylan-imports-asm-file, "o"),
			 "$(exelibs)", "",
			 "$(exeobjs)", 
			 make-command-line("--whole-archive",
					   imports-archive, archive, archives,
					   "--no-whole-archive"),
			 "$(dylan-support)", short-path(search-for-file(as(<file-locator>, $dylan-support))),
			 "$(c-libs)", "",
			 "$(gnulibs)",
			 file-in-directory(gnu-installation(), "lib"),
			 "$(subsystem)", "console",
			 "$(linkopts)", substitute-environment-variables(*linker-options*),
			 "$(base)", build.base-address
			   );

  uninstall(library-filename-with-extensions(library, "exp", "s"));
  map(uninstall, objects);
  uninstall(imports-archive);
  uninstall(archive);
  map(uninstall, archives);

end method;


define method create-archive(linker :: <gnu-linker>) => ()
  shift-targets();
  let target = next-target();
  let archive :: <file-locator>
    = filename-with-extension(concatenate("lib", target), "a");
  echo(next-phase(), "Creating Archive %s", archive);
  let import-file = filename-with-extension(target, "import");
  let glue-imports = make(<string-table>);

  read-imports-in-file(import-file,
		       glue-imports?: glue-imports);
  write-glue-imports(target, glue-imports.key-sequence);
  write-imports-for-executable(target, all?: #t);
  let glue-object :: <string> =
    assemble-file(filename-with-extensions(target, "imp7", "s"));
  let object :: <string> =
    assemble-file(filename-with-extensions(target, "imp", "s"));
  uninstall(archive);
  execute-shell-commands(#t, #f,
			 "make-archive",
			 "$(archive)", archive,
			 "$(objects)", list(glue-object, object));
  uninstall(glue-object);
  uninstall(object);
end method;

// Inherit global-export caches to boost performance

define sideways method make-linker(linker == #"gnu") => (linker :: <gnu-linker>)
  make(<gnu-linker>,
       name: "gnu",
       global-exports: if (instance?(*linker*, <gnu-linker>)) *linker*.global-exports
		       else make(<string-table>)
		       end if)
end method;

define method exports-file-extension(linker :: <gnu-linker>)
 => (extension :: <string>)
  "defs"
end method;

define method linker-library-name(linker :: <gnu-linker>, name :: <string>)
 => (library-name :: <string>)
  let name = as-lowercase(name);
  let pos = subsequence-position(name, ".lib");
  if (pos)
    concatenate("-l", copy-sequence(name, end: pos));
  else
    name
  end if;
end method;


// Hack -- Put all currently gnu-unlinkable DLLs here

define method unlinkable?(library :: <symbol>) => (unlinkable? :: <boolean>)
  dylan-library?(library)
end method;

define method linkable?(linker :: <gnu-linker>, library :: <symbol>) => (linkable? :: <boolean>)
  ~unlinkable?(library)
end method;


// Create a .defs exports file from a statically linked library
// This currently requires DOS Shell

define method create-exports-file
    (linker :: <gnu-linker>, exports-file :: <file-locator>, library-file :: <file-locator>) => ()
  execute-shell-commands(#t, #t,
			 "make-exports",
			 "$(source)", library-file,
			 "$(destination)", exports-file);
end method;

// Create a .import imports file from a .obj
// This currently requires DOS Shell

define method create-imports-file
    (linker :: <gnu-linker>, imports-file :: <file-locator>, 
     source-file :: <file-locator>) => ()
  execute-shell-commands(#t, #t,
			 "make-imports",
			 "$(source)", source-file,
			 "$(destination)", imports-file,
			 // This is not an import so exclude it
			 "$(entry-point)", $dylan-entry-point);
end method;

define variable *linker-installed?* = #f;

// Ensure that all the prerequisites for linking in one's installation
// have been met

define method check-linker-installation(linker :: <gnu-linker>) => ()
  unless (*linker-installed?*)

    // Set up the PATH appropriately for the life span of gnu-linker
    // This puts the constraint on users to install the GNU toolset in
    // the standard "\usr\local" if they don't wish to use the GNU set
    // packaged by Functional Objects

    environment-variable("PATH") :=
      concatenate
        (as(<string>, file-in-directory(gnu-installation(), "bin")),
	 ";",
	 environment-variable("PATH"));

    create-imports-file-for-source
      (filename-with-extension(as(<file-locator>, $dylan-support-imports), #f));

    *linker-installed?* := #t
  end unless;
end method;

// Find a GNU Installation by searching in the hard-wired paths
//   %dylan_release_root%
//   %GNUROOT%
//   \usr\local

define variable *gnu-installation* = #f;

define method gnu-installation () => (installation :: <directory-locator>)
  if (*gnu-installation*)
    *gnu-installation*
  else
    let short-release-dir = shorten-pathname($release-root);
    let release-dir = subdirectory-locator(short-release-dir, "tools", "i386-cygwin32");
    let gnuroot-dir = 
      begin
	let ev = environment-variable("GNUROOT");
	let evp = ev & as(<directory-locator>, ev);
	evp & subdirectory-locator(evp, "i386-cygwin32")
      end;
    let personal-dir = as(<microsoft-directory-locator>, "c:/usr/local/i386-cygwin32");
    *gnu-installation* :=
      case
	file-exists?(release-dir) => release-dir;
	gnuroot-dir & file-exists?(gnuroot-dir) => shorten-pathname(gnuroot-dir);
	file-exists?(personal-dir) => personal-dir;
	otherwise =>
	  build-error("Cannot find the GNU Installation:  "
		      "Looked in:  %s;%s;%s",
		      release-dir,
		      gnuroot-dir | "",
		      personal-dir);
      end case;
  end if;
end method;

define method linker-resource-object-file
    (linker :: <gnu-linker>, resource-file :: <file-locator>)
 => (object-file :: <file-locator>)
  make(<file-locator>,
       directory: resource-file.locator-directory,
       base:      concatenate(resource-file.locator-base, "-resource"),
       extension: "obj")
end method;

define inline function gnu-resource-object-file
    (resource-file :: <file-locator>) => (object-file :: <file-locator>)
  linker-resource-object-file(*linker*, resource-file);
end function;
