Module:    elf-linker
Synopsis:  Support for "linking" via an ELF Linker
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


/// Set variable to indicate that the elf-linker DLL has been loaded
$elf-linker := #"elf";


define constant $elf-exe-link-script = as(<file-locator>, "dylan-elf-exe.script");
define constant $elf-dll-link-script = as(<file-locator>, "dylan-elf-dll.script");


define open class <elf-linker> (<linker>)
end class <elf-linker>;

define sideways method make-linker (linker == #"elf") => (linker :: <elf-linker>)
  $override-default-dll-policy := #"separate-dlls";
  make(<elf-linker>, name: "elf");
end method make-linker;

define method check-linker-installation (linker :: <elf-linker>) => ()
end method check-linker-installation;

define method object-filename-from-base-filename 
    (linker :: <elf-linker>, name :: <file-locator>)
 => (object-filename :: <file-locator>)
  new-filename-extension(name, "o")
end method object-filename-from-base-filename;

define method check-build-dependencies-target (linker :: <elf-linker>, target :: <file-locator>)
 => (target :: <file-locator>)
  target
end method check-build-dependencies-target;

define method linker-library-name (linker :: <elf-linker>, name :: <string>)
 => (library-name :: <string>)
  let name = as-lowercase(name);
  let pos = subsequence-position(name, ".lib");
  if (pos)
    concatenate("-l", copy-sequence(name, end: pos));
  else
    name
  end
end method linker-library-name;

define method exports-file-extension(linker :: <elf-linker>)
 => (extension :: <string>)
  "so"
end method;

define method dll-file-extension(linker :: <elf-linker>)
 => (extension :: <string>)
  "so"
end method;

define method exe-file-extension(linker :: <elf-linker>)
 => (extension :: <string>)
  ""
end method;


define method do-link-dll(linker :: <elf-linker>) => ();
  let library :: <symbol> = *build*.dylanlib;
  let dylan-library? = library.dylan-library?;

  echo(next-phase(), "Linking shared-object %s", *build*.dll);

  execute-shell-commands(#t, #f,
			 "linkdll",
			 "$(dllname)", library-file-name(library),
			 "$(script)", shorten-pathname(search-for-file($elf-dll-link-script)),
			 "$(full-dll-name)", *build*.dll,
			 "$(dlllibs)", 
			 map(method (library)
			       search-name(library)
			     end method,
			     *build*.libs.key-sequence),
			 "$(dllobjs)",
			 concatenate(*build*.objs,
				     *build*.c-objs,
				     *build*.c-src-objs),
			 "$(dylan-support)",
			 if (dylan-library?)
			   let runtime-directory = search-for-runtime-directory();
			   concatenate(runtime-directory, "*.o ",
						   substitute-environment-variables("$(libgc)"),
						   " -lpthread -lc")
			 else
			   ""; // dylan-support is placed directly in the glue file
			 end,
			 "$(c-libs)",
			 concatenate(substitute-environment-variables(*build*.c-libs),
				     *build*.rtlibs),
			 "$(base)", *build*.base-address,
			 "$(linkopts)", substitute-environment-variables(*build*.linkopts),
			 "$(image-version)", *build*.image-version,
			 "$(personal-lib)", $personal-lib | *lib-installation*,
			 "$(system-lib)", $system-lib,
			 "$(libraries)", build-environment-variable("libraries"),
			 "$(libcmt)", build-environment-variable("libcmt"));

end method;


define method installation-name (name :: <pathname>) => (name :: <string>)
  concatenate("lib", as-lowercase(as(<string>, name)))
end method;

define method search-name (name :: <symbol>) => (name :: <string>)
  concatenate("-l", library-file-name(name))
end method;


/// Install libraries, which are also executables, in the lib directory rather
/// than the bin directory.  Note that applications are installed in the bin
/// directory but have their RPATH set to point to the lib directory.
define method do-install-dll (build :: <build>, linker :: <elf-linker>) => ()
  let fullname = installation-name(build.dll);
  let destination = file-in-directory(*lib-installation*, fullname);
  if (newer-file?(build.dll, destination, error?: #f))
    note-library-modified();
    echo(next-phase(), "Installing %s in %s", fullname, *lib-installation*);
    copy-build-file(build.dll, destination, if-exists: #"replace");
    unless ((build == *build*) | build.system-build?)
      copy-build-file(build.dll,
		      file-in-directory(build.build-directory, build.dll),
		      if-exists: #"replace");
      uninstall(build.dll);
    end unless;
  end if;
end method do-install-dll;


/// Link together an EXE

define method do-link-exe(linker :: <elf-linker>) => ();

  do-link-dll(linker);
  do-install-dll(*build*, linker);

  let library :: <symbol> = *build*.dylanlib;
  let dylan-library? = library.dylan-library?;
  let mangled-library :: <string> = linker-mangle(library-file-name(library));
  let entry-point :: <string> = concatenate(mangled-library, "Exe0");

  execute-shell-commands(#t, #f,
			 "linkexe",
			 "$(exename)", library-file-name(library),
			 "$(script)", shorten-pathname(search-for-file($elf-exe-link-script)),
			 "$(full-exe-name)", *build*.app,
			 "$(entry-point)", entry-point,
			 "$(dlllibs)", 
			 search-name(library),
			 "$(dllobjs)",
			 concatenate(#("_main.o"),
				     *build*.c-objs,
				     *build*.c-src-objs),
			 "$(dylan-support)",
			 if (dylan-library?)
			   let runtime-directory = search-for-runtime-directory();
			   concatenate(runtime-directory, "*.o ",
						   substitute-environment-variables("$(libgc)"),
						   " -lpthread -lc")
			 else
			   ""; // dylan-support is placed directly in the glue file
			 end,
			 "$(c-libs)",
			 concatenate(substitute-environment-variables(*build*.c-libs),
				     *build*.rtlibs),
			 "$(linkopts)", substitute-environment-variables(*build*.linkopts),
			 "$(image-version)", *build*.image-version,
			 "$(personal-lib)", $personal-lib | *lib-installation*,
			 "$(system-lib)", $system-lib,
			 "$(libraries)", build-environment-variable("libraries"),
			 "$(libcmt)", build-environment-variable("libcmt"),
			 "$(rpath)", rpath-value(*build*, linker));
end method;


define method do-unify-dll (build :: <build>, linker :: <elf-linker>) => ()
  error("ELF shared-library unification not yet implemented");
end method do-unify-dll;

define method do-unify-exe (build :: <build>, linker :: <elf-linker>, all?) => ()
  error("ELF shared-library unification not yet implemented");
end method do-unify-exe;


define method linker-executable-keyword(linker :: <elf-linker>)
 => (executable-keyword :: <symbol>)
  #"library"
end method;


define method rpath-value(build :: <build>, linker :: <elf-linker>)
  let override = environment-variable("FUNCTIONAL_DEVELOPER_RPATH");
  override
    | format-to-string("%s:%s", $personal-lib | *lib-installation*, $system-lib)
end method rpath-value;

define method search-for-runtime-directory(#key error? = #t)
 => (directory :: false-or(<string>))
  let directory = "runtime";
  let file = "runtime.o";
  let personal-dir = subdirectory-locator($personal-lib, directory);
  let personal-file = file-in-directory(personal-dir, file);
  let system-dir = subdirectory-locator($system-lib, directory);
  let system-file = file-in-directory(system-dir, file);

  case
    file-exists?(personal-file) => as(<string>, personal-dir);
    file-exists?(system-file) => as(<string>, system-dir);
    otherwise =>
      if (error?)
	build-error("Required file %s does not appear in any Installation Area"
		      "Looked in:  %s  %s",
		    file, 
		    personal-dir,
		    system-dir);
      end if;
  end case;
end method;

define method c-compile(linker :: <elf-linker>, file :: <file-locator>) => ()

  echo(next-phase(), "Compiling C file %s", file);
  execute-shell-command(#t, #f,
			build-environment-variable("ccompile"),
			file);

end method;

define method linker-library-path-name(linker :: <elf-linker>)
 => (library-path :: <string>)
  "LD_LIBRARY_PATH";
end method;

define method linker-path-separator(linker :: <elf-linker>)
 => (separator :: <string>)
  ":";
end method;

// There is a separate main object for each library; only the toplevel library's
// copy is included in the executable link.

define method build-main-object?(linker :: <elf-linker>) => (main? :: <boolean>)
  #t
end method;
