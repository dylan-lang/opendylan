Module:    powerpc-elf-linker
Synopsis:  Support for "linking" via an ELF Linker with static linking and library unification
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Set variable to indicate that the PPC elf-linker DLL has been loaded
$elf-linker := #"ppc-elf";


// Specialize the ELF Linker with static linking and library unification
// for the PowerPC.

define class <powerpc-elf-linker> (<elf-linker>)
end class <powerpc-elf-linker>;

define sideways method make-linker (linker == #"ppc-elf") => (linker :: <powerpc-elf-linker>)
  make(<powerpc-elf-linker>, name: "ppc-elf");
end method make-linker;

// Not applicable for static linking.

define method exports-file-extension(linker :: <powerpc-elf-linker>)
 => (extension :: <string>)
  ""
end method;

define method dll-file-extension(linker :: <powerpc-elf-linker>)
 => (extension :: <string>)
  ""
end method;

define method do-link-dll(linker :: <powerpc-elf-linker>) => ();
  error("do-link-dll");
end method;

define method do-install-dll (build :: <build>, linker :: <powerpc-elf-linker>) => ()
end method do-install-dll;


/// Link together an EXE

define method do-link-exe(linker :: <powerpc-elf-linker>) => ()
  error("do-link-exe");
end method;


define method do-unify-dll (build :: <build>, linker :: <powerpc-elf-linker>) => ()
  error("ELF shared-library unification not yet implemented");
end method do-unify-dll;

// For now, because of Boehm Collector problems with our shared libraries on PowerPC
// statically link all objects into a single executable image.

define method do-unify-exe (build :: <build>, linker :: <powerpc-elf-linker>, all?) => ()
  let library :: <string> = *build*.dylanlib;

  execute-shell-commands(#t, #f,
			 "unifyexe",
			 "$(full-exe-name)", *build*.app,
			 "$(objects)", *link-stream*,
			 "$(dylan-support)",
			 begin
			   let runtime-directory = as(<string>, search-for-runtime-directory());
			   concatenate(runtime-directory, "*.o -lgc")
			 end,
			 "$(image-version)", *build*.image-version,
			 "$(personal-lib)", $personal-lib,
			 "$(system-lib)", $system-lib,
			 "$(libraries)", build-environment-variable("libraries"));
  *link-stream* := #();
end method do-unify-exe;

define method static-linking?(linker :: <powerpc-elf-linker>) => (static? :: <boolean>)
  #t
end method;

// All objects and libraries will be accumulated on the command-line.
// This is Linux -- no problems with command-line limits (yet).

define method link-stream-class(linker :: <powerpc-elf-linker>) => (class :: <class>)
  <list>
end method;

define method link-runtime-libraries(stream, linker :: <powerpc-elf-linker>) => (stream)
  stream
end method;

// There is a separate main object for each library; only the toplevel library's
// copy is included in the unified link.

define method build-main-object?(linker :: <powerpc-elf-linker>) => (main? :: <boolean>)
  #t
end method;
