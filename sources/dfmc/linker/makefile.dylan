Module: dfmc-linker
Author: keith and jonathan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Platform-specific makefile configuration.

// We need to generate different makefiles for Unix and Windows since
// they're incompatible for various reasons. In particular, we want
// to use !include with nmake, and there's not always a way to get
// that ignored in Unix makes.

define abstract compiler-open class <makefile-target> (<object>) end;

define compiler-open generic source-suffix
  (back-end :: <back-end>, t :: <makefile-target>);
define compiler-open generic object-suffix
  (back-end :: <back-end>, t :: <makefile-target>);
define compiler-open generic resource-suffix
  (back-end :: <back-end>, t :: <makefile-target>);

define compiler-open generic output-basename
  (back-end :: <back-end>, target :: <makefile-target>, basename :: <string>) => (basename);

define compiler-open generic emit-target-makefile
  (back-end :: <back-end>, t :: <makefile-target>,
   makefile-name, library-description, units,
   #key executable, base-address, linker-options, c-source-files,
	c-header-files, c-object-files, rc-files, c-libraries);

define method makefile-unit-name (t :: <makefile-target>)
  "Makefile"
end method;

define method makefile-target (desc :: <library-description>)
  makefile-target-using-os-name(library-description-os-name(desc))
end method;


define method source-unit (t :: <makefile-target>, unit)
  let b = current-back-end();
  concatenate(output-basename(b, t, unit), ".", source-suffix(b, t));
end method;

define method object-unit (t :: <makefile-target>, unit)
  let b = current-back-end();
  platform-specific-concatenate
    (t, output-basename(b, t, unit), ".", object-suffix(b, t));
end method;

define method object-unit-name (t :: <makefile-target>, unit)
  let b = current-back-end();
  concatenate(output-basename(b, t, unit), ".", object-suffix(b, t));
end method;

define compiler-open generic additional-platform-libraries (t, for-app?);
define compiler-open generic library-unit-name (t, unit);
define compiler-open generic library-unit-link-ref (t, ld, unit);
define compiler-open generic runtime-unit-link-ref (t, unit);

define compiler-open generic makefile-target-using-os-name (name);
define compiler-open generic makefile-variable-ref (t, name);
define compiler-open generic makefile-linker-option-massage (t, name);
define compiler-open generic makefile-rule-separator (t);
define compiler-open generic makefile-pathname (t, #rest components);
define compiler-open generic makefile-target-pathname-ref (t);

define method makefile-rule-separator (t)
  ": "
end method makefile-rule-separator;

define method makefile-variable-ref (t, name)
  concatenate("$(", name, ")")
end method makefile-variable-ref;

define method makefile-linker-option-massage (t, name)
  name
end method makefile-linker-option-massage;

define method makefile-target-pathname-ref (t)
  "$@"
end method makefile-target-pathname-ref;

define function makefile-command(t, command, #rest arguments)
  let expansion
    = with-output-to-string (line)
	let variable = make(limited(<stretchy-vector>, of: <byte-character>));
	let inref? = #f;
	for (c in command)
	  select (c)
	    '{' => 
	      variable.size := 0;
	      inref? := #t;
	    '}' =>
	      write(line, makefile-variable-ref(t, variable));
	      inref? := #f;
	    otherwise =>
	      if (inref?)
		add!(variable, c)
	      else
		write-element(line, c)
	      end;
	  end
	end
      end;
  apply(format-to-string, expansion, arguments)
end function makefile-command;

define function personal-library? (lib :: <library-description>)
  library-description-personal?(lib)
end function;

define method runtime-unit-link-ref (t, unit)
  library-unit-link-ref(t, #f, unit)
end method runtime-unit-link-ref;

define method library-unit-directory (t, lib :: false-or(<library-description>))
  if (lib & personal-library?(lib))
    makefile-variable-ref(t, "OPEN_DYLAN_USER_LIB")
  else
    makefile-variable-ref(t, "OPEN_DYLAN_RELEASE_LIB")
  end if;
end method;

define method library-unit-ref (t, lib, unit)
  makefile-pathname(t,
		    library-unit-directory(t, lib), 
		    library-unit-name(t, unit));
end method;

define method executable-unit (t, unit)
  platform-specific-concatenate(t, unit, ".", executable-suffix(t));
end method;

define method executable-unit-ref (t, unit)
  makefile-pathname(t,
		    makefile-variable-ref(t, "OPEN_DYLAN_USER_BIN"),
		    executable-unit(t, unit));
end method;

define method link-target-separator (t) " " end;

define method makefile-program-setups (t :: <makefile-target>) #() end;


/// UNIX (GNU or vendor make) Makefile specifics ...

define class <unix-makefile-target> (<makefile-target>) end;

define method additional-platform-libraries 
    (t :: <unix-makefile-target>, for-app? :: <boolean>)
  #("-lm")
end method;

define method library-unit-name (t :: <unix-makefile-target>, unit)
  concatenate("lib_", unit, ".", library-suffix(t));
end method;

define method library-unit-link-ref (t :: <unix-makefile-target>, lib, unit)
  library-unit-ref(t, lib, unit)
  // concatenate("-l_", unit);
end method;

define method makefile-pathname (t :: <unix-makefile-target>, #rest components)
  let almost = reduce(rcurry(concatenate, "/"), "", components);
  copy-sequence(almost, end: size(almost) - 1)
end method makefile-pathname;

define method library-suffix (t :: <unix-makefile-target>) 
  "$(OPEN_DYLAN_LIB_SUFFIX)"
end method;

define method makefile-program-setups (t :: <unix-makefile-target>) 
  list("OPEN_DYLAN_RELEASE_BIN=$(OPEN_DYLAN_RELEASE_INSTALL)/bin",
       "OPEN_DYLAN_RELEASE_INCLUDE=$(OPEN_DYLAN_RELEASE_INSTALL)/include",
       "OPEN_DYLAN_RELEASE_LIB=$(OPEN_DYLAN_RELEASE_INSTALL)/lib",
       "OPEN_DYLAN_USER_BIN=$(OPEN_DYLAN_USER_INSTALL)/bin",
       "OPEN_DYLAN_USER_INCLUDE=$(OPEN_DYLAN_USER_INSTALL)/include",
       "OPEN_DYLAN_USER_LIB=$(OPEN_DYLAN_USER_INSTALL)/lib",
       "",
       "CC = gcc",
       "MKDIR = mkdir",
       "CREATE = touch",
       "INSTALLOBJ = @ exit 0; ",
       "INSTALLLIB = cp -p",
       "UNINSTALL = rm -f",
       "LINKAPP = gcc $(OPEN_DYLAN_LINKAPP_FLAGS) -L$(OPEN_DYLAN_USER_LIB) -L$(OPEN_DYLAN_RELEASE_LIB) -o",
       "LINKLIB = $(OPEN_DYLAN_LD) $(OPEN_DYLAN_LINKLIB_FLAGS) -L$(OPEN_DYLAN_USER_LIB) -L$(OPEN_DYLAN_RELEASE_LIB) -o",
       "CFLAGS = $(OPEN_DYLAN_C_FLAGS) -I$(OPEN_DYLAN_RELEASE_INCLUDE)");
end method;

define method object-unit-name (t :: <unix-makefile-target>, unit)
  object-unit(t, unit);
end method;

define method executable-unit (t :: <unix-makefile-target>, unit)
  platform-specific(t, unit);
end method;

define method make-install-command 
    (target :: <unix-makefile-target>, name) => (makefile-command :: <string>)
  format-to-string("(cd ../%s ; $(MAKE) install)", name);
end method;

define method relative-makefile-unit-ref
    (target :: <unix-makefile-target>, name) => (makefile-unit :: <string>)
  format-to-string("../%s/Makefile", name);
end method;

define method compiler-args-for-source (t :: <unix-makefile-target>, name)
  format-to-string("-c %s", name)
end method;

define method compiler-args-for-object (t :: <unix-makefile-target>, name)
  format-to-string("-o %s", name)
end method;

define constant *unix-makefile-target* = make(<unix-makefile-target>);

define method makefile-target-using-os-name (name)
  *unix-makefile-target*
end method;


/// Win32 (nmake) Makefile specifics ...

define class <win32-makefile-target> (<makefile-target>) end;

define method additional-platform-libraries
    (t :: <win32-makefile-target>, for-app? :: <boolean>)
  #()
end method;

define method library-unit-name (t :: <win32-makefile-target>, unit)
  concatenate(unit, ".", library-suffix(t));
end method;

/// Uses the LIB search path to find the library ...
define method library-unit-link-ref (t :: <win32-makefile-target>, lib, unit)
  library-unit-name(t, unit)
end method;

define method makefile-pathname (t :: <win32-makefile-target>, #rest components)
  let almost = reduce(rcurry(concatenate, "\\"), "", components);
  copy-sequence(almost, end: size(almost) - 1)
end method makefile-pathname;

define method library-suffix (t :: <win32-makefile-target>) "lib" end;
define method executable-suffix (t :: <win32-makefile-target>) "exe" end;

define method link-target-separator (t :: <win32-makefile-target>) "" end;

define method makefile-program-setups (t :: <win32-makefile-target>) 
  list("OPEN_DYLAN_RELEASE_BIN=$(OPEN_DYLAN_RELEASE_INSTALL)\\bin",
       "OPEN_DYLAN_RELEASE_INCLUDE=$(OPEN_DYLAN_RELEASE_INSTALL)\\include",
       "OPEN_DYLAN_RELEASE_LIB=$(OPEN_DYLAN_RELEASE_INSTALL)\\lib",
       "OPEN_DYLAN_USER_BIN=$(OPEN_DYLAN_USER_INSTALL)\\bin",
       "OPEN_DYLAN_USER_INCLUDE=$(OPEN_DYLAN_USER_INSTALL)\\include",
       "OPEN_DYLAN_USER_LIB=$(OPEN_DYLAN_USER_INSTALL)\\lib",
       "",
       "APPVER = 4.0",
       "TARGETOS = BOTH",
       "",
       "!include <win32.mak>",
       "",
       "INSTALLOBJ = rem",
       "INSTALLLIB = copy",
       "UNINSTALL = - del /q /f",
       "MKDIR = mkdir",
       "CREATE = touch",
       "",
       "LIB = $(OPEN_DYLAN_USER_LIB);$(OPEN_DYLAN_RELEASE_LIB);$(LIB)",
       "LINKLIB = $(implib) /nologo /out:",
       "LINKAPP = $(link) $(conlflags) $(ldebug) $(conlibsmt) /nologo /out:",
       "",
       // W2 suppresses the warnings about unused labels and local variables.
       // It will, however, generate a warning for each file that /W2 overrides /W3.  (Sigh)
       "CFLAGS = $(cflags) $(cvarsmt) $(cdebug) /W2 /I$(OPEN_DYLAN_RELEASE_INCLUDE) $(OPEN_DYLAN_C_FLAGS)",
       "RFLAGS = $(rcflags) $(rcvars)"
       );
end method;

define method make-install-command
    (target :: <win32-makefile-target>, name) => (makefile-command :: <string>)
  format-to-string("(pushd ..\\%s & $(MAKE) install & popd)", name);
end method;

define method relative-makefile-unit-ref
    (target :: <win32-makefile-target>, name) => (makefile-unit :: <string>)
  format-to-string("..\\%s\\Makefile", name);
end method;

define method compiler-args-for-source (t :: <win32-makefile-target>, name)
  name
end method;

define method compiler-args-for-object (t :: <win32-makefile-target>, name)
  ""
end method;

define constant *win32-makefile-target* = make(<win32-makefile-target>);

define method makefile-target-using-os-name (name == #"win32")
   *win32-makefile-target*
end method;


/// MacOS (MPW) Makefile specifics ...

define class <MPW-makefile-target> (<makefile-target>) end;

define method makefile-rule-separator (t :: <MPW-makefile-target>)
  " \<C4> "			// Option-F (i.e., the "folder" character)
end method makefile-rule-separator;

define method makefile-variable-ref (t :: <MPW-makefile-target>, name)
  concatenate("{", name, "}")
end method makefile-variable-ref;

define method makefile-linker-option-massage (t :: <MPW-makefile-target>, name)
  let name-length = size(name);
  if (name-length > 3 & name[0] = '$' & name[1] = '(' & name[name-length - 1] = ')')
    makefile-variable-ref(t, copy-sequence(name, start: 2, end: name-length - 1))
  else
    name
  end
end method makefile-linker-option-massage;

///---*** NOTE: CWPro5 and/or Carbon will cause changes!
define method additional-platform-libraries
    (t :: <MPW-makefile-target>, for-app? :: <boolean>)
  list("-l\"MSL C.PPC.DLL\"",
       if (for-app?)
	 "-l\"MSL AppRuntime.Lib\""
       else
	 "-l\"MSL ShLibRuntime.Lib\""
       end,
       "-l\"MSL RuntimePPC.DLL\"",
       "-lInterfaceLib",
       "-lMathLib")
end method additional-platform-libraries;

define method library-unit-name (t :: <MPW-makefile-target>, unit)
  concatenate(unit, library-suffix(t));
end method library-unit-name;

define method library-unit-link-ref (t :: <MPW-makefile-target>, lib, unit)
  concatenate("-l", library-unit-name(t, unit))
end method library-unit-link-ref;

define method runtime-unit-link-ref  (t :: <MPW-makefile-target>, unit)
  let unit = select (unit by \=)
	       "run-time" => "DylanRuntime";
	       "gc"       => "BoehmGC";
	       otherwise  => unit
	     end;
  library-unit-link-ref(t, #f, unit)
end method runtime-unit-link-ref;

// Macintosh pathnames frequently contain spaces so we need to quote them appropriately.
// Further, the MPW convention is to include the trailing directory delimiter (:) in the
// value of variables corresponding to directories (e.g., OPEN_DYLAN_USER_LIB) so
// we can't unconditionally add the delimiter after each component.  (SIGH)
define method makefile-pathname (t :: <MPW-makefile-target>, #rest components)
  let almost
    = with-output-to-string (path)
	for (component in components)
	  if (component[0] = '{')
	    format(path, "\"%s\"", component)
	  elseif (position(component, ' ') | position(component, '\t'))
	    format(path, "\"%s\":", component)
	  else
	    format(path, "%s:", component)
	  end
	end
      end;
  if (almost[size(almost) - 1] = ':')
    copy-sequence(almost, end: size(almost) - 1)
  else
    almost
  end
end method makefile-pathname;

define method library-suffix (t :: <MPW-makefile-target>) "Lib" end;
define method executable-suffix (t :: <MPW-makefile-target>) #f end;

define method executable-unit (t :: <MPW-makefile-target>, unit)
  platform-specific(t, unit);
end method executable-unit;

define method makefile-program-setups (t :: <MPW-makefile-target>) 
  list("OPEN_DYLAN_RELEASE_BIN     = {OPEN_DYLAN_RELEASE_INSTALL}Bin:",
       "OPEN_DYLAN_RELEASE_INCLUDE = {OPEN_DYLAN_RELEASE_INSTALL}Include:",
       "OPEN_DYLAN_RELEASE_LIB     = {OPEN_DYLAN_RELEASE_INSTALL}Lib:",
       "OPEN_DYLAN_USER_BIN     = {OPEN_DYLAN_USER_INSTALL}Bin:",
       "OPEN_DYLAN_USER_INCLUDE = {OPEN_DYLAN_USER_INSTALL}Include:",
       "OPEN_DYLAN_USER_LIB     = {OPEN_DYLAN_USER_INSTALL}Lib:",
       "",
       "INSTALLOBJ = Echo Built",	//---*** NOTE: Can we do something that's silent???
       "INSTALLLIB = Duplicate -y",
       "UNINSTALL = Delete -c -ac -i",
       "MKDIR = NewFolder",
       "CREATE = Echo >",
       "",
       //---*** NOTE: There's a bug in CodeWarrior's Link tool that causes it to
       //---*** NOTE: reverse the value of the file type (-t) and creator (-c) options!
       "LINKSTUB = MWLinkPPC -xm stublibrary   -t 'buts' -c ' SPM'"
	 " -fragname {LIBNAME} -@export {LIBNAME}.exp"
	 " -L\"{OPEN_DYLAN_USER_LIB}\" -L\"{OPEN_DYLAN_RELEASE_LIB}\""
	 " -L\"{MWPPCLibraries}\" -L\"{SharedLibraries}\""
	 " -warn -o",
       "LINKLIB  = MWLinkPPC -xm sharedlibrary -t 'blhs' -c 'lyDH'"
	 " -fragname {LIBNAME} -@export {LIBNAME}.exp -sym fullpath"
	 " -L\"{OPEN_DYLAN_USER_LIB}\" -L\"{OPEN_DYLAN_RELEASE_LIB}\""
	 " -L\"{MWPPCLibraries}\" -L\"{SharedLibraries}\""
	 " -warn -o",
       "LINKAPP  = MWLinkPPC -xm application -sym fullpath"
	 " -stacksize 128 -sizemin 8192 -sizemax 8192"
	 " -L\"{OPEN_DYLAN_USER_LIB}\" -L\"{OPEN_DYLAN_RELEASE_LIB}\""
	 " -L\"{MWPPCLibraries}\" -L\"{SharedLibraries}\""
	 " -warn -o",
       "libcmt = -l\"MSL C.PPC.DLL\"",
       "",
       "GCDATASTART = ",
       "GCDATAEND   = ",
       "",
       "CC = MWCPPC",
       "CFLAGS = -sym full -I\"{OPEN_DYLAN_RELEASE_INCLUDE}\""
	 " -w nounusedarg,noimplicitconv,noemptydecl -nomapcr {OPEN_DYLAN_C_FLAGS}",
       "RC = Rez",
       "RFLAGS = "
       );
end method makefile-program-setups;

define method makefile-target-pathname-ref (t :: <MPW-makefile-target>)
  "{Targ}"
end method makefile-target-pathname-ref;

// This is quite messy as MPW doesn't have pushd/popd and, further, Make 
// doesn't actually run the commands, it simply echoes them!  (The user
// is supposed to select the commands in the Worksheet and then execute them.)
define method make-install-command
    (target :: <MPW-makefile-target>, name) => (makefile-command :: <string>)
  format-to-string("Set OldDir `Directory`\n"
		     "\tDirectory ::%s\n"
		     "\tMake install > \"{TempFolder}\"%s\n"
		     "\t\"{TempFolder}\"%s\n"
		     "\tDirectory \"{OldDir}\"\n"
		     "\tUnset OldDir",
		   name, name, name)
end method make-install-command;

define method relative-makefile-unit-ref
    (target :: <MPW-makefile-target>, name) => (makefile-unit :: <string>)
  format-to-string("\"::%s:Makefile\"", name);
end method relative-makefile-unit-ref;

define method compiler-args-for-source (t :: <MPW-makefile-target>, name)
  name
end method compiler-args-for-source;

define method compiler-args-for-object (t :: <MPW-makefile-target>, name)
  format-to-string("-o %s", name)
end method compiler-args-for-object;

define constant *MPW-makefile-target* = make(<MPW-makefile-target>);

define method makefile-target-using-os-name (name == #"carbon")
  *MPW-makefile-target*
end method makefile-target-using-os-name;


//// Generic makefile generation.

/// Generate a list of all C libraries used by this Dylan library and all used
/// Dylan libraries.  Do our best to avoid duplicate entries in the list and to
/// put those entries of the "deeper" used libraries before those of the "shallower"
/// libraries.  (I.e., if A uses B and C, B uses D, and C uses D and E, the order of
/// C libraries should be from D, B, E, C, A.)
define function all-c-libraries (library-description) => (c-libraries :: <sequence>)
  let all-c-libraries = make(<stretchy-vector>);
  let processed-lds = make(<table>);
  local method recurse (ld)
    unless (element(processed-lds, ld, default: #f))
      let lds = directly-used-library-descriptions(ld);
      for (ld in lds using backward-iteration-protocol)
        recurse(ld)
      end;
      element(processed-lds, ld) := #t;
      let c-libraries
        = apply(ldbs-c-libraries, library-description-build-settings(ld));
      for (c-library in c-libraries)
        add-new!(all-c-libraries, c-library, test: \=);
      end;
    end;
  end method recurse;
  recurse(library-description);
  as(<list>, all-c-libraries)
end function all-c-libraries;

// This is a define method and out here rather than a local method in
// the above to work around an emulator bug in #all-keys handling.
define method ldbs-c-libraries 
    (#key c-libraries = #(), #all-keys) => (c-libraries :: <sequence>)
  c-libraries
end method ldbs-c-libraries;

define method emit-makefile 
    (library-description, units, #rest build-keys, #key, #all-keys)
  let target = makefile-target(library-description);
  local method doit (#key executable = #f,
			  base-address = #f,
			  linker-options = #(),
			  c-source-files = #(),
			  c-header-files = #(),
			  c-object-files = #(),
			  rc-files = #(),
			  c-libraries = #())
	  let c-source-files = map(locator-name, c-source-files);
	  let c-header-files = map(locator-name, c-header-files);
	  let c-object-files = map(locator-name, c-object-files);
	  let rc-files = map(locator-name, rc-files);
	  emit-target-makefile(current-back-end(), target,
			       makefile-unit-name(target),
			       library-description, units, 
			       executable: executable,
			       base-address: base-address,
			       linker-options: 
				 map(curry(makefile-linker-option-massage, target),
				     linker-options),
			       c-source-files: c-source-files,
			       c-header-files: c-header-files,
			       c-object-files: c-object-files,
			       rc-files: rc-files,
			       c-libraries: 
				 map(curry(makefile-linker-option-massage, target),
				     all-c-libraries(library-description)))
	end method doit;
  apply(doit, build-keys)
end method emit-makefile;

//// High level utilities.

define method emit-makefile-program-setups (t, stream)
  format(stream, "# Application setup.\n\n");
  let setups = makefile-program-setups(t);
  for (setup in setups)
    format(stream, "%s\n", setup);
  end;
end method;

define method backend-object-file-name (t, source-file-locator)
  let backend-locator = as(<file-locator>, source-file-locator);
  as(<string>,
     merge-locators(make(<file-locator>, base: locator-base(backend-locator),
			                 extension: object-suffix(current-back-end(), t)),
		    backend-locator))
end;

define method backend-resource-file-name (t, source-file-locator)
  let backend-locator = as(<file-locator>, source-file-locator);
  as(<string>,
     merge-locators(make(<file-locator>, base: locator-base(backend-locator),
			                 extension: resource-suffix(current-back-end(), t)),
		    backend-locator))
end;

/// Emits makefile dependencies for any non-Dylan source files included in the project.
define method emit-makefile-object-dependencies
    (back-end, t, stream, sources, objects, objects-names, compilers, dependencies)
  format(stream, "# Object dependencies.\n\n");
  for (source in sources, object in objects, object-name in objects-names,
       compiler in compilers, dependency in dependencies)
    emit-makefile-rule
      (t, stream, object, concatenate(list(source), dependency),
       format-to-string("%s %s %s", 
                        compiler,
			compiler-args-for-source(t, source),
                        compiler-args-for-object(t, object-name)),
       makefile-command(t,
			"{INSTALLOBJ} %s %s",
			object-name, makefile-target-pathname-ref(t)));
    format(stream, "\n");
  end;
  format(stream, "\n");
end method emit-makefile-object-dependencies;

//// Low level utilities.

define method emit-makefile-header (stream)
  format(stream, "# Machine-generated Makefile - do not edit.\n\n");
end method;

define method emit-makefile-separator (stream)
  format(stream, "\n");
end method;

define method emit-makefile-footer (stream)
  format(stream, "\n# eof\n");
end method;

define method emit-makefile-definition (stream, name, units)
  format(stream, "%s = ", name);
  emit-makefile-units(stream, units);
  format(stream, "\n");
end method;

define method emit-makefile-rule (t, stream, target, units, #rest operations)
  format(stream, "%s%s", target, makefile-rule-separator(t));
  emit-makefile-units(stream, units);
  format(stream, "\n");
  for (operation in operations)
    format(stream, "\t%s\n", operation);
  end for;
end method;

define method emit-makefile-units (stream, units)
  for (unit in units) format(stream, "%s ", unit) end;
end method;

define method source-units (t, units)
  map(curry(source-unit, t), units)
end method;

define method object-units (t, units)
  map(curry(object-unit, t), units)
end method;

define method object-units-names (t, units)
  map(curry(object-unit-name, t), units)
end method;

define method glue-unit (unit)
  "_glue"
end method;

define method main-unit (unit)
  "_main"
end method;

define method platform-specific (t, name)
  name
end method;

define method platform-specific-concatenate (t, #rest names)
  apply(concatenate, names)
end method;

// eof

