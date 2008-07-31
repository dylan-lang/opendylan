Module: dfmc-c-linker
Author: keith and jonathan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function c-output-basename (basename :: <string>) => (c-basename)
  basename
end function;

//// C backend Makefile generation ...

define sideways method source-suffix (back-end :: <c-back-end>, t :: <unix-makefile-target>) "c" end;
define sideways method object-suffix (back-end :: <c-back-end>, t :: <unix-makefile-target>) "o" end;

define sideways method source-suffix (back-end :: <c-back-end>, t :: <win32-makefile-target>) "c" end;
define sideways method object-suffix (back-end :: <c-back-end>, t :: <win32-makefile-target>) "obj" end;
define sideways method resource-suffix (back-end :: <c-back-end>, t :: <win32-makefile-target>) "res" end;

define sideways method source-suffix (back-end :: <c-back-end>, t :: <MPW-makefile-target>) "c" end;
define sideways method object-suffix (back-end :: <c-back-end>, t :: <MPW-makefile-target>) "o" end;
define sideways method resource-suffix (back-end :: <c-back-end>, t :: <MPW-makefile-target>) "rsrc" end;

define sideways method output-basename
    (back-end :: <c-back-end>, t :: <makefile-target>, basename :: <string>)
 => (basename)
  c-output-basename(basename);
end method;

define variable *shortend-basenames* = make(<string-table>);

///---*** NOTE: Should be part of the generic backend code, not here ...
///---*** (Obvious way of using a method on <back-end> would be ambiguous)
define sideways method output-basename
    (back-end :: <c-back-end>, t :: <MPW-makefile-target>, basename :: <string>)
 => (basename)
  let full-basename = c-output-basename(basename);
  if (size(full-basename) + size(".X") > 31)
    element(*shortend-basenames*, full-basename, default: #f)
      | begin
	  // Remove characters from the middle...
	  let full-basename-size = size(full-basename);
	  let middle = truncate/(full-basename-size, 2);
	  let excess = full-basename-size + size(".X") - 31;
	  let short-basename = concatenate(copy-sequence(full-basename, end: middle - excess),
					   copy-sequence(full-basename, start: middle));
	  *shortend-basenames*[full-basename] := short-basename
	end
  else
    full-basename
  end
end method;

define method library-string (library)
  as-lowercase(as(<string>, library.library-description-emit-name))
end method;

define method derived-platform-libraries (t, c-libraries)
  #()
end method derived-platform-libraries;

/// If a Win32 executable is linked with the $(guilflags) option, it's a GUI application
/// rather than a console application and will need the Win32Main library to supply an
/// appropriate WinMain function ...
define method derived-platform-libraries (t :: <win32-makefile-target>, c-libraries)
  if (any?(method (x) x = "$(guilflags)" end, c-libraries))
    #("Win32Main.lib")
  else
    #()
  end
end method derived-platform-libraries;

// These two are define methods and out here rather than local methods
// to work around an emulator bug in #all-keys handling.
define method ldbs-executable (#key executable = #f, #all-keys)
 => (executable :: false-or(<string>))
  executable
end method ldbs-executable;


///---*** NOTE: This generic and its methods should probably be moved to the generic backend.
define generic emit-lib-build-commands (t :: <makefile-target>, stream, lib);

define method emit-lib-build-commands (t :: <makefile-target>, stream, lib)
  emit-makefile-rule
    (t, stream, lib, 
     map(curry(makefile-variable-ref, t), #("OBJS", "C_OBJS", "C_SRC_OBJS")),
     makefile-command(t, "{LINKLIB}%s%s {OBJS} {C_OBJS} {C_SRC_OBJS}",
		      link-target-separator(t), lib))
end method emit-lib-build-commands;

define method emit-lib-build-commands (t :: <MPW-makefile-target>, stream, lib)
  emit-makefile-rule
    (t, stream, lib,
     map(curry(makefile-variable-ref, t), #("OBJS", "C_OBJS", "C_SRC_OBJS", "RC_OBJS")),
     // First build the stub library and generate the exports list ...
     makefile-command(t, 
		      "{LINKSTUB}%s%s {RC_OBJS} {OBJS} {C_OBJS} {C_SRC_OBJS}"
			" {LIBS} {C_LIBS} {LIBRTLIBS} {LINKER_OPTIONS}",
		      link-target-separator(t), lib),
     // Remove the CodeWarrior runtime exports to avoid duplicate definitions ...
     // (NOTE: \<A5> is Option-8 and \<B0> is Option-5)
     makefile-command(t, 
		      "StreamEdit -e '/\<A5>__ptmf_null\<B0>/||/\<A5>__vec_longjmp\<B0>/"
			"||/\<A5>longjmp\<B0>/||/\<A5>__terminate\<B0>/"
			"||/\<A5>__initialize\<B0>/delete' -o {LIBNAME}.exp {LIBNAME}.exp"),
     // Rebuild the stub library with the "proper" exports list ...
     makefile-command(t, 
		      "{LINKSTUB}%s%s {RC_OBJS} {OBJS} {C_OBJS} {C_SRC_OBJS}"
			" {LIBS} {C_LIBS} {LIBRTLIBS} {LINKER_OPTIONS}",
		      link-target-separator(t), lib),
     // Rename the stub library so it won't get overwritten ...
     makefile-command(t, "Rename {LIBFILE} {LIBFILE}.stub"),
     // Finally build the actual shared library ...
     makefile-command(t, 
		      "{LINKLIB}%s%s {RC_OBJS} {OBJS} {C_OBJS} {C_SRC_OBJS}"
			" {LIBS} {C_LIBS} {LIBRTLIBS} {LINKER_OPTIONS}",
		      link-target-separator(t), lib))
end method emit-lib-build-commands;


///---*** NOTE: This generic and its methods should probably be moved to the generic backend.
define generic emit-lib-install-commands
    (t :: <makefile-target>, stream, installed-libfile, lib-desc, target);

define method emit-lib-install-commands 
    (t :: <makefile-target>, stream, installed-libfile, lib-desc, target)
  emit-makefile-rule
    (t, stream, installed-libfile,
     map(curry(makefile-variable-ref, t), #("LIBFILE")),
     makefile-command(t, "{UNINSTALL} %s", library-unit-ref(t, lib-desc, target)),
     makefile-command(t, 
		      "{INSTALLLIB} %s %s",
		      makefile-pathname(t, makefile-variable-ref(t, "LIBFILE")),
		      makefile-pathname
			(t, makefile-variable-ref(t, "OPEN_DYLAN_USER_LIB"))))
end method emit-lib-install-commands;

define method emit-lib-install-commands
    (t :: <MPW-makefile-target>, stream, installed-libfile, lib-desc, target)
  let stub-source
    = makefile-pathname(t, concatenate(library-unit-name(t, target), ".stub"));
  let stub-destination
    = library-unit-ref(t, lib-desc, target);
  let shlib-source
    = makefile-pathname(t, library-unit-name(t, target));
  let shlib-destination
    = makefile-pathname(t,
			if (lib-desc & personal-library?(lib-desc))
			  makefile-variable-ref(t, "OPEN_DYLAN_USER_BIN")
			else
			  makefile-variable-ref(t, "OPEN_DYLAN_RELEASE_BIN")
			end if,
			library-unit-name(t, target));
  let xsym-source
    = makefile-pathname(t, concatenate(library-unit-name(t, target), ".xSYM"));
  let xsym-destination
    = makefile-pathname(t,
			if (lib-desc & personal-library?(lib-desc))
			  makefile-variable-ref(t, "OPEN_DYLAN_USER_BIN")
			else
			  makefile-variable-ref(t, "OPEN_DYLAN_RELEASE_BIN")
			end if,
			concatenate(library-unit-name(t, target), ".xSYM"));
  emit-makefile-rule
    (t, stream, installed-libfile,
     map(curry(makefile-variable-ref, t), #("LIBFILE")),
     makefile-command(t, "{UNINSTALL} %s", stub-destination),
     makefile-command(t, "{INSTALLLIB} %s %s", stub-source, stub-destination),
     makefile-command(t, "{UNINSTALL} %s", shlib-destination),
     makefile-command(t, "{INSTALLLIB} %s %s", shlib-source, shlib-destination),
     makefile-command(t, "{UNINSTALL} %s", xsym-destination),
     makefile-command(t, "{INSTALLLIB} %s %s", xsym-source, xsym-destination))
end method emit-lib-install-commands;


///---*** NOTE: This generic and its methods should probably be moved to the generic backend.
define generic emit-app-build-commands (t :: <makefile-target>, stream, exe);

define method emit-app-build-commands (t :: <makefile-target>, stream, exe)
  emit-makefile-rule
    (t, stream, exe,
     map(curry(makefile-variable-ref, t), #("LIBFILE", "MAIN", "RC_OBJS")),
     makefile-command(t,
		      "{LINKAPP}%s%s %s {MAIN} {RC_OBJS} {LIBFILE} {LIBS}"
			" {C_LIBS} %s {APPRTLIBS} {LINKER_OPTIONS}",
		      link-target-separator(t), exe,
		      makefile-pathname(t, makefile-variable-ref(t, "GCDATASTART")),
		      makefile-pathname(t, makefile-variable-ref(t, "GCDATAEND"))))
end method emit-app-build-commands;

define method emit-app-build-commands (t :: <MPW-makefile-target>, stream, exe)
  emit-makefile-rule
    (t, stream, exe,
     map(curry(makefile-variable-ref, t), 
	 #("OBJS", "C_OBJS", "C_SRC_OBJS", "MAIN", "RC_OBJS")),
     makefile-command(t,
		      "{LINKAPP}%s%s {MAIN} {RC_OBJS} {OBJS} {C_OBJS} {C_SRC_OBJS}"
			" {LIBS} {C_LIBS} {APPRTLIBS} {LINKER_OPTIONS}", 
		      link-target-separator(t), exe))
end method emit-app-build-commands;


///---*** NOTE: This generic and its methods should probably be moved to the generic backend.
define generic emit-app-install-commands (t :: <makefile-target>, stream, target);

define method emit-app-install-commands (t :: <makefile-target>, stream, target)
  emit-makefile-rule
    (t, stream, "install-app", #("app"), 
     makefile-command(t, "{UNINSTALL} %s", executable-unit-ref(t, target)),
     makefile-command(t, 
		      "{INSTALLLIB} %s %s",
		      makefile-pathname(t, makefile-variable-ref(t, "APP")),
		      makefile-pathname
			(t, makefile-variable-ref(t, "OPEN_DYLAN_USER_BIN"))))
end method emit-app-install-commands;

define method emit-app-install-commands (t :: <MPW-makefile-target>, stream, target)
  let xsym-source
    = makefile-pathname(t, concatenate(executable-unit(t, target), ".xSYM"));
  let xsym-destination
    = makefile-pathname(t,
			makefile-variable-ref(t, "OPEN_DYLAN_USER_BIN"),
			concatenate(executable-unit(t, target), ".xSYM"));
  emit-makefile-rule
    (t, stream, "install-app", #("app"), 
     makefile-command(t, "{UNINSTALL} %s", executable-unit-ref(t, target)),
     makefile-command(t, 
		      "{INSTALLLIB} %s %s",
		      makefile-pathname(t, makefile-variable-ref(t, "APP")),
		      makefile-pathname
			(t, makefile-variable-ref(t, "OPEN_DYLAN_USER_BIN"))),
     makefile-command(t, "{UNINSTALL} %s", xsym-destination),
     makefile-command(t, "{INSTALLLIB} %s %s", xsym-source, xsym-destination))
end method emit-app-install-commands;


define sideways method emit-target-makefile (back-end :: <c-back-end>,
					     t :: <makefile-target>,
					     makefile-name, lib-desc, units,
					     #key executable = #f,
					          base-address = #f,
					          linker-options = #(),
					          c-source-files = #(),
					          c-header-files = #(),
					          c-object-files = #(),
					          rc-files = #(),
					          c-libraries = #())
  with-build-area-output (stream = lib-desc, name: makefile-name)
    let name = library-description-emit-name(lib-desc);
    let c-objects = map(curry(as,<string>), c-object-files);
    let c-headers = map(curry(as,<string>), c-header-files);
    let c-sources = map(curry(as,<string>), c-source-files);
    // By the time we get here, c-sources is just the filename ...
    let c-source-objects = map(curry(backend-object-file-name, t), c-sources);
    // RC (resource) files apply only to Win32 targets ...
    let rc-sources
      = if (instance?(t, <win32-makefile-target>))
	  map(curry(as,<string>), rc-files)
	else
	  #()
	end;
    // By the time we get here, rc-sources is just the filename ...
    let rc-source-objects = map(curry(backend-resource-file-name, t), rc-sources);
    let target = as-lowercase(as(<string>, name));
    let target-filename = executable | target;
    let glue = glue-unit(target);
    let main = main-unit(target);
    let all-units = add(units, glue);
    let sources = source-units(t, all-units);
    let objects = object-units(t, all-units);
    let objects-names = object-units-names(t, all-units);
    let lib = library-unit-name(t, target-filename);
    let exe = executable-unit(t, target-filename);
    emit-makefile-header(stream);
    emit-makefile-program-setups(t, stream);
    emit-makefile-separator(stream);
    emit-makefile-definition(stream, "OBJS", objects);
    emit-makefile-definition(stream, "SRCS", sources);
    emit-makefile-definition(stream, "LINKER_OPTIONS", linker-options);
    emit-makefile-definition(stream, "C_HEADERS", c-headers);
    emit-makefile-definition(stream, "C_OBJS", c-objects);
    emit-makefile-definition(stream, "C_SRC_OBJS", c-source-objects);
    emit-makefile-definition(stream, "RC_OBJS", rc-source-objects);
    emit-makefile-definition(stream, "C_LIBS", c-libraries);
    // Somewhat bogus - we shouldn't be making assumptions about libraries used
    // by our used libraries.  Should just handle the directly used libraries,
    // and let those fend for themselves.  Unfortunately, this doesn't work
    // for static linking.  (It also doesn't work with shared libraries as
    // symbols aren't re-exported.)
    local method library-filename (ld) => (name :: <string>)
	    apply(ldbs-executable, library-description-build-settings(ld))
	      | library-string(ld)
	  end method;
    let used-libraries = all-used-library-descriptions(lib-desc);
    let used-library-names = map(library-string, used-libraries);
    let used-library-filenames = map(library-filename, used-libraries);
    let directly-used-libraries = directly-used-library-descriptions(lib-desc);
    let directly-used-library-names 
      = map(library-string, directly-used-libraries);
    let directly-used-library-filenames = map(library-filename, directly-used-libraries);
    emit-makefile-definition
      (stream, "LIBS", 
       map(curry(library-unit-link-ref, t), used-libraries, used-library-filenames));
    let runtime-libraries
      = concatenate(map(curry(runtime-unit-link-ref, t), #("run-time", "gc")),
		    additional-platform-libraries(t, #f),
		    derived-platform-libraries(t, c-libraries));
    emit-makefile-definition(stream, "LIBRTLIBS", runtime-libraries);
    let runtime-libraries
      = concatenate(map(curry(runtime-unit-link-ref, t), #("run-time", "gc")),
		    additional-platform-libraries(t, #t),
		    derived-platform-libraries(t, c-libraries));
    emit-makefile-definition(stream, "APPRTLIBS", runtime-libraries);
    let main-objects = list(object-unit(t, main));
    emit-makefile-definition(stream, "MAIN", main-objects);
    emit-makefile-definition(stream, "LIBNAME", list(target-filename));
    // Note: The following name can't be "LIB" because it would shadow a
    // key environment variable under Windows.
    emit-makefile-definition(stream, "LIBFILE", list(lib));
    emit-makefile-definition(stream, "APP", list(exe));
    emit-makefile-separator(stream);
    emit-lib-build-commands(t, stream, lib);
    emit-makefile-separator(stream);
    emit-app-build-commands(t, stream, exe);
    emit-makefile-separator(stream);
    emit-makefile-rule
      (t, stream, "lib", list(lib));
    emit-makefile-separator(stream);
    for (ld in directly-used-libraries, 
	 lib-name in directly-used-library-names,
	 lib-filename in directly-used-library-filenames)
      if (personal-library?(ld))
        apply(emit-makefile-rule, t, stream, lib-filename,
              list(library-unit-ref(t, ld, lib-filename)), #[]);
        emit-makefile-separator(stream);
        apply(emit-makefile-rule,
              t, stream, library-unit-ref(t, ld, lib-filename),
              list(relative-makefile-unit-ref(t, lib-name)),
	      vector(make-install-command(t, lib-name)))
      else
        apply(emit-makefile-rule, t, stream, lib-name, #(), #[]);
      end if;
      emit-makefile-separator(stream);
    end for;
    // NOTE -- This creates inconsistent Makefiles if the library is a system library.
    // In particular, these rules build into PERSONAL_LIB but delete from SYSTEM_LIB.
    let installed-libfile
      = makefile-pathname(t,
			  makefile-variable-ref(t, "OPEN_DYLAN_USER_LIB"),
			  makefile-variable-ref(t, "LIBFILE"));
    emit-lib-install-commands(t, stream, installed-libfile, lib-desc, target);
    emit-makefile-separator(stream);
    emit-makefile-rule
      (t, stream, "install", 
       concatenate(directly-used-library-filenames, 
                   list(installed-libfile)));
    emit-makefile-separator(stream);
    emit-makefile-rule
      (t, stream, "app", list("install", exe));
    emit-makefile-separator(stream);
    emit-app-install-commands(t, stream, target);
    emit-makefile-separator(stream);
    emit-makefile-rule
      (t, stream, "clean", #(), 
       makefile-command(t,
			"{UNINSTALL} {OBJS} {LIBFILE} {APP} {C_SRC_OBJS} {RC_OBJS} %s",
			object-unit(t, main)));
    emit-makefile-separator(stream);
    emit-makefile-rule
      (t, stream, "clean-all", #("clean"), 
       makefile-command(t, "{UNINSTALL} {SRCS} %s", source-unit(t, main)));
    emit-makefile-separator(stream);
    emit-makefile-rule
      (t, stream, "run", #("app"), exe);
    emit-makefile-separator(stream);
    emit-makefile-object-dependencies
      (back-end, t, stream, 
       concatenate(vector(source-unit(t, main)), sources, c-sources, rc-sources),
       concatenate(vector(object-unit(t, main)), objects, c-source-objects, 
		   rc-source-objects),
       concatenate(vector(object-unit-name(t, main)), objects-names,
		   c-source-objects, rc-source-objects),
       concatenate(make(<vector>, size: size(sources) + size(c-sources) + 1,
				  fill: makefile-command(t, "{CC} {CFLAGS}")),
		   make(<vector>, size: size(rc-sources), 
			          fill: makefile-command(t, "{RC} {RFLAGS}"))),
       concatenate(vector(#()), make(<vector>, size: size(sources), fill: #()),
		   make(<vector>, size: size(c-sources),
			          fill: list(makefile-variable-ref(t, "C_HEADERS"))),
		   make(<vector>, size: size(rc-sources),
			          fill: list(makefile-variable-ref(t, "C_HEADERS")))));
    emit-makefile-footer(stream);
    emit-build-system-files(target,
			    lib-desc,
			    objects,
			    main-objects,
			    map(curry(library-unit-link-ref, t), 
				used-libraries, used-library-filenames),
			    runtime-libraries);
  end with-build-area-output;
end method emit-target-makefile;

define method emit-build-system-files 
    (name, ld, objects, main-objects, libraries, runtime-libraries)
  with-build-area-output (link-stream = ld, name: concatenate(name, ".linkexe"))
    for (file-unit in objects)
      format(link-stream, "%s\n", file-unit);
    end for;
    for (file-unit in main-objects)
      format(link-stream, "%s\n", file-unit);
    end for;
    for (lib in libraries.reverse)
      format(link-stream, "%s\n", lib);
    end for;
    for (lib in runtime-libraries)
      format(link-stream, "%s\n", lib);
    end for;
  end with-build-area-output;
  with-build-area-output (link-stream = ld, name: concatenate(name, ".linklib"))
    for (file-unit in objects)
      format(link-stream, "%s\n", file-unit);
    end for;
  end with-build-area-output;
end method emit-build-system-files;
