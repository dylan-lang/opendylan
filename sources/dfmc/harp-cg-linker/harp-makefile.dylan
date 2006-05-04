Module: dfmc-harp-cg-linker
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// Platform-specific makefile configuration.

define sideways method source-suffix (back-end :: <harp-back-end>, t :: <win32-makefile-target>) "asm" end;
define sideways method object-suffix (back-end :: <harp-back-end>, t :: <win32-makefile-target>) "obj" end;
define sideways method resource-suffix (back-end :: <harp-back-end>, t :: <win32-makefile-target>) "res" end;

define sideways method source-suffix (back-end :: <harp-back-end>, t :: <unix-makefile-target>) "s" end;
define sideways method object-suffix (back-end :: <harp-back-end>, t :: <unix-makefile-target>) "o" end;
define sideways method resource-suffix (back-end :: <harp-back-end>, t :: <unix-makefile-target>) "res" end;

define method library-string (library)
  as-lowercase(as(<string>, library.library-description-emit-name));
end method;

// define method run-time-library?(back-end :: <harp-back-end>, unit)
//   #f;
// end method;


// Now emit a build batfile instead
// See harp-scripts.dylan

define variable *emit-makefile?* = #f;

define sideways method emit-target-makefile
    (back-end :: <harp-back-end>,
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
  local method emit-makefile()
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
      let glue = glue-unit(target);
      let all-units = add(units, glue);
      let sources = source-units(t, all-units);
      let objects = object-units(t, all-units);
      let objects-names = object-units-names(t, all-units);
      let lib = library-unit-name(t, target);
      let exe = executable-unit(t, target);
      emit-makefile-header(stream);
      emit-makefile-program-setups(t, stream);
      emit-makefile-separator(stream);
      emit-objs(lib-desc, objects);
      emit-makefile-definition(stream, "OBJS", objects);
      emit-makefile-definition(stream, "C_HEADERS", c-headers);
      emit-makefile-definition(stream, "C_OBJS", c-objects);
      emit-makefile-definition(stream, "C_SRC_OBJS", c-source-objects);
      emit-makefile-definition(stream, "RC_OBJS", rc-source-objects);
      emit-makefile-definition(stream, "C_LIBS", c-libraries);
      emit-makefile-definition(stream, "LINK_OPTIONS", linker-options);
      let used-libraries = all-used-library-descriptions(lib-desc);
    ////
    ///// !"$% NEED A FRIG FOR THE DYLAN LIBRARY HERE
    ////  Want to treat pentium-run-time & mmdw as used libraries
    ////
      let used-library-names = map(library-string, used-libraries);
      let directly-used-libraries = directly-used-library-descriptions(lib-desc);
      let directly-used-library-names 
        = map(library-string, directly-used-libraries);
      emit-libs(lib-desc, map(curry(library-unit-name, t), used-library-names));
      emit-makefile-definition
        (stream, "RTLIBS", additional-platform-libraries(t, #t));
      // Note: The following name can't be "LIB" because it would shadow a
      // key environment variable under Windows.
      emit-makefile-definition(stream, "LIBFILE", list(lib));
      emit-makefile-definition(stream, "APP", list(exe));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, lib, #("$(OBJS)", "$(C_OBJS)", "$(C_SRC_OBJS)"),
         format-to-string
           ("$(LINKLIB)%s%s @objs.lin $(C_OBJS) $(C_SRC_OBJS)",
	    link-target-separator(t), lib));
      // format-to-string("$(INSTALLOBJ) %s $@", lib)
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, exe, #("$(LIBFILE)", "$(RC_OBJS)"), 
         format-to-string
           ("$(LINKAPP)%s%s @libs.lin $(RC_OBJS) $(C_LIBS) $(RTLIBS) $(LINK_OPTIONS)", 
            link-target-separator(t), exe));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "lib", list(lib));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "exe", list(exe));
      emit-makefile-separator(stream);
      for (ld in directly-used-libraries, lib-name in directly-used-library-names)
        if (personal-library?(ld))
          apply(emit-makefile-rule, t, stream, lib-name,
                list(library-unit-ref(t, ld, lib-name)), #[]);
          emit-makefile-separator(stream);
          apply(emit-makefile-rule,
                t, stream, library-unit-ref(t, ld, lib-name),
                list(relative-makefile-unit-ref(t, lib-name)),
	        vector(make-install-command(t, lib-name)))
        else
          apply(emit-makefile-rule, t, stream, lib-name, #(), #[]);
        end if;
        emit-makefile-separator(stream);
      end for;
      // NOTE -- This creates inconsistent Makefiles if the library is a system library.
      // In particular, these rules build into PERSONAL_LIB but delete from SYSTEM_LIB.
      emit-makefile-rule
        (t, stream, "$(OPEN_DYLAN_USER_LIB)/$(LIBFILE)",
         list("$(LIBFILE)"),
         format-to-string
           ("$(UNINSTALL) %s", library-unit-ref(t, lib-desc, target)),
         "$(INSTALLLIB) $(LIBFILE) $(OPEN_DYLAN_USER_LIB)");
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "install", 
         concatenate(directly-used-library-names,
		     #("$(OPEN_DYLAN_USER_LIB)/$(LIBFILE)")));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "app", list("install", exe));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "install-app", #("app"), 
         format-to-string
           ("$(UNINSTALL) %s", executable-unit-ref(t, target)),
         "$(INSTALLLIB) $(APP) $(OPEN_DYLAN_USER_BIN)");
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "clean", #(), 
         format-to-string
           ("$(UNINSTALL) $(LIBFILE) $(APP) $(C_SRC_OBJS) $(RC_OBJS)"));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "clean-all", #("clean"), 
         format-to-string
           ("$(UNINSTALL) *.obj *.asm *.harp"));
      emit-makefile-separator(stream);
      emit-makefile-rule
        (t, stream, "run", #("app"), exe);
      emit-makefile-separator(stream);
      emit-makefile-object-dependencies
        (back-end, t, stream, 
	 concatenate(c-sources, rc-sources),
	 concatenate(c-source-objects, rc-source-objects),
	 concatenate(c-source-objects, rc-source-objects),
	 concatenate(make(<vector>, size: size(c-sources), fill: "$(CC) $(CFLAGS)"),
		     make(<vector>, size: size(rc-sources), fill: "$(RC) $(RFLAGS)")),
         make(<vector>, size: size(c-sources) + size(rc-sources), fill: #("$(C_HEADERS)")));
      emit-makefile-footer(stream);
    end with-build-area-output;
  end method;

  if (*emit-makefile?*)
    emit-makefile();
  end if;  
end method emit-target-makefile;

define method emit-objs(ld, objects)
  with-build-area-output (objs-stream = ld, name: "objs.lin")
    // emit objs.lin file -- library set of .obj's
    for (file-unit in objects)
      format(objs-stream, "%s\n", file-unit);
    end for;
  end with-build-area-output;
end method emit-objs;

define method emit-libs(ld, libraries)
  with-build-area-output (libs-stream = ld, name: "libs.lin")
    // emit libs.lin file -- complete sublibrary set of .libs
    for (lib in libraries.reverse)
      format(libs-stream, "%s\n", lib);
    end for;
  end with-build-area-output;
end method emit-libs;
