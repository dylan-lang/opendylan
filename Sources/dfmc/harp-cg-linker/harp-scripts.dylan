Module: dfmc-harp-cg-linker
Author: Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// These two are define methods and out here rather than local methods
// to work around an emulator bug in #all-keys handling.
define method ldbs-executable (#key executable = #f, #all-keys)
 => (executable :: false-or(<string>))
  executable
end method ldbs-executable;

// define method ldbs-base-address (#key base-address = #f, #all-keys)
//  => (base-address :: false-or(<machine-word>))
//   base-address
// end method ldbs-base-address;

define method emit-build-script(back-end :: <harp-back-end>,
				t :: <makefile-target>,
				lib-desc, units,
				#key executable = #f,
				     base-address = #f,
				     linker-options = #(),
				     c-source-files = #(),
				     c-header-files = #(),
				     c-object-files = #(),
				     rc-files = #(),
				     c-libraries = #())
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
  let all-units = add(units, glue);
  let sources = source-units(t, all-units);
  let objects = object-units(t, all-units);
  let objects-names = object-units-names(t, all-units);
  local method library-filename (ld) => (name :: <string>)
	  apply(ldbs-executable, library-description-build-settings(ld))
	    | library-string(ld)
	end method;
  let used-libraries = all-used-library-descriptions(lib-desc);
////
///// !"$% NEED A FRIG FOR THE DYLAN LIBRARY HERE
////  Want to treat pentium-run-time & mmdw as used libraries
////
  let directly-used-libraries = directly-used-library-descriptions(lib-desc);
  let directly-used-library-names = map(library-string, directly-used-libraries);
  let directly-used-library-filenames = map(library-filename, directly-used-libraries);
  let used-library-names = map(library-string, used-libraries);
  let used-library-filenames = map(library-filename, used-libraries);

  emit-dot-link(target,
		lib-desc,
		objects,
		map(curry(library-unit-name, t), used-library-filenames));

// dylanmakefile emission is now handled by the Project Manager
  /*
  with-build-area-output (stream = lib-desc, name: "dylanmakefile")

    format(stream, "APP: %s\n", target);
    format(stream, "APPNAME: %s\n", target-filename);
    let dll-base = compute-base-address(lib-desc, base-address, used-libraries);
    format(stream, "BASE: /base:%s\n", machine-word-to-string(dll-base, prefix: "0x"));
    emit-script-definition(stream, "LINKOPTS", linker-options);
    emit-script-definition(stream, "OBJS", objects);
    emit-script-definition(stream, "LIBS", directly-used-library-names);
    emit-script-definition(stream, "LIBNAMES", directly-used-library-filenames);
    emit-script-definition(stream, "ALLLIBS", used-library-names);
    emit-script-definition(stream, "ALLLIBNAMES", used-library-filenames);
    emit-script-definition(stream, "C-HEADERS", c-headers);
    emit-script-definition(stream, "C-OBJS", c-objects);
    emit-script-definition(stream, "C-SRC-OBJS", c-source-objects);
    emit-script-definition(stream, "RC-OBJS", rc-source-objects);
    emit-script-definition(stream, "C-LIBS", c-libraries);
    emit-script-definition
      (stream, "RTLIBS", additional-platform-libraries(t, #t));

  end with-build-area-output;
  */
end method emit-build-script;

/// All Functional Developer DLLs that may be used by user libraries reside above
/// this address.  Consequently, it's an upper bound for the base for user
/// libraries including our private libraries (e.g., the compiler and environment)
// define constant $top-for-user-libraries = #x64000;

/// Presume that user libraries fit into 32 pages (128K)
// define constant $allowance-for-user-libraries = #x20;

// define method compute-base-address (ld, explicit-base, libraries)
//  => (computed-base :: <machine-word>)
//   explicit-base
//     | begin
// 	let baseless-libraries = 1;
// 	let top = $top-for-user-libraries;
// 	for (library in libraries)
// 	  let base = apply(ldbs-base-address, library-description-build-settings(library));
// 	  if (base)
// 	    top := min(top,
// 		       as(<integer>, machine-word-unsigned-shift-right(base, 12)))
// 	  else
// 	    baseless-libraries := baseless-libraries + 1
// 	  end
// 	end;
// 	machine-word-unsigned-shift-left
// 	  (as(<machine-word>, top - baseless-libraries * $allowance-for-user-libraries), 12)
//       end
// end method compute-base-address;


// emit .link file
define method emit-dot-link(name, ld, objects, libraries)
  let dylan-library? = dylan-library-library-description?(ld);
  with-build-area-output (link-stream = ld, name: concatenate(name, ".link"))
    // emit library set of .obj's
    for (file-unit in objects)
      format(link-stream, "%s\n", file-unit);
    end for;
    // emit complete sublibrary set of .libs
    if (dylan-library?)
      format(link-stream, "mincrt.lib\n");
      format(link-stream, "pentium-run-time.lib\n");
      format(link-stream, "mmdw.lib\n");
      format(link-stream, "mpsplinth.lib\n");
    else
      format(link-stream, "dylan-support.lib\n");
      for (lib in libraries.reverse)
	format(link-stream, "%s\n", lib);
      end for;
    end if;
  end with-build-area-output;
end method emit-dot-link;

// define method emit-script-definition (stream, name, units)
//   format(stream, "%s: ", name);
//   emit-makefile-units(stream, units);
//   format(stream, "\n");
// end method;

