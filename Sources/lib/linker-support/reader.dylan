Module:    linker-support
Synopsis:  Linker Support for Dylan PC Applications in Dylan
Author:    Nosa Omo (Adapted from Jon Thackray's bash scripts)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Read import information from library-ordered compiler-generated
// .import files, and "dylan-support.import"

define method read-imports() => ()
  let glue-imports = make(<string-table>);
  read-build-imports(*build*, glue-imports);
  finalize-imports(*build*, glue-imports);
end method;

define method build-linker-imports
    (linker :: <dynamic-linker>, export? :: <boolean>)
 => (linker-imports :: <string-table>)
  if (export?) *linker*.fake-imports
  else *linker*.imports
  end if;
end method;

define method finalize-imports
    (build :: <build>, glue-imports :: <string-table>) => ()
  // write assembler for Initializer imports
  write-glue-imports(library-file-name(build.dylanlib), glue-imports.key-sequence);

  let import-file = search-for-file(as(<file-locator>, $dylan-support-imports));
  read-imports-in-file(import-file);
  merge-imports();
end method;


// Read export information from compiler-generated .def files

define method read-exports() => (linker-exports :: <sequence>)
  read-build-exports(*build*);
  finalize-exports(*build*);
end method;

define method read-build-exports
    (build :: <build>,
     #key directory :: false-or(<directory-locator>) = #f) => ()
  let library = build.dylanlib;
  let objs = build.objs;
  for (obj in objs)
    let export-file = new-filename-extension(obj, "def");
    let export-path = if (directory)
			merge-locators(export-file, directory)
		      else
			export-file
		      end;
    read-exports-in-file(export-path);
  end for;
  /*
  for (export-file in create-foreign-exports())
    read-exports-in-file(export-file)
  end for;
  */
  if (library.dylan-library?)
    // for dylan-library create and use extra .def file,
    // "pentium-run-time.def"
    let runtime-export-file = create-exports-file-for-library(as(<file-locator>, $dylan-runtime));

    read-exports-in-file(runtime-export-file, external?: #t);
  end if;
end method;

define method finalize-exports(build :: <build>) => (linker-exports :: <sequence>)
  let linker-exports = sort(*linker*.exports.key-sequence);
  // write exports file for library
  write-global-exports(build, linker-exports);
  linker-exports
end method;

define method read-exports-in-file(export-file :: <file-locator>, #key external?) => ()
  let export-location = as(<file-locator>, export-file);

  with-open-file (stream = export-location)
    with-stream-input()
      let export = read-line(stream);
      do-export-name(export, external?: external?);
    end with-stream-input;
  end with-open-file;
end method;

// Export Registration

define method do-export-name(export :: <string>, #key external?) => ()
  if (export[0] = '_')
    // Retain leading underscores if stdcall calling convention
    let size = export.size;
    let stdcall? = stdcall-name?(export);
    if (stdcall?)
      unless (external?)
	// for in-language stdcall exports, we want to also export the
	// demangled stdcall name for dynamic-linking
	let export-name =  copy-sequence(export, start: 1, end: stdcall?);
	let full-export-name = concatenate("_", export-name);

	*linker*.fixups[export-name] := full-export-name;
	*linker*.duplicate-exports := add(*linker*.duplicate-exports, pair(full-export-name, export));
	*linker*.exports[export-name] := #t;
      end unless;
      *linker*.exports[export] := #t;
    else
      let export-name =  copy-sequence(export, start: 1);
      *linker*.fixups[export-name] := export;
      *linker*.exports[export-name] := #t
    end if
  else
    *linker*.exports[export] := #t
  end if;
end method;

define method stdcall-name?(name :: <string>) => (stdcall?)
  subsequence-position(name, "@");
end method;

// Handle export names that begin with leading underscores that may mess up
// orderings in export tables

define method export-name(export :: <string>) => (export-name :: <string>)
  if (export[0] = '_')
    // Retain leading underscores if stdcall calling convention
    let size = export.size;
    if (stdcall-name?(export))
      export
    else
      copy-sequence(export, start: 1)
    end if
  else
    export
  end if;
end method;

// Restore original export names at assembler creation time

define method export-fixup(export-name :: <string>) => (export :: <string>)
  let fixup? = element(*linker*.fixups, export-name, default: #f);

  fixup? | export-name
end method;

// Handle import names that begin with leading underscores that may mess up
// orderings in import tables

define method import-name(import :: <string>) => (import-name :: <string>)
  if (import[0] = '_')
    // Retain leading underscores if stdcall calling convention
    let size = import.size;
    if (stdcall-name?(import))
      import
    else
      copy-sequence(import, start: 1);
    end if
  else
    import
  end if;
end method;

// Read in export information for a DLL;
// hopefully this has already been read in and is cached

define method read-global-exports
    (executable :: <string>)
 => (exports :: <string-table>)
  let exports = element(*linker*.global-exports, executable, default: #f);

  if (exports)
    echo?("Re-using exports cache of %=", executable);
    exports
  else
    let defs-file
      = filename-with-extension(executable, exports-file-extension(*linker*));
    let defs = search-for-file(defs-file, error?: #f);
    unless (defs)
      build-error("Exports file for %s not found", executable);
    end unless;
    let defs-location = as(<file-locator>, defs);
    let exports-table = make(<string-table>);
    let count = 0;
  
    with-open-file (stream = defs-location)
      with-stream-input()
	  let export = read-line(stream);
	  exports-table[export] := count;
	  count := count + 1;
      end with-stream-input;
    end with-open-file;
    *linker*.global-exports[executable] := exports-table;
    exports-table
  end if;
end method;

// Create exports file from a statically linked library

define open generic create-exports-file
    (linker :: <linker>, destination :: <file-locator>, source :: <file-locator>) => ();

define method create-exports-file-for-library (library-locator :: <file-locator>)
 => (exports-file :: <file-locator>)
  let def-file = filename-with-extension(library-locator, "def");
  // Don't error if we can create it on the fly
  let exports-file = search-for-file(def-file, error?: #f);
  let library-file = search-for-file(filename-with-extension(library-locator, "lib"));

  if (*build*.force? | *build*.link? | ~ exports-file
      | newer-file?(library-file, exports-file))
    let exports-file = file-in-directory(*lib-installation*, def-file.locator-name);

    create-exports-file(*linker*,
			exports-file,
			library-file);
    exports-file
  else
    exports-file
  end if;

end method;

// Create imports file from an object file

define open generic create-imports-file
    (linker :: <linker>, destination :: <file-locator>, 
     source :: <file-locator>) => ();

define method create-imports-file-for-source
    (source :: <file-locator>)
 => (imports-file :: <file-locator>)
  debug-message("Create Imports file for source=%s", source);
  let file = filename-with-extension(source, "import");
  // Don't error if we can create it on the fly
  debug-message("File=%s", file);
  let imports-file = search-for-file(file, error?: #f);
  debug-message("Imports File=%s", imports-file);
  let source-file = search-for-file(filename-with-extension(source, "obj"));
  debug-message("Source File=%s", source-file);

  if (~ imports-file
      | newer-file?(source-file, imports-file))
    debug-message("Imports file out of date...");
    let imports-file = file-in-directory(*lib-installation*, file.locator-name);
    debug-message("Creating imports file %s", as(<string>, imports-file));

    create-imports-file(*linker*,
			imports-file,
			source-file);
    debug-message("Created imports file");
    imports-file
  else
    imports-file
  end if;

end method;

// define method create-foreign-exports(build :: <build>)
//  => (exports-files :: <sequence>)
//   local method create-foreign-export-file(object) => (exports-file :: <file-locator>)
// 	  let exports-file = new-filename-extension(object, "def");
// 	  
// 	  if (~ file-exists?(exports-file)
// 		| newer-file?(object, exports-file))
// 	    create-exports-file(*linker*,
// 				exports-file,
// 				object);
// 	  end if;
// 	  exports-file
// 	end method;
// 
//   map(create-foreign-export-file,
//       build.c-src-objs)
// 
// end method;
// 
// define method create-foreign-imports(build :: <build>)
//  => (imports-files :: <sequence>)
//   local method create-foreign-import-file(object) => (imports-file :: <string>)
// 	  let imports-file = new-filename-extension(object, "import");
// 	  
// 	  if (~ file-exists?(imports-file)
// 		| newer-file?(object, imports-file))
// 	    create-imports-file(*linker*,
// 				imports-file,
// 				object);
// 	  end if;
// 	  imports-file
// 	end method;
// 
//   map(create-foreign-import-file,
//       build.c-src-objs)
// 
// end method;

// Runtime imports are also Dylan imports, so update dylan import table
// to reflect this

define method merge-imports() => ()
  let runtime-imports = *linker*.imports["runtime"];
  let runtime-imports-sequence = runtime-imports.key-sequence;
  let dylan-imports = *linker*.imports[$dylan-executable-name];

  for (import in runtime-imports-sequence)
    dylan-imports[import] := runtime-imports[import];
  end for;
end method;
