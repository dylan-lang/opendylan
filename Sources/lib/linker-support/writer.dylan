Module:    linker-support
Synopsis:  Linker Support for Dylan PC Applications in Dylan
Author:    Nosa Omo (Adapted from Jon Thackray's bash scripts)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Write assembler for import data section of a DLL

define method write-imports() => ()

  for (executable :: <string> in *linker*.imports.key-sequence)
    select (executable by \=)
      "runtime"  => write-runtime-imports();
      otherwise  => write-imports-for-executable(executable);
    end select;
  end for;

end method;

define method write-imports-for-executable
    (executable :: <string>, #key all?) => ()
  let import-file = filename-with-extensions(executable, "imp", "s");
  let linker-imports = *linker*.imports[executable].key-sequence;
  let prefix = linker-mangle(executable);
  let addresses
    = assign-addresses(executable, prefix, linker-imports, all?: all?);

  with-open-file (stream = import-file,
		  direction: #"output")

    // Now make the .idata$2 section (import table directory entry)

    format(stream,
	   "# Import Table Directory Entry\n"
	     "\t.section\t.idata$2\n");
    format(stream,
	   "\t.rva\t%s_idata$4\n",
	   prefix);
    format(stream,
	   "\t.long\t0\n"
	   "\t.long\t0\n");
    format(stream,
	   "\t.rva\t%s_idata$7\n",
	   prefix);
    format(stream,
	   "\t.rva\t%s_idata$5\n",
	   prefix);

    // Now make the .idata$3 section (import table directory terminator)

    format(stream,
	   "# Import Table Directory Terminator\n"
	   "\t.section\t.idata$3\n"
	   "\t.long\t0,0,0,0,0\n");

    // Now make the .idata$4 section (import lookup table)

    format(stream,
	   "# Import Lookup Table\n"
	   "\t.section\t.idata$4\n");
    format(stream,
	   "%s_idata$4:\n",
	   prefix);

    for (address in addresses)
      format(stream,
	     "\t.rva\t%s\n",
	     address.import-id);
    end for;

    format(stream,
	   "\t.long\t0\n");


    // Now make the .idata$5 section (import address table)

    format(stream,
	   "# Import Address Table\n"
	   "\t.section\t.idata$5\n");

    for (import in linker-imports)
      format(stream,
	     "\t.global\t__imp_%s\n",
	     import);
    end for;

    format(stream,
	   "%s_idata$5:\n",
	   prefix);

    for (import in linker-imports,
	 address in addresses)
      format(stream,
	     "__imp_%s:\t.rva\t%s\n",
	     import, address.import-id);
    end for;

    format(stream,
	   "\t.long\t0\n");

    // Now make the .idata$6 section (hint/name table)

    format(stream,
	   "# Hint/Name table\n"
	   "\t.section\t.idata$6\n");

    for (import in linker-imports,
	 address in addresses)
      format(stream,
	     "\t.align\t2;%s:\t.short\t%d;\t.asciz\t%=\n",
	     address.import-id, address.import-position, import.import-name);
    end for;

    format(stream,
	   "\t.section\t.idata$7\n");
    format(stream,
	   "%s_idata$7:\n",
	   prefix);
    format(stream,
	   "\t.asciz\t%=\n",
	   as(<string>, filename-with-extension(executable, "dll")));

  end with-open-file;
end method;

define method write-runtime-imports
    () => ()

  let import-file = as(<file-locator>, $dylan-imports-asm-file);
  let linker-imports = *linker*.imports["runtime"].key-sequence;

  with-open-file (stream = import-file,
                  direction: #"output")
    write-trampolines(stream, linker-imports);
  end with-open-file;

end method;

define method write-glue-imports
    (filename :: <string>, linker-imports :: <sequence>) => ()
  let filename = filename | library-file-name(*build*.dylanlib);
  let import-file = filename-with-extensions(filename, "imp7", "s");

  with-open-file (stream = import-file,
                  direction: #"output")
    write-trampolines(stream, linker-imports);
  end with-open-file;

end method;

// For Initializers and Runtime imports only, create trampolines

define method write-trampolines
    (stream :: <stream>, linker-imports :: <sequence>) => ()

  format(stream,
	 "\t.text\n");
  for (import in linker-imports)
    format(stream,
	   "\t.global\t%s\n",
	   import);
  end for;

  for (import in linker-imports)
    format(stream,
	   "\t.global\t__imp_%s\n",
	   import);
  end for;

  for (import in linker-imports)
    format(stream,
	   "%s:\tjmp\t*__imp_%s\n",
	   import, import);
  end for;

end method;

// Assign addresses to all imports into parent library

define primary class <import-address>(<object>)
  constant slot import-position :: <integer>, required-init-keyword: position:;
  constant slot import-id :: <string>, required-init-keyword: id:;
end class;

define method assign-addresses
    (executable :: <string>, prefix :: <string>, imports :: <sequence>,
     #key all?)
 => (addresses :: <sequence>)

  if (all?)

  let exports :: <sequence> =	sort(imports);
  let address :: <integer> = -1;
  map(method (export :: <string>)
	address := address + 1;
	make(<import-address>,
	     position: address,
	     id: format-to-string("%s_ID%d",
				  prefix,
				  address));
      end method,
      exports);

  else

  let exports = read-global-exports(executable);

  map(method (import :: <string>)
	let export-name = import.export-name;
	let address = element(exports, export-name, default: not-found());

	if (not-found?(address))
	  build-error("Expected import %= not found in exports-file for %= in linking %=",
		      export-name, executable, *build*.dylanlib);
	else
	  select (address by instance?)
	    <integer> =>
	      exports[export-name] :=
		make(<import-address>,
		     position: address,
		     id: format-to-string("%s_ID%d",
					  prefix,
					  address));
	    otherwise => address;
	  end select
	end if
      end method,
      imports);

  end if;

end method;


// Write assembler for export data section of a DLL

define method write-exports
    (build :: <build>, linker-exports :: <sequence>) => ()
  let library = build.dylanlib;
  let export-file = library-filename-with-extensions(library, "exp", "s");
  let size-exports = linker-exports.size;

  with-open-file (stream = export-file,
                  direction: #"output")

    // Now make the header

    format(stream,
           "\t.section\t.edata\n"
           "# The export directory table\n"
           "# Export flags (0)\n"
           "\t.long\t0\n"
           "# Stamp (4)\n"
           "\t.long\t0\n"
           "# Major (8)\n"
           "\t.short\t0\n"
           "# Minor (a)\n"
           "\t.short\t0\n"
           "# Name table RVA (c)\n"
           "\t.rva\tdll_name\n"
           "# Ordinal base (10)\n"
           "\t.long\t1\n"
           "# Number of address table entries (14)\n"
	   );
    format(stream,
           "\t.long\t%d\n",
	   size-exports);
    format(stream,
           "# Number of name pointers (18)\n"
	   );
    format(stream,
           "\t.long\t%d\n",
	   size-exports);
    format(stream,
           "# Export address table RVA (1c)\n"
           "\t.rva\texport_address_table\n"
           "# Name pointer table RVA (20)\n"
           "\t.rva\tname_pointer_table\n"
           "# Ordinal table RVA (24)\n"
           "\t.rva\tordinal_table\n"
           );

    // Generate Duplicate exports
    // (only applies to stdcalls at the moment)
    for (export in *linker*.duplicate-exports)
      format(stream,
	     "\t.equ\t%s,\t%s\n",
	     export.head,
	     export.tail);
    end for;

    format(stream,
	   "export_address_table:\n");
    for (export in linker-exports)
      format(stream,
	     "\t.rva\t%s\n",
	     export.export-fixup);
    end for;

    format(stream,
	   "name_pointer_table:\n");
    for (export in linker-exports)
      format(stream,
	     "\t.rva\t%s_name$\n",
	     export);
    end for;

    format(stream,
	   "ordinal_table:\n");
    for(i from 0 to size-exports - 1)
      format(stream,
	     "\t.short\t%=\n",
	     i);
    end for;

    format(stream,
	   "dll_name:\n"
	   "\t.asciz\t%=\n", "foo.dll");

    for (export in linker-exports)
      format(stream,
	     "%s_name$:\t.asciz\t%=\n",
	     export,
	     export);
    end for;

  end with-open-file;
  
end method;

// Write exports file for library

define method write-global-exports
    (build :: <build>, linker-exports :: <sequence>)
  => ()
  let library :: <symbol> = build.dylanlib;
  let executable :: <string> = library-file-name(library);
  let exports-file = build.libfile;
  let exports-table = make(<string-table>);

  with-open-file (stream = exports-file,
                  direction: #"output")
    for (export in linker-exports,
	 count from 0)
      exports-table[export] := count;
      write(stream, export);
      write(stream, "\n");
    end for;
  end with-open-file;
  *linker*.global-exports[executable] := exports-table;
  values();
end method;

