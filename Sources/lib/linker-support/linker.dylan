Module:    linker-support
Synopsis:  Linker Support for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// A Linker for DLLs

define open class <dynamic-linker>(<linker>)
  constant slot imports :: <string-table> = make(<string-table>);
  constant slot exports :: <string-table> = make(<string-table>);
  constant slot fixups :: <string-table> = make(<string-table>);
  constant slot global-exports :: <string-table>, required-init-keyword: global-exports:;
  slot duplicate-exports :: <list> = #();
end class;

// Some reserved filenames

define constant $dylan-imports-asm-file = "dylanimp7.s";

define constant $dylan-runtime = "pentium-run-time";

define constant $dylan-support-imports = "dylan-support.import";

define constant $link-script = "dylanpe.script";

define constant $dylan-entry-point = "_init_dylan_library";



// Do all the assembly for library

define method assemble-library(build :: <build>, #key archive) => (objects :: <sequence>)
  let library = build.dylanlib;
  let objects = #();

  local method add-object(object :: <file-locator>, archive) => ()
	  if (archive)
	    execute-shell-commands(#t, #f,
				   "make-archive",
				   "$(archive)", archive,
				   "$(objects)", object);
	    objects := add(objects, object);
	  else
	    objects := add(objects, object);
	  end if;
	end method;

  add-object(assemble-file($dylan-imports-asm-file), #f);
  add-object(assemble-file(library-filename-with-extensions(library, "imp7", "s")), #f);

  for (executable :: <string> in *linker*.imports.key-sequence)
    unless (executable.runtime-library?)
      add-object(assemble-file(filename-with-extensions(executable, "imp", "s")), archive);
    end unless;
  end for;
  unless (*linker*.fake-imports.empty?)
    add-object(library-filename-with-extensions(library, "imp", "o"), archive);
  end unless;
  unless (library.dylan-library?)
    add-object(library-filename-with-extensions(library, "imp0", "o"), archive);
  end;
  objects
end method;

// Do the assembly of a source file

define method assemble-file(source :: <file-locator>) => (object :: <file-locator>)
  let object = new-filename-extension(source, "o");

  execute-shell-commands(#t, #f,
			 "assemble",
			 "$(object)", object,
			 "$(source)", source);
  uninstall(source);
  object
end method assemble-file;

define method assemble-file(source :: <string>) => (object :: <file-locator>)
  assemble-file(as(<file-locator>, source))
end method assemble-file;
