Module:    motley
Author:    Seth LaForge
Synopsis:  Motley tool interface
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function motley-invoke 
	(spec-file :: <locator>, project-file :: false-or(<locator>),
	 last-run :: false-or(<date>), #key clean-build? :: <boolean> = #f)
     => (success? :: <boolean>, 
         modified-projects :: <sequence> /* of: <locator> */)
  // Read spec file:
  debug-message("Motley invoked.  Spec: %=, project: %=, last: %=, clean: %=\n",
  		spec-file, project-file, last-run & date-as-string(last-run),
		clean-build?);
  let spec-keys = read-keyword-pair-file(spec-file);
  motley-process-spec(spec-keys, spec-file, project-file, last-run, 
		      clean-build?: clean-build?);
end function motley-invoke;


// Register!

tool-register(#"com-type-library", motley-invoke);


define function motley-process-spec 
	(spec :: <table>, spec-file :: <locator>,
	 project-file :: false-or(<locator>), last-run :: false-or(<date>),
	 #key clean-build? :: <boolean>)
     => (success? :: <boolean>, 
         modified-projects :: <sequence> /* of: <locator> */)
  let keyval = keyword-file-element-value;
  let keyline = keyword-file-element-line;

  local method key (sym :: <symbol>) element(spec, sym, default: #()) end;
  local method single (sym :: <symbol>, #key default = $unsupplied)
    if (sym.key.size = 1)
      sym.key.first.keyval
    elseif (default.unsupplied?)
      tool-error("exactly one %s must be specified", 
		 format-arguments: list(as(<string>, sym)),
		 file: spec-file, 
		 line: if (sym.key.size > 1) sym.key.second.keyline end if);
    elseif (sym.key.size > 1)
      tool-error("no more than one %s may be specified", 
		 format-arguments: list(as(<string>, sym)),
		 file: spec-file, line: sym.key.second.keyline);
    else
      default
    end if;
  end method single;

  let spec-file-modification-date =
	  file-property(spec-file, #"modification-date");
  if (last-run & spec-file-modification-date > last-run)
    // Spec file has changed - we'd better regenerate stuff.
    clean-build? := #t;
  end if;
  let type-library-file = as(<file-locator>, single(type-library:));
  let type-library-modification-date = 
    block ()
      file-property(type-library-file, #"modification-date")
    exception (c :: <file-system-error>)
      tool-error("could not open type library \"%s\"",
      		 format-arguments: list(as(<string>, type-library-file)));
    end block;
  if (last-run & type-library-modification-date > last-run)
    // Type library has changed - we'd better regenerate stuff.
    clean-build? := #t;
  end if;

  with-ole
    let type-library = 
      block ()
	make(<type-library>, file: type-library-file)
      exception (c :: <error>)
	tool-error("reading type library: %s", format-arguments: list(c), 
		   file: type-library-file);
      end block;

    let dest-project = single(project:, default: project-file);
    if (~dest-project)
      tool-error("no destination project specified");
    end if;
    dest-project := as(<file-locator>, dest-project);
    if (dest-project ~= project-file)
      // Project files specified in the spec file should be relative to the
      // spec file.
      dest-project := merge-locators(dest-project, spec-file);
    end if;

    let module-name = single(module:);
    let module-file = as(<file-locator>, single(module-file:));
    let generate = as(<symbol>, single(generate:));
    if (~instance?(generate, <target-types>))
      tool-error("unsupported value given to generate keyword: %s; "
		 "supported values: %s", 
		 format-arguments: list(as(<string>, generate), $target-types), 
		 file: spec-file, line: (generate:).key.first.keyline);
    end if;
    let stub-file = as(<file-locator>, single(stub-file:));
    let modified-projects = #();

    let interfaces-to-translate = element(spec, interfaces:, default: #f);
    interfaces-to-translate :=
      if (interfaces-to-translate)
	let t = make(<string-table>, size: interfaces-to-translate.size);
	for (elem in interfaces-to-translate)
	  t[elem.keyval] := #t;
	end for; t
      end if;

    let server-suffix = single(server-suffix:, default: "");
    let client-suffix = single(client-suffix:, default: "");
    if (server-suffix = "" & client-suffix = "")
      client-suffix := #f;
    elseif (server-suffix = client-suffix)
      tool-error("vtable-server-suffix and vtable-client-suffix keywords "
      		 "have same value: \"%s\"", 
		 format-arguments: list(server-suffix), 
		 file: spec-file, 
		 line: (vtable-client-suffix:).key.first.keyline);
    end if;

    let parameters = make(<motley-parameters>, target-type: generate,
			  to-translate: interfaces-to-translate,
			  server-suffix: server-suffix,
			  client-suffix: client-suffix);

    if (generate-project-from-type-library(type-library, type-library-file,
	    project-file: dest-project, module-name: module-name,
	    module-file: module-file, spec-file: spec-file, 
	    parameters: parameters, stub-file: stub-file, 
	    last-run: last-run, clean-build?: clean-build?))
      modified-projects := add(modified-projects, dest-project);

      // Do we have to add the new project as a subproject?
      if (project-file & dest-project ~= project-file)
	// Manipulate the project file:
	let project = read-project-file(project-file);
	project.project-information-subprojects := 
		add-new!(project.project-information-subprojects, 
			 relative-locator(dest-project, project-file),
			 test: \=);
	write-project-file(project-file, project);
	modified-projects := add(modified-projects, project-file);
      end if;
    end if;
    values(#t, modified-projects)
  end with-ole

end function motley-process-spec;


define function date-as-string (date :: false-or(<date>)) => (r :: <string>)
  if (date)
    date := date + make(<day/time-duration>, days: 0);	// Stupid way to copy
    date.date-time-zone-offset := local-time-zone-offset();
    let (year, month, day, hours, minutes) = decode-date(date);
    format-to-string("%d:%02d %d-%02d-%02d %s", 
		     hours, minutes, year, month, day, local-time-zone-name())
  else
    "<no date>"
  end if
end function date-as-string;

define method generate-project-from-type-library 
	(typelib :: <type-library>, typelib-file :: <locator>, 
	 #key project-file :: <locator>, module-name :: <string>, 
	 module-file :: <locator>, spec-file :: <locator>, 
	 parameters :: <motley-parameters>, stub-file :: <locator>, 
	 last-run :: false-or(<date>), 
	 clean-build? :: <boolean>, force? :: <boolean> = #f) 
     => (project-modified? :: <boolean>)

  module-file := merge-locators(module-file, project-file);
  stub-file := merge-locators(stub-file, project-file);

  let creation-comment = 
	  format-to-string("Creator: created from \"%s\" at %s.",
			   as(<string>, spec-file), 
			   date-as-string(current-date()));
  let doc-comment = format-to-string("");

  let typelib-date = file-property(typelib-file, #"modification-date");
  let any-outdated-targets? :: <boolean> = 
	  clean-build? | ~last-run | ~typelib-date;
  let generated-files = list(module-file, stub-file);
  for (file-loc in generated-files)
    ensure-directories-exist(file-loc);
    let last-mod = file-exists?(file-loc) & 
		   file-property(file-loc, #"modification-date");
    if (~last-mod | (typelib-date & last-mod < typelib-date))
      any-outdated-targets? := #t;
    end if;
    if (~force? & last-mod & (~last-run | last-run < last-mod))
      // Target file is newer than last translation.
      if (~tool-ask-yes-no-question(
	      "The file %s has been modified since %s was last processed - "
	      "do you want to overwrite it?", 
	      as(<string>, relative-locator(file-loc, project-file)),
	      as(<string>, relative-locator(spec-file, project-file))))
	tool-error("processing cancelled by user", file: spec-file);
      end if;
    end if;
  end for;

  // If everything is up to date and we don't have a clean build, go for it:
  if (any-outdated-targets?)

    // Manipulate the project file:
    let project = read-project-file(project-file);
    let project-files = project.project-information-files;
    // Make sure the module file is present, and is the second file in the 
    // list (we assume that the library definition is in the first file):
    let ab-module-file = relative-locator(module-file, project-file);
    if (~member?(ab-module-file, project-files, test: \=))
      project-files := remove!(project-files, ab-module-file, test: \=);
      project-files := replace-subsequence!(project-files, 
					list(ab-module-file), start: 1, end: 1);
    end if;
    // Make sure the stub file is present:
    let ab-stub-file = relative-locator(stub-file, project-file);
    if (~member?(ab-stub-file, project-files, test: \=))
      project-files := remove!(project-files, ab-stub-file, test: \=);
      project-files := replace-subsequence!(project-files, 
				 list(ab-stub-file), start: project-files.size);
    end if;
    project.project-information-files := project-files;
    write-project-file(project-file, project);

    with-open-file (interface = stub-file, direction: #"output")
      format(interface, "Module: %s\n", module-name);
      format(interface, "%s\n%s\n\n", creation-comment, doc-comment);
      let interface = make(<word-wrap-stream>, inner-stream: interface,
			   wrap-offset: 8);
      write-interface(interface, typelib, parameters);
    end with-open-file;

    with-open-file (module = module-file, direction: #"output")
      format(module, "Module: dylan-user\n");
      format(module, "%s\n%s\n\n", creation-comment, doc-comment);
      let module = make(<word-wrap-stream>, inner-stream: module,
			wrap-offset: 8);
      write-module(module, typelib, module-name, parameters);
    end with-open-file;

    #t		// project-modified?
  end if
end method generate-project-from-type-library;
