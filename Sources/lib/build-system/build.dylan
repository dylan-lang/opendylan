Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $default-echo? = #f;
define constant $dylanmakefile  = "dylanmakefile.mkf";
define constant $build-log-file = "build.log";

define variable *libpath* = unsupplied();

// Variables to flag when a Linker is loaded into Build System;
// these must be set in the relevant Linker libraries
// GNU is the default Linker

define constant $microsoft-linker = #"microsoft";
define variable $gnu-linker       = #f;
define variable $ccl-linker       = #f;
define variable $elf-linker       = #f;

define constant $object-file-extension = "obj";
define constant $makefile-extension    = "mkf";
define constant $linkers = #("microsoft", "gnu", "ccl", "elf");

define method choose-linker(arg :: <string>) => (linker)
  select (arg by \=)
    "microsoft" => $microsoft-linker;
    "gnu" => $gnu-linker;
    "ccl" => $ccl-linker;
    "elf" => $elf-linker;
    otherwise => #f;
  end;
end method;

define variable *linker-file-extensions* = #["dll", "exe", "lib", "so"];

define constant $dylan-support = "dylan-support.obj";

/// Allows the default linker in the registry to be overridden (i.e., for C backend)
define variable $override-default-linker = #f;

define settings <linker-settings> (<functional-developer-user-settings>)
  key-name "Linker";
  slot linker-tool :: <symbol>  = select ($os-name)
				    #"win32"  => #"gnu";
				    #"linux"  => #"elf";
				    otherwise => #"gnu";
				  end;
end settings <linker-settings>;

define constant $linker-settings = make(<linker-settings>);

define function default-linker
    () => (linker :: <symbol>)
  $override-default-linker | $linker-settings.linker-tool
end function default-linker;

define function default-linker-setter
    (linker :: <symbol>) => (linker :: <symbol>)
  $linker-settings.linker-tool := linker
end function default-linker-setter;

/// Allows the default DLL policy in the registry to be overriden (i.e., for C backend)
define variable $override-default-dll-policy = #f;

define settings <dll-policy-settings> (<functional-developer-user-settings>)
  key-name "DLL-policy";
  slot linking-policy :: <symbol>  = #"combined-dlls";
end settings <dll-policy-settings>;

define constant $dll-policy-settings = make(<dll-policy-settings>);

define function default-dll-policy
    () => (dll-policy :: <symbol>)
  $override-default-dll-policy | $dll-policy-settings.linking-policy
end function default-dll-policy;

define function default-dll-policy-setter
    (dll-policy :: <symbol>) => (dll-policy :: <symbol>)
  $dll-policy-settings.linking-policy := dll-policy
end function default-dll-policy-setter;

define open class <linker>(<object>)
  constant slot linker-name :: <string>, required-init-keyword: name:;
  constant slot fake-imports :: <string-table> = make(<string-table>);
end class;

define thread variable *linker* = #f;

// The Build Data Structure

define primary class <build>(<object>)
  constant slot dylanlib :: <symbol>, init-keyword: dylanlib:;
  constant slot dylanapp :: false-or(<string>), init-keyword: dylanapp:;
  constant slot libfile :: <file-locator>, init-keyword: libfile:;
  constant slot build-directory :: false-or(<directory-locator>), init-keyword: directory:;
  constant slot linker :: <linker>, init-keyword: linker:;
  slot targets :: <vector> = #[];
  slot more-targets? :: <boolean> = #f;
  slot target :: <integer> = 0;
  constant slot base-address :: <string>, init-keyword: base:;
  constant slot linkopts :: <list>, init-keyword: linkopts:;
  constant slot objs :: <list>, init-keyword: objs:;
  constant slot libs :: <table>, init-keyword: libs:;
  constant slot c-headers :: <list>, init-keyword: c-headers:;
  constant slot c-objs :: <list>, init-keyword: c-objs:;
  constant slot c-src-objs :: <list>, init-keyword: c-src-objs:;
  constant slot rc-objs :: <list>, init-keyword: rc-objs:;
  constant slot c-libs :: <list>, init-keyword: c-libs:;
  constant slot rtlibs :: <list>, init-keyword: rtlibs:;
  constant slot dll :: <file-locator>, init-keyword: dll:;
  constant slot app :: <file-locator>, init-keyword: app:;
  slot indentation :: <string>, init-keyword: indentation:;
  slot dylanecho? :: <boolean> = $default-echo?, init-keyword: echo:;
  slot force? :: <boolean>, init-keyword: force:;
  slot link? :: <boolean>, init-keyword: link:;
  slot backup? :: <boolean>, init-keyword: backup:;
  slot restore? :: <boolean>, init-keyword: restore:;
  slot exports-only? :: <boolean>, init-keyword: exports-only:;
  slot top-level? :: <boolean>;
  slot interactive? :: <boolean> = #f;
  constant slot format-function :: false-or(<function>), init-keyword: format:;
  constant slot logfile :: false-or(<stream>), init-keyword: log:;
  constant slot image-version :: <string>, init-keyword: version:;
  slot build-phase :: <integer> = 0;
  slot build-phases :: <integer> = 0;
  constant slot system-build? :: <boolean> = #f, init-keyword: system?:;
  slot build-up-to-date? = unsupplied();
end class;

define method library-file-name
    (library :: <symbol>) => (file-name :: <string>)
  as-lowercase(as(<string>, library))
end method library-file-name;

define method library-filename-with-extension
    (library :: <symbol>, extension :: false-or(<string>))
 => (file-name :: <file-locator>)
  filename-with-extension(library-file-name(library), extension)
end method library-filename-with-extension;

define method library-filename-with-extensions
    (library :: <symbol>, suffix :: <string>, extension :: false-or(<string>))
 => (file-name :: <file-locator>)
  filename-with-extension(concatenate(library-file-name(library), suffix), extension)
end method library-filename-with-extensions;


ignore(c-headers); // TODO: WHY IS THIS UNREFERENCED?


define thread variable *build* = #f;


define class <build-error>(<condition>)
end class;

define class <build-warning>(<condition>)
end class;

define method dummy-implementation(#rest args) #f end;

define thread variable *unify-all?* :: <boolean> = #f;
define thread variable *exports* :: <list> = #();

define variable *project-build-info* :: <function> = dummy-implementation;

// Progress Callback notifications for builds

define class <build-notifier>(<object>)
end class;

define constant $stage-notifier = make(<build-notifier>);
define constant $phase-notifier = make(<build-notifier>);
define constant $error-notifier = make(<build-notifier>);
define constant $warning-notifier = make(<build-notifier>);

// Wrap this around anything which constitutes a single "build
// transaction".
define macro with-used-project-cache
  { with-used-project-cache ?:body end }
    => { do-with-used-project-cache(method () ?body end) }
end macro;

// A cache used to avoid checking for dependencies of projects multiple times
// for multiply-used libraries during project building
define thread variable *used-project-cache* = #f;

define thread variable *build-cache* = #f;
define thread variable *builds-cache* :: false-or(<object-table>) = #f;

define function do-with-used-project-cache (fn)
  if (*used-project-cache*)
    fn()
  else
    dynamic-bind (*used-project-cache* = make(<object-table>),
		  *builds-cache*       = make(<object-table>),
		  *unify-all?*         = #f,
		  *exports*            = #())
      fn()
    end
  end if
end function;

// Toplevel function invoked from Shell

define method build
    (#key linker = default-linker()) => ()
  configure-build-system();
  block()

    let arguments = application-arguments();

    let (arguments, linker) =
      if (~ arguments.empty? & member?(arguments.first, $linkers, test: \=))
	values(copy-sequence(arguments, start: 1),
	       choose-linker(arguments.first))
      else
	values(arguments, linker)
      end if;
    if ((arguments.size = 1) & simple-target?(arguments[0]))
      *build* := make(<build>);
      *build*.top-level? := #t;
      *build*.targets := arguments;
      do-targets();
    else
      
      let directory :: <directory-locator>
	= if ($personal-build)
	    let key = -1;
	    let library
	      = any?(method (argument :: <string>)
		       key := key + 1;
		       //---*** andrewa: This isn't portable
		       let extension-pos = subsequence-position(argument, ".");
		       let extension
			 = if (extension-pos)
			     copy-sequence(argument, start: extension-pos + 1)
			   end if;
		       if (extension
			     & member?(extension, *linker-file-extensions*, test: \=))
			 arguments[key] := extension;
			 copy-sequence(argument, end: extension-pos)
		       end if
		     end method,
		     arguments);
	    library & subdirectory-locator($personal-build, library)
	  end
	    | working-directory();

      build-system(arguments,
		   directory: directory,
		   linker: linker,
		   configure?: #f);

    end if;

  exception(condition :: <serious-condition>)
    format-out("\n%s\n", condition)
  end block;
end method;

// Toplevel internal function that can be invoked by Dylan Clients

define method build-system
    (build-targets :: <sequence>,
     #key toplevel? = #t,
          directory :: <directory-locator> = working-directory(),
          progress-callback,
	  linker = default-linker(),
	  project-build-info,
	  configure? = #t)
 => (build-successful? :: <boolean>)
  if (configure?)
    configure-build-system();
  end;

  let logfile = #f;
  block()
    with-build-directory (directory)
      set-lib-installation();
      set-dll-installation();
      set-release-installation();
      with-used-project-cache
	let main =
	  method(#key log)
	      if (project-build-info)
		*project-build-info* := project-build-info
	      end if;
	      build-system-internal(build-targets,
				    toplevel?: toplevel?,
				    directory: directory,
				    progress-callback: progress-callback,
				    linker: linker,
				    log: log);
	  end method;

        if (progress-callback)
	  with-open-file (stream = file-in-directory(directory, $build-log-file),
			  direction: #"output")
	    logfile := stream;
	    main(log: stream)
	  end with-open-file;
        else
	  main()
        end if;
      end;
    end;
    #t
  exception(<build-error>)
    if (progress-callback)
      format-out("\nBuild Error encountered; Displaying logfile...\n");
      if (stream-open?(logfile)) close(logfile) end if;
      let file = file-in-directory(directory, $build-log-file);
      run-application(format-to-string("%s %s", 
				       select ($os-name)
					 #"win32"  => "type";
					 #"linux"  => "cat";
					 otherwise => "type";
				       end,
				       as(<string>, shorten-pathname(file))),
		      under-shell?: #t,
		      inherit-console?: #t);
    end if;
    #f
  exception(<build-warning>)
    #t
  end block;

end method;

define method build-system-internal
    (build-targets :: <sequence>,
     #key toplevel? = #t,
          directory,
          linker,
          progress-callback,
	  log,
	  library :: false-or(<symbol>) = #f,
	  system?)
 => ()

  with-build-directory(directory)
    dynamic-bind(*linker* = make-linker(linker | as(<symbol>, *linker*.linker-name)),
		 *build* = read-buildfile(toplevel?: toplevel?,
					  directory: directory,
					  progress-callback: progress-callback,
					  log: log, library: library,
					  dylanmakefile: 
					    if (system?)
					      make(<file-locator>,
						   directory: $system-lib,
						   base:      library-file-name(library),
						   extension: $makefile-extension)
					    end if,
					  system?: system?))

      set-linker-path();

      echo?("Current Directory is %s", directory | "system");

      if (linker) initialize-linker() end if;
      *build*.top-level? := toplevel?;
      *build*.targets := build-targets;
      do-targets();
    end dynamic-bind;
  end;

end method;

// Read the makefile and set up the <build> Data Structure

define open generic object-filename-from-base-filename 
    (linker :: <linker>, name :: type-union(<string>, <file-locator>))
 => (object-filename :: <file-locator>);

//---*** andrewa: can we remove this?
define method object-filename-from-base-filename
    (linker :: <linker>, name :: <string>)
 => (object-filename :: <file-locator>)
  object-filename-from-base-filename(linker, as(<file-locator>, name))
end method object-filename-from-base-filename;

define method object-filename-from-base-filename
    (linker :: <linker>, name :: <file-locator>)
 => (object-filename :: <file-locator>)
  new-filename-extension(name, $object-file-extension)
end method object-filename-from-base-filename;

define method read-buildfile(#key toplevel? = #t,
			          directory :: false-or(<directory-locator>) = #f,
			          progress-callback,
			          log,
			          library :: false-or(<symbol>) = #f,
			          dylanmakefile,
			          system?) => (build :: <build>)
  let build? = library & element(*builds-cache*, library, default: #f);
  let build = build?;
  if (build)
    echo?("read-buildfile of %= avoided", library);
    build.target := 0;
    build.more-targets? := #f;
    build
  else

    let dylanmakefile :: <file-locator> = dylanmakefile | as(<file-locator>, $dylanmakefile);

    if (~file-exists?(dylanmakefile))
      let error =
	if (toplevel?) error
	else build-error
	end if;
      error("%s not found in directory %s",
	    dylanmakefile, directory | $system-lib);
    end if;

    let variables = read-file-header(dylanmakefile);
    let dylan-lib = as(<symbol>, first(element(variables, #"library")));
    let dylan-app :: <string>
      = as-lowercase
          (first(element(variables, linker-executable-keyword(*linker*),
			 default: list(library-file-name(dylan-lib)))));
    let dylan-app-locator = as(<file-locator>, dylan-app);

    //This will give us build locations of used projects
    let libs  = make(<object-table>);
    let build-list = element(variables, #"used-projects", default: #());

    unless (empty?(build-list)) // this shouldn't be necessary
      for(i from 0 below size(build-list) by 3)
	let build-location = build-list[i + 2];
	libs[as(<symbol>, build-list[i])]
	  := if (build-location = "system")
	       "system"
	     else
	       as(<directory-locator>, build-location);
	     end;
      end;
    end unless;

    let objects
      = pair(object-filename-from-base-filename(*linker*, "_glue"),
	     map(curry(object-filename-from-base-filename, *linker*), 
		 element(variables, #"files", default: #())));

    let base-number  = string-to-machine-word(first(element(variables, #"base-address", default: #("0x669E0000"))));
    let base = machine-word-to-string(base-number, prefix: "0x");

    let major-version
      = string-to-integer(first(element(variables, #"major-version", default: #("0"))), 
			  default: 0);
    let minor-version
      = string-to-integer(first(element(variables, #"minor-version", default: #("0"))),
			  default: 0);
    let image-version = format-to-string("%d.%d", major-version, minor-version);

    make(<build>,
	 linker:       *linker*,
	 dylanapp:     dylan-app,
	 dylanlib:     dylan-lib,
	 objs:         objects,
	 libs:         libs,
	 base:         base,
	 linkopts:     map(curry(linker-library-name, *linker*),
			   parse-line(element(variables, #"linker-options", default: #()))),
	 c-headers:    element(variables, #"c-header-files", default: #()),
	 c-objs:       element(variables, #"c-object-files", default: #()),
	 c-src-objs:   map(curry(object-filename-from-base-filename, *linker*),
			   element(variables, #"c-source-files", default: #())),
	 rc-objs:      map(rcurry(new-filename-extension, "res"),
			   element(variables, #"rc-files", default: #())),
	 c-libs:       map(curry(linker-library-name, *linker*),
			   parse-line(element(variables, #"all-c-libraries", default: #()))),
	 rtlibs:       element(variables, #"rtlibs", default: #()),
	 libfile:      filename-with-extension(dylan-app-locator,
					       exports-file-extension(*linker*)),
	 dll:          filename-with-extension(dylan-app-locator,
					       dll-file-extension(*linker*)),
	 app:          filename-with-extension(dylan-app-locator,
					       exe-file-extension(*linker*)),
	 indentation:  if (toplevel?) "  "
		       else concatenate("  ", *build*.indentation)
		       end if,
	 format:       (*build* & *build*.format-function) | progress-callback,
	 log:          (*build* & *build*.logfile) | log,
	 version:      image-version,
	 force:        *build* & *build*.force?,
	 link:         *build* & *build*.link?,
	 backup:       *build* & *build*.backup?,
	 restore:      *build* & *build*.restore?,
	 exports-only: *build* & *build*.exports-only?,
	 echo:         *build* & *build*.dylanecho?,
	 directory:    directory & shorten-pathname(working-directory()),
	 system?:      system?)

  end if;
end method;


// Parses a line of text into a sequence of Data tokens for Builds

define method parse-line(entry :: <list>) => (entries :: <list>)
  reduce(concatenate,
	 #(),
         map(parse-line, entry));
end method;

define method parse-line(original-line :: <string>) => (entries :: <list>)
    local method skip-whitespaces(line :: <string>, start-position :: <integer>)
	   => (new-start-position :: <integer>)
	    let whitespaces = 0;
	    let ch = element(line, start-position, default: #f);

	    while(ch = ' ')
	      whitespaces := whitespaces + 1;
	      ch := element(line, start-position + whitespaces, default: #f);
	    end while;
	    start-position + whitespaces
	  end method;
  local method add-entry(entries, token)
	  if (token.empty?)
	    entries
	  else
	    add(entries, token)
	  end if;
	end method;

  if (original-line.empty?)
    #()
  else
    let start-position = skip-whitespaces(original-line, 0);
    let line = copy-sequence(original-line, start: start-position);
    let result = #();
    while (member?(' ', line))
      let start-position = subsequence-position(line, " ");
      let new-start-position = skip-whitespaces(line, start-position);

      result := add-entry(result, copy-sequence(line, start: 0, end: start-position));
      line := copy-sequence(line, start: new-start-position);
    end while;

    add-entry(result, line).reverse
  end if;
end method;


// Interactive Mode
//   Eliminates Startup Overhead
//   Simulates a Shell                

define class <exit-interactive>(<condition>)
end class;

define variable *command-line* = "";

define method interactive-build() => ()
  unless (*build*.interactive?)
    block()
      install-interactive-targets();
      while (#t)
	echo-prompt("\nBUILD %s ? ", *build*.dylanapp);

	*command-line* := as(<byte-string>, read-line(*standard-input*));
	let tokens = parse-line(*command-line*);

	unless (tokens.empty?)
	  *build*.targets := as(<vector>, tokens);
	  *build*.target := 0;
	  *build*.more-targets? := #f;
	  *build*.dylanecho? := $default-echo?;
	  *build*.force? := #f;
	  *build*.link? := #f;
	  *build*.backup? := #f;
	  *build*.restore? := #f;
	  *build*.exports-only? := #f;
	  *build*.top-level? := #t;
	  *build*.indentation := "  ";
	  *build*.interactive? := #t;

	  block()
	    do-targets();
	  exception(<no-more-targets>) #f;
	  exception(<build-error>) #f;
	  exception(condition :: <serious-condition>)
	    format-out("%s", condition);
	  end block;
	end unless;
      end while;
    exception(<exit-interactive>)
      signal(make(<no-more-targets>));
    end block;
  end unless
end method;


// Make and initialize a Linker
// The Build System's behaviour is thereafter specialized for Linker

define open generic make-linker(linker) => (linker :: <linker>);

define method make-linker(linker == $microsoft-linker) => (linker :: <linker>)
  make(<linker>, name: "microsoft");
end method;

define open generic check-linker-installation(linker :: <linker>) => ();

define method check-linker-installation(linker :: <linker>) => ()
end method;

define method initialize-linker() => ()
  read-linker();
  check-linker-installation(*linker*);
  let name = *linker*.linker-name;
  let pretty-name
    = select (name by \=)
	"microsoft", "Microsoft" => "Microsoft";
	"gnu", "Gnu", "GNU" => "GNU";
	"ccl", "CCL" => "C Compiler &";
	"elf", "Elf", "ELF" => "ELF";
	otherwise => name;
      end;
  let policy = default-dll-policy();
  let policy-pretty-name
    = select (policy)
	#"separate-dlls" => "separate libraries";
	#"combined-runtime-dlls" => "combined runtime libraries";
	#"combined-dlls" => "combined runtime and project libraries";
	otherwise => as(<string>, policy);
      end;
  echo(#f, "Using the %s Linker with %s", pretty-name, policy-pretty-name);
end method;

define open generic linker-library-name(linker :: <linker>, name :: <string>)
 => (linker-name :: <string>);

define method linker-library-name(linker :: <linker>, name :: <string>)
 => (library-name :: <string>)
  name
end method;

define open generic linker-resource-object-file(linker :: <linker>, file :: <file-locator>)
 => (linker-resource-object :: <file-locator>);

define method linker-resource-object-file(linker :: <linker>, name :: <file-locator>)
 => (linker-resource-object :: <file-locator>)
  name
end method;

// Some Linkers, e.g. ELF, have their own version naming conventions
// so have stubbed out binary renaming for them

define open generic linker-executable-keyword(linker :: <linker>)
 => (executable-keyword :: <symbol>);

define method linker-executable-keyword(linker :: <linker>)
 => (executable-keyword :: <symbol>)
  #"executable"
end method;

define open generic linker-library-path-name(linker :: <linker>)
 => (library-path :: <string>);

define method linker-library-path-name(linker :: <linker>)
 => (library-path :: <string>)
  "LIB";
end method;

define open generic linker-path-separator(linker :: <linker>)
 => (separator :: <string>);

define method linker-path-separator(linker :: <linker>)
 => (separator :: <string>)
  ";";
end method;

define method set-linker-path() => ()
  let path-name = linker-library-path-name(*linker*);
  let path-separator = linker-path-separator(*linker*);
  if (*libpath*.unsupplied?)
    *libpath* := environment-variable(path-name);
  end if;
  environment-variable(path-name) :=
    concatenate(($personal-lib & as(<string>, $personal-lib))
		  | as(<string>, *lib-installation*),
		path-separator,
		as(<string>, $system-lib),
		path-separator,
		*libpath* | "");
  echo?("Using linker path %s", environment-variable(path-name));
end method;

define method merged-project-name
    (library :: <symbol>) => (merged-library :: <symbol>)
  let info = find-library-info(library);
  let merge-parent = info & info.info-merge-parent;
  if (merge-parent)
    merge-parent.info-name
  else
    library
  end
end method merged-project-name;

define method merged-project-libraries
    (library :: <symbol>)
 => (parent :: <symbol>, libraries :: <sequence>)
  let library-info = find-library-info(library);
  let parent-info = if (library-info) library-info.info-merge-parent | library-info end;
  let parent-binary-info = parent-info & parent-info.info-binary;
  let parent = if (parent-info) parent-info.info-name else library end;
  values(parent,
	 if (parent-binary-info)
	   let libraries = map(info-name, parent-binary-info.info-merged-libraries);
	   echo?("Merged libraries for %s (parent %=) ==> %=",
		 library,
		 parent-info & parent-info.info-name,
		 libraries);
	   libraries
	 else
	   echo?("No merged libraries found for %s (library %=, parent %=)",
		 library,
		 library-info & library-info.info-name,
		 parent-info & parent-info.info-name);
	   #[]
	 end)
end method merged-project-libraries;
