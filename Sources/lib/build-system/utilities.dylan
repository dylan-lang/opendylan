Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Drive a Remote Application from Dylan

define method execute-shell-command
    (suppress-echo? :: <boolean>, under-shell? :: <boolean>, #rest args) => ()
  let command-line = apply(make-command-line, args);

  if (*build*.dylanecho?)
    echo(#f, format-to-string("Running Application: %s", command-line))
  end if;

  if (*build*.logfile)
    format(*build*.logfile, "\nRunning Application: %s\n", command-line);
    force-output(*build*.logfile);
  end if;

  unless (suppress-echo?)
    echo-to-shell("");
  end unless;

  let build-logfile = *build*.logfile;
  let ignore-return-codes? =
    if (command-line[0] = '!')
      command-line[0] := ' ';
      #t
    end if;
  let status = 
    if (build-logfile)
      let status =
      run-application(command-line,
		      under-shell?: #f,
		      inherit-console?: #f,
		      outputter:
			method(msg :: <byte-string>, #key end: _end)
			    write(build-logfile, msg, end: _end);
			end,
		      minimize?: #t,
		      activate?: #f);
      force-output(build-logfile);
      status
    else
      run-application(command-line,
		      under-shell?: under-shell?,
		      inherit-console?: #t);
    end if;

  unless (ignore-return-codes?)

  if (application-error?(status))
    // Interpret Win32 System Error Codes
    let error-text =
      select (status)
	2 => 
	  let command = tokenize-command-string(command-line);
	  format-to-string("The application %= could not be found", command);
	otherwise =>
	  format-to-string("In Running Application %=", command-line);
      end select;
    build-error(error-text);
  end if;

  end unless;

end method;

// HACK to get around MicroSoft Linker warnings

define method application-error?(status :: <integer>) => (error? :: <boolean>)
  if (*build*.linker.linker-name = "microsoft")
    status > 1
  else
    status ~= 0
  end if;
end method;

// Insert indentations between component tokens of command-lines

define method make-command-line
    (#rest args) => (line :: <string>)
  local method assemble(line, arg)
	  select (arg by instance?)
	    <string>  => if (line.empty?) arg else concatenate(line, " ", arg) end;
	    <symbol>  =>
	      let string = as(<string>, arg);
	      if (line.empty?) string else concatenate(line, " ", string) end;
	    <locator> => 
	      let string = as(<string>, arg);
	      if (line.empty?) string else concatenate(line, " ", string) end;
	    <sequence> =>
	      reduce(assemble, line, arg);
	  end select;
	end method;
  reduce(assemble, "", args)
end method;

// convenient macro to continuously read and process stream input until eof

define macro with-stream-input
  { with-stream-input () ?:body end }
    => { 
	block()
	  while (#t)
	    ?body
	  end while;
	exception(<end-of-stream-error>)
	end block;
       }
end macro;


// Emit link-entries for target unified build to link-script or command-line.
// Each object is either emitted to stream or added to a list depending on
// what the specializable target object is.
// DLL Unification on Win32 will use the link stream emitter because of command
// line limits, and Microsoft linker's convenient use of link scripts.
// For unification on Linux, however, the command-line can be used directly so
// just accumulate all the objects in a <list> for later direct insertion in
// linker command-line. 

define open generic link-stream-class
    (linker :: <linker>) => (class :: <class>);

define method link-stream-class(linker :: <linker>) => (class :: <class>)
  <file-stream>
end method;

define generic open-link-stream
    (class :: <class>, build :: <build>, #rest keywords) => (stream);

define method open-link-stream
    (class :: subclass(<stream>), build :: <build>, #rest keywords) => (stream :: <stream>)
  let file :: <file-locator>
    = file-in-directory(working-directory(),
			format-to-string("unify-%s.link", build.dylanapp));
  apply(make, class, locator: file, keywords)
end method open-link-stream;

define method open-link-stream
    (class == <list>, build :: <build>, #rest keywords) => (stream :: <list>)
  *link-stream* := #()
end method open-link-stream;

define generic close-link-stream(stream) => ();

define method close-link-stream(stream :: <stream>) => ()
  if (stream & stream-open?(stream)) close(stream) end;
end method close-link-stream;

define thread variable *link-stream* = #f;

define method close-link-stream(stream) => ()
  *link-stream* := stream;
end method close-link-stream;

define macro with-link-stream
  { with-link-stream (?stream:variable = ?build:expression,
		      #rest ?keys:expression)
      ?body:body
    end }
  => { begin
	 let ?stream = #f;
	 block ()
	   ?stream := open-link-stream(link-stream-class(*linker*), ?build, ?keys);
	   ?body
	 cleanup
	   close-link-stream(?stream);
	 end
       end }
end macro with-link-stream;

define macro apply!
  { apply!(?function:name, ?stream:name, ?args:*) }
    =>
    { ?stream := apply(?function, ?stream, vector(?args)) }
end macro;


// Read and execute a linker-specific, multi-line batch script and perform
// environment-variable substitution

define method execute-shell-commands
    (suppress-echo? :: <boolean>, under-shell? :: <boolean>, file :: <string>, #rest variables) => ()
  local method process-variables(variables)
	  let new-variables = make(<string-table>);
          let size-vars = variables.size;
	  for (count from 0 below size-vars by 2)
            new-variables[variables[count]] :=
	      make-command-line(variables[count + 1]);
	  end for;
	  new-variables
	end method;

  let command-lines = #();

  command-lines := build-environment-variable(file);

  let variables = process-variables(variables);

  for (command-line in command-lines)
    let command-line =
      substitute-environment-variables(command-line, supplied-values?: variables);
    execute-shell-command(suppress-echo?, under-shell?, command-line);
  end for;

end method;

// Search for a linker script in executable-location:personal-bin:system-bin

define method build-system-linker-file(build-file :: <string>,
				       #key error? = #t) => (build-file :: false-or(<string>))
  let personal-file = *dll-installation* & file-in-directory(*dll-installation*, build-file);
  let system-file = file-in-directory($system-bin, build-file);
  let release-file
    = file-in-directory($executable-name.locator-directory, build-file);

  case
    file-exists?(release-file) => as(<string>, release-file);
    personal-file & file-exists?(personal-file) => as(<string>, personal-file);
    file-exists?(system-file) => as(<string>, system-file);
    otherwise =>
      if (error?)
	build-error("Required Build System Linker File %s does not appear in any Installation Area",
		    build-file);
      end if;
  end case;
end method;

// Display an internal message from Builds in Client Window

define method echo(notifier, format-string :: <string>, #rest args) => ()

  if (*build*.format-function)
    // external formatting
    let message = apply(format-to-string, format-string, args);

    let build-logfile = *build*.logfile;
    if (build-logfile)
      format(build-logfile, "\n%s\n", message);
      force-output(build-logfile);
    end if;

    select (notifier)
      $stage-notifier =>
	*build*.format-function(message, phases: *build*.build-phases);
      $phase-notifier =>
	*build*.format-function(message, phase: *build*.build-phase);
      $error-notifier =>
	*build*.format-function(message, error?: #t);
      $warning-notifier =>
	*build*.format-function(message, warning?: #t);
      otherwise =>
	*build*.format-function(message)
    end select;
  else
    // default formatting to console stream
    format-out("- %s", *build*.indentation);
    case
      format-string.empty? => #f;
      format-string = "\n" =>
        write(*standard-output*, format-string);
      args.empty? =>
	write(*standard-output*, concatenate(format-string, "\n"));
      otherwise =>
	apply(format-out, concatenate(format-string, "\n"), args);
    end case;
    force-output(*standard-output*)
  end if;

end method;

define method echo-to-shell(format-string :: <string>, #rest args) => ()
  unless (*build*.format-function)
    apply(echo, #f, format-string, args)
  end unless;
end method;

define method echo-to-client(notifier, format-string :: <string>, #rest args) => ()
  if (*build*.format-function)
    apply(echo, notifier, format-string, args)
  end if;
end method;

// Extra step-by-step Debugging Information

define method echo?(format-string :: <string>, #rest args) => ()

  if (*build* & *build*.dylanecho?)
    apply(echo, #f, format-string, args);
  end if;
  // apply(debug-message, format-string, args);

end method;

// Prompt for Interactive Mode

define method echo-prompt(format-string :: <string>, #rest args) => ()

  apply(format-out, format-string, args);
  force-output(*standard-output*)

end method;


// Build Error Signaling

define method build-error(format-string :: <string>, #rest args) => ()
  if (*build* & *build*.format-function)
    apply(echo,
	  $error-notifier,
	  concatenate("Build Error: ", format-string),
	  args);
  else
    format-out("- %s", build-indentation());
    format-out("Build Error: ");
    apply(format-out, format-string, args);
    if (*build* & *build*.interactive?)
      format-out("\n- %sExiting Target", build-indentation());
    else
      format-out("\n- %sExiting Build\n", build-indentation());
    end if;
    force-output(*standard-output*);
  end if;
  if (*build*)
    signal(make(<build-error>));
  else
    error("Build Error caught during Initialization");
  end if;
end method;

define method build-warning(format-string :: <string>, #rest args) => ()
  if (*build* & *build*.format-function)
    apply(echo,
	  $warning-notifier,
	  concatenate("Build Warning: ", format-string),
	  args);
  else
    format-out("- %s", build-indentation());
    format-out("Build Warning: ");
    apply(format-out, format-string, args);
    force-output(*standard-output*);
  end if;
  signal(make(<build-warning>));
end method;

define method build-indentation()
  if (*build*)
    *build*.indentation
  else
    ""
  end if;
end method;

define macro with-build-error
  { with-build-error () ?:body end }
    => { 
	block()
	  ?body
	exception(c :: <simple-condition>)
	  apply(build-error,
		c.condition-format-string,
		c.condition-format-arguments);
	end block;
       }
end macro;


// Search for a build file in personal:system roots

define method search-for-file
    (file :: <file-locator>, #key error? = #t)
 => (lib-file :: false-or(<file-locator>))
  let file-name = file.locator-name;
  let personal-file = file-in-directory(*lib-installation*, file-name);
  let system-file = file-in-directory($system-lib, file-name);

  case
    file-exists?(personal-file) => personal-file;
    file-exists?(system-file)   => system-file;
    otherwise =>
      if (error?)
	build-error("Required file %s does not appear in any Installation Area"
		      "Looked in:  %s  %s",
		    file-name,
		    *lib-installation*,
		    $system-lib);
      end if;
  end case;
end method;

define method search-for-build-directory
    (file :: <string>, library :: <symbol>)
 => (build-directory :: false-or(<directory-locator>), system? :: <boolean>, error? :: <boolean>)
  let library-directory-name = library-file-name(library);
  let personal-area = $personal-build & subdirectory-locator($personal-build, library-directory-name);
  let personal-file = personal-area & file-in-directory(personal-area, file);
  let system-area = subdirectory-locator($system-build, library-directory-name);
  let system-file = file-in-directory(system-area, file);

  case
    personal-file & file-exists?(personal-file) => 
      values(personal-area, #f, #f);
    file-exists?(system-file) =>
      values(system-area, #t, #f);
    otherwise => 
      values(#f, #f, #t);
  end case;
end method;


// Changing Build Directories


define method change-directory(directory :: <directory-locator>)
  if (~ file-exists?(directory))
    build-error("Invalid Directory %s",
		directory);
  else
    working-directory() := directory;
  end if;
end method;

define macro with-build-directory
  { with-build-directory (?directory:expression) ?:body end }
    => { 
	let directory = ?directory;
	let previous-directory = if (directory) working-directory(); end;
	// Conditional unwind-protect?
	block()
	  if (directory) change-directory(directory); end;
	  ?body
	cleanup
	  if (previous-directory) change-directory(previous-directory); end if;
        /* Add this if you want builds to only bomb out of one recursive level
           rather than the whole thing
	exception(<build-error>)
	  change-directory(previous-directory);
         */
	end block;
       }
end macro;


// Build File Copying and Deletion

define method copy-build-file(source :: <file-locator>, destination :: <file-locator>,
			      #rest args) => ()
  with-build-error()
    apply(copy-file, source, destination, args);
  end;
end method;

define method delete-build-file(file :: <file-locator>) => ()
  with-build-error()
    delete-file(file);
  end;
end method;

define method process-script(file :: <string>, process :: <function>) => ()
  local method read-next-line(stream) => (line :: <string>)
	  let line = read-line(stream);
	  while((line ~= "") & (line[0] = '#'))
	    line := read-line(stream);
	  end while;
	  line
	end method;
  let locator = as(<file-locator>, file);

  with-open-file(stream = locator)
    with-stream-input()
      let variable = "";
      while (variable = "")
	variable := read-next-line(stream);
      end while;

      let command-lines = #();
      let command-line = read-next-line(stream);
      while (command-line ~= "")
	command-lines := add(command-lines, command-line);
	command-line := read-next-line(stream);
      end while;

      process(variable, reverse(command-lines));
    end with-stream-input;
  end with-open-file;
end method;



/// DLL Unification Support


define constant $glue-imports = "_glue.import";

define constant $fixup-imports = "_imports.import";

define variable $dylan-executable-name :: false-or(<string>) = #f;

define method read-build-imports
    (build :: <build>, glue-imports :: false-or(<string-table>),
     #key directory :: false-or(<directory-locator>) = #f,
          imported-libraries :: false-or(<string-table>) = #f)
 => ()
  let library :: <symbol> = build.dylanlib;

  unless (library.dylan-library?) // No imports for dylan library

    let objs = build.objs;

    for (obj in objs)
      let import-file = new-filename-extension(obj, "import");
      let full-import-file = file-in-directory(directory, import-file.locator-name);
      let glue-imports? = (import-file = $glue-imports) & glue-imports;

      read-imports-in-file(full-import-file,
			   glue-imports?: glue-imports?,
			   imported-libraries: imported-libraries);
    end for;

  end unless;
end method;

define method read-imports-in-file
    (import-file :: <file-locator>,
     #key glue-imports?, 
          imported-libraries :: false-or(<string-table>))
 => ()
  let glue-imports = glue-imports?;

  with-open-file (stream = import-file)
    with-stream-input()
      let export? :: <boolean> = #f;
      let executable :: <string> = as-lowercase(read-line(stream));
      if (executable.dylan-library?)
	executable := as-lowercase(read-line(stream));
	$dylan-executable-name := executable
      end;
      if (imported-libraries)
	// This will change when core-dylan is unified
	unless (executable.runtime-library?)
	  let new-executable :: false-or(<string>)
	    = element(imported-libraries, executable, default: #f);
	  if (new-executable)
	    executable := new-executable
	  else
	    export? := #t
	  end
	end
      end;

      let linker-imports :: false-or(<string-table>)
	= build-linker-imports(*linker*, export?);

      if (linker-imports)
	echo?("Processing imports in %= for %=", import-file, executable);

	let linker-imports :: <string-table> = linker-imports;
	let entries
	  = element(linker-imports, executable, default: #f)
	      | make(<string-table>);

	let import = read-line(stream);
	while (import ~= "")
	  entries[import] := #t;
	  if (glue-imports? & ~ export?)
	    glue-imports[import] := #t;
	  end if;
	  import := read-line(stream);
	end while;

	linker-imports[executable] := entries;

      else

	let import = read-line(stream);
	while (import ~= "")
	  import := read-line(stream);
	end while;

      end if;
    end with-stream-input;
  end with-open-file;
end method;

define open generic build-linker-imports
    (linker :: <linker>, export? :: <boolean>)
 => (linker-imports :: false-or(<string-table>));

define method build-linker-imports
    (linker :: <linker>, export? :: <boolean>)
 => (linker-imports :: false-or(<string-table>))
  if (export?) *linker*.fake-imports
  else #f
  end if;
end method;


// Create indirections for all the "fake" imports as a result
// of DLL unification

define method write-fake-imports(build :: <build>) => ()
  let library :: <symbol> = build.dylanlib;
  let import-file = library-filename-with-extensions(library, "imp", "o");
  let stream = #f;

  block()
    echo?("Writing fake imports for %s to %s", library, import-file);
    echo?("  Fake import libraries: %=", *linker*.fake-imports.key-sequence);
    stream := make(<byte-file-stream>, locator: import-file, direction: #"output");
    let builder :: <coff-builder>
      = make-binary-builder(<coff-builder>, machine: #x14c, big-endian?: #f, destination: stream);

    for (executable :: <string> in *linker*.fake-imports.key-sequence)
      let linker-imports = *linker*.fake-imports[executable].key-sequence;

      for (import :: <string> in linker-imports)
	let import-indirection = concatenate("__imp_", import);

	define-external-symbol(builder, import, unsupplied());
	define-public-symbol(builder, import-indirection, unsupplied());

	select-dylan-section(builder, #"variables", 1);
	add-symbol-definition(builder, import-indirection, unsupplied());

	add-data(builder, import, unsupplied());
      end for;

    end for;

    fixup-coff-builder(builder);
    write-binary(builder.destination, builder)
  cleanup
    if (stream & stream-open?(stream)) close(stream) end;
  end block;

end method;


// Unified COFF generation of .dyimp Dylan Binary sections;
// These sections are used in dynamically linking derived
// Dylan implementation objects

define method open-import-fixups-outputter (build :: <build>)
 => (builder :: <coff-builder>, seen :: <string-table>)

  let library :: <symbol> = build.dylanlib;
  let import-file = library-filename-with-extensions(library, "imp0", "o");
  let stream = make(<byte-file-stream>, locator: import-file, direction: #"output");
  let builder :: <coff-builder>
    = make-binary-builder(<coff-builder>, machine: #x14c, big-endian?: #f, destination: stream);

  builder.dynamic-linking? := #t;

  values(builder, make(<string-table>))

end method;

define method write-import-fixups
    (builder :: <coff-builder>, seen :: <string-table>,
     directory :: <directory-locator>, first? :: <boolean>) => ()
  let import-location = file-in-directory(directory, $fixup-imports);

  with-open-file (stream = import-location)

    let name :: <byte-string> = read-line(stream);
    while (name ~= "")
      let f-name :: <byte-string> = read-line(stream);
      let offset :: <integer> = string-to-integer(read-line(stream));
      
      unless (~first? & element(seen, name, default: #f))
	seen[name] := #t;
	define-external-symbol(builder, f-name, unsupplied(),
			       import?: $imported-name-mangler);
	
	define-public-symbol(builder, name, unsupplied());
	
	select-dylan-section(builder, #"variables", 1);
	add-symbol-definition(builder, name, unsupplied());
	
	add-imported-data(builder, f-name, unsupplied(), offset);
      end unless;
      
      name := read-line(stream);
    end while;
    
  end with-open-file;
  
end method;

define method output-coff-builder-footer
    (builder :: <coff-builder>) => ()

  add-imported-data-fixups(builder);
  fixup-coff-builder(builder);
  write-binary(builder.destination, builder)

end method;

define macro with-import-fixups
  { with-import-fixups (?build:name) ?:body end }
    =>
 {
  let ?=fixups = pair(#f, #f);
  let library :: <symbol> = ?build.dylanlib;

  if  (*linker*.static-linking? | library.dylan-library?)
    ?body;
  else

  block ()

  let (fixups-builder :: <coff-builder>, fixups-seen :: <string-table>) =
    open-import-fixups-outputter(?build);
  ?=fixups.head := fixups-builder; ?=fixups.tail := fixups-seen;

  ?body;

  output-coff-builder-footer(?=fixups.head);

  cleanup
    let fixups-builder :: <coff-builder> = ?=fixups.head;
    let fixups-stream = fixups-builder.destination;
    if (fixups-stream & stream-open?(fixups-stream)) close(fixups-stream) end;
  end block;

  end if;


 }
end macro;

define method write-build-import-fixups
    (build :: <build>) => ()
  with-import-fixups(build)
    write-import-fixups(fixups.head, fixups.tail, build.build-directory, #t);
  end with-import-fixups;
end method;


define inline method dylan-library?
    (library :: <symbol>) => (dylan? :: <boolean>)
  library == #"dylan"
end method;

define inline method dylan-library?
    (executable :: <string>) => (dylan? :: <boolean>)
  dylan-library?(as(<symbol>, executable))
end method;

define inline method runtime-library?
    (library :: <string>) => (runtime? :: <boolean>)
  as(<symbol>, library) == #"runtime"
end method;


/// Mangling Support


define constant $linker-mangle-data
  = vector(#('-', '_'), #('!', 'X'), #('$', 'D'), #('%', 'P'), #('*', 'T'), 
           #('/', 'S'), #('<', 'L'), #('>', 'G'), #('?', 'Q'), #('+', 'A'),
           #('&', 'B'), #('^', 'C'), #('_', 'U'), #('@', 'O'), #('=', 'E'),
           #('~', 'N'));

define variable *linker-mangle-table* = #f;

define method initialize-mangler-table () => ()
  unless (*linker-mangle-table*)
    *linker-mangle-table* := make(<table>);
    for (mangle in $linker-mangle-data)
      *linker-mangle-table*[mangle[0]] := mangle[1];
    end for;
  end unless;
end method;

define method linker-mangle(name :: <string>) => (mangled-name :: <string>)
  initialize-mangler-table();
  let new-name = as-lowercase(copy-sequence(name));
  for (c in new-name,
       pos from 0)
    let new-c = element(*linker-mangle-table*, c, default: #f);
    if (new-c)
      new-name[pos] := new-c;
    end if;
  end for;
  new-name
end method;
