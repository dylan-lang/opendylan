Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable $personal-install :: false-or(<directory-locator>) = #f;
define variable $personal-lib     :: false-or(<directory-locator>) = #f;
define variable $personal-bin     :: false-or(<directory-locator>) = #f;
define variable $personal-build   :: false-or(<directory-locator>) = #f;

// We don't ensure system directories because nothing is installed there.
// By definition they had to have been already created
define variable $system-release      :: false-or(<directory-locator>) = #f;
define variable $system-install      :: false-or(<directory-locator>) = #f;
define variable $system-lib          :: false-or(<directory-locator>) = #f;
define variable $system-bin          :: false-or(<directory-locator>) = #f;
define variable $redistributable-bin :: false-or(<directory-locator>) = #f;
define variable $system-build        :: false-or(<directory-locator>) = #f;

define constant $executable-name :: false-or(<file-locator>)
  = begin
      let filename = application-filename();
      filename & as(<file-locator>, filename)
    end;

define constant $release-root :: false-or(<directory-locator>)
  = $executable-name & locator-directory(locator-directory($executable-name));

//define constant $operating-system-version = version-of-windows();

define constant $environment-variables = make(<string-table>);


define method configure-build-system () => ()
  local method ensure-subdirectory-exists
	    (locator :: <directory-locator>, #rest subpath) 
	 => (subdirectory :: <directory-locator>)
	  let subdirectory = apply(subdirectory-locator, locator, subpath);
	  ensure-directories-exist(subdirectory);
	  subdirectory
	end method ensure-subdirectory-exists;

  $personal-install := user-install-path();
  $personal-lib := $personal-install & ensure-subdirectory-exists($personal-install, "lib");
  $personal-bin := $personal-install & ensure-subdirectory-exists($personal-install, "bin");
  $personal-build := user-build-path() | ($personal-install & subdirectory-locator($personal-install, "build"));

  $system-release := system-release-path();
  $system-install := system-install-path() | $system-release;
  $system-lib := subdirectory-locator($system-install, "lib");
  $system-bin := subdirectory-locator($system-install, "bin");
  $redistributable-bin := subdirectory-locator($system-install, "redistributable");
  $system-build :=
    begin
      let directory = system-build-path() | subdirectory-locator($system-install, "build");
      if (file-exists?(directory))
	shorten-pathname(directory)
      else
	directory
      end if;
    end;
end method;

configure-build-system();

define class <unrecognized-variable>(<simple-error>)
end class;

// Build Environment Variable Substitution

define method substitute-environment-variables(options :: <list>, #key) => (new-options :: <list>)
  local method substitute(option :: <string>)
	  block()
	    substitute-environment-variables(option);
	  exception(<unrecognized-variable>)
	    option
	  end block;
	end method;

  map(substitute, options);

end method;

define method substitute-environment-variables
    (options :: <string>, #key supplied-values?) => (new-options :: <string>)

  let new-options = options;
  while (member?('$', new-options))
    let start-position = subsequence-position(new-options, "$(");
    let end-position = start-position + 1;
    while (new-options[end-position] ~= ')')
      end-position := end-position + 1;
    end while;
    let option = copy-sequence(new-options, start: start-position, end: end-position + 1);
    if (start-position & end-position)
      let value = element(supplied-values? | $environment-variables,
			  as-lowercase(option),
			  default: #f);
      if (value)
	let value =
	  if (value.empty?)
	    ""
	  elseif (instance?(value, <list>))
	    value.first
	  else
	    value
	  end if;
	let options1 = copy-sequence(new-options, end: start-position);
	let options2 = copy-sequence(new-options, start: end-position + 1);
	new-options := concatenate(options1, value, options2);
      else
	signal(make(<unrecognized-variable>,
		    format-string: "Unrecognized Environment Variable %=",
		    format-arguments:
		      list(copy-sequence(new-options, start: start-position + 2, end: end-position))));
      end if;
    else
      error("Incorrect syntax in environment-variable value %s",
	    options);
    end if;
  end while;

  new-options
end method;

define method build-environment-variable(variable :: <string>) => (value)

  let value = element($environment-variables,
		      concatenate("$(", as-lowercase(variable), ")"),
		      default: #f);
  if (value)
    value
  else
    signal(make(<unrecognized-variable>,
		format-string: "Unrecognized Environment Variable %=",
		format-arguments: list(variable)));
  end if;

end method;

define method build-environment-variable-setter
    (value, variable :: <string>) => (value)

  $environment-variables[concatenate("$(", as-lowercase(variable), ")")]
    := if (instance?(value, <string>))
	 substitute-environment-variables(value)
       else
	 value
       end if;

end method;

// All link and compile options are read out of a single text file
// Makes changing or adding Operating-System or Linker specific options easy

define method read-linker() => ()
  let linker-file =
    build-system-linker-file
      (format-to-string("%s-linker.script", *linker*.linker-name));

  read-link-script(linker-file);
  build-environment-variable("system_lib") := as(<string>, $system-lib);

end method;

define method read-link-script(linker-file :: <string>) => ()
  process-script(linker-file,
		 method(variable, command-lines)
		     build-environment-variable(variable) := command-lines
		 end method);
end method;
