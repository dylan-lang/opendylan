Module:    dylan-build
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Person-to-blame: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $user-install = user-install-path();

define constant $personal-lib = subdirectory-locator($user-install, #("lib"));
define constant $personal-bin = subdirectory-locator($user-install, #("bin"));

define constant $system-release = system-release-path();
define constant $system-install = system-install-path() | $system-release;
define constant $system-lib = subdirectory-locator($system-install, #("lib"));
define constant $system-bin = subdirectory-locator($system-install, #("bin"));
define constant $build-system = subdirectory-locator($system-bin, #("build-system"));

define variable *environment-variables* = make(<string-table>);

define class <unrecognized-variable>(<simple-condition>)
end class;

// Build Environment Variable Substitution

define method substitute-environment-variables(options :: <list>) => (new-options :: <list>)
  local method substitute(option :: <string>)
	  block()
	    substitute-environment-variables(option);
	  exception(<unrecognized-variable>)
	    option
	  end block;
	end method;

  map(substitute, options);

end method;

define method substitute-environment-variables(options :: <string>) => (new-options :: <string>)

  let new-options = options;
  while (member?('$', new-options))
    let start-position = subsequence-position(new-options, "$(");
    let end-position = subsequence-position(new-options, ")");
    let option = copy-sequence(new-options, start: start-position, end: end-position + 1);
    if (start-position & end-position)
      let value = element(*environment-variables*,
			  as-lowercase(option),
			  default: #f);
      if (value)
	let options1 = copy-sequence(new-options, end: start-position);
	let options2 = copy-sequence(new-options, start: end-position + 1);
	new-options := concatenate(options1, value, options2);
      else
	signal(make(<unrecognized-variable>,
		    format-string: "Unrecognized Environment Variable %s",
		    format-arguments: option));
      end if;
    else
      error("Incorrect syntax in environment-variable value %s",
	    options);
    end if;
  end while;

  new-options
end method;

define method build-environment-variable(variable :: <string>) => (value :: <string>)

  let value = element(*environment-variables*,
		      concatenate("$(", as-lowercase(variable), ")"),
		      default: #f);
  if (value)
    value
  else
    signal(make(<unrecognized-variable>,
		format-string: "Unrecognized Environment Variable %s",
		format-arguments: variable));
  end if;

end method;

define method build-environment-variable-setter
    (value :: <string>, variable :: <string>) => (value :: <string>)

  *environment-variables*[concatenate("$(", as-lowercase(variable), ")")] :=
    substitute-environment-variables(value);

end method;

// All link and compile options are read out of one-line text files
// Makes changing or adding Operating-System specific options easy
// Change this code by looking up target-OS directory first to add that
// functionality

define method read-build-variable(file :: <string>) => (line :: <string>)
  let file = as-lowercase(file);
  let locator =
    override-locator(subdirectory-locator($build-system, #("generic")),
		     name: file);
  unless (file-exists?(locator))
    error("Missing build system file %s", as(<string>, locator))
  end;
  with-open-file(stream = locator)
    as(<string>, read-line(stream));
  end with-open-file;
end method;

define macro &build-variable-definer
  { define &build-variable ?:name }
    => { 
	build-environment-variable(?"name") := read-build-variable(?"name");
       }
end macro;

define &build-variable libcmt;

define &build-variable guilibsmt;

define &build-variable guilflags;

define &build-variable olelibsmt;

define &build-variable linkdll;

define &build-variable linkexe;

define &build-variable ccompile;

define &build-variable rccompile;


