Module:    win32-environment
Synopsis:  Win32 Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Registry hacking (yuck!)

define constant $personal-directories
  = #(#("FUNCTIONAL_DEVELOPER_USER_ROOT"),
      #("FUNCTIONAL_DEVELOPER_USER_BUILD",      "build"),
      #("FUNCTIONAL_DEVELOPER_USER_INSTALL"),
      #("FUNCTIONAL_DEVELOPER_USER_SOURCES",    "sources"),
      #("FUNCTIONAL_DEVELOPER_USER_REGISTRIES", "sources", "registry"));

define constant $system-directories
  = #(#("FUNCTIONAL_DEVELOPER_RELEASE_ROOT"),
      #("FUNCTIONAL_DEVELOPER_RELEASE_BUILD",      "build"),
      #("FUNCTIONAL_DEVELOPER_RELEASE_INSTALL"),
      #("FUNCTIONAL_DEVELOPER_RELEASE_SOURCES",    "sources"),
      #("FUNCTIONAL_DEVELOPER_RELEASE_REGISTRIES", "sources", "registry"));

define method maybe-set-roots
    (#key personal-root :: false-or(<directory-locator>),
          system-root :: false-or(<directory-locator>))
 => ()
  local method set-variable
	    (variable :: <string>, directory :: <directory-locator>,
	     subdirectories :: <sequence>)
	  let subdirectory = apply(subdirectory-locator, directory, subdirectories);
	  environment-variable(variable) := as(<string>, subdirectory)
	end method set-variable;
  if (personal-root)
    for (directory-info :: <list> in $personal-directories)
      let variable       = directory-info.head;
      let subdirectories = directory-info.tail;
      set-variable(variable, personal-root, subdirectories)
    end
  end;
  if (system-root)
    for (directory-info :: <list> in $system-directories)
      let variable       = directory-info.head;
      let subdirectories = directory-info.tail;
      set-variable(variable, system-root, subdirectories)
    end
  end
end method maybe-set-roots;

define method process-arguments
    (arguments :: <sequence>)
 => (filename :: false-or(<file-locator>))
  let personal-root = #f;
  let system-root   = #f;
  let filename      = #f;
  let arguments     = as(<deque>, arguments);
  while (~empty?(arguments))
    let argument = pop(arguments);
    if (argument[0] == '/')
      select (copy-sequence(argument, start: 1) by \=)
	"personal" => personal-root := as(<directory-locator>, pop(arguments));
	"system"   => system-root   := as(<directory-locator>, pop(arguments));
	otherwise  => #f;
      end
    else
      block ()
	filename := as(<file-locator>, argument)
      exception (error :: <locator-error>)
	environment-startup-error
	  ("Invalid filename '%s' passed to %s",
	   argument,
	   release-product-name());
      end
    end
  end;
  if (release-internal?())
    maybe-set-roots(personal-root: personal-root,
		    system-root:   system-root)
  end;
  filename
end method process-arguments;


/// Now actually start the environment

define method environment-startup-error 
    (message :: <string>, #rest args)
  let message = apply(format-to-string, message, args);
  environment-error-message(message, owner: #f);
  exit-application(-1)
end method environment-startup-error;

//---*** We need to flesh out the argument handling
define method main 
    (name :: <string>, arguments :: <sequence>) => ()
  debug-message("Starting environment: %s with arguments '%='...\n", 
		name, arguments);
  initialize-bitmaps();
  initialize-deuce();
  initialize-editors();
  initialize-source-control();
  block ()
    let filename = process-arguments(arguments);
    if (~filename | file-exists?(filename))
      exit-application(start-environment(filename: filename))
    else
      environment-startup-error("File '%s' does not exist", filename)
    end
  cleanup
    debug-message("Goodbye.\n")
  end
end method main;

main(application-name(), application-arguments());
