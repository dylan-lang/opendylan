Module:    dylan-build
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// Drive a Shell Command from Dylan

define method execute-shell-command
    (suppress-echo? :: <boolean>, command :: <string>, #rest args) => ()
  let command-line =
    reduce(method(line, arg)
	       select(arg by instance?)
		 <list> => reduce(method(line, arg)
				      concatenate(line, " ", arg)
				  end method,
				  line,
				  arg);
		 <string> => concatenate(line, " ", arg);
	       end select;
	   end method,
	   command,
	   args);

  //echo("");
  //shell-command(concatenate("echo ", command-line));
  unless (suppress-echo?)
    echo("");
  end unless;
  let status = run-application(command-line,
			       under-shell?: #t,
			       inherit-console?: #t);

  if (status > 1)
    build-error("In running Shell Command %=", command-line);
  end if;

end method;


// Display an internal message from Builds in Shell Window

define method echo(format-string :: <string>, #rest args) => ()

  format-out("- %s", *build*.indentation);
  apply(format-out, format-string, args);
  force-output(*standard-output*)

end method;

// Extra step-by-step Debugging Information

define method echo?(format-string :: <string>, #rest args) => ()

  if (*build*.dylanecho?)
    apply(echo, format-string, args);
  end if;

end method;

// Changing Build Directories


define method change-directory(directory :: <pathname>)
  if (~ file-exists?(directory))
    build-error("Invalid Directory %s",
		directory);
  else
    working-directory() := directory;
  end if;
end method;

define macro with-directory
  { with-directory (?directory:expression) ?:body end }
    => { 
	let previous-directory = working-directory();
	block()
	  change-directory(?directory);
	  ?body
	cleanup
	  change-directory(previous-directory);
        /* Add this if you want builds to only bomb out of one recursive level
           rather than the whole thing
	exception(<build-error>)
	  change-directory(previous-directory);
         */
	end block;
       }
end macro;

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

// Build File Copying and Deletion

define method copy-build-file(source :: <pathname>, destination :: <pathname>,
			      #rest args) => ()
  with-build-error()
    apply(copy-file, source, destination, args);
  end;
end method;

define method delete-build-file(file :: <pathname>) => ()
  with-build-error()
    delete-file(file);
  end;
end method;
