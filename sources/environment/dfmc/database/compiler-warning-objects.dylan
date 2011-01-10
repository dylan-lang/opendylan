Module:    dfmc-environment-database
Synopsis:  DFM compiler warnings information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Compiler warnings

define sealed method do-compiler-warnings
    (function :: <function>, server :: <dfmc-database>,
     library :: <library-object>,
     #key client) => ()
  let project-object = server.server-project;
  local method do-context-notes
	    (context :: <context>)
	  let program-notes = compilation-context-notes(context);
	  program-notes
	    & do-program-notes
	        (function, project-object, library, program-notes)
	end method do-context-notes;
  let project = library.library-compiler-project;
  let context = browsing-context(server, project);
  let second-context = project-second-context(project, context);
  do-context-notes(context);
  second-context & do-context-notes(second-context)
end method do-compiler-warnings;

define sealed method do-compiler-warnings
    (function :: <function>, server :: <dfmc-database>,
     object :: <source-form-object>,
     #key client) => ()
  let project-object = server.server-project;
  let source-form :: <source-form> = object.compiler-object-proxy;
  local method do-context-notes
	    (context :: <context>)
	  let program-notes = source-form-notes(context, source-form);
	  let library = project-library(project-object);
	  program-notes
	    & do-program-notes
	        (function, project-object, library, program-notes)
	end method do-context-notes;
  let context = browsing-context(server, source-form);
  let project = context.compilation-context-project;
  let second-context = project-second-context(project, context);
  do-context-notes(context);
  second-context & do-context-notes(second-context)
end method do-compiler-warnings;

define function do-program-notes
    (function :: <function>, project :: <project-object>, 
     library :: <library-object>, notes :: <sequence>)
 => ()
  let server = project.project-compiler-database;
  if (server)
    for (program-note :: <program-note> in notes)
      let class
        = select (program-note by instance?)
	    <serious-program-warning> => <serious-compiler-warning-object>;
	    <program-error>           => <compiler-error-object>;
	    otherwise                 => <compiler-warning-object>;
	  end;
      let warning
        = make-environment-object(class,
				  project: server.server-project,
		 		  library: library,
	  			  compiler-object-proxy: program-note);
      function(warning)
    end
  end
end function do-program-notes;

define sealed method compiler-warning-full-message
    (server :: <dfmc-database>, warning :: <compiler-warning-object>)
 => (message :: <string>)
  program-note-message(compiler-object-proxy(warning))
end method compiler-warning-full-message;
    
define sealed method compiler-warning-short-message
    (server :: <dfmc-database>, warning :: <compiler-warning-object>)
 => (message :: <string>)
  //---*** Until we have a browser-support API, just use the first line
  let message = compiler-warning-full-message(server, warning);
  first-line(message)
end method compiler-warning-short-message;
    
define sealed method get-environment-object-primitive-name
    (server :: <dfmc-database>, warning :: <compiler-warning-object>)
 => (message :: <string>)
  compiler-warning-short-message(server, warning)
end method get-environment-object-primitive-name;

define function first-line 
    (string :: <string>) => (line :: <string>)
  let newline-key
    = find-key(string,
	       method (character)
		 member?(character, #('\n', '\r'))
	       end method);
  if (newline-key)
    // Strip off a single trailing period, if any
    if (newline-key > 0 & string[newline-key - 1] = '.')
      newline-key := newline-key - 1
    end;
    copy-sequence(string, end: newline-key)
  else
    string
  end
end function first-line;


define sealed method warning-owner
    (server :: <dfmc-database>, warning :: <compiler-warning-object>)
 => (form :: false-or(<source-form-object>))
  let creator = program-note-creator(compiler-object-proxy(warning));
  when (instance?(creator, <source-form>))
    make-environment-object-for-source-form(server, creator)
  end when
end method warning-owner;

define sealed method environment-object-source-location
    (server :: <dfmc-database>, warning :: <compiler-warning-object>)
 => (location :: false-or(<source-location>))
  program-note-location(compiler-object-proxy(warning))
end method environment-object-source-location;
