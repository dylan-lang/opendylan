Module:    environment-manager
Author:    Hugh Greene
Synopsis:  Functions the Environment provides to external callers.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// -=- AUXILLIARY DEFINITIONS -=-

define class <file-library-not-found-warning> (<simple-warning>)
  keyword format-string: 
    = "No library in the active project contains '%s'";
end class;

define class <file-project-not-found-warning> (<simple-warning>)
  keyword format-string: = "No open project contains '%s'";
end class;

//--- This finds the libraries that match the given pathname
//--- within the active project.
define function find-libraries-from-pathname
    (locator :: <file-locator>)
 => (libraries :: <sequence>
       /* of: vector(<project-object>, <source-record>) */)
  let libraries = make(<stretchy-vector>);
  let project = active-project();
  when (project)
    do-project-file-libraries
      (method (library :: <library-object>, record :: <source-record>)
	 add!(libraries, vector(library, record))
       end,
       project,
       locator);
  end;
  when (empty?(libraries))
    signal(make(<file-library-not-found-warning>,
		format-arguments: vector(locator)));
    // --- The above 'signal' is allowed to return, though we have no
    // restarts yet to, say, allow the user to find/open the project.
  end;
  libraries
end function find-libraries-from-pathname;

define function find-projects-from-pathname
    (locator :: <file-locator>)
 => (projects :: <sequence>
       /* of: vector(<project-object>, <source-record>) */)
  let all-projects = open-projects();
  let projects = #();
  for (proj in all-projects)
    let record = find-project-source-record(proj, locator);
    when (record)
      projects := add!(projects, vector(proj, record));
    end;
  end;
  when (empty?(projects))
    signal(make(<file-project-not-found-warning>,
		format-arguments: vector(locator)));
    // --- The above 'signal' is allowed to return, though we have no
    // restarts yet to, say, allow the user to find/open the project.
  end;
  projects
end function find-projects-from-pathname;

define function location-info->source-location
    (project :: <project-object>, locator :: <file-locator>,
     coords :: <vector>)
 => (location :: false-or(<source-location>))
  let (start-line, start-column, end-line, end-column)
    = apply(values, coords);
  // ---*** Should make a full region-location when they become available.
  ignore(start-column, end-line, end-column);
  // Return the <source-location> within the project.
  // NB the 2nd parameter of MAKE-LINE-LOCATION expects a line that is
  // relative to the source-record, so we have to deduct the distance
  // into the file that the source record starts (ie after the headers).
  let record = find-project-source-record(project, locator);
  let offset = record & source-record-start-line(record);
  when (offset)
    block ()
      make-line-location(record, start-line - offset)
    exception (type-union(<file-does-not-exist-error>,
			  <source-record-missing>))
      #f
    end
  end
end function;


// In order for the server interface to work, it must be possible to
// coerce to all argument types used here, from <strings>.  I use a
// separate GF from "as" so that I can add my own "sideways" methods
// without messing things up for other people.

define generic coerce-for-command-call
    (class :: <class>, object :: <object>)
 => (object :: <object>); // where instance?(object, class)

define method coerce-for-command-call
    (class :: <class>, object :: <object>)
 => (object :: <object>)
  as(class, object)
end method;

define method coerce-for-command-call
    (class :: subclass(<vector>), string :: <string>)
 => (coords :: <vector>)
  // Strings representing a "source region" are assumed to be of the form
  // "[<start-line>[,<start-column>][;<end-line>[,<end-column>]]]"
  // where "<>" surrounds 'placeholders' and "[]" indicates "optional"
  // Any of the placeholders may be empty.

  // Replace with common-dylan:split. --cgay
  local
    method split
	(string :: <string>, char :: <character>)
     => (before :: <string>, after :: <string>)
      let split-index :: false-or(<integer>)
        = find-key(string, curry(\==, char));
      if (split-index)
	values(copy-sequence(string, end: split-index),
	       copy-sequence(string, start: split-index + 1))
      else
	values(string, "")
      end if
    end method,
  
    method get-integer
	(string :: <string>) => (value :: <integer>)
      block()
        string-to-integer(string);
      exception (<error>)
        0
      end block;
    end method;

  // Parse the integers from the string.
  let (start-str :: <string>, end-str :: <string>) = split(string, ';');
  let (start-line-str :: <string>, start-column-str :: <string>)
    = split(start-str, ',');
  let (end-line-str :: <string>, end-column-str :: <string>)
    = split(end-str, ',');
  // Any numbers we can't find default to 0.
  map(get-integer,
      vector(start-line-str, start-column-str,
	     end-line-str, end-column-str))
end method;



/// -=- COMMANDS -=-

// The argument types for each of these functions must be ones to
// which <string>s can be coerced.  In particular, none can be calls
// to "type-union" or "one-of", or constant types defined as such.
// If a command needs more complicated parsing/conversion (say,
// depending on other arguments) it can choose to accept a <string>
// and parse it explicitly.

define macro command-function-definer
  { define command-function ?:name ?args ?:body end }
 => { define function ?name ?args ?body end;
      register-command-function(?#"name", ?name); }
args:
  { ( ?params:* ) } => { ( ?params ) }
  { ( ?params:* ) => ( ?return:* ) } => { ( ?params ) => ( ?return ) }
end macro;

define function call-with-module
    (function :: <function>,
     project :: false-or(<project-object>), file :: <string>,
     #rest args) => (#rest values)
  // Result type depends on FUNCTION
  let module = project & file-module(project, file);
  when (module)
    apply(function, project, module, args)
  end
end function;

define function defaulted-find-project
    (project-name :: <string>)
 => (project :: false-or(<project-object>))
  if (empty?(project-name))
    active-project()
  else
    find-project(project-name)
  end
end function;



// -=- PROPERTIES -=-
// ---*** This should be ditched, probably.

define function do-cmd-properties
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  unless (object)
    object := find-named-definition(project, module, name)
  end;
  when (object)
    show-properties(project, object)
  end
end function do-cmd-properties;

define command-function properties
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
  ignore(client-id, coords);
  call-with-module(do-cmd-properties, defaulted-find-project(project-name), file, name)
end command-function properties;


// -=- COMPLETE -=-

define function do-cmd-complete
    (project :: <project-object>,
     module :: <module-object>,
     name :: <string>)
 => (names :: <sequence> /* of: <string> */)
  // ignore(name, module, project);
  // ---*** Should really call some completion function ...
  #[]
end function do-cmd-complete;

define command-function complete
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>,
     #key initial-match-only? :: <boolean> = #f)
 => (names :: <sequence> /* of: <string> */)
  ignore(client-id, file, coords, name, initial-match-only?);
  call-with-module(do-cmd-complete, defaulted-find-project(project-name), file, name)
end command-function complete;


// -=- FIND REFERENCES -=-

/* --- For use if we end up grouping edit-definitions with find-uses.
define constant $reference-kinds = #[#"definitions", #"uses", #"both"];
*/

define function do-cmd-edit-definitions
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  unless (object)
    object := find-named-definition(project, module, name)
  end;
  when (object)
    let edited? = edit-definition(project, object);
    unless (edited?)
      error("Couldn't find '%s'.",
	    environment-object-display-name(project, object, module))
    end
  end
end function do-cmd-edit-definitions;

define command-function edit-definitions
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
 => ()
  // ignore(client-id, coords);
  call-with-module(do-cmd-edit-definitions, defaulted-find-project(project-name), file, name);
end command-function edit-definitions;


// -=- COMPILE -=-

// Note:
// The 'scope' argument is one of the following:
//  #"project-parse"         => parse source, enough to build browser info;
//  #"project-compile"       => compile only changes in project (usual choice);
//  #"project-clean-compile" => compile all of a project, even if up-to-date;
//  #"project-link"          => link project, to derive target;
//  #"project-build"         => do "compile" then "link".
//  #"project-clean-build"   => do "clean compile" then "link".
//
// Exceptions:
// "compile" may signal a <file-project-not-found-warning>.

define function do-cmd-compile
    (project :: <project-object>,
     scope :: <symbol>,
     location :: <source-location>)
  compile-project-location(project, location, scope);
end function do-cmd-compile;

define command-function compile
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>,
     scope :: <symbol>)
  ignore(client-id, name);
  // Is name needed here?
  // Maybe return compilation log?
  let project = defaulted-find-project(project-name);
  let location
    = project 
        & location-info->source-location
            (project, as(<file-locator>, file), coords);
  when (location)
    do-cmd-compile(project, scope, location);
  end;
end command-function;


// -=- DOCUMENTATION -=-

define function do-cmd-documentation
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  unless (object)
    object := find-named-definition(project, module, name)
  end;
  when (object)
    show-documentation(name, project, module, object)
  end
end;

define command-function documentation
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
 => ()
  ignore(client-id, coords);
  // hughg: Not 100% sure call-with-module is appropriate here.
  call-with-module(do-cmd-documentation, defaulted-find-project(project-name), file, name);
end command-function;


// -=- DESCRIBE -=-

define function do-cmd-describe
    (project :: false-or(<project-object>),
     module :: false-or(<module-object>),
     name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  when (~object & (module & project))
    object := find-named-definition(project, module, name)
  end;
  when (object)
    show-definition-summary(name, project, module, object)
  end
end function do-cmd-describe;

define command-function describe
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
 => (success? :: <boolean>)
  ignore(client-id, coords);
  call-with-module(do-cmd-describe, defaulted-find-project(project-name), file, name);
end command-function describe;


// -=- BROWSE -=-

// Browse an object

define function do-cmd-browse
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  unless (object)
    object := find-named-definition(project, module, name)
  end;
  when (object)
    browse-object(project, object)
  end
end;

define command-function browse
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
 => (success? :: <boolean>)
  ignore(client-id, coords, name);
  call-with-module(do-cmd-browse, defaulted-find-project(project-name), file, name);
end command-function browse;


// Browse the type of an object

define function do-cmd-browse-type
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  unless (object)
    object := find-named-definition(project, module, name)
  end;
  when (object)
    browse-object-type(project, object)
  end
end;

define command-function browse-type
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
 => (success? :: <boolean>)
  ignore(client-id, coords);
  call-with-module(do-cmd-browse-type, defaulted-find-project(project-name), file, name);
end command-function browse-type;


// Browse the generic function of some method call

define function do-cmd-browse-function
    (project :: <project-object>, module :: <module-object>, name :: <string>,
     #key object :: false-or(<environment-object>) = #f)
 => (#rest values)
  unless (object)
    object := find-named-definition(project, module, name)
  end;
  when (object)
    browse-object-generic-function(project, object)
  end
end;

define command-function browse-function
    (client-id, project-name :: <string>,
     file :: <string>, coords :: <vector>, name :: <string>)
 => (success? :: <boolean>)
  ignore(client-id, coords);
  call-with-module(do-cmd-browse-function, defaulted-find-project(project-name), file, name);
end command-function browse-function;


// -=- OPEN A FILE -=-

define function do-cmd-open-file
    (filename :: <string>)
 => (success? :: <boolean>)
  environment-open-file(as(<file-locator>, filename))
end function do-cmd-open-file;

define command-function OpenFile
    (client-id, file :: <string>)
 => (success? :: <boolean>)
  ignore(client-id);
  do-cmd-open-file(file)
end command-function OpenFile;


// -=- PROVIDE RESULTS ASYNCHRONOUSLY -=-

define function do-cmd-provide-results
    (id :: <string>, results :: <vector>)
 => ()
  %provide-results(id, results)
end;

define command-function provide-results
    (client-id, results-id :: <string>, results :: <vector>)
 => ()
  ignore(client-id);
  do-cmd-provide-results(results-id, results)
end command-function provide-results;
