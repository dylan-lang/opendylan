Module:    projects-protocol-internals
Synopsis:  Project manager protocol library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project conditions

define open abstract class <project-condition> (<condition>)
end class <project-condition>;


/// Project errors

define open class <project-error> (<project-condition>, <simple-error>)
end class <project-error>;

define method project-error
    (project :: <project>, format-string :: <string>, #rest format-args)
  error(make(<project-error>,
	     format-string:    format-string,
	     format-arguments: format-args))
end method project-error;

define class <project-file-error> (<project-error>)
  sealed constant slot condition-locator :: <file-locator>,
    required-init-keyword: locator:;
end class <project-file-error>;

define method project-file-error
    (project :: <project>, location :: <file-locator>, 
     format-string :: <string>, #rest format-args)
  error(make(<project-file-error>,
	     project:          project,
	     locator:          location,
	     format-string:    format-string,
	     format-arguments: format-args))
end method project-file-error;


/// Project warnings

define open class <project-warning> (<project-condition>, <simple-warning>)
  sealed constant slot warning-project :: <project>,
    required-init-keyword: project:;
end class <project-warning>;

define open class <project-serious-warning> (<project-warning>)
end class <project-serious-warning>;

define function project-warning
    (project :: <project>, string, #rest args)
  let warning
    = make(<project-warning>,
	   project: project,
	   format-string: string, 
	   format-arguments: args);
  register-project-condition(project, warning);
  signal(warning)
end function project-warning;

define function project-serious-warning 
    (project :: <project>, string, #rest args)
  let warning
    = make(<project-serious-warning>, 
	   project: project,
	   format-string: string,
	   format-arguments: args);
  register-project-condition(project, warning);
  signal(warning)
end function project-serious-warning;

define function resignal-project-warning 
    (project :: <project>, c :: <condition>, 
     #key abort? :: <boolean> = #f)
 => ()
  apply(project-warning, project, c.condition-format-string, c.condition-format-arguments);
  abort? & abort-build(project)
end function resignal-project-warning;


/// Build warnings

define open class <build-warning> (<project-condition>, <simple-warning>)
  sealed constant slot warning-project :: <project>,
    required-init-keyword: project:;
end class <build-warning>;

define open class <build-serious-warning> (<project-warning>)
end class <build-serious-warning>;

define function build-warning
    (project :: <project>, string, #rest args)
  let warning
    = make(<build-warning>,
	   project: project,
	   format-string: string, 
	   format-arguments: args);
  register-project-condition(project, warning);
  signal(warning)
end function build-warning;

define function build-serious-warning 
    (project :: <project>, string, #rest args)
  let warning
    = make(<build-serious-warning>, 
	   project: project,
	   format-string: string,
	   format-arguments: args);
  register-project-condition(project, warning);
  signal(warning)
end function build-serious-warning;

define function resignal-build-warning 
    (project :: <project>, c :: <condition>, 
     #key abort? :: <boolean> = #f)
 => ()
  apply(build-warning, project, c.condition-format-string, c.condition-format-arguments);
  abort? & abort-build(project)
end function resignal-build-warning;
