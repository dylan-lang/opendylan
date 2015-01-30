Module:    environment-tools
Synopsis:  Environment tools
Author:    Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Glue to Editor Manager

define method find-editor
    (frame :: <environment-frame>)
 => (found? :: <boolean>)
  execute-command(make(<editor-open-command>));
  #t        //--- a bit of a cheat
end method find-editor;

define function editor-open-file
    (pathname :: <file-locator>,
     #key start-line, start-column, end-line, end-column, deuce-frame)
 => (succeeded? :: <boolean>)
  let start-line   = start-line   | 0;
  let start-column = start-column | 0;
  let end-line     = end-line     | start-line;
  let end-column   = end-column   | start-column;
  execute-command(make(<editor-open-file-command>,
                       pathname:     as(<string>, pathname),
                       start-line:   start-line,
                       start-column: start-column,
                       end-line:     end-line,
                       end-column:   end-column,
                       editor-frame: deuce-frame));
  #t        //--- a bit of a cheat
end function editor-open-file;

define method editor-new-file
    (#key pathname, deuce-frame)
 => (succeeded? :: <boolean>)
  execute-command(make(<editor-new-file-command>,
                       pathname:     pathname & as(<string>, pathname),
                       editor-frame: deuce-frame))
end method editor-new-file;

define method editor-edit-definitions
    (project :: <project-object>, definitions :: <sequence>,
     #key deuce-frame, title)
 => (succeeded? :: <boolean>)
  execute-command(make(<editor-edit-definitions-command>,
                       project:      project,
                       definitions:  definitions,
                       title:        title,
                       editor-frame: deuce-frame))
end method editor-edit-definitions;


/// Glue from source locations to the Editor Manager

define sideways method edit-definition
    (project :: <project-object>, object :: <environment-object>)
 => (found-definition? :: <boolean>)
  let location = environment-object-source-location(project, object);
  when (location)
    block ()
      edit-source-location(project, location);
      #t
    exception (type-union(<file-does-not-exist-error>,
                          <source-record-missing>))
      #f;
    end
  end
end method edit-definition;

//---*** This needs to edit the newest source, not the canonical source, right?
define sideways method edit-source-location
    (project :: <project-object>, location :: <source-location>) => ()
  // Source location line numbers are 1-based, but we treat editors are 0-based
  let record = source-location-source-record(location);
  let start-offset = source-location-start-offset(location);
  let end-offset   = source-location-end-offset(location);
  let start-line   = source-offset-line(start-offset) - 1;
  let start-column = source-offset-column(start-offset);
  if (start-offset == end-offset)
    edit-source-record
      (project, record,
       start-line: start-line, start-column: start-column)
  else
    let end-line   = source-offset-line(end-offset)   - 1;
    let end-column = source-offset-column(end-offset);
    edit-source-record
      (project, record,
       start-line: start-line, start-column: start-column,
       end-line:   end-line,   end-column:   end-column)
  end
end method edit-source-location;

//---*** This needs to edit the newest source, not the canonical source, right?
define sideways method edit-source-record
    (project :: <project-object>, record :: <file-source-record>,
     #key start-line, start-column, end-line, end-column)
 => ()
  block ()
    let locator    = source-record-location(record);
    let first-line = source-record-start-line(record);
    let start-line = if (start-line) start-line + first-line else 0 end;
    let end-line   = if (end-line)   end-line   + first-line else 0 end;
    editor-open-file
      (locator,
       start-line: start-line, start-column: start-column,
       end-line:   end-line,   end-column:   end-column)
  exception (type-union(<file-does-not-exist-error>,
                        <source-record-missing>))
    #f
  end
end method edit-source-record;


/// Save All Files

define method editor-save-project-files
    (project :: <project-object>, owner :: <frame>)
 => (all-saved? :: <boolean>)
  let subprojects = project-used-projects(project, indirect?: #t);
  let sources
    = apply(concatenate,
            project-all-sources(project),
            map(project-all-sources, subprojects));
  let pathnames = map(source-full-name, sources);
  //--- Maybe the following test should be some 'locator-equal?' function
  let pathnames = remove-duplicates(pathnames, test: \=);
  let reason
    = format-to-string("The following modified files belong to\n"
                       "project %s or its subprojects.\n"
                       "Choose which files to save before building.",
                       environment-object-display-name(project, project, #f));
  //--- In theory, need to deal with the possibility of timeout while
  //--- waiting for results.  In practice, for now, Deuce never times out.
  //--- We might also want to disabled the owner frame while making this call,
  //--- but there's no guarantee the backend can do so.
  //--- We might later want to factor out the reason (and exit-label string).
  execute-command(make(<editor-save-files-command>,
                       project:      project,
                       pathnames:    pathnames,
                       reason:       reason,
                       editor-frame: owner,
                       exit-label:   "&Build"))
end method editor-save-project-files;


///---*** All of the following probably belongs in Environment Protocols

define method project-all-sources
    (project :: <project-object>) => (all-sources :: <sequence>)
  concatenate(project-sources(project), project-other-sources(project))
end method project-all-sources;

//--- 'project-sources-label-key' does something similar, but requires
//--- an <environment-fixed-project-frame> frame, which I don't have.
define method source-full-name
    (name :: <string>) => (name :: <string>)
  name
end method source-full-name;

define method source-full-name
    (locator :: <file-locator>) => (name :: <string>)
  as(<string>, locator)
end method source-full-name;

define method source-full-name
    (source-record :: <source-record>) => (name :: <string>)
  source-record-name(source-record)
end method source-full-name;

define method source-full-name
    (file-source-record :: <file-source-record>) => (name :: <string>)
  source-full-name(source-record-location(file-source-record))
end method source-full-name;


/// 'make-code-viewer'

define open generic make-code-viewer
    (#key project, #all-keys) => (viewer :: <sheet>, gadget :: <sheet>);
