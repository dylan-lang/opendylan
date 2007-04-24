Module:    projects-protocol-internals
Synopsis:  Project manager protocol library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

//---*** What should these really be?
define constant <processor>           = <symbol>;
define constant <operating-system>    = <symbol>;
define constant <linker> = <symbol>;

define constant <abort-reason>
  = one-of(#"never", #"warnings", #"serious-warnings");

define variable *copy-canonical-sources?* :: <boolean> = #t;


/// Workspaces

define open abstract primary class <project-workspace> (<object>)
end class <project-workspace>;

define open generic make-workspace-project
    (workspace :: <project-workspace>, #key, #all-keys)
 => (project :: <project>);

define open generic make-workspace-build-target
    (workspace :: <project-workspace>, #key, #all-keys)
 => (build-target :: <build-target>);

define open generic workspace-processor
    (workspace :: <project-workspace>) => (processor :: <processor>);

define open generic workspace-operating-system
    (workspace :: <project-workspace>) => (os :: <operating-system>);

define open generic workspace-projects
    (workspace :: <project-workspace>) => (projects :: <sequence>);

define open generic workspace-build-request
    (workspace :: <project-workspace>)
 => (request :: false-or(<build-request>));

define variable *default-workspace* :: false-or(<project-workspace>) = #f;

define function default-workspace
    () => (workspace :: <project-workspace>)
  *default-workspace*
    | error("No default project workspace!")
end function default-workspace;

define function default-workspace-setter
    (workspace :: <project-workspace>)
 => (workspace :: <project-workspace>)
  *default-workspace* := workspace
end function default-workspace-setter;

define macro with-workspace-build
  { with-workspace-build (?workspace:name, ?target:name)
      ?body:body
    end }
 => { do-with-workspace-build
       (method (?workspace :: <project-workspace>, ?target :: <build-target>)
	  ?body
	end,
	?workspace, ?target) }
end macro with-workspace-build;

define method do-with-workspace-build
    (function :: <function>, workspace :: <project-workspace>, 
     target :: <build-target>)
 => ()
  block ()
    note-workspace-build-operation-started(workspace, target);
    function(workspace, target)
  cleanup
    note-workspace-build-operation-finished(workspace)
  end
end method do-with-workspace-build;


/// File extensions

define constant $dylan-file-extension      = "dylan";
define constant $dylan-alternate-extension = "dyl";
define constant $project-file-extension    = "hdp";
define constant $lid-file-extension        = "lid";
define constant $database-file-extension   = "ddb";
define constant $profile-file-extension    = "prf";
define constant $cache-file-extension      = "cache";

define function dylan-file-extension
    () => (extension :: <string>)
  $dylan-file-extension
end function dylan-file-extension;

define function project-file-extension
    () => (extension :: <string>)
  $project-file-extension
end function project-file-extension;

define function lid-file-extension
    () => (extension :: <string>)
  $lid-file-extension
end function lid-file-extension;

define function database-file-extension
    () => (extension :: <string>)
  $database-file-extension
end function database-file-extension;

define function profile-file-extension
    () => (extension :: <string>)
  $profile-file-extension
end function profile-file-extension;

define function project-cache-file-extension
    () => (extension :: <string>)
  $cache-file-extension
end function project-cache-file-extension;

define function locator-extension-type
    (locator :: <file-locator>) => (type :: <symbol>)
  let extension = locator.locator-extension | dylan-file-extension();
  select (extension by \=)
    dylan-file-extension()     => #"dylan";
    $dylan-alternate-extension => #"dylan";
    project-file-extension()   => #"hdp";
    lid-file-extension()       => #"lid";
    database-file-extension()  => #"ddb";
    otherwise                  => as(<symbol>, extension);
  end
end function locator-extension-type;
