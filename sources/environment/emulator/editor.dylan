Module:    emulator-environment
Synopsis:  Emulator Environment
Author:    Andy Armstrong, Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// LispWorks editor implementation "extras"
// (I.e., things which are not part of the standard editor protocol,
// but are layered on top of it for convenience in the emulator
// environment.

define method edit-project-source
    (project :: <project-object>, source :: <string>, #key line) => ()
  let location
    = project-source-location(project, source);
  edit-source-location(make-line-location(location, line))
end method edit-project-source;

define method edit-project-source
    (project :: <project-object>, source :: <locator>, #key line) => ()
  let location 
    = project-source-location(project, as(<string>, source));
  edit-source-location(make-line-location(location, line))
end method edit-project-source;
