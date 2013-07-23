Module: dylan-user
Synopsis: Library and module definitions for registry-based projects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library registry-projects
  use dylan;
  // Probably don't need all this, sort it out later...
  use collections;
  use io;
  use system;
  use build-system;

  use file-source-records;
  use dfmc-browser-support;
  use projects;

  export registry-projects;

end library;

define module registry-projects-internal
  use dylan;
  use dylan-extensions;
  use simple-debugging;
  // Probably don't need all this, sort it out later...
  use collectors;
  use set;
  use streams;
  use locators;
  use format;
  use print;
  use standard-io;
  use format-out;
  use operating-system, rename: {load-library => os/load-library};
  use file-system;
  use build-system;

  use file-source-records;
  use dfmc-project-compilation;
  use projects-implementation;
  use lid-projects;
  export
    <registry-project>,
    registry-location,
    <registry-entry-not-found-error>,
    find-registries,
    compute-library-location;
end;

define module registry-projects
  use registry-projects-internal, export: all; 
end;
