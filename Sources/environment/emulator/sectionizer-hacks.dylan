Module:    emulator-environment
Synopsis:  Emulator Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Sectionizer hacks

define lisp-interface
  functions file-truename from cl-user,
end;

define method file-modification-time
    (file :: <locator>) => (time :: <integer>)
  1
end method file-modification-time;

define class <named-line-source-location> (<line-source-location>)
  slot source-location-name :: <string>,
    required-init-keyword: name:;
end class <named-line-source-location>;

define function section-to-source-location
    (record :: <source-record>, section :: <section>)
 => (source-location :: <source-location>)
  let start-line = section-start-line(section);
  make(<named-line-source-location>,
       name: section-defining-line(section),
       source-record: record,
       start-offset: make(<line-source-offset>, line: start-line))
end function section-to-source-location;

define method source-record-top-level-forms-locations
    (project :: <emulator-project>, record :: <source-record>)
 => (locations :: <sequence>)
  let container = sectionize-file(source-record-location(record));
  let sections = source-container-sections(container);
  map(curry(section-to-source-location, record), sections)
end method source-record-top-level-forms-locations;

define method project-source-location
    (project :: <emulator-project>, source :: <string>)
 => (location :: <locator>)
  let cache = project-source-location-cache(project);
  let key = as(<symbol>, source);
  element(cache, key, default: #f)
    | begin
        let directory = project-directory(project);
        let location
          = as(<file-locator>, 
               file-truename(format-to-string("%s%s.dylan", directory, source)));
        cache[key] := location
      end
end method project-source-location;

define method definition-source-location
    (database :: <emulator-database>, project :: <emulator-project>)
 => (source-location :: <source-location>)
  #f
end method definition-source-location;

//---*** Hack to get the name correct in the project tree!
define method project-sources-label-key 
    (frame :: <frame>, object :: <named-line-source-location>)
 => (name :: <string>);
  source-location-name(object)
end method project-sources-label-key;
