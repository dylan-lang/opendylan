Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define generic verify-project-layout(layout :: <project-layout>,
                                       #key create? = #f);

define class <disk-project-layout> (<project-layout>) end;

define class <user-disk-project-layout> (<disk-project-layout>)
  constant slot user-disk-project-file :: <file-locator>,
    init-keyword: project-file:;
  slot user-disk-project-source :: <directory-locator>,
    init-keyword: source-location:;
end;

define method initialize (project :: <user-disk-project-layout>, #rest keys,
                          #key project-file :: <file-locator>,
                          source-record-class, processor, operating-system,
                          build-dir, database-dir, profile-dir,
                          read-only? = #f,
                          #all-keys)
  debug-assert(locator-extension(project-file) = $user-project-suffix,
               "project file doesn't have %s extension",
               $user-project-suffix);
  let source-class = if(source-record-class) source-record-class
                     else <file-source-record> end;
  verify-project-layout(project,
                        project-file: project-file,
                        build-dir: build-dir,
                        database-dir: database-dir,
                        profile-dir: profile-dir,
                        read-only?: read-only?,
                        processor: processor,
                        operating-system: operating-system);
  apply(next-method, project,
        source-record-class:, source-class,
        lid-location:, project-file,
        keys);
  if (~project.project-source-files)
    project.project-read-only? := #t;
    let db = project-database-location(project);
    unless (file-exists?(db))
      error("There is no source and no database to load for the project %s",
            project-file)
    end
  end
end method;

define function build-location-name(project-name :: <string>)
 => (name :: <string>);
  concatenate(project-name, "-build")

end;

/*---*** andrewa: not currently used...
define function project-merge-pathnames(project :: <user-disk-project-layout>, files :: <sequence>)
 => (paths :: <sequence>);
  let location = project.user-disk-project-source;
  map(rcurry(merge-locators, location), files)
end;
*/

define method verify-project-layout(project :: <user-disk-project-layout>,
                                    #key project-file :: <file-locator>,
                                    create? = #t,
                                    read-only? = #f,
                                    processor, operating-system,
                                    build-dir :: false-or(<directory-locator>),
                                    profile-dir :: false-or(<directory-locator>),
                                    database-dir :: false-or(<directory-locator>));
  let project-location = project-file.locator-directory;
  let project-name = locator-base(project-file);

  user-disk-project-source(project) := project-location;

  let build-location =
    if(build-dir)
      build-dir
    else
      let override-build = user-build-path();
      if (override-build)
        subdirectory-locator(override-build, project-name)
      else
        subdirectory-locator(project-location, build-location-name(project-name))
      end;
    end;
  ensure-directories-exist(build-location);

  project-build-location(project) :=
    ~read-only? & build-location;

  // Note that "dylan" db has to be in a directory named "dylan"
  // according to the compiler
  // Let's assume for now that dylan is always looked up in the registry
  // which means that we already know the "right" location
  project-database-location(project)
    := make(<file-locator>,
            directory: database-dir | build-location,
            base:      project-name,
            extension: $dylan-database-suffix);

  project-profile-location(project)
    := make(<file-locator>,
            directory: profile-dir | build-location,
            base:      project-name,
            extension: $dylan-profile-suffix);

  if(read-only? & ~file-exists?(project-database-location(project)))
    error("Project %s is read-only and no compiler database was found")
  end;

end;

define method project-source-location(layout :: <user-disk-project-layout>)
 => (location :: <directory-locator>)
  layout.user-disk-project-source;
end;

define method project-location
    (project :: <user-disk-project-layout>) => (location :: <file-locator>)
  project.user-disk-project-file
end method project-location;

// define variable *default-project-layout-class* = <user-disk-project-layout>;

define method %library-name-from-file(type :: subclass(<lid-project>),
                                      loc :: <file-locator>)
 => (name :: false-or(<symbol>));
  // this means that we are going to read each project file twice for now
  // TO DO: to be fixed later
  let properties =
    block()
      read-file-header(loc);
    exception(e :: <file-system-error>)
      apply(user-warning, e.condition-format-string, e.condition-format-arguments);
      #f
    exception(e :: <file-does-not-exist-error>)
      user-warning("File %s does not exist", as(<string>, loc));
      #f
    exception(e :: <badly-formed-file-header>)
      apply(user-warning, e.condition-format-string, e.condition-format-arguments);
    end;
  properties &
    begin
      let lib-entry = element(properties, #"library", default: #f);
      let name =
        if(lib-entry)
          as(<symbol>, first(lib-entry))
        else
          user-warning("HDP or LID file %s is missing library: keyword", loc);
          #f
        end;
      name
    end;
end;
