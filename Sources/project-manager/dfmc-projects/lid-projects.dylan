Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Lid projects

define sealed class <dfmc-lid-project> (<dfmc-project>, <basic-project>)
end class <dfmc-lid-project>;

define sealed domain make(singleton(<dfmc-lid-project>));
define sealed domain initialize(<dfmc-lid-project>);

define function open-lid-project
    (workspace :: <dfmc-project-workspace>, location :: <file-locator>)
 => (project :: <dfmc-lid-project>)
  let file-date = file-property(location, #"write-date");
  let (library-name, files, properties) = read-lid-data(location);
  let (major-version, minor-version)
    = lid-project-settings(location, properties);
  let target = make-lid-build-target(location, properties);
  make(<dfmc-lid-project>,
       workspace:     workspace,
       file-location: location,
       file-date:     file-date,
       library-name:  library-name,
       major-version: major-version,
       minor-version: minor-version,
       source-files:  files,
       targets:       vector(target),
       target:        target)
end function open-lid-project;

define method refresh-project
    (project :: <dfmc-lid-project>, #key force? = #f) => ()
  // TODO: this needs to update database location if library-name
  // has changed 
  let location = project.project-file-location;
  let file-date = file-property(location, #"write-date");
  if (force? | project.project-file-date ~= file-date)
    let (library-name, files, properties) = read-lid-data(location);
    let (major-version, minor-version)
      = lid-project-settings(location, properties);
    let target = make-lid-build-target(location, properties);
    unless (library-name == project.project-library-name)
      project-warning(project, "Library in project %s changed from %s to %s",
		      project.project-name,
		      project.project-library-name, library-name)
    end;
    project.project-library-name  := library-name;
    project.project-source-files  := files;
    project.project-major-version := major-version;
    project.project-minor-version := minor-version;
    project.project-targets       := vector(target);
    project.project-target        := target;
    project.project-file-date     := file-date;
  end
end method refresh-project;
