Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <dfmc-binary-project> (<dfmc-project>)
  sealed slot project-name :: <project-name>,
    required-init-keyword: name:;
  sealed slot project-database-location :: <file-locator>,
    required-init-keyword: database-location:;
end class <dfmc-binary-project>;

define sealed domain make (singleton(<dfmc-binary-project>));
define sealed domain initialize (<dfmc-binary-project>);

define function open-binary-project
    (workspace :: <dfmc-project-workspace>, location :: <file-locator>)
 => (project :: <dfmc-binary-project>)
  make(<dfmc-binary-project>, 
       database-location: location,
       name: location.locator-base)
end function open-binary-project;

define function find-binary-project
    (workspace :: <dfmc-project-workspace>, name :: <library-name>)
 => (project :: false-or(<dfmc-binary-project>))
  block (return)
    local method maybe-return-project
	      (install-locator :: false-or(<file-locator>)) => ()
	    if (install-locator)
	      let databases-directory
		= ensure-directory-exists(install-locator, "databases");
	      let database-location
		= make(<file-locator>,
		       directory: databases-directory, 
		       base:      as(<string>, name),
		       extension: database-file-extension());
	      if (file-exists?(database-location))
		let project
		  = make(<dfmc-binary-project>,
			 database-location: database-location,
			 name: database-location.locator-base);
		return(project)
	      end
	    end
	end method maybe-return-project;
    maybe-return-project(user-install-path());
    maybe-return-project(system-install-path());
    maybe-return-project(system-release-path());
    #f
  end
end function find-binary-project;

define method project-library-name
    (project :: <dfmc-binary-project>) => (name :: <library-name>)
  //---*** Surely we should get this from the database?
  project.project-name
end method project-library-name;

define method project-read-only?
    (project :: <dfmc-binary-project>) => (read-only? == #t)
  #t
end method project-read-only?;

define method project-directory
    (project :: <dfmc-binary-project>)
 => (location :: <directory-locator>)
  project.project-database-location.locator-directory
end method project-directory;

define method project-source-files
    (project :: <dfmc-binary-project>)
 => (source-files :: <simple-object-vector>)
  #[]
end method project-source-files;

define method project-filename
    (project :: <dfmc-binary-project>) => (filename :: <string>)
  // This information isn't available (yet) in the compiler database.
  // For the moment only our system projects will become binary projects,
  // so we can use the release-info library to get the information.
  //---*** This must be fixed to allow users to use binary projects.
  let name = project.project-library-name;
  let info = find-dll-group(name);
  case
    info      => info.info-dll-name;
    otherwise => as(<string>, name)
  end
end method project-filename;
