Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $database-file-extension = "ddb";

// For now we have to subclass interactive-project because all projects have
// to have interactive contexts open
define class <binary-project> (<interactive-project>, <project>)
  constant slot %project-key :: <symbol>,
    required-init-keyword: key:;
  constant slot project-database-location :: <file-locator>,
    required-init-keyword: database:;
end;

define sealed domain make(singleton(<binary-project>));
define sealed domain initialize(<binary-project>);

define function find-binary-project
    (key, #key processor, operating-system)
 => (project :: false-or(<binary-project>));

  local method %db-location (install-locator :: <directory-locator>)
	  let directory = subdirectory-locator(install-locator, "databases");
	  let database-locator 
	    = make(<file-locator>,
		   directory: directory,
		   base:      as-lowercase(as(<string>, key)),
		   extension: $database-file-extension);
	  debug-out(#"project-manager", 
		    "Looking for %s\n", as(<string>, database-locator));
	  file-exists?(database-locator) & database-locator
	end;
  
  // look in the user's path
  let user-path = user-install-path();
  // system path
  let system-path = system-install-path();
  // release path
  let release-path = system-release-path();

  let database-locator = 
    (user-path & %db-location(user-path))
    |
    (system-path & %db-location(system-path))
    |
    (release-path & %db-location(release-path));

  database-locator & debug-out(#"project-manager",
                               "Found database: %s",
                               as(<string>, database-locator));
      
  database-locator & make(<binary-project>, 
			  database: database-locator,
			  key: as(<symbol>, locator-base(database-locator)))

end;

define method project-compilation-mode-setter(mode, project :: <binary-project>)
  mode
end;

define method project-personal-library?(project :: <binary-project>)
  => (well? :: <boolean>);
  #f
end;

define method project-build-location
    (project :: <binary-project>)
 => (location :: singleton(#f))
  #f
end;

define method project-profile-location
    (project :: <binary-project>)
 => (location :: singleton(#f))
  #f
end;

define method project-source-record-class
    (project :: <binary-project>)
 => (c :: <class>);
  <file-source-record>
end;

define method project-source-location
    (project :: <binary-project>)
 => (location :: <directory-locator>)
  project.project-database-location.locator-directory
end;

define method project-key?(project :: <binary-project>, key) => key?;
  key == project.%project-key
end;

define method project-read-only?(project :: <binary-project>)
  => (well? :: singleton(#t));
  #t
end;

define method project-current-source-records(project :: <binary-project>)
 => (records :: singleton(#[]));
  #[]
end;

/*---*** andrewa: this method gets in the way because the debugger
     *** relies on knowing the source records at the time the project
     *** was built. The next-method actually can return the source records
     *** so this should be a safe change.
define method project-canonical-source-records(project :: <binary-project>)
 => (records :: singleton(#[]));
  #[]
end;
*/

define method make-used-project (project :: <binary-project>,
				 key :: <symbol>, processor, os)
 => project :: <project>;
  make-project(<project>, parent: project,
	       key: key, processor: processor, operating-system: os);
end;

define method project-browsing-context(project :: <binary-project>)
 => context;
  project.project-current-compilation-context
end;

define method project-library-name(project :: <binary-project>)
  => (name :: <symbol>);
  project.%project-key
end;

define method project-inter-library-binding
    (project ::  <lid-project>, used-project :: <binary-project>) 
 => (mode :: one-of(#"tight", #"loose"));
  let bindings = project.project-library-loose-bindings;
  member?(used-project.%project-key, bindings) & #"loose"
    |
    #"tight"
end;

define method project-location
    (project :: <binary-project>)
 => (location :: false-or(<file-locator>));
  project.project-database-location
end;

define method project-target-type
    (project :: <binary-project>)
 => (type :: <project-target-type>);
  #"dll"
end;

define method project-executable-name
    (project :: <binary-project>)
 => (executable :: <string>)
  let key = project.%project-key;
  let info = find-library-info(key);
  case
    info      => info.info-binary-name;
    otherwise => as(<string>, key)
  end
end;

define method project-name(project :: <binary-project>)
  => (name :: <symbol>);
  project.%project-key
end;

define method project-dylan-sources(project :: <binary-project>)
  => (sources :: singleton(#[]));
  #[]
end;

define method %library-name-from-file(type == <binary-project>, 
				      database-locator :: <file-locator>)
 => (name :: false-or(<symbol>));
  as(<symbol>, locator-base(database-locator))
end;

define method project-other-sources(project :: <binary-project>)
 => (sources :: singleton(#[]));
  #[]
end;
