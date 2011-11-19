Module:  registry-projects-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <registry> (<object>)
  constant slot registry-location :: <directory-locator>,
    required-init-keyword: location:;
  constant slot registry-root :: <directory-locator>,
    required-init-keyword: root:;
  constant slot registry-personal? :: <boolean>, 
    required-init-keyword: personal?:;
  slot registry-settings-date = #f;
  slot registry-settings      = #f;
end class <registry>;

define method print-object (registry :: <registry>, stream :: <stream>)
 => ();
  format(stream, "{%s registry in %s}",
	 if (registry.registry-personal?) "personal" else "system" end,
	 as(<string>, registry.registry-location));
end method;

define class <registry-project-layout> (<project-layout>) 
  // Name under which is listed in registry
  constant slot project-registered-name :: <symbol>,
    required-init-keyword: key:;
  slot project-registry :: <registry>,
    init-keyword: registry:;
  slot project-personal-library? :: <boolean>,
    init-keyword: personal-library?:,
    setter: project-personal-library?-slot-setter;
end;

define method initialize (project :: <registry-project-layout>, #rest keys,
			  #key key, source-record-class, processor, operating-system, 
			  #all-keys)
  assert(instance?(key, <symbol>), "<registry-project-layout>: key not a symbol");
  let source-class = if(source-record-class) source-record-class
		     else <file-source-record> end;
  let (lid-location, registry)
    =  compute-library-location(key, processor, operating-system);
  project.project-registry := registry;
  project.project-personal-library? := registry.registry-personal?;
  apply(next-method, project, 
	source-record-class:, source-class,
	lid-location:, lid-location, 
	keys);

  let (builds-dir, db-dir, profile-dir) = project-build-locations(project);
  let personal? = project.project-personal-library?;

  project-build-location(project) := 
    personal? & library-build-locator(builds-dir, key);
  project-database-location(project) := 
    library-database-locator(db-dir, key);
  project-profile-location(project) := 
    library-profile-locator(profile-dir, key);
  let db = project-database-location(project);
  unless (file-exists?(db))
// This gets checked a bit later on, in project-open-compilation-context
// (which also covers cases not covered here, such as an incomplete or
//  obsolete database).
// roman: it's not costly but gives better error message in some cases
    if (~personal?)
      error("System project %s is missing compiler database", key)
    end;
    if (personal? & ~project.project-source-files)
      error("There is no source and no database to load for the project %s",
	    key)
    end
  end;
end method;

// XXX this is only needed for bootstrapping in the emulator
define method project-personal-library?-setter
    (flag, project :: <registry-project-layout>)
  if (flag)
    project.project-personal-library?-slot := #t;
  else
    project.project-build-location := #f;
    project.project-personal-library?-slot := #f;
  end;
end method;

define open class <registry-project> (<registry-project-layout>, <lid-project>)
  // slot project-loaded? :: <boolean> = #f;
end class;

define method project-location
    (project :: <registry-project>) => (location :: <file-locator>)
  project.project-lid-location
end method project-location;

define method initialize (project :: <registry-project>, #rest keys,
			  #key key, #all-keys)
  next-method();
  unless (project-lid-library-name(project) == key)
    user-warning("Library in project %s is actually called %s.\n",
		 key, project-lid-library-name(project));
  end;
end;

*default-project-class* := <registry-project>;

define method note-platform-change (project :: <registry-project>,
				    new-processor, new-os)
  let old-processor = project-compiler-setting(project, processor:);
  let old-os = project-compiler-setting(project, operating-system:);
  unless (old-processor == new-processor & old-os == new-os)
    let key = project.project-registered-name;
    let (lid-location, registry) = 
      compute-library-location(key, new-processor, new-os);
    if (lid-location ~= project.project-lid-location)
      // Have different sources for different platforms, so can't reuse.
      // It's not really necessary to close the project, but this way we
      // can be sure all projects are for the same platform, which might
      // simplify stuff, like looking up projects by name.
      close-project(project);
    else
      let personal? = registry.registry-personal?;
      project.project-personal-library? := personal?;
      let (builds-dir, db-dir, profile-dir) = project-build-locations(project);
      project-build-location(project) := 
	personal? & library-build-locator(builds-dir, key);
      project-database-location(project) := 
	library-database-locator(db-dir, key);
      project-profile-location(project) := 
	library-profile-locator(profile-dir, key);
      project-processor(project) := new-processor;
      project-operating-system(project) := new-os;
    end;
  end;
end method;

define method note-project-loaded (project :: <registry-project>)
  // nothing to do here now
end method;

define method note-compiling-definitions (project :: <registry-project>)
  // nothing to do here now 
end method;

define method update-project-location(project :: <registry-project>)
  let processor = project.project-processor;
  let os = project.project-operating-system;
  let key = project-registered-name(project);
  let (lid-location, registry) =
    compute-library-location(key, processor, os);
  let personal? = registry.registry-personal?;
  project.project-registry := registry;
  project.project-personal-library? := personal?;
  project.project-lid-location := lid-location;
  let (builds-dir, db-dir) = project-build-locations(project);
  project-build-location(project) := 
    personal? & library-build-locator(builds-dir, key);
  project-database-location(project) := 
    library-database-locator(db-dir, key);
  project-profile-location(project) := 
    library-profile-locator(db-dir, key);
end;

// Compute the sources records to give the compiler.
define method project-current-source-records (project :: <registry-project>)
 => sr*;

  if (project-personal-library?(project))
    block()
      compute-compiler-source-records(project)
    exception (<file-does-not-exist-error>)
      update-project-location(project);
      project.project-lid-date := #t; // Force mismatch (probabaly unnecessary)
      compute-compiler-source-records(project);
    end;
  else
    error("Internal error: project-current-source-records");
    // System projects cannot get here
  end;
end method;


define method make-used-project (project :: <registry-project>,
				 key :: <symbol>, processor, os)
                               => project :: <registry-project>;
  make-project(<registry-project>,
	       key: key,
	       source-record-class: project.project-source-record-class,
	       processor: processor,
	       operating-system: os)
end method;

define method project-key? (project :: <registry-project>,
			    key :: <symbol>)
                          => key?;
  project.project-registered-name == key |
    project.project-lid-library-name == key
end method;

define method project-source-location
    (project :: <registry-project>)
 => (location :: <directory-locator>)
  project.project-lid-location.locator-directory
end;

define method update-project-files (project :: <registry-project>) => ();
  // TODO: this needs to update database location if library-name
  // has changed 
  let lid-location = project.project-lid-location;
  let lid-date = file-property(lid-location, #"write-date");
  unless (project.project-lid-date = lid-date)
    let (library-name, files, properties) = read-lid-data(lid-location);
    let build-settings = lid-build-settings(lid-location, properties);
    unless (library-name == project.project-lid-library-name)
      user-warning("Library in project %s changed from %s to %s\n",
		project.project-name,
		project.project-lid-library-name, library-name);
    end;
    project.project-lid-library-name := library-name;
    project.project-source-files := files;

    project.project-lid-date := lid-date;

    reinitialize-lid-project(project);

  end;
end method;
