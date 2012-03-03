Module: registry-projects-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function lookup-system-registries ()
  let registries = system-registry-path();
  registries
    | list(subdirectory-locator(system-release-path(), "sources", "registry"))
end function;

define function lookup-personal-registries ()
  user-registry-path();
end function;

define function make-registry-from-path
    (path :: <directory-locator>, platform, 
     #key personal? = #f)
 => (platform-registry :: <registry>, generic-registry :: <registry>);
  let platform-registry = subdirectory-locator(path, platform);
  let generic-registry  = subdirectory-locator(path, "generic");

  values(make(<registry>,
	      location: platform-registry,
	      root: path.locator-directory,
	      personal?: personal?),
	 make(<registry>,
	      location: generic-registry,
	      root: path.locator-directory,
	      personal?: personal?))
end;

define function find-registries(processor, os)
  let registries = project-dynamic-environment(#"registries");
  if (registries)
    registries
  else
    project-dynamic-environment(#"registries") 
      := find-registries-internal(processor, os)
  end
end;

define function find-registries-internal
    (processor, os)
 => (registries :: <sequence>);
  debug-out(#"project-manager", "Finding registries");
  let platform = platform-namestring(processor, os);
  let generic-personal-registries = #();
  let platform-personal-registries = #();
  let personal-path = lookup-personal-registries();
  personal-path &
    map(method (path :: <directory-locator>)
	  debug-out(#"project-manager",
                    "Making personal registries for %s",
                    as(<string>, path));
	  let (platform-registry, generic-registry)
	    = make-registry-from-path(path, platform, personal?: #t);
	  platform-personal-registries 
	    := add!(platform-personal-registries, platform-registry);
	  generic-personal-registries
	    := add!(generic-personal-registries, generic-registry);
	end,
	reverse(personal-path));

  let generic-system-registries = #();
  let platform-system-registries = #();
  map(method (path :: <directory-locator>) 
	debug-out(#"project-manager",
                  "Making system registries for %s",
                  as(<string>, path));
	let (platform-registry, generic-registry) 
	  = make-registry-from-path(path, platform, personal?: #f);
	platform-system-registries
	  := add!(platform-system-registries, platform-registry);
	generic-system-registries
	  := add!(generic-system-registries, generic-registry);
      end,
      reverse(lookup-system-registries()));

  let registries 
    = concatenate(platform-personal-registries, 
		  platform-system-registries,
		  generic-personal-registries,
		  generic-system-registries);

  registries;
end;

define class <registry-entry-not-found-error> (<simple-error>)
end class <registry-entry-not-found-error>;

define function compute-library-location (key, processor, os)
  let platform = platform-namestring(processor, os);
  let registries = find-registries(processor, os);

  let (lid-location, registry)
    = find-library-locator(key, registries);
  unless (lid-location)
    signal(make(<registry-entry-not-found-error>,
	       format-string: "The project %= does not appear in the registry",
	       format-arguments: vector(key)));
  end;

  values(lid-location, registry)

end;

define function project-build-locations
    (project :: <registry-project-layout>)
 => (builds-locator   :: false-or(<directory-locator>), 
     database-locator :: <directory-locator>,
     profile-locator  :: <directory-locator>)
  let builds-locator = user-build-path();
  let install-locator = user-install-path();

  let database-locator = subdirectory-locator(install-locator, "databases");
  let profile-locator  = subdirectory-locator(install-locator, "profiles");
  ensure-directories-exist(builds-locator);
  ensure-directories-exist(database-locator);
  ensure-directories-exist(profile-locator);
  values(builds-locator, database-locator, profile-locator);
end function;

define function library-build-locator 
    (builds-loc :: <directory-locator>, library-name :: <symbol>)
 => (build-locator :: <directory-locator>)
  let directory
    = subdirectory-locator(builds-loc, as-lowercase(as(<string>, library-name)));
  ensure-directories-exist(directory);
  directory
end function;

define function library-database-locator 
    (database-loc :: <directory-locator>, library-name :: <symbol>)
 => (locator :: <file-locator>)
  make(<file-locator>,
       directory: database-loc,
       base:      as-lowercase(as(<string>, library-name)),
       extension: $dylan-database-suffix)
end function;

define function library-profile-locator 
    (profile-loc :: <directory-locator>, library-name :: <symbol>)
 => (profile-locator :: <file-locator>)
  make(<file-locator>,
       directory: profile-loc,
       base:      as-lowercase(as(<string>, library-name)),
       extension: $dylan-profile-suffix)
end function;
