Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Registry interface

define constant $use-registries      = #t;
define constant $dylan-abstract-host = "abstract://dylan/";

define method find-project-in-registry
    (workspace :: <dfmc-project-workspace>, name :: <library-name>)
 => (project :: false-or(<dfmc-project>))
  if ($use-registries)
    let processor = workspace.workspace-processor;
    let os = workspace.workspace-operating-system;
    let (location, read-only?) 
      = find-library-project-file(name, processor, os);
    location & open-project(workspace, location, read-only?: read-only?)
  end
end method find-project-in-registry;


/// Registry class

define class <registry> (<object>)
  constant slot registry-location :: <file-locator>, 
    required-init-keyword: location:;
  constant slot registry-root-directory :: <string>, 
    required-init-keyword: root-directory:;
  constant slot registry-personal? :: <boolean>, 
    required-init-keyword: personal?:;
  slot %host-info :: false-or(<list>) = #f;
end class <registry>;

define function find-library-project-file
    (workspace :: <dfmc-project-workspace>, name :: <library-name>)
 => (lid-location :: false-or(<file-locator>), read-only? :: <boolean>)
  let registries = workspace.workspace-registries;
  let (registry-entry, registry) 
    = find-registry-entry(name, registries);
  if (registry-entry)
    let prefix = $dylan-abstract-host;
    let prefix-size = prefix.size;
    let location
      = with-open-file (stream = registry-entry)
	  read-line(stream)
	end;
    let filename
      = if (location.size > prefix-size
	      & copy-sequence(location, end: prefix-size) = prefix)
	  let root = as(<directory-locator>, registry.registry-root);
	  let file = as(<posix-physical-locator>,
			copy-sequence(location, start: prefix-size));
	  // Can't just merge them as that would produce a POSIX locator...
	  let file-parent = locator-directory(file);
	  let file-parent-path = file-parent & locator-path(file-parent);
	  as(<string>, 
	     make(<file-locator>,
		  directory: make(<directory-locator>,
				  server: locator-server(root),
				  path: concatenate(locator-path(root) | #[],
						    file-parent-path | #[])),
		  name: locator-name(file)))
	else
	  location
	end;
    let read-only? = ~registry.registry-personal?;
    values(as(<file-locator>, filename), read-only?)
  else
    values(#f, #f)
  end
end function find-library-project-file;

define function find-registry-entry
    (name :: <library-name>, registries :: <simple-object-vector>)
 => (lid-locator :: false-or(<file-locator>),
     registry :: false-or(<registry>))
  let name = as-lowercase(as(<string>, name));
  block (return)
    for (registry :: <registry> in registries)
      let locator = merge-locators(as(<file-locator>, name),
				   registry.registry-location);
      if (file-exists?(locator))
	return(locator, registry)
      end
    end;
    values(#f, #f)
  end
end function find-registry-entry;

define function platform-registries
    (processor :: <processor>, os :: <operating-system>)
 => (registries :: <simple-object-vector>)
  let platform-registries :: <stretchy-object-vector> 
    = make(<stretchy-object-vector>);
  let generic-registries :: <stretchy-object-vector> 
    = make(<stretchy-object-vector>);
  let platform-name = platform-name(processor, os);
  local method add-registries
	    (paths :: <sequence>, #key personal? :: <boolean> = #f)
	  for (path :: <directory-locator> in paths)
	    let (platform-registry, generic-registry)
	      = make-registry-from-path(path, platform-name, personal?: #t);
	    add!(platform-registries, platform-registry);
	    add!(generic-registries, generic-registry)
	  end
	end method add-registries;
  add-registries(lookup-personal-registries(), personal?: #t);
  add-registries(lookup-system-registries(),   personal?: #f);
  concatenate-as(<simple-object-vector>,
		 platform-registries, generic-registries)
end function platform-registries;

define function make-registry-from-path
    (path :: <directory-locator>, platform-name :: <string>, 
     #key personal? = #f)
 => (platform-registry :: <registry>, generic-registry :: <registry>)
  let platform-registry = subdirectory-locator(path, platform-name);
  let generic-registry = subdirectory-locator(path, "generic");
  let root-directory = as(<string>, path.locator-directory);
  values(make(<registry>,
	      location:       platform-registry,
	      root-directory: root-directory,
	      personal?:      personal?),
	 make(<registry>,
	      location:       generic-registry,
	      root-directory: root-directory,
	      personal?:      personal?))
end function make-registry-from-path;

define function platform-name
    (processor :: <processor>, os :: <operating-system>)
 => (name :: <string>)
  concatenate(as-lowercase(as(<string>, processor)),
	      "-",
	      as-lowercase(as(<string>, os)))
end function platform-name;

define function lookup-system-registries 
    () => (registries :: <sequence>)
  let registries = system-registry-path();
  registries
    | vector(subdirectory-locator
	       (system-release-path(), "sources", "registry"))
end function lookup-system-registries;

define function lookup-personal-registries
    () => (registries :: <sequence>)
  user-registry-path()
end function lookup-personal-registries;
