Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant search-debug = method(#rest args) end;

define constant $user-projects-path = user-projects-path();

// this method tries to find the subproject in the cache
//
define method search-for-subproject(project :: <user-project>, key :: <symbol>,
				    procesor, os)
 => (location);
  let used-project-location = #f;
  let cached-location = element(project.%used-projects-cache, key, default: #f);
  let project-location = cached-location & first(cached-location);
  if(project-location & 
       project-location ~= "system-registry" & 
       project-location ~= "user-registry")

    used-project-location 
      := merge-locators(project-location, project.user-disk-project-file.locator-directory);
    unless(file-exists?(used-project-location))
      debug-message("Used Project file: %s no longer accessible",
		    as(<string>, used-project-location));
      user-warning("Used Project file: %s no longer accessible",
		   as(<string>, used-project-location));
      used-project-location := signal(make(<cannot-open-project-file-condition>,
					   project-file-location: used-project-location));
    end unless;
  end if;

  used-project-location;
end;

// the general searching algorithm
define method search-for-project(key :: <symbol>, 
				 #key search-path = $user-projects-path, 
				 depth = #f,
				 procesor, os)
  let project-file-name = concatenate(as(<string>, key), $user-project-suffix);
  local method lookup(projects-directory :: <directory-locator>, return :: <function>)
	  search-debug("search-for-project in %s\n", projects-directory);
	  if(file-exists?(projects-directory))
	    let project-file = find-project-in-subdirectory(projects-directory, 
							    project-file-name,
							    depth: depth);
	    project-file & return(project-file)
	  end
	end method;
  let project-location = 
    block(return)
      search-path
	& do(rcurry(lookup, return), search-path);
      #f
    end block;
  project-location
end;

define function find-project-in-subdirectory(dir :: <directory-locator>, 
					     name :: <byte-string>,
					     #key depth)
 => (locator :: false-or(<file-locator>));
  block(found)
    local method check-name(dir, file-name, type)
	    search-debug("check dir: %s name: %s type: %s\n", dir, file-name, type);
	    let file = 
	      type = #"file" & name = file-name 
	      & make(<file-locator>, directory: dir, name: file-name);
	    file & search-debug("**check dir: %s name: %s type: %s\n", 
				dir, file-name, type);
	    file & found(file);
	  end method;
    dir.locator-directory	// don't search from root
      &
      do-directory-recursively(check-name, dir, 
			       recursive-level: depth | 2);
  end block;
end;

/***
Signature of <function> below
define function process-directory-entry(dir :: <pathname>,
					name :: <byte-string>,
					type :: <file-type>)
 => (value :: <object>);
***/

define method do-directory-recursively(f :: <function>, 
				       directory :: <directory-locator>,
				       #key test = method(dir, name, type) 
						       #t
						   end,
				            recursive-level = 0)
  test(directory, #f, #"directory") &
    %do-directory-recursively(f, directory, 
			      test: test, 
			      recursive-level: recursive-level)
end;

define method %do-directory-recursively(f :: <function>,
					directory :: <directory-locator>,
					#key test, recursive-level)
  local method browse-directory(dir :: <directory-locator>,
				name :: <byte-string>,
				type :: <file-type>)
	  search-debug("Directory: %s, name: %s, type: %s\n", dir, name, type);
	  test(dir, name, type) & 
	    begin
	      f(dir, name, type);
	      if((recursive-level > 0) & (type == #"directory") &
		   name ~= ".." & name ~= "." )
		let new-level = recursive-level - 1;
		let sub-dir = subdirectory-locator(dir, name);
		%do-directory-recursively(f,
					  sub-dir,
					  test: test,
					  recursive-level: new-level);
	      end if;
	    end begin;
	end method;

  search-debug("Directory: %s, level: %d\n", directory, recursive-level);
  do-directory(browse-directory, directory);
  #f;
end;

define class <project-cannot-be-compiled-against> (<project-warning>)
  constant slot condition-project-name :: <symbol>,
    required-init-keyword: project:;
  constant slot condition-subproject-name :: <symbol>,
    required-init-keyword: subproject:;
end;

define method condition-to-string (c :: <project-cannot-be-compiled-against>)
 => (string :: <string>)
  format-to-string("Project %s is read-only and cannot be compiled \nbut it depends on modified project %s", c.condition-project-name, c.condition-subproject-name);
end method;


define sideways method note-used-project (project :: <project>, 
					  subproject :: <project>)
//  format-out("used-project: %s uses %s\n", 
//	     project.project-registered-name,
//	     subproject.project-registered-name);

  if(project.project-read-only? & ~subproject.project-read-only?)
    signal(make(<project-cannot-be-compiled-against>, 
		format-string: "Project %s is read-only and cannot be compiled but it depends on modified project %s",
		format-arguments: list(project.project-name, subproject.project-name),
		project: project.project-name, subproject: subproject.project-name))
  end;
end;
  
define method note-used-project (project :: <user-project>, 
				 subproject :: <project>)
  project.%user-project-used-projects[subproject.project-library-name] := 
    subproject;
  next-method();
end;
  
define method 
    note-used-project (project :: type-union(<system-project>, <binary-project>), 
		       subproject :: <project>)
  next-method();
end;

define generic project-flush-caches(project :: <project>,
				    #key recursive?) => ();

define method project-flush-caches(project :: <project>,
				   #key recursive?) => ();
// do nothing
end;

define method project-flush-caches(project :: <user-project>, 
				     #key recursive? = #t) => ();
  %remove-caches(project);
  if(recursive?)
    do(project-flush-caches, project.%user-project-used-projects);
  end;
end;

define function cache-file-location
    (project :: <user-project>) => (locator :: <file-locator>)
  make(<file-locator>,
       directory: project.project-build-location,
       name:      as(<string>, project.project-name),
       extension: $cache-extension)
end function cache-file-location;

define function %remove-caches(project :: <user-project>) => ();
  let cache-locator = cache-file-location(project);
  debug-message("Flushing caches for project %s", project.project-name);
  block()
    delete-file(cache-locator);
  exception(<file-system-error>)
  end;
  project.%used-projects-cache := #f;
  project.%compiled-source-records := #();
  project.%tools-cache := make(<table>);
  project.%user-project-used-projects := make(<table>);
end;

define function project-initialize-caches(project :: <user-project>)
  let version = 
    string-to-integer(first(project-keyword-property(project, #"Format-version",
						     default: #("1"))));
  local method retrieve-caches(cache-table)
	  let cache = element(cache-table, #"used-projects", default: #f);
	  local method convert-upc (p, f, b)
		  values(p,
			 if (f = "system-registry" | f = "user-registry")
			   f
			 else
			   as(<file-locator>, f)
			 end,
			 if (b = "system")
			   b
			 else
			   as(<directory-locator>, b)
			 end)
		end method;
	  project.%used-projects-cache
	    := if (cache)
		 destructure-flat-assoc-list(cache, 3, convert: convert-upc)
	       else
		 make(<table>)
	       end;

	  let tools-cache = 
	    destructure-flat-assoc-list(element(cache-table, 
						#"tools-cache", 
						default: #[]),
					1,
					convert: method(e) as(<symbol>, e) end);
	  project.%tools-cache := make(<table>);
	  local method convert(f,d)
		  values(merge-locators(as(<file-locator>, f),
					project.project-source-location), 
			 make(<date>, iso8601-string: d))
		end;
	  for(tool in tools-cache)
	    project.%tools-cache[tool] := 
	      destructure-flat-assoc-list(element(cache-table, 
						  tool,
						  default: #[]),
					  2,
					  convert: convert);
	  end;

	  let compiled-records = element(cache-table,
					 #"canonical-sources", default: #());
	  project.%compiled-source-records := 
	    map(curry(project-id-source-record, project), compiled-records);
	end;

  unless(project.%used-projects-cache)
    if(version = 1)
      retrieve-caches(project.project-lid-file-info);
      //fixup the project file
      local method maybe-add-subproject(info)
	      let project-file = as(<file-locator>, first(info));
	      if (locator-extension(project-file) = $user-project-suffix)
		project-add-file(project, project-file);
	      end;
	    end;
      do(maybe-add-subproject, project.%used-projects-cache);
    elseif(version = 2)
      let cache-locator = cache-file-location(project);
      block()
	let properties = read-file-header(cache-locator);
	retrieve-caches(properties);
      exception(<file-does-not-exist-error>)
	project.%used-projects-cache := make(<table>);
	project.%tools-cache := make(<table>);
	debug-message("Couldn't find cache file: %s", as(<string>, cache-locator))
      end;
    end;
  end;
end;

define method make-used-project (project :: <user-project>,
				 key :: <symbol>, processor, os)
 => project :: <project>;
  project-initialize-caches(project);
  // search in the cache
  let subproject-location = search-for-subproject(project, key, processor, os);
  let subproject = subproject-location & 
    block()
      let build-entry = second(element(project.%used-projects-cache, key));
      let read-only? = (build-entry = "system");

      // at this time we don't use the cached build-location for the purpose 
      // of the compiler, i.e. the environment in which the project was run
      // previously has to be the same as the current environment
      // i.e. if HD_USER_BUILD was set it has to be set now and to the same value
      debug-message("Project: %s found subproject %s in cache", 
		    project.project-name, key);
      
      let used-project = 
	make-project-from-file(subproject-location,
			       parent: project,
			       key: key,
			       read-only?: read-only?,
			       processor: processor, operating-system: os);
      used-project
    exception(<file-does-not-exist-error>)
      debug-message("Cannot open project in location: %s",
		    as(<string>, subproject-location));
      // this is extraneous, since it's been checked in search-for-subproject
      user-warning("Cannot open project in location: %s",
		   as(<string>, subproject-location));
      #f
    end;
/*
  // try to find in a sibling directory
  subproject := 
    search-for-project(key, 
		       search-path: 
			 list(project.project-source-location.locator-directory));
*/
  subproject | 
    make-project(<project>, parent: project,
		 key: key, processor: processor, operating-system: os);

end method;

define method make-used-project (project :: <system-project>,
				 key :: <symbol>, processor, os)
                               => project :: <project>;
  // TO DO:
  // this may be an incorrect assumption
  // i.e. that <system-project>s cannot use <user-project>s
  // debug-message("Project: %s making used project: %s", project, key);
  make-project(<project>, parent: project, key: key, processor: processor, operating-system: os)
end method;
