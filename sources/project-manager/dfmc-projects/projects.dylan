Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed class <dfmc-hdp-project> (<dfmc-project>, <basic-project>)
end class <dfmc-hdp-project>;

define sealed domain make (singleton(<dfmc-hdp-project>));
define sealed domain initialize (<dfmc-hdp-project>);

define function open-hdp-project
    (workspace :: <dfmc-project-workspace>, location :: <file-locator>)
 => (project :: <dfmc-hdp-project>)
  read-project-file(workspace, location, $project-file-format-version)
end function open-hdp-project;

define method make-workspace-project
    (workspace :: <dfmc-project-workspace>, #rest args, #key, #all-keys)
 => (project :: <dfmc-project>)
  apply(make, <dfmc-hdp-project>,
	workspace: workspace,
	args)
end method make-workspace-project;


/*
/// Old code
define constant $user-project-suffix = ".hdp";
define constant $lid-project-suffix = ".lid";
define constant $binary-project-suffix = ".ddb";

define constant $dylan-file-type = #".dylan";
define constant $C-source-file-type = #".C";
define constant $C-header-file-type = #".h";
define constant $C-object-file-type = #".obj";
define constant $C-libraries-file-type = #".lib";
define constant $rc-file-type = #".rc";
define constant $ico-file-type = #".ico";
define constant $bmp-file-type = #".bmp";

define constant $include-file-types = type-union(singleton($C-header-file-type),
						 singleton($ico-file-type),
						 singleton($bmp-file-type));

define constant $project-file-type = #".hdp";
define constant $database-file-type = #".ddb";

define constant $project-file-types = type-union(singleton($project-file-type),
						 singleton($database-file-type));

define class <find-project-location-restart> (<simple-restart>)
  constant slot condition-project-location :: <file-locator>, 
    required-init-keyword: location:;
end;

define class <project-not-found> (<simple-error>) 
  constant slot condition-project-name :: <symbol>, 
    required-init-keyword: project-name:;
end;

define method make
    (class == <project-not-found>, #rest keys, #key project-name :: <symbol>)
 => (error :: <project-not-found>)
  apply(next-method, class,
	format-string: "Project %= was not found",
	format-arguments: vector(project-name),
	keys)
end method make;

define method print-object (c :: <find-project-location-restart>, 
			    stream :: <stream>)
 => ();
  format(stream, "Project location restart: %s\n", c.condition-project-location);
end method;

define class <yes-or-no-condition> (<simple-condition>)
end;

define method make(condition :: subclass(<yes-or-no-condition>), 
		   #rest keys, #key yes-or-no)
 => (condition :: <yes-or-no-condition>)
  apply(next-method, condition, 
	format-string: yes-or-no, format-arguments: vector(), keys)
end;

define method print-object (c :: <yes-or-no-condition>,
			    stream :: <stream>)
 => ();
  let text = apply(format-to-string, c.condition-format-string,
		   c.condition-format-arguments);
  format(stream, "Yes-or-no: %s\n", text);
end method;

define class <duplicate-project-condition> (<yes-or-no-condition>)
  constant slot duplicate-project-key :: <symbol>,
    required-init-keyword: key:;
end;

define class <cannot-open-project-file-condition> (<simple-condition>)
  constant slot condition-project-file-location :: <file-locator>,
    required-init-keyword: project-file-location:;
end;

// system-projects are registry projects
// at this time they can be read-only or editable if they are in 
// the personal registry
// at some point we probably should get rid of personal registries 
// then we can uncomment the two methods below
//
define class <system-project> (<registry-project>, <interactive-project>)

end;

define sealed domain make(singleton(<system-project>));
define sealed domain initialize(<system-project>);

/*
define method project-personal-library?(project :: <system-project>)
  #f
end;
*/

// user projects are editable, can reside anywhere on disk
// 
define sealed class <user-project> (<user-disk-project-layout>, 
				    <lid-project>, 
				    <interactive-project>)
  constant slot %source-record-table = make(<equal-table>);
  constant slot %subproject-files = make(<stretchy-vector>);
  slot %user-project-used-projects = make(<table>);
  slot %compiled-source-records = #();
  slot %used-projects-cache = #f;
  slot %tools-cache = #f;
  slot %project-read-only? = #f;
end;

define sealed domain make(singleton(<user-project>));
define sealed domain initialize(<user-project>);

define function %set-target-values(c, processor, operating-system)
 => (processor, operating-system);
  unless (processor & operating-system)
    let (default-processor, default-os) = default-platform-info(c);
    unless (processor) processor := default-processor end;
    unless (operating-system) operating-system := default-os end;
  end;
  values(processor, operating-system)
end;

define generic project-browsing-context(project :: <project>)
 => (context);

define method project-browsing-context(project :: <user-project>)
 => (context);
  project.project-execution-context 
    | 
    project.ensure-project-database
end;

define method project-browsing-context(project :: <system-project>)
 => (context);
  project.project-execution-context 
    | 
  project.ensure-project-database
end;

define method note-project-loaded (project :: <user-project>)
// fixup the library name in the hdp/makefile file
  let context = project.project-current-compilation-context;
  let name = compilation-context-library-name(context);
  let old-name = project.project-lid-library-name;
  if(name)
    let symbol-name :: <symbol> = as(<symbol>, name);
    unless(symbol-name == old-name)
      project.project-lid-library-name := symbol-name;
      debug-message("Changing library name keyword to %s", name);
    end;
  end;
end method;

define constant $replace-project-string = 
  "Project defining library %s is already open as %s\nReplace it ?";
  
define function %project-replace-project-ask(project :: <project>, 
					     close? :: <boolean>)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  debug-message("Asking if to replace %= %s", project, project.project-name);
  let key = project.project-library-name;
  let text = format-to-string($replace-project-string, 
			      key, project.project-location);
  let condition = make(<duplicate-project-condition>,
		       key: key,
		       yes-or-no: text);

  let yes? = 
    signal(condition);
  if(yes? & close?)
    // close? is acted on only if answer is yes 
      %close-project(project); 
      values(#t, #f)
  else
    values(yes?, project)
  end; 
end;

define generic project-replace-project-with?(c :: subclass(<project>),
					     project :: <project>,
					     #key, #all-keys)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));

define method project-replace-project-with?(c == <system-project>,
					    project :: <user-project>,
					    #key 
					    key :: <symbol>,
					    close? = #t)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  project-warning(project, "Cannot replace user project in %s with system project %s",
		  project.user-disk-project-file, key);
  values(#f, project)
end;

define method project-replace-project-with?(c == <system-project>,
					    project :: <system-project>,
					    #key 
					    key :: <symbol>,
					    close? = #t)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  if(close?)
    %close-project(project);
    values(#t, #f);
  else
    values(#t, project)
  end;
end;

define method project-replace-project-with?
    (c == <user-project>,
     project :: <user-project>,
     #key project-file :: <file-locator>,
          close? = #t)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
     
  if(project.user-disk-project-file = project-file)
    debug-message("project file %s is the same as %s", 
		  project-file, project.user-disk-project-file);
    values(#f, project)
  else
    %project-replace-project-ask(project, close?)
  end; 
end method;

define method project-replace-project-with?(c == <user-project>,
					    project :: <system-project>,
					    #key always-replace-system? = #t,
					    project-file :: <file-locator>,
					    close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
     
  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;
end;

define method project-replace-project-with?(c == <user-project>,
					    project :: <binary-project>,
					    #key always-replace-system? = #t,
					    project-file :: <file-locator>,
					    close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
     
  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;
end;

define method project-replace-project-with?(c == <binary-project>,
					    project :: <system-project>,
					    #key always-replace-system? = #t,
					    project-file :: <file-locator>,
					    close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
     
  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;
end;

define method project-replace-project-with?(c == <binary-project>,
					    project :: <user-project>,
					    #key always-replace-system? = #t,
					    project-file :: <file-locator>,
					    close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
     
  %project-replace-project-ask(project, close?)
end;

define method project-replace-project-with?(c == <binary-project>,
					    project :: <binary-project>,
					    #key always-replace-system? = #t,
					    project-file :: <file-locator>,
					    close? = #t, force? = #f)
 => (yes-or-no :: <boolean>, project :: false-or(<project>));
  if(always-replace-system?)
    %close-project(project);
    values(#t, #f)
  else
    %project-replace-project-ask(project, close?)
  end;

end;

define method project-remove-build-products(project :: <user-project>,
					    #key recursive? = #f);
  next-method();
  project-flush-caches(project, recursive?: #f);
end;

// external interface
// this method converts a lid-project into a user-project
define method import-lid-project
    (lid-location :: <file-locator>, 
     #key to-file :: false-or(<file-locator>))
  let project-location
    = to-file & merge-locators(to-file, lid-location.locator-directory);
  let project
    = begin
	let (yes?, project) = 
	  replace-project-with?(<user-project>, 
				// we pass lid-location on purpose
				// since project-location doesn't exist yet
				project-file: lid-location);
	if (yes?)
	  let p = %import-lid-project(lid-location, to-file: project-location);
	  p
	else
	  debug-message("Importing of %s aborted, returning opened project", lid-location);
	  project-warning(project, "Project %s has not been imported", lid-location);
	  project
	end
      end;
  when (project & project.project-namespace-loaded)
    close-unused-projects();
  end;
  project
end;

// internal method for use inside 'make' methods
define method %import-lid-project(lid-location :: <file-locator>,
				  #rest keys,
				    #key to-file = #f, 
				  make-method = make-project,
				  #all-keys) 
 => (project :: <user-project>);
  let project-location = to-file | make(<file-locator>,
					directory: locator-directory(lid-location),
					base: locator-base(lid-location),
					extension: $user-project-suffix);
  let ok? =
    block()
      copy-file(lid-location, project-location, if-exists: #"replace");
      file-property(project-location, #"writeable?") := #t;
      #t
    exception(e :: <file-system-error>)
      project-warning(project, "Project %s has not been imported due to file system error", 
		      lid-location);
      apply(user-warning, e.condition-format-string, e.condition-format-arguments);
      #f
    end;

  if(ok?)
    debug-message("Importing %s to %s", as(<string>, lid-location), 
		  as(<string>, project-location));

    apply(make-method, <user-project>, project-file: project-location, keys);
  else
    #f
  end;
end;

// this method is used when we know for sure that no project defining 
// the same library is open - this can happen when the compiler is asking 
// for a subproject
define method make-project-from-file(file :: <file-locator>, #rest keys, #key, #all-keys)
 => (project :: false-or(<project>));
  apply(primitive-make-project-from-file, file, make-method: make-project, keys)
end;

// this method is a primitive version of the above
// this method uses make by default to make it possible to call it from within make
define method primitive-make-project-from-file(file :: <file-locator>, 
					       #rest keys, #key make-method = make,
					       #all-keys)
 => (project :: false-or(<project>));
  let (key, project-class, init-keyword) =
    project-data-from-file(file);
  if(key)
    select(project-class)
      <lid-project> => apply(%import-lid-project, file, 
			     make-method: make-method, keys);
      <user-project>, <binary-project> =>
	apply(make-method, project-class, init-keyword, file, key: key, keys);
      otherwise => #f;
    end
  end
end;

// This method goes through the replace-project-with? protocol
// internal interface used for opening subprojects
define method open-project-from-file(location :: <file-locator>, 
				     #rest keys, #key, #all-keys)
 => (project :: false-or(<project>));
  
  let (key, project-class, init-keyword) =
    project-data-from-file(location);
  if(key)
    let (yes?, opened-project, key) = 
      apply(replace-project-with?, project-class, 
	    key: key, project-file: location, keys);

    if(yes?)
      apply(make-project, project-class, init-keyword, location, key: key, keys)
    else
      opened-project
    end
  else
    #f
  end 
end;

define method close-project(project :: <user-project>, #key system?)
 => (ok? :: <boolean>);
  let ok? = next-method();
  if(ok? & ~project.project-namespace-loaded)
    let user-projects = project.project-user-projects;
    debug-out(#"project-manager", "Closing subprojects of user-project %s: %s\n",
	      project.project-name, map(project-name, user-projects));
    do(close-project, user-projects)
  end;
  ok?
end;

// this is an external interface
define sideways method open-project(project-file-location :: <file-locator>)
 => (project :: false-or(<user-project>));
  let extention = locator-extension(project-file-location);
  select(extention by \=)
    $user-project-suffix =>  open-hdp-project(project-file-location);
    $lid-project-suffix => import-lid-project(project-file-location);
    otherwise => #f
  end;
end;

define function open-hdp-project(project-file-location :: <file-locator>)
 => (project :: false-or(<user-project>));
//  debug-assert(locator-extension(project-file-location) = $user-project-suffix,
//	       "%s is not a project file", project-file-location);

  let (processor, operating-system) = values(#f, #f);
  let project-location = project-file-location;

  let project
    = begin
	let (yes?, opened-project) = 
	  replace-project-with?(<user-project>, project-file: project-location);
	if(yes?)
	  let project = make-project(<user-project>, project-file: project-location);
	  project
	else
	  debug-message("Open-project: returning already opened project %= %s",
			opened-project, opened-project & opened-project.project-name);
	  if(opened-project)
	    debug-message("The project is already open as %s ", project-location)
	  else
	    user-warning("Couldn't open project in %s ", project-location)
	  end;
	  opened-project
	end;
      end;
  when (project & project.project-namespace-loaded)
    close-unused-projects();
  end;
  project
end;

define function %cached-subprojects(project :: <user-project>)
 => (projects :: <sequence>);
  let table = project.%user-project-used-projects;
  let keys = table.key-sequence;
  map(method(k) table[k] end, keys);
end;

define method all-used-projects(project :: <user-project>, #key system?)
 => (projects :: <sequence>);
  ignore(system?);
  if(project.project-namespace-loaded)
    next-method();
  else
    // project not compiled
    %cached-subprojects(project)
  end;
end;

define method directly-used-projects(project :: <user-project>, #key system?)
 => (projects :: <sequence>);
  ignore(system?);
  if(project.project-namespace-loaded)
    next-method();
  else
    // project not compiled
    %cached-subprojects(project)
  end 
end;

// TO DO: save and lookup in the cache %user-project-used-projects
// this cache is currently filled by the compiler calls
// NO -  we have to use %subproject-files since we know they were added
// explicitly by the user
define function project-user-projects(project :: <project>)
 => (projects :: <sequence>);
  
  let used-projects = make(<stretchy-vector>);
  for(f in project.%subproject-files)
    let project-location 
      = merge-locators(f, project.user-disk-project-file.locator-directory);
    let project-file = as(<string>, project-location);

    unless(member?(project-file, project-build-property(project, #"broken-files") | #[], test: \=))
      let library-name = library-name-from-file(f);
      let used-project 
	= with-compiler-lock ()
	    lookup-named-project(library-name, create?: #f);
	  end;
      used-project & add!(used-projects, used-project)
    end;
  end;
  used-projects
  
end;

define method note-project-made(project :: <user-project>, #key parent) => ();
  let subprojects = project-keyword-property(project,
					     #"subprojects", default: #());
  for(s in subprojects)
    project-add-file(project, s, save?: #f);
  end;

  project-initialize-caches(project);
end;

define method project-key? (project :: <user-project>,
			    key :: <symbol>)
                          => key?;
  project.project-lid-library-name == key
    |
    project.project-name == key
end method;

// TO DO: this should be changed to reflect the hdp file name
define method project-registered-name (project :: <user-project>) 
 => (name :: <symbol>);
  project.project-lid-library-name
//  locator-base(project.project-file-location)
end;

define method project-name
    (project :: <user-project>) => (name :: <symbol>)
  as(<symbol>, locator-base(project.project-file-location))
end method project-name;

define method project-compiler-source-location
    (target :: <dfmc-build-target>)
 => (location :: <directory-locator>);
  if (target.target-copy-sources?)
    target.target-build-location
  else
    target.target-source-location
  end
end method project-compiler-source-location;

define generic project-source-record-location(project :: <project>, 
					      sr :: <source-record>)
 => (location :: false-or(<file-locator>));

define method project-source-record-location(project :: <project>,
					     sr :: <source-record>)
 => (location :: false-or(<file-locator>));
  sr.source-record-location
end;

define method project-source-record-location(project :: <user-project>,
					     sr :: <file-source-record>)
 => (location :: false-or(<file-locator>));
  if(*copy-canonical-sources?*)
    let sr-name = sr.source-record-name;
    sr-name & any?(method(sr)
		       sr.source-record-name = sr-name & sr.source-record-location
		   end,
		   compute-project-source-records(project));
  else
    sr.source-record-location
  end;  
end;

define function project-id-source-record(project :: <user-project>, id) => sr;
  let table = project.%source-record-table;
  let str = as(<string>, id);
  let record = element(table, str, default: #f);
  record |
    (table[str] := 
       id-as-source-record(project-source-record-class(project),
			   project,
			   project-source-location(project), id))
end function;

define function compute-project-source-records(project :: <user-project>)
 => sr*;
  local method id-source-record (id) => sr;
	  project-id-source-record(project, id)
	end method;

  let new-user-records = 
      project-files-to-source-records(project,
				      directory: project.project-source-location,
				      files: project.project-source-files,
				      id-source-record: id-source-record);
  new-user-records
end;

define method project-verify-source-records(project :: <user-project>)
 => (records :: <sequence>);
  if(*copy-canonical-sources?*)
    block()
      compute-project-source-records(project)
    exception(e :: <source-record-error>)
      apply(project-serious-warning, project,
	    e.condition-format-string, e.condition-format-arguments);
      #()
    end;
  else
    next-method()
  end
end;

define method update-project-files (project :: <user-project>) => ();
  if(*copy-canonical-sources?*)
    let new-user-records = compute-project-source-records(project);
      
    let changed-records = choose(method(r) 
				     ~member?(r, project.%compiled-source-records)
				 end,
				 new-user-records);
    unless(empty?(changed-records))
      for(sr in changed-records)
	let source-file = source-record-location(sr);
	let target-file
	  = make(<file-locator>,
		 directory: project.project-build-location, 
		 name: locator-name(source-file));
	debug-message("  Copying %s to build area", as(<string>, source-file));
		
	block()
	  copy-file(source-file, target-file, if-exists: #"replace")
	exception(e :: <file-system-error>) 
	  debug-assert(#f, condition-format-string(e), condition-format-arguments(e))
	end block;
      end;
      project.%compiled-source-records := new-user-records;
    end;
  end;
  // do nothing - we don't allow for changes of HDP files 
  // outside of the environment
  // well, now we do ;-)
end;

define function test-tool(file :: <file-locator>, 
			  project-file :: <file-locator>, 
			  last-run :: false-or(<date>))
 => (success? :: <boolean>, hdp-modified? :: <boolean>, new-projects :: <sequence>);
  debug-message("Test tool processing file %s last run: %s",
		as(<string>, file), last-run & as-iso8601-string(last-run));
  let key = project-data-from-file(project-file);
  let project = lookup-named-project(key, create?: #f);
  // let's see if the project file will be read in again
  project-add-list-property(project, #"other-files", "foo.bar");
  save-project(project);
  project-remove-list-property(project, #"other-files", "foo.bar");
  values(#t, #t, #[])
end;

tool-register(#".foo", test-tool);

define method note-loading-namespace(project :: <user-project>) => ();
  if(project-dynamic-environment(#"compiler-transaction"))
  // we are starting a compiler operation
    debug-assert(~project.project-execution-context, 
		 "Cannot compile %s while connected to the application",
		 project.project-name);

    let other-files = project-build-property(project, #"other-files") | #[];
    local method process-file(f)
	    let full-path = merge-locators(f, project.project-source-location);
	    let tool-name :: false-or(<symbol>) = 
	      tool-name-from-specification(full-path);
	    let tool = tool-name & tool-find(tool-name);
	    if(tool)
	      let tool-cache = element(project.%tools-cache, tool-name, default: #f);
	      local method last-run-date(f, cache)
		      block(found)
			debug-out(#"project-manager", "Looking for %s in tool cache\n",
				  as(<string>, f));
			for(e in cache, el from 0 by 1)
			  debug-out(#"project-manager", "Checking %s in tool cache\n",
				    as(<string>, first(e)));
			  if(first(e) = f)
			    found(second(e), el)
			  end
			end;
			values(#f, #f)
		      end;
		    end;

	      let (last-run, el) = if(tool-cache)
				     last-run-date(full-path, tool-cache);
				   else
				     project.%tools-cache[tool-name] := make(<stretchy-vector>);
				     values(#f, #f);
				   end;
	      block(return)
		let handler <tool-warning-condition> 
		  = method(e, next-handler)
			user-warning(condition-to-string(e));
			unless(tool-warning-recoverable?(e))
			  return()
			end;
		    end; 
		let handler <tool-yes-no-question>
		  = method(e, next-handler)
			let question = apply(format-to-string, e.condition-format-string,
					     e.condition-format-arguments);
			signal(make(<yes-or-no-condition>,
				    yes-or-no: question))
		    end;
		let (success?, hdp-modified?, new-projects)
		  = tool(full-path, project.user-disk-project-file, last-run);
		if(success?)
		  if(el)
		    (project.%tools-cache[tool-name])[el] := list(full-path, current-date())
		  else
		    add!(project.%tools-cache[tool-name], list(full-path, current-date()))
		  end;
		  hdp-modified? & project-read-project-file(project);
		    
		end;
	      end;
	    end;
	  end;

    do(process-file, other-files);

  end;
end;

// this function re-reads the project file
define function project-read-project-file(project :: <user-project>) => ();
  let (library-name, files, properties) = read-lid-data(project.user-disk-project-file);
  project-source-files(project) := files;
  project-lid-file-info(project) := properties;
  reinitialize-lid-project(project);
  let subprojects = project-keyword-property(project,
					     #"subprojects", default: #());
  for(s in subprojects)
    project-add-file(project, s, save?: #f);
  end;

end;

// Compute the sources records to give the compiler.
define method project-current-source-records (project :: <user-project>)
 => sr*;
  block()
    compute-compiler-source-records(project)
// TO DO: should we catch it ?
//  exception (<file-does-not-exist-error>)

  end;
end method;

define method note-compiled-definitions(project :: <user-project>)
  //---*** andrewa: we don't need to save the project, as the dynamic
  //---*** information is all in the caches.
  // save-project(project);
  save-project-caches(project);
  next-method();
end;

define constant $driver-debug = #"driver";
define constant $pm-debug = #"project-manager";

define method %debug-pm() => foo;
  %debugging(#"project-manager")
end;

define method %debugging(subsystem-string :: type-union(<string>, <symbol>), #key on :: <boolean> = #t)
 => (status :: <boolean>);
  let subsystem :: <symbol> = as(<symbol>, subsystem-string); 
  if(on)
    *debug-out* := pair(as(<symbol>, subsystem), *debug-out*)
  else
    *debug-out* := remove(*debug-out*, as(<symbol>, subsystem))
  end;
  if(~empty?(*debug-out*))
    debug-message("Debugging subsystems: %s", *debug-out*);
    #t
  else
    #f
  end;
end;

define function project-build-locations
    (project :: <registry-project-layout>)
 => (builds-locator :: false-or(<directory-locator>), 
     database-locator :: <directory-locator>,
     profile-locator :: false-or(<directory-locator>))
  // XXX what if the registry is in the root directory ?
  let library-root = parent-directory(project.project-registry.registry-root);
  let personal? = project.project-personal-library?;
  let personal-build = user-build-path(); 

  let builds-locator = if (personal?) 
			 if(personal-build)
			   personal-build
			 else
			   subdirectory-locator(library-root, #("build"));
			 end;
		       else 
			 #f
		       end;

  let install-locator = if (personal?) 
			  user-install-path() | library-root
			else 
			  system-install-path() | library-root
			end;

  builds-locator & ensure-directories-exist(builds-locator);
  let database-locator = ensure-directory-exists(install-locator, "databases");
  let profile-locator = ensure-directory-exists(install-locator, "profiles");
  values(builds-locator, database-locator, profile-locator);
end function project-build-locations;

define function library-build-locator
    (builds-loc :: <locator>, library-name :: <library-name>)
 => (builds-locator :: <locator>)
  let dir-name = as-lowercase(as(<string>, library-name));
  ensure-directory-exists(builds-loc, dir-name)
end function library-build-locator;

define function library-database-locator
    (database-loc :: <locator>, library-name :: <library-name>)
 => (database-locator :: <locator>)
  let name = as-lowercase(as(<string>, library-name));
  merge-locators(make(<file-locator>, base: name, extension: $dylan-database-suffix),
		 database-loc)
end function library-database-locator;

define function library-profile-locator 
    (profile-loc :: <locator>, library-name :: <library-name>)
 => (profile-locator :: <locator>)
  let name = as-lowercase(as(<string>, library-name));
  merge-locators(make(<file-locator>, base: name, extension: $dylan-profile-suffix),
		 profile-loc)
end function library-profile-locator;
*/
