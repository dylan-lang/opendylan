Module:   projects-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $emacs-lisp-extension = "el";

define function user-warning(string, #rest args)
  signal(make(<project-warning>, format-string: string, format-arguments: args));
end;

define function user-error(string, #rest args)
  signal(make(<project-serious-warning>, format-string: string, format-arguments: args));
end;

define function user-fatal-error(string, #rest args)
  signal(make(<project-fatal-error>, format-string: string, format-arguments: args));
end;

define constant $compiler-lock = make(<recursive-lock>, name: "compiler-lock");

define constant $pm-lock = make(<recursive-lock>, name: "project-manager-lock");

// The notion of owners is to enable in the future having different 
// project namespaces 
// At this time all project/libraries are shared though
//
define function project-owners(project :: <project>)
 => (owners :: <sequence>);
  project.%project-owners
end;

define function project-dylan-library?(project :: <project>)
 => (bool :: <boolean>);
  project-key?(project, #"dylan")
end;

define function project-add-owner(project :: <project>, 
				  owner :: <project>)
 => (owners :: <sequence>);
  unless(project-dylan-library?(project))
    add-new!(project.%project-owners, owner);
  end;
  project.%project-owners
end;

define function project-remove-owner(project :: <project>, 
				     owner :: <project>)
 => (owners :: <sequence>);
  unless(project-dylan-library?(project))
    remove!(project.%project-owners, owner);
  debug-out(#"project-manager", "Removed owner %s from %s\n",
	    owner.project-name, project.project-name);
  end;
  project.%project-owners
end;

define function project-register-as-owner(project :: <project>)
 => (project :: <project>);
//  map(method(%project)
//	  debug-message("Project: %s\n\towners: %s\n", %project.project-name,
//		        map(project-name, %project.%project-owners))
//      end,
//      project.all-used-projects);
  project
end;

define function project-top-level?(project :: <project>) => (yes :: <boolean>);
  project.%project-top-level?
end;

define function project-top-level?-setter
    (value :: <boolean>, project :: <project>) 
 => (value :: <boolean>);
  project-register-as-owner(project);
  project.%project-top-level? := #t
end;


define thread variable *project-dynamic-environment* = #f;

define function project-dynamic-environment(key :: <symbol>) => object;
  *project-dynamic-environment* & element(*project-dynamic-environment*, key, default: #f)
end;

define function project-dynamic-environment-setter(value, key :: <symbol>) => value;
  if(*project-dynamic-environment*)
    *project-dynamic-environment*[key] := value
  else
    value
  end;
end;

define macro with-project-dynamic-environment
  { with-project-dynamic-environment ?:body end }
    => { do-with-dynamic-environment(method () ?body end) }
end macro;

define function do-with-dynamic-environment (fn)
  if (*project-dynamic-environment*)
    fn()
  else
    dynamic-bind (*project-dynamic-environment* = make(<table>))
      fn()
    end
  end if
end function;

define thread variable *within-compiler-lock?* :: <boolean> = #f;

define function %with-compiler-lock
    (function :: <function>)
  if (*within-compiler-lock?*)
    function()
  else
    with-lock($compiler-lock, timeout: 10.0)
      dynamic-bind (*within-compiler-lock?* = #t)
        function()
      end
    end
  end
end function %with-compiler-lock;

define macro
    with-compiler-transaction
      { with-compiler-transaction ?:body end }
	=> 
	{ %with-compiler-lock
	    (method ()
	       with-used-project-cache
		 do-with-dynamic-environment
		   (method () 
		      project-dynamic-environment(#"compiler-transaction") := #t;
		      ?body 
		    end)
	       end with-used-project-cache
	     end)
	   }
end macro;

define macro 
    with-browsing-transaction
      { with-browsing-transaction(?project:expression) ?:body end }
	=> 
	{ %with-compiler-lock
	    (method ()
	       with-used-project-cache
		 do-with-dynamic-environment
		   (method () 
		      project-dynamic-environment(#"browsing-transaction") := #t;
		      if(?project.ensure-project-database)
			?body 
		      else
			debug-assert("Invalid database query: project %s not compiled",
				     ?project.project-name)
		      end;
		    end)
	       end with-used-project-cache
	     end)
	   }
end macro;

define macro with-project-manager-transaction
  { with-project-manager-transaction ?:body end }
    => {with-lock($pm-lock, timeout: 10.0)
	  with-used-project-cache
	    do-with-dynamic-environment
	      (method () 
		 project-dynamic-environment(#"project-manager-transaction") := #t;
		 ?body 
	       end) 
	  end
	end
	  }
end macro;

// Wrap this around anything which constitutes a single "compilation
// transaction".
define macro with-used-project-cache
  { with-used-project-cache ?:body end }
    => { do-with-used-project-cache(method () ?body end) }
end macro;

/*
define open generic project-verify-databases(project :: <project>)
 => (status :: <boolean>);

define method project-verify-databases(project :: <project>)
 => (status :: <boolean>);
  #t
end;
*/
// this function checks  if the compiler notion of sources is in synch with
// the project sources
// 
define function verify-project-database(project :: <project>, #key verify-if-current = #t)
 => (in-memory? :: <boolean>, current? :: <boolean>, saved? :: <boolean>);
    if (project.project-personal-library?)
      let compiler-sources = 
	compilation-context-sources(project.project-current-compilation-context);
      if(~empty?(compiler-sources))
	project.%database-in-memory := #t;
	project-namespace-loaded(project) := #t;
	project.%database-saved := #t;
	if(verify-if-current)
	  let sr* = project-current-source-records(project);

	  if((size(compiler-sources) == size(sr*)) 
	       & 
	       every?(\==, compiler-sources, sr*))
	    // TODO: Note that this ignores the case where database is
	    // invalidated because some used-library version has changed.
	    project.%database-in-memory-current := #t
	  else
	    project.%database-in-memory-current := #f;
	    // it is still considered saved until compilation
	    // note-database-unsaved(project)
	  end if;
	end;
      else
	// new project without database
	project.%database-in-memory := #f;
	project.%database-in-memory-current := #f;
	// make it #t so the env doesn't ask the user to save
	// uncompiled project
	project.%database-saved := #t;
      end if;
    else
      // system project
      project-namespace-loaded(project) := #t;
      project.%database-in-memory := #t;
      project.%database-in-memory-current := #t;
      project.%database-saved := #t;
    end;
  values(project.%database-in-memory,
	 project.%database-in-memory-current,
	 project.%database-saved)
end;

// Useful information for users of projects

define method project-name (project :: <project>) => (name :: false-or(<symbol>))
  #f
end method project-name;

define method project-location (project :: <project>) => (location :: false-or(<file-locator>))
  #f
end method project-location;

define method print-object (project :: <project>, stream :: <stream>)
 => ();
  let name = project.project-name;
  let location = project.project-location;
  format(stream, "{%s %s", 
	 if(project.project-personal-library?)
	   "editable"
	 else
	   "read only"
	 end,
	 project.object-class);
  if (name | location)
    format(stream, ":");
    name & format(stream, " %s", name);
    location & format(stream, " in %s", as(<string>, location))
  end;
  format(stream, "}")
end method print-object;

// this callback is called before compilation context is open
define open generic note-project-made(project :: <project>, 
				      #key parent :: false-or(<project>))
 =>();

define method note-project-made(project :: <project>, 
				#key parent)
 =>();
end;

//// Subclassing protocol

// Make a project of given class.  This allows computation of initargs
// before calling actual make.  The default method just calls make directly.

// make-project protocol prevents us from using inheritance in project class hierarchy
// so we will use the standard make protocol at a cost of calculating some slots
// in the initialize methods and storing some values
// make-project will be used for registering projects and for connecting to the compiler

define open generic make-project (c :: subclass(<project>),
			     #key key, source-record-class,
			          // initial compiler settings
			          processor, operating-system, mode,
				  load-namespace?,
				  #all-keys)
 => project :: <project>;

define method make-project
    (c :: subclass(<project>), #rest keys, 
     #key key, parent = #f, load-namespace? = #t,
     source-record-class, processor = #f, operating-system = #f, mode)
 => (project :: <project>)
//  with-project-manager-transaction
  with-lock($pm-lock)
  with-used-project-cache
    unless (processor & operating-system)
      let (default-processor, default-os) = default-platform-info(c);
      unless (processor) processor := default-processor end;
      unless (operating-system) operating-system := default-os end;
    end;

    // choose harp for platforms that have it, c for others
    let back-end = 
      session-property(#"compiler-back-end")
    | select (processor)
        #"x86" =>
          select(operating-system)
            #"darwin" => #"c";
            otherwise => #"harp";
          end;
        otherwise => #"c";
      end;

    debug-out(#"project-manager", "Make-project: %s parent: %s\n", key, 
	      parent & parent.project-name);
    let project = 
      apply(make, c, 
	    processor:, processor, operating-system:, operating-system,
		compiler-back-end:, back-end,
	    keys);

    if (mode) project-compilation-mode(project) := mode end;

    if(key == #"dylan")
      project.%project-owners := #[];
    end;

    note-project-made(project, parent: parent);
    //
    // this opens the compilation context
    project-open-compilation-context(project, load-namespace?: load-namespace?);
    if(compilation-definitions-inconsistent?(project.project-current-compilation-context))
      project-reset-database(project)
    end;

    *all-open-projects* := pair(project, *all-open-projects*);
    unless (project.project-personal-library?)
      verify-project-database(project);
    end;

    if(~ parent)
      // if parent is #f this must be a top level project
      // note that a top level project can aquire owners later on
      project.project-top-level? := #t;
    end;
    // we load the namespace only if a top level project has a database
    // since in this case all used projects have to have db's too
    // we attempt to just load the db's without updating sources
    // unless we are in a compiler transaction
    // TO DO: make sure %project-top-level? has meaningful value

    if(project.%project-top-level?)
     verify-project-database(project)
    else
      verify-project-database(project, verify-if-current: #f)
    end;

    unless(load-namespace?)
      if(project.%database-in-memory & 
	   ~project-dynamic-environment(#"compiler-transaction"))
	project-load-namespace(project,
			       update-sources?: #f, update-used?: #f);
      end;
    end;  

    project-set-compilation-parameters(project);
    project.%database-saved & note-database-saved(project);
    project
  end with-used-project-cache
  end 
end method;

define function project-open-compilation-context (project :: <project>,
						  #key load-namespace? = #t)
 => (context)
  with-lock ($compiler-lock)
    debug-out(#"project-manager",
	      "Open compilation context for project %s (load-namespace? %s)\n",
	      project.project-name, load-namespace?);
    if (project.project-dylan-library?)
      debug-message("Opening compilation context for the Dylan library")
    end;
    let handler <library-pack-not-installed> = 
      method (cond, next-handler)
	let project-name = as(<string>, cond.condition-project.project-name);
	let library-pack = cond.condition-library-pack;
	let library-pack-name = library-pack-full-name(library-pack);
	debug-message("You must install %s in order to use the library %s",
		      library-pack-name, project-name);
	user-fatal-error("You must install %s in order to use the library %s",
			 library-pack-name, project-name);
	signal(make(<abort-compilation>, 
		    warnings: 0,
		    serious-warnings: 0,
		    errors: 1))
      end;
    let handler <database-corruption-warning> = 
      method (cond, next-handler)
	apply(debug-message, cond.condition-format-string, cond.condition-format-arguments);
	user-warning("Discarding corrupted compiler database: %s", 
		     as(<string>, condition-database-name(cond)))
      end;
    let handler <database-version-warning> = 
      method (cond, next-handler)
	apply(debug-message, cond.condition-format-string, cond.condition-format-arguments);
	user-warning("Discarding incompatible compiler database: %s", 
		     as(<string>, condition-database-name(cond)))
      end;
    let handler <database-user-version-warning> = 
      method (cond, next-handler)
	apply(debug-message, cond.condition-format-string, cond.condition-format-arguments);
	user-warning("Discarding incompatible compiler database %s", 
		     as(<string>, condition-database-name(cond)))
      end;
    let context = open-compilation-context(project,
					   database-location: 
					     project-database-location(project),
					   profile-location: 
					     project-profile-location(project),
					   build-settings: 
					     project-build-settings(project),
					   read-only?:
					     ~project-personal-library?(project),
					   load-namespace?: load-namespace?);
    if (project.project-dylan-library?)
      debug-message("  Opened compilation context for the Dylan library")
    end;
    // we have to set the context in either case, 
    // otherwise project closing code won't work
    project.project-current-compilation-context := context;

    let (mj, mn, ts) = compilation-context-version(context);
    debug-out(#"project-manager",
	      "Opened compilation context for %s: %s %s %s\n", project.project-name,
	      mj, mn, ts);
    if (project-personal-library?(project)
	  | project-library-definition(context))
      context
    else
      %close-project(project);
      error(make(<system-project-not-usable>, project: project));
      // Something along these lines would be nice, but this is too simple-minded
      // cerror("Convert project to personal",
      //        make(<system-project-not-usable>, project: project));
      // project.project-personal-library? := #t;
      // project-open-compilation-context(project)
    end;
  end
end;

define function project-set-compilation-parameters(project :: <project>, 
						   #key load-namespace? = #t)
 => (context);
  let context = project.project-current-compilation-context
    |
    project-open-compilation-context(project, load-namespace?: load-namespace?);
  let compiler-settings = #();
  local method add-setting (key, value)
	  compiler-settings := pair(key, pair(value, compiler-settings));
	end;
  if(project.project-personal-library?)
    add-setting(mode: project-compilation-mode(project));
    add-setting(processor: project-processor(project));
    add-setting(operating-system: project-operating-system(project));
    add-setting(back-end: project-compiler-back-end(project));
  end;
  add-setting(build-location: project-build-location(project));
  add-setting(library-pack: project-library-pack(project));
  context.compilation-context-compiler-settings := compiler-settings;
  context
end;  

// callback from the compiler
// it's different semantics then note-project-loaded
define sideways method note-definitions-updated(context)
  let project = context.compilation-context-project;
  project.%database-in-memory := #t;
  project.%database-in-memory-current := #t;
  project.%database-saved := #f;
  note-database-unsaved(project);
end;

define method close-project (key, #key system? = #f)
 => (closed? :: <boolean>);
  // a little hack: we don't want a closed project 
  // made a top level project at this point
  // with-compiler-transaction will do that for us
  with-compiler-transaction
    let project-to-close = lookup-named-project(key, create?: #f);
    project-to-close & close-project(project-to-close, system?: system?);
  end;
end method;

// Open project management

define variable *all-open-projects* = #();

define function close-all-projects (#key system? = #f, personal? = #t)
  local method loop ()
	  let project = any?(method (project)
			       if (project-personal-library?(project))
				 personal?
			       else
				 system?
			       end & project
			     end method, *all-open-projects*);
	  if (project)
	    %%close-project(project);
	    loop()
	  end;
	end method;
  loop()
end function;

define function %project-closed?(project :: <project>)
 => (closed? :: <boolean>);
  project.project-current-compilation-context = #f;
end;

define function remove-all-personal-owners(project :: <project>) => ()
  if(project.project-personal-library?)
    let personal-subprojects = all-used-projects(project, system?: #f);
    do(remove-as-owner, personal-subprojects);
  end;
end;

define function remove-as-owner(project :: <project>, #key subprojects = #f) => ();
  do(rcurry(project-remove-owner, project), 
     subprojects | project.directly-used-projects)
end;

define function close-unused-projects(#key system? = #t) => ()
  // initial set of unused projects
  let unused-projects = choose(method (p)
				 empty?(p.project-owners) & ~project-dylan-library?(p)
			       end,
			       *all-open-projects*);
  debug-out(#"project-manager", "Unused projects: %s\n", map(project-name, unused-projects));
  for(s in unused-projects)
    close-subproject(s, system?: system?)
  end;
end;

define function %%close-project
    (project :: <project>, #key subprojects = #f) => ()
  unless(%project-closed?(project))
    let subprojects = subprojects | directly-used-projects(project);
    remove-as-owner(project, subprojects: subprojects);
    project-close-compilation-contexts(project);
    *all-open-projects* := remove!(*all-open-projects*, project);
    note-project-closed(project);
    debug-out(#"project-manager", "Closed project: %s\n", project.project-name);
  end;
end function;

define method %close-project (project :: <project>, #key subprojects = #f)
  unless(project-dylan-library?(project))
    %%close-project(project, subprojects: subprojects);
  end;
  #t
end method;

define function close-subproject(project :: <project>, #key system?)
 => (closed? :: <boolean>);
  unless(project.%project-closed? 
	   | project.%project-top-level? 
	   | (~system? & ~project.project-personal-library?))
    if(empty?(project.project-owners))
      let subprojects = directly-used-projects(project);
      %close-project(project, subprojects: subprojects);
      do(method(p) close-subproject(p, system?: system?) end, subprojects);
      #t
    else
      debug-out(#"project-manager", "Project %s not closed, owners: %s\n", 
		project.project-name,
		map(project-name, project.project-owners));
      #f
    end;
  end;
end;

define method close-project (project :: <project>, #key system? = #f)
 => (closed? :: <boolean>);
  debug-out(#"project-manager", "Closing %s - project owners: %= \n", 
	    project.project-name,
	    map(project-name, project.project-owners));
  
  if(%project-closed?(project))
    #t
  elseif(project.%project-top-level?)
    project.%project-top-level? := #f;
    let closed? = close-subproject(project, system?: system?);
    // this is needed to close subprojects even if they were not compiled against.
    // close-unused-projects(system?: system?);
    map(method(%project)
	    debug-out(#"project-manager", 
		      "Project: %s\n\towners: %s\n", %project.project-name,
		      map(project-name, %project.%project-owners))
	end,
	*all-open-projects*);
    closed?
  else
    debug-message("Closing non top level project %s", project.project-name);
    empty?(project.project-owners) & %close-project(project)
  end
end method;

// A cache used to avoid canonicalizing project sources multple times
// for multiply-used libraries during project parsing.
define thread variable *used-project-cache* = #f;

define function do-with-used-project-cache (fn)
  if (*used-project-cache*)
    fn()
  else
    dynamic-bind (*used-project-cache* = make(<table>))
      fn()
    end
  end if
end function;

define constant <source-record-vector> = limited(<vector>, of: <source-record>);

// to force parse later if we are not in a compiler transaction
define function project-reset-database(project :: <project>)
  unless(project-dynamic-environment(#"compiler-transaction") | %project-closed?(project))
    let context = project-current-compilation-context(project);
    let (mj, mn) = compilation-context-version(context);
    install-project-sources(context, make(<source-record-vector>, size: 0), mj, mn);
    note-database-invalidated(project);
    debug-message("Reset database for project %s", project.project-name);
  end;
end;

define function %delete-file-if-exists(loc :: <physical-locator>)
  block()
    delete-file(loc)
  exception(<file-system-error>)
  end
end;

define function project-remove-database(project :: <project>) => ()
  unless(project-dynamic-environment(#"compiler-transaction"))
    let context = project-current-compilation-context(project);
    // close the context, remove the db file and open context
    // TO DO: what if we are conected
    // environment has to make sure we are not
    let (mj, mn, ts) = compilation-context-version(context);
    remove-as-owner(project);
    debug-out(#"project-manager", "closing context for %s: %s %s %s\n", 
	      project.project-name, mj, mn, ts);
    project-close-compilation-contexts(project);
    let db = project.project-database-location;
    %delete-file-if-exists(db);
    debug-message("Removed database for project %s", project.project-name);
    project-open-compilation-context(project, load-namespace?: #f);
    project-set-compilation-parameters(project);
    
    project.project-namespace-loaded := #f;
    let (mj, mn, ts) = compilation-context-version(context);
    debug-out(#"project-manager", "opened context for %s: %s %s %s\n", 
	      project.project-name, mj, mn, ts);
    verify-project-database(project);
    note-database-invalidated(project)
  end;
end;

define open generic generate-makefile(project :: <base-project>);

define open generic makefile-exists?(project :: <base-project>) 
 => (well? :: <boolean>);

define method project-remove-build-products(project :: <base-project>,
					    #key recursive? = #t);
  if(project-personal-library?(project))
    if(recursive?)
      do(project-remove-database, all-used-projects(project, system?: #f));
    end;
    project-remove-database(project);
    // remove compiler & link products
    makefile-exists?(project) &
    build-system(if (recursive?) #["clean-all"] else #["clean"] end,
		 directory: project.project-build-location,
		 progress-callback: ignore);
  end;
  // no-op for system projects
end;

define open generic project-verify-source-records(project :: <project>)
 => (records :: <sequence>);

// TO DO: be smarter and return correct records
// maybe also check headers
define method project-verify-source-records(project :: <project>)
 => (records :: <sequence>);
  block()
    project-current-source-records(project);
  exception(e :: <source-record-error>)
    apply(user-error, e.condition-format-string, e.condition-format-arguments);
    #()
  end;
end;

define thread variable *canonicalize-force-parse?* = #f;
define thread variable *canonicalize-update-sources?* = #f;

define function canonicalize-project-sources
    (project :: <project>,
     #key force-parse? = *canonicalize-force-parse?*,
          update-sources? = *canonicalize-update-sources?*,
          update-used? = update-sources?,
// this default doesn't seem to be desirable
          force-parse-used? = update-used? & force-parse?)
  if (project-personal-library?(project))
    debug-out(#"driver", "canonicalize-project-sources %s, force-parse? %s,"
		" update-sources? %s, update-used? %s, force-parse-used? %s\n",
	      project, force-parse?, update-sources?, update-used?,
	      force-parse-used?);
    with-used-project-cache
      with-project-dynamic-environment
	dynamic-bind (*canonicalize-force-parse?* = force-parse-used?,
		      *canonicalize-update-sources?* = update-used?)
	  let context = project-current-compilation-context(project);
	  note-loading-namespace(project);
	  let (sr*, major, minor)
	    = if (update-sources?)
		values(project-current-source-records(project),
		       project-major-version(project),
		       project-minor-version(project))
	      else
		// If don't want to update sources, just make sure what's
		// there now is fully installed (e.g. previous processing
		// might have been aborted).
		let (mj, mn) = compilation-context-version(context);
		let compiler-sources = compilation-context-sources(context);
		if(empty?(compiler-sources))
		  // fixup
		  debug-message("Fixing up sources for %s", project.project-name);
		  values(project-current-source-records(project),
			 project-major-version(project),
			 project-minor-version(project))
		else
		  values(compilation-context-sources(context), mj, mn)
		end;
	      end;
	  if (force-parse?) // reset to having empty sources
	    install-project-sources(context, make(<source-record-vector>, size: 0), major, minor);
	  end;
	  internal-message("Loading namespace for %s", project);
	  install-project-sources(context, as(<source-record-vector>, sr*), major, minor);
	  project-namespace-loaded(project) := #t;
          project.%database-in-memory := #t;
	  note-project-loaded(project);
	end dynamic-bind;
      end with-project-dynamic-environment;
    end with-used-project-cache;
  end if;
end function;

define sideways method used-library-context 
    (context, used-library-dylan-name :: <symbol>, #key canonicalize?)
 => (subcontext)
  let cache = *used-project-cache*;
  let project = compilation-context-project(context);
  debug-out(#"driver", "used-library-context %s looking for %s, canonicalize? = %s\n", 
	    project, used-library-dylan-name, canonicalize?);
  let subcontext = 
    (cache & element(cache, used-library-dylan-name, default: #f))
    | with-used-project-cache
	// KLUDGE: used by project-compiler-setting and who knows what else..
	project.project-current-compilation-context := context;
	let key = used-library-project-key(project, used-library-dylan-name);
	let processor = project-compiler-setting(project, processor:);
	let os = project-compiler-setting(project, operating-system:);
	let subproject = find-platform-project(key, processor, os) |
	                   make-used-project(project, key, processor, os);
	let subcontext = project-current-compilation-context(subproject);
        if (~subcontext)
	  debug-message("Project %s: subproject %s with empty subcontext\n", 
			project.project-name, subproject.project-name);
	  subcontext := project-open-compilation-context(subproject);
	end;
        if (canonicalize?)
	  canonicalize-project-sources(subproject);
	  // Only cache canonicalized projects
	  if (cache)
	    cache[used-library-dylan-name] := subcontext;
	  end;
	end;
	subcontext
      end with-used-project-cache;
  let used-project = compilation-context-project(subcontext);
  project-add-owner(used-project, project);
  note-used-project(project, used-project);
  subcontext
end;

define open generic project-compiler-source-location
    (project :: <project>)
 => (location :: <directory-locator>);

define method project-compiler-source-location
    (project :: <project>)
 => (location :: <directory-locator>);
  project.project-source-location;
end;

define function project-id-canonical-source-record(project :: <project>, id) => sr;
  let sr = %project-record-id-source-record(project, id, create?: #f);
  sr & member?(sr, project.project-canonical-source-records) & sr
end;

define method project-record-id-source-record (project :: <project>, id) => sr;
  %project-record-id-source-record (project, id)
end method;

define function %project-record-id-source-record (project :: <project>, id, #key create? = #t) => sr;
  let table = project.project-source-record-table;
  let str = as(<string>, id);
  element(table, str, default: #f) 
    | if (create?)
	let id
	  = id-as-source-record(project-source-record-class(project), 
				project,
				project-compiler-source-location(project),
				id);
	table[str] := id
      end
end function;

define method project-source-record-id (project :: <project>, sr) => id;
  source-record-as-id(sr, project-compiler-source-location(project));
end method;

// Must return a name suitable for use as the base of a file name,
// and unique within project.
define method project-source-record-name (project :: <project>, sr)
 => (name :: false-or(<string>))
  let name = 
  source-record-relative-name(sr, project-compiler-source-location(project));
  if (name & locator-directory(as(<file-locator>, name)))
    let base = source-record-name(sr);
    let cx = project.project-current-compilation-context;
    let index = position(cx.compilation-context-sources, sr);
    concatenate(integer-to-string(index), "=", base)
  else
    name
  end;
end method;

define function project-compiler-setting (project :: <project>,
					  key :: <symbol>)
  let context = project.project-current-compilation-context;
  let settings = compilation-context-compiler-settings(context);
  block (return)
    for (key-or-val in settings, prev = #f then key-or-val,
	 val? = #f then ~val?)
      if (val? & (prev == key)) return(key-or-val) end;
    end for;
    error("%= is not a compiler setting", key);
  end block;
end function;

define function project-compiler-setting-setter (value,
						 project :: <project>,
						 key :: <symbol>)
  let context = project.project-current-compilation-context;
  context & 
    (compilation-context-compiler-settings(context) := list(key, value));
end function;

define function choose-project (test :: <function>)
  any?(method (project) 
	 test(project) & project 
       end, 
       *all-open-projects*);
end function;

define function find-platform-project (key, processor, os)
  //  debug-message("looking up project %s:%s:%s \n", key, processor, os);
  let project = 
    choose-project(method(project)
		       project-key?(project, key) &
		       (processor == #"unknown" |
			  project-compiler-setting(project, processor:) == processor) &
		       (os == #"unknown" |
			  project-compiler-setting(project, operating-system:) == os)
		   end);
  //  unless(project)
  //    debug-message("Not found: creating new project\n");
  //  end;
  project
end function;

define function platform-namestring (processor, os)
  concatenate(as-lowercase(as(<string>, processor)),
	      "-",
	      as-lowercase(as(<string>, os)))
end function;

define function platform-namestring-info (platform) => (processor, os)
  let name = as-lowercase(as(<string>, platform));
  let separator-position = position(name, '-');
  let processor-name = copy-sequence(name, end: separator-position);
  let os-name = copy-sequence(name, start: separator-position + 1);
  values(as(<symbol>, processor-name),
	 as(<symbol>, os-name))
end function;

define function target-platform-name ()
  let (processor, os) = default-platform-info(*default-project-class*);
  platform-namestring(processor, os);
end function;

define function target-platform-name-setter (platform)
  let (old-processor, old-os) = default-platform-info(*default-project-class*);
  let (new-processor, new-os) = platform-namestring-info(platform);
  unless (new-processor == old-processor & new-os == old-os)
    for (project in *all-open-projects*)
      note-platform-change(project, new-processor, new-os);
    end;
    set-default-platform-info(*default-project-class*, new-processor, new-os);
  end;
end function;


// This got moved out of the compiler, it should probably get moved even
// further out into the environment...

// NOTE:
// Dispatch color files now have version information in the first line
// (currently as an ELisp comment, for convenience).  The following may
// merit updates to the minor version number:
//   - Adding or removing "colornames" (but see below for changing existing).
//   - Any similar change to the program-notes colouring.
// These would merit an update to the major version number:
//   - Changing (the spelling of) existing "colornames".
//   - Changing the "function names" ("color-forgrounds/-backgrounds").
//   - Changing the format of the line/column information.
//   - Any more dramatic changes (e.g., moving to a binary file format).
//
// *** NB: Changes here should be coordinated with the dylan-mode for Emacs.
//
// The environment (dylanworks mode in Environment-Deuce) doesn't use this,
// it uses source-record-dispatch-decisions directly.  This function is only
// for emacs support.

define function project-dump-emacs-dispatch-colors (project :: <project>)
  let dir = project.project-build-location;
  when (dir)
    let context = project.project-current-compilation-context;
    for (sr in compilation-context-sources(context))
      let name = sr.source-record-name;
      when (name)
	let file = make(<file-locator>,
			directory: dir,
			base:      name,
			extension: $emacs-lisp-extension);
	with-open-file (stream = file, direction: #"output")
	  dump-source-record-emacs-dispatch-colors(context, sr, stream);
	end with-open-file;
      end when;
    end for;
  end when;
end project-dump-emacs-dispatch-colors;

define function dump-source-record-emacs-dispatch-colors
    (context, sr :: <source-record>, stream :: <stream>)

  // Version info (magic-word, major-version, minor-version)
  write(stream, "; HQNDYLANCOLRINFO 1 0\n");

  // Convert source record locations to file locations
  let start-in-file = source-record-start-line(sr);
  local method write-bounds (start-offset, end-offset)
	  format(stream, "(%d %d %d %d)\n",
		 start-in-file + source-offset-line(start-offset),
		 source-offset-column(start-offset),
		 start-in-file + source-offset-line(end-offset),
		 source-offset-column(end-offset));
	end;

  // Foreground colors, for dispatch and others.
  let open-type = #f;
  local method close-group () => ();
	  if (open-type) format(stream, "))\n") end;
	end method;
  local method open-group (type) => ();
	  if (open-type ~== type)
	    close-group();
	    format(stream, "\n(color-foregrounds color-%s '(\n", type);
	    open-type := type;
	  end;
	end;
  local method write-loc (start-offset, end-offset, type) => ();
	  open-group(type);
	  write-bounds(start-offset, end-offset);
	end method;
  let dds = source-record-dispatch-decisions(context, sr);
  for (i from 0 below dds.size by 3)
    write-loc(dds[i], dds[i + 1], dds[i + 2])
  end;
  close-group();

  // Now let's dump out the conditions.
  write(stream, "\n(color-backgrounds color-program-notes '(\n");
  for (note in source-record-notes(context, sr) | #[])
    let loc = note.program-note-location;
    if (loc & loc.source-location-source-record == sr)
      write-bounds(loc.source-location-start-offset,
		   loc.source-location-end-offset)
    end;
  end for;
  format(stream, "))\n");

end dump-source-record-emacs-dispatch-colors;
