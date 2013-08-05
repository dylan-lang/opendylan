Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $project-file-format-version = 2;

define function lid-project-build-settings 
    (p :: <lid-project>)
 => (executable :: false-or(<string>),
     base-address,
     debug-command :: false-or(<string>),
     debug-arguments :: false-or(<string>),
     debug-machine :: false-or(<string>),
     debug-directory :: false-or(<string>),
     start-function :: false-or(<string>),
     linker-options :: <sequence>,
     c-source-files :: <sequence>,
     c-header-files :: <sequence>,
     c-object-files :: <sequence>,
     c-libraries :: <sequence>,
     rc-files :: <sequence>,
     jam-includes :: <sequence>)

// extend this function if you don't want to save default values, 
// i.e. values that were never explicitly set
  values(project-build-property(p, #"executable") | #f,
	 project-build-property(p, #"base-address") | #f,
	 project-build-property(p, #"debug-command") | #f,
	 project-build-property(p, #"debug-arguments") | #f,
	 project-build-property(p, #"debug-machine") | #f,
	 project-build-property(p, #"debug-directory") | #f,
	 project-build-property(p, #"start-function") | #f,

	 project-build-property(p, #"linker-options") | #[],
	 project-build-property(p, #"c-source-files") | #[],
	 project-build-property(p, #"c-header-files") | #[],
	 project-build-property(p, #"c-object-files") | #[],
	 project-build-property(p, #"c-libraries") | #[],
	 project-build-property(p, #"rc-files") | #[],
	 project-build-property(p, #"jam-includes") | #[])
end function lid-project-build-settings;

/// Generate a list of all C libraries used by this Dylan library and all used
/// Dylan libraries.  Do our best to avoid duplicate entries in the list and to
/// put those entries of the "deeper" used libraries before those of the "shallower"
/// libraries.  (I.e., if A uses B and C, B uses D, and C uses D and E, the order of
/// C libraries should be from D, B, E, C, A.)
define function all-c-libraries 
    (project :: <lid-project>) 
 => (c-libraries :: <sequence>)
  let all-c-libraries = make(<stretchy-vector>);
  local method doit (p)
	  let c-libraries
	    = apply(ldbs-c-libraries, project-build-settings(p));
	  for (c-library in c-libraries)
	    add-new!(all-c-libraries, c-library, test: \=);
	  end;
	end method doit;
  let projects = reverse(all-used-projects(project));
  for (p in projects)
    doit(p)
  end;
  doit(project);
  as(<list>, all-c-libraries)
end function all-c-libraries;

// This is a define method and out here rather than a local method in
// the above to work around an emulator bug in #all-keys handling.
define method ldbs-c-libraries 
    (#key c-libraries = #(), #all-keys) => (c-libraries :: <sequence>)
  c-libraries
end method ldbs-c-libraries;

define function save-source-files-list-in-lid(p :: <lid-project>, stream :: <stream>)
  save-list-value(stream, #"Files", 
		  map(method(l) 
			  locator-base(l)
		      end, 
		      p.project-source-files))
end function save-source-files-list-in-lid;

define method save-source-files-in-hdp(p :: <user-project>, stream :: <stream>)
  p.project-source-files &
    save-list-value(stream, #"Files", 
		    map(method(l) 
			    let relative-path = relative-locator(l, p.project-source-location);
			    as(<string>, 
			       make(<file-locator>,
				    directory: relative-path.locator-directory,
				    base:      relative-path.locator-base))
			end, 
			p.project-source-files))
end method save-source-files-in-hdp;

define method save-project-as-lid-file(p :: <user-project>, #key to-file = #f)
  let file = as(<file-locator>, (to-file | p.project-lid-location));
  with-open-file(stream = file, direction: #"output")
    save-single-value(stream, #"Library", p.project-lid-library-name);
    save-source-files-list-in-lid(p, stream);
    save-lid-info(p, stream);
  end
end method save-project-as-lid-file;

define method convert-path-to-relative
    (locator :: <physical-locator>, dir :: <directory-locator>)
 => (r :: <string>)
  as(<string>, relative-locator(locator, dir))
end method convert-path-to-relative;

define method convert-path-to-relative
    (filename :: <string>, dir :: <directory-locator>)
 => (r :: <string>)
  as(<string>, relative-locator(as(<file-system-locator>, filename), dir))
end method convert-path-to-relative;

define function convert-path-to-filename
    (filename :: <string>) => (f :: <string>)
  locator-name(as(<file-locator>, filename))
end function convert-path-to-filename;

define method project-relative-file
    (project :: <user-project>, file :: <string>)
 => (relative-file :: <string>)
  project-relative-file(project, as(<file-locator>, file))
end method project-relative-file;

define method project-relative-file
    (project :: <user-project>, file :: <file-locator>)
 => (relative-file :: <string>)
  let project-directory = project.user-disk-project-file.locator-directory;
  convert-path-to-relative(file, project-directory)
end method project-relative-file;

define function save-lid-info
    (p :: <lid-project>, stream :: <stream>, #key flatten-extras? :: <boolean> = #f)
  let (executable, base-address-string, 
       debug-command, debug-arguments-string, debug-machine, debug-directory,
       start-function, linker-options, c-source-files,
       c-header-files, c-object-files, c-libraries, rc-files, jam-includes)
    = lid-project-build-settings(p);
  let relative = if (flatten-extras?)
		   curry(map, convert-path-to-filename)
		 else
		   curry(map, rcurry(convert-path-to-relative, 
				     p.project-lid-location.locator-directory))
		 end;
  save-single-value(stream, #"executable", executable);
  save-single-value(stream, #"base-address", 
		    base-address-string | compute-base-address(p, #f));
  save-single-value(stream, #"debug-command", debug-command);
  save-single-value(stream, #"debug-arguments", debug-arguments-string);
  save-single-value(stream, #"debug-machine", debug-machine);
  save-single-value(stream, #"debug-directory", debug-directory);
  save-single-value(stream, #"start-function", start-function);
  save-list-value(stream, #"linker-options", linker-options);
  save-list-value(stream, #"c-source-files", relative(c-source-files));
  save-list-value(stream, #"c-header-files", relative(c-header-files));
  save-list-value(stream, #"c-object-files", relative(c-object-files));
  save-list-value(stream, #"c-libraries", c-libraries);
  save-list-value(stream, #"rc-files", relative(rc-files));
  save-list-value(stream, #"jam-includes", relative(jam-includes));
  save-single-value(stream, #"major-version", p.project-major-version);
  save-single-value(stream, #"minor-version", p.project-minor-version);
  save-single-value(stream, #"library-pack", p.project-library-pack);
  save-single-value(stream, #"compilation-mode", p.project-compilation-mode);
  save-single-value(stream, #"target-type", p.project-target-type);
  save-list-value(stream, #"loose-library-bindings", p.project-library-loose-bindings);
end;  

/*---*** andrewa: not currently used...
define generic save-user-keyword(s :: <stream>, key :: <symbol>, val );

define method save-user-keyword(s :: <stream>, key :: <symbol>, val )
  save-list-value(s, key, val)
end;
/**
define method save-user-keyword(s :: <stream>, key :: <standard-keyword>, val )
  // do nothing
end;
*/

define function save-user-keywords(p :: <user-project>, stream :: <stream>)
  let keywords = p.user-project-keywords;
  for (k in keywords.key-sequence)
    unless (member?(k, $standard-lid-keyword))
      save-user-keyword(stream, k, keywords[k])
    end
  end;
end;
*/

define method save-single-value(s :: <stream>, keyword :: <symbol>, value)
  value & format(s, "%s:\t%s\n", keyword, value);
end;

define method save-list-value(s :: <stream>, keyword :: <symbol>, values :: <sequence>)
  unless (empty?(values))
    format(s, "%s:", keyword);
    for (value in values)
      format(s, "\t%s\n", as(<string>, value));
    end for;
  end unless;
end;

define method save-project(p :: <user-project>, #rest keys, #key save-db?, 
			   to-file = #f, #all-keys)
  if(~p.project-read-only?)
    let project-file 
      = if (to-file)
	  select (to-file by instance?)
	    <string> =>
	      let to-locator = as(<file-locator>, to-file);
	      merge-locators(make(<file-locator>,
				  directory: to-locator.locator-directory,
				  base:      to-locator.locator-base,
				  extension: $user-project-suffix),
			     p.project-source-location);
	    <file-locator> => 
	      make(<file-locator>,
		   directory: to-file.locator-directory,
		   base:      to-file.locator-directory,
		   extension: $user-project-suffix);
	  end
	else
	  p.user-disk-project-file
	end;
    
    let save? = file-property(project-file, #"writeable?");

    if(save?)
      with-open-file(stream = project-file, direction: #"output")
	write-comment(stream, "This file is generated, please don't edit");
        save-single-value(stream, #"Format-version",
			  integer-to-string($project-file-format-version));
        save-single-value(stream, #"Library", p.project-lid-library-name);
        save-source-files-in-hdp(p, stream);
        save-list-value(stream, #"subprojects", 
			map(curry(project-relative-file, p),
			    p.%subproject-files));
        save-lid-info(p, stream);
        let other-files = project-build-property(p, #"other-files") | #[];
        save-list-value(stream, #"other-files", 
			map(curry(project-relative-file, p),
			    other-files));
  
        write-comment(stream, "additional keywords");
        // save-user-keywords(p, stream);
      end;
    else
      user-warning("Cannot save: project file %s is read only", 
		   as(<string>, project-file))
    end;
    next-method();
  end;
end;

define constant $cache-extension = "cache";

define function save-project-caches(project :: <user-project>) => ();
  let cache-file = cache-file-location(project);
  debug-out(#"project-manager",
            "Saving caches for project %s in %s",
            project.project-name,
            as(<string>, cache-file));
  with-open-file(stream = cache-file, direction: #"output")
    write-comment(stream, 
		  "the information below is based on the last compile");
    save-used-projects-cache(project, stream);
    save-list-value(stream, #"canonical-sources", 
		    map(rcurry(source-record-as-id, project.project-source-location),
			project.%compiled-source-records));
    save-tools-cache(project, stream);
  end;
end;

define function write-comment(stream, comment :: <string>)
  save-single-value(stream, #"Comment", comment)
end;

define function save-tools-cache(project :: <user-project>, stream :: <stream>)
  if(project.%tools-cache)
    let tools = project.%tools-cache.key-sequence;
    save-list-value(stream, #"tools-cache", tools);
    for(tool in tools)
      let cache = element(project.%tools-cache, tool, default: #[]);
      let list-cache = #();
      do(method(e) 
	     list-cache := pair(project-relative-file(project, first(e)),
				pair(as-iso8601-string(second(e)), list-cache))
	 end,
	 cache);
      save-list-value(stream, tool, list-cache);
    end;
  end;
end;
    

define function save-used-projects-cache(p :: <user-project>, stream :: <stream>)
  let info = #();
  local method relative(location, to :: <directory-locator>) 
	  if(location == #"system-registry" | location == #"user-registry")
	    location
	  elseif  (~location)
	    #"system"
	  else
	    convert-path-to-relative(location, to)
	  end
	end;
		     
  for(k in p.%user-project-used-projects.key-sequence)
    let subproject = p.%user-project-used-projects[k];
    let subproject-location = select(subproject by instance?)
				<system-project> =>   
				  if(subproject.project-personal-library?) 
				    #"user-registry" 
				  else 
				    #"system-registry" 
				  end;
				<binary-project> =>
				  subproject.project-database-location;
				<user-project> =>
				  subproject.user-disk-project-file;
			      end select;
    let subproject-build = subproject.project-build-location;
    let project-directory = p.user-disk-project-file.locator-directory;
    let relative-project-file-location = relative(subproject-location,
						  project-directory);
    let relative-build-directory-location = relative(subproject-build,
						     p.project-build-location);
    info := pair(k, pair(relative-project-file-location,
			 pair(relative-build-directory-location, info)));
  end;
  save-list-value(stream, #"used-projects", info)
end;

//define constant $no-value = #(#"no-value");

define generic destructure-flat-assoc-list(list :: <sequence>, rank :: <integer>,
					   #key convert, #all-keys)
 => (collection :: <collection>);

// should specialize on the collection classes

define method destructure-flat-assoc-list(list-seq :: <sequence>, rank == 1,
					  #key convert = identity,
					  collection-class = <stretchy-vector>)
 => (collection :: <vector>)
  let collection = make(collection-class);
  do(method(l) add!(collection, convert(l)) end, list-seq);
  collection
end;

define method destructure-flat-assoc-list(list-seq :: <sequence>, rank == 2,
					  #key convert = method(a,b) values(a,b) end,
					  collection-class = <stretchy-vector>)
 => (collection :: <collection>)
  let collection = make(collection-class);
  for(i from 0 below size(list-seq) by rank)
    let (a,b) = convert(list-seq[i], list-seq[i + 1]);
    add!(collection, list(a,b));
  end;
  collection
end;

define method destructure-flat-assoc-list(list-seq :: <sequence>, rank == 3,
					  #key convert = method(a,b,c) 
							     values(as(<symbol>, a),b,c) 
							 end,
					  table-class = <table>)
 => (table :: <table>)
  let table = make(table-class);

  for(i from 0 below size(list-seq) by rank)
    let (a,b,c) = convert(list-seq[i], list-seq[i + 1], list-seq[i + 2]);
    table[a] := list(b,c)
  end;

  table
end;
      

/// All Functional Developer DLLs that may be used by user libraries reside above
/// this address.  Consequently, it's an upper bound for the base for user
/// libraries including our private libraries (e.g., the compiler and environment)
define constant $top-for-user-libraries = #x64000;

/// Presume that user libraries fit into 32 pages (128K)
define constant $allowance-for-user-libraries = #x20;
define method library-base
    (#key base-address = #f, #all-keys) => (base :: false-or(<machine-word>))
  base-address
end method;

define method compute-base-address (project :: <lid-project>, explicit-base :: false-or(<machine-word>))
 => (computed-base :: <string>)
  let base-address =
    explicit-base
    | begin
	let baseless-libraries = 1;
	let top = $top-for-user-libraries;
	let projects = all-used-projects(project);
	for (p in projects)
	  let base = apply(library-base, project-build-settings(p));
	  if (base)
	    let base :: <machine-word> = base;
	    top := min(top,
		       as(<integer>, machine-word-unsigned-shift-right(base, 12)))
	  else
	    baseless-libraries := baseless-libraries + 1
	  end
	end;
	let machine-word :: <machine-word> = 
	  as(<machine-word>, top - baseless-libraries * $allowance-for-user-libraries);
	machine-word-unsigned-shift-left(machine-word, 12)
      end begin;
  machine-word-to-string(base-address, prefix: "0x")
end method compute-base-address;

