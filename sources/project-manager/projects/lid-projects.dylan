Module: lid-projects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//// Default file suffices.

define constant $dylan-database-suffix = "ddb";
define constant $dylan-profile-suffix  = "prf";
define variable *dylan-source-suffix*  = "dylan";

define constant $standard-lid-keyword = #[#"comment",
					  #"library",
					  #"files",
					  #"lid",
					  #"compilation-mode",
					  #"loose-library-bindings",
					  #"executable",
					  #"base-address",
					  #"linker-options",
					  #"c-source-files",
					  #"c-header-files",
					  #"c-object-files",
					  #"c-libraries",
					  #"rc-files",
					  #"major-version",
					  #"minor-version",
					  #"library-pack",
                                          #"jam-includes"];

define constant $simple-build-keyword = #[#"executable",
					  #"base-address",
					  #"debug-command",
					  #"debug-arguments",
					  #"debug-machine",
					  #"debug-directory",
					  #"start-function",
					  #"major-version",
					  #"minor-version",
					  #"library-pack"];


define constant $list-build-keyword = #[#"linker-options",
					#"c-source-files",
					#"c-files",
					#"c-header-files",
					#"c-object-files",
					#"c-libraries",
					#"rc-files",
					#"other-files",
					#"broken-files",
                                        #"jam-includes"];

define open generic project-lid-library-name
  (p :: <project>) => (name :: <symbol>);

define open generic project-registered-name
  (p :: <project>) => (name :: <symbol>);

define open generic project-lid-location
  (p :: <project>) => (lid :: <file-locator>);

define open abstract class <lid-project> (<project>)
  slot project-major-version :: <integer>,
    init-keyword: major-version:,
    setter: project-major-version-slot-setter;
  slot project-minor-version :: <integer>,
    init-keyword: minor-version:,
    setter: project-minor-version-slot-setter;
  slot project-library-pack :: <integer> = 0,
    init-keyword: library-pack:,
    setter: project-library-pack-slot-setter;
  slot project-library-loose-bindings :: <sequence> = #(),
    init-keyword: loose-bindings:,
    setter: project-library-loose-bindings-slot-setter;
  slot project-library-tight-bindings :: <sequence> = #(),
    init-keyword: tight-bindings:,
    setter: project-library-tight-bindings-slot-setter;
  slot project-lid-location :: <file-locator>,
    init-keyword: lid-location:;
  slot project-lid-date,
    init-keyword: lid-date:;
  slot project-lid-library-name :: <symbol>,
    init-keyword: library-name:;
  slot project-build-settings-slot :: false-or(<sequence>) = #f,
    init-keyword: build-settings:;
  slot project-lid-file-info,
    init-keyword: lid-file-info:;
  slot project-source-files :: false-or(<vector>),
    init-keyword: source-files:;
  slot project-source-record-class :: <class>,
    init-keyword: source-record-class:;
  slot project-compilation-mode :: <symbol>, 
    init-keyword: compilation-mode:,
    setter: project-compilation-mode-slot-setter;
  slot project-compiler-back-end :: <symbol>,
    init-keyword: compiler-back-end:,
    setter: project-compiler-back-end-slot-setter;
  slot project-processor, 
    init-keyword: processor:,
    setter: project-processor-slot-setter;
  slot project-operating-system, 
    init-keyword: operating-system:,
    setter: project-operating-system-slot-setter;
  slot project-target-type :: <project-target-type> = #"executable", 
    init-keyword: target-type:,
    setter: project-target-type-slot-setter;
end class;

define method project-read-only?(project :: <lid-project>) => (flag :: <boolean>);
  ~project.project-personal-library?
end;

define method project-dylan-sources(project :: <lid-project>)
 => (sources :: <vector>);
  project.project-source-files | #[]
end;


// TO DO: we are checking only the locator equivalency
// is this OK long term ?

define method 
    project-source-canonical-source-record(project :: <lid-project>, file :: <file-locator>)
 => (record :: false-or(<source-record>), modified? :: <boolean>);
  let sr = any?(method(sr) sr.source-record-location = file & sr end, 
		project.project-canonical-source-records);
  values(sr, sr & source-record-modified?(sr))
end;

define method project-keyword-property (project :: <lid-project>,
					key :: <symbol>,
					#key default = unsupplied())
 => (value);
  let properties = project-lid-file-info(project);
  let value = element(properties, key, default: $unfound);
  if (found?(value))
    value
  else
    next-method()
  end;
end method;

define function project-keyword-property-setter
    (value, project :: <lid-project>, key :: <symbol>)
  let properties = project-lid-file-info(project);  
  properties[key] := value
end;

define method project-build-settings(project :: <lid-project>)
 => (settings :: <sequence>);
  unless(project.project-build-settings-slot)
    let src-location = project-source-location(project);
    let properties = project-lid-file-info(project);
    project.project-build-settings-slot := 
      lid-build-settings(src-location, properties);
  end;
  project.project-build-settings-slot
end;


define method project-build-property(project :: <lid-project>,
				     key :: <symbol>)
 => (property);
  case
    member?(key, $list-build-keyword) =>
      project-keyword-property(project, key, default: #f);
    member?(key, $simple-build-keyword) =>
      let property = project-keyword-property(project, key, default: #f);
      property & first(property);
    otherwise =>
      error("project-build-property: %s is not a build property", key);
      #f;
  end case;
end;

/** TODO: commented out for the sake of the emulator
define method project-build-property(project :: <lid-project>,
				     key :: <symbol>)
 => (property == #f);
  error("project-build-property: %s is not a build property", key);
  #f
end;

define method project-build-property(project :: <lid-project>,
				     key :: <extra-build-keyword>)
 => (property :: false-or(<sequence>));
  project-keyword-property(project, key, default: #f);
end;
**/

/** TO DO: commented out for the sake of emulator
define method project-build-property(project :: <lid-project>,
				     key :: <simple-build-keyword>)
 => (property :: false-or(<string>));
  let property = project-keyword-property(project, key, default: #f);
  if(property)
    first(property)
  else
    #f
  end;
end;
**/

define method project-build-property-setter(property,
                                            project :: <lid-project>,
	      			            key :: <symbol>)
 => (property);
  case
    member?(key, $list-build-keyword) =>
      // invalidate current settings
      project.project-build-settings-slot := #f;
      project-keyword-property(project, key) := property;
    member?(key, $simple-build-keyword) =>
      // invalidate current settings
      project.project-build-settings-slot := #f;
      project-keyword-property(project, key) := property & vector(property);
    otherwise =>
      error("project-build-property-setter: %s is not an extra build property", key);
  end case;
  property
end;

/*
define method project-build-property-setter(value :: false-or(<sequence>),
					    project :: <lid-project>,
					    key :: <extra-build-keyword>)
 => (property :: false-or(<sequence>));
  // invalidate current settings
  project.project-build-settings-slot := #f;
  project-keyword-property(project, key) := value
end;

define method project-build-property-setter(value :: false-or(<sequence>),
					    project :: <lid-project>,
					    key :: <symbol>)
 => (property == #f)
  error("project-build-property-setter: %s is not an extra build property", key);
  #f
end;
*/

//---*** andrewa: it would be nice to preserve the case here...
define method project-executable-name(project :: <lid-project>)
 => (name :: <string>);
  let executable-properties
    = project-keyword-property(project, #"executable", default: #f);
  let symbol
    = if (executable-properties)
	executable-properties.first
      else
	project.project-registered-name
      end;
  as-lowercase(as(<string>, symbol))
end;

define method project-executable-name-setter(name :: <string>, 
					     project :: <lid-project>) 
 => (name :: <string>);
  // invalidate current settings
  project.project-build-settings-slot := #f;
  project-keyword-property(project, #"executable") := list(name);
  name
end;

define method project-target-type-setter(type :: <project-target-type>, 
					 project :: <lid-project>)
 => (type :: <project-target-type>);
  project-keyword-property(project, #"target-type") := list(type);
  project.project-target-type-slot := type
end;

define method project-library-loose-bindings-setter(bindings :: <sequence>, 
						    project :: <lid-project>)
 => (bindings :: <sequence>);
  project-keyword-property(project, #"loose-library-bindings") := bindings;
  project.project-library-loose-bindings-slot := bindings
end;

define method project-library-tight-bindings-setter(bindings :: <sequence>, 
						    project :: <lid-project>)
 => (bindings :: <sequence>);
  let properties = project-lid-file-info(project);
  properties[#"tight-library-bindings"] := bindings;
  project.project-library-tight-bindings-slot := bindings
end;

define method project-name
    (project :: <lid-project>) => (name :: <symbol>)
  project.project-registered-name
end method project-name;

define method project-library-name
    (project :: <lid-project>) => (name :: <symbol>)
  project.project-lid-library-name
end method project-library-name;

define method project-compiler-back-end-setter(back-end, project :: <lid-project>)
  project-compiler-back-end-slot(project) := back-end;
  project-compiler-setting(project, back-end:) := back-end;
end;

define method project-processor-setter(processor, project :: <lid-project>)
  project-processor-slot(project) := processor;
  project-compiler-setting(project, processor:) := processor;
end;

define method project-operating-system-setter(os, project :: <lid-project>)
  project-operating-system-slot(project) := os;
  project-compiler-setting(project, opearating-system:) := os;
end;

define method project-major-version-setter(version :: <integer>, 
					   project :: <lid-project>)
 => (version :: <integer>);
  project-keyword-property(project, #"major-version")
    := list(integer-to-string(version));
  project.project-major-version-slot := version
end;

define method project-minor-version-setter(version :: <integer>, 
					   project :: <lid-project>)
 => (version :: <integer>);
  project-keyword-property(project, #"minor-version")
    := list(integer-to-string(version));
  project.project-minor-version-slot := version
end;

define method project-library-pack-setter(library-pack :: <integer>, 
					  project :: <lid-project>)
 => (version :: <integer>);
  project-keyword-property(project, #"library-pack")
    := list(integer-to-string(library-pack));
  project.project-library-pack-slot := library-pack;
  project-compiler-setting(project, library-pack:) := library-pack
end;

define method project-compilation-mode-setter(mode, project :: <lid-project>)
  project-compilation-mode-slot(project) := mode;
  project-keyword-property(project, #"compilation-mode") := list(mode);
  // TO DO: this is a conservative approach to avoid a crash in compiler/dood
  project-reset-database(project);
  do(project-reset-database, project.project-owners);
  project-compiler-setting(project, mode:) := mode;
end;

define method project-library-version(p :: <lid-project>)
 => (major-ver :: <integer>, minor-ver :: <integer>);
  values(p.project-major-version, p.project-minor-version)
end;

define method project-inter-library-binding
    (project ::  <lid-project>, used-project :: <lid-project> ) 
 => (mode :: one-of(#"tight", #"loose"));
  let binding = project-dynamic-environment(#"default-binding");
  let default-binding = binding & as(<symbol>, binding);
  let loose-bindings = project.project-library-loose-bindings;
  let tight-bindings = project.project-library-tight-bindings;
  
  member?(used-project.project-registered-name, loose-bindings) & #"loose"
      |
  member?(used-project.project-registered-name, tight-bindings) & #"tight"
    |
    (default-binding | *default-inter-library-binding*)

end;

define method initialize (project :: <lid-project>, #rest keys,
			  #key lid-location = #f,
			  library-name = #f,
			  lid-file-info = #f,
			  source-record-class = #f,
			  processor, operating-system, mode, #all-keys)
  next-method();
  assert(source-record-class, "<lid-project>: source-record-class not supplied");
  project-source-record-class(project) := source-record-class; 

  unless (library-name & lid-file-info)
    assert(lid-location, "<lid-project>: lid-location not supplied");
    project-lid-location(project) := lid-location;
    project-lid-date(project) := file-property(lid-location, #"write-date");
    let (library-name, files, properties) = read-lid-data(lid-location);
    project-lid-library-name(project) := library-name;
    project-lid-file-info(project) := properties;
    project-source-files(project) := files;
  end;

  apply(reinitialize-lid-project, project, keys)

end method;

define method reinitialize-lid-project(project :: <lid-project>,
				       #key processor, operating-system, mode, 
				       #all-keys) => ();
  let properties = project-lid-file-info(project);

  let compilation-mode 
    = if (mode)
        mode
      else
        let declared-mode 
          = element(properties, #"compilation-mode", default: #f);
        if (declared-mode) 
	  as(<symbol>, declared-mode.first) 
	else 
	  *default-compilation-mode* 
	end;
      end if;
  
  project-compilation-mode-slot(project) := compilation-mode;

  let major-version = element(properties, #"major-version", default: #f);
  if(major-version) 
    major-version := string-to-integer(major-version.first);
  else 
    major-version := *default-library-major-version*
  end;

  project-major-version-slot(project) := major-version;

  let minor-version = element(properties, #"minor-version", default: #f);
  if(minor-version) minor-version := string-to-integer(minor-version.first)
  else minor-version := *default-library-minor-version* end;
  project-minor-version-slot(project) := minor-version;

  let library-pack = element(properties, #"library-pack", default: #f);
  when (library-pack)
    let string = library-pack.first;
    let (pack, unused) = string-to-integer(string, default: -1);
    when (pack = -1 | unused < size(string))
      pack := library-pack-number(as(<symbol>, string))
    end;
    library-pack := pack
  end;
  project-library-pack-slot(project) := library-pack | *default-library-library-pack*;

// those two slots are defaulted in the class definition
  let target-type = element(properties, #"target-type", default: #f);
  if(target-type) 
    project-target-type-slot(project) := as(<symbol>, first(target-type))
  end;

  let loose-bindings = element(properties, #"loose-library-bindings", default: #());
  if(loose-bindings)
    project-library-loose-bindings-slot(project) 
      := map(curry(as, <symbol>), loose-bindings)
  end;
  let tight-bindings = element(properties, #"tight-library-bindings", default: #());
  if(tight-bindings)
    project-library-tight-bindings-slot(project) 
      := map(curry(as, <symbol>), tight-bindings)
  end;
end method;

define open abstract class <project-layout> (<project>) 
  slot project-build-location :: false-or(<directory-locator>), 
    setter: project-build-location-slot-setter; // build-location can be #f
  slot project-database-location :: <file-locator>,
    setter: project-database-location-slot-setter;
  slot project-profile-location :: <file-locator>,
    setter: project-profile-location-slot-setter;
end;

define method project-build-location-setter
    (location :: false-or(<directory-locator>), project :: <project-layout>)
 => (location :: false-or(<directory-locator>))
  project-build-location-slot(project) := location;
  project-compiler-setting(project, build-location:) := location;
end;
  
define method project-database-location-setter
    (location :: <file-locator>, project :: <project-layout>)
 => (location :: <file-locator>)
  project-database-location-slot(project) := location;
  project-compiler-setting(project, database-location:) := location;
end;
  
define method project-profile-location-setter
    (location :: <file-locator>, project :: <project-layout>)
 => (location :: <file-locator>)
  project-profile-location-slot(project) := location;
  project-compiler-setting(project, profile-location:) := location;
end;
  
define function lid-build-settings (source-loc, properties) 
 => (settings :: <list>)
  let build-settings = #();
  local method add-setting(key, value)
	  build-settings := pair(key, pair(value, build-settings))
	end;
  local method source-dir (file-name)
	  merge-locators(as(<file-locator>, file-name), source-loc)
	end;
  let c-names = element(properties, #"c-source-files", default: #f);
  if (c-names)
    add-setting(c-source-files: map(source-dir, c-names))
  else
    let c-names = element(properties, #"c-files", default: #f);
    if (c-names) add-setting(c-source-files: map(source-dir, c-names)) end
  end;
  let h-names = element(properties, #"c-header-files", default: #f);
  if (h-names) add-setting(c-header-files: map(source-dir, h-names)) end;
  let o-names = element(properties, #"c-object-files", default: #f);
  if (o-names) add-setting(c-object-files: map(source-dir, o-names)) end;
  let c-libs = element(properties, #"c-libraries", default: #f);
  if (c-libs) add-setting(c-libraries: c-libs) end;
  let rc-names = element(properties, #"rc-files", default: #f);
  if (rc-names) add-setting(rc-files: map(source-dir, rc-names)) end;
  let jam-names = element(properties, #"jam-includes", default: #f);
  if (jam-names) add-setting(rc-files: map(source-dir, jam-names)) end;
  //
  //---*** NOTE: Gwydion uses Executable: as well to name the file but,
  //---*** in their implementation, its presence also indicates that
  //---*** we're building an application rather than a library.
  let executable = element(properties, #"executable", default: #f);
  if (executable) add-setting(executable: first(executable)) end;
  //
  let raw-base-address = element(properties, #"base-address", default: #f);
  if (raw-base-address) 
    let (base-address, extra?) = string-to-machine-word(first(raw-base-address));
    //---*** Should we do something (i.e., complain) if there's extra text?
    add-setting(base-address: base-address)
  end;
  let linker-options = element(properties, #"linker-options", default: #f);
  if (linker-options) add-setting(linker-options: linker-options) end;
  //
  //---*** Are there any other build settings that we need to support?
  build-settings
end function;

// TO DO:
// we calculate here the location of dylan source files relative to lid/project file location
// In other places we use project-source-location
// remnant of registry projects, but stll works
define function read-lid-data (lid-location :: <file-locator>)
  let (library-name, file-names, properties)
    = read-file-library-description(lid-location);
  let files = file-names & 
    map-as(<vector>, method (name :: <string>)
		       let relative-file = as(<file-locator>, name);
		       merge-locators(make(<file-locator>,
					   directory: relative-file.locator-directory,
					   base:      relative-file.locator-base,
					   extension: *dylan-source-suffix*),
				      lid-location.locator-directory)
		     end,
	   file-names);
  values(library-name, files, properties)
end function;

define open generic update-project-files (project :: <lid-project>) => ();

define open generic update-project-location(project :: <lid-project>);

define open generic project-compiler-source-files
    (project :: <lid-project>)
 => (location :: false-or(<sequence>));

define method project-compiler-source-files
    (project :: <lid-project>)
 => (location :: false-or(<sequence>));
  project.project-source-files;
end;

define function project-files-to-source-records
    (project :: <lid-project>, #key directory, files, id-source-record)
 => sr*;
  //debug-message("to-source-records: directory: %s, files: %s",
  //    as(<string>, directory),
  //	map(curry(as, <string>), files));
  let sr-class = project.project-source-record-class;
  local
    method add-file (srs-so-far, file)
      let ids = file-source-record-ids(sr-class, directory, file);
      for (id in ids,
	   srs = srs-so-far then pair(id-source-record(id), srs))
      finally srs;
      end for;
    end method;
  reverse!(reduce(add-file, #(), files));
end;

define function compute-compiler-source-records 
    (project :: <lid-project>)
 => sr*;
  update-project-files(project);
  local 
    method id-source-record (id)
      project-record-id-source-record(project, id)
    end method;

  let directory = project.project-compiler-source-location;
  let project-files = project.project-compiler-source-files;
  project-files & 
    project-files-to-source-records(project, 
				    directory: directory, 
				    files: project-files,
				    id-source-record: id-source-record)
end;


define method note-compiled-definitions (project :: <lid-project>)
  generate-makefile(project);
  copy-extra-records(project, build-settings: project.project-build-settings);
  // This is just for emacs support (not used by environment).
  project-dump-emacs-dispatch-colors(project);
  // This is just for windbg support (not used by environment).
  maybe-dump-combined-sources(project);
end;

define method copy-extra-records (project :: <lid-project>,
				  #rest keys, #key build-settings = #())
  local method doit (#key executable = #f,
			  base-address = #f,
			  linker-options = #(),
			  c-source-files = #(),
			  c-header-files = #(),
			  c-object-files = #(),
			  rc-files = #(),
			  c-libraries = #())
	  local method do-one-set (source-files)
		  for (source-file in source-files)
		    debug-message("  Copying %s to build area", as(<string>, source-file));
		    let target-file
		      = make(<file-locator>,
			     directory: project.project-build-location, 
			     name: locator-name(source-file));
		    block()
		      copy-file(source-file, target-file, if-exists: #"replace")
		    exception(e :: <file-system-error>) 
		      apply(user-warning, condition-format-string(e),
			    condition-format-arguments(e))
		    end block;
		  end for
		end method do-one-set;
	  if (~empty?(c-source-files) | ~empty?(c-header-files) | ~empty?(c-object-files)
		| ~empty?(rc-files))
	    debug-message("Copying extra files for: %s", project);
	    do-one-set(c-source-files);
	    do-one-set(c-header-files);
	    do-one-set(c-object-files);
	    do-one-set(rc-files);
	  end
	end method doit;
  apply(doit, build-settings)
end method copy-extra-records;
