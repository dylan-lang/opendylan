Module:    projects-protocol-internals
Synopsis:  Project manager protocol library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $project-file-format-version = 3;

//---*** What should these really be?
define constant <project-name>        = <symbol>;
define constant <library-name>        = <symbol>;
define constant <compilation-mode>    = <symbol>;
define constant <project-target-type> = one-of(#"executable", #"dll");


/// Project properties

define open abstract primary class <project> (<object>)
end class <project>;

define open generic find-project
    (workspace :: <project-workspace>, library-name :: <library-name>)
 => (project :: false-or(<project>));

define open generic open-project
    (workspace :: <project-workspace>, locator :: <file-locator>,
     #key read-only? :: <boolean>)
 => (project :: <project>);

define open generic project-name
    (project :: <project>)
 => (name :: false-or(<project-name>));

define open generic project-title
    (project :: <project>)
 => (name :: <string>);

define open generic project-owners
    (project :: <project>)
 => (owners :: <sequence>);

define open generic project-library-name
    (project :: <project>)
 => (name :: false-or(<library-name>));

define open generic project-location
    (project :: <project>)
 => (location :: false-or(<file-locator>));

define open generic project-directory
    (project :: <project>)
 => (location :: false-or(<directory-locator>));

define open generic project-minor-version
    (project :: <project>)
 => (version :: <integer>);

define open generic project-minor-version-setter
    (version :: <integer>, project :: <project>)
 => (version :: <integer>);

define open generic project-major-version
    (project :: <project>)
 => (version :: <integer>);

define open generic project-major-version-setter
    (version :: <integer>, project :: <project>)
 => (version :: <integer>);

define open generic project-read-only?
    (project :: <project>) => (read-only? :: <boolean>);

define open generic project-source-files
    (project :: <project>) => (files :: <simple-object-vector>);

define open generic save-project
    (project :: <project>, #key filename) => ();

define open generic register-project-condition
    (project :: <project>, condition :: <project-condition>)
 => ();


/// User settings

define open abstract primary class <project-user-settings> (<object>)
end class <project-user-settings>;

define open generic project-user-settings
    (project :: <project>)
 => (settings :: false-or(<project-user-settings>));


/// Basic project

define variable $default-library-major-version :: <integer> = 0;
define variable $default-library-minor-version :: <integer> = 0;

define open abstract primary class <basic-project> (<project>)
  constant slot project-workspace :: <project-workspace>,
    required-init-keyword: workspace:;
  constant slot project-name :: <symbol>,
    required-init-keyword: name:;
  constant slot project-owners :: <stretchy-object-vector>
      = make(<stretchy-object-vector>),
    init-keyword: owners:;
  constant slot project-file-location :: <file-locator>,
    required-init-keyword: file-location:;
  slot project-file-date :: <date>,
    required-init-keyword: file-date:;
/*---*** Can we avoid this?
  slot project-file-info :: <table>,
    init-keyword: file-info:;
*/
  slot project-library-name :: <symbol>,
    required-init-keyword: library-name:;
  slot project-major-version :: <integer> = $default-library-major-version,
    init-keyword: major-version:;
  slot project-minor-version :: <integer> = $default-library-minor-version,
    required-init-keyword: minor-version:;
  slot project-source-files :: <simple-object-vector>,
    required-init-keyword: source-files:;
  slot project-targets :: <stretchy-object-vector> 
      = make(<stretchy-object-vector>);
  slot project-target :: <build-target>,
    required-init-keyword: target:;
  slot project-user-settings :: false-or(<project-user-settings>) = #f,
    init-keyword: user-settings:;
  slot project-can-be-closed? :: <boolean>,
    required-init-keyword: can-be-closed?:;
end class <basic-project>;

define method project-title
    (project :: <basic-project>) => (title :: <string>)
  as-lowercase(as(<string>, project.project-name))
end method project-title;

define method project-directory
    (project :: <basic-project>) => (directory :: <directory-locator>)
  project.project-location.locator-directory
end method project-directory;


/// Reading projects

define method read-project-file
    (workspace :: <project-workspace>, location :: <file-locator>,
     format == 2)
 => (project :: <project>)
  let file-date = file-property(location, #"write-date");
  let (library-name, files, properties) = read-lid-data(location);
  let (major-version, minor-version)
    = lid-project-settings(location, properties);
  let files
    = concatenate(files,
		  get-list-property(location, properties, #"c-files"),
		  get-list-property(location, properties, #"c-source-files"),
		  get-list-property(location, properties, #"c-header-files"),
		  get-list-property(location, properties, #"c-libraries"),
		  get-list-property(location, properties, #"rc-files"),
		  get-list-property(location, properties, #"other-files"));
  let project
    = make-workspace-project
        (workspace,
	 file-location: location,
	 file-date:     file-date,
	 library-name:  library-name,
	 major-version: major-version,
	 minor-version: minor-version,
	 source-files:  files);
  let target
    = make-lid-build-target(workspace, project, location, properties);
  add!(project.project-targets, target);
  project.project-target := target;
  project
end method read-project-file;

define method read-project-file
    (workspace :: <project-workspace>, location :: <file-locator>,
     format == 3)
 => (project :: <project>)
  let file-date = file-property(location, #"write-date");
  let (library-name, files, properties) = read-lid-data(location);
  let (major-version, minor-version)
    = lid-project-settings(location, properties);
  let project
    = make-workspace-project
        (workspace,
	 file-location: location,
	 file-date:     file-date,
	 library-name:  library-name,
	 major-version: major-version,
	 minor-version: minor-version,
	 source-files:  files);
  let target
    = make-lid-build-target(workspace, project, location, properties);
  add!(project.project-targets, target);
  project.project-target := target;
  project
end method read-project-file;


/// Reading LID files

define function read-lid-data 
    (lid-location :: <file-locator>)
 => (library-name :: <library-name>,
     files :: <sequence>, 
     properties :: <table>)
  let (library-name, file-names, properties)
    = read-file-library-description(lid-location);
  let dylan-type = dylan-file-extension();
  let project-directory = lid-location.locator-directory;
  let files
    = map-as(<vector>,
	     method (name :: <string>)
	       let filename = as(<file-locator>, name);
	       let pathname = merge-locators(filename, project-directory);
	       make(<file-locator>,
		    directory: pathname.locator-directory,
		    base:      pathname.locator-base,
		    extension: dylan-type)
	     end,
	     file-names);
  values(library-name, files, properties)
end function read-lid-data;

define function read-file-library-description
    (location :: <file-locator>)
 => (library-name :: <library-name>, 
     file-names :: <sequence>, 
     properties :: <table>)
  let properties = read-file-library-description-internal(location);

  let library-name = get-symbol-property(location, properties, #"library");
  let files-entry  = get-list-property(location, properties, #"files");
  unless (library-name)
    signal(make(<badly-formed-file-header>, 
		format-string:
		  "The library description file %= does not contain "
		  "the entry for library, which is mandatory.",
		format-arguments: vector(location)))
  end;
  values(library-name, files-entry | #[], properties)
end function read-file-library-description;

define function read-file-library-description-internal
    (location :: <file-locator>) => (properties :: <table>)
  iterate do-it ()
    block ()
      let properties :: <table> = read-file-header(location);
      let lid-file = get-single-property(location, properties, #"lid");
      local method merge-properties 
		(p1 :: <table>, p2 :: <table>) 
	     => (merged-properties :: <table>)
	      do(method (key) p2[key] := p1[key] end,
		 key-sequence(p1));
	      p2
	    end method merge-properties;
		
      if (lid-file)
	let lid-locator
	  = make(<file-locator>,
		 directory: location.locator-directory,
		 name:      lid-file);
	let original-properties 
	  = read-file-library-description-internal(lid-locator);
	merge-properties(properties, original-properties)
      else
	properties
      end;
    exception (r :: <simple-restart>)
      do-it()
    end
  end
end function read-file-library-description-internal;

define function make-lid-build-target
    (workspace :: <project-workspace>, project :: <project>, 
     location :: <file-locator>, properties :: <table>)
 => (target :: <build-target>)
  let loose-bindings 
    = get-symbol-list-property
        (location, properties, #"loose-library-bindings");
  let tight-bindings
    = get-symbol-list-property
        (location, properties, #"tight-library-bindings");
  let compilation-mode 
    = get-symbol-property(location, properties, #"compilation-mode")
        | *default-compilation-mode*;
  let target-type
    = get-symbol-property(location, properties, #"target-type");
  let filename
    = get-symbol-property(location, properties, #"executable");
  make-workspace-build-target
    (workspace,
     project:          project,
     name:             project.project-name,
     filename:         filename,
     compilation-mode: compilation-mode,
     processor:        workspace.workspace-processor,
     operating-system: workspace.workspace-operating-system,
     type:             target-type,
     loose-bindings:   loose-bindings,
     tight-bindings:   tight-bindings)
end function make-lid-build-target;

define method lid-project-settings
    (location :: <file-locator>, properties :: <table>)
 => (major-version :: <integer>,
     minor-version :: <integer>)
  let major-version :: <integer>
    = get-integer-property(location, properties, #"major-version")
        | $default-library-major-version;
  let minor-version :: <integer>
    = get-integer-property(location, properties, #"minor-version")
        | $default-library-minor-version;
  values(major-version, minor-version)
end method lid-project-settings;


/// Saving

define method save-project
    (project :: <project>, #key filename) => ()
  if (project.project-read-only?)
    project-error(project, "Cannot save read-only project %s",
		  project.project-title)
  end;
  let filename = filename | project.project-location;
  with-open-file (stream = filename, direction: #"output")
    write-project-file(stream, project, $project-file-format-version)
  end
end method save-project;

define method save-project-as-lid-file
    (project :: <project>, #key filename) => ()
  let filename = filename | project.project-location;
  with-open-file (stream = filename, direction: #"output")
    write-lid-file(stream, project)
  end
end method save-project-as-lid-file;

define method write-lid-file
    (stream :: <stream>, project :: <project>) => ()
  write-lid-library-info(stream, project);
  write-lid-compiler-info(stream, project);
  write-lid-linker-info(stream, project);
  write-lid-debug-info(stream, project);
end method write-lid-file;

define function write-lid-library-info
    (stream :: <stream>, project :: <project>, 
     #key files? :: <boolean> = #t)
 => ()
  write-value(stream, #"library",       project.project-library-name);
  if (files?)
    write-source-files
      (stream, project, #"files", type: #"dylan",
       extension?: #f)
  end;
  write-lid-library-version(stream, project)
end function write-lid-library-info;

define function write-lid-library-version
    (stream :: <stream>, project :: <project>)
 => ()
  write-value(stream, #"major-version", project.project-major-version);
  write-value(stream, #"minor-version", project.project-minor-version);
end function write-lid-library-version;

define function write-lid-compiler-info
    (stream :: <stream>, project :: <project>) => ()
  let target = project.project-target;
  let compilation-mode = target.target-compilation-mode;
  let loose-bindings   = target.target-library-loose-bindings;
  write-value(stream, #"compilation-mode", compilation-mode);
  write-list-value(stream, #"loose-library-bindings", loose-bindings);
end function write-lid-compiler-info;

define function write-lid-linker-info
    (stream :: <stream>, project :: <project>,
     #key base-address-string :: false-or(<string>) = #f)
 => ()
  let target = project.project-target;
  write-value(stream, #"executable",   target.target-filename);
  write-value(stream, #"target-type",  target.target-type);
  write-value(stream, #"base-address", 
	      base-address-string
		| target.target-base-address-string);
  write-list-value(stream, #"linker-options", target.target-linker-options);
  write-source-files(stream, project, #"c-source-files", type: #"c");
  write-source-files(stream, project, #"c-header-files", type: #"h");
  write-source-files(stream, project, #"rc-files",       type: #"rc");
  write-source-files
    (stream, project, #"c-object-files", type: #"obj", relative?: #f);
  write-source-files
    (stream, project, #"c-libraries",    type: #"lib",  relative?: #f)
end function write-lid-linker-info;

define function write-lid-debug-info
    (stream :: <stream>, project :: <project>) => ()
  let target = project.project-target;
  write-value(stream, #"debug-command",   target.target-debug-command);
  write-value(stream, #"debug-arguments", target.target-debug-arguments);
  write-value(stream, #"debug-machine",   target.target-debug-machine);
  write-value(stream, #"debug-directory", target.target-debug-directory);
  write-value(stream, #"start-function",  target.target-start-function);
end function write-lid-debug-info;

define function write-source-files
    (stream :: <stream>, project :: <project>, keyword :: <symbol>,
     #key test :: <function> = identity,
          type :: false-or(<symbol>) = #f,
          subprojects? :: <boolean> = #f,
          relative? :: <boolean> = #t,
          extension? :: <boolean> = #t)
 => ()
  let files 
    = project-source-files-of-type
        (project, test: test, type: type, subprojects?: subprojects?);
  let project-directory = project.project-directory;
  unless (empty?(files))
    write-keyword(stream, keyword);
    for (source-file :: <file-locator> in files)
      let pathname
	= case
	    relative? => source-file;
	    otherwise => relative-locator(source-file, project-directory);
	  end;
      write-list-item
	(stream,
	 case
	   extension? => 
	     pathname;
	   otherwise =>
	     make(<file-locator>,
		  directory: pathname.locator-directory,
		  base:      pathname.locator-base);
	 end)
    end
  end
end function write-source-files;

define function project-source-files-of-type
    (project :: <project>, 
     #key test :: <function> = identity,
          type :: false-or(<symbol>) = #f,
          subprojects? :: <boolean> = #f)
 => (source-files :: <list>)
  let files :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  local method add-project-files (project :: <project>)
	  for (source-file :: <file-locator> in project.project-source-files)
	    if (test(source-file)
		  & (~type | source-file.locator-extension-type == type))
	      add-new!(files, source-file, test: \=)
	    end
	  end
	end method add-project-files;
  if (subprojects?)
    //---*** We need to be careful of the order here...
    let target = project.project-target;
    do-target-files
      (method (file :: <project-file-information>)
	 let project = file.file-project;
	 project & add-project-files(project)
       end,
       target, type: <project-file-information>)
  end;
  add-project-files(project);
  as(<list>, files)
end function project-source-files-of-type;

define method write-value
    (stream :: <stream>, keyword :: <symbol>, value) => ()
  value & format(stream, "%s:\t%s\n", keyword, value)
end method write-value;

define method write-list-value
    (stream :: <stream>, keyword :: <symbol>, values :: <sequence>) => ()
  unless (empty?(values))
    write-keyword(stream, keyword);
    do(curry(write-list-item, stream), values)
  end
end method write-list-value;

define function write-comment
    (stream, comment :: <string>)
  write-value(stream, #"Comment", comment)
end function write-comment;

define method write-keyword
    (stream :: <stream>, keyword :: <symbol>) => ()
  format(stream, "%s:", keyword)
end method write-keyword;

define method write-list-item
    (stream :: <stream>, value :: <string>) =>  ()
  format(stream, "\t%s\n", value)
end method write-list-item;

define method write-list-item
    (stream :: <stream>, value :: <file-locator>) =>  ()
  write-list-item(stream, as(<string>, value))
end method write-list-item;

define method write-project-file
    (stream :: <stream>, project :: <project>, format == 2)
 => ()
  write-comment(stream, "This file is generated, please don't edit");
  write-value(stream, #"format-version", format);
  write-value(stream, #"library", project.project-library-name);
  write-source-files(stream, project, #"files", type: #"dylan");
  write-lid-library-version(stream, project);
  write-lid-compiler-info(stream, project);
  write-lid-linker-info(stream, project);
  write-lid-debug-info(stream, project);
  write-source-files(stream, project, #"other-files",
		     test: method (locator :: <file-locator>)
			     let type = locator.locator-extension-type;
			     select (type)
			       #"dylan", #"project", #"lib", #"ico", #"bmp",
			       #"c", #"h", #"rc" =>
				 #t;
			       otherwise =>
				 #f;
			     end
			   end)
end method write-project-file;

define method write-project-file
    (stream :: <stream>, project :: <project>, format == 3)
 => ()
  write-comment(stream, "This file is generated, please don't edit");
  write-value(stream, #"format-version", format);
  write-value(stream, #"library", project.project-library-name);
  write-source-files(stream, project, #"files");
  write-lid-library-version(stream, project);
  write-lid-compiler-info(stream, project);
  write-lid-linker-info(stream, project);
  write-lid-debug-info(stream, project)
end method write-project-file;
