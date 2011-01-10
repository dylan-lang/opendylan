Module:    projects-protocol-internals
Synopsis:  Project manager protocol library
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic target

define variable *default-compilation-mode* :: <compilation-mode> = #"tight";

define open abstract class <file-information> (<object>)
  constant slot file-location :: <file-locator>,
    required-init-keyword: locator:;
  constant slot file-timestamp :: <date>,
    required-init-keyword: timestamp:;
end class <file-information>;

define open abstract class <import-file-information> (<file-information>)
  slot file-generated-files :: false-or(<simple-object-vector>) = #f,
    init-keyword: generated-files:;
end class <import-file-information>;

define open abstract class <build-state> (<object>)
  constant slot build-file-information :: <object-table>
    = make(<object-table>);
end class <build-state>;

define open abstract primary class <basic-build-target> (<build-target>)
  constant slot target-project :: <project>,
    required-init-keyword: project:;
  constant slot target-name :: <symbol>,
    required-init-keyword: name:;
  slot target-subtargets :: <simple-object-vector>,
    required-init-keyword: subtargets:;
  slot %files :: false-or(<sequence>) = #f;
  slot target-processor :: <processor>,
    required-init-keyword: processor:;
  slot target-operating-system :: <operating-system>, 
    required-init-keyword: operating-system:;
  slot target-compilation-mode :: <compilation-mode> = *default-compilation-mode*,
    init-keyword: compilation-mode:;
  slot target-copy-sources? :: <boolean> = *copy-canonical-sources?*,
    init-keyword: copy-sources?:;
  slot target-linker :: false-or(<linker>) = #f,
    init-keyword: linker:;
  slot target-filename :: false-or(<string>) = #f,
    init-keyword: filename:;
  slot target-type :: <project-target-type> = #"executable", 
    init-keyword: target-type:;
  slot target-linker-options :: false-or(<string>) = #f,
    init-keyword: linker-options:;
  slot target-base-address-string :: false-or(<string>) = #f,
    init-keyword: base-address-string:;
  slot target-library-loose-bindings :: <sequence> = #(),
    init-keyword: loose-bindings:;
  slot target-library-tight-bindings :: <sequence> = #(),
    init-keyword: tight-bindings:;
  slot target-build-state :: false-or(<build-state>) = #f,
    init-keyword: build-state:;
  slot %targets :: false-or(<stretchy-object-vector>) = #f;
  slot target-debug-command :: false-or(<string>) = #f,
    init-keyword: debug-command:;
  slot target-debug-arguments :: false-or(<string>) = #f,
    init-keyword: debug-arguments:;
  slot target-debug-machine :: false-or(<string>) = #f,
    init-keyword: debug-machine:;
  slot target-debug-directory :: false-or(<string>) = #f,
    init-keyword: debug-directory:;
  slot target-start-function :: false-or(<string>) = #f,
    init-keyword: start-function:;

  constant slot target-source-records-table :: <string-table>
    = make(<string-table>);
end class <basic-build-target>;

define method target-title
    (target :: <basic-build-target>) => (title :: <string>)
  as-lowercase(as(<string>, target.target-name))
end method target-title;


/// Subtargets

define method target-build-targets
    (target :: <basic-build-target>) => (targets :: <sequence>)
  target.%targets
    | begin
	let targets :: <stretchy-object-vector>
	  = make(<stretchy-object-vector>);
	local method do-targets
		  (target :: <basic-build-target>) => ()
		do(do-targets, target.target-subtargets);
		add!(targets, target)
	      end method do-targets;
	do-targets(target);
	target.%targets := targets
      end
end method target-build-targets;

define inline method do-build-targets
    (function :: <function>, target :: <basic-build-target>) => ()
  do(function, target.target-build-targets)
end method do-build-targets;


/// Source files

define open generic read-file-information
    (target :: <build-target>, extension :: <symbol>, file :: <file-locator>)
 => (information :: <file-information>);

define open generic generate-files
    (target :: <build-target>, information :: <file-information>)
 => (files :: <simple-object-vector>);

//--- We almost certainly want different targets to support different
//--- sets of sources, but for now we just use the project sources.
define method target-source-files
    (target :: <basic-build-target>) => (source-files :: <sequence>)
  target.target-project.project-source-files
end method target-source-files;

define method target-files
    (target :: <build-target>) => (files :: <sequence>)
  target.%files
    | refresh-target-files(target)
end method target-files;

define method refresh-target-files
    (target :: <build-target>, 
     #key clean? :: <boolean> = #f)
 => (files :: <sequence>)
  let project = target.target-project;
  let source-files :: <deque> = as(<deque>, target.target-source-files);
  let files :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let dylan-extension = as(<symbol>, dylan-file-extension());
  while (~empty?(files))
    block ()
      let source-file :: <file-locator> = pop(source-files);
      let file = refresh-file-information(target, source-file, clean?: clean?);
      let generated-files
	= file.file-generated-files
	    | begin
		file.file-generated-files := generate-files(target, file)
	      end;
      for (source-file in generated-files using backward-iteration-protocol)
	push(source-files, source-file)
      end;
      add!(files, file)
    exception (error :: <project-file-error>)
      let location = error.condition-locator;
      build-serious-warning
	(project, "Skipping %s: %s",
	 if (location.locator-base)
	   location.locator-name
	 else
	   as(<string>, location)
	 end,
	 error.condition-to-string)
    end
  end;
  target.%files := files
end method refresh-target-files;

define method do-target-files
    (function :: <function>, target :: <basic-build-target>,
     #key type :: false-or(subclass(<file-information>)) = #f)
  let files = target.target-files;
  do(method (file :: <file-information>)
       if (~type | instance?(file, type))
	 function(file)
       end
     end,
     files)
end method do-target-files;

define method refresh-file-information
    (target :: <basic-build-target>, file :: <file-locator>,
     #key clean? :: <boolean> = #f)
 => (information :: <file-information>)
  let information = target-source-file-information(target, file);
  if (clean?
	| begin
	    let timestamp = information.file-timestamp;
	    timestamp < file-property(file, #"modification-date")
	  end)
    let type = file.locator-extension-type;
    target-source-file-information(target, file)
      := read-file-information(target, type, file)
  else
    file
  end
end method refresh-file-information;

define method target-source-file-information
    (target :: <basic-build-target>, file :: <file-locator>)
 => (information :: false-or(<file-information>))
  let build = target.target-build-state;
  if (build)
    let table = build.build-file-information;
    element(table, file, default: #f)
  end
end method target-source-file-information;

define method target-source-file-information-setter
    (information :: <file-information>, target :: <basic-build-target>,
     file :: <file-locator>)
 => (information :: <file-information>)
  let build = target.target-build-state;
  assert(build, "Setting source file information with no build state!");
  let table = build.build-file-information;
  element(table, file) := information
end method target-source-file-information-setter;


/// Source records

define constant <target-source-record> = <file-source-record>;
define constant <source-record-id>     = <string>;

define method target-source-records
    (target :: <basic-build-target>)
 => (source-records :: <stretchy-object-vector>)
  let project       = target.target-project;
  let copy-sources? = target.target-copy-sources?;
  let files         = target.target-files;
  let source-records :: <stretchy-object-vector> 
    = make(<stretchy-object-vector>);
  let directory 
    = case
	copy-sources? => target.target-build-directory;
	otherwise     => project.project-directory;
      end;
  do-target-files
    (method (file :: <dylan-file-information>)
       let source-location = file.file-location;
       let location
	 = case
	     copy-sources? => source-location.locator-name;
	     otherwise     => source-location;
	   end;
       do(method (id :: <source-record-id>)
	    add!(source-records, target-id-source-record(target, id))
	  end,
	  file-source-record-ids(<target-source-record>, directory, location))
     end,
     target,
     type: <dylan-file-information>);
  source-records
end method target-source-records;

define method target-id-source-record
    (target :: <basic-build-target>, id :: <source-record-id>)
 => (source-record :: <target-source-record>)
  let table = target.target-source-records-table;
  element(table, id, default: #f)
    | begin
	let project = target.target-project;
	let directory = project.project-directory;
	let record
	  = id-as-source-record(<target-source-record>, project, directory, id);
	element(table, id) := record
      end
end method target-id-source-record;
