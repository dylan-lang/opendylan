Module:    projects-protocol-internals
Synopsis:  Project build protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Build target protocols

define open abstract primary class <build-target> (<object>)
end class <build-target>;

define macro target-protocols-definer
  { define target-protocols ()
      ?settings:*
    end }
    => { ?settings }
 settings:
  { } => { }
  { setting ?name:name :: ?type:expression; ... }
    => { define open generic "target-" ## ?name
             (target :: <build-target>)
          => (?name :: ?type);
         define open generic "target-" ## ?name ## "-setter"
             (?name :: ?type, target :: <build-target>)
          => (?name :: ?type);
         ... }
  { constant setting ?name:name :: ?type:expression; ... }
    => { define open generic "target-" ## ?name
             (target :: <build-target>)
          => (?name :: ?type);
         ... }
end macro target-protocols-definer;

define open generic target-project
    (target :: <build-target>) => (project :: <project>);

define open generic target-source-files
    (target :: <build-target>) => (files :: <sequence>);

define open generic target-files
    (target :: <build-target>) => (files :: <sequence>);

define open generic project-targets
    (project :: <project>)
 => (targets :: <sequence>);

define open generic project-target
    (project :: <project>)
 => (target :: <build-target>);

define open generic target-source-records
    (target :: <build-target>) => (records :: <sequence>);

define open generic target-canonical-source-records
    (target :: <build-target>) => (records :: <sequence>);

define open generic target-file-canonical-source-record
    (target :: <build-target>, source :: <file-locator>)
 => (record :: false-or(<source-record>), modified? :: <boolean>);

define target-protocols ()
  setting processor :: <processor>;
  setting operating-system :: <operating-system>;
  setting compilation-mode :: <compilation-mode>;
  setting copy-sources? :: <boolean>;
  setting read-only? :: <boolean>;

  setting build-directory :: false-or(<directory-locator>);
  setting database-directory :: false-or(<directory-locator>);
  setting profile-directory :: false-or(<directory-locator>);
  setting bin-directory :: false-or(<directory-locator>);
  setting release-directory :: false-or(<directory-locator>);

  setting library-loose-bindings :: <sequence>;
  setting library-tight-bindings :: <sequence>;

  setting linker :: false-or(<linker>);
  setting filename :: false-or(<string>);
  setting type :: <project-target-type>;
  setting linker-options :: false-or(<string>);
  setting base-address-string :: false-or(<string>);

  setting debug-command :: false-or(<string>);
  setting debug-arguments :: false-or(<string>);
  setting debug-machine :: false-or(<string>);
  setting debug-directory :: false-or(<string>);
  setting start-function :: false-or(<string>);
end target-protocols;

define method target-workspace
    (target :: <build-target>) => (workspace :: <project-workspace>)
  target.target-project.project-workspace
end method target-workspace;


/// Build protocols

define class <build-request> (<object>)
  constant slot build-target :: <build-target>,
    required-init-keyword: target:;
  constant slot build-compile? :: <boolean> = #t,
    init-keyword: compile?:;
  constant slot build-link? :: <boolean> = #t,
    init-keyword: link?:;
  constant slot build-release? :: <boolean> = #t,
    init-keyword: release?:;
  constant slot build-subprojects? :: <boolean> = #t,
    init-keyword: subprojects:;
  constant slot build-clean? :: <boolean> = #f,
    init-keyword: clean?:;
  constant slot build-copy-sources? :: <boolean> = *copy-canonical-sources?*,
    init-keyword: copy-sources?:;
  constant slot build-compilation-mode :: false-or(<compilation-mode>) = #f,
    init-keyword: compilation-mode:;
  constant slot build-save? :: <boolean> = #t,
    init-keyword: save?:;
  constant slot build-linker :: false-or(<linker>) = #f,
    init-keyword: linker:;
  constant slot build-target-type :: false-or(<project-target-type>) = #f,
    init-keyword: target-type:;
  constant slot build-unify? :: <boolean> = #t,
    init-keyword: unify?:;
  constant slot build-exports? :: <boolean> = #t,
    init-keyword: exports?:;
  constant slot build-abort-reason :: <abort-reason> = #"never",
    init-keyword: abort-reason:;
  constant slot build-debug-output :: <sequence> = #[],
    init-keyword: debug-output:;
  constant slot build-progress-callback :: false-or(<function>) = #f,
    init-keyword: progress-callback:;
end class <build-request>;

define open generic open-project-database
    (project :: <project>,
     #key target :: false-or(<build-target>))
 => ();

define open generic remove-target-build-products
    (target :: <build-target>, #key subprojects? :: <boolean>)
 => ();

define open generic targets-to-recompile
    (target :: <build-target>)
 => (targets :: <sequence>);

define open generic targets-to-relink
    (target :: <build-target>)
 => (targets :: <sequence>);

define open generic compile-target-library
    (target :: <build-target>, build :: <build-request>)
 => ();

define open generic generate-link-makefile
    (target :: <build-target>, build :: <build-request>)
 => ();

define open generic link-target-executable
    (target :: <build-target>, build :: <build-request>)
 => ();

define open generic note-workspace-build-operation-started
    (workspace :: <project-workspace>, target :: <build-target>)
 => ();

define open generic note-workspace-build-operation-finished
    (workspace :: <project-workspace>)
 => ();


/// Build operations

define function build-project
    (project :: <project>, build :: <build-request>)
 => (success? :: <boolean>)
  let target = build.build-target;
  build-project-target(target, build)
end function build-project;

define method build-project-target
    (target :: <build-target>, build :: <build-request>)
 => (success? :: <boolean>)
  let workspace = target.target-project.project-workspace;
  with-workspace-build (workspace, target)
    block (return)
      local method perform-build-operation
		(function :: <function>, phase :: <string>,
		 start :: <integer>, finish :: <integer>)
	      note-build-phase-starting(target, phase, start, finish);
	      function(target, build);
	      note-build-phase-finished(target, phase, start, finish)
	    end method perform-build-operation;
      perform-build-operation(prepare-targets, "Preparing", 0,  10);
      perform-build-operation(compile-targets, "Compiling", 10, 80);
      perform-build-operation(link-targets,    "Linking",   80, 90);
      perform-build-operation(release-targets, "Releasing", 90, 100);
      #t
    exception (<abort>)
      #f
    end
  end
end method build-project-target;

define function abort-build
    (project :: <project>)
  build-serious-warning(project, "Aborting compilation due to errors");
  abort()
end function abort-build;

define method prepare-targets
    (target :: <build-target>, build :: <build-request>) => ()
  let targets       = target.target-build-targets;
  let clean?        = build.build-clean?;
  let total         = targets.size;
  note-build-progress(target, 0, total);
  for (subtarget :: <build-target> in targets,
       index :: <integer> from 0)
    build-message(target, "Updating sources for %s", target.target-title);
    refresh-target-files(subtarget, clean?: clean?);
    copy-source-files(subtarget, build);
    note-build-progress(target, index, total)
  end
end method prepare-targets;

define method compile-targets
    (target :: <build-target>, build :: <build-request>) => ()
  let targets = build-compile-targets(target, build);
  for (subtarget :: <build-target> in targets,
       index :: <integer> from 1)
    compile-target-library(subtarget, build)
  end
end method compile-targets;

define method link-targets
    (target :: <build-target>, build :: <build-request>) => ()
  let targets = build-link-targets(target, build);
  for (subtarget :: <build-target> in targets,
       index :: <integer> from 1)
    install-build-targets(subtarget, build);
    if (subtarget == target)
      link-target-executable(target, build)
    end
  end
end method link-targets;

define method release-targets
    (target :: <build-target>, build :: <build-request>) => ()
  if (build.build-release?)
    build-target-release(target)
  end
end method release-targets;

define method copy-source-files
    (target :: <build-target>, build :: <build-request>) => ()
  if (target.target-copy-sources?)
    install-build-files(target, <dylan-file-information>)
  end
end method copy-source-files;

define method build-compile-targets
    (target :: <build-target>, build :: <build-request>)
 => (targets :: <sequence>)
  let compile?     = build.build-compile?;
  let clean?       = build.build-clean?;
  let subprojects? = build.build-subprojects?;
  let targets :: <sequence>
    = case
	~compile? => #[];
	clean?    => target.target-build-targets;
	otherwise => target.targets-to-recompile;
      end;
  if (~subprojects?)
    for (subtarget :: <build-target> in targets)
      unless (subtarget == target)
	let project = subtarget.target-project;
	build-serious-warning
	  (project, "Ignoring changes in %s", target.target-title)
      end
    end;
    if (member?(target, targets))
      vector(target)
    else
      #[]
    end
  else
    targets
  end
end method build-compile-targets;

define method build-link-targets
    (target :: <build-target>, build :: <build-request>)
 => (targets :: <sequence>)
  let link?  = build.build-link?;
  let clean? = build.build-clean?;
  let subprojects? = build.build-subprojects?;
  let targets :: <sequence>
    = case
	~link?    => #[];
	clean?    => target.target-build-targets;
	otherwise => target.targets-to-relink;
      end;
  if (~subprojects?)
    for (subtarget :: <build-target> in targets)
      unless (subtarget == target)
	let project = subtarget.target-project;
	build-serious-warning
	  (project, "Ignoring changes in %s", target.target-title)
      end
    end;
    if (member?(target, targets))
      vector(target)
    else
      #[]
    end
  else
    targets
  end
end method build-link-targets;

define method install-build-targets
    (target :: <build-target>, build :: <build-request>) => ()
  let files = target.target-files;
  generate-link-makefile(target, build);
  install-build-files(target, <linker-file-information>)
end method install-build-targets;

define method install-build-files
    (target :: <build-target>, type :: subclass(<file-information>)) => ()
  let directory = target.target-build-directory;
  do-target-files
    (method (file)
       install-file
	 (target, file, directory,
	  "Installing %s in the build directory")
     end,
     target, type: type)
end method install-build-files;

define method install-file
    (target :: <build-target>, file :: <file-information>,
     new-directory :: <directory-locator>, message :: false-or(<string>))
 => ()
  let project   = target.target-project;
  let directory = project.project-directory;
  let original  = merge-locators(file.file-location, directory);
  let copy
    = make(<file-locator>,
	   directory: new-directory,
	   base:      original.locator-base,
	   extension: original.locator-extension);
  block()
    message & build-message(target, message, original);
    copy-file(original, copy, if-exists: #"replace")
  exception (error :: <file-system-error>)
    apply(build-serious-warning, project,
	  error.condition-format-string,
	  error.condition-format-arguments)
  end
end method install-file;

define method build-target-release
    (target :: <build-target>) => ()
  error("Not yet implemented!")
end method build-target-release;


/// Build progress

define function build-message
    (target :: <build-target>, format-string :: <string>, #rest format-arguments)
 => ()
  apply(debug-message, format-string, format-arguments)
end function build-message;

define function note-build-phase-starting
    (target :: <build-target>, phase :: <string>,
     start :: <integer>, finish :: <integer>)
 => ()
  build-message(target, "Starting %s", phase)
end function note-build-phase-starting;

define function note-build-phase-finished
    (target :: <build-target>, phase :: <string>,
     start :: <integer>, finish :: <integer>)
 => ()
  build-message(target, "Finished %s", phase)
end function note-build-phase-finished;

define function note-build-progress
    (target :: <build-target>, position :: <integer>, total :: <integer>)
 => ()
  build-message(target, "Progressing: %d/%d", position, total)
end function note-build-progress;


/// Build information

define open abstract class <basic-build-state> (<build-state>)
end class <basic-build-state>;


/// Recognized file extensions

define class <basic-file-information> (<file-information>)
end class <basic-file-information>;

define class <dylan-file-information> (<file-information>)
end class <dylan-file-information>;

define class <project-file-information> (<file-information>)
  sealed constant slot file-project :: false-or(<project>),
    required-init-keyword: project:;
end class <project-file-information>;

define class <linker-file-information> (<file-information>)
end class <linker-file-information>;

define class <release-file-information> (<file-information>)
end class <release-file-information>;

//--- Copy all unrecognized files into the release directory.
//--- Are there file types for which this is the wrong thing to do.
define constant $default-file-information-class = <release-file-information>;

define table $file-information-classes
  = { #"dylan" => <dylan-file-information>,

      #"hdp"   => <project-file-information>,
      #"lid"   => <project-file-information>,
      #"ddb"   => <project-file-information>,

      #"c"     => <linker-file-information>,
      #"h"     => <linker-file-information>,
      #"rc"    => <linker-file-information>,
      #"ico"   => <linker-file-information>,
      #"bmp"   => <linker-file-information>,
      #"lib"   => <linker-file-information>,
      #"obj"   => <linker-file-information>,

      #"dll"   => <release-file-information>,
      #"html"  => <release-file-information> };

define method read-file-information
    (target :: <build-target>, extension :: <symbol>, file :: <file-locator>)
 => (information :: <file-information>)
  let class :: subclass(<file-information>)
    = element($file-information-classes, extension,
	      default: $default-file-information-class);
  make(class,
       location: file)
end method read-file-information;
