Module:    dfmc-projects
Synopsis:  DFMC project manager interface
Author:    Andy Armstrong, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Useful constants

define constant $platform-variable = "OPEN_DYLAN_PLATFORM_NAME";
define constant $default-platform  = $platform-name;

define constant <debug-target>   = <object>;
define constant <transaction-id> = <object>;


/// DFMC build request

define class <dfmc-build-state> (<basic-build-state>)
  slot build-canonical-projects :: <object-set> = make(<object-set>);
end class <dfmc-build-state>;

define method build-canonicalized-project
    (build :: <dfmc-build-state>, project :: <dfmc-project>)
 => (canonicalized? :: <boolean>)
  member?(project, build.build-canonical-projects)
end method build-canonicalized-project;

define method build-canonicalized-project-setter
    (canonicalized? :: <boolean>, build :: <dfmc-build-state>, 
     project :: <dfmc-project>)
 => (canonicalized? :: <boolean>)
  add-new!(build.build-canonical-projects, project)
end method build-canonicalized-project-setter;

define method clear-build-canonicalized-projects
    (build :: <dfmc-build-state>) => ()
  remove-all-keys!(build.build-canonical-projects)
end method clear-build-canonicalized-projects;


/// DFMC project workspace

define class <dfmc-project-workspace> (<project-workspace>)
  constant slot workspace-processor :: <processor>,
    required-init-keyword: processor:;
  constant slot workspace-operating-system :: <operating-system>,
    required-init-keyword: operating-system:;
  constant slot workspace-projects :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  slot workspace-build-target :: false-or(<dfmc-build-target>) = #f;
  slot %registries :: false-or(<simple-object-vector>) = #f;
  slot workspace-debug-target :: false-or(<debug-target>) = #f;
end class <dfmc-project-workspace>;

define method make-default-workspace
    () => (workspace :: <dfmc-project-workspace>)
  let (processor, os) = default-platform-info();
  make(<dfmc-project-workspace>,
       processor:        processor,
       operating-system: os)
end method make-default-workspace;

default-workspace() := make-default-workspace();

define method workspace-registries
    (workspace :: <dfmc-project-workspace>)
 => (registries :: <simple-object-vector>)
  workspace.%registries
    | begin
	let processor  = workspace.workspace-processor;
	let os         = workspace.workspace-operating-system;
	workspace.%registries := platform-registries(processor, os)
      end
end method workspace-registries;

define method compilation-context-workspace
    (context :: <context>) => (workspace :: <dfmc-project-workspace>)
  let project = context.compilation-context-project;
  project.project-workspace
end method compilation-context-workspace;

define method note-workspace-build-operation-started
    (workspace :: <dfmc-project-workspace>, target :: <dfmc-build-target>)
 => ()
  workspace.workspace-build-target := target
end method note-workspace-build-operation-started;

define method note-workspace-build-operation-finished
    (workspace :: <dfmc-project-workspace>) => ()
  let target = workspace.workspace-build-target;
  workspace.workspace-build-target := #f;
  clear-build-canonicalized-projects(target.target-build-state)
end method note-workspace-build-operation-finished;


/// DFMC project

define sealed abstract class <dfmc-project> (<project>)
  constant slot project-execution-contexts :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
end class <dfmc-project>;

define sealed method open-project
    (workspace :: <dfmc-project-workspace>, locator :: <file-locator>,
     #key read-only? :: <boolean> = #f)
 => (project :: <dfmc-project>)
  find-open-project(workspace, locator, read-only?: read-only?)
    | begin
	let extension = locator.locator-extension;
	let project :: <dfmc-project>
	  = select (extension by \=)
	      lid-file-extension() =>
		open-lid-project(workspace, locator);
	      project-file-extension() =>
		open-hdp-project(workspace, locator);
	      database-file-extension() =>
		open-binary-project(workspace, locator);
	      otherwise =>
		error("Invalid project file %s", locator);
	    end;
	add!(workspace.workspace-projects, project);
	project
      end
end method open-project;

define sealed method find-open-project
    (workspace :: <dfmc-project-workspace>, locator :: <file-locator>,
     #key read-only? :: <boolean> = #f)
 => (project :: false-or(<dfmc-project>))
  let projects = workspace.workspace-projects;
  block (return)
    for (project :: <dfmc-project> in projects)
      if (project.project-file-location = locator
	    & project.project-read-only? == read-only?)
	return(project)
      end
    end;
    #f
  end
end method find-open-project;

define sealed method find-project
    (workspace :: <dfmc-project-workspace>, name :: <library-name>)
 => (project :: false-or(<dfmc-project>))
  let projects = workspace.workspace-projects;
  block (return)
    for (project :: <dfmc-project> in projects)
      if (project.project-name == name)
	return(project)
      end
    end;
    find-project-in-registry(workspace, name)
      | find-binary-project(workspace, name)
  end
end method find-project;


/// DFMC build target

define class <dfmc-build-target> (<build-target>)
  slot target-compilation-context :: false-or(<context>) = #f,
    init-keyword: compilation-context:;
end class <dfmc-build-target>;

define method make-workspace-build-target
    (workspace :: <dfmc-project-workspace>, #rest args, #key, #all-keys)
 => (target :: <dfmc-build-target>)
  apply(make, <dfmc-build-target>, args)
end method make-workspace-build-target;

define method target-canonical-source-records
    (target :: <dfmc-build-target>) => (records :: <sequence>)
  let context = target.target-compilation-context;
  case
    context   => context.compilation-context-sources;
    otherwise => #[];
  end
end method target-canonical-source-records;

define method compilation-context-target
    (context :: <context>) => (target :: <dfmc-build-target>)
  let workspace = context.compilation-context-workspace;
  workspace.workspace-build-target
end method compilation-context-target;

define method target-browsing-context
    (target :: <dfmc-build-target>) => (context :: <context>)
  target.target-compilation-context
end method target-browsing-context;


/// Platform information

define sideways method default-platform-info 
    () => (processor :: <processor>, os :: <operating-system>)
  let platform 
    = environment-variable($platform-variable)
        | $default-platform;
  let name = as-lowercase(as(<string>, platform));
  let separator-position = position(name, '-');
  let processor-name = copy-sequence(name, end: separator-position);
  let os-name = copy-sequence(name, start: separator-position + 1);
  values(as(<symbol>, processor-name),
	 as(<symbol>, os-name))
end method default-platform-info;
