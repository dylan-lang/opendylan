User-project library
--------------------

Developers: to continue using the old setup:

access(projects-implementation, *default-project-class*) := <registry-project>;

Otherwise the setup is as follows:

All registry projects are system projects and will not be recompiled
(usually ;-).

Projects are looked up 
1. first in the workspace, using for now WEBSTER_PLATFORM_NAME
2. then in the registry using for now the variable WEBSTER_SYSTEM_ROOT, if
found a <system-project> is created
3. otherwise <project-not-found-error> is signalled and
<find-project-location-restart> is established;

Example code in the env:

define function lookup-project(key :: <symbol>)
  let handler (<project-not-found>) = condition-handler;
  lookup-named-project(key);
end;

define function condition-handler(condition :: <project-not-found>,
				  next-handler)
  format-out("Condition: %=\n", condition);
  signal(make(<find-project-location-restart>, location:
		as(<file-locator>, "/u/roman/dylan/lib/test-project/test-project.dw")));
end;

The following api's are supported for <user-project>

define function new-user-project(name :: <string>, location :: <locator>) 
 => (project :: <user-project>);
// creating a new project involves creating three files: 'name'.prj,
//'name'.lid, and library.dylan with initial definitions of a library
// and a module. It's because for now I cannot create pur in-core
// projects
// currently the project layout is as follows: prj, lid and source
// files in one directory, all build products in subdirectory 'build'
// there can be more than one project in a directory

define method project-add-file(p :: <user-project>, file :: <string>)
// no suffix, also for now saves project

define method project-add-file(p :: <user-project>, file-locator :: <locator>)

define method project-remove-file(p :: <user-project>, file ::<string>) 
// no suffix, also for now saves project

define method project-sort-files(p :: <user-project>, test-function)
// test function will be given strings representing file names without
// suffices

define method save-project(p :: <user-project>)

define method import-lid-project(lid-location :: <locator>) => project

define method open-project(project-file-location :: <locator>) => project

Interactivity api for the environment (not fully tested):

Both <system-project> and <user-project> support interactivity.


define method project-current-debug-target-setter(p :: <project>, debug-target)	
// project is ignored

define method evaluate-expression(project :: <interactive-project>,
				  runtime-context,
				  module :: <symbol>,
				  source :: <byte-string>) 
 => source-record-id

define method project-id-interactive-record(p :: <interactive-project>,
					    id)
 => (sr :: false-or(<interactive-source-record>));

define method project-interactive-execution-notes(p :: <interactive-project>,
						  #key record-id)
// currently record-id is ignored and the last transaction is assumed
