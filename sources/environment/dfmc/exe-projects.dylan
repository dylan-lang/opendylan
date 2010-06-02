Module:    dfmc-environment
Author:    Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed primary class <exe-project> (<native-project-object>)
  constant sealed slot %name :: <string>,
    required-init-keyword: name:;
  sealed slot project-debug-filename :: <file-locator>,
    required-init-keyword: debug-filename:;
  sealed slot project-debug-arguments :: <string> = "";
  sealed slot project-debug-machine-address :: false-or(<string>) = #f;
  sealed slot project-start-function-name :: false-or(<string>) = #f;
end class <exe-project>;

define sideways sealed method create-exe-project-from-file
    (locator :: <file-locator>)
 => (project :: false-or(<project-object>))
  let project-name = locator.locator-name;
  let project :: <exe-project>
    = make(<exe-project>, name: project-name, debug-filename: locator);
  note-user-project-opened(project);
  project
end method create-exe-project-from-file;

define sealed method get-environment-object-primitive-name
    (server :: <exe-project>, project :: <exe-project>)
 => (name :: false-or(<string>))
  project.%name
end method get-environment-object-primitive-name;

define sealed method open-project-compiler-database
    (project-object :: <exe-project>, #key warning-callback, error-handler)
 => (database :: false-or(<compiler-database>))
  #f
end method open-project-compiler-database;

define method project-opened-by-user?
    (project-object :: <exe-project>) => (by-user? :: <boolean>)
  #t
end method project-opened-by-user?;

define method project-opened-by-user?-setter
    (by-user? :: <boolean>, project-object :: <exe-project>)
 => (by-user? :: <boolean>)
  by-user?
end method project-opened-by-user?-setter;

define sealed method project-sources
    (project :: <exe-project>, #key) => (sources :: <sequence>)
  #[]
end method project-sources;

define sealed method project-canonical-sources
    (project :: <exe-project>) => (sources :: <sequence>)
  #[]
end method project-canonical-sources;

define sealed method env/project-other-sources
    (project :: <exe-project>, #key) => (sources :: <sequence>)
  #[]
end method env/project-other-sources;

define sealed method project-directory
    (project :: <exe-project>) => (directory :: <directory-locator>)
  project.project-debug-filename.locator-directory
end method project-directory;

define sealed method project-build-filename
    (project :: <exe-project>) => (filename :: <file-locator>)
  project.project-debug-filename
end method project-build-filename;

define sealed method project-build-directory
    (project :: <exe-project>) => (directory :: <directory-locator>)
  project.project-directory
end method project-build-directory;

define sealed method project-bin-directory
    (project :: <exe-project>) => (filename :: <directory-locator>)
  project.project-directory
end method project-bin-directory;

define sealed method update-application
    (project :: <exe-project>, #key progress-callback) => ()
  values();
end method update-application;

define sealed method parse-project-source
    (project-object :: <exe-project>, 
     #key warning-callback, progress-callback, error-handler, 
          process-subprojects?) 
 => (well? :: <boolean>)
  error("You cannot parse an executable-only project!")
end method parse-project-source;

define sealed method build-project 
    (project :: <exe-project>,
     #key clean?, link? = #t, release?, output = #[], 
          warning-callback, progress-callback, error-handler, 
          save-databases?, copy-sources?, process-subprojects?, messages)
 => (well? :: <boolean>)
  error("You cannot build an executable-only project!")
end method build-project;

define sealed method remove-project-build-products
    (project-object :: <exe-project>,
     #key error-handler, process-subprojects? = #t)
 => ()
  error("You cannot remove build products for executable-only projects!")
end method remove-project-build-products;

define sealed method link-project
    (project-object :: <exe-project>,
     #key progress-callback, error-handler, process-subprojects?,
          build-script, target, arch, force?, unify?, release?, messages)
 => ()
  error("You cannot link an executable-only project!")
end method link-project;

define sealed method do-used-projects
    (function :: <function>, project-object :: <exe-project>,
     #key indirect?, read-only?)
 => ()
  #f
end method do-used-projects;


/// PROJECT-FILENAME (environment-protocols)

define sealed method project-filename
    (project :: <exe-project>)
 => (filename == #f)
  #f
end method project-filename;

define sealed method project-object-file-location
    (project :: <exe-project>)
 => (filename == #f)
  #f
end method project-object-file-location;


/// CLOSE-PROJECT (environment-protocols)

define sealed method env/close-project
    (project :: <exe-project>) => ()
  #f
end method env/close-project;

/// PROJECT-DATABASE-CHANGED? (environment-protocols)

define sealed method project-database-changed?(project-object :: <exe-project>)
 => (yes :: <boolean>);
  #f
end method project-database-changed?;

/// PROJECT-SOURCES-CHANGED? (environment-protocols)

define sealed method project-sources-changed?(project-object :: <exe-project>)
 => (yes :: <boolean>);
  #f
end method project-sources-changed?;


/// Project properties

define sealed method env/project-read-only?
    (project-object :: <exe-project>) => (read-only? :: <boolean>)
  #t
end method env/project-read-only?;

define sealed method project-can-be-built?
    (project-object :: <exe-project>) => (can-be-built? :: <boolean>)
  #f
end method project-can-be-built?;

define sealed method project-can-be-debugged?
    (project-object :: <exe-project>) => (can-be-debugged? :: <boolean>)
  #t
end method project-can-be-debugged?;

define sealed method project-compiled?
    (project-object :: <exe-project>) => (compiled? :: <boolean>)
  #t
end method project-compiled?;

define sealed method project-interface-type
    (project-object :: <exe-project>)
 => (interface-type :: <project-interface-type>)
  //--- Is there any way for us to really tell?
  #"gui"
end method project-interface-type;

define sealed method env/project-target-type
    (project-object :: <exe-project>)
 => (target-type :: env/<project-target-type>)
  let filename = project-build-filename(project-object);
  select (filename.environment-locator-type)
    #"exe"    => #"executable";
    #"dll"    => #"dll";
    otherwise => error("Unexpected extension for exe project %=", filename);
  end
end method env/project-target-type;
