Module:    dummy-environment
Synopsis:  Dummy Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// The dummy project

define class <dummy-project> (<project-object>)
end class <dummy-project>;

define class <dummy-application> (<application>)
end class <dummy-application>;

define class <dummy-database> (<compiler-database>)
end class <dummy-database>;

define method make-dummy
    (class :: subclass(<project-object>), #rest args, #key, #all-keys)
 => (object :: <application-object>)
  apply(make, class, proxy: #"dummy", args)
end method make-dummy;

define method make-dummy
    (class :: subclass(<application-object>), #rest args, #key, #all-keys)
 => (object :: <application-object>)
  apply(make, class, application-object-proxy: #"dummy", args)
end method make-dummy;

define method make-dummy
    (class :: subclass(<compiler-object>), #rest args, #key, #all-keys)
 => (object :: <compiler-object>)
  apply(make, class, compiler-object-proxy: #"dummy", args)
end method make-dummy;

define method make-dummy
    (class :: subclass(<application-and-compiler-object>),
     #rest args, #key, #all-keys)
 => (object :: <compiler-object>)
  apply(make, class, 
	application-object-proxy: #"dummy",
	compiler-object-proxy: #"dummy",
	args)
end method make-dummy;


/// Define all of the objects in this fake system

define constant $othello-project
  = make-dummy(<dummy-project>,  name: "othello");
define constant $dylan-project
  = make-dummy(<dummy-project>,  name: "dylan");
define constant $othello-application
  = make(<dummy-application>, project: $othello-project, filename: "othello.exe");
define constant $othello-database
  = make(<dummy-database>, project: $othello-project, name: "othello.pdb");
define constant $othello-library
  = make-dummy(<library-object>, name: "othello");
define constant $othello-module
  = make-dummy(<module-object>);
define constant $othello-module-name
  = make-dummy(<module-name-object>, name: "othello");
define constant $othello-dylan-module-name
  = make-dummy(<module-name-object>, name: "dylan");

define constant $dylan-library
  = make-dummy(<library-object>, name: "dylan");
define constant $dylan-module
  = make-dummy(<module-object>);
define constant $dylan-module-name
  = make-dummy(<module-name-object>, name: "dylan");

define constant $object-class
  = make-dummy(<class-object>);
define constant $class-class
  = make-dummy(<class-object>);
define constant $function-class
  = make-dummy(<class-object>);
define constant $othello-game-class
  = make-dummy(<class-object>);
define constant $play-othello-function
  = make-dummy(<generic-function-object>);
define constant $play-othello-method
  = make-dummy(<method-object>);

define constant $object-name
  = make-dummy(<binding-name-object>);
define constant $class-name
  = make-dummy(<binding-name-object>);
define constant $othello-object-name
  = make-dummy(<binding-name-object>);
define constant $othello-class-name
  = make-dummy(<binding-name-object>);
define constant $othello-game-name
  = make-dummy(<binding-name-object>);
define constant $play-othello-name
  = make-dummy(<binding-name-object>);

define constant $othello-thread
  = make-dummy(<thread-object>, name: "othello");


/// Project modeling

define sideways method find-project
    (name :: <string>) => (project :: false-or(<dummy-project>))
  if (as-lowercase(name) = "othello")
    let project = $othello-project;
    project-compiler-database(project) := $othello-database;
    project
  end
end method find-project;

define method get-environment-object-primitive-name
    (project :: <dummy-project>, object :: <dummy-project>)
 => (name :: <string>)
  "Othello"
end method get-environment-object-primitive-name;

define method project-library
    (project :: <dummy-project>) => (library :: <library-object>)
  select (project)
    $othello-project => $othello-library;
    $dylan-project   => $dylan-library;
  end
end method project-library;

define method project-used-projects
    (project :: <dummy-project>) => (projects :: <sequence>)
  if (project == $othello-project)
    vector($dylan-project)
  end
end method project-used-projects;

define method project-sources
    (project :: <dummy-project>) => (sources :: <sequence>)
  vector("library.dylan",
         "module.dylan",
         "game.dylan",
         "algorithms.dylan",
         "board.dylan")
end method project-sources;

define method project-directory
    (project :: <dummy-project>) => (directory :: <string>)
  "c:/dylan/othello"
end method project-directory;

define method project-source-location
    (project :: <dummy-project>, source :: <string>)
 => (location :: <locator>)
  let directory = project-directory(project);
  as(<file-locator>, format-to-string("%s/%s.dylan", directory, source));
end method project-source-location;

define method compile-project 
    (project :: <dummy-project>,
     #key progress-callback, error-handler, save-databases?, process-subprojects?) => ()
  #f
end method compile-project;

define method recompile-project
    (project :: <dummy-project>,
     #key progress-callback, error-handler, save-databases?, process-subprojects?) => ()
  #f
end method recompile-project;

define method link-project
    (project :: <dummy-project>,
     #key progress-callback, error-handler, process-subprojects?) => ()
  #f
end method link-project;

//---*** A hack, but a useful one for now!
define method edit-project-source
    (project :: <dummy-project>, source :: <string>, #key line) => ()
  #f
end method edit-project-source;

define method environment-object-source
    (project :: <dummy-project>, object :: <environment-object>)
 => (source :: <string>)
  "define fake-macro foo ()"
  "  //--- Do something!"
  "  #f"
  "end fake-macro foo;"
end method environment-object-source;


/// Library modeling

define method library-project 
    (database :: <dummy-database>, library :: <library-object>)
 => (project :: false-or(<dummy-project>))
  if (library == $othello-library)
    $othello-project
  end
end method library-project;

define method do-namespace-names
    (function :: <function>, database :: <dummy-database>,
     library :: <library-object>,
     #key client, imported? = #t)
 => ()
  select (library)
    $othello-library => 
      function($othello-module-name);
      if (imported?)
	function($othello-dylan-module-name)
      end;
    $dylan-library =>
      function($dylan-module-name);
  end
end method do-namespace-names;


/// Module modeling

define method module-library
    (database :: <dummy-database>, module :: <module-object>)
 => (library :: <library-object>)
  ignore(database);
  select (module)
    $dylan-module   => $dylan-library;
    $othello-module => $othello-library;
  end
end method module-library;

define method find-module
    (database :: <dummy-database>, name :: <string>, #key library)
 => (module :: false-or(<module-object>))
  ignore(library, database);
  select (as-lowercase(name) by \=)
    "othello" => $othello-module;
    "dylan"   => $dylan-module;
  end
end method find-module;

define method do-namespace-names
    (function :: <function>, database :: <dummy-database>, 
     module :: <module-object>,
     #key client, imported? = #t)
 => ()
  select (module)
    $othello-module =>
      function($othello-game-name);
      function($play-othello-name);
      if (imported?)
	function($othello-class-name);
	function($othello-object-name);
      end;
    $dylan-module =>
      function($class-name);
      function($object-name);
  end
end method do-namespace-names;

define method do-used-definitions
    (function :: <function>, database :: <dummy-database>, 
     module :: <module-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  if (module = $othello-module)
    function($dylan-module)
  end
end method do-used-definitions;


/// Name objects

define method environment-object-name
    (database :: <dummy-database>, object :: <environment-object>,
     namespace :: <module-object>)
 => (name :: <name-object>)
  select (namespace)
    $othello-module =>
      select (object)
	$object-class          => $othello-object-name;
	$class-class           => $othello-class-name;
	$othello-game-class    => $othello-game-name;
	$play-othello-function => $play-othello-name;
	otherwise => #f;
      end;
    $dylan-module =>
      select (object)
        $object-class          => $object-name;
        $class-class           => $class-name;
	otherwise => #f;
      end;
  end
end method environment-object-name;

define method name-value
    (project :: <dummy-project>, name :: <binding-name-object>)
 => (value :: <application-object>)
  select (name)
    $othello-game-name                 => $othello-game-class;
    $play-othello-name                 => $play-othello-function;
    $object-name, $othello-object-name => $object-class;
    $class-name, $othello-class-name   => $class-class;
    $othello-module-name               => $othello-module;
  end
end method name-value;

define method name-value
    (project :: <dummy-project>, name :: <module-name-object>)
 => (module :: <module-object>)
  select (name)
    $othello-module-name       => $othello-module;
    $othello-dylan-module-name => $othello-module;
    $dylan-module-name         => $dylan-module;
  end
end method name-value;

define method name-namespace
    (database :: <dummy-database>, name :: <module-name-object>)
 => (library :: <library-object>)
  select (name)
    $othello-module-name       => $othello-library;
    $othello-dylan-module-name => $othello-library;
    $dylan-module-name         => $dylan-library;
  end
end method name-namespace;

define method name-namespace
    (database :: <dummy-database>, name :: <binding-name-object>)
 => (library :: <library-object>)
  select (name)
    $othello-game-name   => $othello-module;
    $play-othello-name   => $othello-module;
    $othello-object-name => $othello-module;
    $othello-class-name  => $othello-module;
    $object-name         => $dylan-module;
    $class-name          => $dylan-module;
  end
end method name-namespace;

define method environment-object-home-name
    (database :: <dummy-database>, object :: <environment-object>)
 => (name :: false-or(<name-object>))
  select (object)
    $othello-module        => $othello-module-name;
    $dylan-module          => $dylan-module-name;
    $othello-game-class    => $othello-game-name;
    $play-othello-function => $play-othello-name;
    $object-class          => $object-name;
    $class-class           => $class-name;
    otherwise              => #f;
  end
end method environment-object-home-name;

//---*** Do we really need this as well as environment-object-display-name?
define method get-environment-object-primitive-name
    (database :: <dummy-database>, name :: <name-object>)
 => (name :: false-or(<string>))
  select (name)
    $object-name          => "<object>";
    $class-name           => "<class>";
    $othello-object-name  => "dylan/<object>";
    $othello-class-name   => "dylan/<class>";
    $othello-game-name    => "<othello-game>";
    $play-othello-name    => "play-othello";
  end
end method get-environment-object-primitive-name;

define method name-type 
    (project :: <dummy-project>, name :: <name-object>)
 => (type :: <type-object>)
  select (name)
    $play-othello-name => $function-class;
    otherwise          => $class-class;
  end
end method name-type;


/// Class objects

define method application-object-class
    (project :: <dummy-project>, object :: <application-object>)
 => (class :: false-or(<class-object>))
  #f
end method application-object-class;

define method do-direct-subclasses
    (function :: <function>, 
     project :: <dummy-project>,
     class :: <class-object>,
     #key client)
 => ()
  if (class == $object-class)
    do(function,
       vector($class-class, $function-class, $othello-game-class))
  end
end method do-direct-subclasses;

define method do-direct-superclasses
    (function :: <function>, 
     project :: <dummy-project>,
     class :: <class-object>, 
     #key client)
 => ()
  if (class ~= $object-class)
    function($object-class)
  end
end method do-direct-superclasses;

define method do-direct-methods
    (function :: <function>, 
     project :: <dummy-project>, 
     class :: <class-object>, 
     #key client)
 => ()
  #f
end method do-direct-methods;

define method do-direct-slots
    (function :: <function>, 
     project :: <dummy-project>, 
     class :: <class-object>,
     #key client)
 => ()
  #f
end method do-direct-slots;

define method do-all-superclasses
    (function :: <function>,
     project :: <dummy-project>,
     class :: <class-object>,
     #key client)
 => ()
  #f
end method do-all-superclasses;

define method do-all-slots
    (function :: <function>,
     project :: <dummy-project>,
     class :: <class-object>, 
     #key client)
 => ()
  #f
end method do-all-slots;

define method do-init-keywords
    (function :: <function>, 
     project :: <dummy-project>, 
     class :: <class-object>,
     #key client)
 => ()
  #f
end method do-init-keywords;


/// Function objects

define method function-parameters
    (project :: <dummy-project>, function :: <function-object>)
 => (required :: <parameters>,
     rest :: false-or(<parameter>),
     keys :: <optional-parameters>,
     next :: false-or(<parameter>),
     values :: <parameters>,
     rest-value :: false-or(<parameter>))
  values(#(), #f, #(), #f, #(), #f)
end method function-parameters;

define method do-generic-function-methods
    (function :: <function>, project :: <dummy-project>,
     generic-function :: <generic-function-object>,
     #key client)
 => ()
  function($play-othello-method)
end method do-generic-function-methods;

define method method-specializers 
    (database :: <dummy-database>, object :: <method-object>)
 => (specializers :: <sequence>)
  #()
end method method-specializers;

define method method-generic-function
    (database :: <dummy-database>, object :: <method-object>) 
 => (function :: <generic-function-object>)
  $play-othello-function
end method method-generic-function;


/// Compiler warning objects

define method do-compiler-warnings
    (function :: <function>, 
     project :: <dummy-project>,
     object :: <environment-object>, 
     #key client)
 => ()
  #f
end method do-compiler-warnings;


/// Project execution

define constant $dummy-thread
  = make(<thread-object>, application-object-proxy: #f);

define method project-valid-code?
    (project :: <dummy-project>, code :: <string>, #key module)
 => (valid :: <boolean>, reason :: <string>)
  let object
    = project-execute-code(project, code, $dummy-thread, module: module);
  if (object)
    values(#t, "")
  else
    values(#f, "Dummy code execution can only evaluate variables!")
  end
end method project-valid-code?;

define method project-execute-code
    (project :: <dummy-project>, code :: <string>, 
     thread :: <thread-object>,
     #key module, stack-frame)
 => (results :: <sequence>, success? :: <boolean>)
  ignore(module, thread, stack-frame);
  let object
    = select (code by \=)
        "<object>;"       => $object-class;
        "<class>;"        => $class-class;
        "<function>;"     => $function-class;
        "<othello-game>;" => $othello-game-class;
        "play-othello;"   => $play-othello-function;
        otherwise         => #f;
      end;
  if (object)
    values(vector(object), #t)
  else
    values(#[], #f)
  end
end method project-execute-code;

define method project-bind-variable
    (project :: <dummy-project>,
     variable-name :: <string>,
     object :: <application-object>,
     #key module)
 => (success? :: <boolean>)
  //---*** Just do nothing!
  #t
end method project-bind-variable;


/// Some default methods to stop things crashing

//--- A default method doing nothing
define method do-used-definitions
    (function :: <function>, database :: <dummy-database>, 
     definition :: <definition-object>,
     #key modules, libraries, client)
 => ()
  ignore(modules, libraries, client);
  #f
end method do-used-definitions;

define method composite-object-contents
    (application :: <dummy-application>, object :: <composite-object>,
     #key inherited? = #t)
 => (names :: <sequence>, values :: <sequence>)
  values(vector("application"), vector(application))
end method composite-object-contents;


/// Applications

define method update-application
    (project :: <dummy-project>, #key progress-callback)
 => ()
  let sources = project-sources(project);
  let name = environment-object-primitive-name(project, project);
  let progress-range = 2 * size(sources);
  let i = 0;
  for (source in sources)
    let filename = format-to-string("file %s.dylan", source);
    progress-callback(i, progress-range,
		      label: format-to-string("Compiling %s", source));
    i := i + 1;
    progress-callback(i, progress-range,
		      label: format-to-string("Loading %s", source));
    i := i + 1;
  end;
  progress-callback(progress-range, progress-range,
                    label: format-to-string("Updated %s", name))
end method update-application;

define method application-threads
    (application :: <dummy-application>, #key client)
 => (threads :: <sequence>)
  vector($othello-thread)
end method application-threads;

define method run-application
    (project :: <dummy-project>, #key debug?, filename, arguments)
 => (application :: <application>)
  project-application(project) := $othello-application;
  application-state($othello-application)
    := if (debug?) #"stopped" else #"running" end;
  $othello-application
end method run-application;

define method continue-application
    (project :: <dummy-project>) => ()
  application-state($othello-application) := #"running";
end method continue-application;

define method stop-application
    (project :: <dummy-project>) => ()
  application-state($othello-application) := #"stopped";
end method stop-application;

define method close-application
    (project :: <dummy-project>) => ()
  project-application(project) := #f;
  application-state($othello-application) := #"closed";
end method close-application;

