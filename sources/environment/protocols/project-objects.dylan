Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong, Jason Trenouth, Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project objects

define constant <server-path-type>
  = one-of(#"compiler", #"application", #"both");

define constant $first-numeric-id = 10000;

define open abstract primary class <project-object>
    (<server>, <environment-object>)
  slot project-proxy = #f,
    init-keyword: proxy:;
  sealed slot project-application :: false-or(<application>) = #f,
    init-keyword: application:;
  sealed slot project-compiler-database :: false-or(<compiler-database>) = #f,
    init-keyword: compiler-database:;
  sealed slot project-next-numeric-id :: <integer> = $first-numeric-id;
  constant sealed slot project-object-table :: <table> = make-object-cache();
  constant sealed slot project-query-database :: <table> = make-object-cache();
  sealed slot project-server-path :: <server-path-type> = #"compiler",
    init-keyword: server-path:;
  constant sealed slot environment-object-breakpoints :: <collection> = make(<table>);
  constant sealed slot source-location-breakpoints :: <collection> = make(<source-location-table>);
  sealed slot project-properties :: <list> = #();
  constant sealed slot project-profile-state :: <profile-state> = make(<profile-state>);
end class <project-object>;

define open generic server-project
    (server :: <server>) => (project :: <project-object>);

define open generic project-proxy
    (project :: <project-object>) => (proxy);

define open generic project-application
    (project :: <project-object>)
 => (application :: false-or(<application>));

define open generic project-compiler-database
    (project :: <project-object>)
 => (compiler-database :: false-or(<compiler-database>));

define open generic project-database-changed?(project :: <project-object>)
 => (yes? :: <boolean>);

define open generic project-sources-changed?(project :: <project-object>)
 => (yes? :: <boolean>);


// open-project should attempt to open an existing project

define open generic open-project
    (locator :: <file-locator>) => (project :: false-or(<project-object>));

// find-project should either find the project in the work space, or open
// a project if not found.

define open generic find-project
    (name :: <string>) => (project :: false-or(<project-object>));

// new-project should create new user project, let's change the name to reflect this

define open generic create-new-user-project
    (name :: <object>, location :: <object>) => (project :: false-or(<project-object>));

// accordingly, new-project-from-file doesn't make sense,
// let's change the name
define open generic open-project-from-file
    (locator :: <file-locator>)
 => (project :: false-or(<project-object>));

define open generic create-exe-project-from-file
    (locator :: <file-locator>) => (project :: false-or(<project-object>));

define open generic import-project-from-file
    (locator :: <file-locator>, #key filename)
 => (project :: false-or(<project-object>));

define open generic close-project
    (project :: <project-object>) => ();

define open generic open-projects
    () => (projects :: <sequence>);

define open generic project-opened-by-user?
    (project :: <project-object>) => (by-user? :: <boolean>);

define open generic project-opened-by-user?-setter
    (by-user? :: <boolean>, project :: <project-object>)
 => (by-user? :: <boolean>);

define open generic project-read-only?
    (project :: <project-object>) => (read-only? :: <boolean>);

define open generic project-can-be-built?
    (project :: <project-object>) => (can-be-built? :: <boolean>);

define open generic project-can-be-debugged?
    (project :: <project-object>) => (can-be-debugged? :: <boolean>);

define open generic project-compiled?
    (project :: <project-object>) => (compiled? :: <boolean>);

define open generic project-sources
    (project :: <project-object>, #key) => (sources :: <sequence>);

define open generic project-start-function-name
    (project :: <project-object>) => (name :: false-or(<string>));

define open generic project-start-function-name-setter
    (name :: false-or(<string>), project :: <project-object>)
 => (name :: false-or(<string>));

define open generic project-canonical-sources
    (project :: <project-object>) => (sources :: <sequence>);

define open generic project-other-sources
    (project :: <project-object>, #key) => (sources :: <sequence>);

define open generic project-directory
    (project :: <project-object>) => (directory :: <directory-locator>);

define open generic project-filename
    (project :: <project-object>)
 => (filename :: false-or(<file-locator>));

define open generic project-build-filename
    (project :: <project-object>)
 => (filename :: false-or(<file-locator>));

define open generic project-build-filename-setter
    (filename :: <file-locator>, project :: <project-object>)
 => (filename :: <file-locator>);

define open generic project-debug-filename
    (project :: <project-object>) => (filename :: false-or(<file-locator>));

define open generic project-debug-filename-setter
    (filename :: false-or(<file-locator>), project :: <project-object>)
 => (filename :: false-or(<file-locator>));

define open generic project-debug-arguments
    (project :: <project-object>) => (arguments :: <string>);

define open generic project-debug-arguments-setter
    (arguments :: <string>, project :: <project-object>)
 => (arguments :: <string>);

define open generic project-debug-machine-address
    (project :: <project-object>) => (name :: false-or(<string>));

define open generic project-debug-machine-address-setter
    (name :: false-or(<string>), project :: <project-object>)
 => (name :: false-or(<string>));

define open generic project-debug-machine
    (project :: <project-object>) => (machine :: false-or(<machine>));

define open generic project-debug-machine-setter
    (machine :: false-or(<machine>), project :: <project-object>)
 => (machine :: false-or(<machine>));

define open generic project-debug-directory
    (project :: <project-object>)
 => (directory :: false-or(<directory-locator>));

define open generic project-debug-directory-setter
    (directory :: false-or(<directory-locator>), project :: <project-object>)
 => (directory :: false-or(<directory-locator>));

define open generic project-build-directory
    (project :: <project-object>)
 => (directory :: false-or(<directory-locator>));

define open generic project-bin-directory
    (project :: <project-object>)
=> (directory :: <directory-locator>);

define open generic project-release-directory
    (project :: <project-object>)
 => (directory :: <directory-locator>);

define open generic project-add-source-record
    (project :: <project-object>, record :: <object>) => ();

define open generic project-remove-source-record
    (project :: <project-object>, record :: <object>) => ();

define open generic project-reorder-source-records
    (project :: <project-object>, compare-function :: <function>) => ();

define open generic save-project
    (project :: <project-object>,
     #key save-database? :: <boolean> = #f,
          filename :: false-or(<file-locator>) = #f) => ();

define open generic save-project-database
    (project :: <project-object>) => ();


define open generic do-project-used-libraries
    (function :: <function>, server :: <server>, project :: <project-object>) => ();

define open generic do-project-file-libraries
    (function :: <function>, server :: <server>, file :: <file-locator>) => ();

define open generic do-used-projects
    (function :: <function>, project :: <project-object>,
     #key indirect?, read-only?)
 => ();

define open generic project-bind-variable
    (server :: <server>, variable-name :: <string>,
     object :: <application-object>,
     #key module)
 => (success? :: <boolean>);


/// Project property protocols

define constant <compilation-mode> = one-of(#"loose", #"tight");
define constant <project-target-type> = one-of(#"executable", #"dll");
define constant <project-interface-type> = one-of(#"console", #"gui");

define open generic session-property
    (key :: <symbol>) => (value);

define open generic session-property-setter
    (value, key :: <symbol>) => (value);

define open generic project-compilation-mode
    (project :: <project-object>) => (mode :: <compilation-mode>);

define open generic project-compilation-mode-setter
    (mode :: <compilation-mode>, project :: <project-object>)
 => (mode :: <compilation-mode>);

define open generic project-compiler-back-end
          (project :: <project-object>) => (back-end :: <symbol>);

define open generic project-compiler-back-end-setter
    (back-end :: <symbol>, project :: <project-object>)
 => (back-end :: <symbol>);

define open generic project-target-type
    (project :: <project-object>) => (target-type :: <project-target-type>);

define open generic project-target-type-setter
    (target-type :: <project-target-type>, project :: <project-object>)
 => (target-type :: <project-target-type>);

define open generic project-interface-type
    (project :: <project-object>) => (interface-type :: <project-interface-type>);

define open generic project-interface-type-setter
    (interface-type :: <project-interface-type>, project :: <project-object>)
 => (interface-type :: <project-interface-type>);

define open generic project-base-address
    (project :: <project-object>) => (address :: false-or(<machine-word>));

define open generic project-base-address-setter
    (address :: false-or(<machine-word>), project :: <project-object>)
 => (address :: false-or(<machine-word>));

define open generic project-major-version
    (project :: <project-object>) => (version :: <integer>);

define open generic project-major-version-setter
    (version :: <integer>, project :: <project-object>)
 => (version :: <integer>);

define open generic project-minor-version
    (project :: <project-object>) => (version :: <integer>);

define open generic project-minor-version-setter
    (version :: <integer>, project :: <project-object>)
 => (version :: <integer>);


/// File extensions

define function environment-locator-type
    (locator :: <file-locator>) => (type :: false-or(<symbol>))
  let extension = locator.locator-extension;
  if (extension)
    //---*** Shouldn't use \= on the extension...
    select (as-lowercase(extension) by \=)
      project-file-extension()      => #"hdp";
      lid-file-extension()          => #"lid";
      dylan-file-extension(), "dyl" => #"dylan";
      executable-file-extension()   => #"exe";
      "htm", "html"                 => #"html";
      otherwise                     => as(<symbol>, extension);
    end
  end
end function environment-locator-type;

define open generic project-file-extension
    () => (extension :: <string>);

define open generic lid-file-extension
    () => (extension :: <string>);

define open generic dylan-file-extension
    () => (extension :: <string>);

define open generic executable-file-extension
    () => (extension :: <string>);


/// Current project handling

define variable *current-project* :: false-or(<project-object>) = #f;

define function current-project
    () => (project :: false-or(<project-object>))
  *current-project*
end function current-project;

define function current-project-setter
    (project :: false-or(<project-object>))
 => (project :: false-or(<project-object>))
  *current-project* := project
end function current-project-setter;


/// project building

define open generic open-project-compiler-database
    (project :: <project-object>, #key warning-callback, error-handler)
 => (database :: false-or(<compiler-database>));

define open generic parse-project-source
    (project :: <project-object>,
     #key warning-callback, progress-callback, error-handler,
          process-subprojects?)
 => (well? :: <boolean>);

define open generic build-project
    (project :: <project-object>,
     #key clean?, link?, release?, output,
          warning-callback, progress-callback, error-handler,
          save-databases?, process-subprojects?, messages)
 => (built? :: <boolean>);

define open generic clean-project
    (project :: <project-object>,
     #key error-handler, process-subprojects?)
 => ();

define open generic link-project
    (project :: <project-object>,
     #key progress-callback,
          error-handler,
          process-subprojects?,
          build-script, target, arch, force?, unify?, release?, messages)
 => ();

define open generic default-build-script
    () => (build-script :: <file-locator>);
define open generic default-build-script-setter
    (build-script :: <file-locator>) => (build-script :: <file-locator>);


/// Source records

//---*** Maybe subsumed by do-project-file-libraries
define open generic find-project-source-record
    (project :: <project-object>, filename :: <file-locator>)
 => (record :: false-or(<source-record>));

define open generic find-source-record-library
    (project :: <project-object>, record :: <source-record>)
 => (library :: false-or(<library-object>));

define open generic source-record-projects
    (source-record :: <source-record>) => (projects :: <sequence>);

// The return type is unspecified because it may change if the info moves
// to compiler databases, or be different for non-file source records.
define open generic source-record-colorization-info
    (project :: <project-object>, source-record :: <source-record>)
 => (info /* :: false-or(<object>) */);


/// Source editing

// this gf has to take a server, because <source-location>
// is an abstract type and we may have to do some processing
define open generic edit-source-location
    (server :: <server>, source-location :: <source-location>) => ();

define open generic edit-source-record
    (server :: <server>, source-record :: <source-record>,
     #key start-line, start-column, end-line, end-column)
 => ();

define open generic edit-definition
    (server :: <server>, object :: <environment-object>)
 => (found-definition? :: <boolean>);


/// Top level forms

define open generic source-record-top-level-forms
    (server :: <server>, sr :: <source-record>, #key project)
 => (source-forms :: <sequence>);


/// Some project implementation

define function project-name
    (project :: <project-object>) => (name :: <string>)
  environment-object-primitive-name(project, project)
end function project-name;

define function project-full-build-filename
    (project :: <project-object>) => (pathname :: <file-locator>)
  let bin-directory = project.project-bin-directory;
  let filename = project.project-build-filename;
  merge-locators(filename, bin-directory)
end function project-full-build-filename;

define function project-used-projects
    (project :: <project-object>, #key indirect?, read-only?)
 => (projects :: <sequence>)
  let projects = make(<stretchy-vector>);
  do-used-projects
    (method (project :: <project-object>)
       add!(projects, project)
     end,
     project,
     indirect?: indirect?,
     read-only?: read-only?);
  projects
end function project-used-projects;

define method open-project
    (locator :: <file-locator>)
 => (project :: false-or(<project-object>))
  select (locator.environment-locator-type)
    #"exe" =>
      create-exe-project-from-file(locator);
    #"hdp", #"lid", #"ddb" =>
      open-project-from-file(locator);
    otherwise =>
      error("Internal error: open-project called on '%s' with non-project extension %=",
            locator,
            locator.locator-extension);
  end
end method open-project;

define method server-project
    (project :: <project-object>) => (project :: <project-object>)
  project
end method server-project;

define method environment-object-type-name
    (object :: <project-object>) => (label :: <string>)
  "Project"
end method environment-object-type-name;


/// Project query database handling

define method record-client-query
    (project :: <project-object>,
     client,
     object :: <environment-object>,
     type :: <query-type>)
 => ()
  ignore(type);
  let database = project-query-database(project);
  let clients
    = element(database, object, default: #f)
      | (element(database, object) := make(<stretchy-vector>));
  add-new!(clients, client)
end method record-client-query;

define method note-object-properties-changed
    (project :: <project-object>,
     object :: <environment-object>,
     type :: <query-type>)
 => ()
  let clients = element(project-query-database(project), object, default: #());
  for (client in clients)
    note-object-properties-changed(client, object, type)
  end
end method note-object-properties-changed;


/// Id caching
//
// Note that only unique ids are cached

define method lookup-environment-object-by-id
    (project :: <project-object>, id :: <id>)
 => (object :: false-or(<environment-object>))
  #f
end method lookup-environment-object-by-id;

define method cache-environment-object-with-id
    (project :: <project-object>, id :: <id>,
     object :: <environment-object-with-id>)
 => (object :: <environment-object-with-id>)
  object
end method cache-environment-object-with-id;

define method lookup-environment-object-by-id
    (project :: <project-object>, id :: <integer>)
 => (object :: false-or(<environment-object>))
  let table = project-object-table(project);
  element(table, id, default: #f)
end method lookup-environment-object-by-id;

define method cache-environment-object-with-id
    (project :: <project-object>, id :: <integer>,
     object :: <environment-object-with-id>)
 => (object :: <environment-object-with-id>)
  let table = project-object-table(project);
  element(table, id) := object
end method cache-environment-object-with-id;

define method lookup-environment-object-by-id
    (project :: <project-object>, id :: <unique-id>)
 => (object :: false-or(<environment-object>))
  let table = project-object-table(project);
  element(table, id, default: #f)
end method lookup-environment-object-by-id;

define method cache-environment-object-with-id
    (project :: <project-object>, id :: <unique-id>,
     object :: <environment-object-with-id>)
 => (object :: <environment-object-with-id>)
  let table = project-object-table(project);
  element(table, id) := object
end method cache-environment-object-with-id;


/// Lookup environment object

define sealed generic lookup-environment-object
    (class :: subclass(<environment-object>),
     #key project :: <project-object>,
          id :: false-or(<id-or-integer>),
          application-object-proxy,
          compiler-object-proxy)
 => (object :: false-or(<environment-object>),
     id :: false-or(<id-or-integer>));

define method lookup-environment-object
    (class :: subclass(<application-object>),
     #key project :: <project-object>,
          id :: false-or(<id-or-integer>),
          application-object-proxy: proxy,
          compiler-object-proxy)
 => (object :: false-or(<application-object>),
     id :: false-or(<id-or-integer>))
  ignore(compiler-object-proxy);
  let application = project-application(project);
  let object-for-proxy
    = proxy & lookup-environment-object-by-proxy(application, proxy);
  case
    object-for-proxy =>
      let id = id | environment-object-id(project, object-for-proxy);
      values(object-for-proxy, id);
    subtype?(class, <environment-object-with-id>) =>
      let id = id | application-proxy-id(application, proxy);
      if (id)
        values(lookup-environment-object-by-id(project, id), id)
      else
        values(#f, generate-unique-id(project))
      end;
    otherwise =>
      values(#f, #f);
  end
end method lookup-environment-object;

define method lookup-environment-object
    (class :: subclass(<compiler-object>),
     #key project :: <project-object>,
          id :: false-or(<id-or-integer>),
          application-object-proxy,
          compiler-object-proxy: proxy)
 => (object :: false-or(<compiler-object>),
     id :: false-or(<id-or-integer>))
  ignore(application-object-proxy);
  let database = project-compiler-database(project);
  let object-for-proxy
    = proxy & lookup-environment-object-by-proxy(database, proxy);
  case
    object-for-proxy =>
      let id = id | environment-object-id(project, object-for-proxy);
      values(object-for-proxy, id);
    subtype?(class, <environment-object-with-id>) =>
      let id = id | compiler-database-proxy-id(database, proxy);
      if (id)
        values(lookup-environment-object-by-id(project, id), id)
      else
        values(#f, generate-unique-id(project))
      end;
    otherwise =>
      values(#f, #f);
  end
end method lookup-environment-object;

define method lookup-environment-object
    (class :: subclass(<application-and-compiler-object>),
     #key project :: <project-object>,
          id :: false-or(<id-or-integer>),
          application-object-proxy,
          compiler-object-proxy)
 => (object :: false-or(<application-and-compiler-object>),
     id :: false-or(<id-or-integer>))
  let application = project-application(project);
  let database = project-compiler-database(project);
  block (return)
    local method maybe-return (object) => ()
            object
              & return(object, id | environment-object-id(project, object))
          end method maybe-return;
    maybe-return
      (application
         & application-object-proxy
         & lookup-environment-object-by-proxy(application, application-object-proxy));
    maybe-return
      (database
         & compiler-object-proxy
         & lookup-environment-object-by-proxy(database, compiler-object-proxy));
    if (subtype?(class, <environment-object-with-id>))
      let id
        = case
            id =>
              id;
            application-object-proxy =>
              application-proxy-id(application, application-object-proxy);
            compiler-object-proxy =>
              compiler-database-proxy-id(database, compiler-object-proxy);
            otherwise =>
              error("lookup-environment-object called with no id or proxies");
          end;
      if (id)
        values(lookup-environment-object-by-id(project, id), id)
      else
        values(#f, generate-unique-id(project))
      end
    else
      values(#f, #f)
    end
  end
end method lookup-environment-object;


/// Numeric id handling

define sealed method generate-unique-id
    (project :: <project-object>) => (id :: <integer>)
  //---*** This needs to be thread safe...
  let number = project.project-next-numeric-id;
  project.project-next-numeric-id := number + 1;
  number
end method generate-unique-id;


/// Project object interning

define sealed generic make-environment-object
    (class :: subclass(<environment-object>),
     #key project :: <project-object>,
          library :: false-or(<library-object>),
          id :: false-or(<id-or-integer>),
          application-object-proxy,
          compiler-object-proxy)
 => (object :: <environment-object>);

define method make-environment-object
    (class :: subclass(<application-object>),
     #rest args,
     #key project :: <project-object>,
          library :: false-or(<library-object>),
          id :: false-or(<id-or-integer>),
          application-object-proxy: proxy,
          compiler-object-proxy)
 => (object :: <application-object>)
  ignore(compiler-object-proxy);
  let application = project-application(project);
  debug-assert(application,
               "Project %= has no application", project);
  debug-assert(id | proxy,
               "make-environment-object called with no id or proxy");
  let (object, id)
    = lookup-environment-object
        (class,
         project: project, id: id, application-object-proxy: proxy);
  let object
    = if (instance?(object, class))
        let old-proxy = application-object-proxy(object);
        debug-assert(~old-proxy | old-proxy == proxy,
                     "Environment object %= found for two proxies: %=, %=",
                     object, old-proxy, proxy);
        application-object-proxy(object) := proxy;
        object
      else
        make(class, application-object-proxy: proxy,
             id: id, library: library)
      end;
  id & cache-environment-object-with-id(project, id, object);
  cache-environment-object(application, proxy, object)
end method make-environment-object;

define method make-environment-object
    (class :: subclass(<compiler-object>),
     #rest args,
     #key project :: <project-object>,
          library :: false-or(<library-object>),
          id :: false-or(<id-or-integer>),
          application-object-proxy,
          compiler-object-proxy: proxy)
 => (object :: <compiler-object>)
  ignore(application-object-proxy);
  let database = project-compiler-database(project);
  debug-assert(database,
               "Project %= has no compiler database %=", project);
  debug-assert(id | proxy,
               "make-environment-object called with no id or proxy");
  let (object, id)
    = lookup-environment-object
        (class,
         project: project, id: id, compiler-object-proxy: proxy);
  let object
    = if (instance?(object, class))
        let old-proxy = compiler-object-proxy(object);
        /*---*** Need to work out why this fails sometimes...
        debug-assert(~old-proxy | old-proxy == proxy,
                     "Environment object %= found for two proxies: %=, %=",
                     object, old-proxy, proxy);
        */
        unless (~old-proxy | old-proxy == proxy)
          debug-out(#"environment-protocols",
                    "Environment object %= found for two proxies: %=, %=",
                    object, old-proxy, proxy)
        end;
        compiler-object-proxy(object) := proxy;
        object
      else
        make(class, compiler-object-proxy: proxy, id: id, library: library)
      end;
  id & cache-environment-object-with-id(project, id, object);
  cache-environment-object(database, proxy, object)
end method make-environment-object;

define method make-environment-object
    (class :: subclass(<application-and-compiler-object>),
     #rest args,
     #key project :: <project-object>,
          library :: false-or(<library-object>),
          id :: false-or(<id-or-integer>),
          application-object-proxy,
          compiler-object-proxy)
 => (object :: <application-and-compiler-object>)
  let application = project-application(project);
  let database = project-compiler-database(project);
  let (object, id)
    = lookup-environment-object
        (class,
         project: project, id: id,
         application-object-proxy: application-object-proxy,
         compiler-object-proxy: compiler-object-proxy);
  let object
    = if (instance?(object, class))
        object
      else
        make(class,
             application-object-proxy: application-object-proxy,
             compiler-object-proxy: compiler-object-proxy,
             id: id, library: library)
      end;
  id & cache-environment-object-with-id(project, id, object);
  if (application & application-object-proxy)
    cache-environment-object(application, application-object-proxy, object)
  end;
  if (database & compiler-object-proxy)
    cache-environment-object(database, compiler-object-proxy, object)
  end;
  if (application-object-proxy)
    application-object-proxy(object) := application-object-proxy
  end;
  if (compiler-object-proxy)
    compiler-object-proxy(object) := compiler-object-proxy
  end;
  object
end method make-environment-object;


/// Server handling

define sealed generic choose-server
    (project :: <project-object>, object :: <environment-object>,
     #key error?, default-server)
 => (server :: false-or(<server>));

define method ensure-application-server
    (project :: <project-object>, object :: <application-object>,
     #key error?)
 => (application :: false-or(<application>))
  let application = project-application(project);
  case
    ~application =>
      error? & closed-server-error(object);
    ensure-application-proxy(application, object) =>
      application;
    invalid-object?(project, object) =>
      invalid-object-error(project, object);
    otherwise =>
      #f;
  end
end method ensure-application-server;

define method ensure-database-server
    (project :: <project-object>, object :: <compiler-object>,
     #key error?)
 => (database :: false-or(<compiler-database>))
  let database = project-compiler-database(project);
  case
    ~database =>
      error? & closed-server-error(object);
    ensure-database-proxy(database, object) =>
      database;
    invalid-object?(project, object) =>
      invalid-object-error(project, object);
    otherwise =>
      #f;
  end
end method ensure-database-server;

define method choose-server
    (project :: <project-object>, object :: <application-object>,
     #key error?, default-server)
 => (server :: false-or(<application>))
  ignore(default-server);
  ensure-application-server(project, object, error?: error?)
end method choose-server;

define method choose-server
    (project :: <project-object>, object :: <compiler-object>,
     #key error?, default-server)
 => (server :: false-or(<compiler-database>))
  ignore(default-server);
  ensure-database-server(project, object, error?: error?)
end method choose-server;

define method choose-server
    (project :: <project-object>, object :: <application-and-compiler-object>,
     #key error?, default-server)
 => (server :: false-or(<server>))
  let database = project-compiler-database(project);
  let application = project-application(project);
  local method maybe-application
            () => (application :: false-or(<application>))
          ensure-application-proxy(application, object) & application
        end method maybe-application;
  local method maybe-compiler-database
            () => (database :: false-or(<compiler-database>))
          ensure-database-proxy(database, object) & database
        end method maybe-compiler-database;
  let server
    = case
        database & application =>
          let server-path = default-server | project-server-path(project);
          select (server-path)
            #"compiler"    => maybe-compiler-database() | maybe-application();
            #"application" => maybe-application() | maybe-compiler-database();
            // #"both"     => ???;
            otherwise      => unknown-server-path-error(server-path);
          end;
        database    => maybe-compiler-database();
        application => maybe-application();
        otherwise   => #f;
      end;
  case
    ~database & ~application =>
      error? & closed-server-error(object);
    server =>
      server;
    invalid-object?(project, object) =>
      invalid-object-error(project, object);
    otherwise =>
      #f;
  end
end method choose-server;

define method environment-object-home-server?
    (project :: <project-object>, object :: <application-object>)
 => (home? :: <boolean>)
  let application = project-application(project);
  application & environment-object-home-server?(application, object)
end method environment-object-home-server?;

define method environment-object-home-server?
    (project :: <project-object>, object :: <compiler-object>)
 => (home? :: <boolean>)
  let database = project-compiler-database(project);
  database & environment-object-home-server?(database, object)
end method environment-object-home-server?;

define method environment-object-home-server?
    (project :: <project-object>, object :: <application-and-compiler-object>)
 => (home? :: <boolean>)
  let database = project-compiler-database(project);
  let application = project-application(project);
  (database & environment-object-home-server?(database, object))
    | (application & environment-object-home-server?(application, object))
end method environment-object-home-server?;


/// Environment object existence

define method environment-object-exists?
    (project :: <project-object>, object :: <environment-object>)
 => (exists? :: <boolean>)
  #t
end method environment-object-exists?;

define method environment-object-exists?
    (project :: <project-object>, object :: <application-object>)
 => (exists? :: <boolean>)
  application-object-exists?(project, object)
end method environment-object-exists?;

define method environment-object-exists?
    (project :: <project-object>, object :: <compiler-object>)
 => (exists? :: <boolean>)
  compiler-object-exists?(project, object)
end method environment-object-exists?;

define method environment-object-exists?
    (project :: <project-object>, object :: <application-and-compiler-object>)
 => (exists? :: <boolean>)
  compiler-object-exists?(project, object)
    | application-object-exists?(project, object)
end method environment-object-exists?;

define method application-object-exists?
    (project :: <project-object>, object :: <application-object>)
 => (exists? :: <boolean>)
  let application = project-application(project);
  application & ensure-application-proxy(application, object) & #t
end method application-object-exists?;

define method compiler-object-exists?
    (project :: <project-object>, object :: <compiler-object>)
 => (exists? :: <boolean>)
  let database = project-compiler-database(project);
  database & ensure-database-proxy(database, object) & #t
end method compiler-object-exists?;

define method invalid-object?
    (project :: <project-object>, object :: <application-object>)
 => (invalid? :: <boolean>)
  ~application-object-proxy(object)
end method invalid-object?;

define method invalid-object?
    (project :: <project-object>, object :: <compiler-object>)
 => (invalid? :: <boolean>)
  ~compiler-object-proxy(object)
end method invalid-object?;

define method invalid-object?
    (project :: <project-object>, object :: <application-and-compiler-object>)
 => (invalid? :: <boolean>)
  ~application-object-proxy(object)
    & ~compiler-object-proxy(object)
end method invalid-object?;


/// Closed server errors

define function unknown-server-path-error
    (server-path :: <symbol>)
  error("Server path %= not known!", server-path)
end function unknown-server-path-error;

define method closed-server-error
    (object :: <application-object>)
  error(make(<closed-server-error>,
             format-string: "Attempting to query %= from closed application",
             format-arguments: vector(object)))
end method closed-server-error;

define method closed-server-error
    (object :: <compiler-object>)
  error(make(<closed-server-error>,
             format-string: "Attempting to query %= from closed project",
             format-arguments: vector(object)))
end method closed-server-error;

define method closed-server-error
    (object :: <application-and-compiler-object>)
  error(make(<closed-server-error>,
             format-string: "Attempting to query %= from closed application or project",
             format-arguments: vector(object)))
end method closed-server-error;

define function invalid-object-error
    (project :: <project-object>, object :: <environment-object>)
  error(make(<invalid-object-error>,
             format-string: "Querying obsolete object %=",
             format-arguments: vector(object),
             project: project,
             object: object))
end function invalid-object-error;


/// Environment objects

define method get-environment-object-primitive-name
    (project :: <project-object>, object :: <environment-object>)
 => (name :: false-or(<string>))
  let server = choose-server(project, object);
  server & get-environment-object-primitive-name(server, object)
end method get-environment-object-primitive-name;

define method environment-object-source
    (project :: <project-object>, object :: <compiler-object>)
 => (source :: false-or(<string>))
  let server = choose-server(project, object);
  server & environment-object-source(server, object)
end method environment-object-source;


/// Application objects

define method invalidate-application-proxy
    (project :: <project-object>, object :: <application-object>)
 => ()
  //--- Note that we don't bother removing this object from the
  //--- cache because it is a weak table so the GC will do it
  //--- for us more efficiently.
  application-object-proxy(object) := #f
end method invalidate-application-proxy;


/// Compiler objects

define method invalidate-compiler-proxy
    (project :: <project-object>, object :: <compiler-object>)
 => ()
  //--- Note that we don't bother removing this object from the
  //--- cache because it is a weak table so the GC will do it
  //--- for us more efficiently.
  compiler-object-proxy(object) := #f
end method invalidate-compiler-proxy;


/// Source record stuff

define method source-record-top-level-forms
    (project :: <project-object>, sr :: <source-record>,
     #key project: subproject)
 => (source-forms :: <sequence>)
  let database = project-compiler-database(project);
  if (database)
    source-record-top-level-forms(database, sr, project: subproject)
  else
    #[]
  end
end method source-record-top-level-forms;

define method source-record-projects
    (record :: <source-record>) => (projects :: <stretchy-vector>)
  let projects = make(<stretchy-vector>);
  let filename = source-record-location(record);
  for (project in open-projects())
    let records = project-sources(project);
    if (member?(filename, records,
                test: method (filename :: <locator>, record :: <source-record>) => (true?)
                        filename = source-record-location(record)
                      end))
      add!(projects, project)
    end
  end;
  projects;
end method source-record-projects;

define method source-record-projects
    (record :: <interactive-source-record>) => (projects :: <vector>)
  vector(source-record-project(record))
end method;

//---*** This version doesn't deal with links, we probably shouldn't have this at all.
//---*** The emulator has its own version of this...
define method find-project-source-record
    (project :: <project-object>, filename :: <file-locator>)
 => (record :: false-or(<source-record>))
  block (return)
    for (record in project-sources(project))
      when (record.source-record-location = filename)
        return(record)
      end
    end
  end
end method find-project-source-record;

define method project-canonical-source-record
    (project :: <project-object>, record :: <source-record>)
 => (canonical-record :: false-or(<source-record>))
  let canonical-sources = project-canonical-sources(project);
  block (return)
    for (source in canonical-sources)
      if (source == record)
        return(source)
      end
    end;
    for (source in canonical-sources)
      if (source-record-name(source) = source-record-name(record))
        return(source)
      end
    end
  end
end method project-canonical-source-record;

define method project-canonical-filename
    (project :: <project-object>, file :: <file-locator>)
 => (canonical-filename :: false-or(<file-locator>))
  if (~project-read-only?(project))
    make(<native-file-locator>,
         directory: project.project-build-directory,
         base:      file.locator-base,
         extension: file.locator-extension)
  end
end method project-canonical-filename;


/// Library handling

define method do-project-used-libraries
    (function :: <function>, server :: <project-object>,
     project :: <project-object>)
 => ()
  assert(server = project,
         "Querying used libraries for %= using different server %=",
         project, server);
  let database = project-compiler-database(project);
  database & do-project-used-libraries(function, database, project)
end method do-project-used-libraries;

define function project-used-libraries
    (server :: <server>, project :: <project-object>)
 => (libraries :: <sequence>)
  collect-environment-objects(do-project-used-libraries, server, project)
end function project-used-libraries;

define method do-project-file-libraries
    (function :: <function>, project :: <project-object>,
     file :: <file-locator>)
 => ()
  let database = project-compiler-database(project);
  database & do-project-file-libraries(function, database, file)
end method do-project-file-libraries;


/// Active project

define variable *active-project* :: false-or(<project-object>) = #f;

define function active-project
    () => (project :: false-or(<project-object>))
  *active-project*
end function active-project;

define function active-project-setter
    (project :: false-or(<project-object>))
 => (project :: false-or(<project-object>))
  *active-project* := project;
  let message
    = if (project)
        make(<project-now-active-message>, project: project);
      else
        make(<no-active-project-message>)
      end;
  broadcast($project-channel, message);
  project
end function active-project-setter;

define method note-user-project-opened
    (project-object :: <project-object>) => ()
  let message = make(<project-opened-message>, project: project-object);
  broadcast($project-channel, message);
end method note-user-project-opened;


/// Finding environment objects by name

define method find-environment-object
    (project :: <project-object>, id :: <id-or-integer>,
     #rest keys,
     #key, #all-keys)
 => (object :: false-or(<environment-object>))
  lookup-environment-object-by-id(project, id)
    | begin
        let server-path = project-server-path(project);
        let database = project-compiler-database(project);
        //---*** andrewa: don't query the application server for now
        let application = #f; // project-application(project);
        let server :: false-or(<server>)
          = select (server-path)
              #"compiler"    => database | application;
              #"application" => application | database;
              // #"both"     => ???;
              otherwise      => unknown-server-path-error(server-path);
            end;
        server & apply(find-environment-object, server, id, keys)
      end
end method find-environment-object;


/// Project start function

define function project-start-function
    (project :: <project-object>)
 => (function :: false-or(<function-object>))
  let library = project-library(project);
  let name = project-start-function-name(project);
  if (library & name)
    let module = library-default-module(project, library);
    if (module)
      let object = find-environment-object(project, name, module: module);
      if (instance?(object, <function-object>))	object end
    end
  end
end function project-start-function;


/// Debugging

define method find-machine
    (address :: <string>) => (machine :: false-or(<machine>))
  block (return)
    do-machine-connections
      (method (machine :: <machine>)
         if (address = machine-network-address(machine))
           return(machine)
         end
       end,
       include-local?: #f);
    #f
  end
end method find-machine;

define method project-debug-machine
    (project :: <project-object>) => (machine :: false-or(<machine>))
  let address = project-debug-machine-address(project);
  if (address)
    find-machine(address)
      | block ()
          make(<machine>, network-address: address)
        exception (<remote-connection-failed-error>)
          #f
        end
  end
end method project-debug-machine;

define method project-debug-machine-setter
    (machine :: false-or(<machine>), project :: <project-object>)
 => (machine :: false-or(<machine>))
  let name
    = if (machine & machine ~== environment-host-machine())
        machine-network-address(machine)
      end;
  project-debug-machine-address(project) := name;
  machine
end method project-debug-machine-setter;


/// Playground project
///
/// This shouldn't be necessary, but the project manager lets us down
///---*** andrewa: aim to remove this one day...

define constant $playground-project-name         = "dylan-playground";
define constant $gui-playground-project-name     = "gui-dylan-playground";

define function playground-project-name
    () => (name :: <string>)
  if (release-contains-library-pack?(#"GUI"))
    $gui-playground-project-name
  else
    $playground-project-name
  end
end function playground-project-name;

define function find-playground-project
    () => (project :: false-or(<project-object>))
  find-project(playground-project-name())
end function find-playground-project;

// The "just-name?:" keyword should be passed as #t when you want to check
// the playground-project-ness but the project may not have been opened
// properly yet.
define function playground-project?
    (project :: <project-object>, #key just-name? :: <boolean> = #f)
 => (playground? :: <boolean>)
  let project-name = environment-object-primitive-name(project, project);
  (just-name? | project.project-read-only?)
    & project-name = playground-project-name()
end function playground-project?;

define function playground-application-filename
    (project :: <project-object>)
 => (filename :: false-or(<file-locator>))
  let lid-filename = project.project-filename;
  if (lid-filename)
    let filename
      = make(<file-locator>,
             directory: lid-filename.locator-directory,
             name:      project.project-build-filename.locator-name);
    debug-out(#"environment-protocols",
              "Playground filename: %s",
              as(<string>, filename));
    file-exists?(filename) & filename
  end
end function playground-application-filename;
