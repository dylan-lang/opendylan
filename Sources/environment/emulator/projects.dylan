Module:    emulator-environment-backend
Synopsis:  Emulator Environment Backend
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Project handling

define constant $emulator-filename = "emulator-application";

//---*** We need some way to uncache projects!
define variable *projects* = make(<table>);

define class <emulator-project> (<project-object>)
end class <emulator-project>;

define method ensure-emulator-project
    (proxy) => (project :: <project-object>)
  element(*projects*, proxy, default: #f)
    | (element(*projects*, proxy) := make(<emulator-project>, proxy: proxy))
end method ensure-emulator-project;

define method ensure-server-object-of-class
    (project :: <emulator-project>, object,
     class :: subclass(<project-object>))
 => (object :: <class-object>)
  ensure-emulator-project(object)
end method ensure-server-object-of-class;


/// Project opening/closing

define method find-project
    (name :: <string>) => (project :: false-or(<project-object>))
  let dylan-project = find-dylan-project(name);
  if (dylan-project)
    let project = ensure-emulator-project(dylan-project);
    ensure-application(project);
    project
  end
end method find-project;

define method open-project-from-file
    (locator :: <locator>)
 => (project :: false-or(<project-object>))
  let base = locator.locator-base;
  base & find-project(base)
end method open-project-from-file;

define method import-project-from-file
    (locator :: <locator>)
 => (project :: false-or(<project-object>))
  open-project-from-file(locator)
end method import-project-from-file;

define method open-projects
    () => (projects :: <sequence>)
  as(<vector>, *projects*)
end method open-projects;

define method project-used-projects
    (project :: <emulator-project>) => (projects :: <sequence>)
  let dylan-libraries
    = dylan-library-used-libraries
        (compiler-object-proxy(project-library(project)));
  map(method (library)
        let name = dylan-library-name(library);
        find-project(name)
      end,
      dylan-libraries)
end method project-used-projects;

define method get-environment-object-primitive-name
    (project :: <emulator-project>, object :: <emulator-project>)
 => (name :: <string>)
  dylan-project-name(project-proxy(object))
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (project :: <emulator-project>, application :: <emulator-application>)
 => (name :: <string>)
  "dylan-emulator"
end method get-environment-object-primitive-name;

define method get-environment-object-primitive-name
    (project :: <emulator-project>, database :: <emulator-database>)
 => (name :: <string>)
  "emulator-database"
end method get-environment-object-primitive-name;

define method project-library
    (database :: <emulator-database>) => (library :: <library-object>)
  let project = server-project(database);
  let proxy = dylan-project-libraries(project-proxy(project))[0];
  make-environment-object(<library-object>,
                          project: project,
                          compiler-object-proxy: proxy)
end method project-library;

define method project-directory
    (project :: <emulator-project>) => (directory :: <string>)
  dylan-project-directory(project-proxy(project))
end method project-directory;

define method make-source-record
    (locator :: <locator>) => (source-record :: <source-record>)
  make(<flat-file-source-record>,
       location: locator,
       date: file-property(locator, #"modification-date"))
end method make-source-record;

define method make-source-record
    (filename :: <string>) => (source-record :: <source-record>)
  make-source-record(as(<file-locator>, file-truename(filename)))
end method make-source-record;

define method project-sources
    (project :: <emulator-project>) => (sources :: <sequence>)
  let (libraries, files) = dylan-project-contents(project-proxy(project));
  ignore(libraries);
  map(method (file)
       make-source-record(project-source-location(project, file))
      end,
      files)
end method project-sources;


/// Source records

define sealed method source-record-colorization-info
    (project :: <emulator-project>, source-record :: <source-record>)
 => (info :: singleton(#f))
  // No color info in the emulator, sorry.
  #f
end method source-record-colorization-info;



/// Project properties

define method project-read-only?
    (project :: <emulator-project>)
 => (read-only? :: <boolean>)
  #f
end method project-read-only?;

define method project-compiled?
    (project :: <emulator-project>)
 => (compiled? :: <boolean>)
  //---*** This doesn't matter as linking doesn't mean anything in the emulator
  #f
end method project-compiled?;

define method project-compilation-mode
    (project :: <emulator-project>)
 => (compilation-mode :: <compilation-mode>)
  #"loose"
end method project-compilation-mode;

define method project-compilation-mode-setter
    (compilation-mode :: <compilation-mode>, project :: <emulator-project>)
 => (compilation-mode :: <compilation-mode>)
  //---*** Should we try to do this better?
  compilation-mode
end method project-compilation-mode-setter;

define method project-target-type
    (project :: <emulator-project>)
 => (target-type :: <project-target-type>)
  #"executable"
end method project-target-type;

define method project-target-type-setter
    (target-type :: <project-target-type>, project :: <emulator-project>)
 => (target-type :: <project-target-type>)
  //---*** Should we try to do this better?
  target-type
end method project-target-type-setter;

define method project-major-version
    (project :: <emulator-project>)
 => (version :: <integer>)
  1
end method project-major-version;

define method project-major-version-setter
    (version :: <integer>, project :: <emulator-project>)
 => (version :: <integer>)
  //---*** Should we do better?
  version
end method project-major-version-setter;

define method project-minor-version
    (project :: <emulator-project>)
 => (version :: <integer>)
  1
end method project-minor-version;

define method project-minor-version-setter
    (version :: <integer>, project :: <emulator-project>)
 => (version :: <integer>)
  //---*** Should we do better?
  version
end method project-minor-version-setter;


/// File extensions

define sideways method project-file-extension
    () => (extension :: <string>)
  "hdp"
end method project-file-extension;

define sideways method lid-file-extension
    () => (extension :: <string>)
  "lid"
end method lid-file-extension;

define sideways method dylan-file-extension
    () => (extension :: <string>)
  "dylan"
end method dylan-file-extension;


/// Project building

define method compile-project 
    (project :: <emulator-project>, #key progress-callback, error-handler) => ()
  let records = project-sources(project);
  let range = size(records) * 2;
  for (record in records,
       count from 0 by 2)
    let needs-compiling? = ~project-file-compiled?(project, record);
    let needs-loading? = ~project-file-loaded?(project, record);
    let filename = locator-name(as(<file-locator>, source-record-location(record)));
    progress-callback
      & progress-callback
          (count, range,
           label: format-to-string
                    ("%s %s...",
                     if (needs-compiling?) "Compiling" else "Skipping" end,
                     filename));
    if (needs-compiling?)
      compile-project-file(project, record);
      needs-loading? := #t;
    end;
    if (needs-loading?)
      progress-callback
        & progress-callback
            (count + 1, range, label: format-to-string("Loading %s...", filename));
      load-project-file(project, record);
    end
  end;
  ensure-application(project)
end method compile-project;

//--- In the emulator we have to build it!
define method parse-project-source
    (project :: <emulator-project>, #key progress-callback, error-handler) => ()
  compile-project(project, 
		  progress-callback: progress-callback, 
		  error-handler: error-handler)
end method parse-project-source;

define method recompile-project 
    (project :: <emulator-project>, #key progress-callback, error-handler) => ()
  let files = project-sources(project);
  let range = size(files) * 2;
  for (file in files,
       count from 0 by 2)
    progress-callback
      & progress-callback
          (count, range, label: format-to-string("Compiling %s...", file));
    compile-project-file(project, file);
    progress-callback
      & progress-callback
        (count + 1, range, label: format-to-string("Loading %s...", file));
    load-project-file(project, file);
  end;
  ensure-application(project);
end method recompile-project;

define method link-project
    (project :: <emulator-project>, #key progress-callback, error-handler) => ()
  error("Wouldn't it be nice to be able to link in Lisp!")
end method link-project;

define method dylan-project-loaded? 
    (project :: <emulator-project>) => (loaded? :: <boolean>)
  let project-name = environment-object-primitive-name(project, project);
  let library = find-dylan-library(project-name);
  block (return)
    do-dylan-library-modules
      (method (module-name)
         ignore(module-name);
         return(#t)
       end,
       library);
    #f
  end
end method dylan-project-loaded?;

define method ensure-application
    (project :: <emulator-project>) => ()
  if (dylan-project-loaded?(project))
    project-application(project)
      := make(<emulator-application>, 
	      project: project, 
	      filename: $emulator-filename);
    project-compiler-database(project)
      := make(<emulator-database>, project: project)
  end
end method ensure-application;

define method update-application
    (project :: <emulator-project>, #key progress-callback) => ()
  //--- These turn out to be the same in the emulator
  compile-project(project, progress-callback: progress-callback);
  ensure-application(project);
end method update-application;

define method run-application
    (project :: <emulator-project>, #key debug?, filename, arguments)
 => (application :: false-or(<emulator-application>))
  update-application(project)
end method run-application;

define method compile-project-file
    (project :: <emulator-project>, record :: <source-record>)
 => ()
  let filename = as(<string>, source-record-location(record));
  compile-dylan-project-file(project-proxy(project), filename)
end method compile-project-file;

define method load-project-file
    (project :: <emulator-project>, record :: <source-record>)
 => ()
  let filename = as(<string>, source-record-location(record));
  load-dylan-project-file(project-proxy(project), filename)
end method load-project-file;

define method project-file-compiled?
    (project :: <emulator-project>, record :: <source-record>)
 => (compiled? :: <boolean>)
  let filename = as(<string>, source-record-location(record));
  dylan-project-file-compiled?(filename)
end method project-file-compiled?;

define method project-file-loaded?
    (project :: <emulator-project>, record :: <source-record>)
 => (loaded? :: <boolean>)
  let filename = as(<string>, source-record-location(record));
  dylan-project-file-loaded?(project-proxy(project), filename)
end method project-file-loaded?;

define method project-valid-code?
    (project :: <emulator-project>, code :: <string>, thread :: <thread-object>,
     #key module, runtime-context)
 => (valid :: <boolean>, warnings :: <sequence>)
  ignore(thread, runtime-context);
  let emulator-module
    = if (module) compiler-object-proxy(module) else #() end;
  let (expression, reason) = parsed-dylan-form(code, module: emulator-module);
  if (expression)
    values(#t, "")
  else
    values(#f, reason)
  end
end method project-valid-code?;

define method project-execute-code
    (project :: <emulator-project>, code :: <string>, 
     thread :: <thread-object>, #key module)
 => (results :: <sequence>, success? :: <boolean>)
  ignore(thread);
  let emulator-module
    = if (module) compiler-object-proxy(module) else #() end;
  let results = evaluate-dylan-form(code, module: emulator-module);
  if (results)
    values(map-as(<vector>,
                  curry(ensure-server-object, project),
                  results),
           #t)
  else
    values(#[], #f)
  end
end method project-execute-code;

define method project-bind-variable
    (project :: <emulator-project>,
     variable-name :: <string>, 
     object :: <application-object>,
     #key module)
 => (success? :: <boolean>)
  if (module)
    bind-dylan-variable(variable-name, 
                        application-object-proxy(object),
                        compiler-object-proxy(module));
    #t
  end
end method project-bind-variable;


/// Library handling

define method get-environment-object-primitive-name
    (database :: <emulator-database>, object :: <library-object>)
 => (name :: <string>)
  dylan-library-name(compiler-object-proxy(object))
end method get-environment-object-primitive-name;

define class <dylan-module-name> (<object>)
  slot dylan-module-name-module,
    required-init-keyword: module:;
end class <dylan-module-name>;

define constant $module-names = make(<table>);

define method make-dylan-module-name
    (dylan-module) => (name :: <dylan-module-name>)
  element($module-names, dylan-module, default: #f)
    | begin
        $module-names[dylan-module]
          := make(<dylan-module-name>, module: dylan-module)
      end
end method make-dylan-module-name;

define method do-namespace-names
    (function, database :: <emulator-database>, library :: <library-object>,
     #key client, imported? = #t)
 => ()
  let project = server-project(database);
  let proxy = compiler-object-proxy(library);
  do-dylan-library-modules
    (method (module)
       let name-proxy = make-dylan-module-name(module);
       let name
         = ensure-server-object-of-class(project, name-proxy, <module-name-object>);
       function(name)
     end,
     proxy,
     imported: if (imported?) #t else #() end)
end method do-namespace-names;

define method library-project
    (database :: <emulator-database>, library :: <library-object>)
 => (project :: <emulator-project>)
  let name = environment-object-primitive-name(database, library);
  find-project(name)
end method library-project;

define method do-used-definitions
    (function :: <function>, database :: <emulator-database>, 
     library :: <library-object>,
     #key modules, libraries, client)
 => ()
  let used-libraries
    = map(curry(ensure-server-object, database),
          dylan-library-used-libraries
            (compiler-object-proxy(library)));
  do(function, used-libraries)
end method do-used-definitions;

define method do-client-definitions
    (function :: <function>, database :: <emulator-database>, 
     library :: <library-object>,
     #key modules, libraries, client)
 => ()
  let client-libraries
    = map(curry(ensure-server-object, database),
          dylan-library-client-libraries
            (compiler-object-proxy(library)));
  do(function, client-libraries)
end method do-client-definitions;


/// Modules

define method get-environment-object-primitive-name
    (database :: <emulator-database>, module :: <module-object>)
 => (name :: <string>)
  dylan-module-name(compiler-object-proxy(module))
end method get-environment-object-primitive-name;

define method do-namespace-names
    (function, database :: <emulator-database>, module :: <module-object>, 
     #key client, imported? = #t)
 => ()
  do-dylan-module-variables
    (method (variable)
       let name
	 = ensure-server-object-of-class
	     (database, variable, <binding-name-object>);
       function(name)
     end,
     compiler-object-proxy(module),
     inherited?: imported?,
     internal?:  #t)
end method do-namespace-names;

define method environment-object-home-name
    (database :: <emulator-database>, module :: <module-object>)
 => (name :: false-or(<name-object>))
  let library = module-library(database, module);
  block (return)
    do-namespace-names
      (method (name)
	 if (name-value(database, name) = module)
           return(name)
         end
       end,
       database,
       library);
    #f
  end
end method environment-object-home-name;

define method environment-object-name
    (database :: <emulator-database>, module :: <module-object>,
     library :: <library-object>)
 => (name :: false-or(<name-object>))
  ignore(namespace);
  //---*** This needs to really be smarter
  environment-object-home-name(database, module)
end method environment-object-name;

define method module-library
    (database :: <emulator-database>, module :: <module-object>)
 => (library :: <library-object>)
  let project = server-project(database);
  let library = dylan-module-library(compiler-object-proxy(module));
  if (library ~= #())
    ensure-server-object(project, library)
  else
    error("No library for module %=!", module)
  end
end method module-library;

define method do-used-definitions
    (function :: <function>, database :: <emulator-database>, 
     module :: <module-object>,
     #key modules, libraries, client)
 => ()
  let used-modules
    = map(curry(ensure-server-object, database),
          dylan-module-used-modules
            (compiler-object-proxy(module)));
  do(function, used-modules)
end method do-used-definitions;

define method do-client-definitions
    (function :: <function>, database :: <emulator-database>, module :: <module-object>,
     #key modules, libraries, client)
 => ()
  let client-modules
    = map(curry(ensure-server-object, database),
          dylan-module-client-modules
            (compiler-object-proxy(module)));
  do(function, client-modules)
end method do-client-definitions;


/// Names

define method get-environment-object-primitive-name
    (database :: <emulator-database>, name :: <module-name-object>)
 => (name :: <string>)
  let proxy = compiler-object-proxy(name);
  dylan-module-name(dylan-module-name-module(proxy));
end method get-environment-object-primitive-name;

define method name-value
    (database :: <emulator-database>, name :: <module-name-object>)
 => (object :: <module-object>)
  let proxy = compiler-object-proxy(name);
  let value = dylan-module-name-module(proxy);
  value & ensure-server-object(database, value)
end method name-value;

define method name-namespace
    (database :: <emulator-database>, name :: <module-name-object>)
 => (namespace :: <namespace-object>)
  let proxy = compiler-object-proxy(name);
  let module = dylan-module-name-module(proxy);
  let namespace-proxy = dylan-module-library(module);
  (namespace-proxy & ensure-server-object(database, namespace-proxy))
    | error("No namespace found for name %=", name)
end method name-namespace;

define method get-environment-object-primitive-name
    (database :: <emulator-database>, name :: <binding-name-object>)
 => (name :: <string>)
  let proxy = compiler-object-proxy(name);
  dylan-symbol-name(proxy)
end method get-environment-object-primitive-name;

define method name-value
    (database :: <emulator-database>, name :: <binding-name-object>)
 => (object :: <environment-object>)
  let proxy = compiler-object-proxy(name);
  let value = dylan-variable-value(proxy, #f);
  value & ensure-server-object(database, value)
end method name-value;

define method name-namespace
    (database :: <emulator-database>, name :: <binding-name-object>)
 => (namespace :: <namespace-object>)
  let proxy = compiler-object-proxy(name);
  //---*** How do I get from an object to its module?
  let namespace-proxy = #f;
  (namespace-proxy & ensure-server-object(database, namespace-proxy))
    | error("No namespace found for name %=", name)
end method name-namespace;

define method name-imported?
    (database :: <emulator-database>, name :: <name-object>)
 => (imported? :: <boolean>)
  let proxy = compiler-object-proxy(name);
  select (proxy by instance?)
    <dylan-module-name> =>
      #f;
    otherwise =>
      //---*** Don't have access to the module!
      // dylan-variable-imported?(proxy, ?module?);
      #f
  end
end method name-imported?;

define method name-exported?
    (database :: <emulator-database>, name :: <name-object>)
 => (exported? :: <boolean>)
  //---*** We should implement this for real
  #f
end method name-exported?;


/// Hacks

define method find-project-source-record
    (project :: <emulator-project>, filename :: type-union(<locator>, <string>))
 => (record :: false-or(<source-record>))
  block (return)
    if (file-exists?(filename))
      let locator = as(<file-locator>, file-truename(as(<string>, filename)));
      for (record in project-sources(project))
	when (locator = source-record-location(record))
	  return(record)
	end
      end
    end
  end
end method find-project-source-record;
