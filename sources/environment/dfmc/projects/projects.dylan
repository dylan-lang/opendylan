Module:    dfmc-environment-projects
Author:    Roman Budzianowski, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Note that we don't need to lock this because the environment
// already deals with locking of the compiler.
define variable *compiling?* :: <boolean> = #f;

define sealed abstract primary class <dfmc-project-object>
    (<native-project-object>)
  sealed slot %library-name :: <string>, init-keyword: library-name:;
  sealed slot %user-project? :: <boolean> = #f, init-keyword: user-project?:;
  sealed slot %database-current? :: <boolean> = #f;
  sealed slot %database-saved? :: <boolean> = #f;
  sealed slot %warnings :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
end class <dfmc-project-object>;

define sealed primary class <dfmc-lid-project-object> (<dfmc-project-object>)
end class <dfmc-lid-project-object>;

define sealed primary class <dfmc-hdp-project-object> (<dfmc-project-object>)
  sealed slot %project-location :: false-or(<file-locator>) = #f,
    init-keyword: project-location:;
end class <dfmc-hdp-project-object>;

define function project-opened?(project :: <project-object>)
 => (opened? :: <boolean>)
  project.project-proxy & #t
end function project-opened?;

define sealed method find-open-project
    (key :: <project>) => (project :: false-or(<project-object>))
  any?(method (project-object :: <project-object>)
         (project-object.project-proxy == key) & project-object
       end,
       open-projects())
end method find-open-project;

define sealed method find-open-project
    (key :: <symbol>) => (project :: false-or(<project-object>))
  any?(method (project-object :: <project-object>)
         project-opened?(project-object)
           & (project-object.project-proxy.project-library-name == key)
           & project-object
       end,
       open-projects())
end method find-open-project;

define method project-opened-by-user?
    (project :: <dfmc-project-object>) => (by-user? :: <boolean>)
  project.%user-project?;
end method project-opened-by-user?;

define method project-opened-by-user?-setter
    (by-user? :: <boolean>, project-object :: <dfmc-project-object>)
 => (by-user? :: <boolean>)
  unless (by-user? == project-object.%user-project?)
    project-object.%user-project? := by-user?;
    by-user? & note-user-project-opened(project-object)
  end;
  by-user?
end method project-opened-by-user?-setter;

define thread variable *closing-project* = #f;

define sealed sideways method note-project-closed
    (project :: <project>) => ();
  unless(*closing-project* = project)
    let project-object = find-open-project(project);
    project-object & env/close-project(project-object)
  end;
end method note-project-closed;

define sealed sideways method note-database-saved
    (project :: <project>)
  let project-object = find-open-project(project);
  project-object & (project-object.%database-saved? := #t)
end method note-database-saved;

define sealed sideways method note-database-unsaved
    (project :: <project>)
  let project-object = find-open-project(project);
  project-object & (project-object.%database-saved? := #f)
end method note-database-unsaved;

define method do-all-client-projects
    (function :: <function>, project-object :: <project-object>)
  let project = project-object.ensure-project-proxy;
  // Refresh all projects that contain this project
  for (project-object :: <project-object> in open-projects())
    let proxy = project-object.project-proxy;
    if (project == proxy | (proxy & member?(project, proxy.all-used-projects)))
      function(project-object)
    end
  end
end method do-all-client-projects;

define sealed sideways method note-database-invalidated
    (project :: <project>)
  let project-object = find-open-project(project);
  if (project-object)
    let compiling? = *compiling?*;
    let database = project-compiler-database(project-object);
    // We really should only invalidate the proxies that
    // are from this particular project...
    if (database)
      debug-message("Invalidating database for '%s'",
                    environment-object-primitive-name
                      (project-object, project-object));
      invalidate-compiler-database(database);
      project-compiler-database(project-object) := #f
    else
      debug-message("Database for '%s' already invalidated",
                    environment-object-primitive-name
                      (project-object, project-object))
    end;
    clear-project-warnings(project-object);
    if (compiling?)
      note-project-warnings-updated(project-object)
    else
      note-database-updated(project-object)
    end
  end
end method note-database-invalidated;

define sealed method project-database-changed?
    (project-object :: <dfmc-project-object>)
 => (yes :: <boolean>)
  project-object.project-compiler-database
    & ~project-object.%database-saved?
end method project-database-changed?;

define sealed method project-sources-changed?
    (project-object :: <dfmc-project-object>)
 => (yes :: <boolean>)
  ~project-object.%database-current?
end method project-sources-changed?;

define constant $default-progress-message = "";

define variable *warning-callback* :: false-or(<function>) = #f;
define variable *progress-callback* :: false-or(<function>) = #f;
define variable *progress-section* :: <string> = "";
define variable *progress-message* :: <string> = "";
define variable *progress-internal?* :: <boolean> = #f;
define variable *progress-position* :: <integer> = 0;
define variable *progress-range* :: <integer> = 100;

// Switch off all compiler messages
show-compiler-messages?() := #f;

define function update-project-progress () => ()
  *progress-callback*
    & *progress-callback*(*progress-position*, *progress-range*,
                          heading-label:  *progress-section*,
                          item-label:     *progress-message*)
end function update-project-progress;

define function dfmc-project-library-name
    (project :: <project>) => (name :: <string>)
  let project-object = find-open-project(project);
  let library
    = project-object
        & project-compiler-database(project-object)
        & project-library(project-object);
  if (library)
    environment-object-primitive-name(project-object, library)
  else
    as-lowercase(as(<string>, project.project-library-name))
  end
end function dfmc-project-library-name;

define sealed sideways method project-stage-text
    (project :: <project>, #rest args) => ()
  let name = dfmc-project-library-name(project);
  let stage = apply(format-to-string, args);
  *progress-section* := stage;
  *progress-message* := $default-progress-message;
  update-project-progress()
end method project-stage-text;

define sealed sideways method project-progress-text
    (project :: <project>, #rest args) => ()
  unless (*progress-internal?*)
    let name = dfmc-project-library-name(project);
    if (empty?(*progress-section*))
      *progress-section* := format-to-string("Processing library %s", name)
    end;
    *progress-message* := apply(format-to-string, args);
    update-project-progress()
  end
end method project-progress-text;

define sealed sideways method project-internal-progress-text
    (project :: <project>, #rest args) => ()
  if (*progress-internal?*)
    *progress-message* := apply(format-to-string, args);
    update-project-progress()
  end
end method project-internal-progress-text;

define sealed sideways method project-progress-report
    (project :: <project>, sofar :: <integer>, abort-safe? :: <boolean>)
  //---*** We really should handle abort-safe? somehow...
  ignore(abort-safe?);
  *progress-position* := sofar;
  update-project-progress()
end method project-progress-report;

define function start-progress-reporting
    (project-object :: <project-object>,
     progress-callback :: false-or(<function>),
     #key section :: false-or(<string>),
          warning-callback :: false-or(<function>))
 => ()
  let project = project-object.ensure-project-proxy;
  *warning-callback*  := warning-callback;
  *progress-callback* := progress-callback;
  *progress-position* := 0;
  *progress-range*    := 100;
  if (section)
    *progress-section* := section
  end;
  project-progress-text(project, $default-progress-message)
end function start-progress-reporting;

define function stop-progress-reporting () => ()
  *warning-callback*  := #f;
  *progress-callback* := #f;
  *progress-section*  := "";
end function stop-progress-reporting;

define macro with-progress-reporting
  { with-progress-reporting
        (?project:expression, ?callback:expression, ?options:*)
      ?body:body
    end }
    => { block ()
           start-progress-reporting(?project, ?callback, ?options);
           ?body
         cleanup
           stop-progress-reporting()
         end }
end macro with-progress-reporting;

define sealed method project-condition-handler
    (project :: <project-object>, condition :: <condition>,
     handler :: <function>, next-handler :: <function>)
  select (condition by instance?)
    <project-not-found> =>
      let name = condition.condition-project-name;
      let filename
        = handler(#"project-not-found", as(<string>, name));
      if (filename)
        let location = as(<file-locator>, filename);
        signal(make(<find-project-location-restart>, location: location));
      else
        abort()
      end;
    <cannot-open-project-file-condition> =>
      let location = condition.condition-project-file-location;
      let filename
        = handler(#"project-file-not-found", as(<string>, location));
      filename & as(<file-locator>, filename);
    <duplicate-project-condition> =>
      let replace? = handler(#"yes-no", condition-to-string(condition));
      if(replace?)
        let key = duplicate-project-key(condition);
        let old-project = find-open-project(key);
        old-project & env/close-project(old-project);
      end;
      replace?;
    <yes-or-no-condition> =>
      handler(#"yes-no", condition-to-string(condition));
    <project-fatal-error> =>
      handler(#"fatal-error", condition-to-string(condition));
    <project-warning> =>
      record-project-warning(project, condition);
      //---*** andrewa: let's try going without the handler
      // handler(#"warning", condition-to-string(condition));
    otherwise =>
      next-handler();
  end
end method project-condition-handler;

define macro with-project-location-handler
  { with-project-location-handler (?project:expression, ?handler:expression)
      ?body:body
    end }
 => { local method condition-handler
                (condition :: <condition>, next-handler :: <function>)
              project-condition-handler
                (?project, condition, ?handler, next-handler)
            end;
      let handler (<yes-or-no-condition>) = condition-handler;
      let handler (<project-warning>) = condition-handler;
      let handler (<project-not-found>) = condition-handler;
      let handler (<cannot-open-project-file-condition>) = condition-handler;
      ?body }
end macro with-project-location-handler;

define sealed method build-project
    (project-object :: <dfmc-project-object>,
     #key clean? = #f, link? = #t, release? = #f, output = #[],
          warning-callback :: false-or(<function>),
          progress-callback :: false-or(<function>), error-handler,
          save-databases? = #f, copy-sources? = #f,
          process-subprojects? = #t,
          messages = #"external")
 => (built? :: <boolean>)
  let old-copy-sources? = *copy-canonical-sources?*;
  block ()
    let project = project-object.ensure-project-proxy;
    let assembler-output? = member?(#"assembler", output) | unsupplied();
    let dfm-output?       = member?(#"dfm", output);
    let harp-output?      = member?(#"harp", output);
    note-project-compilation-started(project-object);
    *progress-internal?*      := (messages == #"internal");
    *copy-canonical-sources?* := copy-sources?;
    let aborted?
      = with-progress-reporting
            (project-object, progress-callback,
             warning-callback: warning-callback)
          with-compiler-transaction
            with-project-location-handler (project-object, error-handler)
              if (process-subprojects?)
                update-libraries(project,
                                 force?: clean?,
                                 save?:  save-databases?,
                                 copy-sources?: copy-sources?,
                                 abort-on-all-warnings?:     #f,
                                 abort-on-serious-warnings?: #f,
                                 assembler-output?: assembler-output?,
                                 dfm-output?:       dfm-output?,
                                 harp-output?:      harp-output?)
              else
                compile-library(project,
                                force-parse?:   clean?,
                                force-compile?: clean?,
                                save?: save-databases?,
                                copy-sources?: copy-sources?,
                                abort-on-all-warnings?:     #f,
                                abort-on-serious-warnings?: #f,
                                assembler-output?: assembler-output?,
                                dfm-output?:       dfm-output?,
                                harp-output?:      harp-output?)
              end
            end
          end
        end;
    if (~aborted? & link?)
      link-project(project-object,
                   progress-callback: progress-callback,
                   error-handler: error-handler,
                   process-subprojects?: process-subprojects?,
                   messages: messages,
                   release?: release?)
    end;
    ~aborted?
  cleanup
    //--- We do this in a cleanup since the database is changed even
    //--- if the compile is aborted.
    note-project-compilation-finished(project-object);
    *copy-canonical-sources?* := old-copy-sources?;
  end
end method build-project;

define sealed method clean-project
    (project-object :: <dfmc-project-object>,
     #key error-handler, process-subprojects? = #t)
 => ()
  let project = project-object.ensure-project-proxy;
  if (project)
    project-remove-build-products(project, recursive?: process-subprojects?);
    clear-project-warnings(project-object);
    project-object.project-compiler-database := #f
  end;
end method clean-project;

define function note-project-compilation-started
    (project-object :: <dfmc-project-object>) => ()
  assert(~*compiling?*,
         "Attempting to build during another build!");
  assert(~project-object.project-application,
         "Attempting to compile with an application open!");
  assert(~project-object.ensure-project-proxy.project-execution-context,
         "Attempting to compile with an interactive context open!");
  *compiling?* := #t;
end function note-project-compilation-started;

define function note-project-compilation-finished
    (project-object :: <dfmc-project-object>) => ()
  *compiling?* := #f;
  let used-projects = project-object.project-proxy.all-used-projects;
  for (subproject-object :: <project-object> in open-projects())
    if (~empty?(project-canonical-sources(subproject-object)))
      let open-database?
        = project-object == subproject-object
            | member?(subproject-object.project-proxy, used-projects);
      note-database-updated(subproject-object, open-database?: open-database?)
    end
  end
end function note-project-compilation-finished;

define function note-database-updated
    (project-object :: <dfmc-project-object>, #key open-database? = #t) => ()
  if (open-database? & ~empty?(project-canonical-sources(project-object)))
    ensure-project-compiler-database(project-object)
  end;
  let message
    = make(<project-database-updated-message>, project: project-object);
  broadcast($project-channel, message);
  note-project-warnings-updated(project-object)
end function note-database-updated;

define sealed method parse-project-source
    (project-object :: <dfmc-project-object>,
     #key warning-callback :: false-or(<function>),
          progress-callback :: false-or(<function>), error-handler,
          process-subprojects? = #t)
 => (parsed? :: <boolean>);
  block ()
    note-project-compilation-started(project-object);
    with-compiler-transaction
      let project = project-object.ensure-project-proxy;
      with-progress-reporting
          (project-object, progress-callback,
           warning-callback: warning-callback)
        let aborted?
          = with-project-location-handler (project-object, error-handler)
              load-library(project-object.%library-name)
            end;
        ~aborted?
      end
    end
  cleanup
    //--- We do this in a cleanup since the database is changed even
    //--- if the parse is aborted.
    note-project-compilation-finished(project-object);
  end
end method parse-project-source;

define function ensure-project-compiler-database
    (project-object :: <dfmc-project-object>) => ()
  let project = project-object.ensure-project-proxy;
  if (~project-object.project-compiler-database
        & ~empty?(project-object.project-canonical-sources))
    %make-project-compiler-database(project-object)
  end
end function ensure-project-compiler-database;

define function %make-project-compiler-database
    (project-object :: <dfmc-project-object>)
 => (database :: false-or(<dfmc-database>))
  let project = project-object.project-proxy;
  let (context, in-memory?, current?, saved?)
    = ensure-project-database(project);
  project-object.%database-saved? := saved?;
  project-object.%database-current? := current?;
  if (in-memory?)
    let database
      = make(<dfmc-database>, project: project-object, proxy: project);
    project-object.project-compiler-database := database;
    database
  end
end function %make-project-compiler-database;

define function %make-project-object
    (project :: <project>) => (project-object :: <project-object>)
  find-open-project(project)
    | begin
        let name = as(<string>, project-library-name(project));
        let class
          = select (project by instance?)
              <user-project> => <dfmc-hdp-project-object>;
              otherwise      => <dfmc-lid-project-object>;
            end;
        let project-object = make(class, proxy: project, library-name: name);
        open-project-compiler-database(project-object);
        project-object
      end
end function %make-project-object;

//--- Is there a way to not make this sideways
//---*** It is unfortunate that we have to always return a project
//---*** here, what if it isn't in the registry...
define sealed sideways method find-project
    (library-name :: <string>)
 => (project :: <project-object>)
  any?(method (project-object :: <project-object>)
         instance?(project-object, <dfmc-project-object>)
           & project-object.%library-name = library-name
           & project-object
       end,
       open-projects())
    | make(<dfmc-lid-project-object>, library-name: library-name)
end method find-project;

//--- Note that this has to be a method on <dfmc-database> so that
//--- the proxies get properly resolved after a build invalidates
//--- them.
define sealed sideways method library-project
    (server :: <dfmc-database>, library :: <library-object>)
 => (project :: <project-object>)
  %make-project-object(library.compiler-object-proxy)
end method library-project;

define variable *link-error-handler* = #f;

define sealed method link-project-progress
    (message :: <string>, #key error?, warning?, phases, phase) => ()
  case
    error? =>
      //---*** Should do something better with linker errors
      *link-error-handler*
        & *link-error-handler*(#"link-error", message);
    warning? =>
      *link-error-handler*
        & *link-error-handler*(#"link-warning", message);
    phases =>
      *progress-section*  := message;
      *progress-message*  := "";
      *progress-range*    := phases;
    phase =>
      *progress-message*  := message;
      *progress-position* := phase;
    otherwise =>
      *progress-message*  := message;
  end;
  update-project-progress()
end method link-project-progress;

define sealed method link-project
    (project-object :: <dfmc-project-object>,
     #key progress-callback, error-handler, process-subprojects? = #t,
          build-script, target, arch, force?, unify?, release?, messages)
 => ()
  ignore(progress-callback, messages);
  let project = project-object.ensure-project-proxy;
  let name = dfmc-project-library-name(project);
  with-progress-reporting (project-object, progress-callback,
                           section: format-to-string("Linking %s", name))
    block ()
      *link-error-handler* := error-handler;
      let extent
        = case
            ~process-subprojects? => #"not-recursive";
            force?                => #"all";
            otherwise             => #"changes";
          end;
      with-project-location-handler (project-object, error-handler)
        link-library(project,
                     progress-callback: link-project-progress,
                     extent:      extent,
                     mode:        if (unify?) #"combine" end,
                     target-type: target,
                     arch:        arch,
                     build-script: build-script | default-build-script(),
                     release?:    release?)
      end
    cleanup
      *link-error-handler* := #f
    end
  end
end method link-project;

define sealed sideways method env/default-build-script
    () => (build-script :: <file-locator>)
  default-build-script()
end method env/default-build-script;

define sealed sideways method env/default-build-script-setter
    (build-script :: <file-locator>) => (build-script :: <file-locator>);
  default-build-script() := build-script
end method env/default-build-script-setter;

define sealed method env/close-project
    (project-object :: <dfmc-project-object>) => ()
  let project = project-object.project-proxy;
  dynamic-bind(*closing-project* = project)
    let closed?
      = if (project)
          close-project(project);
        else
          #t
        end;
    project-object.%user-project? := #f;
    if (closed?)
      remove!(*open-projects*, project-object);
      if (project-object.project-proxy)
        project-object.project-proxy := #f;
        // debug-message("dfmc-projects: setting proxy of %s to %f",
        //             project.project-name)
      end
    end;
    let message = make(<project-closed-message>, project: project-object);
    broadcast($project-channel, message);
    if (project-object == active-project())
      active-project()
        := any?(method (project-object :: <project-object>)
                  project-opened?(project-object)
                    & project-object.project-can-be-debugged?
                    & project-object
                end,
                open-projects())
    end
  end dynamic-bind;
end method env/close-project;

define sealed method project-sources
    (project-object :: <dfmc-project-object>, #key error-handler = #f)
 => (sources :: <sequence>)
  let project = project-object.ensure-project-proxy;
  with-project-manager-transaction
    if (project-read-only?(project))
      let records = make(<stretchy-vector>);
      for (record :: <source-record> in project-canonical-sources(project-object))
        if (record.source-record-name) add!(records, record) end
      end;
      records
    else
      with-project-location-handler(project-object,
                                    error-handler | method(#rest args) end)
        let records = project-source-records(project);
        records
      end;
    end
  end with-project-manager-transaction
end method project-sources;

define sealed method project-canonical-sources
    (project :: <dfmc-project-object>) => (sources :: <sequence>)
  let records = project-canonical-source-records(project.ensure-project-proxy);
  // map(method(r) source-record-name(r) end, records);
  records
end method project-canonical-sources;

define sealed method env/project-other-sources
    (project-object :: <dfmc-project-object>, #key) => (sources :: <sequence>)
  let project = project-object.ensure-project-proxy;
  let sources
    = with-project-manager-transaction
        project-other-sources(project);
      end;
  map(curry(as, <file-locator>), sources)
end method env/project-other-sources;

define sealed method project-filename
    (project :: <dfmc-project-object>) => (filename :: <file-locator>)
  project-location(project.ensure-project-proxy)
end method project-filename;

define sealed method project-filename
    (project :: <dfmc-hdp-project-object>) => (filename :: <file-locator>)
  let proxy = project.project-proxy;
  if (proxy)
    proxy.project-location
  else
    project.%project-location
  end
end method project-filename;

define sealed method project-directory
    (project :: <dfmc-project-object>) => (directory :: <directory-locator>)
  project.project-filename.locator-directory
end method project-directory;

define sealed method project-build-filename
    (project-object :: <dfmc-project-object>)
 => (filename :: <file-locator>)
  let project = project-object.ensure-project-proxy;
  as(<file-locator>, project-executable-pathname(project-object, project))
end method project-build-filename;

define sealed method project-build-filename-setter
    (filename :: <file-locator>, project-object :: <dfmc-project-object>)
 => (filename :: <file-locator>)
  let name = filename.locator-base;
  let project = project-object.ensure-project-proxy;
  unless (project.project-executable-name = name)
    project.project-executable-name := name;
    save-project(project)
  end;
  filename
end method project-build-filename-setter;

define sealed method project-build-directory
    (project :: <dfmc-project-object>) => (filename :: <directory-locator>)
  project-build-location(project.ensure-project-proxy)
end method project-build-directory;

define sealed method project-bin-directory
    (project :: <dfmc-project-object>) => (filename :: <directory-locator>)
  project-bin-location(project.ensure-project-proxy)
end method project-bin-directory;

define sealed method project-release-directory
    (project :: <dfmc-project-object>) => (filename :: <directory-locator>)
  //---*** This should probably come from the project manager
  subdirectory-locator(project-directory(project), "release")
end method project-release-directory;

define sealed method get-environment-object-primitive-name
    (server :: <dfmc-project-object>,
     project-object :: <dfmc-lid-project-object>)
 => (name :: false-or(<string>))
  as-lowercase(project-object.%library-name)
end method get-environment-object-primitive-name;

define sealed method get-environment-object-primitive-name
    (server :: <dfmc-project-object>,
     project-object :: <dfmc-hdp-project-object>)
 => (name :: false-or(<string>))
  let locator = project-object.%project-location;
  let project-name = locator & locator.locator-base;
  project-name | as-lowercase(project-object.%library-name)
end method get-environment-object-primitive-name;

define function update-project-build-property
    (project-object :: <dfmc-project-object>, property :: <symbol>, value)
 => ()
  let project = project-object.ensure-project-proxy;
  unless (project-build-property(project, property) = value)
    project-build-property(project, property) := value;
    save-project(project)
  end
end function update-project-build-property;


/// Debugging

define sealed method project-debug-filename
    (project :: <dfmc-project-object>)
 => (filename :: false-or(<file-locator>))
  let property
    = project-build-property(project.ensure-project-proxy, #"debug-command");
  property & as(<file-locator>, property)
end method project-debug-filename;

define sealed method project-debug-filename-setter
    (command :: false-or(<file-locator>), project :: <dfmc-project-object>)
 => (command :: false-or(<file-locator>))
  update-project-build-property(project, #"debug-command", command & as(<string>, command));
  command
end method project-debug-filename-setter;

define sealed method project-debug-arguments
    (project :: <dfmc-project-object>)
 => (arguments :: <string>)
  project-build-property(project.ensure-project-proxy, #"debug-arguments") | ""
end method project-debug-arguments;

define sealed method project-debug-arguments-setter
    (arguments :: <string>, project :: <dfmc-project-object>)
 => (arguments :: <string>)
  let maybe-arguments = if (~empty?(arguments)) arguments end;
  update-project-build-property(project, #"debug-arguments", maybe-arguments);
  arguments
end method project-debug-arguments-setter;

define sealed method project-debug-machine-address
    (project :: <dfmc-project-object>)
 => (address :: false-or(<string>))
  project-build-property(project.ensure-project-proxy, #"debug-machine")
end method project-debug-machine-address;

define sealed method project-debug-machine-address-setter
    (address :: false-or(<string>), project :: <dfmc-project-object>)
 => (address :: false-or(<string>))
  update-project-build-property(project, #"debug-machine", address);
  address
end method project-debug-machine-address-setter;

define sealed method project-debug-directory
    (project :: <dfmc-project-object>)
 => (directory :: false-or(<directory-locator>))
  let directory
    = project-build-property(project.ensure-project-proxy, #"debug-directory");
  directory & as(<directory-locator>, directory)
end method project-debug-directory;

define sealed method project-debug-directory-setter
    (directory :: false-or(<directory-locator>), project :: <dfmc-project-object>)
 => (directory :: false-or(<directory-locator>))
  update-project-build-property(project, #"debug-directory", as(<string>, directory));
  directory
end method project-debug-directory-setter;

define sealed method project-start-function-name
    (project :: <dfmc-project-object>)
 => (function-name :: false-or(<string>))
  project-build-property(project.ensure-project-proxy, #"start-function")
end method project-start-function-name;

define sealed method project-start-function-name-setter
    (function-name :: false-or(<string>), project :: <dfmc-project-object>)
 => (function-name :: false-or(<string>))
  update-project-build-property(project, #"start-function", function-name);
  function-name
end method project-start-function-name-setter;


/// Project properties

define sideways method env/session-property
    (key :: <symbol>) => (value)
  session-property(key)
end method env/session-property;

define sideways method env/session-property-setter
    (value, key :: <symbol>) => (value)
  session-property(key) := value
end method env/session-property-setter;

define sealed method env/project-read-only?
    (project :: <dfmc-project-object>)
 => (read-only? :: <boolean>)
  project.ensure-project-proxy.project-read-only?
end method env/project-read-only?;

define sealed method project-can-be-built?
    (project-object :: <dfmc-project-object>) => (can-be-built? :: <boolean>)
  ~env/project-read-only?(project-object)
end method project-can-be-built?;

define sealed method project-can-be-debugged?
    (project-object :: <dfmc-project-object>) => (can-be-debugged? :: <boolean>)
  //--- Always return #t, because if all else fails we'll use dll-wrap.exe.
  #t
  // project-object.env/project-target-type == #"executable"
  //   | project-object.project-debug-filename ~= #f
end method project-can-be-debugged?;

define constant $build-makefile = "dylanmakefile";

define sealed method project-compiled?
    (project :: <dfmc-project-object>)
 => (compiled? :: <boolean>)
  //---*** There should be a cleaner way to do this
  let build-location = project-build-location(project.ensure-project-proxy);
  if (build-location)
    let makefile
      = make(<native-file-locator>,
             directory: as(<directory-locator>, build-location),
             name:      $build-makefile);
    file-exists?(makefile)
  else
    #t
  end
end method project-compiled?;

define sealed method env/project-compilation-mode
    (project :: <dfmc-project-object>)
 => (compilation-mode :: <compilation-mode>)
  select (project-compilation-mode(project.ensure-project-proxy))
    #"incremental" => #"loose";
    #"loose"       => #"loose";
    #"tight"       => #"tight";
  end
end method env/project-compilation-mode;

define sealed method env/project-compilation-mode-setter
    (compilation-mode :: <compilation-mode>, project :: <dfmc-project-object>)
 => (compilation-mode :: <compilation-mode>)
  let proxy = project.ensure-project-proxy;
  unless (proxy.project-compilation-mode == compilation-mode)
    proxy.project-compilation-mode := compilation-mode;
    save-project(proxy)
  end;
  compilation-mode
end method env/project-compilation-mode-setter;

define sealed method env/project-compiler-back-end
    (project :: <dfmc-project-object>)
 => (back-end :: <symbol>)
  project-compiler-back-end(project.ensure-project-proxy);
end method env/project-compiler-back-end;

define sealed method env/project-compiler-back-end-setter
    (back-end :: <symbol>, project :: <dfmc-project-object>)
 => (back-end :: <symbol>)
  let proxy = project.ensure-project-proxy;
  unless (proxy.project-compiler-back-end == back-end)
    proxy.project-compiler-back-end := back-end;
    save-project(proxy);
  end;
  back-end
end method env/project-compiler-back-end-setter;

define sealed method env/project-target-type
    (project :: <dfmc-project-object>)
 => (target-type :: env/<project-target-type>)
  project-target-type(project.ensure-project-proxy)
end method env/project-target-type;

define sealed method env/project-target-type-setter
    (target-type :: env/<project-target-type>, project :: <dfmc-project-object>)
 => (target-type :: env/<project-target-type>)
  let proxy = project.ensure-project-proxy;
  unless (proxy.project-target-type == target-type)
    proxy.project-target-type := target-type;
    save-project(proxy)
  end;
  target-type
end method env/project-target-type-setter;

define constant $gui-linker-option = "$(guilflags)";

define sealed method project-interface-type
    (project-object :: <dfmc-project-object>)
 => (interface-type :: <project-interface-type>)
  let project = project-object.ensure-project-proxy;
  let linker-options = project-build-property(project, #"linker-options");
  let gui-type?
    = linker-options
        & member?($gui-linker-option, linker-options, test: \=);
  case
    gui-type? => #"gui";
    otherwise => #"console";
  end
end method project-interface-type;

define sealed method project-interface-type-setter
    (interface-type :: <project-interface-type>,
     project-object :: <dfmc-project-object>)
 => (interface-type :: <project-interface-type>)
  unless (project-object.project-interface-type == interface-type)
    let project = project-object.ensure-project-proxy;
    let linker-options = project-build-property(project, #"linker-options") | #();
    let new-linker-options
      = select (interface-type)
          #"gui"     => add-new(linker-options, $gui-linker-option, test: \=);
          #"console" => remove(linker-options,  $gui-linker-option, test: \=);
        end;
    project-build-property(project, #"linker-options") := new-linker-options;
    save-project(project);
  end;
  interface-type
end method project-interface-type-setter;

define sealed method env/project-base-address
    (project :: <dfmc-project-object>)
 => (address :: false-or(<machine-word>))
  let proxy = project.ensure-project-proxy;
  let address-value = project-build-property(proxy, #"base-address");
  address-value & string-to-machine-word(address-value)
end method env/project-base-address;

define sealed method env/project-base-address-setter
    (address :: false-or(<machine-word>), project :: <dfmc-project-object>)
 => (address :: false-or(<machine-word>))
  let proxy = project.ensure-project-proxy;
  let address-value = address & machine-word-to-string(address, prefix: "0x");
  project-build-property(proxy, #"base-address") := address-value;
  save-project(proxy);
  address
end method env/project-base-address-setter;

define sealed method env/project-major-version
    (project :: <dfmc-project-object>)
 => (version :: <integer>)
  project-major-version(project.ensure-project-proxy)
end method env/project-major-version;

define sealed method env/project-major-version-setter
    (version :: <integer>, project :: <dfmc-project-object>)
 => (version :: <integer>)
  let proxy = project.ensure-project-proxy;
  unless (proxy.project-major-version == version)
    proxy.project-major-version := version;
    save-project(proxy)
  end;
  version
end method env/project-major-version-setter;

define sealed method env/project-minor-version
    (project :: <dfmc-project-object>)
 => (version :: <integer>)
  project-minor-version(project.ensure-project-proxy)
end method env/project-minor-version;

define sealed method env/project-minor-version-setter
    (version :: <integer>, project :: <dfmc-project-object>)
 => (version :: <integer>)
  let proxy = project.ensure-project-proxy;
  unless (proxy.project-minor-version == version)
    proxy.project-minor-version := version;
    save-project(proxy)
  end;
  version
end method env/project-minor-version-setter;


/// File extensions

//--- These should really be defined in the project manager or somewhere
define constant $exe-file-extension   = "exe";
define constant $lid-file-extension   = "lid";
define constant $dylan-file-extension = "dylan";

define sealed sideways method project-file-extension
    () => (extension :: <string>)
  let extension = $user-project-suffix;
  //--- We can't be sure whether this starts with a '.'
  if (size(extension) > 0 & extension[0] = '.')
    copy-sequence(extension, start: 1)
  else
    extension
  end
end method project-file-extension;

define sealed sideways method lid-file-extension
    () => (extension :: <string>)
  $lid-file-extension
end method lid-file-extension;

define sealed sideways method dylan-file-extension
    () => (extension :: <string>)
  $dylan-file-extension
end method dylan-file-extension;

define sealed sideways method executable-file-extension
    () => (extension :: <string>)
  $exe-file-extension
end method executable-file-extension;


/// Project source handling

// TODO: should this be moved to environment-dfmc-database now that it gets
// the info from the database?
define sealed method source-record-colorization-info
    (project-object :: <dfmc-project-object>, sr :: <source-record>)
 => (info :: false-or(<vector>))
  let project = project-object.ensure-project-proxy;
  let context = project.ensure-project-database;
  context & dfmc/source-record-dispatch-decisions(context, sr);
end method source-record-colorization-info;

define sealed method do-used-projects
    (function :: <function>, project-object :: <dfmc-project-object>,
     #key indirect?, read-only?)
 => ()
  let project = project-object.ensure-project-proxy;
  %do-used-projects
    (function, project, indirect?: indirect?, read-only?: read-only?)
end method do-used-projects;

define sealed method do-used-projects
    (function :: <function>, project-object :: <dfmc-hdp-project-object>,
     #key indirect?, read-only?)
 => ()
  let project = project-object.ensure-project-proxy;
  %do-used-projects
    (function, project,
     indirect?: indirect?, read-only?: read-only?,
     extra-projects: project.project-user-projects)
end method do-used-projects;

define sealed method %do-used-projects
    (function :: <function>, project :: <project>,
     #key indirect?, read-only?, extra-projects, order = #"compilation")
 => ()
  let used-projects
    = case
        indirect? => project.all-used-projects;
        otherwise => project.directly-used-projects;
      end;
  let used-projects
    = if (extra-projects)
        remove-duplicates(concatenate(extra-projects, used-projects))
      else
        used-projects
      end;
  let used-projects
    = select (order)
        #"compilation" => reverse(used-projects);
        otherwise      => used-projects;
      end;
  for (subproject :: <project> in used-projects)
    if (read-only? | ~project-read-only?(subproject))
      function(%make-project-object(subproject))
    end
  end
end method %do-used-projects;


/// Opening of projects

define function abort-project-opening
    (name :: <string>)
  debug-message("Internal error: failed to open project %=", name);
  debug-message("  Aborting from opening of project...");
  abort()
end function abort-project-opening;

define sealed method open-project-compiler-database
    (project-object :: <dfmc-project-object>,
     #key warning-callback :: false-or(<function>),
          error-handler :: false-or(<function>))
 => (database :: false-or(<compiler-database>))
  let opened? = project-object.project-proxy ~== #f;
  block ()
    *warning-callback* := warning-callback;
    project-object.project-compiler-database
      | begin
          unless (project-object.project-proxy)
            %open-project-compiler-database
              (project-object, error-handler: error-handler)
          end;
          %make-project-compiler-database(project-object);
          register-unprocessed-warnings(project-object);
          project-object.project-compiler-database
        end
  cleanup
    *warning-callback* := #f
  end
end method open-project-compiler-database;

define sealed method %open-project-compiler-database
    (project-object :: <dfmc-lid-project-object>, #key error-handler)
 => ()
  with-project-manager-transaction
    with-project-location-handler (project-object, error-handler)
      let name = project-object.%library-name;
      let project = lookup-named-project(name);
      if (project)
        project-object.project-proxy := project;
      else
        env/close-project(project-object);
        abort-project-opening(as(<string>, name))
      end
    end
  end with-project-manager-transaction
end method %open-project-compiler-database;

define sealed method %open-project-compiler-database
    (project-object :: <dfmc-hdp-project-object>, #key error-handler)
 => ()
  with-project-manager-transaction
    with-project-location-handler (project-object, error-handler)
      let locator = project-object.%project-location;
      let project = open-project(locator);
      if (~project | find-open-project(project))
        env/close-project(project-object);
        abort-project-opening(as(<string>, locator))
      end;
      let name = as(<string>, project-library-name(project));
      project-object.%library-name := name;
      project-object.project-proxy := project;
    end
  end with-project-manager-transaction
end method %open-project-compiler-database;

define sealed method ensure-project-proxy
    (project-object :: <dfmc-project-object>) => (proxy :: <project>)
  project-object.project-proxy
    | error("Internal error: project database not opened for %s",
            environment-object-primitive-name(project-object, project-object))
end method ensure-project-proxy;

define sealed sideways method open-project-from-file
    (locator :: <file-locator>)
 => (project :: false-or(<project-object>))
  select (locator.environment-locator-type)
    #"lid" =>
      //---*** We should really be able to pass a filename
      let library-name = library-name-from-file(locator);
      library-name & find-project(as(<string>, library-name));
    #"hdp" =>
      if (file-exists?(locator))
        any?(method (project-object :: <project-object>)
               project-object.project-filename = locator
                 & project-object
             end,
             open-projects())
          | make(<dfmc-hdp-project-object>, project-location: locator)
      end;
    #"ddb" =>
      error("Cannot open .ddb files as projects yet!");
    otherwise =>
      error("Attempting to open non-project file %s using OPEN-PROJECT",
            locator);
  end
end method open-project-from-file;

define sealed sideways method import-project-from-file
    (locator :: <file-locator>, #key filename :: false-or(<file-locator>))
 => (project :: false-or(<project-object>))
  let project = import-lid-project(locator, to-file: filename);
  project & %make-project-object(project)
end method import-project-from-file;

define sealed sideways method create-new-user-project
    (name :: <string>, location :: <file-locator>)
 => (project :: false-or(<project-object>))
  let project = new-user-project(name, location);
  project & %make-project-object(project)
end method create-new-user-project;

define method note-project-sources-updated
    (project-object :: <dfmc-project-object>) => ()
  let message
    = make(<project-sources-updated-message>, project: project-object);
  broadcast($project-channel, message)
end method note-project-sources-updated;

define sealed method project-add-source-record
    (project :: <dfmc-project-object>,
     record :: type-union(<string>, <file-locator>))
 => ()
  project-add-file(project.ensure-project-proxy, record);
  note-project-sources-updated(project)
end method project-add-source-record;

define sealed method project-remove-source-record
    (project :: <dfmc-project-object>, record :: <string>)
 => ()
  project-remove-file(project.ensure-project-proxy, record);
  note-project-sources-updated(project)
end method project-remove-source-record;

define sealed method project-reorder-source-records
    (project :: <dfmc-project-object>, compare-function :: <function>)
 => ()
  project-sort-files(project.ensure-project-proxy, compare-function);
  note-project-sources-updated(project)
end method project-reorder-source-records;

define sealed method env/save-project
    (project-object :: <dfmc-project-object>,
     #key save-database? :: <boolean>,
          filename :: false-or(<file-locator>))
 => ()
  let project = project-object.ensure-project-proxy;
  if (filename)
    select (filename.environment-locator-type)
      #"lid" =>
        save-project-as-lid-file(project, to-file: filename);
      #"hdp" =>
        save-project(project, save-db?: save-database?, to-file: filename);
      #"ddb" =>
        error("Cannot save .ddb projects!");
      otherwise =>
        assert(#f,
               "Attempted to save project with invalid file extension '%s'",
               filename.locator-extension);
    end
  else
    save-project(project, save-db?: save-database?)
  end
end method env/save-project;

define sealed method env/save-project-database
    (project-object :: <dfmc-project-object>)
 => ()
  let project = project-object.ensure-project-proxy;
  save-project-database(project)
end method env/save-project-database;


/// UTILITIES
///---*** andrewa: stolen from build-system, this is just to get
///---*** things going for the beta...

//---*** andrewa: This should really be in projects!
define sealed method project-bin-location
    (project :: <project>) => (location :: <directory-locator>)
  case
    $personal-bin =>
      $personal-bin;
    project-read-only?(project) =>
      let release-locator = system-release-path();
      subdirectory-locator(release-locator, "bin");
    otherwise =>
      let parent-directory = project.project-build-location.locator-directory;
      subdirectory-locator(parent-directory, "bin");
  end
end method project-bin-location;


/// source-location-environment-object

define sealed method source-location-environment-object
    (project-object :: <dfmc-project-object>, location :: <source-location>)
 => (object :: false-or(<source-form-object>))
  let record = location.source-location-source-record;
  let subproject = record.source-record-project;
  if (subproject)
    let database = project-object.project-compiler-database;
    let start-offset = location.source-location-start-line;
    database
      & source-record-environment-object(database, subproject, record, start-offset)
  end
end method source-location-environment-object;

define sealed method find-source-record-library
    (project-object :: <dfmc-project-object>, record :: <source-record>)
 => (library :: false-or(<library-object>))
  let project = record.source-record-project;
  project & %maybe-make-library(project-object, project)
end method find-source-record-library;


/// Project warnings

define constant $project-warnings  :: <object-table> = make(<object-table>);

define sealed method %project-warnings
    (project :: <project>) => (warnings :: <sequence>)
  element($project-warnings, project, default: #[])
end method %project-warnings;

define sealed method register-project-warning
    (project :: <project>, warning :: <project-warning>)
 => ()
  let warnings :: <stretchy-object-vector>
    = element($project-warnings, project, default: #f)
        | begin
            element($project-warnings, project) := make(<stretchy-vector>)
          end;
  add!(warnings, warning)
end method register-project-warning;

define sealed method clear-project-warnings
    (project-object :: <dfmc-project-object>) => ()
  let project = project-object.ensure-project-proxy;
  project-object.%warnings.size := 0;
  remove-key!($project-warnings, project)
end method clear-project-warnings;

define method do-compiler-warnings
    (function :: <function>, project-object :: <dfmc-project-object>,
     object :: <project-object>,
     #key client)
 => ()
  assert(project-object == object,
         "Querying %= through different project %=!",
         object, project-object);
  let project = project-object.ensure-project-proxy;
  let database = project-object.project-compiler-database;
  let show-read-only? = project.project-read-only?;

  local
    method do-project-library-warnings
        (project :: <project>) => ()
      let context = project.project-browsing-context;
      if (context.project-library-definition)
        let library
          = make-environment-object
              (<library-object>,
               project: project-object,
               compiler-object-proxy: project);
        do-compiler-warnings
          (function, project-object, library, client: client)
      end
    end method do-project-library-warnings,

    method do-project-warnings
        (project :: <project>) => ()
      if (show-read-only? | ~project.project-read-only?)
        for (warning :: <project-warning> in project.%project-warnings)
          let warning = make(<project-warning-object>, proxy: warning);
          function(warning)
        end;
        database & do-project-library-warnings(project)
      end
    end method do-project-warnings;

  do(do-project-warnings, reverse(project.all-used-projects));
  do-project-warnings(project)
end method do-compiler-warnings;

define sealed method record-project-warning
    (project-object :: <dfmc-project-object>, warning :: <project-warning>)
 => ()
  //---*** We really need to know which subproject we are dealing with...
  let project = project-object.project-proxy;
  if (project)
    register-unprocessed-warnings(project-object);
    register-project-warning(project, warning);
    note-project-warnings-updated(project-object);
    let callback = *warning-callback*;
    if (callback)
      callback(make(<project-warning-object>, proxy: warning))
    end
  else
    add!(project-object.%warnings, warning)
  end
end method record-project-warning;

define sealed method register-unprocessed-warnings
    (project-object :: <dfmc-project-object>) => ()
  let project = project-object.ensure-project-proxy;
  let unprocessed-warnings = project-object.%warnings;
  unless (empty?(unprocessed-warnings))
    do(curry(register-project-warning, project), unprocessed-warnings);
    size(unprocessed-warnings) := 0
  end
end method register-unprocessed-warnings;

define sealed sideways method project-condition-report
    (project :: <project>, condition :: <program-condition>)
  let project-object = find-open-project(project);
  if (project-object)
    local method note-new-warning
              (project-object :: <project-object>)
            ensure-project-compiler-database(project-object);
            note-project-warnings-updated(project-object)
          end method note-new-warning;
    note-new-warning(project-object);
    do-all-client-projects(note-new-warning, project-object);
    let callback = *warning-callback*;
    if (callback)
      let library = project-library(project-object);
      library
        & do-program-notes
            (callback, project-object, library, vector(condition))
    end
  end
end method project-condition-report;

define method note-project-warnings-updated
    (project-object :: <project-object>) => ()
  let message
    = make(<project-warnings-updated-message>, project: project-object);
  broadcast($project-channel, message)
end method note-project-warnings-updated;


/// Project warning objects

define sealed method warning-owner
    (project-object :: <dfmc-project-object>,
     warning :: <project-warning-object>)
 => (owner :: false-or(<dfmc-project-object>))
  block (return)
    let condition = warning.environment-object-proxy;
    for (warnings keyed-by project in $project-warnings)
      if (member?(condition, warnings))
        return(%make-project-object(project))
      end
    end;
  end
end method warning-owner;

define sealed method environment-object-library
    (server :: <dfmc-project-object>, warning :: <project-warning-object>)
 => (library :: false-or(<library-object>))
  let owner = warning-owner(server, warning);
  owner & project-library(owner)
end method environment-object-library;

define sealed method environment-object-source-location
    (server :: <dfmc-project-object>, warning :: <project-warning-object>)
 => (location :: false-or(<source-location>))
  #f
end method environment-object-source-location;

define sealed method compiler-warning-full-message
    (server :: <dfmc-project-object>, warning :: <project-warning-object>)
 => (message :: <string>)
  let condition :: <project-warning> = warning.environment-object-proxy;
  condition-to-string(condition)
end method compiler-warning-full-message;

define sealed method compiler-warning-short-message
    (server :: <dfmc-project-object>, warning :: <project-warning-object>)
 => (message :: <string>)
  // Just use the first line
  let message = compiler-warning-full-message(server, warning);
  first-line(message)
end method compiler-warning-short-message;

define sealed method get-environment-object-primitive-name
    (server :: <dfmc-project-object>, warning :: <project-warning-object>)
 => (message :: <string>)
  compiler-warning-short-message(server, warning)
end method get-environment-object-primitive-name;

define function first-line
    (string :: <string>) => (line :: <string>)
  let newline-key
    = find-key(string,
               method (character)
                 member?(character, #('\n', '\r'))
               end method);
  if (newline-key)
    // Strip off a single trailing period, if any
    if (newline-key > 0 & string[newline-key - 1] = '.')
      newline-key := newline-key - 1
    end;
    copy-sequence(string, end: newline-key)
  else
    string
  end
end function first-line;
