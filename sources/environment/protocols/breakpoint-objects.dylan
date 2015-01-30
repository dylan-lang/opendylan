Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong, Chris Page, Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <BREAKPOINT-STATE> (environment-protocols)

define constant <breakpoint-state>
  = one-of(
           #"created",
           #"destroyed",
           #"stop?",
           #"message?",
           #"transient?",
           #"profile?",
           #"directions",
           #"enabled?",
           #"test"
           );

/// <BREAKPOINT-DIRECTION> (environment-protocols)

define constant <breakpoint-direction>
  = one-of(#"in", #"out");

/// INITIALIZATION CONSTANTS

define constant $default-breakpoint-stop?           = #t;
define constant $default-breakpoint-message?        = "";
define constant $default-breakpoint-transient?      = #f;
define constant $default-breakpoint-enabled?        = #t;
define constant $default-breakpoint-profile?        = #f;
define constant $default-breakpoint-test            = #f;
define constant $default-breakpoint-directions      = vector(#"in", #"out");
define constant $default-breakpoint-entry-function? = #f;

/// <BREAKPOINT-OBJECT> (environment-protocols)

define abstract class <breakpoint-object> (<application-object>)
  constant slot breakpoint-project :: <project-object>,
    required-init-keyword: project:;
  slot breakpoint-library :: false-or(<library-object>) = #f,
    init-keyword: library:;
  slot breakpoint-object :: <object>,
    init-keyword: object:;
  slot breakpoint-stop? :: <boolean> = $default-breakpoint-stop?,
    setter: %breakpoint-stop?-setter,
    init-keyword: stop?:;
  slot breakpoint-message? :: false-or(<string>) = $default-breakpoint-message?,
    setter: %breakpoint-message?-setter,
    init-keyword: message?:;
  slot breakpoint-transient? :: <boolean> = $default-breakpoint-transient?,
    setter: %breakpoint-transient?-setter,
    init-keyword: transient?:;
  slot breakpoint-enabled? :: <boolean> = $default-breakpoint-enabled?,
    setter: %breakpoint-enabled?-setter,
    init-keyword: enabled?:;
  slot breakpoint-profile? :: <boolean> = $default-breakpoint-profile?,
    setter: %breakpoint-profile?-setter,
    init-keyword: profile?:;
  slot breakpoint-test = $default-breakpoint-test,
    setter: %breakpoint-test-setter,
    init-keyword: test:;
end class <breakpoint-object>;

/// <ENVIRONMENT-OBJECT-BREAKPOINT-OBJECT> (environment-protocols)

define abstract class <environment-object-breakpoint-object>
    (<breakpoint-object>)
end class <environment-object-breakpoint-object>;

define method environment-object-library
    (server :: <server>, breakpoint :: <environment-object-breakpoint-object>)
 => (library :: false-or(<library-object>))
  breakpoint.breakpoint-library
    | begin
        let object = breakpoint.breakpoint-object;
        //---*** andrewa: why wouldn't this always be <environment-object>?
        if (instance?(object, <environment-object>))
          let library = environment-object-library(server, object);
          breakpoint.breakpoint-library := library
        else
          debug-out(#"environment-protocols",
                    "Breakpoint %= has unexpected object %=",
                    breakpoint, object)
        end
      end
end method environment-object-library;

/// <FUNCTION-BREAKPOINT-OBJECT> (environment-protocols)

define abstract class <function-breakpoint-object>
    (<environment-object-breakpoint-object>)
  slot breakpoint-directions :: <sequence> = $default-breakpoint-directions,
    setter: %breakpoint-directions-setter,
    init-keyword: directions:;
  slot breakpoint-entry-function? :: <boolean> = $default-breakpoint-entry-function?,
    init-keyword: entry-function?:;
  slot breakpoint-entry-point? :: <boolean> = #f,
    init-keyword: entry-point?:;
end class <function-breakpoint-object>;

define class <class-breakpoint-object>
    (<environment-object-breakpoint-object>)
end class <class-breakpoint-object>;

define class <simple-function-breakpoint-object>
    (<function-breakpoint-object>)
end class <simple-function-breakpoint-object>;

define class <generic-function-breakpoint-object>
    (<function-breakpoint-object>)
end class <generic-function-breakpoint-object>;

define class <method-breakpoint-object>
    (<function-breakpoint-object>)
end class <method-breakpoint-object>;

/// <SOURCE-LOCATION-BREAKPOINT-OBJECT> (environment-protocols)

define class <source-location-breakpoint-object> (<breakpoint-object>)
end class <source-location-breakpoint-object>;

define method environment-object-library
    (server :: <server>, breakpoint :: <source-location-breakpoint-object>)
 => (library :: false-or(<library-object>))
  breakpoint.breakpoint-library
    | begin
        let project = server.server-project;
        let location = breakpoint.breakpoint-object;
        let record = location.source-location-source-record;
        let library = find-source-record-library(project, record);
        breakpoint.breakpoint-library := library
      end
end method environment-object-library;

/// SOURCE-LOCATION-BREAKPOINTS (environment-protocols)

define open generic source-location-breakpoints
    (project :: <project-object>)
 => (breakpoints :: <collection>);

/// ENVIRONMENT-OBJECT-BREAKPOINTS (environment-protocols)

define open generic environment-object-breakpoints
    (project :: <project-object>)
 => (breakpoints :: <collection>);

/// PROJECT-BREAKPOINTS (environment-protocols)

define open generic project-breakpoints
    (project :: <project-object>)
 => (breakpoints :: <collection>);

define method project-breakpoints
    (project :: <project-object>)
 => (breakpoints :: <collection>)
  concatenate-as(<vector>,
                 as(<vector>, project.source-location-breakpoints),
                 as(<vector>, project.environment-object-breakpoints));
end method project-breakpoints;

/// MAKE (dylan)

define method make
    (class == <breakpoint-object>, #rest args, #key object)
 => (breakpoint :: <breakpoint-object>)
  let class = class-for-breakpoint(object);
  assert(class, "Attempting to make breakpoint for unrecognized object %=", object);
  apply(make, class, args);
end method make;

define method make
    (class :: subclass(<breakpoint-object>), #rest args, #key)
 => (breakpoint :: <breakpoint-object>)
  let breakpoint = apply(find-breakpoint, class, args);
  if (breakpoint)
    apply(reinitialize-breakpoint, breakpoint, args);
    breakpoint;
  else
    next-method();
  end if;
end method make;

/// CLASS-FOR-BREAKPOINT

define generic class-for-breakpoint
    (object :: <object>)
 => (class :: false-or(<class>));
//---*** was this, but the compiler can't deal with it!
// => (class :: false-or(subclass(<breakpoint-object>)));

define method class-for-breakpoint
    (object :: <object>)
 => (class :: singleton(#f))
  #f
end method class-for-breakpoint;

define method class-for-breakpoint
    (object :: <source-location>)
 => (class :: subclass(<breakpoint-object>))
  <source-location-breakpoint-object>
end method class-for-breakpoint;

define method class-for-breakpoint
    (object :: <class-object>)
 => (class :: subclass(<breakpoint-object>))
  <class-breakpoint-object>
end method class-for-breakpoint;

define method class-for-breakpoint
    (object :: <simple-function-object>)
 => (class :: subclass(<breakpoint-object>))
  <simple-function-breakpoint-object>
end method class-for-breakpoint;

define method class-for-breakpoint
    (object :: <method-object>)
 => (class :: subclass(<breakpoint-object>))
  <method-breakpoint-object>
end method class-for-breakpoint;

define method class-for-breakpoint
    (object :: <generic-function-object>)
 => (class :: subclass(<breakpoint-object>))
  <generic-function-breakpoint-object>
end method class-for-breakpoint;


/// ENSURE-BREAKPOINT-FOR-ALL-METHODS

define function ensure-breakpoint-for-all-methods
    (breakpoint :: <generic-function-breakpoint-object>) => ()
  let project         = breakpoint.breakpoint-project;
  let object          = breakpoint.breakpoint-object;
  let enabled?        = breakpoint.breakpoint-enabled?;
  let transient?      = breakpoint.breakpoint-transient?;
  let message?        = breakpoint.breakpoint-message?;
  let profile?        = breakpoint.breakpoint-profile?;
  let stop?           = breakpoint.breakpoint-stop?;
  let test            = breakpoint.breakpoint-test;
  let directions      = breakpoint.breakpoint-directions;
  let entry-function? = breakpoint.breakpoint-entry-function?;
  let entry-point?    = breakpoint.breakpoint-entry-point?;
  for (function in generic-function-object-methods(project, object))
    make(<method-breakpoint-object>,
         project:         project,
         object:          function,
         enabled?:        enabled?,
         transient?:      transient?,
         message?:        message?,
         profile?:        profile?,
         stop?:           stop?,
         test:            test,
         directions:      directions,
         entry-function?: entry-function?,
         entry-point?:    entry-point?)
  end
end function;


/// INITIALIZE (dylan)

define method initialize
    (breakpoint :: <breakpoint-object>, #rest args, #key) => ()
  next-method();
  apply(initialize-breakpoint, breakpoint, args);
  note-breakpoint-state-changed(breakpoint, #"created")
end method initialize;

/// INITIALIZE-BREAKPOINT (environment-protocol)

define open generic initialize-breakpoint
    (breakpoint :: <breakpoint-object>, #key, #all-keys)
 => ();

define method initialize-breakpoint
    (breakpoint :: <breakpoint-object>, #key)
 => ()
end method initialize-breakpoint;

define method initialize-breakpoint
    (breakpoint :: <source-location-breakpoint-object>, #key object)
 => ()
  next-method();
  let breakpoints = breakpoint.breakpoint-project.source-location-breakpoints;
  element(breakpoints, object) := breakpoint
end method initialize-breakpoint;

define method initialize-breakpoint
    (breakpoint :: <environment-object-breakpoint-object>, #key object)
 => ()
  next-method();
  let breakpoints
    = breakpoint.breakpoint-project.environment-object-breakpoints;
  element(breakpoints, object) := breakpoint
end method initialize-breakpoint;

define method initialize-breakpoint
    (breakpoint :: <generic-function-breakpoint-object>, #key)
 => ()
  next-method();
  ensure-breakpoint-for-all-methods(breakpoint);
end method initialize-breakpoint;

/// FIND-BREAKPOINT (environment-protocol)

define open generic find-breakpoint
    (class :: subclass(<breakpoint-object>), #rest args, #key, #all-keys)
 => (breakpoint :: false-or(<breakpoint-object>));

define method find-breakpoint
    (class == <breakpoint-object>, #rest args, #key project, object)
 => (breakpoint :: false-or(<breakpoint-object>));
  let class = class-for-breakpoint(object);
  class & apply(find-breakpoint, class, args)
end method find-breakpoint;

define method find-breakpoint
    (class :: subclass(<environment-object-breakpoint-object>),
     #rest args, #key project, object)
 => (breakpoint :: false-or(<environment-object-breakpoint-object>))
  element(project.environment-object-breakpoints, object, default: #f)
end method find-breakpoint;

define method find-breakpoint
    (class :: subclass(<source-location-breakpoint-object>),
     #rest args, #key project, object)
 => (breakpoint :: false-or(<source-location-breakpoint-object>))
  element(project.source-location-breakpoints, object, default: #f)
end method find-breakpoint;

/// REINITIALIZE-BREAKPOINT (environment-protocol)

define open generic reinitialize-breakpoint
    (breakpoint :: <breakpoint-object>, #rest args, #key, #all-keys)
 => ();

define method reinitialize-breakpoint
    (breakpoint :: <breakpoint-object>,
     #rest args,
     #key enabled? = unsupplied(),
          message? = unsupplied(),
          stop? = unsupplied(),
          transient? = unsupplied(),
          profile? = unsupplied(),
          test = unsupplied())
 => ()
  if (supplied?(enabled?))
    breakpoint.breakpoint-enabled? := enabled?;
  end if;
  if (supplied?(transient?))
    breakpoint.breakpoint-transient? := transient?;
  end if;
  if (supplied?(profile?))
    breakpoint.breakpoint-profile? := profile?;
  end if;
  if (supplied?(message?))
    breakpoint.breakpoint-message? := message?;
  end if;
  if (supplied?(stop?))
    breakpoint.breakpoint-stop? := stop?;
  end if;
  if (supplied?(test))
    breakpoint.breakpoint-test := test;
  end if;
end method reinitialize-breakpoint;

define method reinitialize-breakpoint
    (breakpoint :: <function-breakpoint-object>,
     #rest args,
     #key directions = unsupplied(),
          entry-function? = unsupplied(),
          entry-point? = unsupplied())
 => ()
  next-method();
  if (supplied?(directions))
    breakpoint.breakpoint-directions := directions;
  end if;
  if (supplied?(entry-function?))
    breakpoint.breakpoint-entry-function? := entry-function?;
  end if;
  if (supplied?(entry-point?))
    breakpoint.breakpoint-entry-point? := entry-point?;
  end if;
end method reinitialize-breakpoint;

define method reinitialize-breakpoint
    (breakpoint :: <generic-function-breakpoint-object>, #rest args, #key)
 => ()
  next-method();
  ensure-breakpoint-for-all-methods(breakpoint);
end method reinitialize-breakpoint;

/// DESTROY-BREAKPOINT (environment-protocol)

define open generic destroy-breakpoint
    (breakpoint :: <breakpoint-object>) => ();

define method destroy-breakpoint
    (breakpoint :: <breakpoint-object>)
 => ()
  note-breakpoint-state-changed(breakpoint, #"destroyed")
end method destroy-breakpoint;

define method destroy-breakpoint
    (breakpoint :: <source-location-breakpoint-object>)
 => ()
  let breakpoints = breakpoint.breakpoint-project.source-location-breakpoints;
  remove-key!(breakpoints, breakpoint.breakpoint-object);
  next-method()
end method destroy-breakpoint;

define method destroy-breakpoint
    (breakpoint :: <environment-object-breakpoint-object>)
 => ()
  let breakpoints
    = breakpoint.breakpoint-project.environment-object-breakpoints;
  remove-key!(breakpoints, breakpoint.breakpoint-object);
  next-method()
end method destroy-breakpoint;

define method destroy-breakpoint
    (breakpoint :: <generic-function-breakpoint-object>)
 => ()
  next-method();
  do-generic-breakpoint-methods(destroy-breakpoint, breakpoint)
end method destroy-breakpoint;

define method destroy-breakpoint
    (breakpoint :: <method-breakpoint-object>)
 => ()

  let project = breakpoint.breakpoint-project;

  ///// BREAKPOINT-FOR-METHOD (Local convenience function)
  //    Given a <method-object>, returns a <method-breakpoint-object> if
  //    there is a breakpoint on the method, otherwise returns #f.
  //    The implementation is clearly trivial, but having this improves
  //    the clarity of some other code in this function.

  local method breakpoint-for-method
            (m :: <method-object>)
         => (maybe-bp :: false-or(<method-breakpoint-object>))
          find-breakpoint(<method-breakpoint-object>,
                          project: project, object: m)
        end method breakpoint-for-method;


  ///// ZERO-BREAKPOINTED-METHODS? (Local convenience function).
  //    Given a <generic-function-object>, returns #f if one or more of
  //    its methods has an associated breakpoint, otherwise returns #t.

  local method zero-breakpointed-methods?
            (gf :: <generic-function-object>)
         => (well? :: <boolean>)
          let methods = generic-function-object-methods(project, gf);
          ~any?(breakpoint-for-method, methods);
        end method zero-breakpointed-methods?;


  ///// MAYBE-GARBAGE-COLLECT-GF-BREAKPOINT (Local convenience function).
  //    Takes a <generic-function-object>. If a breakpoint exists for the
  //    generic function, but none of its methods have associated
  //    breakpoints, removes the breakpoint on the generic.

  local method maybe-garbage-collect-gf-breakpoint
            (gf :: <generic-function-object>)
         => ()
          let gf-breakpoint
            = find-breakpoint(<generic-function-breakpoint-object>,
                              project: project,
                              object: gf);
          if (gf-breakpoint & zero-breakpointed-methods?(gf))
            //---*** phoward. Jason noted that this call to DESTROY-BREAKPOINT
            // will try to destroy all subordinate method breakpoints. But,
            // we've obviously just determined that there aren't any, meaning
            // that DESTROY-BREAKPOINT here is going to do some redundant and
            // fruitless work. We should try to abstract out a more primitive
            // function for destroying a GF breakpoint.
            destroy-breakpoint(gf-breakpoint);
          end if;
        end method maybe-garbage-collect-gf-breakpoint;

  next-method();
  let method-object = breakpoint.breakpoint-object;
  // The breakpoint might be on an object that no longer exists, so
  // be careful not to do any unnecessary queries on it.
  if (environment-object-exists?(project, method-object))
    let gf-object = method-generic-function(project, method-object);
    if (gf-object)
      // It maybe the case that we have just destroyed the last surviving
      // method breakpoint for a breakpointed generic function. If this is
      // the case, we can destroy the generic function breakpoint itself.
      maybe-garbage-collect-gf-breakpoint(gf-object);
    end if;
  end if;
end method destroy-breakpoint;


/// BREAKPOINT GETTERS (environment-protocol)

define open generic breakpoint-project
    (breakpoint :: <breakpoint-object>)
 => (project :: <project-object>);

define open generic breakpoint-object
    (breakpoint :: <breakpoint-object>)
 => (object :: <object>);

define open generic breakpoint-object-setter
    (object :: <object>, breakpoint :: <breakpoint-object>)
 => (object :: <object>);

define open generic breakpoint-stop?
    (breakpoint :: <breakpoint-object>)
 => (stop? :: <boolean>);

define open generic breakpoint-message?
    (breakpoint :: <breakpoint-object>)
 => (message? :: false-or(<string>));

define open generic breakpoint-transient?
    (breakpoint :: <breakpoint-object>)
 => (transient? :: <boolean>);

define open generic breakpoint-profile?
    (breakpoint :: <breakpoint-object>)
 => (profile? :: <boolean>);

define open generic breakpoint-enabled?
    (breakpoint :: <breakpoint-object>)
 => (enabled? :: <boolean>);

define open generic breakpoint-directions
    (breakpoint :: <breakpoint-object>)
 => (directions :: <sequence>);

define open generic breakpoint-test
    (breakpoint :: <breakpoint-object>)
 => (test);

define open generic breakpoint-entry-function?
    (breakpoint :: <breakpoint-object>)
 => (entry-function? :: <boolean>);

/// BREAKPOINT SETTERS (environment-protocol)

define open generic breakpoint-stop?-setter
    (stop? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (stop? :: <boolean>);

define open generic breakpoint-message?-setter
    (message? :: false-or(<string>), breakpoint :: <breakpoint-object>)
 => (message? :: false-or(<string>));

define open generic breakpoint-transient?-setter
    (transient? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (transient? :: <boolean>);

define open generic breakpoint-profile?-setter
    (profile? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (profile? :: <boolean>);

define open generic breakpoint-enabled?-setter
    (enabled? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (enabled? :: <boolean>);

define open generic breakpoint-directions-setter
    (directions :: <sequence>, breakpoint :: <breakpoint-object>)
 => (directions :: <sequence>);

define open generic breakpoint-test-setter
    (test, breakpoint :: <breakpoint-object>)
 => (test);

define open generic breakpoint-entry-function?-setter
    (entry-function? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (entry-function? :: <boolean>);

define open generic breakpoint-entry-point?-setter
    (entry-point? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (entry-point? :: <boolean>);

define method breakpoint-stop?-setter
    (stop? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (stop? :: <boolean>)
  breakpoint.%breakpoint-stop? := stop?;
  note-breakpoint-state-changed(breakpoint, #"stop?");
  stop?
end method breakpoint-stop?-setter;

define method breakpoint-stop?-setter
    (stop? :: <boolean>, breakpoint :: <generic-function-breakpoint-object>)
 => (stop? :: <boolean>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-stop?-setter, stop?), breakpoint);
  stop?
end method breakpoint-stop?-setter;

define method breakpoint-message?-setter
    (message? :: false-or(<string>), breakpoint :: <breakpoint-object>)
 => (message? :: false-or(<string>))
  breakpoint.%breakpoint-message? := message?;
  note-breakpoint-state-changed(breakpoint, #"message?");
  message?
end method breakpoint-message?-setter;

define method breakpoint-message?-setter
    (message? :: false-or(<string>), breakpoint :: <generic-function-breakpoint-object>)
 => (message? :: false-or(<string>))
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-message?-setter, message?), breakpoint);
  message?
end method breakpoint-message?-setter;

define method breakpoint-transient?-setter
    (transient? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (transient? :: <boolean>)
  breakpoint.%breakpoint-transient? := transient?;
  note-breakpoint-state-changed(breakpoint, #"transient?");
  transient?
end method breakpoint-transient?-setter;

define method breakpoint-transient?-setter
    (transient? :: <boolean>, breakpoint :: <generic-function-breakpoint-object>)
 => (transient? :: <boolean>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-transient?-setter, transient?), breakpoint);
  transient?
end method breakpoint-transient?-setter;

define method breakpoint-profile?-setter
    (profile? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (profile? :: <boolean>)
  breakpoint.%breakpoint-profile? := profile?;
  note-breakpoint-state-changed(breakpoint, #"profile?");
  profile?
end method breakpoint-profile?-setter;

define method breakpoint-profile?-setter
    (profile? :: <boolean>, breakpoint :: <generic-function-breakpoint-object>)
 => (profile? :: <boolean>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-profile?-setter, profile?), breakpoint);
  profile?
end method breakpoint-profile?-setter;

define method breakpoint-enabled?-setter
    (enabled? :: <boolean>, breakpoint :: <breakpoint-object>)
 => (enabled? :: <boolean>)
  breakpoint.%breakpoint-enabled? := enabled?;
  note-breakpoint-state-changed(breakpoint, #"enabled?");
  enabled?
end method breakpoint-enabled?-setter;

define method breakpoint-enabled?-setter
    (enabled? :: <boolean>, breakpoint :: <generic-function-breakpoint-object>)
 => (enabled? :: <boolean>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-enabled?-setter, enabled?), breakpoint);
  enabled?
end method breakpoint-enabled?-setter;

define method breakpoint-directions-setter
    (directions :: <sequence>, breakpoint :: <function-breakpoint-object>)
 => (directions :: <sequence>)
  breakpoint.%breakpoint-directions := directions;
  note-breakpoint-state-changed(breakpoint, #"directions");
  directions
end method breakpoint-directions-setter;

define method breakpoint-directions-setter
    (directions :: <sequence>, breakpoint :: <generic-function-breakpoint-object>)
 => (directions :: <sequence>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-directions-setter, directions), breakpoint);
  directions
end method breakpoint-directions-setter;

define method breakpoint-test-setter
    (test, breakpoint :: <breakpoint-object>)
 => (test)
  breakpoint.%breakpoint-test := test;
  note-breakpoint-state-changed(breakpoint, #"test");
  test
end method breakpoint-test-setter;

define method breakpoint-test-setter
    (test, breakpoint :: <generic-function-breakpoint-object>)
 => (test)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-test-setter, test), breakpoint);
  test
end method breakpoint-test-setter;

define method breakpoint-entry-function?-setter
    (entry-function? :: <boolean>,
     breakpoint :: <generic-function-breakpoint-object>)
 => (entry-function? :: <boolean>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-entry-function?-setter, entry-function?), breakpoint);
  entry-function?
end method breakpoint-entry-function?-setter;

define method breakpoint-entry-point?-setter
    (entry-point? :: <boolean>,
     breakpoint :: <generic-function-breakpoint-object>)
 => (entry-point? :: <boolean>)
  next-method();
  do-generic-breakpoint-methods(curry(breakpoint-entry-point?-setter, entry-point?), breakpoint);
  entry-point?
end method breakpoint-entry-point?-setter;

/// NOTE-BREAKPOINT-STATE-CHANGED (environment-protocol)

define open generic note-breakpoint-state-changed
    (breakpoint :: <breakpoint-object>, state :: <breakpoint-state>)
  => ();

//---*** andrewa: temporary solution to solve the problem that
//---*** breakpoint objects don't always have a proxy. Ultimately
//---*** we should use the id protocol to create breakpoints that
//---*** aren't connected to a particular server.
define method choose-server
    (project :: <project-object>, breakpoint :: <breakpoint-object>,
     #key error?, default-server)
 => (application :: false-or(<application>))
  ignore(default-server);
  project-application(project)
    | if (error?) closed-server-error(breakpoint) end
end method choose-server;

define method note-breakpoint-state-changed
    (breakpoint :: <breakpoint-object>, state :: <breakpoint-state>)
  => ()
  let project :: <project-object> = breakpoint.breakpoint-project;
  server-note-breakpoint-state-changed(project, breakpoint, state);
  let server = choose-server(project, breakpoint);
  if (server)
    server-note-breakpoint-state-changed(server, breakpoint, state);
  end if;
/*
  // phoward alternative implementation that might fix bug 2030
  for (project in open-projects())
    let application = project.project-application;
    let proxy = project.project-proxy;
    server-note-breakpoint-state-changed(project, breakpoint, state);
    if (application)
      server-note-breakpoint-state-changed(application, breakpoint, state,
                                           use-project-proxy: proxy);
    end if
  end for
*/
end method note-breakpoint-state-changed;


/// WITH-COMPRESSED-BREAKPOINT-STATE-CHANGES (environment-protocols)

/// These three thread variables are "(internal)".
define thread variable *compress-breakpoint-state-changes* :: <boolean> = #f;
define thread variable *last-breakpoint-state-change-state* :: false-or(<breakpoint-state>) = #f;
define thread variable *last-breakpoint-state-change-project* :: false-or(<project-object>) = #f;

define inline function do-with-compressed-breakpoint-state-changes
    (continuation :: <function>) => ()
  dynamic-bind (*compress-breakpoint-state-changes* = #t)
    continuation();
  end;
  if (*last-breakpoint-state-change-project*
        & *last-breakpoint-state-change-state*)
    broadcast($project-channel,
              make(<all-breakpoints-state-change-message>,
                   project: *last-breakpoint-state-change-project*,
                   state: *last-breakpoint-state-change-state*));
  end if;
end function do-with-compressed-breakpoint-state-changes;

define macro with-compressed-breakpoint-state-changes
  { with-compressed-breakpoint-state-changes () ?body:body end }
  => { do-with-compressed-breakpoint-state-changes(method () ?body end) }
end macro;


/// NOTE-BREAKPOINT-STATE-CHANGES-FAILED (environment-protocols)

define open generic note-breakpoint-state-changes-failed
    (server :: <server>, breakpoints :: <sequence>,
     state :: <breakpoint-state>) => ();

define method note-breakpoint-state-changes-failed
    (project :: <project-object>, breakpoints :: <sequence>,
     state :: <breakpoint-state>) => ()
  broadcast($project-channel,
            make(<breakpoint-state-changes-failed-message>,
                 project: project,
                 breakpoints: breakpoints,
                 state: state));
end method note-breakpoint-state-changes-failed;


/// SERVER-NOTE-BREAKPOINT-STATE-CHANGED (environment-protocols)

define open generic server-note-breakpoint-state-changed
    (server :: <server>, breakpoint :: <breakpoint-object>,
     state :: <breakpoint-state>, #key use-project-proxy)
  => ();

define method server-note-breakpoint-state-changed
    (project :: <project-object>, breakpoint :: <breakpoint-object>,
     state :: <breakpoint-state>, #key use-project-proxy = #f)
 => ()
  if (*compress-breakpoint-state-changes*)
    *last-breakpoint-state-change-project* := project;
    *last-breakpoint-state-change-state* := state;
  else
    broadcast($project-channel, make(<single-breakpoint-state-change-message>,
                                     project: project,
                                     breakpoint: breakpoint,
                                     state: state));
  end if;
end method server-note-breakpoint-state-changed;


/// DO-GENERIC-BREAKPOINT-METHODS (environment-protocol)
///
/// NB Only changes the state of those breakpoints that still exist.
/// Does not make any new breakpoints to fill in the gaps made by the
/// user explicitly destroying them (or by hits on transients).

define open generic do-generic-breakpoint-methods
    (operation :: <function>,
     breakpoint :: <generic-function-breakpoint-object>)
 => ();

define method do-generic-breakpoint-methods
    (operation :: <function>,
     breakpoint :: <generic-function-breakpoint-object>)
 => ()
  let project = breakpoint.breakpoint-project;
  let function = breakpoint.breakpoint-object;
  for (m in generic-function-object-methods(project, function))
    let breakpoint
      = element(project.environment-object-breakpoints, m, default: #f);
    if (breakpoint)
      operation(breakpoint);
    end if;
  end for;
end method do-generic-breakpoint-methods;

/// ENVIRONMENT-OBJECT-SOURCE-LOCATION{-SETTER}

define method environment-object-source-location
    (project :: <project-object>,
     breakpoint :: <source-location-breakpoint-object>)
 => (location :: <source-location>)
  breakpoint-object(breakpoint)
end method environment-object-source-location;

define method environment-object-source-location
    (project :: <project-object>,
     breakpoint :: <function-breakpoint-object>)
 => (location :: <source-location>)
  environment-object-source-location(project, breakpoint-object(breakpoint))
end method environment-object-source-location;


/// Tracing

define function trace-function
    (project :: <project-object>, function :: <function-object>)
 => (breakpoint :: <function-breakpoint-object>)
  make(<breakpoint-object>,
       project:      project,
       object:       function,
       stop?:        #f,
       entry-point?: #t)
end function trace-function;
