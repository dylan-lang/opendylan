Module:    dfmc-application
Synopsis:  concrete application server implementation
Author:    Bill Chiles, Jason Trenouth, Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

///// <APPLICATION-PROXY-FACTORY>
//    
//    The top level of this data structure is a string-table that maps
//    library names to a second level of string-table. This table maps
//    module names to a _third_ level of string-table. This final level
//    maps binding names to pairs of the form
//    #(<BINDING-PROXY>, <STATIC-DYLAN-RUNTIME-PROXY>) for constants or
//    #(<BINDING-PROXY>, <APPLICATION-VARIABLE>) for variables.
//
//    At the top level of the data structure, these pairs are also held
//    in a sequence of ordered data.

define class <application-proxy-factory> (<object>)

   slot static-proxies :: <page-relative-object-table>;
   slot static-address-proxies :: <page-relative-object-table>;
   slot per-transaction-proxies :: <page-relative-object-table>;
   constant slot proxy-factory-known-names :: <string-table> 
     = make(<string-table>);
   constant slot proxy-factory-ordered-data :: <stretchy-vector>
     = make(<stretchy-vector>);
   constant slot proxy-factory-proxy-to-id-mappings :: <table>
     = make(<table>);
   constant slot proxy-factory-last-object-exchanged-for-class :: <pair>
     = pair(#f, #f);
end class;


/// Implementation class for abstract <application>.
///

/// <dfmc-application> -- Concrete Class for Exported Abstract Class
///
/// Andy decided we would export an identifier for this class, and he wants
/// all the actual implementation slots in a distinct kind of object that
/// <dfmc-application> points to.  <target-application> is defined below.
///
/// Don't mistake this name.  This class should have no dependencies on DFMC
/// since this code is written in terms of the Debugger-manager.
///
define sealed class <dfmc-application> (<application>)

  //
  // This holds the <debug-target> that the debugger needs to talk to the
  // target application.  When users of the Environment-protocol library
  // make calls on an application server (that is, <application>), those
  // calls are implemented by regarding application-debug-target.
  //
  // There is no portable way to access this application handle, as there is
  // with the application's project, so this detail of the implementation
  // has no ramifications in the user model.

  slot application-target-app :: false-or(<target-application>) = #f;

  slot application-stop-reason :: false-or(<stop-reason>) = #f;

  // phoward added the following slots

  slot application-tether-status :: <application-startup-option> = #"start";

  slot pause-before-termination-flag :: <boolean> = #f;

  // Whenever the application enters a debugger transaction due to
  // one or more breakpoints, this slot holds the list of
  // <breakpoint-object> instances.

  constant slot application-signalling-breakpoints :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  constant slot application-function-breakpoint-values :: <object-table>
    = make(<object-table>);

  // This is the proxy factory, which deals with how environment objects
  // and proxies are exchanged.

  slot application-proxy-factory :: <application-proxy-factory>;

  // When the interactor returns, the a mapping from the transaction
  // ID to the sequence of return values is installed in this table.
  // The value in the table is a pair, with the head of the pair being
  // a history variable name, and the tail being the actual <remote-value>
  // result.

  constant slot interactor-results-table :: <table> = make(<table>);

  // USER OBJECT MODELING
  // This is a stretchy sequence of <pair>s. The head of each pair is
  // a <remote-value> representing a statically-defined class object in
  // the runtime. The tail is an environment-side subclass of
  // <user-object> that should be used (in preference to <user-object>
  // itself) to model runtime instances that are members of the
  // associated runtime class.
  // The order in this sequence is important, since there may be subclass
  // relationships between the various classes. Runtime instances are
  // tested against each class in turn, and the first match is the
  // class that will be used. If there are no matches, then <user-object>
  // will be used.

  slot runtime-class-user-class-mappings :: <stretchy-vector> 
     = make(<stretchy-vector>);

  slot runtime-class-user-class-mappings-initialized? :: <boolean> = #f;

  // CALLBACKS
  // When the <dfmc-application> finally gets an associated
  // <target-application>, the DM's manage-running-application will be
  // called. Although the actual callbacks will be written internally,
  // they will delegate to callbacks that must be registered
  // (via environment protocols) with the project.

  slot registered-stop-reason-callback :: false-or(<function>) = #f;
  slot registered-debugger-transaction-prolog :: false-or(<function>) = #f;
  slot registered-debugger-transaction-epilog :: false-or(<function>) = #f;
  slot registered-interactor-handler :: false-or(<function>) = #f;
  slot registered-library-init-handler :: false-or(<function>) = #f;

  //slot instruction-step-flag :: <boolean> = #f;

  // All callback registrations are atomic...

  constant slot callback-registration-lock :: <simple-lock> 
     = make(<simple-lock>);

  // This table maps <remote-thread>s to <application-thread-state>
  // model objects.

  constant slot application-thread-state-model :: <object-table> = make(<object-table>);

  // Finally, an incremental integer counter that ticks whenever
  // a thread is created in the application. This is used when
  // assigning titles for threads. Starts at 1 so that the first
  // thread is titled "Thread 1" rather than "Thread 0".

  slot application-thread-counter :: <integer> = 1;
  slot evaluator-thread-counter :: <integer> = 1;

  // The Dylan Thread Manager -- an application-dedicated spy-spawned,
  // spy-spawning thread
  // Exclusively reserved for thread-spawning in an application being
  // debugged at any point in the application life cycle

  slot dylan-thread-manager :: false-or(<remote-thread>) = #f;

  // slot dylan-open-interactor :: false-or(<remote-thread>) = #f;

  // slot start-debugging-thread :: <thread>;

  // The following table records failures to resolve/set/clear break-
  // points during a debugger transaction. The key is a state
  // change (an instance of <breakpoint-state>), and the value is
  // a sequence of breakpoints that could not be put into that state.

  constant slot breakpoint-state-change-failures :: <table> = make(<table>);

  // The following slot holds a growable vector of all project
  // proxies that have been interacted against while the runtime
  // has been connected. When the runtime is closed down, all
  // entries in this vector must be notified using
  // PROJECT-CURRENT-DEBUG-TARGET-SETTER with #f.

  // This slot has no init-value, because RUN-APPLICATION always 
  // puts a freshly allocated stretchy-vector into it.

  slot interactor-contexts-used :: <stretchy-vector>;

  slot application-initial-breakpoint :: false-or(<breakpoint-object>) = #f;
  slot application-loaded-dylan-library? :: <boolean> = #f;
  slot application-initialized-interactive-threads? :: <boolean> = #f;
  slot application-just-initialized? :: <boolean> = #f;
  slot application-just-finished-execution? :: <boolean> = #f;
  slot application-reached-interaction-point? :: <boolean> = #f;

  /// Callbacks
  slot application-initialized-callback :: false-or(<function>) = #f;
  slot application-process-created-callback :: false-or(<function>) = #f;
  slot application-debugging-callback :: false-or(<function>) = #f;
  slot application-thread-message-callback :: false-or(<function>) = #f;
  slot application-threads-changed-callback :: false-or(<function>) = #f;
  slot application-thread-finished-callback :: false-or(<function>) = #f;
  slot application-started-interaction-callback :: false-or(<function>) = #f;
  slot application-just-interacted-callback :: false-or(<function>) = #f;
  slot application-interactive-results-callback :: false-or(<function>) = #f;
  slot application-finished-execution-callback :: false-or(<function>) = #f;
  slot application-process-finished-callback :: false-or(<function>) = #f;

  /// Messages
  constant slot application-thread-message-depths :: <object-table>
    = make(<object-table>);

  /// Profiling
  constant slot application-profile-state :: <dfmc-application-profile-state>
    = make(<dfmc-application-profile-state>);
end class;


/// INITIALIZE
///
/// Inform interested parties the application has been made and initialized.

define method initialize (application :: <dfmc-application>, #key)
  next-method();
  register-debugger-manager-callbacks(application);
  let project :: <project-object> = application.server-project;
  project.project-application := application; // NB lace up project 
                                              // and application before 
                                              // broadcast
  broadcast($project-channel, #"make-application", project);
end method;

define method reset-application (application :: <dfmc-application>) => ()
  remove-all-keys!(application.application-thread-message-depths)
end method reset-application;


define method register-application-callbacks
    (application :: <dfmc-application>,
     #key initialized-callback :: false-or(<function>),
          process-created-callback :: false-or(<function>),
          debugging-callback :: false-or(<function>),
          thread-message-callback :: false-or(<function>),
          threads-changed-callback :: false-or(<function>),
          thread-finished-callback :: false-or(<function>),
          started-interaction-callback :: false-or(<function>),
          just-interacted-callback :: false-or(<function>),
          interactive-results-callback :: false-or(<function>),
          finished-execution-callback :: false-or(<function>),
          process-finished-callback :: false-or(<function>))
 => ()
  application.application-initialized-callback
    := initialized-callback;
  application.application-process-created-callback
    := process-created-callback;
  application.application-debugging-callback
    := debugging-callback;
  application.application-thread-message-callback
    := thread-message-callback;
  application.application-threads-changed-callback
    := threads-changed-callback;
  application.application-thread-finished-callback
    := thread-finished-callback;
  application.application-started-interaction-callback
    := started-interaction-callback;
  application.application-just-interacted-callback
    := just-interacted-callback;
  application.application-interactive-results-callback
    := interactive-results-callback;
  application.application-finished-execution-callback
    := finished-execution-callback;
  application.application-process-finished-callback
    := process-finished-callback;
end method register-application-callbacks;

define method invoke-application-callback
    (application :: <dfmc-application>, slot :: <function>, #rest args) => ()
  let callback :: false-or(<function>) = application.slot;
  callback & apply(callback, application.application-client, args)
end method invoke-application-callback;


///// APPLICATION-DEBUG? (Environment Protocol Method)
//    Indicates whether the application is running for debugging purposes.
//    TODO: Remove in favour of APPLICATION-STARTUP-OPTION.
//          This method has been retained purely for backwards
//          compatability.

define method application-debug?
    (application :: <dfmc-application>) => (debug? :: <boolean>)
  application.application-tether-status ~== #"start"
end method;

define method application-just-stepped?
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (just-stepped? :: <boolean>)
  let stop-reason = application.application-stop-reason;
  instance?(stop-reason, <source-step-stop-reason>)
    & stop-reason-thread-object(application, stop-reason) == thread
end method application-just-stepped?;

define method application-stopped-thread
    (application :: <dfmc-application>)
 => (thread :: false-or(<thread-object>))
  let stop-reason = application.application-stop-reason;
  stop-reason & stop-reason-thread-object(application, stop-reason)
end method application-stopped-thread;


///// APPLICATION-PAUSE-BEFORE-TERMINATION? (Environment Protocol Method)
//    Indicates whether the application should pause with user
//    notification when it reaches the natural end of its execution.
//    The environment does this for console applications in order to
//    prevent the final console output from disappearing before its
//    contents can really be examined.

define method application-pause-before-termination?
    (application :: <dfmc-application>) => (pause? :: <boolean>)
  application.pause-before-termination-flag
end method application-pause-before-termination?;


///// APPLICATION-STARTUP-OPTION (Environment Protocol Method)
//    Indicates how the application was launched.

define method application-startup-option
    (application :: <dfmc-application>) 
 => (opt :: <application-startup-option>)
  application.application-tether-status
end method application-startup-option;


/// GET-ENVIRONMENT-OBJECT-PRIMITIVE-NAME (Environment Protocol Method)
/// 
/// A name for the application itself.

define method get-environment-object-primitive-name
    (server :: <server>, application :: <dfmc-application>)
 => (name :: <string>)
  as(<string>, application.application-filename)
end method get-environment-object-primitive-name;


///// PERFORM-APPLICATION-TRANSACTION (Environment Protocol Method)

define method perform-application-transaction
    (application :: <dfmc-application>, function :: <function>)
 => (#rest values)
  let target = application.application-target-app;
  if (target)
    perform-debugger-transaction(target, function)
  else
    function()
  end
end method perform-application-transaction;


///// APPLICATION-JUST-INTERACTED? (Environment Protocol Method)
//    Returns #t iff the current debugger transaction was brought about
//    (at least in part) by the return of an interaction.

define method application-just-interacted?
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (well? :: <boolean>)
  application.application-target-app.dm-application-just-interacted?;
end method;

///// APPLICATION ERROR PROTOCOLS

define method application-just-hit-dylan-error?
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (dylan-error? :: <boolean>)
  let stop-reason = application.application-stop-reason;
  instance?(stop-reason, <dylan-invoke-debugger-stop-reason>)
end method application-just-hit-dylan-error?;

define method application-just-hit-error?
    (application :: <dfmc-application>, thread :: <thread-object>)
 => (dylan-error? :: <boolean>)
  let stop-reason = application.application-stop-reason;
  instance?(stop-reason, <dylan-invoke-debugger-stop-reason>)
    | (instance?(stop-reason, <exception-stop-reason>)
	 & ~instance?(stop-reason, <invoke-debugger-stop-reason>))
end method application-just-hit-error?;

