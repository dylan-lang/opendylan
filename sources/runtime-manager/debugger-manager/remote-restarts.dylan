module:    dm-internals
synopsis:  Modelling and signalling remote restarts on threads
author:    Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <REMOTE-RESTART>
//    Models an available restart within the application.

define class <remote-restart> (<object>)

  slot remote-restart-formatted-description :: <string>,
    init-value: "Undescribed Restart",
    init-keyword: description:;

  slot remote-restart-string-formatted? :: <boolean>,
    init-value: #f,
    init-keyword: formatted?:;

  constant slot remote-restart-abort? :: <boolean>,
    init-value: #f,
    init-keyword: abort?:;

  constant slot remote-restart-function :: <remote-value>,
    required-init-keyword: function:;

  constant slot remote-restart-test :: <remote-value>,
    required-init-keyword: test:;

  constant slot remote-restart-type :: <remote-value>,
    required-init-keyword: type:;

  constant slot remote-restart-init-args :: <vector>,
    required-init-keyword: init-args:;

  constant slot remote-restart-format-args :: false-or(<vector>),
    init-value: #f,
    init-keyword: format-args:;

  constant slot remote-restart-format-string :: false-or(<remote-value>),
    init-value: #f,
    init-keyword: format-string:;

  constant slot remote-restart-debug-target :: <debug-target>,
    required-init-keyword: target:;

  constant slot remote-restart-index :: <integer>,
    required-init-keyword: index:;

end class;


///// REMOTE-RESTART-DESCRIPTION
//    Generates a formatted description of a restart.

define method remote-restart-description
    (remote-restart :: <remote-restart>) => (str :: <string>)
  if (remote-restart.remote-restart-string-formatted?)
    remote-restart.remote-restart-formatted-description
  elseif (~remote-restart.remote-restart-format-string)
    remote-restart.remote-restart-formatted-description
  else
    remote-restart.remote-restart-formatted-description :=
         apply(remote-format-to-string,
               remote-restart.remote-restart-debug-target,
               dylan-string-data
                  (remote-restart.remote-restart-debug-target,
                   remote-restart.remote-restart-format-string),
               remote-restart.remote-restart-format-args);
    remote-restart.remote-restart-string-formatted? := #t;
    remote-restart.remote-restart-formatted-description
  end if
end method;


///// AVAILABLE-RESTARTS-FOR-THREAD
//    Returns a sequence of <remote-restart> objects for the available
//    restarts on the thread within this debugger transaction.

define method available-restarts-for-thread
    (application :: <debug-target>, thread :: <remote-thread>)
      => (restarts :: <sequence>)
  let dm-thread = find-thread(application, thread);
  if (~application.dylan-application?)
    #[]
  elseif (dm-thread.cached-restarts)
    dm-thread.cached-restarts
  elseif (dm-thread.thread-pause-state-description == #"random-location")
    // We must not attempt to find restarts for a thread whose
    // state is unknown, since it will be unsafe to try and signal it.
    #[]
  elseif (~application.static-object-directory.directory-keywords-initialized?)
    // When looking for restarts, we need to find their descriptions, for
    // which we need the addresses of the #"format-string" and
    // #"format-arguments" keywords in the runtime. If these have not yet
    // been found, then the application has not proceeded far enough through
    // its initializations to look for restarts.
    #[]
  else
    use-thread-for-spy-functions(application, thread);
    let path = application.debug-target-access-path;
    let i = 1;

    let restart-type =
      lookup-static-object(application, "<restart>", "dylan");
    let abort-type =
      lookup-static-object(application, "<abort>", "dylan");

    let handlers =
      thread-current-active-handlers
        (application, thread);

    // Now go through all the handlers and try to find those for
    // <simple-restart>. Make a local proxy for each one and store them in
    // a stretchy vector.

    let remote-restarts = make(<stretchy-vector>, size: 0);
    for (this-handler in handlers)
      let handler-type = dylan-handler-type(application, this-handler);
      if (remote-subclass?(application, 
                           handler-type,
                           restart-type))
        let is-abort =
          remote-subclass?(application, handler-type, abort-type);
        let this-args-vector = 
          dylan-handler-init-arguments(application, this-handler);
        let this-format-string =
          get-keyword-value-from-vector(application, 
                                        this-args-vector,
                                        #"format-string");
        let this-format-vector =
          get-keyword-value-from-vector(application,
                                        this-args-vector,
                                        #"format-arguments");
        let this-test =
          dylan-handler-test(application, this-handler);
        let this-function =
          dylan-handler-function(application, this-handler);
        let this-restart =
          make(<remote-restart>, target: application,
               function: this-function,
               type: handler-type,
               init-args: 
                 canonicalize-sequence(application, this-args-vector),
               format-args:
                 if (this-format-vector)
                   canonicalize-sequence(application, this-format-vector)
                 else
                   #[]
                 end if,
               format-string: this-format-string,
               abort?: is-abort,
               test: this-test,
               index: i);
        i := i + 1;
        add!(remote-restarts, this-restart);
        ignore(this-restart.remote-restart-function);
        ignore(this-restart.remote-restart-test);
        ignore(this-restart.remote-restart-type);
        ignore(this-restart.remote-restart-init-args);
      end if;
    end for;
    dm-thread.cached-restarts := remote-restarts;
    remote-restarts;
  end if;
end method;


///// SIGNAL-RESTART-ON-THREAD
//    Instructs a thread to invoke a restart as soon as the application is
//    allowed to resume.
//    TODO: This is an utterly pants implementation. Far better would be
//          to introduce some spy functionality.

define method signal-restart-on-thread
    (application :: <debug-target>, thread :: <remote-thread>,
     rst :: <remote-restart>)
       => ()

  use-thread-for-spy-functions(application, thread);
  let dylan-runtime = application.application-dylan-runtime-library;
  let dylan-library = application.application-dylan-library;

  let dylan-invoker = 
    find-symbol(application.debug-target-access-path,
                "call_dylan_function",
                library: dylan-runtime);
  let restart-invoker =
    resolve-dylan-name(application, "spy-invoke-numbered-restart",
                       $dylan-internal,
                       indirect?: #t,
                       library: dylan-library);
  if (dylan-invoker & restart-invoker)
    let (ret-addr, cookie)
      = call-debugger-function
          (application,
           remote-call,
           application.debug-target-access-path, 
           thread,
           dylan-invoker.remote-symbol-address,
           restart-invoker,
           as-remote-value(1),
           integer-as-tagged-remote-value(rst.remote-restart-index));
  end if;
end method;
