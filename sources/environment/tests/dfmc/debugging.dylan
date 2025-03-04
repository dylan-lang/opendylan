Module:    dfmc-environment-test-suite
Synopsis:  DFMC Environment Tests
Author:    Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $debugging-test-application = "cmu-test-suite";
define constant $debugging-library-id
  = make(<library-id>, name: $debugging-test-application);
define constant $debugging-module-id
  = make(<module-id>, name: "dylan-test", library: $debugging-library-id);

define variable *test-application-application* :: false-or(<application>) = #f;

define variable *transaction-id* = #f;

define function dbg-machine () => (machine :: <machine>);
  let network-address = environment-variable("OPEN_DYLAN_DEBUGGING_MACHINE");
  if (network-address)
    corba/orb-init(make(corba/<arg-list>), "Open Dylan ORB");
    let password
      = environment-variable("OPEN_DYLAN_DEBUGGING_PASSWORD")
      | error ("Environment variable OPEN_DYLAN_DEBUGGING_PASSWORD is not set");
    make(<machine>,
         network-address: network-address,
         password: password)
  else
    environment-host-machine()
  end if
end function;

define constant $project-message-queue = make(<deque>);
define constant $project-message-notification
  = make(<notification>, lock: make(<lock>));

define function debugging-project-message-callback (message :: <project-message>)
  with-lock (associated-lock($project-message-notification))
    push-last($project-message-queue, message);
    release($project-message-notification);
  end with-lock;
end function;

define method note-application-interactive-results
    (context :: <object>, thread :: <thread-object>, transaction-id)
 => ()
  *transaction-id* := transaction-id;
end method note-application-interactive-results;

define function clear-project-messages ()
  with-lock (associated-lock($project-message-notification))
    $project-message-queue.size := 0;
  end with-lock;
end function;

define function await-project-message () => (message :: <project-message>);
  with-lock (associated-lock($project-message-notification))
    while (empty?($project-message-queue))
      wait-for($project-message-notification);
    end while;
    pop($project-message-queue)
  end with-lock
end function;

define function initialize-application-client
    (client :: <object>, application :: <application>) => ()
  register-application-callbacks
    (application,
     initialized-callback:         note-application-initialized,
     interactive-results-callback: note-application-interactive-results);
end function;

define function open-debugging-test-project () => ()
  let application
    = open-project(test-project-location($debugging-test-application));
  open-project-compiler-database
    (application, error-handler: project-condition-handler);

  test-project-build(application, link?: #t);

  unless (open-project-compiler-database
            (application, error-handler: project-condition-handler))
    parse-project-source(application);
  end unless;
  assert(application.project-start-function-name);
  assert(application.project-target-type == #"executable");
  *test-application* := application;

  tune-in($project-channel, debugging-project-message-callback,
          message-type: <project-message>);

  let machine = dbg-machine();
  *test-application-application*
    := run-application(*test-application*,
                       initialize-client: initialize-application-client,
                       startup-option: #"debug",
                       share-console?: #t,
                       machine: machine);

  iterate loop (initialized? = #f, stopped? = #t, transient-bp-count = 0)
    debugger-message("I=%= S=%= BP=%d", initialized?, stopped?, transient-bp-count);
    unless (initialized? & stopped? & zero?(transient-bp-count))
      let message = await-project-message();
      select (message by instance?)
        <run-application-requested-message> =>
          loop(initialized?, stopped?, transient-bp-count);
        <run-application-failed-message> =>
          error("run-application failed for %s",
                message.message-project.project-name);
        <application-initialized-message> =>
          debugger-message("  Application initialized");
          loop(#t, stopped?, transient-bp-count);
        <application-state-changed-message> =>
          let state = *test-application-application*.application-state;
          debugger-message("  Application state is now %s", state);
          if (state == #"running")
            loop(initialized?, #f, transient-bp-count);
          else
            loop(initialized?, #t, transient-bp-count);
          end if;
        <single-breakpoint-state-change-message> =>
          let breakpoint = message.message-breakpoint;
          let state = message.message-breakpoint-state;
          debugger-message
            ("  Breakpoint %s (%=) state is now %s",
             breakpoint,
             environment-object-id(*test-application*, breakpoint),
             state);
          if (breakpoint.breakpoint-transient?)
            select (state)
              #"created" =>
                loop(initialized?, stopped?, transient-bp-count + 1);
              #"destroyed" =>
                loop(initialized?, stopped?, transient-bp-count - 1);
            end select;
          else
            loop(initialized?, stopped?, transient-bp-count);
          end if;
        <application-threads-changed-message> =>
          for (thread in *test-application-application*.application-threads)
            debugger-message
              ("  Thread %s (%s) index %s state %s suspended %=",
               thread,
               environment-object-primitive-name(*test-application*, thread),
               thread-index(*test-application*, thread),
               thread-state(*test-application*, thread),
               thread.thread-suspended?);
          end for;
          loop(initialized?, stopped?, transient-bp-count);
        otherwise =>
          debugger-message("%=", message);
          loop(initialized?, stopped?, transient-bp-count);
      end select;
    end unless;
  end iterate;
end function;

define function close-debugging-test-project () => ()
  close-application(*test-application-application*);
  iterate loop()
    let state = *test-application-application*.application-state;
    debugger-message("closing, state is %s", state);
    if (state ~== #"closed")
      let message = await-project-message();
      debugger-message("%=", message);
      loop();
    end if;
  end iterate;
  tune-out($project-channel, debugging-project-message-callback);
  close-project(*test-application*);
end function;

define test stack-trace-test ()
  for (thread in *test-application-application*.application-threads)
    let thread-name
      = environment-object-primitive-name(*test-application*, thread);
    select (thread-name by \=)
      "Main thread" =>
        let top-frame = first(thread-complete-stack-trace(*test-application*, thread));
        check-equal("Main thread stack frame type",
                    #"dylan-call",
                    stack-frame-type(*test-application*, top-frame));

        let top-function = stack-frame-function(*test-application*, top-frame);
        check-equal("Main thread start function name",
                    "do-tautologies (<sequence>)",
                    environment-object-display-name
                      (*test-application*, top-function, #f,
                       qualify-names?: #f));

        let (location, exact?)
          = stack-frame-source-location(*test-application*, top-frame);
        let sr = source-location-source-record(location);
        check-equal("Main thread start function source record",
                    "run-tests.dylan",
                    source-record-location(sr).locator-name);

        let locals-count =
          stack-frame-local-variable-count(*test-application*, top-frame);
        check-equal("Stack frame has 1 local variable",
                    1, locals-count);
        let locals = stack-frame-local-variables(*test-application*, top-frame);
        let local-name
          = print-environment-object-to-string(*test-application*, locals[0]);
        check-equal("Stack frame local variable name", "tests", local-name);

      "Thread Manager", "Spy Thread" =>
        #f;

      otherwise =>
        #f
    end select;
  end for;
end test;

define test registers-test ()
  let gp-registers
    = application-registers(*test-application*, category: #"general-purpose");
  check-true("Some general-registers are known", gp-registers.size > 0);

  for (register in gp-registers)
    let register-name
      = environment-object-primitive-name(*test-application*, register);
    check-equal("General register can be looked up by name",
                register,
                lookup-register-by-name(*test-application*, register-name));
  end for;

  for (thread in *test-application-application*.application-threads)
    let thread-name
      = environment-object-primitive-name(*test-application*, thread);
    let top-frame
      = first(thread-complete-stack-trace(*test-application*, thread));
    for (register in gp-registers)
      let register-name
        = environment-object-primitive-name(*test-application*, register);
      let contents
        = register-contents(*test-application*, register, thread,
                            stack-frame-context: top-frame);
      check-true(format-to-string("Thread %s top frame register %s readable",
                                  thread-name, register-name),
                 contents);
    end for;
  end for;
end test;

define constant $interactivity-spec
  = #[
      #("1 + 2;", "3"),
      #("3 + 4;", "7"),
      #("$0 + $1;", "10"),
      #("""
           define class <foo> (<object>)
             slot a, init-keyword: a:;
           end class <foo>;
        """),
      #("make(<foo>, a: 10);", "\\{<foo>: \\d{5}\\}"),
      #("$3.a = 10;", "#t"),
      #("""
           define class <bar> (<foo>)
             slot b, init-keyword: b:;
           end class <bar>;
        """),
      #("make(<bar>, a: 20, b: 30);", "\\{<bar>: \\d{5}\\}"),
      #("$5.a = 20;", "#t"),
      #("$5.b = 30;", "#t"),
      #("""
           define function foo (x :: <integer>)
             x + 1
           end;
        """),
      #("foo(5);", "6"),
      #("""
           define function foo (x :: <integer>)
             x + 2
           end;
        """),
      #("foo(5);", "7")];

define test interactivity-test ()
  let module
    = find-module(*test-application*, id-name($debugging-module-id));
  let threads = *test-application-application*.application-threads;
  let main-thread
    = find-element(threads,
                   method (thread :: <thread-object>)
                     environment-object-primitive-name(*test-application*, thread)
                       = "Main thread"
                   end);
  for (item in $interactivity-spec)
    let top-frame
      = first(thread-complete-stack-trace(*test-application*, main-thread));

    *transaction-id* := #f;
    clear-project-messages();

    let code = item.head;
    debugger-message("Evaluate: %s", code);

    let transaction-id
      = project-execute-code(*test-application*, code, main-thread,
                             module: module,
                             stack-frame: top-frame);
    iterate loop()
      let message = await-project-message();

      select (message by instance?)
        <application-state-changed-message> =>
          let state = *test-application-application*.application-state;
          debugger-message("  Application state is now %s", state);
          select (state)
            #"stopped" =>
              #f;
            otherwise =>
              loop();
          end select;

        otherwise =>
          debugger-message("%=", message);
          loop();
      end select;
    end iterate;

    check-equal("Check transaction ID", transaction-id, *transaction-id*);

    let reference-values = item.tail;
    let result-values
      = fetch-interactor-return-values(*test-application*, transaction-id);
    check-equal("Check return value count",
                reference-values.size, result-values.size);
    for (reference :: <string> in reference-values,
         result :: <pair> in result-values)
      let pattern = compile-regex(concatenate("^", reference, "$"));
      let (name :: <string>, value :: <environment-object>)
        = values(result.head, result.tail);
      let value-string
        = print-environment-object-to-string
            (*test-application*, value, namespace: module);
      debugger-message("  %s = %s", name, value-string);
      check-true("Return value", regex-position(pattern, value-string));
    end;
    dispose-interactor-return-values(*test-application*, transaction-id);
  end for;
end test;

define suite dfmc-environment-debugging-suite
    (setup-function:   open-debugging-test-project,
     cleanup-function: close-debugging-test-project,
     when: method ()
             select ($os-name)
               #"win32" =>
                 #t;
               otherwise =>
                 values(#f, "debugging is not yet supported on this platform");
             end select
           end method)
  test stack-trace-test;
  test registers-test;
  test interactivity-test;
end suite;
