Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Thread objects

define class <thread-object> (<application-object>)

  slot thread-runtime-state :: <symbol>,
    init-value: #"running";

  slot thread-suspended? :: <boolean>,
    init-value: #f;

end class <thread-object>;

define open generic thread-stack-trace
    (server :: <server>, thread :: <thread-object>)
 => (top-frame :: <stack-frame-object>);

define open generic thread-complete-stack-trace
    (server :: <server>, thread :: <thread-object>)
 => (all-frames :: <sequence>);

define open generic thread-index
    (server :: <server>, thread :: <thread-object>)
 => (index :: <integer>);

define open generic thread-state
    (server :: <server>, thread :: <thread-object>)
 => (state :: <symbol>);

define open generic create-application-thread
    (server :: <server>, title :: <string>)
 => (thread :: <thread-object>);

define open generic suspend-application-thread
    (server :: <server>, thread :: <thread-object>)
 => ();

define open generic resume-application-thread
    (server :: <server>, thread :: <thread-object>)
 => ();

define open generic thread-current-interactor-level
    (server :: <server>, thread :: <thread-object>)
 => (level :: <integer>);

define open generic add-application-object-to-thread-history
    (server :: <server>, thread :: <thread-object>,
     application-object :: <application-object>)
 => (varname :: false-or(<string>));

define open generic application-default-interactor-thread
    (server :: <server>) => (thread :: false-or(<thread-object>));

/// Stepping (Added by phoward, 29 May 1997, at request of jason)

define open generic step-application-out
    (server :: <server>, thread :: <thread-object>,
     #key stack-frame)
 => ();

define open generic step-application-over
    (server :: <server>, thread :: <thread-object>,
     #key stack-frame)
 => ();

define open generic step-application-into
    (server :: <server>, thread :: <thread-object>)
 => ();

define open generic application-just-hit-dylan-error?
    (server :: <server>, thread :: <thread-object>)
 => (hit? :: <boolean>);

define open generic application-just-hit-error?
    (server :: <server>, thread :: <thread-object>)
 => (hit? :: <boolean>);

define open generic application-just-hit-breakpoint?
    (server :: <server>, thread :: <thread-object>)
 => (hit? :: <boolean>);

define open generic application-just-interacted?
    (server :: <server>, thread :: <thread-object>)
 => (interacted? :: <boolean>);

define open generic application-just-stepped?
    (server :: <server>, thread :: <thread-object>)
 => (stepped? :: <boolean>);

/// CURRENT-STOP-BREAKPOINTS

define open generic current-stop-breakpoints 
    (server :: <server>, thread :: <thread-object>)
 => (breakpoints :: <sequence>);

define method current-stop-breakpoints 
    (project :: <project-object>, thread :: <thread-object>)
 => (breakpoints :: <sequence>)
  let server = choose-server(project, thread);
  if (server)
    current-stop-breakpoints(server, thread);
  else
    //---*** andrewa: this used to return #f which was a type error,
    //---*** is it better to return the empty sequence or to change
    //---*** the return type?
    #()
  end if;
end method;


/// Implementation

// Override the default caching method, because the name can change
// depending on when it is accessed (it can appear as a foreign thread).

define method environment-object-primitive-name
    (server :: <server>, object :: <thread-object>)
  => (name :: <string>)
  get-environment-object-primitive-name(server, object)
end method environment-object-primitive-name;

define method environment-object-type-name
    (object :: <thread-object>) => (label :: <string>)
  "Application thread"
end method environment-object-type-name;

define method thread-stack-trace
    (project :: <project-object>, thread :: <thread-object>)
 => (top-frame :: <stack-frame-object>)
  let server = choose-server(project, thread, error?: #t);
  thread-stack-trace(server, thread)
end method thread-stack-trace;

define method thread-complete-stack-trace
    (project :: <project-object>, thread :: <thread-object>)
 => (stack :: <sequence>)
  let server = choose-server(project, thread);
  if (server)

    // Dispatch to the server to get the sequence of frames.

    let stack = thread-complete-stack-trace(server, thread);

    // Stack frame environment objects have cache slots that enable them to
    // reference each other. These are filled in lazily, except when this
    // method is called to get a complete stack trace all at once.
    // So, in this case, we have to fill all the cache slots in now.
    // Find out how many frames there are, and hence the indices in the
    // sequence of the first (top) frame, and the last (bottom) frame.

    let count = size(stack);
    let last = count - 1;
    stack[0].stack-frame-top? := #t;
    stack[last].stack-frame-bottom? := #t;

    // The stack must have at least one frame, but may conceivably
    // (though this will probably never happen in real life) contain
    // _only_ one frame, in which case 'last' is equal to zero. That's
    // the reason for having these two 'unless' clauses. The bodies of
    // these should almost always get executed, but it should also be
    // impossible for an array-boundary error to occur.

    unless (stack[0].stack-frame-bottom?)
      stack[0].previous-frame-cache-slot := stack[1]
    end unless;
    unless (stack[last].stack-frame-top?)
      stack[last].next-frame-cache-slot := stack[last - 1]
    end unless;

    // Now go through whatever frames might exist between the "top" and
    // "bottom" frames, linking them together via their cache slots.

    for (i from 1 below last)
      stack[i].previous-frame-cache-slot := stack[i + 1];
      stack[i].next-frame-cache-slot := stack[i - 1];
    end for;

    // And return the sequence
    stack
  else
    #()
  end if
end method;

define method thread-index
    (project :: <project-object>, thread :: <thread-object>)
 => (index :: <integer>)
  let server = choose-server(project, thread, error?: #t);
  thread-index(server, thread)
end method;

define method thread-state
    (project :: <project-object>, thread :: <thread-object>)
 => (state :: <symbol>)
  let server = choose-server(project, thread);
  if (server)
    thread-state(server, thread)
  else
    #"obsolete"
  end if
end method;

define method create-application-thread
    (project :: <project-object>, title :: <string>)
 => (thread :: <thread-object>)
  let application = project.project-application;
  if (application)
    create-application-thread(application, title)
  else
    error(make(<closed-server-error>,
	       format-string: "Attempting to create thread from closed application"))
  end
end method;

define method suspend-application-thread
    (project :: <project-object>, thread :: <thread-object>)
 => ()
  let server = choose-server(project, thread, error?: #t);
  suspend-application-thread(server, thread)
end method;

define method resume-application-thread
    (project :: <project-object>, thread :: <thread-object>)
 => ()
  let server = choose-server(project, thread, error?: #t);
  resume-application-thread(server, thread)
end method;


define method step-application-out
    (project :: <project-object>, thread :: <thread-object>,
     #key stack-frame = #f)
 => ()
  let server = choose-server(project, thread);
  if (server)
    step-application-out(server, thread, stack-frame: stack-frame)
  end if
end method;

define method step-application-over
    (project :: <project-object>, thread :: <thread-object>,
     #key stack-frame = #f)
       => ()
  let server = choose-server(project, thread);
  if (server)
    step-application-over(server, thread, stack-frame: stack-frame)
  end if
end method;

define method step-application-into
    (project :: <project-object>, thread :: <thread-object>)
 => ()
  let server = choose-server(project, thread);
  if (server)
    step-application-into(server, thread)
  end if
end method;

define method application-just-hit-dylan-error?
    (project :: <project-object>, thread :: <thread-object>)
 => (dylan-error? :: <boolean>)
  let server = choose-server(project, thread);
  server & application-just-hit-dylan-error?(server, thread);
end method;

define method application-just-hit-error?
    (project :: <project-object>, thread :: <thread-object>)
 => (error? :: <boolean>)
  let server = choose-server(project, thread);
  server & application-just-hit-error?(server, thread);
end method;

define method application-just-hit-breakpoint?
    (project :: <project-object>, thread :: <thread-object>)
 => (breakpoint? :: <boolean>)
  let server = choose-server(project, thread);
  server & application-just-hit-breakpoint?(server, thread);
end method;

define method application-just-interacted?
    (project :: <project-object>, thread :: <thread-object>)
 => (interacted? :: <boolean>)
  let server = choose-server(project, thread);
  server & application-just-interacted?(server, thread);
end method;

define method application-just-stepped?
    (project :: <project-object>, thread :: <thread-object>)
 => (stepped? :: <boolean>)
  let server = choose-server(project, thread);
  server & application-just-stepped?(server, thread);
end method;

define method thread-current-interactor-level
    (project :: <project-object>, thread :: <thread-object>)
 => (level :: <integer>)
  let server = choose-server(project, thread);
  if (server)
    thread-current-interactor-level(server, thread)
  else
    0
  end if
end method;

define method add-application-object-to-thread-history
    (project :: <project-object>, thread :: <thread-object>,
     application-object :: <application-object>)
 => (varname :: false-or(<string>))
  let server = choose-server(project, thread);
  server & 
    add-application-object-to-thread-history
      (server, thread, application-object)
end method;

define method application-default-interactor-thread
    (project :: <project-object>) => (thread :: false-or(<thread-object>))
  let application = project.project-application;
  application & application.application-default-interactor-thread
end method;
