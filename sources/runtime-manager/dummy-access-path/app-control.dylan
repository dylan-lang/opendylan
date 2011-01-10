module:     access-path-implementation
synopsis:   Functions for debugee control
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $legal-application-states = #[#"running",
                                              #"unstarted",
                                              #"stopped",
                                              #"dead",
                                              #"post-mortem"];

define method set-application-state (ap :: <access-path>, state :: <symbol>)

       if (member? (state, $legal-application-states))
          ap.state := state;
       else
          error ("Attempted to set an illegal application state.");
       end if

end method;


///// Generic functions.


define generic restart (ap :: <access-path>) => ();

define generic stop (ap :: <access-path>) => ();

define generic continue (ap :: <access-path>) => ();

define generic continue-unhandled (ap :: <access-path>) => ();

define generic suspend-thread
             (ap :: <access-path>, thread :: <remote-thread>) => ();

define generic resume-thread
             (ap :: <access-path>, thread :: <remote-thread>) => ();

define generic step (ap :: <access-path>, n :: <integer>) => ();

define generic step-over (ap :: <access-path>, n :: <integer>) => ();

define generic step-out (ap :: <access-path>) => ();

define generic application-state-running? (ap :: <access-path>)
                                           => (_ :: <boolean>);

define generic application-state-stopped? (ap :: <access-path>)
                                           => (_ :: <boolean>);

define generic application-state-unstarted? (ap :: <access-path>)
                                             => (_ :: <boolean>);

define generic application-state-dead? (ap :: <access-path>)
                                        => (_ :: <boolean>);

define generic application-state-post-mortem? (ap :: <access-path>)
                                               => (_ :: <boolean>);

define generic kill-application (ap :: <access-path>,
                                 #key do-cleanups? = #f) => ();


///// RESTART

// This operation is always legal.

define method restart (ap :: <access-path>) => ()
  restart-application (ap.connection);
  set-application-state (ap, #"running");
end method;

define method restart-application (conn :: <local-access-connection>) => ()
  reset-runtime(conn.process);
end method;


///// STOP

// This operation is only legal if the application is running. This performs
// an immediate "stamp on the brakes".

define method stop (ap :: <access-path>) => ()
  if (application-state-running? (ap))
    stop-application (ap.connection);
    set-application-state (ap, #"stopped");
  else
    error ("Attempted to stop a non-running application.");
  end if
end method;

define method stop-application (conn :: <local-access-connection>) => ()
end method;


///// CONTINUE

define method continue (ap :: <access-path>) => ()
  if (application-state-stopped? (ap))
    continue-application (ap.connection);
    do-threads(method (t :: <remote-thread>)
                 t.stack := #f
               end method,
               ap);
    set-application-state (ap, #"running");
  else
     error ("Attempted to continue a non-stopped application.");
  end if
end method;

define method continue-application (conn :: <local-access-connection-32>) => ()
end method;


///// CONTINUE-UNHANDLED

define method continue-unhandled (ap :: <access-path>) => ()
  if (application-state-stopped? (ap))
    continue-application (ap.connection);
    do-threads(method (t :: <remote-thread>)
                 t.stack := #f
               end method,
               ap);
    set-application-state (ap, #"running");
  else
     error ("Attempted to continue a non-stopped application.");
  end if
end method;


///// SUSPEND-THREAD

define method suspend-thread
             (ap :: <access-path>, thread :: <remote-thread>) => ()
  if (~thread-suspended? (thread))
    thread-suspended?(thread) := #t;
    thread-state(thread) := "Suspended by debugger";
    suspend-application-thread (ap.connection, thread);
  end if
end method;

define method suspend-application-thread 
            (conn :: <local-access-connection>, thread :: <remote-thread>)
  thread.nub-descriptor.suspend-count
    := thread.nub-descriptor.suspend-count - 1;
end method;


///// RESUME-THREAD


define method resume-thread
             (ap :: <access-path>, thread :: <remote-thread>) => ()

  if (thread-suspended? (thread))
    thread-suspended?(thread) := #f;
    thread-state(thread) := "[Can't get thread state]";
    resume-application-thread (ap.connection, thread);
  end if
end method;

define method resume-application-thread 
            (conn :: <local-access-connection>, thread :: <remote-thread>)
  thread.nub-descriptor.suspend-count
    := thread.nub-descriptor.suspend-count + 1;
end method;


///// STEP

define method step (ap :: <access-path>, n :: <integer>) => ()
  if (application-state-stopped? (ap))
    step-application (ap.connection);
    set-application-state (ap, #"running");
  else
    error ("Tried to step a non-stopped application.");
  end if
end method;

define method step-application (conn :: <local-access-connection-32>,
                                n :: <integer>) => ()
end method;


///// STEP-OVER

define method step-over (ap :: <access-path>, n :: <integer>) => ()
  if (application-state-stopped? (ap))
    step-over-application (ap.connection, n);
    set-application-state (ap, #"running");
  else
    error ("Tried to step a non-stopped application.");
  end if
end method;

define method step-over-application (conn :: <local-access-connection-32>,
                                     n :: <integer>) => ()
end method;


///// STEP-OUT


define method step-out (ap :: <access-path>) => ()
  if (application-state-stopped? (ap))
    step-out-application (ap.connection);
    set-application-state (ap, #"running");
  else
    error ("Tried to step a non-stopped application.");
  end if
end method;

define method step-out-application (conn :: <local-access-connection-32>) => ()
end method;


///// APPLICATION-STATE-...?

define method application-state-running? 
    (ap :: <access-path>) => (_ :: <boolean>)
  ap.state == #"running"
end method;


define method application-state-stopped? 
    (ap :: <access-path>) => (_ :: <boolean>)
  ap.state == #"stopped"
end method;


define method application-state-unstarted? 
    (ap :: <access-path>) => (_ :: <boolean>)
  ap.state == #"unstarted"
end method;


define method application-state-dead? (ap :: <access-path>) => (_ :: <boolean>)
  ap.state == #"dead"
end method;


define method application-state-post-mortem? 
    (ap :: <access-path>) => (_ :: <boolean>)
  ap.state == #"post-mortem"
end method;


