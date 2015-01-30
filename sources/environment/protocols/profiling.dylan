Module:    environment-protocols
Synopsis:  Environment profiling protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <profile-sampling-style>
  = one-of(#"interval", #"allocation", #"breakpoints");

define class <profile-state> (<object>)
  sealed slot profile-state-profiling-enabled? :: <boolean> = #f;
  sealed slot profile-state-default-options :: false-or(<profile-options>) = #f;
  sealed slot profile-state-current-options :: false-or(<profile-options>) = #f;
  sealed slot profile-state-last-profile :: false-or(<application-profile>) = #f;
end class <profile-state>;

define open generic profile-snapshot-available-values
    (server :: <server>) => (values :: <sequence>);

define open generic start-profiling-application
    (server :: <server>,  #key options :: false-or(<profile-options>))
 => ();

define open generic stop-profiling-application
    (server :: <server>)
 => (profile :: false-or(<application-profile>));

define open generic process-profiling-results
    (application :: <application>)
 => (profile :: false-or(<application-profile>));

define method profiling-enabled?
    (server :: <server>) => (enabled? :: <boolean>)
  let project = server.server-project;
  project.project-profile-state.profile-state-profiling-enabled?
end method profiling-enabled?;

define method project-default-profile-options
    (project :: <project-object>) => (options :: false-or(<profile-options>))
  project.project-profile-state.profile-state-default-options
end method project-default-profile-options;

define method project-default-profile-options-setter
    (options :: false-or(<profile-options>), project :: <project-object>)
 => (options :: false-or(<profile-options>))
  project.project-profile-state.profile-state-default-options := options
end method project-default-profile-options-setter;

define method project-last-profile
    (project :: <project-object>) => (profile :: false-or(<application-profile>))
  let application = project.project-application;
  let profile-state = project.project-profile-state;
  if (application & project.profiling-enabled?)
    profile-state.profile-state-last-profile
      := process-profiling-results(application)
  else
    profile-state.profile-state-last-profile
  end
end method project-last-profile;

define class <profile-sampling-options> (<object>)
  sealed constant slot profile-sampling-style :: <profile-sampling-style>,
    required-init-keyword: style:;
  sealed constant slot profile-sampling-rate :: false-or(<integer>) = #f,
    init-keyword: rate:;
end class <profile-sampling-options>;

define class <profile-snapshot-options> (<object>)
  sealed constant slot profile-snapshot-values :: <sequence>,
    required-init-keyword: values:;
  sealed constant slot profile-snapshot-stack-depth :: false-or(<integer>) = #f,
    init-keyword: stack-depth:;
end class <profile-snapshot-options>;

define class <profile-options> (<object>)
  sealed constant slot profile-sampling-options :: <profile-sampling-options>,
    required-init-keyword: sampling-options:;
  sealed constant slot profile-snapshot-options :: <profile-snapshot-options>,
    required-init-keyword: snapshot-options:;
end class <profile-options>;

define open abstract class <application-profile> (<object>)
  constant sealed slot application-profile-options :: <profile-options>,
    required-init-keyword: options:;
  constant sealed slot application-profile-snapshots :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  constant sealed slot application-profile-threads :: <stretchy-object-vector>
    = make(<stretchy-object-vector>);
  sealed slot application-total-wall-time :: <integer> = 0;
  sealed slot application-total-page-faults :: <integer> = 0;
end class <application-profile>;

define open abstract class <application-snapshot> (<object>)
  sealed constant slot application-snapshot-wall-time :: false-or(<integer>),
    required-init-keyword: wall-time:;
  sealed constant slot application-snapshot-page-faults :: false-or(<integer>),
    required-init-keyword: page-faults:;
  sealed constant slot application-snapshot-thread-snapshots :: <simple-object-vector>,
    required-init-keyword: thread-snapshots:;
end class <application-snapshot>;

define open abstract class <thread-snapshot> (<object>)
  sealed constant slot thread-snapshot-thread :: <thread-object>,
    required-init-keyword: thread:;
  sealed constant slot thread-snapshot-cpu-time :: false-or(<integer>),
    required-init-keyword: cpu-time:;
  sealed constant slot thread-snapshot-allocated-class :: false-or(<class-object>),
    required-init-keyword: allocated-class:;
  sealed constant slot thread-snapshot-allocation :: false-or(<integer>),
    required-init-keyword: allocation:;
  sealed slot %snapshots :: false-or(<simple-object-vector>) = #f;
end class <thread-snapshot>;

define open abstract class <thread-frame-snapshot> (<object>)
  constant sealed slot frame-snapshot-function :: false-or(<application-code-object>),
    required-init-keyword: function:;
  constant sealed slot frame-snapshot-source-location :: false-or(<source-location>),
    required-init-keyword: source-location:;
end class <thread-frame-snapshot>;

define open generic application-profile-options
    (profile :: <application-profile>) => (options :: <profile-options>);

define open generic thread-snapshot-stack-size
    (application :: <application>, snapshot :: <thread-snapshot>)
 => (size :: <integer>);

define open generic process-thread-snapshot-frame-snapshots
    (application :: <application>, snapshot :: <thread-snapshot>)
 => (frame-snapshots :: <simple-object-vector>);


/// Project dispatching methods

define method start-profiling-application
    (project :: <project-object>,
     #key options :: false-or(<profile-options>) = #f)
 => ()
  let state = project.project-profile-state;
  let options = options | state.profile-state-default-options;
  assert(~state.profile-state-profiling-enabled?,
         "Attempting to start profiling when it is already enabled");
  let application = project.project-application;
  state.profile-state-current-options := options;
  state.profile-state-profiling-enabled? := #t;
  application & ensure-profiling-started(application);
  broadcast($project-channel,
            make(<profiling-state-change-message>,
                 project: project,
                 enabled?: #t))
end method start-profiling-application;

define function ensure-profiling-started
    (application :: <application>) => ()
  let project = application.server-project;
  let state = project.project-profile-state;
  let options = state.profile-state-current-options;
  start-profiling-application(application, options: options)
end function ensure-profiling-started;

define method stop-profiling-application
    (project :: <project-object>)
 => (profile :: false-or(<application-profile>))
  let state = project.project-profile-state;
  assert(state.profile-state-profiling-enabled?,
         "Attempting to stop profiling when it is already stopped");
  block ()
    let application = project.project-application;
    if (application)
      state.profile-state-last-profile
        := stop-profiling-application(application)
    else
      state.profile-state-last-profile
    end
  cleanup
    state.profile-state-profiling-enabled? := #f;
    broadcast($project-channel,
              make(<profiling-state-change-message>,
                   project: project,
                   enabled?: #f))
  end
end method stop-profiling-application;

define method clear-profiling-results
    (server :: <server>) => ()
  let project = server.server-project;
  let state = project.project-profile-state;
  assert(~profiling-enabled?(project),
         "Attempting to clear profiling results while profiling is active");
  state.profile-state-last-profile := #f
end method clear-profiling-results;

define method profile-snapshot-available-values
    (project :: <project-object>) => (values :: <sequence>)
  let application = project.project-application;
  assert(application,
         "Attempting to query snapshot values with no application!");
  profile-snapshot-available-values(application)
end method profile-snapshot-available-values;


/// Some convenience functions built on these protocols

define inline function application-total-snapshots
    (profile :: <application-profile>) => (total :: <integer>)
  profile.application-profile-snapshots.size
end function application-total-snapshots;

define inline function do-application-profile-snapshots
    (function :: <function>, profile :: <application-profile>)
 => ()
  do(function, profile.application-profile-snapshots)
end function do-application-profile-snapshots;

define inline function do-application-profile-threads
    (function :: <function>, profile :: <application-profile>)
 => ()
  do(function, profile.application-profile-threads)
end function do-application-profile-threads;

define function application-snapshot-thread-snapshot
    (snapshot :: <application-snapshot>, thread :: <thread-object>)
 => (thread-snapshot :: false-or(<thread-snapshot>))
  let snapshots = snapshot.application-snapshot-thread-snapshots;
  block (return)
    for (thread-snapshot :: <thread-snapshot> in snapshots)
      if (thread-snapshot.thread-snapshot-thread == thread)
        return(thread-snapshot)
      end
    end;
    #f
  end
end function application-snapshot-thread-snapshot;

define inline function do-application-snapshot-thread-snapshots
    (function :: <function>, snapshot :: <application-snapshot>)
 => ()
  do(function, snapshot.application-snapshot-thread-snapshots)
end function do-application-snapshot-thread-snapshots;

define function do-thread-profile-snapshots
    (function :: <function>, application :: <application>,
     profile :: <application-profile>, thread :: <thread-object>)
 => (threads :: <sequence>)
  let snapshots :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-application-profile-snapshots
    (method (application-snapshot :: <application-snapshot>)
       let thread-snapshot
         = application-snapshot-thread-snapshot(application-snapshot, thread);
       if (thread-snapshot)
         function(application-snapshot, thread-snapshot)
       end
     end,
     profile);
  snapshots
end function do-thread-profile-snapshots;

define function thread-profile-snapshots
    (application :: <application>, profile :: <application-profile>,
     thread :: <thread-object>)
 => (threads :: <sequence>)
  let snapshots :: <stretchy-object-vector> = make(<stretchy-vector>);
  do-thread-profile-snapshots
    (method (snapshot :: <thread-snapshot>)
       add!(snapshots, snapshot)
     end,
     application, profile, thread);
  snapshots
end function thread-profile-snapshots;

define function thread-snapshot-frame-snapshots
    (application :: <application>, snapshot :: <thread-snapshot>)
 => (snapshots :: <simple-object-vector>)
  snapshot.%snapshots
    | begin
        let snapshots
          = process-thread-snapshot-frame-snapshots(application, snapshot);
        snapshot.%snapshots := snapshots
      end
end function thread-snapshot-frame-snapshots;

define function do-thread-snapshot-functions
    (function :: <function>, application :: <application>,
     snapshot :: <thread-snapshot>)
 => ()
  let frame-snapshots = thread-snapshot-frame-snapshots(application, snapshot);
  for (frame-snapshot :: <thread-frame-snapshot> in frame-snapshots)
    function(frame-snapshot.frame-snapshot-function,
             frame-snapshot.frame-snapshot-source-location)
  end
end function do-thread-snapshot-functions;

define function thread-snapshot-functions
    (application :: <application>, snapshot :: <thread-snapshot>)
 => (functions :: <sequence>)
  map(frame-snapshot-function,
      thread-snapshot-frame-snapshots(application, snapshot))
end function thread-snapshot-functions;
