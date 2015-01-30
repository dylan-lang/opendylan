Module:    dfmc-application
Synopsis:  environment profiling backend implementation
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Constants

define constant $default-sampling-rate = 50;
define constant $snapshot-values
  = #[#"cpu", #"wall", #"page-faults", #"allocation", #"class"];


/// Profiler state

define class <dfmc-application-profile-state> (<object>)
  sealed slot state-profile-initialized? :: <boolean> = #f;
  sealed slot state-profile-options :: <profile-options> = default-profile-options();
  sealed slot state-last-profile :: false-or(<dfmc-application-profile>) = #f;
  sealed slot state-current-profile :: false-or(<dfmc-application-profile>) = #f;
end class <dfmc-application-profile-state>;

define sealed domain make (subclass(<dfmc-application-profile-state>));
define sealed domain initialize (<dfmc-application-profile-state>);

define sealed method default-profile-options
    () => (options :: <profile-options>)
  make(<profile-options>,
       sampling-options: make(<profile-sampling-options>,
                              style:         #"interval",
                              sampling-rate: $default-sampling-rate),
       snapshot-options: make(<profile-snapshot-options>,
                              values:        $snapshot-values,
                              stack-depth:   #f))
end method default-profile-options;

define method state-class-profiling?
    (state :: <dfmc-application-profile-state>,
     #key options = state.state-profile-options)
 => (class-profiling? :: <boolean>)
  let sampling-options = options.profile-sampling-options;
  let sampling-style = sampling-options.profile-sampling-style;
  sampling-style == #"allocation"
end method state-class-profiling?;


/// Application profiles

define class <dfmc-application-profile> (<application-profile>)
  constant sealed slot %ip-cache :: <object-table> = make(<object-table>);
end class <dfmc-application-profile>;

define sealed domain make (subclass(<dfmc-application-profile>));
define sealed domain initialize (<dfmc-application-profile>);

define sealed method start-profiling-application
    (application :: <dfmc-application>,
     #key options :: false-or(<profile-options>) = #f)
 => ()
  let target = application.application-target-app;
  let profile-state = application.application-profile-state;
  let options = options | profile-state.state-profile-options;
  let class-profiling? = state-class-profiling?(profile-state, options: options);
  let profile = make(<dfmc-application-profile>, options: options);
  profile-state.state-profile-options := options;
  profile-state.state-current-profile := profile;
  profile-state.state-last-profile    := profile;
  if (class-profiling?)
    maybe-initialize-allocation-profiling(application)
  else
    maybe-initialize-cpu-profiling(application)
  end
end method start-profiling-application;

define method maybe-initialize-cpu-profiling
    (application :: <dfmc-application>) => ()
  let profile-state = application.application-profile-state;
  if (application.profiling-enabled?
        & profile-state
        & ~profile-state.state-profile-initialized?
        & ~profile-state.state-class-profiling?
        & application.application-loaded-dylan-library?)
    let target = application.application-target-app;
    debugger-message("Initializing CPU profiling");
    with-debugger-transaction (target)
      start-profiling(target, class-profiling?: #f);
      profile-state.state-profile-initialized? := #t
    end
  end
end method maybe-initialize-cpu-profiling;

define method maybe-initialize-allocation-profiling
    (application :: <dfmc-application>) => ()
  let profile-state = application.application-profile-state;
  if (application.profiling-enabled?
        & profile-state
        & ~profile-state.state-profile-initialized?
        & profile-state.state-class-profiling?
        & application.application-initialized-interactive-threads?)
    let target = application.application-target-app;
    debugger-message("Initializing allocation profiling");
    //---*** Temporary hack, this may not always be what we want but
    //---*** for now switch on breakpoints on all threads
    let interactive-thread = application-open-interactor-thread(application);
    with-debugger-transaction (target)
      if (interactive-thread)
        set-application-class-breakpoint(application, interactive-thread, #f)
      end;
      start-profiling(target, class-profiling?: #t);
      profile-state.state-profile-initialized? := #t
    end
  end
end method maybe-initialize-allocation-profiling;

define method set-application-class-breakpoint
    (application :: <dfmc-application>, thread :: <thread-object>,
     #rest args)
 => (transaction)
  execute-function
    (application,
     method ()
       let target = application.application-target-app;
       let remote-thread = thread.application-object-proxy;
       apply(dm-set-application-class-breakpoint,
             target, remote-thread, args)
     end,
     thread, state: #"running")
end method set-application-class-breakpoint;

define sealed method stop-profiling-application
    (application :: <dfmc-application>)
 => (profile :: <dfmc-application-profile>)
  let target = application.application-target-app;
  let profile-state = application.application-profile-state;
  let options = profile-state.state-profile-options;
  let class-profiling? = state-class-profiling?(profile-state, options: options);
  let interactive-thread
    = if (class-profiling?)
        //---*** Temporary hack, this may not always be what we want but
        //---*** for now switch on breakpoints on all threads
        application-open-interactor-thread(application);
      end;
  with-debugger-transaction (target)
    if (interactive-thread)
      debugger-message("Stopping general class breakpoint");
      let remote-thread = interactive-thread.application-object-proxy;
      clear-application-class-breakpoint
        (application, interactive-thread, #f, stop-profile?: #t);
      #f
    else
      debugger-message("Failed to stop general class breakpoint");
      debugger-message("  thread: %=, class profiling?: %=",
                       interactive-thread, class-profiling?);
    end;
    block ()
      unless (class-profiling?)
        stop-profiling(target);
      end;
      profile-state.state-profile-initialized? := #f;
      profile-state.state-last-profile
        := process-profiling-results(application)
    cleanup
      profile-state.state-current-profile := #f
    end
  end
end method stop-profiling-application;

define method clear-application-class-breakpoint
    (application :: <dfmc-application>, thread :: <thread-object>,
     #rest args) => (transaction)
  execute-function
    (application,
     method ()
       let target = application.application-target-app;
       let remote-thread = thread.application-object-proxy;
       apply(dm-clear-application-class-breakpoint,
             target, remote-thread, args)
     end,
     thread, state: #"running")
end method clear-application-class-breakpoint;


/// Application snapshots

define class <dfmc-application-snapshot> (<application-snapshot>)
  constant sealed slot %thread-snapshots :: <simple-object-vector>,
    required-init-keyword: thread-snapshots:;
end class <dfmc-application-snapshot>;

define sealed domain make (subclass(<dfmc-application-snapshot>));
define sealed domain initialize (<dfmc-application-snapshot>);


/// Thread snapshots

define class <dfmc-thread-snapshot> (<thread-snapshot>)
  constant sealed slot %snapshot :: dm-<thread-snapshot>,
    required-init-keyword: snapshot:;
  sealed slot %snapshot-frames :: false-or(<simple-object-vector>) = #f;
end class <dfmc-thread-snapshot>;

define sealed domain make (subclass(<dfmc-thread-snapshot>));
define sealed domain initialize (<dfmc-thread-snapshot>);

define class <dfmc-thread-frame-snapshot> (<thread-frame-snapshot>)
end class <dfmc-thread-frame-snapshot>;

define sealed domain make (subclass(<dfmc-thread-frame-snapshot>));
define sealed domain initialize (<dfmc-thread-frame-snapshot>);

define sealed method profile-snapshot-available-values
    (application :: <dfmc-application>) => (values :: <sequence>)
  $snapshot-values
end method profile-snapshot-available-values;

define sealed method thread-snapshot-stack-size
    (application :: <dfmc-application>, snapshot :: <dfmc-thread-snapshot>)
 => (size :: <integer>)
  snapshot.%snapshot.instruction-pointers.size
end method thread-snapshot-stack-size;

define sealed method process-thread-snapshot-frame-snapshots
    (application :: <dfmc-application>, snapshot :: <dfmc-thread-snapshot>)
 => (frames :: <simple-object-vector>)
  let project = application.server-project;
  let target = application.application-target-app;
  let path = target.debug-target-access-path;
  let profile-state = application.application-profile-state;
  let profile = profile-state.state-last-profile;
  let ip-cache = profile.%ip-cache;
  let raw-snapshot = snapshot.%snapshot;
  let ips = raw-snapshot.instruction-pointers;
  local method compute-ip-frame-snapshot
            (ip :: <remote-value>)
          element(ip-cache, ip, default: #f)
            | begin
                let source-location
                  = remote-address-source-location
                      (target, ip, line-only?: #t, interactive-only?: #f,
                       exact-only?: #f);
                let object
                  = if (source-location)
                      source-location-environment-object
                        (project, source-location)
                    end;
                let (object, location)
                  = if (object)
                      values(object, source-location)
                    else
                      let (start-ip, finish-ip)
                        = function-bounding-addresses(path, ip);
                      let object
                        = make-environment-object-for-runtime-value
                            (application, start-ip, address?: #t);
                      values(object, #f)
                    end;
                let (object, location)
                  = if (instance?(object, <application-code-object>))
                      values(object, location)
                    else
                      debugger-message("Corrupted profile stack! Found %=",
                                       object
                                         & environment-object-display-name
                                             (project, object, #f));
                      values(#f, #f)
                    end;
                if (object)
                  let frame-snapshot
                    = make(<dfmc-thread-frame-snapshot>,
                           function:        object,
                           source-location: location);
                  ip-cache[ip] := frame-snapshot
                end
              end
        end method compute-ip-frame-snapshot;
  remove!(map-as(<simple-object-vector>, compute-ip-frame-snapshot, ips), #f)
end method process-thread-snapshot-frame-snapshots;


/// Profile processing

define sealed method process-profiling-results
    (application :: <dfmc-application>)
 => (profile :: <dfmc-application-profile>)
  let project = application.server-project;
  let profile-state = application.application-profile-state;
  if (project.profiling-enabled?)
    let target = application.application-target-app;
    let data = profile-data(target);
    let profile = profile-state.state-current-profile;
    let threads = profile.application-profile-threads;
    let total-wall-time = profile.application-total-wall-time;
    let total-page-faults = profile.application-total-page-faults;
    let snapshots = profile.application-profile-snapshots;
    let raw-threads = data.dm-application-profile-threads;
    let raw-snapshots = data.application-snapshots;
    debugger-message("Processing profile results: threads=%d, snapshots=%d",
                     raw-threads.size, raw-snapshots.size);
    do(method (raw-snapshot :: dm-<application-snapshot>)
         let raw-snapshots = raw-snapshot.thread-snapshots;
         let thread-snapshots
           = map-as(<simple-object-vector>,
                    method (raw-snapshot :: dm-<thread-snapshot>)
                      let remote-thread = raw-snapshot.profile-thread;
                      let thread
                        = make-environment-object
                            (<thread-object>,
                             project: project,
                             application-object-proxy: remote-thread);
                      let raw-class = raw-snapshot.allocated-class;
                      let class
                        = if (raw-class)
                            make-environment-object-for-runtime-value
                              (application, raw-class)
                          end;
                      make(<dfmc-thread-snapshot>,
                           thread:          thread,
                           snapshot:        raw-snapshot,
                           cpu-time:        raw-snapshot.cpu-time-increment,
                           allocation:      raw-snapshot.allocation-increment,
                           allocated-class: class)
                    end,
                    raw-snapshots);
         let wall-time   = raw-snapshot.wall-time-increment;
         let page-faults = raw-snapshot.page-faults-increment;
         add!(snapshots,
              make(<dfmc-application-snapshot>,
                   wall-time:        wall-time,
                   page-faults:      page-faults,
                   thread-snapshots: thread-snapshots));
         total-wall-time   := total-wall-time + wall-time;
         total-page-faults := total-page-faults + page-faults;
       end,
       raw-snapshots);
    do(method (raw-thread :: <remote-thread>)
         add-new!(threads,
                  make-environment-object
                    (<thread-object>,
                     project: project,
                     application-object-proxy: raw-thread))
       end,
       raw-threads);
    application-total-wall-time(profile)   := total-wall-time;
    application-total-page-faults(profile) := total-page-faults;
    reset-profile-data(target);
    debugger-message("Returning, %d snapshots, %d threads",
                     profile.application-profile-snapshots.size,
                     profile.application-profile-threads.size);
    profile
  else
    profile-state.state-last-profile
      | error("Attempting to process results without having profiled!")
  end
end method process-profiling-results;
