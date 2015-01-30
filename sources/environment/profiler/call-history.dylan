Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profiler

define frame <profiler-call-history> (<simple-frame>)
  sealed slot profiler-show-zero-cpu-time? :: <boolean> = #f;
  pane call-history-page (frame)
    make(<tab-control-page>,
         label: "Call History",
         id: #"call-history",
         child: frame.call-history-displayer);
  pane call-history-displayer (frame)
    make(<tree-control-displayer>,
         element-label: "form",
         information-available?-function: curry(profiler-has-results?, frame),
         transaction-function: curry(perform-profiler-transaction, frame),
         children-generator: curry(frame-references, frame),
         label-key: curry(frame-reference-label, frame),
         activate-callback: curry(profiler-activate-callback, frame));
end frame <profiler-call-history>;

define method refresh-profiler-page
    (frame :: <profiler-call-history>, page == #"call-history",
     #key clean? = #f, new-thread? = #t)
 => ()
  let project = frame.ensure-frame-project;
  let thread = frame.frame-current-thread;
  let displayer = frame.call-history-displayer;
  debug-out(#"environment-profiler", "Refreshing call history page (clean?: %=)", clean?);
  unless (~thread & ~displayer.displayer-object)
    displayer-object(displayer, clean?: clean?, new-thread?: new-thread?)
      := thread
  end
end method refresh-profiler-page;

define method invalidate-profiler-page
    (frame :: <profiler-call-history>, page == #"call-history") => ()
  // frame.call-history-displayer.displayer-valid? := #f
  invalidate-frame-property-page(frame, frame.call-history-displayer, #f, page)
end method invalidate-profiler-page;

define method frame-references
    (frame :: <profiler-call-history>, thread :: <thread-object>)
 => (functions :: <vector>)
  let project = frame.ensure-frame-project;
  let profile = frame.profiler-application-profile;
  let history = process-profile-call-history(project, profile, thread);
  make-frame-reference-wrappers(frame, history.call-history-root-references)
end method frame-references;

define method frame-references
    (frame :: <profiler-call-history>, wrapper :: <profile-frame-wrapper>)
 => (functions :: <vector>)
  let references = wrapper.wrapper-profile-references;
  make-frame-reference-wrappers(frame, references)
end method frame-references;

define method frame-references
    (frame :: <profiler-call-history>, wrapper :: <allocated-class-wrapper>)
 => (functions :: <vector>)
  #[]
end method frame-references;

define class <profile-frame-wrapper> (<object-wrapper>)
  constant slot wrapper-profile-references :: <stretchy-object-vector>,
    required-init-keyword: references:;
  slot wrapper-profile-amount :: <integer>,
    required-init-keyword: amount:;
end class <profile-frame-wrapper>;

define class <allocated-class-wrapper> (<object-and-location-wrapper>)
end class <allocated-class-wrapper>;

define method make-frame-reference-wrappers
    (frame :: <profiler-call-history>, references :: <sequence>)
 => (wrappers :: <stretchy-object-vector>)
  let project = frame.ensure-frame-project;
  let profile = project.project-last-profile;
  let sampling-options = profile.application-profile-options.profile-sampling-options;
  let style = sampling-options.profile-sampling-style;
  let wrappers :: <stretchy-object-vector> = make(<stretchy-object-vector>);
  let show-zero-cpu-time? = frame.profiler-show-zero-cpu-time?;
  let show-foreign-functions? = frame.profiler-show-foreign-functions?;
  let allocation-profiling? = profile.allocation-profiling?;
  let last-frame-wrapper :: false-or(<profile-frame-wrapper>) = #f;
  local method add-wrapper
            (wrapper :: <profile-frame-wrapper>) => ()
          if (last-frame-wrapper
                & wrapper.wrapper-object = last-frame-wrapper.wrapper-object)
            let amount     = wrapper.wrapper-profile-amount;
            let references = wrapper.wrapper-profile-references;
            last-frame-wrapper.wrapper-profile-amount
              := last-frame-wrapper.wrapper-profile-amount + amount;
            let all-references = last-frame-wrapper.wrapper-profile-references;
            for (reference in references)
              add!(all-references, reference)
            end
          else
            last-frame-wrapper := wrapper;
            add!(wrappers, wrapper)
          end
        end method add-wrapper;
  for (reference in references)
    select (reference by instance?)
      <profile-frame-history> =>
        let function = reference.profile-frame-frame.frame-snapshot-function;
        if (instance?(function, <internal-method-object>)
              | (~show-foreign-functions?
                   & instance?(function, <foreign-object>)))
          let references = reference.profile-frame-references;
          let subwrappers = make-frame-reference-wrappers(frame, references);
          for (wrapper :: <profile-frame-wrapper> in subwrappers)
            add-wrapper(wrapper)
          end
        else
          if (show-zero-cpu-time? | reference.profile-frame-cpu-time >= 0)
            let amount
              = if (allocation-profiling?)
                  reference.profile-frame-allocation
                else
                  reference.profile-frame-cpu-time
                end;
            let wrapper
              = make(<profile-frame-wrapper>,
                     object:     function,
                     amount:     amount,
                     references: reference.profile-frame-references);
            add-wrapper(wrapper)
          end
        end;
      <profile-frame-allocated-class> =>
        let class    = reference.profile-frame-allocated-class;
        let location = reference.profile-frame-source-location;
        add!(wrappers,
             make(<allocated-class-wrapper>,
                  object:          class,
                  source-location: location));

    end
  end;
  wrappers
end method make-frame-reference-wrappers;

define method frame-reference-label
    (frame :: <profiler-call-history>, wrapper :: <allocated-class-wrapper>)
 => (label :: <string>)
  let class = wrapper.wrapper-object;
  format-to-string("Allocated %s",
                   frame-default-object-name(frame, class))
end method frame-reference-label;

define method frame-reference-label
    (frame :: <profiler-call-history>, wrapper :: <profile-frame-wrapper>)
 => (label :: <string>)
  let project = frame.ensure-frame-project;
  let function = wrapper.wrapper-object;
  let amount = wrapper.wrapper-profile-amount;
  let profile = project.project-last-profile;
  let units
    = if (profile.allocation-profiling?)
        "bytes"
      else
        "ms"
      end;
  format-to-string("%d %s: %s",
                   amount,
                   units,
                   frame-default-object-name(frame, function))
end method frame-reference-label;
