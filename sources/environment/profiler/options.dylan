Module:    environment-profiler
Synopsis:  The profiling tool provided by the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profiler options dialog

//---*** These should come from a protocol....
define constant $default-sampling-style = #"interval";
define constant $default-sampling-rate  = 50;

define pane <profiler-general-options-page> ()
  sealed slot %show-foreign-functions? :: <boolean>,
    required-init-keyword: show-foreign-functions?:;
  sealed slot %sampling-style :: <profile-sampling-style>,
    required-init-keyword: style:;
  sealed slot %sampling-rate :: <integer>,
    required-init-keyword: rate:;
  pane %show-foreign-functions-pane (pane)
    make(<check-button>,
         label: "Include &foreign functions in reports",
         value: pane.%show-foreign-functions?,
         value-changed-callback: method (gadget)
                                   pane.%show-foreign-functions?
                                     := gadget-value(gadget)
                                 end);
  pane %sampling-style-pane (pane)
    make(<radio-box>,
         orientation: #"vertical",
         items: #[#["Sample at a fixed &rate", #"interval"],
                  #["Sample at &allocation",   #"allocation"],
                  #["Sample at &breakpoints",  #"breakpoints"]],
         value: pane.%sampling-style,
         label-key: first,
         value-key: second,
         value-changed-callback: method (gadget)
                                   let style = gadget-value(gadget);
                                   gadget-enabled?(pane.%sampling-rate-pane)
                                     := style == #"interval";
                                   pane.%sampling-style := style
                                 end);
  pane %sampling-rate-pane (pane)
    make(<text-field>,
         enabled?:   pane.%sampling-style == #"interval",
         value-type: <integer>,
         value:      pane.%sampling-rate,
         value-changed-callback: method (gadget)
                                   pane.%sampling-rate := gadget.gadget-value
                                 end);
  layout (pane)
    vertically (spacing: 2)
      grouping ("Options", max-width: $fill)
        vertically (spacing: 2)
          pane.%show-foreign-functions-pane;
        end
      end;
      grouping ("Sampling style", max-width: $fill)
        vertically (spacing: 8)
          pane.%sampling-style-pane;
        end
      end;
      grouping ("Sampling rate", max-width: $fill)
        horizontally (spacing: 8, y-alignment: #"center")
          pane.%sampling-rate-pane;
          make(<label>, label: "milliseconds")
        end
      end
    end;
end pane <profiler-general-options-page>;

define method frame-edit-options (frame :: <profiler>) => ()
  let framem = frame.frame-manager;
  let project = frame.ensure-frame-project;
  let options = project.project-default-profile-options;
  with-frame-manager (framem)
    let sampling-options = options & options.profile-sampling-options;
    let style = sampling-options & sampling-options.profile-sampling-style;
    let rate  = sampling-options & sampling-options.profile-sampling-rate;
    let general-page
      = make(<profiler-general-options-page>,
             show-foreign-functions?: frame.profiler-show-foreign-functions?,
             style: style | $default-sampling-style,
             rate:  rate  | $default-sampling-rate);
    let pages
      = vector(make(<property-page>, child: general-page, label: "General"));
    local method update-environment-options (dialog) => ()
            ignore(dialog);
            let foreign? = general-page.%show-foreign-functions?;
            let style    = general-page.%sampling-style;
            let rate     = general-page.%sampling-rate;
            let sampling-options
              = make(<profile-sampling-options>,
                     style: style,
                     rate:  rate);
            let snapshot-options
              = make(<profile-snapshot-options>,
                     values: #[#"cpu", #"wall", #"page-faults", #"allocation", #"class"],
                     depth:  #f);
            let options
              = make(<profile-options>,
                     sampling-options: sampling-options,
                     snapshot-options: snapshot-options);
            frame.profiler-show-foreign-functions? := foreign?;
            project.project-default-profile-options := options
          end method update-environment-options;
    let dialog
      = make(<property-frame>,
             title: "Profiling Options",
             owner: frame,
             mode:  #"modal",
             pages: pages,
             exit-callback: method (dialog)
                              update-environment-options(dialog);
                              exit-dialog(dialog);
                            end);
    start-dialog(dialog)
  end
end method frame-edit-options;
