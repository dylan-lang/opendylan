Module:    environment-debugger
Author:    Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable $notifier-dialog-width  :: <integer> = 500;
define variable $notifier-dialog-height :: <integer> = 350;

define sealed frame <notifier> (<dialog-frame>)
  sealed slot notifier-abort-restart :: false-or(<restart-object>) = #f;
  sealed constant slot notifier-project :: <project-object>,
    required-init-keyword: project:;
  sealed constant slot notifier-thread :: false-or(<thread-object>),
    required-init-keyword: remote-thread:;
  pane notifier-context-pane (dialog)
    make(<text-editor>,
         lines: 2,
         read-only?: #t, tab-stop?: #t,
         scroll-bars: #"none");
  pane notifier-restarts-pane (dialog)
    make(<list-box>,
         activate-callback: exit-dialog,
         label-key: curry(application-restart-message, dialog.notifier-project));
  pane notifier-abort-button (dialog)
    make-notifier-button
      (dialog, #"abort",    "Abort the current operation and continue %s");
  pane notifier-continue-button (dialog)
    make-notifier-button
      (dialog, #"continue", "Continue %s using the selected restart");
  pane notifier-debug-button (dialog)
    make-notifier-button
      (dialog, #"debug"     "Debug %s");
  pane notifier-exit-button (dialog)
    make-notifier-button
      (dialog, #"exit",     "Exit %s");
  pane notifier-button-box (dialog)
    make(<radio-box>,
         child: begin
                  let project = dialog.notifier-project;
                  let name = project.project-application-short-name;
                  vertically (spacing: 5)
                    dialog.notifier-abort-button;
                    vertically (spacing: 5)
                      dialog.notifier-continue-button;
                      horizontally (spacing: 0)
                        make(<null-pane>, width: 18, fixed-width?: #t);
                        dialog.notifier-restarts-pane;
                      end
                    end;
                    dialog.notifier-debug-button;
                    dialog.notifier-exit-button;
                  end
                end,
         value-changed-callback: method (gadget)
                                   gadget-enabled?(dialog.notifier-restarts-pane)
                                     := gadget-value(gadget) == #"continue"
                                 end);
  layout (dialog)
    horizontally (spacing: 8, y-alignment: #"top")
      make(<label>,
           label: $internal-error-bitmap,
           width:  32, min-width:  32, max-width:  32,
           height: 32, min-height: 32, max-height: 32);
      vertically (spacing: 8, x-alignment: #"left")
        make(<label>,
             label: format-to-string("APPLICATION ERROR: %s",
                                     project-application-short-name
                                       (dialog.notifier-project)),
             text-style: make(<text-style>, weight: #"bold", size: #"large"));
        dialog.notifier-context-pane;
        dialog.notifier-button-box
      end
    end;
  keyword title: = "Notifier";
  keyword cancel-callback: = #f;
  keyword width:  = $notifier-dialog-width;
  keyword height: = $notifier-dialog-height;
  input-focus (dialog)
    dialog.dialog-exit-button;
end frame <notifier>;

define sealed method initialize
    (dialog :: <notifier>, #key) => ()
  next-method();
  let project :: <project-object> = dialog.notifier-project;
  let application :: <application> = project.project-application;
  let thread :: false-or(<thread-object>) = dialog.notifier-thread;
  let restarts
    = if (thread) application-thread-restarts(project, thread) else #[] end if;
  let abort-restart
    = any?(conjoin(curry(application-restart-abort?, project), identity),
           restarts);
  let continue-restart = ~empty?(restarts) & restarts[0];
  let default-action
    = case
        abort-restart    => #"abort";
        continue-restart => #"continue";
        otherwise        => #"debug";
      end;
  let context-pane    = dialog.notifier-context-pane;
  let restarts-pane   = dialog.notifier-restarts-pane;
  let button-box      = dialog.notifier-button-box;
  let abort-button    = dialog.notifier-abort-button;
  let continue-button = dialog.notifier-continue-button;
  dialog.frame-title := as(<string>, application.application-filename);
  dialog.notifier-abort-restart := abort-restart;
  gadget-items(restarts-pane) := restarts;
  gadget-value(restarts-pane) := continue-restart;
  gadget-value(button-box)    := default-action;
  gadget-value(context-pane)  := application.application-stop-reason-message;
  gadget-enabled?(abort-button)    := (abort-restart ~== #f);
  gadget-enabled?(continue-button) := (continue-restart ~== #f);
  gadget-enabled?(restarts-pane)   := (default-action == #"continue");
end method initialize;

define sealed domain make (singleton(<notifier>));
define sealed domain initialize (<notifier>);

define function choose-debug?
    (project :: <project-object>, thread :: false-or(<thread-object>),
     #key owner)
  => (debug? :: <boolean>)
  ~application-just-hit-error?(project, thread)
    | ~$debugger-settings.notifier-dialog
    | block (return)
        let notifier
          = make(<notifier>,
                 owner: owner,
                 project: project,
                 remote-thread: thread);
        if (start-dialog(notifier))
          select (gadget-value(notifier.notifier-button-box))
            #"abort"    => notifier-invoke-abort(notifier);
            #"continue" => notifier-invoke-restart(notifier);
            #"debug"    => return(#t);
            #"exit"     => close-application(project);
          end;
          #f
        end
      end
end function choose-debug?;

define method project-application-short-name
    (project :: <project-object>) => (name :: <string>)
  let application = project.project-application;
  locator-name(application.application-filename)
end method project-application-short-name;

define method notifier-application-message
    (dialog :: <notifier>, format-string :: <string>)
 => (message :: <string>)
  let project = dialog.notifier-project;
  format-to-string(format-string,
                   project.project-application-short-name)
end method notifier-application-message;

define method make-notifier-button
    (dialog :: <notifier>, id :: <symbol>, format-string :: <string>)
 => (button :: <radio-button>)
  make(<radio-button>,
       id: id,
       label: notifier-application-message(dialog, format-string))
end method make-notifier-button;

define function notifier-invoke-restart
    (dialog :: <notifier>) => ()
  let restart :: false-or(<restart-object>) = dialog.notifier-restarts-pane.gadget-value;
  let thread :: false-or(<thread-object>) = dialog.notifier-thread;
  if (restart & thread)
    invoke-application-restart(dialog.notifier-project, thread, restart);
  end
end function notifier-invoke-restart;

define function notifier-invoke-abort
    (dialog :: <notifier>) => ()
  let restart :: false-or(<restart-object>) = dialog.notifier-abort-restart;
  let thread :: false-or(<thread-object>) = dialog.notifier-thread;
  if (restart & thread)
    invoke-application-restart(dialog.notifier-project, thread, restart);
  end
end function notifier-invoke-abort;
