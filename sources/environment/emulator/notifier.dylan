Module:    emulator-environment
Synopsis:  Emulator Environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*---*** No longer used
/// A simple Dylan notifier

define frame <notifier> (<dialog-frame>)
  slot notifier-condition = #f,
    init-keyword: condition:;
  slot notifier-restarts = #[],
    init-keyword: restarts:;
  slot notifier-abort-restart = #f,
    init-keyword: abort-restart:;
  slot notifier-continue-restart = #f,
    init-keyword: continue-restart:;
  pane notifier-error-message-pane (notifier)
    make-multi-line-label
      (notifier,
       format-to-string("Condition: %s",
			princ-to-string(notifier-condition(notifier))));
  pane notifier-restarts-pane (notifier)
    make(<list-control>,
         items: notifier-restarts(notifier),
         value: notifier-continue-restart(notifier),
         activate-callback: method (sheet)
                              let restart = gadget-value(sheet);
                              invoke-restart-interactively(restart)
                            end,
         label-key: curry(notifier-print-restart, notifier));
  layout (frame)
    vertically (spacing: 8)
      frame.notifier-error-message-pane;
      frame.notifier-restarts-pane
    end;
end frame <notifier>;

define method make-multi-line-label 
    (frame :: <frame>, string :: <string>, 
     #key max-width = 80, max-lines = 10)
 => (sheet :: <sheet>)
  let panes = make(<stretchy-vector>);
  with-frame-manager (frame-manager(frame))
    let string-size = size(string);
    let position = 0;
    block (return)
      for (count from 1,
           until position >= string-size)
        if (max-lines & count > max-lines)
          add!(panes, make(<label>, label: "..."));
          return()
        end;
        let end-position = min(position + max-width, string-size);
        if (end-position < string-size)
          while (end-position > position & string[end-position - 1] ~= ' ')
            end-position := end-position - 1
          end;
          if (end-position <= position)
            end-position := min(position + max-width, string-size);
          end
        end;
        let label
          = copy-sequence(string, start: position, end: end-position);
        add!(panes, make(<label>, label: label));
        position := end-position;
        while (position < string-size & string[position] = ' ')
          position := position + 1;
        end;
      end
    end
  end;
  make(<column-layout>, children: panes, spacing: 2)
end method make-multi-line-label;

define method notifier-print-restart
    (notifier :: <notifier>, restart) => (string :: <string>)
  format-to-string("%s%s",
                   select (restart)
                     notifier-continue-restart(notifier) => "(continue) ";
                     notifier-abort-restart(notifier)    => "(abort) ";
                     otherwise                           => "";
                   end,
                   princ-to-string(restart))
end method notifier-print-restart;

define method notify-user-of-condition
    (condition, restarts, #key abort-restart, continue-restart) => ()
  with-frame-manager (port-default-frame-manager(default-port()))
    block (return)
      //---*** What should the owner be?
      let notifier 
        = make(<notifier>, mode: #"modeless",
               condition: condition,
               restarts: restarts,
               mode: #"modeless",
               abort-restart: abort-restart,
               continue-restart: continue-restart,
               title: "Error Notifier",
               cancel-button: make(<push-button>,
                                   label: "Abort",
                                   activate-callback: cancel-dialog),
               help-button: make(<push-button>,
                                 label: "Debug",
                                 activate-callback: method(sheet)
                                                      return()
                                                    end));
      let restart
        = block ()
            let ok? = start-frame(notifier);
            if (ok?)
              let restarts-pane = notifier-restarts-pane(notifier);
              gadget-value(restarts-pane)
            else
              notifier-abort-restart(notifier)
            end
          cleanup
            destroy-frame(notifier)
          end;
      invoke-restart-interactively(restart)
    end
  end
end method notify-user-of-condition;

install-notifier-function(notify-user-of-condition);
*/

//---*** Let's not install this anymore, as it is too risky
/*
//--- Hack to override the LispWorks inspector!
define method environment-browse-object
    (object) => ()
  let object
    = if (instance?(object, <environment-object>))
        object
      else
        ensure-server-object(current-project(), object)
      end;
  if (object) browse-object(object) end
end method environment-browse-object;
*/
