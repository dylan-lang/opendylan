Module:       CAPI-DUIM
Synopsis:     CAPI back-end
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Dialog handling

// This class wraps up the real window system object
define sealed class <capi-dialog-top-level-mirror> (<capi-top-level-mirror>)
  sealed slot dialog-aborted? = #f;
end class <capi-dialog-top-level-mirror>;

define method make-top-level-mirror
    (_port :: <capi-port>, frame :: <dialog-frame>, sheet :: <capi-top-level-sheet>)
  let modeless? = modeless-dialog?(frame);
  let mirror
    = make-capi-mirror(_port, sheet, <capi-dialog-top-level-mirror>,
                       title: frame-title(frame) | #(),
                       internal-border: *top-level-sheet-border*,
                       parent: convert-to-screen());
  mirror
end method make-top-level-mirror;

define method modeless-dialog? 
    (mirror :: <capi-dialog-top-level-mirror>)
 => (modeless :: <boolean>)
  let sheet = mirror-sheet(mirror);
  let frame = sheet-frame(sheet);
  modeless-dialog?(frame)
end method modeless-dialog?;

define method modeless-dialog? 
    (dialog :: <dialog-frame>)
 => (modeless :: <boolean>)
  frame-mode(dialog) = #"modeless";
end method modeless-dialog?;

define method map-top-level-mirror (mirror :: <capi-dialog-top-level-mirror>)
  let sheet = mirror-sheet(mirror);
  let owner-frame = frame-owner(sheet-frame(sheet));
  let top-sheet   = owner-frame & top-level-sheet(owner-frame);
  let owner = top-sheet & sheet-mirror(top-sheet);
  let rep = representation(mirror);
  assert(~instance?(rep, <empty-list>),
         "Failed to find a representation for dialog top-level %=", sheet);
  display-dialog-representation
    (rep, ~modeless-dialog?(mirror) | #(),
     pop-up-from: (owner & representation(owner)) | #(),
     // immediate-return: #t,
     // use-default-event-queue: #(),
     warp-pointer: #t);
end method map-top-level-mirror;

define method port-start-frame
    (port :: <capi-port>, frame :: <dialog-frame>)
 => (status-code :: false-or(<integer>))
  ignore(port);
  block ()
    frame-mapped?(frame) := #t;
    let sheet    = top-level-sheet(frame);
    let mirror   = sheet  & sheet-mirror(sheet);
    let aborted? = mirror & dialog-aborted?(mirror);
    ~aborted? & 0
  cleanup
    frame-mapped?(frame) := #f;
    //--- Unfortunately CAPI destroys the window, so we have to
    //--- act as if it has been detached.
    detach-frame(frame-manager(frame), frame);
  end;
end method port-start-frame;

define method ensure-pane-created (mirror :: <capi-dialog-top-level-mirror>)
  when (instance?(representation(mirror), <list>))
    let modal? = ~modeless-dialog?(mirror);
    create-dialog(mirror, convert-to-screen(), modal? | #());
  end
end method ensure-pane-created;


/// Piggy-back on the default dialogs from gadget-panes

define method frame-wrapper
    (framem :: <capi-frame-manager>, 
     dialog :: <dialog-frame>,
     layout :: false-or(<sheet>))
 => (sheet :: false-or(<sheet>))
  default-dialog-frame-wrapper(framem, dialog, layout)
end method frame-wrapper;

define method update-frame-layout
    (framem :: <capi-frame-manager>, frame :: <dialog-frame>) => ()
  update-default-dialog-layout(framem, frame)
end method update-frame-layout;

define method dialog-needs-title-pane?
    (framem :: <capi-frame-manager>, dialog :: <dialog-frame>)
 => (needs-title-pane? :: <boolean>)
  ~modeless-dialog?(dialog)
end method dialog-needs-title-pane?;

define method make-top-level-drawing-pane
    (framem :: <capi-frame-manager>, children :: <sequence>)
 => (sheet :: <sheet>)
  make(<capi-top-level-drawing-pane>,
       y-spacing: 6,
       x-alignment: #"center",
       children: children)
end method make-top-level-drawing-pane;


/// Dialog flow control

// Generate an ordinary exit event
define method do-exit-dialog
    (framem :: <capi-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  let top-sheet = top-level-sheet(dialog);
  when (top-sheet)
    let mirror = sheet-mirror(top-sheet);
    exit-dialog-representation(representation(mirror), #f);
    distribute-event(port(dialog),
		     make(<dialog-exit-event>,
			  frame: dialog,
			  destroy-frame?: destroy?))
  end
end method do-exit-dialog;

// Generate an "error" exit event
define method do-cancel-dialog 
    (framem :: <capi-frame-manager>, dialog :: <dialog-frame>, #key destroy? = #t) => ()
  let top-sheet = top-level-sheet(dialog);
  when (top-sheet)
    let mirror = sheet-mirror(top-sheet);
    dialog-aborted?(mirror) := #t;
    abort-dialog-representation(representation(mirror));
    distribute-event(port(dialog),
		     make(<dialog-cancel-event>,
			  frame: dialog,
			  destroy-frame?: destroy?))
  end
end method do-cancel-dialog;
