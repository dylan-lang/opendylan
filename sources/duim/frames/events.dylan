Module:       duim-frames-internals
Synopsis:     DUIM frames
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Frame events

// This event is the first event a frame will see
// It gets put at the head of the event queue, before any other events
define sealed class <frame-created-event> (<frame-event>)
end class <frame-created-event>;

// This event gets sent when the UI for a frame gets destroyed by
// a call to 'destroy-frame'
define sealed class <frame-destroyed-event> (<frame-event>)
end class <frame-destroyed-event>;

// This event is sent the very first time a frame gets layed out,
// after it gets created but before it gets destroyed
define sealed class <frame-layed-out-event> (<frame-event>)
end class <frame-layed-out-event>;

// This event is sent after a frame gets mapped
define sealed class <frame-mapped-event> (<frame-event>)
end class <frame-mapped-event>;

// This event is sent after a frame gets unmapped
define sealed class <frame-unmapped-event> (<frame-event>)
end class <frame-unmapped-event>;

define abstract class <frame-focus-event> (<frame-event>)
end class <frame-focus-event>;

// This event is sent every time a frame gets the input focus
define sealed class <frame-focus-in-event> (<frame-focus-event>)
end class <frame-focus-in-event>;

// This event is sent every time a frame loses the input focus
define sealed class <frame-focus-out-event> (<frame-focus-event>)
end class <frame-focus-out-event>;

// This event is sent every time the input focus in a frame changes
define sealed class <frame-input-focus-changed-event> (<frame-event>)
  sealed constant slot event-old-focus :: false-or(<sheet>),
    required-init-keyword: old-focus:;
  sealed constant slot event-new-focus :: false-or(<sheet>),
    required-init-keyword: new-focus:;
end class <frame-input-focus-changed-event>;

// This event gets sent when someone calls 'frame-exit'.  The default
// 'handle-event' method just exits the frame, but hackers can write
// their own method that queries the user, for example.
define sealed class <frame-exit-event> (<frame-event>)
  sealed constant slot event-destroy-frame? = #f,
    init-keyword: destroy-frame?:;
end class <frame-exit-event>;

// This event get sent when a dialog exits normally.
define sealed class <dialog-exit-event> (<frame-exit-event>)
end class <dialog-exit-event>;

// This event get sent when a dialog is cancelled.
define sealed class <dialog-cancel-event> (<frame-exit-event>)
end class <dialog-cancel-event>;

// This event gets sent when the UI for a frame has been shut down
define sealed class <frame-exited-event> (<frame-event>)
  sealed constant slot event-status-code :: false-or(<integer>) = #f,
    init-keyword: status-code:;
end class <frame-exited-event>;

// This event gets sent when a whole application is shut down,
// kind of analogous to the Windows 'PostQuitMessage'
define sealed class <application-exited-event> (<frame-exited-event>)
end class <application-exited-event>;


/// Seal some domains for the concrete event classes

define sealed domain make (singleton(<frame-created-event>));
define sealed domain initialize (<frame-created-event>);

define sealed domain make (singleton(<frame-destroyed-event>));
define sealed domain initialize (<frame-destroyed-event>);

define sealed domain make (singleton(<frame-mapped-event>));
define sealed domain initialize (<frame-mapped-event>);

define sealed domain make (singleton(<frame-unmapped-event>));
define sealed domain initialize (<frame-unmapped-event>);

define sealed domain make (singleton(<frame-focus-in-event>));
define sealed domain initialize (<frame-focus-in-event>);

define sealed domain make (singleton(<frame-focus-out-event>));
define sealed domain initialize (<frame-focus-out-event>);

define sealed domain make (singleton(<frame-input-focus-changed-event>));
define sealed domain initialize (<frame-input-focus-changed-event>);

define sealed domain make (singleton(<frame-exit-event>));
define sealed domain initialize (<frame-exit-event>);

define sealed domain make (singleton(<frame-exited-event>));
define sealed domain initialize (<frame-exited-event>);

define sealed domain make (singleton(<application-exited-event>));
define sealed domain initialize (<application-exited-event>);
