Module:    environment-manager
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $environment-channel :: <channel> = make(<channel>);

define open abstract class <environment-message> (<object>)
end class;

// A starting message is to be sent very early on, when lots of things
// may not be initialized properly.  This is probably only useful for
// registering for later messages etc.
// This message is to be sent by the initial thread.
define class <environment-starting-message> (<environment-message>)
end class;

// A started message is to be sent when the main environment frame has
// been created and mapped (i.e., during the <frame-mapped-event>).
// This message is to be sent on the <environment-primary-frame> thread.
define class <environment-started-message> (<environment-message>)
end class;

//---*** hughg: We may later want one message when the user asks to close
//---*** the primary frame and another if the action is cancelled (e.g.,
//---*** because some file is unsaved).  (If the action goes ahead, we can
//---*** just wait for the <environment-stopped-message>, unless we need
//---*** something to happen on the primary-window thread.
// A stopping message is sent when the main environment frame is
// definitely closing (i.e., during the <frame-destroyed-event>).
// This message is to be sent on the <environment-primary-frame> thread.
define class <environment-stopping-message> (<environment-message>)
end class;

// A stopped message is to be sent just before the environment process
// exits (i.e., after wait-for-shutdown() returns).
// This message is to be sent by the initial thread.
//---*** Perhaps we should use some kind of <application-exited-event>?
define class <environment-stopped-message> (<environment-message>)
end class;

// The emulator currently doesn't send shutdown notification.
//
// Those who want to be notified of startup/shutdown should do the
// following early on.  Substitute the name of your callback function
// for "my-callback", of course!
//   tune-in($environment-channel, my-callback,
//           message-type: <environment-shutdown-message>);
//                   // or <environment-startup-message>, or whatever.
// See the channels library for more info.
