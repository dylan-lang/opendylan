Module:    environment-internal-commands
Synopsis:  The internal-only commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Internal properties

// Debugger property

define class <debugger-property> (<environment-property>)
end class <debugger-property>;

define command-property debugger => <debugger-property>
  (summary:       "Debugger property",
   documentation: "Property to specify whether to crash into the debugger",
   type:          <boolean>)
end command-property debugger;

define method show-property
    (context :: <environment-context>, property :: <debugger-property>)
 => ()
  message(context, "Debugger: %s",
	  if (context.context-server.server-debugger?) "on" else "off" end)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <debugger-property>,
     debugger? :: <boolean>,
     #key save?)
 => ()
  context.context-server.server-debugger? := debugger?
end method set-property;

// Profile commands property

define class <profile-commands-property> (<environment-property>)
end class <profile-commands-property>;

define command-property profile-commands => <profile-commands-property>
  (summary:       "Profile commands property",
   documentation: "Property to specify whether to profile commands",
   type:          <boolean>)
end command-property profile-commands;

define method show-property
    (context :: <environment-context>, property :: <profile-commands-property>)
 => ()
  message(context, "Profile commands: %s",
	  if (context.context-server.server-profile-commands?) "on" else "off" end)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <profile-commands-property>,
     profile-commands? :: <boolean>,
     #key save?)
 => ()
  context.context-server.server-profile-commands? := profile-commands?
end method set-property;

// Patching property

define class <patching-property> (<environment-property>)
end class <patching-property>;

define command-property patching => <patching-property>
  (summary:       "Patching property",
   documentation: "Property to specify whether to build patch DLLs",
   type:          <boolean>)
end command-property patching;

define method show-property
    (context :: <environment-context>, property :: <patching-property>)
 => ()
  message(context, "Patching: %s",
	  if (environment-variable("OPEN_DYLAN_SYSTEM_DEVELOPER") = "yes") "on" else "off" end)
end method show-property;

define method set-property
    (context :: <environment-context>, property :: <patching-property>,
     patching? :: <boolean>,
     #key save?)
 => ()
  environment-variable("OPEN_DYLAN_SYSTEM_DEVELOPER") := if (patching?) "yes" else "" end;
end method set-property;


/// Registry commands

define command-group internal into environment
    (summary: "miscellaneous internal commands",
     documentation: "Miscellaneous internal-only commands.")
  property debugger;
  property patching;
  property profile-commands;
end command-group internal;
