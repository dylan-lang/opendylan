module:      devel-dbg-ui
synopsis:    A function for outputting debugger messages.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DEBUGGER-MESSAGE
//    Prints a message onto the console with appropriate decoration
//    and extra formatting. Use as a direct substitute for format-out

define method debugger-message (s :: <string>, #rest stuff)
       format-out (";;; ");
       apply (format-out, s, stuff);
       format-out ("\n")
end method;


///// WELCOME-BANNER
//    Prints a bunch of welcome messages.

define method welcome-banner ()
  if (*dbg-version* == #"console-debugger")
    debugger-message("The Dylan Console Debugger:");
  else
    debugger-message("The Dylan Console Debugger and Interactive Compiler:");
  end if;
  debugger-message("It does exactly what it says on the tin.");
  debugger-message("Bugs should be reported, and assigned to Paul Howard.");
  debugger-message("Contact: phoward (Longstanton House, tel: 5456).");
  debugger-message("");
end method;
