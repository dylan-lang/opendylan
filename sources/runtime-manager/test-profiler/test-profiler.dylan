module:    test-profiler
synopsis:  A simple profiler to get some profile data from an application
author:    Keith Dennison
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Handler for Stop-reason Callbacks
// =================================
//
// We ignore all stop reasons except the one generated when the process is
// created - then we turn the profiler manager on.
//
define method process-stop-reason
    (application :: <debug-target>,
     stop-reason :: <stop-reason>)
     => (result :: <boolean>)
  #f
end method;

define method process-stop-reason
    (application :: <profile-target>,
     stop-reason :: <create-process-stop-reason>)
     => (result :: <boolean>)
  start-profiling (application);
  #f
end method;



// Method to Profile an Application
// ================================
//
define method profile-application(name :: <string>,
                                  #key arguments = #f, interval = 50)
                              => ()
  let application = #f;
  if (arguments)
    application := make (<profile-target>, application: name,
                         arguments: arguments);
  else
    application := make (<profile-target>, application: name);
  end if;
  control-profiling (application, interval: interval);
  manage-running-application (application,
                              stop-reason-callback: process-stop-reason);
  process-profile-data (application);
  print-profile-results (application, name, interval);
end;



// Some hacks to implement the command line interface
// ==================================================
//
// First, something to convert a string to an integer.
//
define method string-to-integer (string :: <string>)
  let result = 0;
  for (c in string)
    if ((c < '0') | (c > '9'))
      format-out ("Error: %s not an integer\n", string);
    else
      let digit = as (<integer>, c) - as(<integer>, '0');
      result := result * 10 + digit;
    end if;
  end for;
  result
end method;

// Now something to parse the command line and start the profiler
//
define method profiler ()
  let args = command-arguments ();
  if (size (args) < 1)
    format-out ("Error: no command to profile");
  else
    let interval = 50;
    let i = 0;
    while ((args[i])[0] = '-')
      let s = args[i];
      if (s[1] = 'i')
        i := i + 1;
        interval := string-to-integer (args[i]);
      end if;
      i := i + 1;
    end while;

    if (i + 1 < size (args))
      profile-application (args[i], arguments: args[i + 1],
                           profile-interval: interval);
      else
        profile-application (args[i], profile-interval: interval);
      end if;
  end if;
end method;



// Starting things off...
// ======================
//
profiler();
