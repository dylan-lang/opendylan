module:        devel-dbg-ui
synopsis:      The execution of stop-mode debugger commands
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/*

This file executes debugger commands in terms of the Debugger Manager
and not the access path. This file is not a temporary hack (unless
you count the fact that the console debugger is just one BIG temporary
hack!)

*/

define variable *last-debugger-command* = make (<null-command>);
define variable *opening-command* = make (<null-command>);
define variable *quiet?* = #f;
define variable *large-object-table* = make(<table>);

///// SPEW-LOCAL-VARIABLE

define method spew-local-variable 
     (var :: <lexical-variable>, val :: <remote-value>, 
      frame-index :: <integer>)
 => ()
  let name = var.lexical-variable-name;
  let cross-reference = #f;
  let path = *open-application*.debug-target-access-path;
  let (page, offset) = page-relative-address(path, val);
  let subtable = element(*large-object-table*, page, default: #f);
  if (subtable)
    cross-reference := element(subtable, offset, default: #f);
  end if;
  if (cross-reference)
    format-out("   %s  =  (Same as %s in frame %d)\n",
               name, tail(cross-reference), head(cross-reference));
  else
    let (classname, slots, vals, repeats, rname, rvals) =
      describe-dylan-object(*open-application*, val);
    let slot-count =
      slots.size + (if (repeats) repeats else 0 end);
    if (slot-count >= *current-debugger-options*.large-object-threshold)
     let subtable = element(*large-object-table*, page, default: #f);
      if (subtable)
        subtable[offset] := pair(frame-index, name);
      else
        subtable := make(<table>);
        subtable[offset] := pair(frame-index, name);
        *large-object-table*[page] := subtable;
      end if;
    end if;
    format-out("   %s  =  Instance of %s\n", name, as-uppercase(classname));
    for (i from 0 below size(slots))
      let hex-object =
        remote-value-as-string(path, vals[i], 16);
      let printed-object =
        debugger-print-object 
          (*open-application*, vals[i], length: 10, level: 3);
      format-out
        ("     0x%s : %s = %s\n", hex-object, slots[i], printed-object);
    end for;
    if (repeats)
      for (i from 0 below repeats)
        let hex-object =
          remote-value-as-string(path, rvals[i], 16);
        let printed-object =
          debugger-print-object
           (*open-application*, rvals[i], length: 10, level: 3);
        format-out
          ("     0x%s : %s[%d] = %s\n", hex-object, rname, i, printed-object);
      end for;
    end if
  end if
end method;


///// SPEW-THREAD

define method spew-thread (t :: <remote-thread>, signalling? :: <boolean>) 
    => ()
  let (dylan-thread?, tname) = 
    remote-thread-information(*open-application*, t);
  let stack-frame = first-stack-frame(*open-application*, t);
  *large-object-table* := make(<table>);
  let i = 1;
  let style =
    if (dylan-thread?) "Dylan Thread" else "Foreign Thread" end if;
  let relevant-option =
    if (signalling?)
      *current-debugger-options*.signalling-thread-detail
    else
      *current-debugger-options*.other-threads-detail
    end if;
  let signal-message = 
    if (signalling?) "signalling" else "non-signalling" end if;
  debugger-message("-------------- Report for %s %s \"%s\""
                   "--------------\n",
                   signal-message, style, tname);
  unless (relevant-option == #"summary")
    while (stack-frame & (i <= *current-debugger-options*.stack-trace-limit))
      debugger-print-stack-frame
        (stack-frame, i, full?: (relevant-option == #"verbose"));
      format-out("\n");
      if (instance?(stack-frame, <call-frame>) & 
          dylan-call-frame?(*open-application*, stack-frame) &
          (relevant-option == #"verbose"))
        let (live-variables, live-values) =
          live-frame-lexical-variables(*open-application*, stack-frame);
        for (v from 0 below size(live-variables))
          block ()
            spew-local-variable(live-variables[v], live-values[v], i);
	  exception (<remote-access-violation-error>)
            debugger-message("Memory access problems printing this variable.");
  	  exception (<error>)
            debugger-message("Internal error printing this variable.");
	  end block;
          format-out("\n");
        end for;
        format-out("\n");
      end if;
      stack-frame := previous-stack-frame(*open-application*, stack-frame);
      i := i + 1;
    end while;
  end unless;
  execute-debugger-command(make(<enumerate-registers-command>));
  format-out("\n");
  if (dylan-thread?)
    execute-debugger-command(make(<view-multiple-values-command>));
    format-out("\n");
  end if;
end method;


///// SPEW-BUG-REPORT
//    Prints a verbose bug-report to the console. This includes a list of
//    all threads, a list of loaded DLLs, and verbose stack traces with
//    all local variables described.

define method spew-bug-report () => ()
  let path = *open-application*.debug-target-access-path;
  debugger-message("++++++++++++++ DYLAN APPLICATION FAILURE REPORT "
                   "++++++++++++++");
  debugger-message("++++++++++ (Generated by the Dylan Console Debugger) "
                   "+++++++++++");
  debugger-message("");
  debugger-message("APPLICATION NAME: %s", 
                   *opening-command*.application-filename);
  debugger-message("COMMAND ARGUMENTS: %s",
                   *opening-command*.application-command-arguments);
  debugger-message ("Application broke in thread %s.",
                     *open-application*.stopped-thread.thread-name);
  debugger-message("");
  debugger-message("--------------------------------------------------------");
  format-out("\n");
  execute-debugger-command(make(<enumerate-threads-command>));
  format-out("\n");
  execute-debugger-command(make(<enumerate-libraries-command>));
  format-out("\n");
  unless (*current-debugger-options*.signalling-thread-detail == #"ignore")
    spew-thread(*open-application*.stopped-thread, #t);
  end unless;
  for (i from 1 to size(*open-application*.application-thread-table))
    let this-thread = *open-application*.application-thread-table[i - 1];
    *open-application*.selected-thread := i;
    unless (this-thread == *open-application*.stopped-thread)
      unless (*current-debugger-options*.other-threads-detail == #"ignore")
        spew-thread(this-thread, #f);
      end unless;
    end unless;
  end for;
  debugger-message("--------------------------------------------------------");
  debugger-message("");
  debugger-message("Summary of signalled condition:");
  if (*open-application*.most-recent-interesting-stop-reason)
    react-to-stop-reason
      (*open-application*, 
       *open-application*.most-recent-interesting-stop-reason);
  else
    debugger-message("     (No stop reason information available)")
  end if;
  debugger-message("");
  debugger-message("++++++++++++++++++++++++++ END "
                   "++++++++++++++++++++++++++");
  format-out("\n");
end method;


///// CHECKED-REACT-TO-STOP-REASON

define method checked-react-to-stop-reason
                (application :: <application>,
                 stop-reason :: <stop-reason>)
                 => (interested? :: <boolean>)
  let interested? = #t;
  block()
    interested? := react-to-stop-reason(application, stop-reason);
  exception (illegal-access :: <remote-access-violation-error>)
    debugger-message ("*** ERROR: Tried to read illegal runtime memory.");
    debugger-message ("           Aborted stop-reason processing.");
  exception (bp-error :: <debug-point-error>)
    debugger-message ("*** ERROR: Failed to set or clear a breakpoint.");
    debugger-message ("           Aborted stop-reason processing.");
  end block;
  if (interested?)
    application.most-recent-interesting-stop-reason := stop-reason
  end if;
  interested?
end method;


/////
///// BORING ONES AND CATCH-ALLS
/////


define method execute-debugger-command
    (command :: <null-command>)
end method;

define method execute-debugger-command
    (command :: <debugger-command>) => ()
  debugger-message ("Sorry, that command is not implemented in this version.");
end method;

define method execute-debugger-command
    (command :: <bad-command>) => ()
  debugger-message ("Unknown or ill-formed command.");
end method;

define method execute-debugger-command
                (command :: <last-command-command>) => ()
  execute-debugger-command (*last-debugger-command*);
end method;

define method execute-debugger-command
    (command :: <bug-report-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    spew-bug-report()
  else
    debugger-message("No open application to generate report from!");
  end if
end method;

define method execute-debugger-command (command :: <shutup-command>) => ()
  *quiet?* := ~*quiet?*
end method;

define method execute-debugger-command
                (command :: <cry-in-the-dark-command>) => ()
  *last-debugger-command* := command;
  debugger-message("Command                                Shorthand");
  debugger-message("-------                                ---------");
  debugger-message("open <filename> [<arguments>]              o");
  debugger-message("--- Opens a new program.");
  debugger-message("restart                                    r");
  debugger-message("--- Restarts the program from the beginning.");
  debugger-message("continue [<restart-number>]                c");
  debugger-message("--- Lets the program continue, or invoke a restart.");
  debugger-message("options                                    opt");
  debugger-message("--- Lists available restarts on the selected thread.");
  debugger-message("    (Restarts are numbered for the 'continue' command).");
  debugger-message("kill                                       k");
  debugger-message("--- Kills off the program.");
  debugger-message("quit                                       q");
  debugger-message("--- Kills off any program and exits the debugger.");
  debugger-message("step (over | out | into)                   s");
  debugger-message("--- Steps through execution at source-code level.");
  debugger-message("break <expression>                         b");
  debugger-message("--- Sets a breakpoint on an address.");
  debugger-message("clear [<expression>]                       cl");
  debugger-message("--- Clears one or all breakpoints.");
  debugger-message("break line <number> of <filename>          b");
  debugger-message("--- Sets a breakpoint on a source code line.");
  debugger-message("clear line <number> of <filename>          b");
  debugger-message("--- Clears a breakpoint on a source code line.");
  debugger-message("clear <breakpoint-code>                    cl");
  debugger-message("--- Clears a breakpoint.");
  debugger-message("disable <breakpoint-code>                  bd");
  debugger-message("--- Disables a breakpoint.");
  debugger-message("enable <breakpoint-code>                   be");
  debugger-message("--- Enables a breakpoint.");
  debugger-message("ignore <breakpoint-code> [for <count>]     bi");
  debugger-message("--- Prevents a breakpoint from stopping the program.");
  debugger-message("trace <expression>                         t");
  debugger-message("--- Traces call to a function.");
  debugger-message("untrace [<expression>]                     ut");
  debugger-message("--- Stops tracing calls to one or all functions.");
  debugger-message("breakpoints                                bp");
  debugger-message("--- Lists all breakpoint information.");
  debugger-message("backtrace [<limit>] [verbose]              bt [v]");
  debugger-message("--- Displays the stack, with local variables if verbose.");
  debugger-message("report                                     br");
  debugger-message("--- Generates a fully verbose bug report.");
  debugger-message("frame [<frame-number>]                     f");
  debugger-message("--- Selects and prints a stack frame.");
  debugger-message("up                                         u");
  debugger-message("--- Selects the stack frame above the current one.");
  debugger-message("down                                       d");
  debugger-message("--- Selects the stack frame below the current one.");
  debugger-message("top");
  debugger-message("--- Selects the topmost stack frame.");
  debugger-message("bottom                                     bot");
  debugger-message("--- Selects the bottom-most stack frame.");
  debugger-message("mv");
  debugger-message("--- Prints the contents of the thread's MV buffer.");
  debugger-message("print <expression>                         p");
  debugger-message("--- Prints a value as a dylan object.");
  debugger-message("describe <expression>                      des");
  debugger-message("--- Shows the values of all slots in a dylan object.");
  debugger-message("explode <expression>                       exp");
  debugger-message("--- Shows all the elements in a dylan collection object.");
  debugger-message("walk <expression>");
  debugger-message("--- Traces a dylan data structure, and gives statistics.");
  debugger-message("evaluate <expression>                      eval");
  debugger-message("--- Evaluates a dylan expression remotely.");
  debugger-message("    Also sets up a restart for aborting the evaluation.");
  debugger-message("[from <pathname>] download {<object-filename>, ...}");
  debugger-message("--- Downloads the contents of object files into program.");
  debugger-message("    Used to test interactive downloading.");
  debugger-message("display <expression> [lines <count>]       disp");
  debugger-message("--- Displays memory, in longhex unless directive given:");
  debugger-message("    (Append command with ascii, bytes or disasm).");
  debugger-message("show (threads | libraries | registers)     sh");
  debugger-message("--- Lists the known threads, libraries or registers.");
  debugger-message("in thread <thread-index>");
  debugger-message("--- Selects a specific thread for debugging.");
  debugger-message("in module <module-name>");
  debugger-message("--- Selects a default dylan module name.");
  debugger-message("in library <library-name>");
  debugger-message("--- Selects a default dylan library name.");
  debugger-message("set <expression> <expression>");
  debugger-message("--- Sets a memory location with a value.");
  debugger-message("nearto <expression>                        n");
  debugger-message("--- Shows three close symbols to an address.");
  debugger-message("profile (Please see documentation)         prof");
  debugger-message("--- Controls the profiling engine.");
  debugger-message("help                                       h");
  debugger-message("--- Displays this list, obviously.");
  debugger-message("shutup                                     su");
  debugger-message("--- Switches non-essential output on/off.");
end method;


/////
///// OPENING AND CONTROLLING APPLICATIONS
/////


define method execute-debugger-command
   (command :: <enumerate-processes-command>) => ()
  debugger-message("Active debuggable processes on %s",
                   command.command-connection.connection-hostname);
  debugger-message("-------------------------------------------------------");
  debugger-message("");
  do-processes
    (method (p :: <remote-process>)
       debugger-message("%10s   %s", 
                        p.remote-process-system-identifier,
                        p.remote-process-name);
     end method,
     command.command-connection);
end method;

define method get-process-by-id
   (connection :: <debugger-connection>, id :: <string>)
     => (p? :: false-or(<remote-process>))
  block (return)
    do-processes
      (method (p :: <remote-process>) => ()
         if (p.remote-process-system-identifier = id)
           return(p)
         end if
       end method,
       connection);
    return(#f)
  end block;
end method;

define method execute-debugger-command
   (command :: <open-application-command>) 
       => ()

  local method strip-extension (leaf :: <string>) => (stripped :: <string>)
    let i = 0;
    let limit = size(leaf);
    let stripped = "";
    while ((i < limit) & (leaf[i] ~== '.'))
      stripped := concatenate(stripped, add!("", leaf[i]));
      i := i + 1;
    end while;
    stripped;
  end method;

  local method strip-to-leaf(filename :: <string>) => (leaf :: <string>)
    let i = size(filename) - 1;
    let leaf = "";
    while((i >= 0) & (filename[i] ~== ':') & (filename[i] ~== '/') &
          (filename[i] ~== '\\'))
      leaf := concatenate(add!("", filename[i]), leaf);
      i := i - 1;
    end while;
    leaf;
  end method;

  *last-debugger-command* := command;
  let top-level = 
    *current-debugger-options*.component-name |
    strip-extension(strip-to-leaf(command.application-filename));

  if (*open-application*)
     debugger-message 
       ("Application already open. Kill it before opening another.");
  else
     block ()
       if (*current-debugger-options*.spawn-new-console?)
         debugger-message ("Begin debugging session: %s",
                           command.application-filename);
         debugger-message ("Attempting to start application...");
       end if;

       // Remember the command that we used to open this application.
       *opening-command* := command;
       *current-bp-cookie* := 0;
       install-first-compilation-context();
       *open-application* := 
           make (<application>,
                 start-in-own-shell?: 
                   *current-debugger-options*.spawn-new-console?,
                 compilation-context: *current-compilation-context*,
                 top-level-component-name: top-level,
                 debugger-connection: command.command-connection,
                 application: command.application-filename,
                 arguments: concatenate(top-level,
                                        " ",
                                        command.application-command-arguments));

       *open-application*.default-name-context.context-module := top-level;
       *open-application*.default-name-context.context-library := top-level;

       enable-stop-thread();
       keyboard-interrupt-polling-thread?() := #f;

       /* **************** RUN UNDER THE DEBUGGER *************** */

       manage-running-application (*open-application*,
                                   stop-reason-callback:
                                      checked-react-to-stop-reason,
                                   poll-for-stop-callback:
                                      check-stop-button,
                                   ready-to-continue-callback:
                                      process-stop-mode);

       /* ******************************************************* */

       if (*current-debugger-options*.spawn-new-console?)
         debugger-message ("Finished debugging %s", 
                           command.application-filename);
         *open-application* := #f;
       end if;

     exception (pants :: <access-path-creation-error>)
         debugger-message ("Could not open %s",
                           command.application-filename);
         *open-application* := #f;
     end block;
  end if
end method;


define method execute-debugger-command
   (command :: <attach-application-command>) 
       => ()

  let remote-process :: false-or(<remote-process>) =
    get-process-by-id(command.command-connection,
                      command.application-process-id);

  local method strip-extension (leaf :: <string>) => (stripped :: <string>)
    let i = 0;
    let limit = size(leaf);
    let stripped = "";
    while ((i < limit) & (leaf[i] ~== '.'))
      stripped := concatenate(stripped, add!("", leaf[i]));
      i := i + 1;
    end while;
    stripped;
  end method;

  local method strip-to-leaf(filename :: <string>) => (leaf :: <string>)
    let i = size(filename) - 1;
    let leaf = "";
    while((i >= 0) & (filename[i] ~== ':') & (filename[i] ~== '/') &
          (filename[i] ~== '\\'))
      leaf := concatenate(add!("", filename[i]), leaf);
      i := i - 1;
    end while;
    leaf;
  end method;

  *last-debugger-command* := command;
  if (~remote-process)
     debugger-message
       ("No such process with ID %s is running on this connection.",
        command.application-process-id);
     debugger-message
       ("Use the command LIST to see the running processes.");
  elseif (*open-application*)
     debugger-message 
       ("Application already open. Kill it before opening another.");
  else
    let top-level = 
      *current-debugger-options*.component-name |
      strip-extension(strip-to-leaf(remote-process.remote-process-name));
     block ()
       if (*current-debugger-options*.spawn-new-console?)
         debugger-message ("Begin debugging session: Process<%s> [%s]",
                           command.application-process-id, top-level);
         debugger-message ("Attempting to attach to running process...");
       end if;

       // Remember the command that we used to open this application.
       *opening-command* := command;
       *current-bp-cookie* := 0;
       *open-application* := 
           make (<application>,
                 compilation-context: *current-compilation-context*,
                 top-level-component-name: top-level,
                 debugger-connection: command.command-connection,
                 system-attachment-information: command.system-JIT-information,
                 process: remote-process);
 
       *open-application*.default-name-context.context-module := top-level;
       *open-application*.default-name-context.context-library := top-level;
       *current-debugger-options*.stop-at-system-initialization? := #f;

       enable-stop-thread();
       keyboard-interrupt-polling-thread?() := #f;

       /* **************** RUN UNDER THE DEBUGGER *************** */

       manage-running-application (*open-application*,
                                   stop-reason-callback:
                                      checked-react-to-stop-reason,
                                   poll-for-stop-callback:
                                      check-stop-button,
                                   ready-to-continue-callback:
                                      process-stop-mode);

       /* ******************************************************* */

       if (*current-debugger-options*.spawn-new-console?)
         debugger-message ("Finished debugging Process<%s>", 
                           command.application-process-id);
         *open-application* := #f;
       end if;

     exception (pants :: <access-path-creation-error>)
         debugger-message ("Could not attach to process <%s>",
                           command.application-process-id);
         *open-application* := #f;
     end block;
  end if
end method;

define method execute-debugger-command
    (command :: <profile-debug-command>) => ()
  *last-debugger-command* := command;
  *profiler-debug?* := ~*profiler-debug?*;
end method;

define method execute-debugger-command
    (command :: <profile-application-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    debugger-message("First kill the application you are currently debugging")
  else
    debugger-message("Begin profiling run: %s",
                     command.application-filename);
    debugger-message("Attempting to start application...");
    // TODO: Profile
    debugger-message("Finished profiling %s", command.application-filename);
    *open-application* := #f;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-inclusive-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let argument = profile-inclusive-command-argument(command);
    if (argument == #"unknown")
      debugger-message("Profile Inclusive = %=", 
		       application-profile-inclusive?(*open-application*));
    else
      application-profile-inclusive?(*open-application*) := argument;
    end if;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-exclusive-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let argument = profile-exclusive-command-argument(command);
    if (argument == #"unknown")
      debugger-message("Profile Exclusive = %=", 
		       application-profile-exclusive?(*open-application*));
    else
      application-profile-exclusive?(*open-application*) := argument;
    end if;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-within-invocation-command>)
  *last-debugger-command* := command;
  if (*open-application*)
    let (targets, objects, target-types) =
      compute-function-breakpoint-targets
        (*open-application*, command.profiler-command-function);
    if (targets = #[])
      debugger-message("Error: this does not yield any legal profile points.");
    else
      let catch-bps = make(<vector>, size: size(targets));
      let options = profiler-command-options(command);
      for (i from 0 below size(targets))
        catch-bps[i] := 
          make(<profile-on-tracepoint>,
               address: targets[i],
               callback: profile-on-callback,
               return-callback: profile-off-callback,
	       profiler-options: options);
        register-debug-point(*open-application*, catch-bps[i]);
      end for;
      *open-application*.profiler-entry-breakpoints := catch-bps;
      debugger-message("Successfully set up the profiler.");
    end if
  else
    debugger-message("No open application. Use open <filename>.");
  end if
end method;

define method execute-debugger-command
    (command :: <start-profiling-open-application-command>) => ()
  *last-debugger-command* := command;
  ignore(command.profiler-command-threads);
  if (*open-application*)
    let profiler-run = *open-application*.application-profiler-run;
    if (profiler-run & ~profiler-run.profiler-results)
      debugger-message("*** This application is already being profiled.");
      debugger-message("    Use 'profile results' to print results.");
    else
      let options = command.profiler-command-options;
      start-profiling-with-options(*open-application*, options);
      debugger-message("Switched on the profiler.");
      debugger-message("Use 'profile results' to print results.");
    end if
  else
    debugger-message("No open application. Use 'open' or 'profile' <name>");
  end if
end method;

define method execute-debugger-command
    (command :: <stop-profiling-open-application-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let profiler-run = *open-application*.application-profiler-run;
    if (profiler-run)
      unless (profiler-run.profiler-results)
	stop-profiling(*open-application*);
	debugger-message("Switched off the profiler.");
      end;
      debugger-message("Processing results, please wait...");
      process-profile-data(*open-application*);
      debugger-message("Done processing results.");
      print-profile-results(*open-application*);
    else
      debugger-message("*** This application has not been profiled.");
      debugger-message("    There are no results to display");
    end if
  else
    debugger-message("No open application. Use 'open' or 'profile' <name>");
  end if
end method;

define method execute-debugger-command
    (command :: <stop-profiling-and-show-hotspots-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let profiler-run = *open-application*.application-profiler-run;
    if (profiler-run)
      unless (profiler-run.profiler-results)
	stop-profiling(*open-application*);
	debugger-message("Switched off the profiler.");
      end;
      debugger-message("Processing results, please wait...");
      process-profile-data(*open-application*);
      debugger-message("Done processing results.");
      print-profile-results(*open-application*);
    else
      debugger-message("*** This application has not been profiled.");
      debugger-message("    There are no results to display");
    end if
  else
    debugger-message("No open application. Use 'open' or 'profile' <name>");
  end if
end method;

define method execute-debugger-command
    (command :: <stop-profiling-and-ignore-results-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    if (*open-application*.application-profiler-run)
      stop-profiling(*open-application*);
      debugger-message("Switched off the profiler.");
      *open-application*.application-profiler-run := #f;
    else
      debugger-message("*** This application is not being profiled.");
    end if
  else
    debugger-message("No open application. Use 'open' or 'profile' <name>");
  end if
end method;

define method execute-debugger-command
    (command :: <profile-set-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let name = profile-set-command-name(command);
    let set  = profile-set-command-set(command);
    let sets = application-profile-sets(*open-application*);
    profile-set-name(set) := name;
    sets[name] := set;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-height-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let height 
      = profile-height-command-argument(command);
    if (height)
      application-profile-height(*open-application*) := height;
    else
      debugger-message
	("Profile Height = %d", 
	 application-profile-height(*open-application*));
    end if;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-limit-command>) => ()
  execute-profile-limit-command
    (command, application-profile-limit-0, application-profile-limit-0-setter);
  execute-profile-limit-command
    (command, application-profile-limit-1, application-profile-limit-1-setter);
  execute-profile-limit-command
    (command, application-profile-limit-2, application-profile-limit-2-setter);
end method;

define method execute-debugger-command
    (command :: <profile-limit-0-command>) => ()
  execute-profile-limit-command
    (command, application-profile-limit-0, application-profile-limit-0-setter);
end method;

define method execute-debugger-command
    (command :: <profile-limit-1-command>) => ()
  execute-profile-limit-command
    (command, application-profile-limit-1, application-profile-limit-1-setter);
end method;

define method execute-debugger-command
    (command :: <profile-limit-2-command>) => ()
  execute-profile-limit-command
    (command, application-profile-limit-2, application-profile-limit-2-setter);
end method;

define method execute-debugger-command
    (command :: <profile-top-n-command>) => ()
  execute-profile-top-n-command
    (command, application-profile-top-n-0, application-profile-top-n-0-setter);
  execute-profile-top-n-command
    (command, application-profile-top-n-1, application-profile-top-n-1-setter);
  execute-profile-top-n-command
    (command, application-profile-top-n-2, application-profile-top-n-2-setter);
end method;

define method execute-debugger-command
    (command :: <profile-top-n-0-command>) => ()
  execute-profile-top-n-command
    (command, application-profile-top-n-0, application-profile-top-n-0-setter);
end method;

define method execute-debugger-command
    (command :: <profile-top-n-1-command>) => ()
  execute-profile-top-n-command
    (command, application-profile-top-n-1, application-profile-top-n-1-setter);
end method;

define method execute-debugger-command
    (command :: <profile-top-n-2-command>) => ()
  execute-profile-top-n-command
    (command, application-profile-top-n-2, application-profile-top-n-2-setter);
end method;

define method execute-profile-top-n-command
    (command, profile-top-n :: <function>, profile-top-n-setter :: <function>)
 => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let top-n = profile-top-n-command-argument(command);
    if (top-n)
      profile-top-n(*open-application*) := top-n;
    else
      debugger-message
	("Profile Top-N = %d", profile-top-n(*open-application*));
    end if;
  end if;
end method;

define method execute-profile-limit-command
    (command, profile-limit :: <function>, profile-limit-setter :: <function>)
 => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let limit = profile-limit-command-argument(command);
    if (limit)
      profile-limit(*open-application*) := limit;
    else
      debugger-message
	("Profile Limit = %d", profile-limit(*open-application*));
    end if;
  end if;
end method;

define method application-profile-find-function
    (application :: <application>, ip :: <remote-value>)
 => (function :: <single-function-descriptor>)
  let tree = application-profile-functions(application);
  let node =
    if (tree)
      function-tree-find-node
	(tree, ip, rcurry(function-tree-make-node, application))
    else
      application-profile-functions(application)
	:= function-tree-make-node(#f, ip, application)
    end if;

  node.function-tree-function-descriptor
end method;

define method execute-debugger-command
    (command :: <profile-show-aggregates-command>) => ()
  *last-debugger-command* := command;
  let app = *open-application*;
  if (app)
    let expression = profile-show-aggregates-command-argument(command);
    let ips = compute-function-breakpoint-targets(app, expression);
    for (ip in ips)
      let descriptor = application-profile-find-function(app, ip);
      debugger-message("IP %=", ip);
      let filter     = application-profile-filter(app);
      let aggregates = application-profile-aggregates(app);
      let functions
       = compute-function-descriptor-aggregates(descriptor, filter, aggregates);
      for (function in functions)
        debugger-message("  %=", function-descriptor-display-name(function));
      end for;
    end for;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-sets-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let sets = application-profile-sets(*open-application*);
    profile-sets-print(sets);
  end if;
end method;

define method compute-dll-sets () => (res :: <sequence>)
  if (*open-application*)
    let dll-sets = 
      collecting ()
	local method collect-dll (library :: <remote-library>)
                let leaf = filename-strip-to-leaf(library-image-name(library));
                let name = as(<symbol>, as-lowercase(filename-strip-suffix(leaf)));
		collect(make(<profile-set-dll>, dll: name));
	      end method;
	do-libraries(collect-dll, *open-application*.debug-target-access-path);
	collect(make(<profile-set-dll>, dll: #"ntdll"));
	collect(make(<profile-set-dll>, dll: #"kernel32"));
      end collecting;
    dll-sets
  else
    #[]
  end if;
end method;

define method execute-profile-aggregates-command
    (command, profile-aggregates :: <function>, profile-aggregates-setter :: <function>)
 => ()
  *last-debugger-command* := command;
  if (*open-application*)
    block (blow)
      let lump-names = profile-aggregates-command-arguments(command);
      if (empty?(lump-names))
	let lumps = profile-aggregates(*open-application*);
	profile-sets-print(lumps);
      else
	let lumps = make(<table>);
	let sets  = application-profile-sets(*open-application*);
	for (lump-name in lump-names)
          if (lump-name == $tokenLibraries)
            let dlls = compute-dll-sets();
            for (dll in dlls)
              let name = profile-set-dll(dll);
              lumps[name] := dll;
              profile-set-name(dll) := name;
            end for;
          else
	    let lump-value = element(sets, lump-name, default: #f);
	    if (lump-value)
	      lumps[lump-name] := lump-value;
	    else 
	      debugger-message("Profile-set %= not found\n", lump-name);
	      blow();
	    end if
          end if;
	end for;
	profile-aggregates(*open-application*) := lumps;
      end if;
    end block;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-aggregates-command>) => ()
  execute-profile-aggregates-command
    (command, application-profile-aggregates-1, application-profile-aggregates-1-setter);
  execute-profile-aggregates-command
    (command, application-profile-aggregates-2, application-profile-aggregates-2-setter);
end method;

define method execute-debugger-command
    (command :: <profile-aggregates-1-command>) => ()
  execute-profile-aggregates-command
    (command, application-profile-aggregates-1, application-profile-aggregates-1-setter);
end method;

define method execute-debugger-command
    (command :: <profile-aggregates-2-command>) => ()
  execute-profile-aggregates-command
    (command, application-profile-aggregates-2, application-profile-aggregates-2-setter);
end method;

define method execute-profile-filter-command 
    (command, profile-filter :: <function>, profile-filter-setter :: <function>) => ()
  let set = profile-filter-command-argument(command);
  if (set)
    profile-filter(*open-application*) := set;
  elseif (*open-application*)
    let filter = profile-filter(*open-application*);
    if (filter)
      format-out(";;; ");
      profile-set-print(filter);
      format-out("\n");
    else
      debugger-message("No filter");
    end if;
  end if;
end method;

define method execute-debugger-command
    (command :: <profile-filter-command>) => ()
  execute-profile-filter-command
    (command, application-profile-filter-1, application-profile-filter-1-setter);
  execute-profile-filter-command
    (command, application-profile-filter-2, application-profile-filter-2-setter);
end method;

define method execute-debugger-command
    (command :: <profile-filter-1-command>) => ()
  execute-profile-filter-command
    (command, application-profile-filter-1, application-profile-filter-1-setter);
end method;

define method execute-debugger-command
    (command :: <profile-filter-2-command>) => ()
  execute-profile-filter-command
    (command, application-profile-filter-2, application-profile-filter-2-setter);
end method;

define method execute-debugger-command
    (command :: <kill-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
     debugger-message ("Sending kill command...");
     kill-application (*open-application*);
     debugger-message ("Debugger Manager thinks it worked.");
     *open-application*.still-in-stop-mode? := #f;
  else
     debugger-message ("No open application. Use open <filename>.");
  end if;
end method;   

define method execute-debugger-command
    (command :: <exit-debugger-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
     debugger-message ("Sending kill command...");
     kill-application (*open-application*);
     debugger-message ("Debugger Manager thinks it worked.");
     *open-application*.still-in-stop-mode? := #f;
  end if;
  *global-quit* := #t;
end method;   

define method execute-debugger-command
    (command :: <continue-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
     *open-application*.still-in-stop-mode? := #f;
     debugger-message ("Application continuing. Type Control-break or press STOP button if needed.");
  else
     debugger-message ("No open application. Use open <filename>.");
   end if
end method;

define method execute-debugger-command
    (command :: <thread-control-command>)

  *last-debugger-command* := command;
  if (*open-application*)
     debugger-message ("Thread-level control not yet implemented.");
     debugger-message ("Just use stop/continue/kill for control.");
     ignore(command.thread-number);
  else
     debugger-message ("No open application. Use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <restart-command>)

  *last-debugger-command* := command;
  if (*open-application*)
    debugger-message ("Disposing stored state...");
    restart-application(*open-application*);
    debugger-message 
      ("Killing and re-starting. Old windows might stay visible.");
    *open-application*.still-in-stop-mode? := #f;
  else
    execute-debugger-command(*opening-command*);
  end if
end method;


/////
///// SOURCE-CODE STEPPING
/////

define method execute-debugger-command
    (command :: <step-into-command>)
  *last-debugger-command* := command;
  if (*open-application*)
    if (instruct-thread-to-step-into(*open-application*,
                                     *open-application*.stopped-thread))
      *open-application*.still-in-stop-mode? := #f;
    else
      debugger-message("The Debugger Manager cannot step this thread.");
    end if;
  else
    debugger-message("There is no open application!");
  end if
end method;

define method execute-debugger-command
    (command :: <step-out-command>)
  *last-debugger-command* := command;
  if (*open-application*)
    let selected-index = *open-application*.current-frame-index;
    let stack = *open-application*.current-stack;
    let stack-size = size(stack);
    let call-frame-object =
      if ((selected-index > 0) & (selected-index <= stack-size) &
              (instance?(stack[selected-index - 1], <call-frame>)))
        stack[selected-index - 1]
      else
        #f
      end if;
    if (instruct-thread-to-step-out(*open-application*,
                                     *open-application*.stopped-thread,
                                    call-frame: call-frame-object))
      *open-application*.still-in-stop-mode? := #f;
    else
      debugger-message("The Debugger Manager cannot step this thread.");
    end if;
  else
    debugger-message("There is no open application!");
  end if
end method;

define method execute-debugger-command
    (command :: <step-over-command>)
  *last-debugger-command* := command;
  if (*open-application*)
    let selected-index = *open-application*.current-frame-index;
    let stack = *open-application*.current-stack;
    let stack-size = size(stack);
    let call-frame-object =
      if ((selected-index > 0) & (selected-index <= stack-size) &
              (instance?(stack[selected-index - 1], <call-frame>)))
        stack[selected-index - 1]
      else
        #f
      end if;
    if (instruct-thread-to-step-over(*open-application*,
                                     *open-application*.stopped-thread,
                                     call-frame: call-frame-object))
      *open-application*.still-in-stop-mode? := #f;
    else
      debugger-message("The Debugger Manager cannot step this thread.");
    end if;
  else
    debugger-message("There is no open application!");
  end if
end method;


/////
///// GETTING INFORMATION ABOUT THREADS AND DLLs (and registers)
/////

define method execute-debugger-command
    (command :: <enumerate-restarts-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
    let thread = *open-application*.stopped-thread;
    let selected-index = *open-application*.selected-thread;
    let table = *open-application*.application-thread-table;
    if ((selected-index > 0) & (selected-index <= size(table)))
      thread := table[selected-index - 1];
    end if;
    let rst-vector = available-restarts-for-thread(*open-application*, thread);
    let rst-count = size(rst-vector);
    if (rst-count == 0)
      debugger-message("This thread is not prepared to handle any restarts.");
    else
      debugger-message("This thread is prepared to handle restarts :-");
      for (i from 1 to rst-count)
         debugger-message("  %d : %s", i,
                          rst-vector[i - 1].remote-restart-description);
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if;
end method;

define method execute-debugger-command
    (command :: <continue-via-restart-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
    let thread = *open-application*.stopped-thread;
    let selected-index = *open-application*.selected-thread;
    let table = *open-application*.application-thread-table;
    if ((selected-index > 0) & (selected-index <= size(table)))
      thread := table[selected-index - 1];
    end if;
    let rst-vector = available-restarts-for-thread(*open-application*, thread);
    let rst-count = size(rst-vector);
    let index = command.command-restart-index;
    if ((index > 0) & (index <= rst-count))
      signal-restart-on-thread
         (*open-application*, thread, rst-vector[index - 1]);
      *open-application*.still-in-stop-mode? := #f;
     debugger-message
	("Application continuing. Type Control-break or press STOP button if needed.");
    else
      debugger-message("ERROR: That is not a valid restart on this thread!");
    end if
  else
    debugger-message("No open application. Use open <filename>.");
  end if;
end method;

define method execute-debugger-command
    (command :: <view-multiple-values-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
    let thread = *open-application*.stopped-thread;
    let selected-index = *open-application*.selected-thread;
    let table = *open-application*.application-thread-table;
    if ((selected-index > 0) & (selected-index <= size(table)))
      thread := table[selected-index - 1];
    end if;
    let val-vector = thread-current-mv-vector(*open-application*, thread);
    let val-count = size(val-vector);
    if (val-count == 0)
      debugger-message("The MV area for this thread is empty.");
    else
      debugger-message("Current MV Area for this thread:");
      for (i from 0 below val-count)
        let this-value = val-vector[i];
        let hex-object =
          remote-value-as-string(*open-application*.debug-target-access-path,
                                 this-value,
                                 16);
        let hist-index = add-history-value(this-value);
        let printable = debugger-print-object(*open-application*, this-value,
                                              length: 5, level: 2);
        format-out("   0x%s  $%d : MV[%d] = %s\n",
                   hex-object,
                   hist-index,
                   i,
                   printable);
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <enumerate-registers-command>) => ()

  let i = *open-application*.selected-thread;
  let count = size(*open-application*.application-thread-table);
  let rthread =
    if ((i >= 1) & (i <= count))
      *open-application*.application-thread-table[i - 1];
    else
      *open-application*.stopped-thread;
    end if;

  local method show-register-data (r :: <unassigned-remote-register>)
     let printable = as (<string>, r.register-name);
     let reg-active =
       active-register(*open-application*.debug-target-access-path,
                       rthread,
                       r);
     let val = read-value(*open-application*.debug-target-access-path,
                          reg-active);
     format-out ("   [%s] = %s   ", printable, formatted-hex-to-string(val));
     if (dylan-object?(*open-application*, val))
       format-out("%s",
                  debugger-print-object(*open-application*, val));
     else
       format-out("(Not a dylan object)");
     end if;
     format-out("\n");
  end method;

  *last-debugger-command* := command;
  if (*open-application*)
     debugger-message ("--- Register set for this thread:");
     do-registers (show-register-data,
                   *open-application*.debug-target-access-path);
  else
     debugger-message ("No open application.");
  end if
end method;
           
define method execute-debugger-command
    (command :: <enumerate-threads-command>) => ()

  local method show-thread-data (i :: <integer>, t :: <remote-thread>)
      let indicator =
        if (*open-application*.selected-thread == i)
           "[*]"
        else
           ""
        end if;

      let (dylan-thread?, dylan-name) =
        remote-thread-information(*open-application*, t);

      let id = 
        if (dylan-thread?)
          if (dylan-name = t.thread-name)
            "Anonymous Dylan thread"
          else
            format-to-string("Dylan thread \"%s\"", dylan-name)
	  end if
	else
          "Foreign thread"
	end if;
      debugger-message ("%d. Thread: %s (%s)  Priority: %d %s", i,
                        t.thread-name, id, t.thread-priority, indicator);
  end method;

  *last-debugger-command* := command;
  if (*open-application*)
     debugger-message ("Active application threads:");
     let i = 1;
     for (thr in *open-application*.application-thread-table)
         show-thread-data (i, thr);
         format-out("\n");
         i := i + 1;
     end for;
  else
     debugger-message ("No open application: use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <enumerate-libraries-command>) => ()

  let library-table = make(<string-table>);

  local method index-of-final-split (s :: <string>) =>
             (index :: <integer>)
          let i = s.size - 1;
          block (return)
            while (i >= 0)
              if ((s[i] == '/') | (s[i] == '\\'))
                return(i)
              else
                i := i - 1
              end if;
            end while;
            i;
          end block
        end method;

  local method split-string-at (s :: <string>, split-index :: <integer>)
                           => (s1 :: <string>, s2 :: <string>)
          let s1 = make(<string>, size: (split-index + 1));
          let s2 = make(<string>, size: (s.size - split-index - 1));
          for (i from 0 to split-index)
            s1[i] := s[i]
          end for;
          for (i from (split-index + 1) below s.size)
            s2[i - split-index - 1] := s[i]
          end for;
          values(s1, s2)
        end method;

  local method sort-into-table(lib :: <remote-library>) => ()
    let fullname = lib.library-image-name;
    let final-split = index-of-final-split(fullname);
    let (path, filename) = split-string-at(fullname, final-split);
    let libraries-on-this-path =
      element(library-table, path, default: #f);
    if (libraries-on-this-path)
      add!(libraries-on-this-path, pair(lib, filename));
    else
      libraries-on-this-path := make(<stretchy-vector>, size: 1);
      libraries-on-this-path[0] := pair(lib, filename);
      library-table[path] := libraries-on-this-path;
    end if;
  end method;

  local method record-less-than? (x, y) => (well? :: <boolean>)
          head(x).library-core-name < head(y).library-core-name
        end method;

  local method size-less-than? (x, y) => (well? :: <boolean>)
          library-table[x].size < library-table[y].size
        end method;

  *last-debugger-command* := command;
  if (*open-application*)
    debugger-message("Loaded remote libraries:");
    debugger-message("");
    do-libraries (sort-into-table,
                  *open-application*.debug-target-access-path);
    let directory-list = sort(key-sequence(library-table),
                              test: size-less-than?);
    for (directory in directory-list)
      unless (directory == "")
        debugger-message("Modules loaded from: %s", directory);
        let library-list = library-table[directory];
        let sorted-library-list = sort(library-list, test: record-less-than?);
        for (p in sorted-library-list)
          let l = head(p);
          let (majv, minv) = l.library-version;
          let base = l.library-base-address;
          debugger-message("    [Version %d.%d]  %=  %s",
                           majv, minv, base, tail(p));
        end for;
        debugger-message("");
      end unless
    end for;
  else
    debugger-message ("No open application: use open <filename>");
  end if;
end method;


/////
///// WALKING THE STACK
/////


define method execute-debugger-command
                (command :: <backtrace-command>)

  *last-debugger-command* := command;
  if (*open-application*)
     let thread = *open-application*.stopped-thread;
     let index = *open-application*.current-frame-index;
     let frame = #f;
     let full = command.verbose-trace?;
     while (index <= size (*open-application*.current-stack))
       frame := *open-application*.current-stack[index - 1];
       debugger-print-stack-frame (frame, index, full?: full);
       format-out ("\n");
       index := index + 1;
     end while;
     frame := previous-stack-frame (*open-application*, frame);
     while (frame)
       *open-application*.current-stack :=
           add! (*open-application*.current-stack, frame);
       debugger-print-stack-frame (frame, index, full?: full);
       format-out ("\n");
       index := index + 1;
       frame := previous-stack-frame (*open-application*, frame);
     end while;
  else
     debugger-message ("No open application: use open <filename>");
  end if;
end method;

define method execute-debugger-command
                (command :: <limited-backtrace-command>)

  *last-debugger-command* := command;
  let limit = command.frame-count-limit;
  if (*open-application*)
     let thread = *open-application*.stopped-thread;
     let index = *open-application*.current-frame-index;
     let counter = 0;
     let frame = #f;
     let full = command.verbose-trace?;
     while ((counter < limit) & 
            (index <= size (*open-application*.current-stack)))
       frame := *open-application*.current-stack[index - 1];
       debugger-print-stack-frame (frame, index, full?: full);
       format-out ("\n");
       index := index + 1;
       counter := counter + 1;
     end while;
     frame := previous-stack-frame (*open-application*, frame);
     while ((counter < limit) & (frame))
       *open-application*.current-stack :=
           add! (*open-application*.current-stack, frame);
       debugger-print-stack-frame (frame, index, full?: full);
       format-out ("\n");
       index := index + 1;
       counter := counter + 1;
       frame := previous-stack-frame (*open-application*, frame);
     end while;
  else
     debugger-message ("No open application: use open <filename>");
  end if;
end method;

define method execute-debugger-command
                (command :: <frame-command>)

  *last-debugger-command* := command;
  if (*open-application*)
     let index = *open-application*.current-frame-index;
     debugger-print-stack-frame 
          (*open-application*.current-stack[index - 1],
           index, full?: #t);
  else
     debugger-message ("No open application: use open <filename>");
  end if;
end method;

define method execute-debugger-command
                (command :: <up-command>)

       *last-debugger-command* := command;
       if (*open-application*)
          let index = *open-application*.current-frame-index;
          if (index > 1)
             index := index - 1;
             *open-application*.current-frame-index := index;
          end if;
          debugger-print-stack-frame
                  (*open-application*.current-stack[index - 1],
                   index, full?: #t);
       else
          debugger-message ("No open application: use open <filename>");
       end if;
end method;

define method execute-debugger-command
                (command :: <down-command>)

       *last-debugger-command* := command;
       if (*open-application*)
          let index = *open-application*.current-frame-index;
          if (index < size (*open-application*.current-stack))
             index := index + 1;
             *open-application*.current-frame-index := index;
          else
             let last-known-frame = *open-application*.current-stack[index - 1];
             let new-frame = previous-stack-frame (*open-application*,
                                                   last-known-frame);
             if (new-frame)
                index := index + 1;
                *open-application*.current-stack := 
                    add! (*open-application*.current-stack, new-frame);
                *open-application*.current-frame-index := index;
             end if;
          end if;
          debugger-print-stack-frame
                   (*open-application*.current-stack[index - 1],
                    index, full?: #t);
       else
          debugger-message ("No open application: use open <filename>");
       end if;
end method;

define method execute-debugger-command
                (command :: <top-command>)

       *last-debugger-command* := command;
       if (*open-application*)
          let index = *open-application*.current-frame-index;
          if (index > 1)
             index := 1;
             *open-application*.current-frame-index := index;
          end if;
          debugger-print-stack-frame
                  (*open-application*.current-stack[index - 1],
                   index, full?: #t);
       else
          debugger-message ("No open application: use open <filename>");
       end if;
end method;

define method execute-debugger-command
                (command :: <bottom-command>)

       *last-debugger-command* := command;
       if (*open-application*)
          let index = size (*open-application*.current-stack);
          let frame = *open-application*.current-stack[index - 1];
          frame := previous-stack-frame (*open-application*, frame);
          while (frame)
                *open-application*.current-stack :=
                    add! (*open-application*.current-stack, frame);
                frame := previous-stack-frame (*open-application*,
                                               frame);
          end while;
          index := size (*open-application*.current-stack);
          *open-application*.current-frame-index := index;
          debugger-print-stack-frame
                 (*open-application*.current-stack[index - 1],
                  index, full?: #t);
       else
          debugger-message ("No open application: use open <filename>");
       end if
end method;

define method execute-debugger-command
                (command :: <into-frame-command>)

  *last-debugger-command* := command;
  if (*open-application*)
     let index = command.frame-index;
     if ((index > 0) & 
           (index <= size (*open-application*.current-stack)))
        *open-application*.current-frame-index := index;
        debugger-print-stack-frame
          (*open-application*.current-stack[index - 1], index, full?: #t);
     elseif (index > 0)
        let gen-index = size(*open-application*.current-stack);
        let frame = *open-application*.current-stack[gen-index - 1];
        debugger-message("Hang on, I've got to build some stack...");
        frame := previous-stack-frame(*open-application*, frame);
        while ((frame) & (gen-index < index))
          add!(*open-application*.current-stack, frame);
          gen-index := gen-index + 1;
          frame := previous-stack-frame(*open-application*, frame);
        end while;
        if (gen-index == index)
          *open-application*.current-frame-index := index;
          debugger-print-stack-frame
            (*open-application*.current-stack[index - 1], index, full?: #t);
        else
          debugger-message("No such stack frame %d.", index);
        end if
     end if
  else
     debugger-message("No open application: use open <filename>");
  end if
end method;


/////
///// SETTING AND CLEARING BREAKPOINTS
/////


define method execute-debugger-command
                (command :: <breakpoints-command>)

  local method break-info(bp :: <console-debugger-breakpoint>)
               => (s :: <string>)
    let cookie-bit = format-to-string("%d", bp.debugger-breakpoint-cookie);
    let enabled-bit =
      if (bp.debugger-breakpoint-enabled?)
        " Enabled"
      else
        "Disabled"
      end if;
    concatenate("{", cookie-bit, ":", enabled-bit, "}");
  end method;

  *last-debugger-command* := command;
  if (*open-application*)
     debugger-message ("\nBreakpoints currently set:");

     for (bp in *open-application*.active-breaks)
         let rbp = bp.registered-break;
         let addr = bp.break-address;
         if (instance? (rbp, <C-function-entry-breakpoint>))
           debugger-message ("%s C Function breakpoint   @ %s", 
                             break-info(rbp), 
                             formatted-hex-to-string(addr));
         elseif (instance? (rbp, <dylan-function-entry-breakpoint>))
           debugger-message ("%s Dylan method breakpoint @ %s",
                             break-info(rbp), 
                             formatted-hex-to-string(addr));
         else
           debugger-message ("Something funny         @ %s",
                             formatted-hex-to-string(addr));
         end if
     end for;

     debugger-message ("\nTracepoints currently set:");

     for (tp in *open-application*.active-traces)
         let rtp = tp.registered-trace;
         let addr = tp.trace-address;
         if (instance? (rtp, <C-function-entry-trace>))
           debugger-message ("C Function trace        @ %s",
                             formatted-hex-to-string(addr));
         elseif (instance? (rtp, <dylan-function-entry-trace>))
           debugger-message ("Dylan method trace      @ %s",
                             formatted-hex-to-string(addr));
         else
           debugger-message ("Something funny         @ %s",
                             formatted-hex-to-string(addr));
         end if;
     end for;

     debugger-message ("\nSource Code Breakpoints currently set:");

     for (sbp in *open-application*.active-source-breaks)
       let rbp = sbp.registered-break;
       let addr = sbp.break-address;
       debugger-message("%s Line %d of %s           @ %s",
                        break-info(rbp),
                        rbp.source-breakpoint-linenumber,
                        rbp.source-breakpoint-filename,
                        formatted-hex-to-string(addr));
     end for;
  else
     debugger-message ("No open application. Use open <filename>.");
  end if
end method;

define method filename-strip-to-leaf(filename :: <string>) => (leaf :: <string>)
  let i = size(filename) - 1;
  let leaf = "";
  while((i >= 0) & (filename[i] ~== ':') & (filename[i] ~== '/') &
	(filename[i] ~== '\\'))
    leaf := concatenate(add!("", filename[i]), leaf);
    i := i - 1;
  end while;
  leaf;
end method;

define method filename-strip-suffix(filename :: <string>) => (leaf :: <string>)
  let pos = subsequence-position(filename, ".");
  if (pos)
    copy-sequence(filename, end: pos)
  else
    filename
  end if;
end method;

define method execute-debugger-command
    (command :: <break-source-command>) => ()

  *last-debugger-command* := command;
  if (*open-application*)
    let path = *open-application*.debug-target-access-path;

    // To resolve ambiguous leaf-filenames, first try to resolve the
    // source location using the selected DLL. If that succeeds, then
    // that's the address we'll use. Otherwise, perform a general search
    // and use whatever crops up, if anything.

    ignore(command.breakpoint-source-column);

    let (code-location, exact?) =
      resolve-source-location
        (path, 
         filename-strip-to-leaf(command.breakpoint-source-filename),
         line: command.breakpoint-source-linenumber,
         library: 
           find-library-called
             (*open-application*, command.breakpoint-source-dll));

    unless (code-location)
      let (general-code-location, general-exact?) =
        resolve-source-location
          (path, 
           filename-strip-to-leaf(command.breakpoint-source-filename),
           line: command.breakpoint-source-linenumber);
      code-location := general-code-location;
      exact? := general-exact?;
    end unless;

    if (code-location)
      let breaker = 
        find-active-source-break(*open-application*, code-location);
      if (breaker)
        debugger-message("There is already a breakpoint at this location.");
      else
        let bp = make(<source-breakpoint>,
                      address: code-location,
                      callback: source-breakpoint-callback,
                      filename: command.breakpoint-source-filename,
                      linenumber: command.breakpoint-source-linenumber);
        debugger-message("Source location breakpoint SET at %s.",
                         formatted-hex-to-string(code-location));
        add-active-source-break(*open-application*,
                                make(<source-break-description>,
                                     address: code-location, break: bp));
      end if
    else
      debugger-message("Error: Could not find an address for that location.");
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if;
end method;

define method execute-debugger-command
    (command :: <clear-source-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let path = *open-application*.debug-target-access-path;
    ignore(command.clear-source-column);
    let (code-location, exact?) =
      resolve-source-location(path, command.clear-source-filename,
                              line: command.clear-source-linenumber);
    if (code-location)
      let breaker = 
        find-active-source-break(*open-application*, code-location);
      if (breaker)
        debugger-message("Source location breakpoint CLEAR at %s.",
                         formatted-hex-to-string(code-location));
        remove-active-source-break(*open-application*, breaker);
      else
        debugger-message("There is no breakpoint at this location.");
      end if
    else
      debugger-message("Error: Could not find an address for that location.");
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if;
end method;

define method execute-debugger-command
    (command :: <break-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let path = *open-application*.debug-target-access-path;
    let (targets, objects, target-types) = 
      compute-function-breakpoint-targets(*open-application*, command.address);
    if (targets = #[])
      debugger-message("Error: this does not yield any legal breakpoints.");
    else
      for (i from 0 below size(targets))
        let (closest, offset) =
          symbol-relative-address(path, targets[i]);
        let actual-bp-address =
          if ((closest) & (target-types[i] ~== #"direct"))
            closest.first-frame-breakable-address    
          else
            targets[i]
          end if;
        if (find-active-break(*open-application*, actual-bp-address))
          debugger-message("Cannot set compound breakpoint at %s",
                           formatted-hex-to-string(actual-bp-address));
        else
          select(target-types[i])
            #"foreign" =>
              let bp = make(<C-function-entry-breakpoint>,
                            address: actual-bp-address,
                            callback: breakpoint-callback,
                            symbol: if (closest)
                                      closest.remote-symbol-name;
                                    else
                                      "Anonymous foreign breakpoint"
                                    end if);
              debugger-message("Foreign breakpoint SET at %s",
                               formatted-hex-to-string(actual-bp-address));
              add-active-break(*open-application*,
                               make(<break-description>,
                                    address: actual-bp-address, break: bp));

            #"direct" =>
              let bp = make(<C-function-entry-breakpoint>,
                            address: actual-bp-address,
                            callback: breakpoint-callback,
                            symbol: if (closest)
                                      closest.remote-symbol-name;
                                    else
                                      "Anonymous foreign breakpoint"
                                    end if);
              debugger-message("Direct breakpoint SET at %s",
                               formatted-hex-to-string(actual-bp-address));
              add-active-break(*open-application*,
                               make(<break-description>,
                                    address: actual-bp-address, break: bp));

            #"dylan-method-iep" =>
              let bp = make(<dylan-method-entry-breakpoint>,
                            address: actual-bp-address,
                            callback: breakpoint-callback,
                            symbol: if (closest)
                                      let (np, mp, lp, meth?, iepn?, mm, mn)
                                        = demangle-dylan-name
                                            (closest.remote-symbol-name);
                                      let pr = 
                                        concatenate(np, ":", mp, ":", lp);
                                      if (meth?)
                                        pr := concatenate(pr, "#", mn);
                                      end if;
                                      pr;
                                    else
                                      "Anonymous dylan method"
                                    end if,
                            function-object:
                              command.address.expression-value);
              debugger-message("Dylan method breakpoint SET at %s",
                               formatted-hex-to-string(actual-bp-address));
              add-active-break(*open-application*,
                               make(<break-description>,
                                    address: actual-bp-address, break: bp));

            otherwise =>
              debugger-message("Error: in calculating this breakpoint.");

          end select;
        end if;
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if
end method;

define method execute-debugger-command
    (command :: <break-class-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let class :: <simple-expression> = command.address;
    evaluate-simple-expression(*open-application*, class);
    if (class.expression-evaluated?)
      if (set-application-class-breakpoint
	    (*open-application*, 
	     *open-application*.stopped-thread,
	     class.expression-value))
	*open-application*.still-in-stop-mode? := #f;
      end
    else
      debugger-message("Error: Could not evaluate class expression.");
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end;
end method;

define method execute-debugger-command
    (command :: <clear-specific-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (targets, objects, target-types) = 
      compute-function-breakpoint-targets(*open-application*, command.address);
    if (targets = #[])
      debugger-message("Error: this does not yield any legal breakpoints.");
    else
      for (i from 0 below size(targets))
        let (closest, offset) =
          symbol-relative-address(*open-application*.debug-target-access-path,
                                  targets[i]);
        let actual-bp-address =
          if ((closest) & (target-types[i] ~== #"direct"))
            closest.first-frame-breakable-address
          else
            targets[i]
          end if;
        let breaker = find-active-break(*open-application*, actual-bp-address);
        if (~breaker)
          debugger-message("Cannot find any breakpoint at %s",
                           formatted-hex-to-string(actual-bp-address));
        else
          select(target-types[i])
            #"foreign" =>
              debugger-message("Foreign breakpoint CLEAR at %s",
                               formatted-hex-to-string(actual-bp-address));
              remove-active-break(*open-application*, breaker);

            #"direct" =>
              debugger-message("Direct breakpoint CLEAR at %s",
                               formatted-hex-to-string(actual-bp-address));
              remove-active-break(*open-application*, breaker);

            #"dylan-method-iep" =>
              debugger-message("Dylan method breakpoint CLEAR at %s",
                               formatted-hex-to-string(actual-bp-address));
              remove-active-break(*open-application*, breaker);

            otherwise =>
              debugger-message("Error: in calculating this breakpoint.");

          end select;
        end if;
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if
end method;

define method execute-debugger-command
    (command :: <clear-class-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let class :: <simple-expression> = command.address;
    evaluate-simple-expression(*open-application*, class);
    if (class.expression-evaluated?)
      if (clear-application-class-breakpoint
	    (*open-application*,
	     *open-application*.stopped-thread,
	     class.expression-value))
	*open-application*.still-in-stop-mode? := #f
      end
    else
      debugger-message("Error: Could not evaluate class expression.");
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end;
end method;

define method execute-debugger-command
    (command :: <clear-all-class-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    if (clear-application-class-breakpoints
	  (*open-application*,
	   *open-application*.stopped-thread))
      *open-application*.still-in-stop-mode? := #f
    end
  else
    debugger-message("No open application. Use open <filename>.");
  end;
end method;


define method execute-debugger-command
    (command :: <trace-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let path = *open-application*.debug-target-access-path;
    let (targets, objects, target-types) = 
      compute-function-breakpoint-targets(*open-application*, command.address);
    if (targets = #[])
      debugger-message("Error: this does not yield any legal tracepoints.");
    else
      for (i from 0 below size(targets))
        if (find-active-trace(*open-application*, targets[i]))
          debugger-message("Cannot set compound tracepoint at %s",
                           formatted-hex-to-string(targets[i]));
        else
          let (closest, previous, next) =
            nearest-symbols(path, targets[i]);
          select(target-types[i])
            #"foreign" =>
              let bp = make(<C-function-entry-trace>,
                            address: targets[i],
                            callback: entry-tracepoint-callback,
                            return-callback: return-tracepoint-callback,
                            symbol: if (closest)
                                      closest.remote-symbol-name;
                                    else
                                      "Anonymous foreign breakpoint"
                                    end if);
              debugger-message("Foreign tracepoint SET at %s",
                               formatted-hex-to-string(targets[i]));
              add-active-trace(*open-application*,
                               make(<trace-description>,
                                    address: targets[i], trace: bp));

            #"dylan-method-iep" =>
              let bp = make(<dylan-method-entry-trace>,
                            address: targets[i],
                            callback: entry-tracepoint-callback,
                            return-callback: return-tracepoint-callback,
                            symbol: if (closest)
                                      let (np, mp, lp, meth?, iepn?, mm, mn)
                                        = demangle-dylan-name
                                            (closest.remote-symbol-name);
                                      let pr = 
                                        concatenate(np, ":", mp, ":", lp);
                                      if (meth?)
                                        pr := concatenate(pr, "#", mn);
                                      end if;
                                      pr;
                                    else
                                      "Anonymous dylan method"
                                    end if,
                            function-object: objects[i]);
              debugger-message("Dylan method tracepoint SET at %s",
                               formatted-hex-to-string(targets[i]));
              add-active-trace(*open-application*,
                               make(<trace-description>,
                                    address: targets[i], trace: bp));

            otherwise =>
              debugger-message("Error: in calculating this tracepoint.");

          end select;
        end if;
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if
end method;

define method execute-debugger-command
    (command :: <untrace-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    debugger-message("Clearing all tracepoints....");
    for (tp in *open-application*.active-traces)
      deregister-debug-point(*open-application*, tp.registered-trace);
    end for;
    *open-application*.active-traces := make(<stretchy-vector>, size: 0);
    debugger-message("Done.");
  else
    debugger-message("No open application. Use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <clear-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    debugger-message("Clearing all breakpoints....");
    for (bp in *open-application*.active-breaks)
      deregister-debug-point(*open-application*, bp.registered-break);
    end for;
    *open-application*.active-breaks := make(<stretchy-vector>, size: 0);
    for (bp in *open-application*.active-source-breaks)
      deregister-debug-point(*open-application*, bp.registered-break);
    end for;
    *open-application*.active-source-breaks := 
      make(<stretchy-vector>, size: 0);
  else
    debugger-message("No open application. Use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <clear-cookie-breakpoint-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (bp, description) = 
      find-debugger-breakpoint(*open-application*, 
                               command.command-breakpoint-cookie);
    if (bp)
      debugger-message("Breakpoint %d Cleared.",
                       command.command-breakpoint-cookie);
      remove-active-break(*open-application*, description);
    else
      debugger-message("Error: Don't know about this breakpoint, mate.");
    end if
  else
    debugger-message("No open application. Use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <disable-breakpoint-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (bp, description) = 
      find-debugger-breakpoint(*open-application*, 
                               command.command-breakpoint-cookie);
    if (bp)
      if (bp.debugger-breakpoint-enabled?)
        bp.debugger-breakpoint-enabled? := #f;
        deregister-debug-point(*open-application*, bp);
        debugger-message("Breakpoint %d Disabled",
                         command.command-breakpoint-cookie);
      else
        debugger-message("Breakpoint is already disabled.");
      end if;
    else
      debugger-message("Error: Don't know about this breakpoint, mate.");
    end if
  else
    debugger-message("No open application. Use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <enable-breakpoint-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (bp, description) = 
      find-debugger-breakpoint(*open-application*, 
                               command.command-breakpoint-cookie);
    if (bp)
      if (bp.debugger-breakpoint-enabled?)
        if (bp.debugger-breakpoint-always-ignore?)
          debugger-message("No longer ignoring breakpoint %d",
                           command.command-breakpoint-cookie);
          bp.debugger-breakpoint-always-ignore? := #f;
        else
          debugger-message("Breakpoint is already enabled.");
        end if;
      else
        debugger-message("Breakpoint %d Enabled",
                         command.command-breakpoint-cookie);
        bp.debugger-breakpoint-enabled? := #t;
        register-debug-point(*open-application*, bp);
      end if
    else
      debugger-message("Error: Don't know about this breakpoint, mate.");
    end if
  else
    debugger-message("No open application. Use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <blanket-ignore-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (bp, description) = 
      find-debugger-breakpoint(*open-application*, 
                               command.command-breakpoint-cookie);
    if (bp)
      debugger-message("Ignoring breakpoint %d",
                       command.command-breakpoint-cookie);
      bp.debugger-breakpoint-always-ignore? := #t;
    else
      debugger-message("Error: Don't know about this breakpoint, mate.");
    end if
  else
    debugger-message("No open application. Use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <counted-ignore-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (bp, description) = 
      find-debugger-breakpoint(*open-application*, 
                               command.command-breakpoint-cookie);
    if (bp)
      debugger-message("Ignoring the next %d hits on breakpoint %d",
                       command.command-ignore-count,
                       command.command-breakpoint-cookie);
      bp.debugger-breakpoint-ignore? := #t;
      bp.debugger-breakpoint-ignore-count := 0;
      bp.debugger-breakpoint-ignore-level := command.command-ignore-count;
    else
      debugger-message("Error: Don't know about this breakpoint, mate.");
    end if
  else
    debugger-message("No open application. Use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <untrace-specific-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    let (targets, objects, target-types) = 
      compute-function-breakpoint-targets(*open-application*, command.address);
    if (targets = #[])
      debugger-message("Error: this does not yield any legal tracepoints.");
    else
      for (i from 0 below size(targets))
        let breaker = find-active-trace(*open-application*, targets[i]);
        if (~breaker)
          debugger-message("Cannot find any tracepoint at %s",
                           formatted-hex-to-string(targets[i]));
        else
          select(target-types[i])
            #"foreign" =>
              debugger-message("Foreign tracepoint CLEAR at %s",
                               formatted-hex-to-string(targets[i]));
              remove-active-trace(*open-application*, breaker);

            #"dylan-method-iep" =>
              debugger-message("Dylan method tracepoint CLEAR at %s",
                               formatted-hex-to-string(targets[i]));
              remove-active-trace(*open-application*, breaker);

            otherwise =>
              debugger-message("Error: in calculating this tracepoint.");

          end select;
        end if;
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if
end method;


/////
///// ALTERING THE CONTEXT OF COMMANDS
/////


define method execute-debugger-command
    (command :: <change-thread-command>)
  *last-debugger-command* := command;
  if (*open-application*)
     if ((command.new-thread >= 1) &
         (command.new-thread
            <= size (*open-application*.application-thread-table)))
        let rthread = *open-application*.application-thread-table
                         [command.new-thread - 1];
        *open-application*.selected-thread := command.new-thread;
        build-top-of-stack (*open-application*, rthread);
     else
        debugger-message ("No such thread: %d", command.new-thread);
     end if
  else
     debugger-message ("No open application. Use open <filename>");
  end if;
end method;


define method execute-debugger-command
   (command :: <change-module-command>)
     
  *last-debugger-command* := command;
  if (*open-application*)
     *open-application*.default-name-context.context-module :=
              command.new-module;
  else
     debugger-message ("No open application. Use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <mode-C-command>)

  *last-debugger-command* := command;
  if (*open-application*)
     *open-application*.language-mode := $language-mode-C;
  else
     debugger-message ("No open application, hence no language mode!");
  end if
end method;

define method execute-debugger-command
    (command :: <mode-Dylan-command>)

  *last-debugger-command* := command;
  if (*open-application*)
     *open-application*.language-mode := $language-mode-Dylan;
  else
     debugger-message ("No open application, hence no language mode!");
  end if
end method;


/////
///// PRINTING AND DESCRIBING REMOTE VALUES
/////


define method execute-debugger-command
    (command :: <print-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    evaluate-simple-expression(*open-application*, command.object);
    if (command.object.expression-evaluated?)
      let hist-index = add-history-value(command.object.expression-value);
      let hex-object =
        remote-value-as-string(*open-application*.debug-target-access-path,
                               command.object.expression-value,
                               16);
      format-out("0x%s  $%d : ", hex-object, hist-index);
      if (dylan-object?(*open-application*, command.object.expression-value))
        format-out("%s\n",
                   debugger-print-object(*open-application*,
                                         command.object.expression-value,
                                         length: 20, level: 3));
      else
        format-out("0x%s\n", hex-object);
      end if;
    else
      debugger-message("Error: Could not evaluate this expression.");
    end if;
  else
    debugger-message ("No open application: use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <explode-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    evaluate-simple-expression(*open-application*, command.object);
    if (command.object.expression-evaluated?)
      if (dylan-object?(*open-application*, command.object.expression-value))
        let (key-vector, value-vector) = 
          remote-collection-inspect(*open-application*,
                                    command.object.expression-value);
        if (value-vector.size == 0)
          debugger-message("No elements.");
        else
          for (i from 0 below value-vector.size)
            let key-hist-index =
	      if (key-vector) add-history-value(key-vector[i]) else #f end if;
            let val-hist-index =
              add-history-value(value-vector[i]);
            let key-hex-object =
              if (key-vector)
                remote-value-as-string
                  (*open-application*.debug-target-access-path, key-vector[i],
                   16);
	      else
                #f
	      end if;
            let val-hex-object =
              remote-value-as-string
                (*open-application*.debug-target-access-path, value-vector[i],
                 16);
            let key-printed =
              if (key-vector)
                debugger-print-object(*open-application*, key-vector[i]);
	      else
                #f
	      end if;
            let val-printed =
              debugger-print-object(*open-application*, value-vector[i]);
            if (key-vector)
              format-out("0x%s  $%d : %s  -->  0x%s  $%d : %s\n",
                         key-hex-object, key-hist-index, key-printed,
                         val-hex-object, val-hist-index, val-printed);
	    else
              format-out("%4d  -->  0x%s  $%d : %s\n",
                         i, val-hex-object, val-hist-index, val-printed);
	    end if;
          end for;
        end if
      else
        debugger-message("Eh? Call that a collection? It's not even a dylan "
                         "value!");
      end if
    else
      debugger-message("Error: Could not evaluate this expression.");
    end if
  else
    debugger-message("No open application: use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <walk-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    evaluate-simple-expression(*open-application*, command.object);
    if (command.object.expression-evaluated?)
      let walker =
        make(<remote-walker>, target: *open-application*,
             root-object: command.object.expression-value,
             classes: command.command-class-set,
             modules: command.command-module-set,
             class-status: command.command-class-status,
             module-status: command.command-module-status);
      debugger-message("Scanning...");
      walk-from(walker, command.object.expression-value);
      debugger-message("Done.");
      debugger-message("Generating Results...");
      display-walk-results(walker);
    else
      debugger-message("Could not evaluate this expression.");
    end if;
  else
    debugger-message("Erm...where's your application then, mate?");
  end if
end method;

define method execute-debugger-command
    (command :: <describe-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    evaluate-simple-expression(*open-application*, command.object);
    if (command.object.expression-evaluated?)
      if (dylan-object?(*open-application*, command.object.expression-value))
        let (cname, slots, vals, repeats, rname, rvals) =
          describe-dylan-object (*open-application*, 
                                 command.object.expression-value);
        format-out ("class = %s\n", as-uppercase(cname));
        for (i from 0 below size(slots))
          let hex-object =
            remote-value-as-string(*open-application*.debug-target-access-path,
                                   vals[i],
                                   16);
          let hist-index = add-history-value (vals[i]);
          format-out ("0x%s  $%d : %s = %s\n",
                      hex-object,
                      hist-index,
                      slots[i],
                      debugger-print-object (*open-application*,
                                             vals[i],
                                             length: 4, level: 1));
        end for;
        if (repeats)
          for (i from 0 below repeats)
            let hist-index = add-history-value (rvals[i]);
            let hex-object =
              remote-value-as-string
                (*open-application*.debug-target-access-path,
                 rvals[i],
                 16);
            format-out ("0x%s  $%d : %s[%d] = %s\n",
                        hex-object,
                        hist-index,
                        rname,
                        i,
                        debugger-print-object (*open-application*,
                                               rvals[i],
                                               length: 4, level: 1));
          end for
        end if
      else
        debugger-message("Error: Foreign object. Cannot describe");
      end if;
    else
      debugger-message("Error: Could not evaluate this expression.");
    end if;
  else
    debugger-message ("No open application: use open <filename>");
  end if
end method;

define method print-evaluator-c-result
    (application :: <application>, bp :: <c-interaction-returned>,
     thread :: <remote-thread>)
  => (interested? :: <boolean>)
  let c-result = bp.c-function-result;
  let hex-object = remote-value-as-string(application.debug-target-access-path,
                                          c-result,
                                          16);
  format-out("<C Evaluator> 0x%s\n", hex-object);
  #t;
end method;

define method print-evaluator-result
    (application :: <application>, bp :: <dylan-return-breakpoint>,
     thread :: <remote-thread>)
       => (interested? :: <boolean>)
  let dylan-result = bp.dylan-function-result;
  let (remote-result-class,
       remote-result-slots,
       remote-result-values,
       remote-result-repeats,
       remote-result-repeat-slot,
       remote-result-repeat-values) = describe-dylan-object
                                         (application, dylan-result);
  if (remote-result-repeats)
    format-out("<Evaluator> ");
    for (i from 0 below remote-result-repeats)
      let this-value = remote-result-repeat-values[i];
      let hex-object =
        remote-value-as-string(application.debug-target-access-path,
                               this-value,
                               16);
      let hist-index = add-history-value(this-value);
      format-out("0x%s  $%d : %s\n            ",
                 hex-object,
                 hist-index,
                 debugger-print-object(application, this-value,
                                       length: 4, level: 1));
    end for;
    format-out("\n");
  else
    let hist-index = add-history-value(dylan-result);
    let hex-object =
      remote-value-as-string(application.debug-target-access-path,
                             dylan-result,
                             16);
    format-out("<Evaluator> 0x%s  $%d : ", hex-object, hist-index);
    format-out("%s\n",
               debugger-print-object(application, dylan-result,
                                     length: 4, level: 1));
  end if;
  #t
end method;

define method execute-debugger-command
    (command :: <nearto-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    evaluate-simple-expression(*open-application*, command.target-address);
    if (command.target-address.expression-evaluated?)
      let path = *open-application*.debug-target-access-path;
      let (closest, previous, next)
        = nearest-symbols(path, command.target-address.expression-value);
      let (iclose, ioffset)
        = symbol-table-symbol-relative-address
            (*open-application*.debug-target-symbol-table,
             command.target-address.expression-value);

      if (closest)
        debugger-message ("Closest is %s at %s",
                          closest.remote-symbol-name,
                          formatted-hex-to-string
                                  (closest.remote-symbol-address));
      end if;
      if (previous)
        debugger-message ("Which is preceded by %s at %s",
                          previous.remote-symbol-name,
                          formatted-hex-to-string
                                  (previous.remote-symbol-address));
      end if;
      if (next)
        debugger-message ("And followed by %s at %s",
                          next.remote-symbol-name,
                          formatted-hex-to-string
                                   (next.remote-symbol-address));
      end if;
      if (iclose)
        debugger-message ("Closest (including interactive) is %s at %s",
                          iclose.remote-symbol-name,
                          formatted-hex-to-string
                                  (iclose.remote-symbol-address));
      end if;
    else
      debugger-message("Error: Could not evaluate this expression.");
    end if
  else
    debugger-message("No open application. Use open <filename>");
  end if;
end method;

define method execute-debugger-command
    (command :: <evaluate-expression-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    if (instance?(command.expression, <simple-expression>))
      execute-debugger-command(make(<print-command>, 
                                    object: command.expression));
    elseif (instance?(command.expression, <function-expression>))
      if (evaluate-as-far-as-possible(*open-application*, command.expression))
        let function-bit = 
          command.expression.function-expression.expression-value;
        let args-bit =
          make(<vector>, 
               size: size(command.expression.function-arguments-list));
        let i = 0;
        let thread =
          if ((*open-application*.selected-thread > 0) &
              (*open-application*.selected-thread <=
                 size(*open-application*.application-thread-table)))
            *open-application*.application-thread-table
              [*open-application*.selected-thread - 1]
            else
              *open-application*.stopped-thread
            end if;
        for (arg-expression in command.expression.function-arguments-list)
          args-bit[i] := arg-expression.expression-value;
          i := i + 1;
        end for;
        if (dylan-object?(*open-application*, function-bit) &
            apply(invoke-dylan,
                  *open-application*,
                  thread,
                  function-bit,
                  print-evaluator-result,
                  args-bit))
          debugger-message("Evaluating Dylan Function Call...");
          *open-application*.still-in-stop-mode? := #f;
        elseif (apply(invoke-c,
                      *open-application*,
                      thread,
                      function-bit,
                      print-evaluator-c-result,
                      args-bit))
          debugger-message("Evaluating C Function Call...");
          *open-application*.still-in-stop-mode? := #f;
        else
          debugger-message("Error: Cannot perform interaction");
        end if;
      else
        debugger-message("Error: some arguments could not be evaluated");
      end if;
    else
      debugger-message("Error: Cannot evaluate this expression");
    end if
  else
    debugger-message("No open application: use open <filename>");
  end if;
end method;


/////
///// MEMORY EXAMINATION
/////


define method formatted-hex-to-string
    (hex-object :: <remote-value>) => (str :: <string>)
  if (*open-application*)
    concatenate 
      ("0x", remote-value-as-string
               (*open-application*.debug-target-access-path, hex-object, 16));
  else
    "0x????????"
  end if;
end method;


define method formatted-byte-to-string
    (hex-object :: <integer>) => (str :: <string>);
  if (*open-application* & (hex-object >= 0) & (hex-object < 256))
    let first-attempt = format-to-string("0x%2x", hex-object);
    if (first-attempt[2] == ' ') first-attempt[2] := '0' end if;
    first-attempt;
  else
    "0x??"
  end if
end method;


define method execute-debugger-command
    (command :: <display-from-command>) => ()

  local method get-that-offset(opcode) => (offset :: <integer>)
    let offset = 0;
    block ()
      offset := general-opcode-offset(opcode);
    exception(<error>)
      // Potentially no applicable methods!!!
      offset := 0;
    end block;
    offset;
  end method;

  local method find-that-symbol (address)
                   => (maybe-sym :: false-or(<string>), offset :: <integer>)
    let maybe-sym = #f;
    let offset = 0;
    let machine-word-address = #f;
    block ()
      machine-word-address := as(<machine-word>, address);
    exception(<error>)
      machine-word-address := as(<machine-word>, 0);
    end block;
    // Now this is utterly tits-up. We have a <machine-word> for the address,
    // and we are going to pass it to a function that expects a <remote-value>
    // as the address. Now guess what - these two types are equivalent, but
    // that's an implementation detail that we shouldn't be aware of at this
    // level. Oh balls!
    if (*open-application*)
      let table = *open-application*.debug-target-symbol-table;
      let (remote-sym, remote-offset) =
        symbol-table-symbol-relative-address(table, machine-word-address);
      if (remote-sym)
        maybe-sym := remote-sym.remote-symbol-name;
        offset := remote-offset;
      end if
    end if;
    values(maybe-sym, offset);
  end method;

  if (*open-application*)
    evaluate-simple-expression
        (*open-application*, command.starting-address);
    let path = *open-application*.debug-target-access-path;
    if (command.starting-address.expression-evaluated?)
      select(command.display-format)
        #"word" =>
          let indexer = 0;
          let starter = command.starting-address.expression-value;
          for (this-line from 0 below command.line-count)
            format-out("%s :: ", 
                       formatted-hex-to-string
                         (indexed-remote-value(starter, indexer)));
            for (this-column from 0 to 3)
              let addr = indexed-remote-value(starter, indexer);
              let this-word = 
                read-value(*open-application*.debug-target-access-path, addr);
              format-out("%s  ", formatted-hex-to-string(this-word));
              indexer := indexer + 1;
            end for;
            format-out("\n");
          end for;

        #"byte" =>
          let indexer = 0;
          let starter = command.starting-address.expression-value;
          let byte-count = command.line-count * 8;
          let byte-string = 
            read-byte-string(*open-application*.debug-target-access-path,
                             starter,
                             byte-count);
          let bvec = as(<byte-vector>, byte-string);
          for (this-line from 0 below command.line-count)
            format-out("%s :: ",
                       formatted-hex-to-string
                         (byte-indexed-remote-value(starter, indexer)));
            for (this-column from 0 to 7)
              format-out("%s  ", 
                         formatted-byte-to-string(bvec[indexer]));
              indexer := indexer + 1;
            end for;
            format-out("\n");
          end for;

        #"ascii" =>
          let indexer = 0;
          let starter = command.starting-address.expression-value;
          let byte-count = command.line-count * 16;
          let byte-string = 
            read-byte-string(*open-application*.debug-target-access-path,
                             starter,
                             byte-count);
          let bvec = as(<byte-vector>, byte-string);
          for (this-line from 0 below command.line-count)
            format-out("%s :: ",
                       formatted-hex-to-string
                         (byte-indexed-remote-value(starter, indexer)));
            for (this-column from 0 to 15)
              if ((bvec[indexer] > 31) & (bvec[indexer] < 128))
                format-out("%c ", as(<character>, bvec[indexer]));
              else
                format-out(". ");
              end if;
              indexer := indexer + 1;
            end for;
            format-out("\n");
          end for;

        #"disassembly" =>
          let indexer = 0;
          let starter = command.starting-address.expression-value;
          let byte-count = command.line-count * 16;
          let byte-string = 
            read-byte-string(*open-application*.debug-target-access-path,
                             starter,
                             byte-count);
          let bvec = as(<byte-vector>, byte-string);
          let (opcodes, new-index) =
            decode-opcodes(bvec, 0, size(bvec) - 1);
          let opcode-offsets = map(get-that-offset, opcodes);
          let integer-address = #f;
          block ()
            integer-address := 
              as-integer(command.starting-address.expression-value);
          exception (<error>)
            integer-address := 0;
          end block;
          let str-list = 
            opcodes-to-string(opcodes, integer-address, find-that-symbol);
          debugger-message("Disassembly from %s",
                           formatted-hex-to-string(starter));
          let limit =
            if (str-list.size > command.line-count)
              command.line-count
            else
              str-list.size
            end if;
          for (i from 0 below limit)
            let current-addr = 
               byte-indexed-remote-value(starter, opcode-offsets[i]);
            let (sym, offset) =
               symbol-table-symbol-relative-address
                  (*open-application*.debug-target-symbol-table,
                   current-addr);
            if (sym & (offset == 0))
              debugger-message("%s %s",
                               formatted-hex-to-string(current-addr),
                               if (sym.remote-symbol-library)
                                 let dll = sym.remote-symbol-library;
                                 let dll-name = 
                                   as-uppercase(dll.library-core-name);
                                 let sym-name = sym.remote-symbol-name;
                                 concatenate(dll-name, "!", sym-name);
                               else
                                 let dll-name = "INTERACTIVE";
                                 let sym-name = sym.remote-symbol-name;
                                 concatenate(dll-name, "!", sym-name);
                               end if)
            end if;
            debugger-message("%s %s", 
                             formatted-hex-to-string(current-addr),
                             str-list[i]);
          end for;

      end select;
    else
      debugger-message("Error: Could not evaluate this expression.");
    end if;
  else
    debugger-message("No open application: use open <filename>");
  end if;
end method;


/////
///// SETTING UP EXCEPTIONS TO DEBUG AT FIRST CHANCE
/////


define method execute-debugger-command
    (command :: <exceptions-command>) => ()
  if (*open-application*)
    let path = *open-application*.debug-target-access-path;
    let exception-list = receivable-first-chance-exceptions(path);
    if (size(exception-list) == 0)
      debugger-message("This command is not relevant to your application.");
    else
      debugger-message("The following exception types can be debugged at");
      debugger-message("first chance. For each one, please type:");
      debugger-message("     Y - To debug at first chance.");
      debugger-message("     N - To ignore at first chance.");
      format-out("\n");
      for (this-exception-type in exception-list)
        let name = exception-name(path, this-exception-type);
        format-out("       %s ", name);
        if (receiving-first-chance?(path, this-exception-type))
          format-out(" (Currently debugging at first chance) ?\n");
        else
          format-out(" (Currently ignoring at first chance) ?\n");
        end if;
        let valid-answer = #f;
        let answer = #f;
        let attempts = 0;
        let prompt = "     [Y or N] > ";
        while (~valid-answer)
          format-out("%s", prompt);
          attempts := attempts + 1;
          answer := read-line(*standard-input*);
          format-out("\n");
          if ((instance?(answer, <string>)) &
              (size(answer) > 0) &
              (as-uppercase(answer[0]) == 'Y')) 
            valid-answer := #t;
            receiving-first-chance?(path, this-exception-type) := #t;
          elseif ((instance?(answer, <string>)) &
              (size(answer) > 0) &
              (as-uppercase(answer[0]) == 'N')) 
            valid-answer := #t;
            receiving-first-chance?(path, this-exception-type) := #f;
          end if;
        end while;
      end for;
    end if;
  else
    debugger-message("No open application. Use open <filename>.");
  end if;
end method;


/////
///// ASSIGNING A VALUE INTO MEMORY
/////


define method execute-debugger-command
    (command :: <set-command>) => ()
  if (*open-application*)
    evaluate-simple-expression(*open-application*,
                               command.set-command-lhs);
    evaluate-simple-expression(*open-application*,
                               command.set-command-rhs);
    if (command.set-command-lhs.expression-has-lvalue?)
      if (command.set-command-rhs.expression-evaluated?)
        block ()
          write-value(*open-application*.debug-target-access-path,
                      command.set-command-lhs.expression-lvalue,
                      command.set-command-rhs.expression-value);
          debugger-message("Successfully assigned value in runtime");
        exception (pants :: <remote-access-violation-error>)
          debugger-message("Illegal assignment operation in application");
        end block;
      else
        debugger-message("Error: Could not evaluate right operand to a value");
      end if;
    else
      debugger-message("Error: Could not evaluate left operand to an address");
    end if;
  else
    debugger-message("No open application: use open <filename>");
  end if
end method;


/////
///// DOWNLOADING A BUNCH OF OBJECT FILES INTERACTIVELY
/////

define method execute-debugger-command
    (command :: <download-object-files-command>) => ()
  if (*open-application*)
    // Locate and read <coff-file> objects for each filename.
    let actual-coff-files = make(<stretchy-vector>);
    for (filename in command.command-object-filename-sequence)
      block ()
        debugger-message("Reading and installing (snigger!) %s ...",
                         filename);
        let cf = read-coff-file(filename);
        add!(actual-coff-files, cf);
      exception (<error>)
        debugger-message("Error: Could not read %s", filename);
      end block;
    end for;
    // Call the interactive downloader.
    block ()
      debugger-message("Beginning interactive download...");
      download-object-files
        (*open-application*, 
         actual-coff-files,
         library: *open-application*.default-name-context.context-library);
      debugger-message("Interactive download completed.");
    exception (<error>)
      debugger-message("Error: Interactive transaction did not work.");
    end block;
  else
    debugger-message("No open application to download into.");
  end if
end method;


/////
///// TOGGLING BETWEEN COMMAND LISTENER AND DYLAN LISTENER
/////

define method execute-debugger-command
    (command :: <switch-listener-modes-command>) => ()
  if (*open-application*)
    if (*current-listener-mode* == $listen-for-commands)
      *current-listener-mode* := $listen-for-dylan
    else
      *current-listener-mode* := $listen-for-commands
    end if
  else
    debugger-message("You don't wanna do that! (Open an application first).");
  end if
end method;

define method execute-debugger-command
    (command :: <add-print-directive-command>) => ()
  unless (*class-lookup-table*)
    build-class-lookup-table()
  end unless;
  add-inspection-extension(command.new-directive);
  debugger-message("Added print directive for %s from %s",
                   command.new-directive.print-directive-class,
                   command.new-directive.print-directive-module);
end method;


///// REMOTE DEBUGGING COMMANDS

define method execute-debugger-command
    (command :: <enumerate-open-connections-command>) => ()
  debugger-message("Current Open Debugger Connections.");
  debugger-message("----------------------------------\n");
  do-open-debugger-connections
    (method (dc :: <debugger-connection>) => ()
       let desc = describe-debugger-connection(dc);
       debugger-message("%s", desc);
     end method);
end method;

define method execute-debugger-command
    (command :: <attempt-connection-command>) => ()
  block()
    debugger-message("Attempting to open a connection to %s....",
                     command.command-network-address);
    make(<remote-debugger-connection>,
         network-address: command.command-network-address,
         password: command.command-password);
    debugger-message("Done.");
  exception (<open-debugger-connection-failure>)
    debugger-message("Failed to establish connection.");
  exception (<open-debugger-connection-password-mismatch>)
    debugger-message("You have not supplied the correct password.");
  end block;
end method;

