Module:    environment-application-commands
Synopsis:  The application commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Profile application command

define class <profile-application-command> (<project-command>)
  constant slot %stop? :: <boolean> = #f,
    init-keyword: stop?:;
  constant slot %allocation? :: <boolean> = #f,
    init-keyword: allocation?:;
end class <profile-application-command>;

define command-line profile => <profile-application-command>
    (summary:       "control application profiling",
     documentation: "Control the profiling of the application.")
  flag stop = "Stop profiling [start by default]";
  flag allocation = "Class-based allocation profiling [off by default]";
end command-line profile;

define method do-execute-command
    (context :: <environment-context>,
     command :: <profile-application-command>)
 => ()
  let project = context.context-project;
  case
    command.%stop? =>
      stop-profiling-application(project);
      message(context, "Profiling stopped");
    command.%allocation? =>
      let sampling-options
	= make(<profile-sampling-options>,
	       style: #"allocation",
	       rate:  50);
      let snapshot-options
	= make(<profile-snapshot-options>,
	       values: #[#"cpu", #"wall", #"page-faults", #"allocation",
			 #"class"],
	       depth: #f);
      let options
	= make(<profile-options>,
	       sampling-options: sampling-options,
	       snapshot-options: snapshot-options);
      start-profiling-application(project, options: options);
      message(context, "Started allocation profiling...");
    otherwise =>
      start-profiling-application(project);
      message(context, "Started profiling...");
  end
end method do-execute-command;


///---*** To do

/*
  heap-statistics
*/


/// Profiling commands

define command-group profiling into environment
    (summary: "profiling commands",
     documentation: "Commands to profile an application.")
  command  profile;
end command-group profiling;
