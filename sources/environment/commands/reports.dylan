Module:    environment-commands
Synopsis:  The commands provided by the environment
Author:	   Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Documentation report
///
/// This isn't in environment-reports because it is only useful here.

define constant $documentation-report
  = make(<report-info>,
	 name:    #"documentation",
	 title:   "Command line documentation",
	 class:   <report>,
	 edition: #"basic");

define function command-reports
    () => (reports :: <sequence>)
  concatenate(vector($documentation-report), available-reports())
end function command-reports;

define function write-command-documentation
    (stream :: <stream>, context :: <environment-context>)
  let main-group = context.context-command-group;
  let subgroups = collect-command-info(main-group, <command-group>, sort?: #t);
  write-command-group-documentation
    (stream, context, main-group, show-contents?: #f);
  for (subgroup :: <command-group> in subgroups)
    unless (subgroup == main-group)
      write-command-group-documentation(stream, context, subgroup)
    end
  end
end function write-command-documentation;

define function write-command-group-documentation
    (stream :: <stream>, context :: <environment-context>, 
     group :: <command-group>,
     #key show-contents? :: <boolean> = #t)
 => ()
  let separator = make(<byte-string>, size: 60, fill: '-');
  format(stream, "%s\n\n", separator);
  display-help(stream, context, group);
  if (show-contents?)
    let properties
      = collect-command-info(group, <command-property>, sort?: #t);
    for (property :: <command-property> in properties)
      format(stream, "\n");
      display-help(stream, context, property, group: group);
    end;
    let command-lines
      = collect-command-info(group, <basic-command-line>, sort?: #t);
    for (command-line :: <basic-command-line> in command-lines)
      format(stream, "\n%s\n\n", command-line.command-info-title);
      display-help(stream, context, command-line, group: group);
      format(stream, "\n")
    end
  end
end function write-command-group-documentation;


/// Report properties

define class <reports-property> (<environment-property>)
end class <reports-property>;

define command-property reports => <reports-property>
  (summary:       "Available reports",
   documentation: "The set of available reports.")
end command-property reports;

define method show-property
    (context :: <environment-context>, property :: <reports-property>)
 => ()
  let stream = context.context-server.server-output-stream;
  print-table(stream, as(<vector>, command-reports()),
	      label-key: method (info :: <report-info>)
			   as-uppercase(as(<string>, info.report-info-name))
			 end,
	      value-key: report-info-title,
	      sort?:     #t)
end method show-property;


/// Export

define class <export-command> (<project-command>)
  constant slot %report :: <symbol>,
    required-init-keyword: report:;
  constant slot %file :: false-or(<file-locator>) = #f,
    init-keyword: file:;
  constant slot %format :: false-or(<symbol>) = #f,
    init-keyword: format:;
end class <export-command>;

define command-line export => <export-command>
    (summary:       "exports project information",
     documentation: "Exports information from the specified project.")
  argument report :: <symbol> = "the report to generate";
  keyword  file :: <file-locator>  = "the filename for the report";
  keyword  format :: <symbol> = "the format for the report";
end command-line export;

define sealed method do-execute-command
    (context :: <environment-context>, command :: <export-command>)
 => ()
  let project  = context.context-project;
  let report   = command.%report;
  let filename = command.%file;
  let format   = command.%format | #"text";
  let info     = find-report-info(report);
  case
    (report == #"documentation") =>
      if (filename)
	with-open-file (stream = filename, direction: #"output")
	  write-command-documentation(stream, context)
	end;
	message(context, "Wrote documentation to %s", filename)
      else
	let stream = context.context-server.server-output-stream;
	write-command-documentation(stream, context)
      end;
    info =>
      if (~member?(format, info.report-info-formats))
        command-error("The %s report does not support the '%s' format",
                      report, format);
      end;
      let report
	= make(info.report-info-class,
	       project: project,
	       format: format);
      if (filename)
	with-open-file (stream = filename, direction: #"output")
	  write-report(stream, report)
	end;
	message(context, "Wrote %s to %s",
		info.report-info-title, filename)
      else
	let stream = context.context-server.server-output-stream;
	write-report(stream, report)
      end;
    otherwise =>
      command-error("No such report '%s'", report);
  end
end method do-execute-command;


/// Project commands

define command-group reports
    (summary: "report commands",
     documentation: "Commands for report generation.")
  property reports;
  command  export;
end command-group reports;
