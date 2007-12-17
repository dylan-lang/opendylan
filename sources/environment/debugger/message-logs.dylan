Module:    environment-debugger
Author:    Jason Trenouth, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Thread message log

define sealed class <thread-message-log> (<object>)
  sealed slot thread-message-log-buffer :: <buffer>;
  sealed slot thread-message-log-stream :: <interval-stream>;
end class <thread-message-log>;

define sealed method make 
    (class == <thread-message-log>,
     #key project :: <project-object>, thread :: false-or(<thread-object>))
 => (log :: <thread-message-log>)
  project-thread-message-log(project, thread)
    | next-method()
end method;

define sealed method initialize 
    (log :: <thread-message-log>,
     #key project :: <project-object>, thread :: false-or(<thread-object>))
 => ()
  let buffer = interactor-buffer-for-thread(project, thread);
  let stream :: <interval-stream>
    = make(<interval-stream>,
	   interval: buffer,
	   direction: #"output");
  log.thread-message-log-buffer := buffer;
  log.thread-message-log-stream := stream;
  let logs = project-thread-message-logs(project);
  element(logs, thread) := log;
end method initialize;

define function project-thread-message-logs
    (project :: <project-object>)
 => (message-logs :: <object-table>)
  let properties = project.project-properties;
  get-property(properties, #"thread-message-logs", default: #f)
    | begin
	let logs = make(<object-table>);
	put-property!(properties, #"thread-message-logs", logs);
	logs
      end
end function project-thread-message-logs;

define function project-thread-message-log 
    (project :: <project-object>, thread :: <thread-object>)
 => (log :: false-or(<thread-message-log>))
  let logs = project-thread-message-logs(project);
  element(logs, thread, default: #f)
end function project-thread-message-log;

define function reset-project-thread-message-logs 
    (project :: <project-object>) => ()
  let logs = project-thread-message-logs(project);
  remove-all-keys!(logs);
end function reset-project-thread-message-logs;


/// Message printing

/// NB If we get an application-wide message, broadcast it to all
/// the threads.
/// 
/// ---*** DEBUGGER: This may be a bit costly, but we're probably
/// stopping because the user clicked on the stop button anyway. If it
/// proves too expensive then we can invent a holding pen for
/// broadcast messages that message logs grab the latest entries from
/// when they update.

define sealed method print-application-message
    (device == #f, project :: <project-object>, thread == #f, message :: <string>)
 => ()
end method print-application-message;

define sealed method print-application-message
    (device :: <symbol>, project :: <project-object>, thread == #f, message :: <string>)
 => ()
  let application = project.project-application;
  let threads = if (application) application.application-threads else #[] end;
  for (thread :: <thread-object> in threads)
    print-application-message(device, project, thread, message)
  end for;
end method print-application-message;

define sealed method print-application-message
    (device == #"environment", project :: <project-object>, thread :: <thread-object>, 
     message :: <string>)
 => ()
  let log :: <thread-message-log>
    = make(<thread-message-log>, project: project, thread: thread);
  let buffer :: <basic-shell-buffer> = log.thread-message-log-buffer;
  let stream :: <interval-stream>    = log.thread-message-log-stream;
  let last-node = buffer-end-node(buffer);
  let prev-node = node-previous(last-node);
  let section = prev-node & node-section(prev-node);
  let line
    = select (section by instance?)
	<dylanworks-shell-section> => section-output-line(section);
	<section>                  => section-end-line(section);
	otherwise                  => #f;
      end;
  if (line)
    stream-position(stream) := line-start(line);
    write(stream, message);
    new-line(stream);
    if (instance?(section, <dylanworks-shell-section>))
      section-output-line(section) := bp-line(stream-position(stream))
    end;
    redisplay-section(section, editor: $environment-editor, centering: 1.0)
  else
    debug-message("Ignoring application message: %s", message)
  end
end method print-application-message;

define sealed method print-application-message
    (device == #"console", project :: <project-object>, thread :: <thread-object>, message :: <string>)
 => ()
  let thread-prefix :: <string>
    = if (thread)
	format-to-string("Thread %d", thread-index(project, thread))
      else
	environment-object-display-name(project, project, #f)
      end if;
  debug-message("%s: %s\n", thread-prefix, message)
end method print-application-message;
