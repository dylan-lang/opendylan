module: sockets
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *host* = #f;
define variable *remote-application* = #f;
define variable *remote-application-control* = #f;
define variable *sockets-report?* = #f;

define constant end-of-stream-signal = as(<character>, 127);
define constant ready-to-receive-data-signal = as(<character>, 128);
define constant error-signal = as(<character>, 129);
define constant abort-remote-listener-signal = as(<character>, 130);
define constant abort-remote-evaluator-signal = as(<character>, 131);
define constant restart-query-signal = as(<character>, 132);
define constant port = 999;

define method connect-to-server(host :: <string>)
  *host* := host;
end method connect-to-server;

define class <abort-listener> (<condition>)
end class <abort-listener>;

define class <abort-evaluator> (<condition>)
end class <abort-evaluator>;

define class <closed-socket> (<condition>)
end class <closed-socket>;

import-cl-functions(dylan(remote-recursive-dylan-listen)(as: lisp/dylan-listener));

import-cl-functions(dylan(remote-command-reader)(as: lisp/dylan-reader));

define method sockets-report? (string :: <string>)
  *sockets-report?* & format-out(string);
  force-output(*standard-output*);
end method sockets-report?;

define constant tcp-buffer-handshake =
  method (#key arg)
    send-command(as(<string>, arg));
    buffer-handshake-processor();
  end method;

define method buffer-handshake-processor ()
  let tcp-buffer-held? = *remote-application*.output-buffer.buffer-held?;
  sockets-report?("\n*** BUFFER-HANDSHAKE-PROCESSOR ***\n");
  block (continue)
    while (#t)
      let char = as(<character>, read-element(*remote-application*));
      select (char by \=)

	// REMOTE APPLICATION says all is OK - continue sending data from stream
	ready-to-receive-data-signal =>
	  continue();

	// REMOTE APPLICATION is reporting an error - start a new listener
	error-signal => 
	  *remote-application*.output-buffer.buffer-held? := #f;
	  block ()
	    sockets-report?("\n*** ERROR SIGNAL ***\n");
	    lisp/dylan-listener(receive-string-from-application());
	  exception (<abort-listener>)
	  end;
	  *remote-application*.output-buffer.buffer-held? := tcp-buffer-held?;

	// REMOTE APPLICATION is invoking a restart query
	restart-query-signal =>
	  *remote-application*.output-buffer.buffer-held? := #f;
	  sockets-report?("\n*** RESTART QUERY SIGNAL ***\n");
	  remote-restart-query();
	  *remote-application*.output-buffer.buffer-held? := tcp-buffer-held?;

	// REMOTE APPLICATION is aborting from this listener loop
	abort-remote-listener-signal =>
	  sockets-report?("\n*** ABORTING REMOTE LISTENER LOOP ***\n");
	  signal(make(<abort-listener>));

	// REMOTE APPLICATION is aborting from this listener evaluator
	abort-remote-evaluator-signal =>
	  sockets-report?("\n*** ABORTING REMOTE LISTENER EVALUATOR***\n");
	  signal(make(<abort-evaluator>));

	// otherwise listen to REMOTE APPLICATION
	otherwise =>
	  format-out("%s", char);
	  force-output(*standard-output*);

      end
    end
  end;
  sockets-report?("\n*** CONTINUE SENDING DOSS ***\n");
end method buffer-handshake-processor;

define method remote-restart-query ()
  format-out("\ntype a form to be evaluated: ");
  block ()
    lisp/dylan-reader();
  exception (<restart>, description: "redo restart query")
    remote-restart-query();
  end;
  sockets-report?("\n*** EXITING RESTART QUERY HANDLER ***\n");
end method remote-restart-query;

define method connect-to-application (*executable* :: <string>)
  close-application();
  *remote-application* := make(<tcp-stream>,
			       host: *host*, port: port, element-type: <byte>);
  *remote-application-control* := make(<tcp-stream>,
				       host: *host*, port: port);
  send-string(*executable*);
  // deferred buffer-handshake setting
  *remote-application*.output-buffer-handshake := tcp-buffer-handshake;
  *remote-application*
end method connect-to-application;


define method send-string (a-string :: <string>)
  write(*remote-application*, as(<byte-vector>, a-string));
  force-output(*remote-application*)
end method send-string;

define method send-command (a-string :: <string>)
  if (~*remote-application-control*)
    signal(make(<closed-socket>))
  end;
  if (~empty?(a-string))
    write(*remote-application-control*, a-string);
  end;
  write-element(*remote-application-control*, ' ');
  force-output(*remote-application-control*)
end method send-command;

define method set-current-application(application)
  *remote-application* := application
end method set-current-application;

define method receive-string-from-application()
  let *listen-to-application* = #t;
  let the-string = "";
  while (*listen-to-application*)
    let char = as(<character>, read-element(*remote-application*));
    select (char by \=)
      end-of-stream-signal =>
	*listen-to-application* := #f;
      otherwise =>
	the-string := add(the-string, char);
    end
  end;
  reverse(the-string)
end method receive-string-from-application;

define method listen-to-application()
  let *listen-to-application* = #t;
  while (*listen-to-application*)
    let char = as(<character>, read-element(*remote-application*));
    select (char by \=)
      end-of-stream-signal =>
	*listen-to-application* := #f;
      otherwise =>
	format-out("%s", char);
	force-output(*standard-output*);
    end
  end
end method listen-to-application;

define method close-application()
  if (*remote-application*)
    close(*remote-application*);
    *remote-application* := #f
  end;
  if (*remote-application-control*)
    close(*remote-application-control*);
    *remote-application-control* := #f
  end
end method close-application;
