Module: asp-view
Author:	Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Debugging support
define variable $start-ticks :: <integer> = current-timestamp();

define function delta () => (r :: <integer>) 
  current-timestamp() - $start-ticks 
end;

define function log (#rest format-args) => ()
  apply(debug-message, format-args);
  let msg = format-to-string("%d: %s",
			     delta(), apply(format-to-string, format-args));
  let log-name = format-to-string("aspview-%x.log", $start-ticks);
  with-open-file (log-file = log-name, direction: #"output",
		                       if-exists: #"append",
		                       if-does-not-exist: #"create")
    write(log-file, msg);
  end;
end;


log("ASP Viewer Starting up.\n");
