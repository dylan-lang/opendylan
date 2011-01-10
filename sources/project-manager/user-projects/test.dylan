Module: user-projects
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant format-out = access(format-out, format-out);

define function lookup-project(key :: <symbol>)
  let handler (<project-not-found>) = condition-handler;
  lookup-named-project(key);
end;

define function condition-handler(condition :: <project-not-found>,
				  next-handler)
  format-out("Condition: %=\n", condition);
  signal(make(<find-project-location-restart>, 
	      location:
		as(<file-locator>,
		   "/u/roman/dylan/lib/test-project/test-project.hdp")));
end;
