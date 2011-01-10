Module:    tool-invoke
Synopsis:  A standalone program to invoke Functional Developer tools on 
	   specification files.
Author:    7/98 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method main () => ()
  let handler <tool-warning-condition> =
    method (c :: <tool-warning-condition>, next :: <function>) => ()
      format-out("%s.\n", c);
      if (~ c.tool-warning-recoverable?)
        exit-application(2);
      end if;
    end method;
  local method yn-handler (c :: <tool-yes-no-question>, next :: <function>) 
		       => (answer :: <boolean>)
    format-out("%s (y/n) ", c);
    force-output(*standard-output*);
    let resp = read-line(*standard-input*);
    if (resp.size = 0) yn-handler(c, next)
    elseif (resp.first = 'y' | resp.first = 'Y') #t
    elseif (resp.first = 'n' | resp.first = 'N') #f
    else yn-handler(c, next)
    end if;
  end method yn-handler;
  let handler <tool-yes-no-question> = yn-handler;

  let args = application-arguments();
  if (size(args) < 1 | size(args) > 2)
    format-out("Usage: %s <specification-file-name> [<project-file-name>]\n", 
      	       application-name());
    exit-application(1);
  end if;
  let spec-file = as(<file-locator>, args[0]);
  let project-file = element(args, 1, default: #f);
  if (project-file)
    project-file := as(<file-locator>, project-file);
  end if;
  
  let tool-name = tool-name-from-specification(spec-file);
  let tool = tool-find(tool-name);
  if (tool)
    let (success?, modified-projects) = tool(spec-file, project-file, #f);
    if (~success?)
      format-out("Tool failed!\n");
      exit-application(3);
    end if;
    format-out("Modified projects: %=.\n", modified-projects);
  else
    format-out("Tool \"%s\" could not be found.\n", as(<string>, tool-name));
  end if;
end method main;

main();
