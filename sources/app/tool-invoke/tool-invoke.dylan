Module:    tool-invoke
Synopsis:  A standalone program to invoke Open Dylan tool plugins, good for
           use while developing new plugins.
Author:    7/98 Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// To use this tool to develop a new plugin, replace "use protobuf-tool;" in
// library.dylan with the name of your plugin library and then rebuild.

// TODO(cgay): add third command-line arg to pass prev-run date.

define method main () => ()
  let handler <tool-warning-condition> =
    method (c :: <tool-warning-condition>, next :: <function>) => ()
      format-out("%s.\n", c);
      if (~ c.tool-warning-recoverable?)
        exit-application(2);
      end if;
    end method;
  local
    method yn-handler (c :: <tool-yes-no-question>, next :: <function>) => (answer :: <boolean>)
      format-out("%s (y/n) ", c);
      force-out();
      let resp = read-line(*standard-input*);
      if (resp.size = 0)
        yn-handler(c, next)
      elseif (resp.first = 'y' | resp.first = 'Y')
        #t
      elseif (resp.first = 'n' | resp.first = 'N')
        #f
      else
        yn-handler(c, next)
      end if;
    end method;
  let handler <tool-yes-no-question> = yn-handler;

  let args = application-arguments();
  if (size(args) < 1
        | size(args) > 2
        | member?(args[0], #("help", "-help", "-h", "--help"), test: \=))
    format-out("Usage: %s <specification-file-name> [<project-file-name>]\n",
               locator-base(as(<file-locator>, application-name())));
    exit-application(1);
  end if;
  let spec-file = as(<file-locator>, args[0]);
  let project-file = element(args, 1, default: #f);
  if (project-file)
    project-file := as(<file-locator>, project-file);
  end if;

  let tool-name = tool-name-from-specification(spec-file);
  if (tool-name == #"dylan")
    // The #"dylan" default for files with no extension is not useful to us here.
    tool-name := #f;
  end;
  let tool = tool-name & tool-find(tool-name);
  if (tool)
    let (success?, modified-projects) = tool(spec-file, project-file, #f);
    if (~success?)
      format-out("Tool failed!\n");
      exit-application(3);
    end if;
    format-out("Modified projects: %=.\n", modified-projects);
  else
    format-out("Tool for file %s could not be found.\n", as(<string>, spec-file))
  end if;
end method main;

main();
