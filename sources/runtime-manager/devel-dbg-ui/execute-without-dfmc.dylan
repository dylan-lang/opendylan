module:     devel-dbg-ui
synopsis:   Implementations of debugger commands that normally invoke
            DFMC, but are forced not to because this version of the debugger
            does not have it.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant *dbg-version* = #"console-debugger";

define function install-first-compilation-context () => ()

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

  let top-level-lib = 
     strip-extension(strip-to-leaf(*opening-command*.application-filename));
  let project = #f; /* lookup-named-project(top-level-lib); */
  let context = #f;
//  if (project)
//    project.project-current-compilation-context;
//  else
//    #f
//  end if;
//debugger-message("Setting compilation context to %=\n", context);
  *current-compilation-context* := context;
end function;

define function string-complete? (string, #key module = "internal",
				          library = "dylan",
				          target = #"fake-target",
				          context = #"fake-context")
 => (well? :: <boolean>, conditions :: <sequence>)
  values(#f, #[])
end function;

define method execute-debugger-command
    (command :: <drop-into-compiler-command>) => ()
  debugger-message("The compiler is not a part of this version.")
end method;


define method execute-debugger-command
    (command :: <execute-source-command>) => ()
  ignore(command.command-source-string);
  debugger-message("The compiler is not a part of this version.");
end method;


define method execute-debugger-command
    (command :: <change-library-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    *open-application*.default-name-context.context-library :=
      command.new-library;
  else
    debugger-message ("No open application. Use open <filename>");
  end if;
end method;

