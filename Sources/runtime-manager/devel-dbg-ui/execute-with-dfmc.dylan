module:     devel-dbg-ui
synopsis:   Implementations of debugger commands that invoke the DFMC
            compiler
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant *dbg-version* = #"interactive-compiler";

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
  let project = lookup-named-project(top-level-lib);
  let context =
    if (project)
      project.project-current-compilation-context;
    else
      #f
    end if;
  debugger-message("Setting compilation context to %=\n", context);
  *current-compilation-context* := context;
end function;

define method execute-debugger-command
    (command :: <drop-into-compiler-command>) => ()
  debugger-message("Entering the compiler's command loop...");
  unless (*open-application*)
    debugger-message("Note: there is no live runtime at present.")
  end unless;
  block()
    run-command-loop(*top-level-loop*)
  exception(<end-of-stream-error>)
    format-out("End of stream\n");
  exception(<end-of-loop>)
    format-out("Quit compiler\n");
  end block;
  debugger-message("Returning to the debugger command loop...");
  unless (*open-application*)
    debugger-message("Note: there is no live runtime at present.")
  end unless;
end method;

define method execute-debugger-command
    (command :: <execute-source-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    unless (command.command-source-string)
      let valid-form = #f;
      let form = "";
      let prompt = "Type a form > ";
      debugger-message("*** Perform an interactive evaluation.");
      while (~valid-form)
        format-out("%s", prompt);
        let next-line = read-line(*standard-input*);
        if (~empty?(next-line))
          form := concatenate(form, "\n", next-line);
          if (string-complete?(form))
            valid-form := #t;
          end if;
        end if;
      end while;
      command.command-source-string := form;
    end unless;
    format-out("\n");
    debugger-message("Sending to the compiler...");
    let selected-thread = *open-application*.stopped-thread;
    block ()
      execute-string
        (command.command-source-string,
         context: current-runtime-context
                    (*open-application*, selected-thread),
         library: *open-application*.default-name-context.context-library,
         module: *open-application*.default-name-context.context-module);
      debugger-message("Continuing runtime...");
      *open-application*.still-in-stop-mode? := #f;
    exception (condition :: <serious-condition>)
      let error-message :: <string>
         = block ()
	     format-to-string("%s", condition)
           exception (error :: <error>)
	     block ()
	       format-to-string
                 ("*** Crashed printing condition of class %=: %s ***",
                  condition.object-class, error)
	     exception (<error>)
	       "This error is so bad, I can't print it!!"
	     end
           end;
      format-out("\nError: %s\n", error-message);
    end block
  else
    debugger-message("No open application. Use open <filename>");
  end if
end method;

define method execute-debugger-command
    (command :: <change-library-command>) => ()
  *last-debugger-command* := command;
  if (*open-application*)
    *open-application*.default-name-context.context-library :=
      command.new-library;
    debugger-message("Making sure compiler has definitions loaded.");
    load-library(as(<symbol>, command.new-library));
    debugger-message("Done.");
  else
    debugger-message ("No open application. Use open <filename>");
  end if;
end method;

//// TODO: Put this somewhere in the compiler we can get at it.

define function execute-string (string, #key module = "internal",
				             library = "dylan",
				             target = *open-application*,
				             context = #"fake-context",
                                             harp-output? = #t)
  let sr = make(<string-template-source-record>, // Kludge!!!
		contents: as(<byte-vector>, string),
		module: as(<symbol>, module),
		name: "Test");
  let ld = lookup-library-description(library);
  let ild = lookup-interactive-context(target, ld);
  execute-source(ild, context, list(sr), harp-output?: harp-output?)
end function;

define function string-complete? (string, #key module = "internal",
				          library = "dylan",
				          target = *open-application*,
				          context = #"fake-context")
 => (well? :: <boolean>, conditions :: <sequence>)
  let sr = make(<string-template-source-record>, // Kludge!!!
		contents: as(<byte-vector>, string),
		module: as(<symbol>, module),
		name: "Test");
  let ld = lookup-library-description(library);
  let ild = lookup-interactive-context(target, ld);
  source-complete?(ild, context, list(sr));
end function;


define method lookup-library-description 
    (library-key :: <object>)
  project-current-compilation-context(lookup-named-project(library-key))
end method;

define method lookup-library-description 
    (library-key :: <library-description>)
  library-key
end method;

define method lookup-library-description (library-key :: <project>)
  project-current-compilation-context(library-key)
end;

// eof
