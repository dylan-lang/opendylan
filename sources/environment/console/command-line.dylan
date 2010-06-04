Module:    console-environment
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define abstract class <basic-main-command> (<basic-command>)
  constant slot %echo-input? :: <boolean> = #f,
    init-keyword: echo-input?:;
  constant slot %profile-commands? :: <boolean> = #f,
    init-keyword: profile-commands?:;
  constant slot %personal-root :: false-or(<directory-locator>) = #f,
    init-keyword: personal-root:;
  constant slot %system-root :: false-or(<directory-locator>) = #f,
    init-keyword: system-root:;
  constant slot %internal-debug :: false-or(<sequence>) = #f,
    init-keyword: internal-debug:;
  constant slot %project :: false-or(<file-locator>) = #f,
    init-keyword: project:;
  constant slot %help?          :: <boolean> = #f,
    init-keyword: help?:;
  constant slot %logo?          :: <boolean> = #t,
    init-keyword: logo?:;
  constant slot %debugger?      :: <boolean> = #f,
    init-keyword: debugger?:;
  constant slot %import?        :: <boolean> = #f,
    init-keyword: import?:;
  constant slot %build?         :: <boolean> = #f,
    init-keyword: build?:;
  constant slot %compile?       :: <boolean> = #f,
    init-keyword: compile?:;
  constant slot %link?          :: <boolean> = #f,
    init-keyword: link?:;
  constant slot %subprojects?   :: <boolean> = #t,
    init-keyword: subprojects?:;
  constant slot %clean?         :: <boolean> = #f,
    init-keyword: clean?:;
  constant slot %release?       :: <boolean> = #f,
    init-keyword: release?:;
  constant slot %build-script :: false-or(<file-locator>) = #f,
    init-keyword: build-script:;
  constant slot %target :: false-or(<symbol>) = #f,
    init-keyword: target:;
  constant slot %force? :: <boolean> = #f,
    init-keyword: force?:;
  constant slot %unify? :: <boolean> = #f,
    init-keyword: unify?:;
  constant slot %not-recursive? :: <boolean> = #f,
    init-keyword: not-recursive?:;
  constant slot %save? :: <boolean> = #t,
    init-keyword: save?:;
  constant slot %link-dll? :: <boolean> = #f,
    init-keyword: link-dll?:;
  constant slot %link-exe? :: <boolean> = #f,
    init-keyword: link-exe?:;
  constant slot %gnu-exports? :: <boolean> = #f,
    init-keyword: gnu-exports?:;
  constant slot %arch :: false-or(<symbol>) = #f,
    init-keyword: arch:;
  constant slot %debug-info :: <symbol> = #"full",
    init-keyword: debug-info:;
  constant slot %messages :: false-or(<symbol>) = #f,
    init-keyword: messages:;
  constant slot %harp?          :: <boolean> = #f,
    init-keyword: harp?:;
  constant slot %assemble?                   = #f,
    init-keyword: assemble?:;
  constant slot %dfm?           :: <boolean> = #f,
    init-keyword: dfm?:;
/*---*** fill this in later.
  constant slot %exports?       :: <boolean> = #f,
    init-keyword: exports?:;
  let gc? = #f;
  let mode = #f;
  let libraries :: <sequence> = make(<stretchy-vector>);
*/
end class <basic-main-command>;

//---*** We need to implement these...
ignore(%gnu-exports?, %debug-info, %messages);

define method execute-main-command
    (context :: <server-context>, command :: <basic-main-command>)
 => (status-code :: <integer>)
  local method run
	    (class :: subclass(<command>), #rest arguments) => ()
	  let command = apply(make, class, server: context, arguments);
	  execute-command(command)
	end method run;
  if (command.%internal-debug)
    let parts = as(<list>, command.%internal-debug);
    debugging?() := #t;
    debug-parts() := parts;     // For simple-debugging's debug-out.
    *dfmc-debug-out* := parts;  // For dfmc-common's debug-out.
  end if;
  let filename = command.%project;
  if (command.%import?)
    run(<import-project-command>, file: filename)
  else
    run(<open-project-command>, file: filename)
  end;
  let dw-options?
    = command.%link-dll?  | command.%link-exe?;
  let build? = command.%build? | dw-options?;
  if (build? | command.%compile?)
    run(<build-project-command>, 
	clean?:      command.%clean?,
	save?:       command.%save?,
	link?:       #f,
	release?:    command.%release?,
	subprojects: command.%subprojects? & ~command.%not-recursive?,
        output:      begin
                       let output = make(<stretchy-object-vector>);
                       if (command.%assemble?) add!(output, #"assembler") end;
                       if (command.%dfm?) add!(output, #"dfm") end;
                       if (command.%harp?) add!(output, #"harp") end;
                       output
                     end)
  end;
  if (build? | command.%link?)
    let target
      = command.%target
          | case
	      command.%link-dll? => #"dll";
	      command.%link-exe? => #"executable";
	    end;
    run(<link-project-command>, 
	build-script: command.%build-script,
	target:      target,
	arch:        command.%arch,
	force?:      command.%force? | command.%clean?,
	subprojects: command.%subprojects? & ~command.%not-recursive?,
	unify?:      command.%unify?)
  end;
  $success-exit-code;
end method execute-main-command;

define method execute-main-loop
    (context :: <server-context>, command :: <basic-main-command>)
 => (status-code :: <integer>)
  let echo-input? = command.%echo-input?;
  let profile-commands? = command.%profile-commands?;
  command-line-loop
    (context.context-server, 
     debugger?:         command.%debugger?,
     echo-input?:       echo-input?,
     profile-commands?: profile-commands?);
  $success-exit-code;
end method execute-main-loop;

define method do-execute-command
    (context :: <server-context>, command :: <basic-main-command>)
 => (status-code :: <integer>)
  block (return)
    let handler (<serious-condition>)
      = method (condition :: <serious-condition>, next-handler :: <function>)
	  if (command.%debugger?)
	    next-handler()
	  else
	    display-condition(context, condition);
	    message(context, "Exiting with return code %d", $error-exit-code);
	    return($error-exit-code)
	  end
	end;
    local method run
	      (class :: subclass(<command>), #rest arguments) => ()
	    let command = apply(make, class, server: context, arguments);
	    execute-command(command)
	  end method run;
    if (command.%help?)
      let command-line
	= select (command by instance?)
	    <main-command> => $main-command-line;
	    otherwise      => $internal-main-command-line;
	  end;
      let filename = as(<file-locator>, application-filename());
      run(<help-command>, 
	  command: command-line,
	  title: as-uppercase(locator-base(filename)));
      $success-exit-code
    else
      command.%logo? & message(context, dylan-banner());
      let personal-root = command.%personal-root;
      let system-root   = command.%system-root;
      personal-root
	& set-named-property(context, #"personal-root", personal-root);
      system-root
	& set-named-property(context, #"system-root",   system-root);
      case
	command.%project => execute-main-command(context, command);
	otherwise        => execute-main-loop(context, command);
      end
    end
  end
end method do-execute-command;
