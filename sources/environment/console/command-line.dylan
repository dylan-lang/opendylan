Module:    console-environment
Synopsis:  The command line version of the environment
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
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
  constant slot %show           :: false-or(<command-property>) = #f,
    init-keyword: show:;
  constant slot %help?          :: <boolean> = #f,
    init-keyword: help?:;
  // If this is true, show a welcome message for the interactive compiler.
  constant slot %logo?          :: <boolean> = #f,
    init-keyword: logo?:;
  constant slot %version?       :: <boolean> = #f,
    init-keyword: version?:;
  constant slot %shortversion?  :: <boolean> = #f,
    init-keyword: shortversion?:;
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
  constant slot %back-end :: false-or(<symbol>) = #f,
    init-keyword: back-end:;
  constant slot %build-script :: false-or(<file-locator>) = #f,
    init-keyword: build-script:;
  constant slot %target :: false-or(<symbol>) = #f,
    init-keyword: target:;
  constant slot %force? :: <boolean> = #f,
    init-keyword: force?:;
  constant slot %jobs :: <integer> = 1,
    init-keyword: jobs:;
  constant slot %verbose? :: <boolean> = #f,
    init-keyword: verbose?:;
  constant slot %unify? :: <boolean> = #f,
    init-keyword: unify?:;
  constant slot %harp?          :: <boolean> = #f,
    init-keyword: harp?:;
  constant slot %assemble?                   = #f,
    init-keyword: assemble?:;
  constant slot %dfm?           :: <boolean> = #f,
    init-keyword: dfm?:;
  constant slot %dispatch-coloring :: false-or(<symbol>) = #f,
    init-keyword: dispatch-coloring:;
  // When #t serious warnings DO NOT cause an error exit status to be returned
  // to the shell.
  constant slot %allow-serious-warnings? :: <boolean> = #f,
    init-keyword: allow-serious-warnings?:;
/*---*** fill this in later.
  constant slot %exports?       :: <boolean> = #f,
    init-keyword: exports?:;
  let gc? = #f;
  let mode = #f;
  let libraries :: <sequence> = make(<stretchy-vector>);
*/
end class <basic-main-command>;

define method execute-main-command
    (context :: <server-context>, command :: <basic-main-command>)
 => (exit-code :: <integer>)
  local method run (class :: subclass(<command>), #rest arguments)
                => (#rest values)
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
  let build? = command.%build?;
  let exit-code = #f;
  if (build? | command.%compile?)
    // By default the build command returns an error status for serious
    // warnings. This makes it possible for scripting to abort on serious
    // warnings.
    exit-code
      := run(<build-project-command>,
             clean?:      command.%clean?,
             link?:       #f,
             release?:    command.%release?,
             verbose?:    command.%verbose?,
             subprojects: command.%subprojects?,
             output:      begin
                            let output = make(<stretchy-object-vector>);
                            if (command.%assemble?) add!(output, #"assembler") end;
                            if (command.%dfm?) add!(output, #"dfm") end;
                            if (command.%harp?) add!(output, #"harp") end;
                            output
                          end,
             dispatch-coloring: command.%dispatch-coloring,
             allow-serious-warnings?: command.%allow-serious-warnings?);
  end;
  if (build? | command.%link?)
    let target = command.%target;
    run(<link-project-command>,
        build-script: command.%build-script,
        target:      target,
        force?:      command.%force? | command.%clean?,
        jobs:        command.%jobs,
        verbose?:    command.%verbose?,
        subprojects: command.%subprojects?,
        unify?:      command.%unify?)
  end;
  exit-code | $success-exit-code
end method execute-main-command;

define method execute-main-loop
    (context :: <server-context>, command :: <basic-main-command>)
 => (status-code :: <integer>)
  message(context, dylan-banner());
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
            message(context, "Exiting with return code %d",
                    $unexpected-error-exit-code);
            return($unexpected-error-exit-code)
          end
        end;
    local method run
              (class :: subclass(<command>), #rest arguments)
           => (success :: <integer>)
            let command-line = $main-command-line;
            let filename = as(<file-locator>, application-filename());
            let command = apply(make, class,
                                server: context,
                                command: command-line,
                                title: as-uppercase(locator-base(filename)),
                                arguments);
            execute-command(command);
            $success-exit-code
          end method run;
    if (command.%help?)
      message(context, "%s %s\n", release-product-name(), release-short-version());
      run(<help-command>)
    elseif (command.%version?)
      run(<version-command>)
    elseif (command.%shortversion?)
      run(<version-command>, short: "short")
    elseif (command.%show)
      run(<show-property-command>, property: command.%show);
    else
      if (command.%logo?)
        message(context, dylan-banner());
      else
        message(context, "%s %s\n", release-product-name(), release-short-version());
      end;
      let personal-root = command.%personal-root;
      let system-root   = command.%system-root;
      let back-end      = command.%back-end;
      personal-root
        & set-named-property(context, #"personal-root", personal-root);
      system-root
        & set-named-property(context, #"system-root",   system-root);
      back-end
        & set-named-property(context, #"compiler-back-end", back-end);
      case
        command.%project => execute-main-command(context, command);
        otherwise        => execute-main-loop(context, command);
      end
    end
  end
end method do-execute-command;
