Module:       djam
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $platform-variable = "OPEN_DYLAN_TARGET_PLATFORM";
define constant $default-platform = $platform-name;

define function default-platform-name ()
 => (platform-name :: <symbol>)
  as(<symbol>, environment-variable($platform-variable) | $default-platform)
end function default-platform-name;

define function main(name, arguments)
  let state = make(<jam-state>);

  // Useful built-in variables
  jam-variable(state, "OS") := vector(as(<string>, $os-name));
  jam-variable(state, "OSPLAT") := vector(as(<string>, $machine-name));
  jam-variable(state, "TARGET_PLATFORM") := vector(as(<string>, default-platform-name()));

  select($os-name)
    #"win32" =>
      jam-variable(state, "NT") := #["true"];
    #"linux", #"freebsd", #"darwin" =>
      jam-variable(state, "UNIX") := #["true"];
  end select;

  jam-variable(state, "JAMDATE") := vector(as-iso8601-string(current-date()));
  jam-variable(state, "JAMVERSION") := #["2.5"];

  // Custom built-in functions
  jam-rule(state, "ECHO")
    := jam-rule(state, "Echo")
    := method(jam :: <jam-state>, #rest lol) => (result :: <sequence>);
         if(lol.size > 0)
           for(arg in lol[0])
             format(*standard-output*, "%s ", arg);
           end for;
           new-line(*standard-output*);
         end if;
         #[]
       end;
  jam-rule(state, "EXIT")
    := jam-rule(state, "Exit")
    := method(jam :: <jam-state>, #rest lol) => (result :: <sequence>);
         if(lol.size > 0)
           for(arg in lol[0])
             format(*standard-output*, "%s ", arg);
           end for;
           new-line(*standard-output*);
         end if;
         exit-application(1);
         #[]
       end;

  local
    method progress(message :: <string>, #rest args)
      apply(format, *standard-output*, message, args);
      new-line(*standard-output*);
    end;

  block ()
    let handler <warning>
      = method (w :: <warning>, next :: <function>)
          format(*standard-error*, "djam: %s\n", w);
        end;
    
    let targets = make(<stretchy-vector>);
    let force? = #f;
    iterate loop(i :: <integer> = 0)
      if (i < arguments.size)
        let arg = arguments[i];
        if (arg = "-f")
          let file = as(<file-locator>, arguments[i + 1]);
          jam-read-file(state, file);
          loop(i + 2);
        elseif (arg = "-s")
          let setting = arguments[i + 1];
          block (done)
            for (c in setting, j from 0)
              if (c == '=')
                jam-variable(state, copy-sequence(setting, end: j))
                  := vector(copy-sequence(setting, start: j + 1));
                done();
              end if;
            finally
              error("unrecognized -s argument: %s", setting);
            end;
          end block;
          loop(i + 2);
        elseif (arg = "-a")
          force? := #t;
          loop(i + 1);
        else
          add!(targets, arg);
          loop(i + 1);
        end;
      end if;
    end iterate;

    if (targets.empty?)
      jam-target-build(state, #["all"],
                       force?: force?, progress-callback: progress);
    else
      jam-target-build(state, targets, 
                       force?: force?, progress-callback: progress);
    end if;
  exception (e :: <error>)
    format(*standard-error*, "djam: %s\n", e);
    exit-application(1);
  end block;
end function;

main(application-name(), application-arguments());
