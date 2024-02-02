Module:       build-system
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method jam-read-mkf
    (jam :: <jam-state>, file :: <file-locator>)
 => ();
  let variables = read-file-header(file);
  let image = concatenate(element(variables, #"library"),
                          element(variables, #"executable", default: #()));

  // DylanLibrary image : version ;
  let version
    = concatenate(element(variables, #"major-version", default: #()),
                  element(variables, #"minor-version", default: #()));
  jam-invoke-rule(jam, "DylanLibrary", image, version);

  // DylanLibraryLinkerOptions image : options ;
  // DylanLibraryBaseAddress image : address ;
  // DylanLibraryCLibraries image : libraries ;
  // DylanLibraryCObjects image : objects ;
  // DylanLibraryCSources image : sources ;
  // DylanLibraryCHeaders image : headers ;
  // DylanLibraryC++Sources image ; sources ;
  // DylanLibraryRCFiles image : rcfiles ;
  // DylanLibraryJamIncludes image : includes ;
  let rule-specs
    = #[#["DylanLibraryJamIncludes", #"jam-includes", #f],
        #["DylanLibraryFiles", #"files", #f],
        #["DylanLibraryBaseAddress", #"base-address", #f],
        #["DylanLibraryLinkerOptions", #"linker-options", #t],
        #["DylanLibraryCLibraries", #"c-libraries", #t],
        #["DylanLibraryCObjects", #"c-object-files", #f],
        #["DylanLibraryCSources", #"c-source-files", #f],
        #["DylanLibraryCHeaders", #"c-header-files", #f],
        #["DylanLibraryC++Sources", #"c++-source-files", #f],
        #["DylanLibraryRCFiles", #"rc-files", #f]];
  for (spec in rule-specs)
    let value = element(variables, spec[1], default: #());
    let expanded = if (spec[2]) jam-expand-list(jam, value) else value end;
    unless (expanded.empty?)
      jam-invoke-rule(jam, spec[0], image, expanded);
    end unless;
  end for;

  // DylanLibraryUses image : library : dir ;
  let used-projects = element(variables, #"used-projects", default: #());
  for (i from 0 below used-projects.size by 3)
    jam-invoke-rule(jam, "DylanLibraryUses",
                    image,
                    vector(used-projects[i]),
                    vector(used-projects[i + 2]));
  end for;
end method;



define thread variable *mangler* = make(<mangler>);

define method jam-mangle
    (jam :: <jam-state>, names :: <sequence>)
 => (result :: <sequence>);
  select (names.size by \=)
    1 =>
      vector(mangle-name-raw(*mangler*, names[0]));
    3 =>
      vector(mangle-binding-spread(*mangler*, names[0], names[1], names[2]));
    otherwise =>
      #[];
  end select;
end method;



define variable *cached-build-script* :: false-or(<file-locator>) = #f;
define variable *cached-jam-state* :: false-or(<jam-state>) = #f;

define function make-jam-state
    (build-script :: <file-locator>,
     #key build-directory :: <directory-locator>,
          compiler-back-end)
 => (jam :: <jam-state>);
  // Ensure that the build-scripts haven't been modified, and the
  // various variable values installed into the cached state are still
  // valid.
  let personal-root
    = $personal-install & as(<string>, $personal-install);
  let compiler-back-end-string
    = compiler-back-end & as-lowercase(as(<string>, compiler-back-end));
  let state-variables
    = vector(".",                    as(<string>, build-directory),
             "SYSTEM_ROOT",          as(<string>, $system-install),
             "PERSONAL_ROOT",        personal-root,
             "SYSTEM_BUILD_SCRIPTS", as(<string>, system-build-scripts-path()),
             "TARGET_PLATFORM",      as(<string>, target-platform-name()),
             "COMPILER_BACK_END",    compiler-back-end-string);
  if (build-script = *cached-build-script*
        & *cached-jam-state*
        & ~jam-state-stale?(*cached-jam-state*)
        & block (result)
            for (i from 0 below state-variables.size by 2)
              let jam-value
                = jam-variable(*cached-jam-state*, state-variables[i]);
              let value
                = jam-value & element(jam-value, 0, default: #f);
              if (value ~= state-variables[i + 1])
                result(#f)
              end if;
            end for;
            #t
          end)
    jam-state-copy(*cached-jam-state*)
  else
    let state = make(<jam-state>);

    // Install the state variables
    for (i from 0 below state-variables.size by 2)
      let value = state-variables[i + 1];
      if (value)
        jam-variable(state, state-variables[i]) := vector(value);
      end if;
    end for;

    // Useful built-in variables
    jam-variable(state, "OS") := vector(as(<string>, $os-name));
    jam-variable(state, "OSPLAT") := vector(as(<string>, $machine-name));

    select ($os-name)
      #"win32" =>
        jam-variable(state, "NT") := #["true"];
      #"linux", #"freebsd", #"darwin" =>
        jam-variable(state, "UNIX") := #["true"];
    end select;

    jam-variable(state, "JAMDATE")
      := vector(as-iso8601-string(current-date()));
    jam-variable(state, "JAMVERSION") := #["2.5"];

    // Custom built-in functions
    jam-rule(state, "ECHO")
      := jam-rule(state, "Echo")
      := method (jam :: <jam-state>, #rest lol) => (result :: <sequence>);
           let message = "";
           if (lol.size > 0)
             for (arg in lol[0], first? = #t then #f)
               message
                 := if (first?) arg else concatenate(message, " ", arg) end;
             end for;
           end if;
           signal("%s", message);
           #[]
         end;
    jam-rule(state, "EXIT")
      := jam-rule(state, "Exit")
      := method (jam :: <jam-state>, #rest lol) => (result :: <sequence>);
           let message = "";
           if (lol.size > 0)
             for (arg in lol[0], first? = #t then #f)
               message
                 := if (first?) arg else concatenate(message, " ", arg) end;
             end for;
           end if;
           error("%s", message);
           #[]
         end;

    jam-rule(state, "IncludeMKF")
      := method
             (jam :: <jam-state>, includes :: <sequence>)
          => (result :: <sequence>);
           for (target-name in includes)
             let (locator, target) = jam-target-bind(jam, target-name);
             if (file-exists?(locator))
               jam-read-mkf(jam, locator)
             else
               error(make(<file-does-not-exist-error>, locator: locator));
             end if;
           end;
           #[]
         end method;
    jam-rule(state, "DFMCMangle")
      := jam-mangle;

    jam-read-file(state, build-script);

    *cached-build-script* := build-script;
    *cached-jam-state* := state;
    jam-state-copy(state)
  end if
end function;
