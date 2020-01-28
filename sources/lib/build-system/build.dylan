Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo, Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dylanmakefile  = "dylanmakefile.mkf";
define constant $build-log-file = "build.log";
define constant $platform-variable = "OPEN_DYLAN_TARGET_PLATFORM";
define constant $default-platform = $platform-name;

define function target-platform-name ()
 => (platform-name :: <symbol>)
  as(<symbol>, environment-variable($platform-variable) | $default-platform)
end function target-platform-name;

define settings <build-system-settings> (<open-dylan-user-settings>)
  key-name "Build-System";
  slot build-script :: <string>
    = as(<string>, calculate-default-build-script());
end settings <build-system-settings>;

define constant $build-system-settings = make(<build-system-settings>);

define function default-build-script
    () => (script :: <file-locator>)
  as(<file-locator>, $build-system-settings.build-script)
end function default-build-script;

define function default-build-script-setter
    (script :: <file-locator>) => (script :: <file-locator>)
  $build-system-settings.build-script := as(<string>, script);
  script
end function default-build-script-setter;

define function calculate-default-build-script ()
 => (script :: <file-locator>)
  merge-locators(as(<file-locator>,
                    concatenate(as(<string>, target-platform-name()),
                                "-build.jam")),
                 system-build-scripts-path())
end;



define method change-directory (directory :: <directory-locator>)
  if (~file-exists?(directory))
    error("Invalid Directory %s", directory);
  else
    working-directory() := directory;
  end if;
end method;

define macro with-build-directory
  { with-build-directory (?directory:expression) ?:body end }
    => {
        let directory = ?directory;
        let previous-directory = if (directory) working-directory(); end;
        block()
          if (directory) change-directory(directory); end;
          ?body
        cleanup
          if (previous-directory) change-directory(previous-directory) end if;
        end block;
       }
end macro;



// Top level internal function that can be invoked by Dylan Clients
// Arguments:
// build-targets - A sequence of strings naming Jam build targets like
//     "clean", "exe", "dll", etc. See sources/jamfiles/posix-build.jam.
// directory - The directory in which to perform the build.
// progress-callback - A function to call with build progress messages.
//     Called with the message string and `warning?: #t` if the message
//     is a warning.
// build-script - Jam build script <file-locator>
// compiler-back-end - #"llvm", #"c", #"harp", or #f.
define method build-system
    (build-targets :: <sequence>,
     #key directory :: <directory-locator> = working-directory(),
          progress-callback :: <function> = ignore,
          build-script = default-build-script(),
          compiler-back-end,
          force?,
          jobs :: <integer> = 1)
 => (build-successful? :: <boolean>);
  configure-build-system();
  let log-file
    = make(<file-locator>, directory: directory, name: $build-log-file);
  with-open-file (stream = log-file, direction: #"output")
    local
      method wrap-progress-callback(message-string :: <string>, #rest keys)
        write-line(stream, message-string);
        apply(progress-callback, message-string, keys);
      end method;

    block ()
      let handler <warning>
        = method (w :: <warning>, next :: <function>)
            wrap-progress-callback(condition-to-string(w), warning?: #t);
          end;

      let jam
        = make-jam-state(build-script,
                         compiler-back-end: compiler-back-end,
                         build-directory: directory);
      with-build-directory (directory)
        jam-read-mkf(jam, as(<file-locator>, $dylanmakefile));

        jam-target-build(jam, build-targets,
                         progress-callback: wrap-progress-callback,
                         force?: force?,
                         jobs: jobs);
      end;
    exception (e :: <error>)
      wrap-progress-callback(condition-to-string(e), error?: #t);
    end block;
  end with-open-file;
end method;

define method build-system-variable
    (name :: <string>,
     #key default :: false-or(<sequence>) = #[],
          directory :: <directory-locator> = working-directory(),
          build-script = default-build-script(),
          compiler-back-end)
 => (value :: false-or(<sequence>));
  configure-build-system();
  let jam
    = make-jam-state(build-script,
                     compiler-back-end: compiler-back-end,
                     build-directory: directory);
  jam-variable(jam, name, default: default)
end method;
