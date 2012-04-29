Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo, Peter S. Housel
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $dylanmakefile  = "dylanmakefile.mkf";
define constant $build-log-file = "build.log";

define settings <build-system-settings> (<open-dylan-user-settings>)
  key-name "Build-System";
  slot build-script :: <string>
    = as(<string>,
         merge-locators(as(<file-locator>,
                           concatenate(as(<string>, $platform-name),
                                       "-build.jam")),
                        $system-lib));
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



// Toplevel internal function that can be invoked by Dylan Clients

define method build-system
    (build-targets :: <sequence>,
     #key directory :: <directory-locator> = working-directory(),
          progress-callback :: <function> = ignore,
	  build-script = default-build-script(),
          compiler-back-end,
	  project-build-info,
          force?,
	  configure? = #t)
 => (build-successful? :: <boolean>);
  if (configure?)
    configure-build-system();
  end;

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
                         progress-callback: wrap-progress-callback,
                         build-directory: directory);
      with-build-directory (directory)
        jam-read-mkf(jam, as(<file-locator>, $dylanmakefile));
        
        format(stream, "building targets:");
        for (target in build-targets)
          format(stream, " %s", target);
        end for;
        new-line(stream);
        
        jam-target-build(jam, build-targets,
                         progress-callback: wrap-progress-callback,
                         force?: force?);
      end;
    exception (e :: <error>)
      wrap-progress-callback(condition-to-string(e), error?: #t);
    end block;
  end with-open-file;
end method;
