Module: user-projects
Author: Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method makefile-exists?
    (project :: <lid-project>) => (well? :: <boolean>);
  let build = project.project-build-location;
  let make-locator
    = make(<file-locator>,
           directory: build,
           name: $dylan-makefile);
  file-exists?(make-locator)
end;

define sideways method generate-makefile (project :: <lid-project>)
  let context = project.project-current-compilation-context;
  let used-projects =
    map(compilation-context-project, used-compilation-contexts(context));
  let info = #();
  let build = project.project-build-location;
  for (p in used-projects)
    let location = p.project-build-location;
    let name = p.project-library-name;
    info := pair(name, pair("dummy",
                            pair(if (~location)
                                   #"system"
                                 else
                                   convert-path-to-relative(location, build)
                                 end, info)));
  end;

  let make-locator
    = make(<file-locator>,
           directory: build,
           name:      $dylan-makefile);
  with-open-file(stream = make-locator, direction: #"output")
    write-comment(stream, "This build file is generated, please don't edit");
    save-single-value(stream, #"Library", project.project-lid-library-name);
    save-lid-info(project, stream, flatten-extras?: #t);
    save-list-value(stream, #"Files",
                    compilation-context-object-names(context));
    save-list-value(stream, #"used-projects", info);
    save-list-value(stream, #"all-c-libraries", all-c-libraries(project));
  end;
end;
