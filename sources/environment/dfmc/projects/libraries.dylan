Module:    dfmc-environment-projects
Author:    Roman Budzianowski, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sealed method do-project-used-libraries
    (function :: <function>, server :: <dfmc-project-object>, 
     project-object :: <project-object>)
 => ()
  assert(server == project-object,
	 "Querying libraries for %= through different project %=",
	 project-object, server);
  let project = project-object.ensure-project-proxy;
  if (project-object.project-compiler-database)
    for (subproject :: <project> in project.all-used-projects)
      let library = %maybe-make-library(project-object, subproject);
      library & function(library)
    end
  end
end method do-project-used-libraries;

// Return the library of a project
define sealed method project-library
    (project-object :: <dfmc-project-object>)
 => (library :: false-or(<library-object>))
  let project = project-object.ensure-project-proxy;
  if (project-object.project-compiler-database)
    %maybe-make-library(project-object, project)
  end
end method project-library;

define function %maybe-make-library
    (project-object :: <dfmc-project-object>, project :: <project>)
 => (library :: false-or(<library-object>))
  let context = project.project-browsing-context;
  if (context & context.project-library-definition)
    make-environment-object(<library-object>,
			    project: project-object,
			    compiler-object-proxy: project)
  end
end function %maybe-make-library;

