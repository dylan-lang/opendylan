Module:    dylan-build
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Read in Shell Environment Variables for Builds

define method read-environment-variable
    (variable :: <string>, #key required? = #f) 
 => (value :: false-or(<string>))
  let value = environment-variable(variable);
  if (value | ~required?)
    value
  else
    build-error("Required Environment Variable %s is not set",
                variable);
  end if;
end method;

// Build Error Signaling

define method build-error(format-string :: <string>, #rest args) => ()
  format-out("- %s", build-indentation());
  format-out("Build Error: ");
  apply(format-out, format-string, args);
  format-out("\n- %sExiting Build", build-indentation());
  force-output(*standard-output*);
  if (*build*)
    signal(make(<build-error>));
  else
    error("Build Error caught during Initialization");
  end if;
end method;

define method build-indentation()
  if (*build*)
    *build*.indentation
  else
    ""
  end if;
end method;

define function system-install-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DYLAN_RELEASE_INSTALL");
  path & ensure-directories-exist(path)
end;

define function system-registry-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DYLAN_RELEASE_REGISTRIES");
  path & as(<locator>, ensure-directory-name(path))
end;

define function system-release-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DYLAN_RELEASE");
  if(path)
    as(<locator>, ensure-directory-name(path))
  else
    let exe = application-filename();
    unless (exe)
      build-error("Cannot locate Functional Developer release directory")
    end;
    parent-directory(exe);
  end if;
end;

define function user-registry-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DYLAN_USER_REGISTRIES");
  path & as(<locator>, ensure-directory-name(path));
end;

define function user-install-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DYLAN_PROJECTS_INSTALL");
  if(path)
    ensure-directories-exist(ensure-directory-name(path));
  else
    let new-path = subdirectory-locator(home-directory(), #("dylan", "install"));
    ensure-directories-exist(new-path);
  end;
end;

define function user-projects-path()
 => (path :: false-or(<list>));
  let env = environment-variable("FUNCTIONAL_DYLAN_PROJECTS");
  let path =
    if(env)
      map(method(p) as(<locator>, ensure-directory-name(p)) end,
	  tokenize-environment-variable(env))
    else
      list(subdirectory-locator(home-directory(), #("dylan", "projects")))
    end;
  path
end;

define function user-build-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DYLAN_PROJECTS_BUILD");
  path & as(<locator>, ensure-directory-name(path))
end;

