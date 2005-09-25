Module:    build-system
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
    error("Required Environment Variable %s is not set", variable);
  end if;
end method;


define function system-install-path()
 => (path :: false-or(<locator>));
  let path = read-environment-variable("FUNCTIONAL_DEVELOPER_RELEASE_INSTALL");
  if (path)
    let directory = as(<directory-locator>, path);
    ensure-directories-exist(directory);
    directory
  end
end;

define function system-registry-path()
 => (path :: false-or(<sequence>));
  let path
    = read-environment-variable("FUNCTIONAL_DEVELOPER_RELEASE_REGISTRIES");
  path &
    map(method(p) as(<directory-locator>, p) end,
        tokenize-environment-variable(path))
end;

define function system-release-path()
 => (path :: false-or(<locator>))
  let path = read-environment-variable("FUNCTIONAL_DEVELOPER_RELEASE");
  if (path)
    as(<directory-locator>, path)
  else
    application-filename-path()
  end if;
end;

define function system-build-path()
 => (path :: false-or(<locator>))
  let path = read-environment-variable("FUNCTIONAL_DEVELOPER_RELEASE_BUILD");
  path & as(<directory-locator>, path)
end;

define function application-filename-path
    ()
 => (path :: <directory-locator>)
  let exe = application-filename();
  unless (exe)
    error("Cannot locate %s release directory", release-product-name())
  end;
  let exe-directory = locator-directory(as(<file-locator>, exe));
  locator-directory(exe-directory)
end;

define function user-registry-path
    ()
 => (path :: false-or(<sequence>));
  let path = read-environment-variable("FUNCTIONAL_DEVELOPER_USER_REGISTRIES");
  path 
    & map(method (p :: <string>)
            as(<directory-locator>, p)
          end,
          tokenize-environment-variable(path))
end;

define function user-install-path
    ()
 => (path :: false-or(<directory-locator>))
  read-environment-path("FUNCTIONAL_DEVELOPER_USER_INSTALL");
end;

define variable *user-projects-path* = #f;

define function user-projects-path-setter
    (paths :: <sequence>)
  *user-projects-path* := map(method(p) as(<directory-locator>, p) end,
                              paths)
end;

define function user-projects-path
    ()
 => (path :: false-or(<sequence>));
  *user-projects-path*
    |
    begin
      let path = environment-variable("FUNCTIONAL_DEVELOPER_USER_PROJECTS");
      path &
        map(method(p) as(<directory-locator>, p) end,
            tokenize-environment-variable(path))
    end
end;

define function user-build-path()
 => (path :: false-or(<locator>));
  read-environment-path("FUNCTIONAL_DEVELOPER_USER_BUILD", default: "build");
end;

define function read-environment-path
    (name :: <string>, #key default :: false-or(<string>))
 => (path :: false-or(<directory-locator>))
  let path = read-environment-variable(name);
  if (~path)
    let root-path = user-root-path();
    if (root-path)
      if (default)
        path := subdirectory-locator(root-path, default);
      else
        root-path
      end;
    end;
  else
    path := as(<directory-locator>, path)
  end;
  path & ensure-directories-exist(path);
  path;
end;

define function user-root-path()
 => (path :: false-or(<directory-locator>));
  let path = read-environment-variable("FUNCTIONAL_DEVELOPER_USER_ROOT");
  as(<directory-locator>, path | home-directory())
end;
