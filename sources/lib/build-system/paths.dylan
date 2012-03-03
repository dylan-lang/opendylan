Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Roman Budzianowski
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define function system-install-path()
 => (path :: false-or(<locator>));
  let path = environment-variable("OPEN_DYLAN_RELEASE_INSTALL");
  if (path)
    as(<directory-locator>, path);
  end
end;

define function system-registry-path()
 => (path :: false-or(<sequence>));
  let path = environment-variable("OPEN_DYLAN_RELEASE_REGISTRIES");
  path &
    map(method(p) as(<directory-locator>, p) end,
        tokenize-environment-variable(path))
end;

define function system-release-path()
 => (path :: false-or(<locator>))
  let path = environment-variable("OPEN_DYLAN_RELEASE");
  if (path)
    as(<directory-locator>, path)
  else
    application-filename-path()
  end if;
end;

define function system-build-path()
 => (path :: false-or(<locator>))
  let path = environment-variable("OPEN_DYLAN_RELEASE_BUILD");
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
  let path = environment-variable("OPEN_DYLAN_USER_REGISTRIES");
  let registries = path
    & map(method (p :: <string>)
            as(<directory-locator>, p)
          end,
          tokenize-environment-variable(path));
  let cwd = working-directory();
  if (cwd)
    add!(registries | #(), subdirectory-locator(cwd, "registry"))
  else
    registries
  end;
end;

define function user-install-path
    ()
 => (path :: <directory-locator>)
  read-environment-path("OPEN_DYLAN_USER_INSTALL");
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
      let path = environment-variable("OPEN_DYLAN_USER_PROJECTS");
      path &
        map(method(p) as(<directory-locator>, p) end,
            tokenize-environment-variable(path))
    end
end;

define function user-build-path()
 => (path :: <locator>);
  read-environment-path("OPEN_DYLAN_USER_BUILD", default: "build");
end;

define function read-environment-path
    (name :: <string>, #key default :: false-or(<string>))
 => (path :: <directory-locator>)
  let path = environment-variable(name);
  if (~path)
    let root-path = user-root-path();
    if (default)
      path := subdirectory-locator(root-path, default);
    else
      path := root-path
    end;
  else
    path := as(<directory-locator>, path)
  end;
  path;
end;

define function user-root-path()
 => (path :: <directory-locator>);
  let path = environment-variable("OPEN_DYLAN_USER_ROOT");
  if (path)
    as(<directory-locator>, path)
  elseif ($os-name == #"win32")
    let path =
      begin
        let appdata = environment-variable("APPDATA");
        if (appdata)
          as(<directory-locator>, appdata);
        end;
      end;
    subdirectory-locator(path | home-directory() | temp-directory(), "Open-Dylan")
  else
    subdirectory-locator(working-directory(), "_build")
  end
end;
