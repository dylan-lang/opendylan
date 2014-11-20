Module:    build-system
Synopsis:  A build-system for Dylan PC Applications in Dylan
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define variable $personal-install :: false-or(<directory-locator>) = #f;
define variable $personal-bin     :: false-or(<directory-locator>) = #f;

// We don't ensure system directories because nothing is installed there.
// By definition they had to have been already created
define variable $system-install      :: false-or(<directory-locator>) = #f;

define method configure-build-system () => ()
  $personal-install := user-install-path();
  $personal-bin :=
    $personal-install & subdirectory-locator($personal-install, "bin");

  $system-install := system-install-path() | system-release-path();
end method;

configure-build-system();
