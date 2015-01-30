Module:    Dylan-User
Synopsis:  Environment Manager
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dfmc-environment
  use dfmc-environment-projects;
  use dfmc-environment-database;
  use dfmc-application;

  use environment-imports;
  use environment-protocols,
    rename: { close-project => env/close-project,
              project-source-location => env/project-source-location,
              project-other-sources => env/project-other-sources,
              open-project => env/open-project,
              save-project =>  env/save-project,
              project-compilation-mode => env/project-compilation-mode,
              project-compilation-mode-setter => env/project-compilation-mode-setter,
              project-executable-name => env/project-executable-name,
              project-executable-name-setter => env/project-executable-name-setter,
              project-target-type => env/project-target-type,
              <project-target-type> => env/<project-target-type>,
              project-target-type-setter => env/project-target-type-setter,
              project-base-address => env/project-base-address,
              project-base-address-setter => env/project-base-address-setter,
              project-major-version => env/project-major-version,
              project-major-version-setter => env/project-major-version-setter,
              project-minor-version => env/project-minor-version,
              project-minor-version-setter => env/project-minor-version-setter,
              project-read-only? => env/project-read-only? };
end module dfmc-environment;
