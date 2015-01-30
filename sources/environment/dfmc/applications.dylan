Module:    dfmc-environment
Author:    Roman Budzianowski, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Initialization

define sideways sealed method make-project-application
    (project :: <native-project-object>,
     #rest keys,
     #key arguments,
     #all-keys)
 => (application :: <dfmc-application>)
  apply(make, <dfmc-application>,
        project: project,
        arguments: arguments | "",
        keys)
end method make-project-application;

define sideways method note-application-initialized
    (project-object :: <dfmc-project-object>) => ()
  let application = project-object.project-application;
  connect-tether-to-project(application, project-object.project-proxy);
  next-method()
end method note-application-initialized;

