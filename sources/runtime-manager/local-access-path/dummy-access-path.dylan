module:     access-path-implementation
synopsis:   Implementation of the <access-path> class
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define sideways method make-access-connection
    (ap :: <access-path>, conn :: <local-debugger-connection>,
     #key description = ap.access-path-application)
 => (conn :: <access-connection>)
  signal(make(<access-path-creation-error>));
end method;
