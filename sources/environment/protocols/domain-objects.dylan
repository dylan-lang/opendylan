Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Domain object

define class <domain-object> (<definition-object>)
end class <domain-object>;

define open generic domain-specializers
    (server :: <server>, object :: <domain-object>)
 => (specializers :: <sequence>);


/// Implementations

define method domain-specializers
    (project :: <project-object>, object :: <domain-object>)
 => (specializers :: <sequence>)
  let server = choose-server(project, object, error?: #t);
  domain-specializers(server, object)
end method domain-specializers;

define method environment-object-type-name
    (object :: <domain-object>) => (label :: <string>)
  "Domain"
end method environment-object-type-name;
