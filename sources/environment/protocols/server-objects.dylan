Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Server objects

define open abstract class <server> (<object>)
end class <server>;

define sealed class <closed-server-error> (<simple-error>)
end class <closed-server-error>;

define sealed class <invalid-object-error> (<simple-error>)
  constant slot condition-project :: <project-object>,
    required-init-keyword: project:;
  constant slot condition-object :: <environment-object>,
    required-init-keyword: object:;
end class <invalid-object-error>;

define constant <query-type> = <symbol>;

define open generic record-client-query
    (server :: <server>, client, object, type :: <query-type>)
 => ();

define function make-object-cache
    () => (cache :: <table>)
  make(<table>, weak: #"key")
end function make-object-cache;
