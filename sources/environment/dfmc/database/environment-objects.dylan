Module:    dfmc-environment-database
Synopsis:  DFM compiler environment object information
Author:    Andy Armstrong, Chris Page
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Environment objects

define sealed method find-environment-object
    (server :: <dfmc-database>, id :: <id>, #key, #all-keys)
 => (object :: false-or(<environment-object>))
  let proxy = find-compiler-database-proxy(server, id, imported?: #t);
  if (proxy)
    select (proxy by instance?)
      <source-form> =>
	make-environment-object-for-source-form(server, proxy);
      <project> =>
	make-environment-object(<library-object>,
				project: server.server-project,
				compiler-object-proxy: proxy);
      otherwise =>
	debug-out(#"dfmc-environment-database",
                  "Failed to make environment object for %=",
                  proxy);
	#f;
    end
  end
end method find-environment-object;
