Module:    environment-protocols
Synopsis:  Environment protocols
Author:    Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Server objects

define open abstract primary class <compiler-database>
    (<server>, <environment-object>)
  sealed constant slot server-project :: <project-object>,
    required-init-keyword: project:;
  sealed constant slot compiler-object-table :: <table> = make-object-cache();
end class <compiler-database>;

define open generic find-compiler-database-proxy
    (database :: <compiler-database>, id :: <id>, #key imported? = #f)
 => (compiler-proxy);

define open generic compiler-database-proxy-id
    (database :: <compiler-database>, proxy)
 => (id :: false-or(<id>));


/// Implementation

define method choose-server
    (project :: <project-object>, database :: <compiler-database>,
     #key error?, default-server)
 => (database :: <compiler-database>)
  ignore(error?, default-server);
  database
end method choose-server;

define method record-client-query
    (database :: <compiler-database>, client, object :: <compiler-object>,
     type :: <query-type>)
 => ()
  record-client-query(server-project(database), client, object, type)
end method record-client-query;

define method environment-object-type-name
    (database :: <compiler-database>) => (label :: <string>)
  "Database"
end method environment-object-type-name;

define method lookup-environment-object-by-proxy
    (database :: <compiler-database>, proxy)
 => (object :: false-or(<compiler-object>))
  element(compiler-object-table(database), proxy, default: #f)
end method lookup-environment-object-by-proxy;

define method cache-environment-object
    (database :: <compiler-database>, proxy, object :: <compiler-object>)
 => (object :: <compiler-object>)
  element(compiler-object-table(database), proxy) := object
end method cache-environment-object;

define method environment-object-home-server?
    (database :: <compiler-database>, object :: <compiler-object>)
 => (home? :: <boolean>)
  let proxy = compiler-object-proxy(object);
  proxy & lookup-environment-object-by-proxy(database, proxy) & #t
end method environment-object-home-server?;


/// Proxy handling

//--- This default method means that the objects will never get linked, so
//--- we need a real solution in the database server implementation.
define method find-compiler-database-proxy
    (database :: <compiler-database>, id :: <id>, #key imported? = #f)
 => (compiler-proxy)
  #f
end method find-compiler-database-proxy;

define function ensure-database-proxy
    (database :: <compiler-database>, object :: <compiler-object>)
 => (proxy)
  compiler-object-proxy(object)
    | begin
        let project = server-project(database);
        let id = environment-object-id(project, object);
        if (instance?(id, <id>))
          let proxy = find-compiler-database-proxy(database, id);
          if (proxy)
            compiler-object-proxy(object) := proxy
          end
        end
      end
end function ensure-database-proxy;

define function invalidate-compiler-database
    (database :: <compiler-database>) => ()
  let project = database.server-project;
  let object-table = compiler-object-table(database);
  for (object in object-table)
    invalidate-compiler-proxy(project, object)
  end;
  remove-all-keys!(object-table)
end function invalidate-compiler-database;
