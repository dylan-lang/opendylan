Module:       database-viewer
Author:       Andy Armstrong, Keith Playford
Synopsis:     A simple database viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define method open-database
    (database :: <string>,
     #key user-name :: false-or(<string>) = #f,
          password :: false-or(<string>) = #f)
 => (connection :: <odbc-connection>)
  let dbms = make(<odbc-dbms>);
  with-dbms(dbms)
    let user = make(<user>, user-name: user-name | "", password: password | "");
    let db = make(<database>, datasource-name: database);
    connect(db, user)
  end with-dbms
end method open-database;

define method query-database 
    (connection :: <connection>, query :: <byte-string>) 
 => (headings :: <sequence>, rows :: <sequence>)
  with-connection (connection)
    let statement = make(<sql-statement>, text: query);
    let result-set = execute(statement);
    let results = map-as(<simple-object-vector>, identity, execute(query));
    let headings = statement.statement-column-names;
    values(headings, results)
  end;
end method query-database;

define method close-database (connection :: <connection>) => ()
  disconnect(connection)
end method close-database;

define method list-all-catalogs
    (connection :: <connection>) => ()
  with-connection (connection)
    catalogs()
  end
end method list-all-catalogs;
