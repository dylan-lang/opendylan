Module:   select-viewer
Synopsis: A simple sql query viewer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// TODO: Create and define an <odbc-dbms> object.
define constant $dbms :: <odbc-dbms> = make(<odbc-dbms>);

define class <database-data> (<object>)
  constant slot name-data :: <byte-string>,
    required-init-keyword: name:;
  constant slot user-name-data :: <byte-string>,
    required-init-keyword: user-name:;
  constant slot password-data :: <byte-string>,
    required-init-keyword: password:;
end class;

// TODO: Implement open-database. Whatever you return is passed in to query-database
// and close-database.
define method open-database 
    (name :: <byte-string>, user-name :: <byte-string>, password :: <byte-string>)
 => (dbd :: <database-data>)
  make(<database-data>, name: name, user-name: user-name, password: password);
end method;

// TODO: Implement query-interface. Use compute-column-headings on the result
// sequence to generate the headings for you.
define method query-database 
    (dbd :: <database-data>, query :: <byte-string>) 
 => (headings :: <sequence>, rows :: <sequence>)
  with-dbms ($dbms)
    let db = make(<database>, datasource-name: name-data(dbd));
    let login 
      = make(<user>, user-name: user-name-data(dbd), password: password-data(dbd));
    with-database (db, login)
      let results = map-as(<simple-object-vector>, identity, execute(query));
      let headings = compute-column-headings(results);
      values(headings, results)
    end;
  end;
end method;

define method compute-column-headings 
    (results :: <sequence>) => (headings :: <simple-object-vector>)
  if (empty?(results))
    #[]
  else
    let n-cols = size(first(results));
    let headings = make(<simple-object-vector>, size: n-cols);
    for (i from 0 below n-cols)
      headings[i] := format-to-string("Column %=", i + 1);
    end;
    headings
  end;
end method;

// TODO: Implement close-database.
define method close-database (dbd) => ()
end method;
