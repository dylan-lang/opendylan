Module:    select-viewer-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define constant $dbms :: <odbc-dbms> = make(<odbc-dbms>);

// This locking is a hack and I need to come up with a better story now that 
// you can have multiple query windows.

define constant $query-lock :: <simple-lock> = make(<simple-lock>);

/*
define class <database-data> (<object>)
  constant slot name-data :: <byte-string>,
    required-init-keyword: name:;
  constant slot user-name-data :: <byte-string>,
    required-init-keyword: user-name:;
  constant slot password-data :: <byte-string>,
    required-init-keyword: password:;
end class;
*/

// F9E81C00-1350-11D2-BD36-0000C06AF1D3
define constant $select-viewer-class-id =
  make-GUID(#xF9E81C00, #x1350, #x11D2,
	    #xBD, #x36, #x00, #x00, #xC0, #x6A, #xF1, #xD3);

define dispatch-interface <select-viewer> (<simple-dispatch>)
  uuid $select-viewer-class-id;
  documentation "Select Viewer exmaple server";
  member-function open-database
      (name :: <string>, user-name :: <string>, password :: <string>)
   => (database-data :: <sequence>);
  member-function query-database
      (database-data :: <sequence>, query :: <string>)
   => (headings-then-rows :: <sequence>);
  member-function close-database
      (database-data :: <sequence>) => ();
end;

define method open-database 
    (this :: <select-viewer>,
     name :: <string>, user-name :: <string>, password :: <string>)
// => (dbd :: <database-data>)
 => (status :: <SCODE>, dbd :: <sequence>)
//  make(<database-data>, name:name, user-name: user-name, password: password);
  ignore(this);
  values($S-OK, vector(name, user-name, password))
end method;

define method query-database 
/*
    (dbd :: <database-data>, query :: <byte-string>) 
 => (headings :: <sequence>, rows :: <sequence>)
*/
    (this :: <select-viewer>, dbd :: <sequence>, query :: <string>) 
 => (status :: <SCODE>, headings-then-rows :: <sequence>)
  ignore(this);
  with-lock ($query-lock)
    with-dbms ($dbms)
//      let db = make(<database>, datasource-name: name-data(dbd));
      let db = make(<database>, datasource-name: as(<byte-string>, first(dbd)));
      let login 
//        = make(<user>, user-name: user-name-data(dbd), password: password-data(dbd));
        = make(<user>, user-name: as(<byte-string>, second(dbd)),
               password: as(<byte-string>, third(dbd)));
      with-database (db, login)
        let results = as(<list>, execute(as(<byte-string>, query)));
        let headings = compute-column-headings(results);
//        values(headings, results)
        values($S-OK, vector(headings, results))
      end;
    end;
  end;
end method;

define method compute-column-headings 
    (results :: <list>) => (headings :: <simple-object-vector>)
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

define method close-database (this :: <select-viewer>, db :: <sequence>)
 => (status :: <SCODE>)
  ignore(this);
  $S-OK
end method;

// eof

