Module:   select-viewer
Synopsis: A simple sql query viewer
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <database-data> = <object>;

// TODO: Create and define an <odbc-dbms> object.

// TODO: Implement open-database. Whatever you return is passed in to query-database
// and close-database.
define method open-database 
    (name :: <byte-string>, user-name :: <byte-string>, password :: <byte-string>)
 => (dbd :: <database-data>)
  #t
end method;

// TODO: Implement query-interface. Use compute-column-headings on the result
// sequence to generate the headings for you.
define method query-database 
    (dbd :: <database-data>, query :: <byte-string>) 
 => (headings :: <sequence>, rows :: <sequence>)
  let results 
    = #[["Clanger", "Tiny",  "Eats soup"],
        ["Dragon",  "Soup",  "Makes soup"],
        ["Tree",    "Music", "Grows notes"]];
    let headings = compute-column-headings(results);
    values(headings, results)
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
define method close-database (dbd :: <database-data>) => ()
end method;

// eof

