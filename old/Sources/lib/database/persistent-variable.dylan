module:    database
author:    jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define persistent-class <persistent-variable> (<persistent-object>)
  slot persistent-value;
end;

define method add-root 
    (connection :: <database-connection>, name :: <symbol>, value)
  let variable = persistent-make(connection, <persistent-variable>);
  variable.persistent-value := value;
  db-add-association
    (connection, connection.db-database.db-roots-id, name, 
     variable, db-roots, db-roots-setter, db-root-table);
  name;
end;

define method root
    (connection :: <database-connection>, name :: <symbol>,
     #key default = (unsupplied))
  let database = connection.db-database;
  let variable = element(database.db-root-table, name, default: #F);
  if (variable)
    variable.persistent-value;
  else
    default;  // should error
  end;
end;

define method root-setter
    (value, connection :: <database-connection>, name :: <symbol>)
  let database = connection.db-database;
  let variable = element(database.db-root-table, name, default: #F);
  if (variable)
    variable.persistent-value := value;
  else
    add-root(connection, name, value);
  end;
  value;
end;

// eof
