Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql-odbc!database.dylan(trunk.2) $


define concrete class <odbc-database> (<database>)
  slot datasource-name :: <string>,
    required-init-keyword: datasource-name:;
end class;


define method make-dbms-specific
    (type == <database>, dbms :: <odbc-dbms>, #rest more-args)
  => (database :: <odbc-database>)
  apply(make, <odbc-database>, more-args);
end method;