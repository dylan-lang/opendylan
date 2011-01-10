Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: !user.dylan(D-kan.1) $


define concrete class <odbc-user> (<user>)
  slot user-name :: <string>,
    init-keyword: user-name:,
    init-value: "";

  slot password :: <string>,
    init-keyword: password:,
    init-value: "";
end class;


define method make-dbms-specific
    (type == <user>, dbms :: <odbc-dbms>, #rest more-args)
  => (user :: <odbc-user>)
  apply(make, <odbc-user>, more-args);
end method;
