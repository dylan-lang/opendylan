Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define abstract sealed class <odbc-result-set> (<result-set-substrate>)
  slot %record :: <odbc-record>,
    required-init-keyword: record:;

  // Record numbers start at 0.
  slot %current-record-number :: <integer> = -1;
end class;



define method next-record(result-set :: <odbc-result-set>)
 => ()
  let stmt = result-set.%record.statement;
  let return-code = SQLFetch(stmt.%statement-handle);
  assert-odbc-goodness(return-code,
		       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  result-set.%current-record-number := result-set.%current-record-number + 1;
end method;


define method next-result-set-state(result-set :: <odbc-result-set>,
				    state :: <integer-result-set-state>)
 => (next-state :: <integer-result-set-state>)
  next-record(result-set);
  state;
end method;


define method acquire-initial-dbcs-state(result-set :: <odbc-result-set>)
 => (initial-state :: <integer-result-set-state>)
  let initial-state = next-method();
  block ()
    next-result-set-state(result-set, initial-state);
  exception (condition :: <data-not-available>)  //++ correct condition
    initial-state.at-end? := #t;
    initial-state;
  end block;
end method;


define method acquire-dbcs-element(result-set :: <odbc-result-set>,
				   key :: <integer>)
 => (dbcs-element :: <object>)
  while (key > result-set.%current-record-number)
    next-record(result-set);
  end while;

  if (key = result-set.%current-record-number)
    result-set.liaison(result-set.%record);
  else  // key < result-set.%current-record-number
    error(make(<data-not-available>));
  end if;
end method;
