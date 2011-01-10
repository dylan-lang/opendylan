Module: sql-odbc-implementation
Author: eec 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql-odbc!connection.dylan(trunk.10) $

define concrete class <odbc-connection> (<connection>)
  constant slot user :: <odbc-user>,
    required-init-keyword: user:;

  constant slot database :: <odbc-database>,
    required-init-keyword: database:;

  slot %connection-handle :: <connection-handle> = $null-connection-handle,
    init-keyword: connection-handle:;

  constant slot %allocated-sql-statements = make(<table>, weak: #"key");
end class;

define method initialize(connection :: <odbc-connection>, #key initial-value)
 => ()
  next-method();

  // Setup finalization.
  finalize-when-unreachable(connection);
  connection.dbms.%allocated-connections[connection] := #t;
end method;

define constant $connection-finalization-lock :: <lock> = make(<lock>);
// See comment in sql-statement.dylan on $sql-statement-finalization-lock.
define method finalize(connection :: <odbc-connection>)
 => ()
  do(finalize, connection.%allocated-sql-statements.key-sequence);

  with-lock($connection-finalization-lock)
    if (connection.%connection-handle ~= $null-connection-handle)
      disconnect(connection); //++ What if a condition is signalled?
    end if;
  end with-lock;

  notify-of-finalization(connection);
  next-method();
end method;

define method connect(database :: <odbc-database>,
		      user :: <odbc-user>,
		      #key connection-string :: <string> = "",
		      dbms :: <odbc-dbms> = default-dbms())
  => (connection :: <odbc-connection>)
  let env-handle = dbms.%environment-handle;
  let (return-code, connection-handle) =
    SQLAllocHandle($sql-handle-dbc,env-handle);
  assert-odbc-goodness(return-code, env-handle, connection-handle,
		       $null-statement-handle);

//  format-out("\nconnect:db=%=\n",database);

  let return-code = if (connection-string = "")
		      nice-SQLConnect(connection-handle,
				      database.datasource-name,
				      user.user-name,
				      user.password);
		    else
		      nice-SQLDriverConnect(connection-handle,
					    connection-string);
		    end if;
  assert-odbc-goodness(return-code, env-handle, connection-handle,
		       $null-statement-handle);

  let connection = make(<odbc-connection>,
			user: user, database: database,
			connection-handle: connection-handle,
			dbms: dbms);

  with-lock(*all-connections-lock*)
    push-last(*all-connections*, connection);
  end with-lock;

  if (dbms.%dbms-name = #f)
    dbms-name(dbms, connection: connection);
  end if;

  if (dbms.%dbms-version = #f)
    dbms-version(dbms, connection: connection);
  end if;

/* this is the default now.
  if (dbms.%dbms-name.as-lowercase = "microsoft sql server")
    dbms.%dbms-rowset-size := 10;
  end if;
*/

  //+++debugging
  let return-code = nice-SQLSetConnectAttr(connection-handle, $SQL-OPT-TRACE,
					   $SQL-OPT-TRACE-ON);
  assert-odbc-goodness(return-code, env-handle, connection-handle,
		       $null-statement-handle);
  //+++debugging end

  connection;
end method;


define method disconnect(connection :: <odbc-connection>,
			 #key terminate-statements :: <boolean>)
 => ()
  // Cleanup all associated SQL-statement objects before disconnecting.
  do(finalize, connection.%allocated-sql-statements.key-sequence);

  let return-code = SQLDisconnect(connection.%connection-handle);
  assert-odbc-goodness(return-code,
		       connection.dbms.%environment-handle,
		       connection.%connection-handle,
		       $null-statement-handle);

  let return-code = SQLFreeHandle($sql-handle-dbc, 
				  connection.%connection-handle);
  assert-odbc-goodness(return-code,
		       connection.dbms.%environment-handle,
		       connection.%connection-handle,
		       $null-statement-handle);
  connection.%connection-handle := $null-connection-handle;

  with-lock(*all-connections-lock*)
    remove!(*all-connections*,connection);
  end with-lock;
end method;

