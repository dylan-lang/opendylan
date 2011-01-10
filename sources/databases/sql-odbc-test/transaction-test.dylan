Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!transaction-test.dylan(trunk.4) $


define test transaction-test-1()
  with-connection(*transaction-connection*)
     check-true("Transaction test 1 - empty transaction body",
		begin
		  with-transaction()
  		  end with-transaction;
		  #t
	        end);
  end with-connection;
end test;


define test transaction-test-2()
  with-connection(*transaction-connection*)
    with-transaction(rollback: do-rollback, commit: do-commit)
      check-true("Transaction test 2 - rollback function",
	         instance?(do-rollback, <function>));
      check-true("Transaction test 2 - commit function",
  	       instance?(do-commit, <function>));
    end with-transaction;
  end with-connection;
end test;


define test transaction-test-3()
  with-transaction(transaction-mode: read-only,
		   diagnostics-size: 10,
		   connection: *transaction-connection*)
    let connection = default-connection();
/*    check-equal("Transaction test 3 - transaction mode",
		connection.attributes.transaction-mode,
		$read-only);
    check-equal("Transaction test 3 - diagnostics size",
		connection.attributes.diagnostics-size,
		10);*/
  end with-transaction;
end test;


define test transaction-test-4()
  with-connection(*transaction-connection*)
    block ()
      let statement = make(<sql-statement>,
			   text: "create table trans_table (col_1 number)");
      execute(statement);

      statement.text := "create unique index trans_index "
      "on trans_table(col_1)";
      execute(statement);

      with-transaction()
        statement.text := "insert into trans_table(col_1) values(?)";
        execute(statement, parameters: vector(1));
        execute(statement, parameters: vector(2));
        check-condition("Transaction test 4 - integrity constraint violation",
			<integrity-constraint-violation>,
			execute(statement, parameters: vector(2)));
      end with-transaction;
    cleanup
      let statement = make(<sql-statement>,
			   text: "drop table trans_table");
      execute(statement);
    end block;
  end with-connection;
end test;


define variable *transaction-connection* = #f;

define method create-transaction-test-table()
  with-connection(*transaction-connection*)
    let statement = make(<sql-statement>,
			 text: "create table dwsql (col_1 varchar(1), "
			   "col_2 number, col_3 number)",
			 input-indicator: $null-value);
    execute(statement);

    statement.text := "insert into dwsql(col_1, col_2, col_3) values(?, ?, ?)";
    for (i from as(<integer>, 'a') to as(<integer>, 'z'))
      execute(statement,
	      parameters: vector(as(<character>, i), i, 
				 if (even?(i)) i else $null-value end if));
    end for;
  end with-connection;
end method;

define method transaction-test-setup()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *transaction-connection* := connect(database, user);
    create-transaction-test-table();
  end with-dbms;
end method;

define method transaction-test-cleanup()
  with-connection(*transaction-connection*)
    execute("drop table dwsql");
  end with-connection;
  disconnect(*transaction-connection*);
  *transaction-connection* := #f;
end method;

define suite transaction-test-suite(setup-function: transaction-test-setup,
				    cleanup-function: transaction-test-cleanup)
  test transaction-test-1;
  test transaction-test-2;
  test transaction-test-3;
  test transaction-test-4;
end suite;
