Module: sql-odbc-test-bignums
Author: yduJ
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!big-integer-tests.dylan(trunk.1) $

define variable *big-int-connection* = #f;

define constant $my-large-integer = 1073741823;

define test big-int-insertion-test()
  with-dbms(*the-dbms*)
    with-connection(*big-int-connection*)
      with-transaction()
	let statement 
	  = make(<sql-statement>,
		 text: "insert into dwsql(col_1, col_2)"
			"values(?, ?)");
	for (i from 0 to 10)
	  execute(statement,
		  parameters: vector(i, i + $my-large-integer));
	end for;
      end with-transaction;
    end with-connection;
  end with-dbms;
end test;
      
define test big-int-extraction-test()
  with-dbms(*the-dbms*)
    with-connection(*big-int-connection*)
      with-transaction ()
	let rs = execute("select * from dwsql");

	for (i from 0 to 10)
	  let record = rs[i];
	  check-equal("extraction of small integer in bigint test",
		      record[0], i);
	  check-equal("extraction of large integer in bigint test",
		      record[1], i + $my-large-integer);
	end for;
      end with-transaction;
    end with-connection;
  end with-dbms;
end test;

define method create-big-int-test-table()
  with-connection(*big-int-connection*)
    let statement = make(<sql-statement>,
			 text: "create table dwsql ("
			   "col_1 number, col_2 number)",
			 input-indicator: $null-value);
    execute(statement);
  end with-connection;
end method;

define method big-integer-setup()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *big-int-connection* := connect(database, user);
    create-big-int-test-table();
  end with-dbms;
end method;

define method big-integer-cleanup()
  with-connection(*big-int-connection*)
    execute("drop table dwsql");
  end with-connection;
  disconnect(*big-int-connection*);
  *big-int-connection* := #f;
end method;



define suite big-integer-test-suite (setup-function: big-integer-setup,
				     cleanup-function: big-integer-cleanup)
  test big-int-insertion-test;
  test big-int-extraction-test;
end suite;
