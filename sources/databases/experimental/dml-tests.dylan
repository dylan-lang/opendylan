Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!dml-tests.dylan(trunk.4) $


// Destructures the first record returned from a query.
define macro execute-singleton
  { execute-singleton(?body:*) }
    => { let result-set = execute(?body);
	 if (instance?(result-set, <empty-result-set>))
	   signal("Execute-singleton: query returned an empty result-set\n");
	 end if;
	 let first-record = element(result-set, 0);
	 apply(values, first-record);
	}
end macro;

define test insertion-test()
  insertion-test-body();
end test;
define function insertion-test-body()
  with-connection(*dml-connection*)
    block (exit)
      with-transaction()
        execute("create table dwsql (col_1 varchar(1), col_2 number)");

        let stmt = make(<sql-statement>, 
                        text: "insert into dwsql (col_1, col_2) values(?, ?)");
        for (i from as(<integer>, 'a') to as(<integer>, 'z')) 
	  execute(stmt, parameters: vector(as(<character>, i), i));
        end for;

        let count = execute-singleton("select count(*) from dwsql",
                                      datatype-hints: vector(<sql-integer>),
				      coercion-policy: $no-coercion);
	format-out("Count: %=\n", count.pointer-value);
	format-out("Count address: %=\n", count.pointer-address);
        check("Insertion-test Check count", \=, count.pointer-value, 26);
      end with-transaction;

    cleanup
      execute("drop table dwsql");
    end block;
  end with-connection;
end function;


define test null-insertion-test-1()
  null-insertion-test-1-body();
end test;
define function null-insertion-test-1-body()
  with-connection(*dml-connection*)
    block (exit)
      with-transaction()
        execute("create table dwsql (col_1 varchar(1), col_2 number)");

        let statement = make(<sql-statement>,
	                     text: "insert into dwsql(col_1, col_2) "
			       "values(?, ?)",
			     input-indicator: $null-value);
        execute(statement, parameters: vector('a', $null-value));


        let count = execute-singleton("select count(*) from dwsql "
	 			        "where col_2 is null",
				      datatype-hints: vector(<sql-integer>),
				      coercion-policy: $no-coercion);
        check("null-insertion-test-1 Check count", \=, count.pointer-value, 1);
      end with-transaction;

    cleanup 
      execute("drop table dwsql");
    end block;
  end with-connection;
end function;



define test null-insertion-test-2()
  null-insertion-test-2-body();
end test;
define function null-insertion-test-2-body()
  // Uses a domain value (-1) to indicate null-value
  with-connection(*dml-connection*)
    block (exit)
      with-transaction()
        execute("create table dwsql (col_1 varchar(1), col_2 number)");

        let statement = make(<sql-statement>,
			     text: "insert into dwsql(col_1, col_2) "
			       "values('a', ?)",
			     input-indicator: -1);
        execute(statement, parameters: vector(-1));

        let count = execute-singleton("select count(*) from dwsql "
				        "where col_2 is null",
				      datatype-hints: vector(<sql-integer>),
				      coercion-policy: $no-coercion);
        check("Null-insertion-test-2 Check count", \=, count.pointer-value, 1);
      end with-transaction;

    cleanup
      execute("drop table dwsql");
    end block;
  end with-connection;
end function;


define test null-selection-test()
  null-selection-test-body();
end test;
define function null-selection-test-body()
  with-connection(*dml-connection*)
    block (exit)
      with-transaction()
        execute("create table dwsql (col_1 varchar(1), col_2 number)");

        let insert-statement = make(<sql-statement>,
				    text: "insert into dwsql(col_1, col_2) "
				      "values(?, ?)",
				    input-indicator: $null-value);

        for (i :: <integer> from as(<integer>, 'a') to as(<integer>, 'z'))
	  execute(insert-statement, 
		  parameters: vector(if (even?(i)) 
			  	       $null-value
				     else 
				       as(<character>, i) 
				     end if,
				     i));
        end for;

        let query-statement 
	  = make(<sql-statement>,
	         text: "select col_1, col_2 from dwsql",
	         output-indicator: $null-value,
		 datatype-hints: vector(<sql-character-varying>, <sql-integer>),
	         coercion-policy: $no-coercion);
        let result = execute(query-statement);
        let null-count = 0;
      
        for (record in result)
	  let col1 = record[0];
	  let col2 = record[1];
	  if (even?(col2.pointer-value) & col1 == $null-value)
	    null-count := null-count + 1;
	  end if;
        end for;

	format-out("null-count: %=\n", null-count);
        check("Null-selection-test Check count", \=, null-count, 13);
      end with-transaction;

    cleanup 
      execute("drop table dwsql");
    end block;
  end with-connection;
end function;


define variable *dml-connection* = #f;

define method dml-test-setup() => ()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *dml-connection* := connect(database, user);
  end with-dbms;
end method;

define method dml-test-cleanup() => ()
  disconnect(*dml-connection*);
  *dml-connection* := #f;
end method;

define suite dml-test-suite(setup-function: dml-test-setup,
			    cleanup-function: dml-test-cleanup)
  test insertion-test;
  test null-insertion-test-1;
  test null-insertion-test-2;
  test null-selection-test;
end suite;
