Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: DBdylan-sql-odbc-test!datatype-tests.dylan(trunk.2) $

define constant $a-thru-z-range = make(<range>, from: $a-value, to: $z-value);

define generic float-equal(float-1 :: <float>, float-2 :: <float>)
 => (equal? :: <boolean>);

define method float-equal(float-1 :: <float>, float-2 :: <float>)
 => (equal? :: <boolean>)
  abs(float-1 - float-2) < 0.5s5;
end method;


define test float-test()
  with-connection(*datatype-connection*)
    with-transaction()
      let answer = map(curry(\*, 1.1), $a-thru-z-range);
      let query = make(<sql-statement>,
	  	       text: "select col_3 from dwsql order by col_3");
      let result-set = execute(query);

      check-true("Double float test",
	         every?(method(record, answer-item)
                            check-true("Record element of type float",
                                       instance?(record[0], <float>));
                            float-equal(record[0], answer-item);
		        end method,
		        result-set,
		        answer));
    end with-transaction;
  end with-connection;
end test;

define test date-test()
  with-connection(*datatype-connection*)
    with-transaction()
      let answer = make(<date>, year: 1997, month: 1, day: 1);
      let query = make(<sql-statement>,
                       text: "select col_4 from dwsql");
      let result-set = execute(query);
      check-true("Date test",
                 every?(method(record)
                          instance?(record[0], <date>) &
                            record[0] = answer;
                        end method,
                        result-set));
    end with-transaction;
  end with-connection;
end test;

define test string-test()
  with-connection(*datatype-connection*)
    with-transaction()
      let answer = map(integer-to-string, 
                       make(<range>, from: $a-value, to: $z-value));
      let query = make(<sql-statement>, text: "select col_5 from dwsql");
      let result-set = execute(query);
      check-true("String test",
                 every?(method(record)
                          instance?(record[0], <string>) &
                            member?(record[0], answer, test: \=)
                        end method,
                        result-set));
    end with-transaction;
  end with-connection;
end test;



define method create-datatype-test-table()
  with-connection(*datatype-connection*)
    let statement = make(<sql-statement>,
			 text: "create table dwsql (col_1 varchar(1), "
			   "col_2 number, col_3 number, col_4 date, "
                            "col_5 varchar(5))",
			 input-indicator: $null-value);
    execute(statement);

    let statement 
      = make(<sql-statement>,
             text: "insert into dwsql(col_1, col_2, col_3, col_4, col_5)"
                    "values(?, ?, ?, ?, ?)");

    for (i from as(<integer>, 'a') to as(<integer>, 'z'))
      execute(statement,
	      parameters: vector(as(<character>, i), 
				 i, 
				 i * 1.1,
                                 make(<date>, year: 1997, month: 1, day: 1),
                                 integer-to-string(i)));
    end for;
  end with-connection;
end method;


define variable *datatype-connection* = #f;

define method datatype-test-setup()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *datatype-connection* := connect(database, user);
    create-datatype-test-table();
  end with-dbms;
end method;

define method datatype-test-cleanup()
  with-connection(*datatype-connection*)
    execute("drop table dwsql");
  end with-connection;
  disconnect(*datatype-connection*);
  *datatype-connection* := #f;
end method;


define suite datatype-test-suite(setup-function: datatype-test-setup,
				 cleanup-function: datatype-test-cleanup)
  test float-test;
  test date-test;
  test string-test;
end suite;

