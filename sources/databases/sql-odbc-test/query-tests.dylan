Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!query-tests.dylan(trunk.7) $


define test forward-only-query-test()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
	   	      text: "select col_1, col_2 from dwsql");
        let result-set = execute(query);
        check-true("forward-only result-set query test: "
		   "result set instance of <forward-only-result-set", 
	           instance?(result-set, <forward-only-result-set>));
        let result-set-size = result-set.size;
        check-true("forward-only result-set query test: result-set size check",
	           result-set-size = 26);
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;


define test explicit-coercion-query-test()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		      text: "select col_1, col_2 from dwsql",
		      coercion-policy: vector(curry(as, <byte-string>),
				              curry(as, <integer>)));
        let result-set = execute(query);
        check-true("Explicit coercion result-set test: "
		   "result set instance of <forward-only-result-set>", 
	           instance?(result-set, <forward-only-result-set>));

        let record-count :: <integer> = 0;
        let not-found = make(<pair>);
        let col-1 = #f;
        let col-2 = #f;

        check-true("Explicit coercion result-set test, structure check",
	           every?(method(record)
			    record-count := record-count + 1;
			    col-1 := element(record, 0, default: not-found);
			    col-2 := element(record, 1, default: not-found);
			    
			    instance?(record, <simple-object-vector>) &
			      col-1 ~== not-found 
			      & col-2 ~== not-found &
			      instance?(col-1, <byte-string>) &
			      instance?(col-2, <integer>)
		          end method,
		          result-set));

        check-true("Explicit coercion result-set test: result-set size check", 
	           record-count = 26);
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;

define test default-coercion-query-test()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		      text: "select col_1, col_2 from dwsql",
		      coercion-policy: #"default-coercion");
        let result-set = execute(query);

        check-true("Coercion result-set query test 2: "
		   "result set instance of <forward-only-result-set>", 
	           instance?(result-set, <forward-only-result-set>));

        let record-count :: <integer> = 0;
        let not-found = make(<pair>);
        let col-1 = #f;
        let col-2 = #f;
    
        check-true("Coercion result-set-query test 2",
	           every?(method(record)
			    record-count := record-count + 1;
			    col-1 := element(record, 0, default: not-found);
			    col-2 := element(record, 1, default: not-found);
			    
			    instance?(record, <simple-object-vector>) &
			      col-1 ~== not-found &
			      col-2 ~== not-found &
			      instance?(col-1, <byte-string>) &
			      instance?(col-2, <number>)
		          end method,
		          result-set));
        check-true("Coercion result-set query test 2: result-set size check", 
	           record-count = 26);
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;


//-------------------- liaison-query test --------------------

define class <ack> (<object>)
  constant slot col-1, init-keyword: col-1:;

  constant slot col-2, init-keyword: col-2:;
end class;

define test liaison-query-test()
  local method liaison-fn(rec :: <record>)
	  make(<ack>, col-1: rec[0], col-2: rec[1]);
	end method;

  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		      text: "select col_1, col_2 from dwsql",
		      coercion-policy: #"default-coercion");
        let result-set = execute(query, liaison: liaison-fn);

        let record-count :: <integer> = 0;

        check-true("Liaison query test",
	           every?(method(record)
			    record-count := record-count + 1;
			    instance?(record, <ack>);
		          end method,
		          result-set));

        check-true("Liaison query test: result-size check", record-count = 26);
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;


//-------------------- null test -------------------

define test null-query-test()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		      text: "select col_1, col_3 from dwsql",
		      output-indicator: $null-value,
		      coercion-policy: #"default-coercion");
        let result-set = execute(query, liaison: identity);
        let not-found = make(<pair>);
        let record-count :: <integer> = 0;
        let col-1 = #f;
        let col-3 = #f;

        check-true("Null query test",
	           every?(method(record)
			    record-count := record-count + 1;
			    col-1 := element(record, 0, default: not-found);
			    col-3 := element(record, 1, default: not-found);

			    col-1 ~== not-found &
			      col-3 ~== not-found &
			      instance?(col-1, <string>) &
			      col-3 = $null-value
		          end method,
		          result-set));
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;


//-------------------- scrollable test -------------------

define test scrollable-query-test-1()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		      text: "select col_2 from dwsql order by col_2",
		      datatype-hints: vector(<sql-integer>));
        let result-set 
          = execute(query, result-set-policy: $scrollable-result-set-policy);

        check-true("Scrollable result-set identity test",
	           instance?(result-set, <scrollable-result-set>));
        check-true("Scrollable result-set test 1",
	           every?(method (record, answer) 
		  	    record[0] = answer
		          end method, 
		          result-set, 
		          make(<range>, from: $a-value, to: $z-value)));
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;


define test scrollable-query-test-2()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		      text: "select col_2 from dwsql order by col_2",
		      datatype-hints: vector(<sql-integer>));
        let result-set = execute(query,
			     result-set-policy: $scrollable-result-set-policy);

        check-true("Scrollable result-set test 2",
	           every?(method (record, answer)
			    record[0] = answer
		          end method,
		          result-set,
		          make(<range>, from: $a-value, to: $z-value)));
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;


define test scrollable-query-test-3()
  with-connection(*query-connection*)
    with-transaction()
      let query = #f;
      block ()
        query := make(<sql-statement>,
		     text: "select col_2 from dwsql order by col_2",
		     datatype-hints: vector(<sql-integer>));
        let result-set = execute(query, 
			     result-set-policy: $scrollable-result-set-policy);
        check-true("Scrollable query test 3",
	           result-set[25][0] = as(<integer>, 'z') &
		     result-set[0][0] = as(<integer>, 'a') &
		     result-set[12][0] = as(<integer>, 'm'));
      cleanup
        finalize(query);
      end block;
    end with-transaction;
  end with-connection;
end test;

define test multiple-queries()
  with-connection(*query-connection*)
    check-true("Multiple queries on same sql-statement",
               begin
                 let query = #f;
                 block ()
                   with-transaction ()
                     let query = make(<sql-statement>,
                                      text: "select col_2 from dwsql where col_2 > ?",
                                      datatype-hints: vector(<sql-integer>));
                     let result-set = execute(query, parameters: vector(10));
                     let result-set = execute(query, parameters: vector(10));
                   end with-transaction;
                   #t;
                 cleanup
                   unless(query = #f)
                     finalize(query);
                   end unless;
                 exception (condition :: <diagnostic>)
                   #f;
                 end block;
               end);
  end with-connection;
end test;

define method create-query-test-table()
  with-connection(*query-connection*)
    with-transaction()
      let statement = #f;
      block ()
        statement := make(<sql-statement>,
			  text: "create table dwsql (col_1 varchar(1), "
			    "col_2 number, col_3 number)",
			  input-indicator: $null-value);
        execute(statement);

        statement.text := "insert into dwsql(col_1, col_2, col_3)"
          "values(?, ?, ?)";
        for (i from as(<integer>, 'a') to as(<integer>, 'z'))
          execute(statement,
	          parameters: vector(as(<character>, i), i, $null-value));
        end for;
      cleanup
        finalize(statement);
      end block;
    end with-transaction;
  end with-connection;
end method;


define variable *query-connection* = #f;

define method query-test-setup()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *query-connection* := connect(database, user);
    create-query-test-table();
  end with-dbms;
end method;

define method query-test-cleanup()
  with-connection(*query-connection*)
    with-transaction()
      let statement = #f;
      block ()
        statement := make(<sql-statement>, text: "drop table dwsql");
        execute(statement);
      cleanup
        finalize(statement);
      end block;
    end with-transaction;
  end with-connection;
  disconnect(*query-connection*);
  *query-connection* := #f;
end method;


define suite query-test-suite(setup-function: query-test-setup,
			      cleanup-function: query-test-cleanup)
  test forward-only-query-test;
  test explicit-coercion-query-test;
  test default-coercion-query-test;
  test liaison-query-test;
  test null-query-test;
  test scrollable-query-test-1;
  test scrollable-query-test-2;
  test scrollable-query-test-3;
  test multiple-queries;
end suite;

