Module:       database-stress-tool
Author:       Andy Armstrong
Synopsis:     A simple database stress tool
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $database  = "testDB";
define constant $table     = "Accounts";
define constant $statement = "SELECT * FROM Accounts";
define constant $update-statement
  = "INSERT INTO Accounts"
    " (Name, Balance, Limit)"
    " VALUES('Test', 50, 1000)";

define constant $phases     = 10;
define constant $iterations = 100;

define class <database-stress-test> (<object>)
  constant slot test-database :: <string>,
    required-init-keyword: database:;
  constant slot test-table :: <string>,
    required-init-keyword: table:;
  constant slot test-statement :: <string>,
    required-init-keyword: statement:;
  constant slot test-truncate? :: <boolean> = #f,
    init-keyword: truncate?:;
  constant slot test-transactions? :: <boolean> = #f,
    init-keyword: transactions?:;
  constant slot test-commit? :: <boolean> = #t,
    init-keyword: commit?:;
  constant slot test-phases :: <integer>,
    required-init-keyword: phases:;
  constant slot test-iterations :: <integer>,
    required-init-keyword: iterations:;
  constant slot test-share-connections? :: <boolean>,
    required-init-keyword: share-connections?:;
  constant slot test-quiet? :: <boolean> = #f,
    init-keyword: quiet?:;
  slot test-elapsed-time :: <integer> = 0;
  slot test-total-allocation :: <integer> = 0;
end class <database-stress-test>;

define method perform-test
    (test :: <database-stress-test>) => ()
  format-out("Performing database stress test\n");
  format-out("  database:     %s\n", test.test-database);
  format-out("  table:        %s\n", test.test-table);
  format-out("  statement:    %s\n", test.test-statement);
  format-out("  phases:       %s\n", test.test-phases);
  format-out("  iterations:   %s\n", test.test-iterations);
  format-out("  operation:    %s, %s, %s\n",
	     if (test.test-truncate?) "truncate" else "no truncation" end,
	     if (test.test-transactions?) "transactions" else "no transactions" end,
	     if (test.test-commit?) "commit" else "rollback" end,
	     if (test.test-share-connections?) "single connection" else "multiple connections" end);
  format-out("\n");
  let connection = open-database(test.test-database);
  if (test.test-truncate?)
    with-connection (connection)
      block ()
	execute(format-to-string("DELETE FROM %s", test.test-table));
	format-out("\nTruncated table!\n\n");
      exception (error :: <data-not-available>)
	format-out("\nAttempted to truncate table, but it was empty!\n\n");
      end
    end
  end;
  let verbose? = ~test.test-quiet?;
  block ()
    for (phase from 1 to test.test-phases)
      if (phase > 1 & ~test.test-share-connections?)
	verbose? & format-out("Creating new connection...\n");
        disconnect(connection);
        connection := open-database(test.test-database)
      end;
      ~verbose? & format-out(".");
      with-connection (connection)
	if (test.test-transactions?)
	  verbose? & format-out("Starting transaction...\n");
	  with-transaction (commit: commit, rollback: rollback)
	    perform-test-phase(test, phase);
	    if (test.test-commit?)
	      verbose? & format-out("Committing transaction\n");
	      commit()
	    else
	      verbose? & format-out("Rolling back transaction\n");
	      rollback()
	    end
	  end
	else
	  perform-test-phase(test, phase)
	end
      end
    end
  cleanup
    disconnect(connection)
  end;
  let total-records = test.test-phases * test.test-iterations;
  let total-time = test.test-elapsed-time;
  let total-allocation = test.test-total-allocation;
  verbose? & format-out("\n");
  format-out("\n");
  format-out("Database stress test completed\n");
  format-out("  Total records %d\n", total-records);
  format-out("  Total time %d ms (average %s ms)\n", 
             total-time, round/(total-time, total-records));
  format-out("  Total allocation %d (average %s)\n", 
             total-allocation, round/(total-allocation, total-records))
end method perform-test;

define method perform-test-phase
    (test :: <database-stress-test>, phase :: <integer>)
 => ()
  let verbose? = ~test.test-quiet?;
  profiling(cpu-time-seconds, cpu-time-microseconds, allocation)
    verbose? & format-out("Phase %d: ", phase);
    let record-index = phase * test.test-phases;
    for (record from 1 to test.test-iterations)
      let result = execute(test.test-statement, result-set-policy: #f);
      if (result & verbose?)
	format-out("\nResult set not #f but %=\n", result)
      end;
      verbose? & format-out(".")
    end;
    verbose? & format-out("\n");
  results
    let time-milliseconds = (cpu-time-seconds * 1000) + round/(cpu-time-microseconds, 1000);
    if (verbose?)
      format-out("Phase %d: took %d.%d seconds, allocated %d\n",
                 phase, cpu-time-seconds, cpu-time-microseconds, allocation);
    end;
    test.test-elapsed-time := test.test-elapsed-time + time-milliseconds;
    test.test-total-allocation := test.test-total-allocation + allocation
  end
end method perform-test-phase;

define method open-database
    (table :: <string>) => (connection :: <odbc-connection>)
  let dbms = make(<odbc-dbms>);
  with-dbms(dbms)
    let user = make(<user>);
    let db = make(<database>, datasource-name: table);
    connect(db, user)
  end with-dbms
end method open-database;
