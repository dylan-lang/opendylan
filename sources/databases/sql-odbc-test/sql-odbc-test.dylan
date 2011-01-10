Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!sql-odbc-test.dylan(trunk.5) $


define constant $access-db-name = "access";
define constant $access-user-name = "";
define constant $access-user-password = "";
define constant $access-detect-null-column = #f;
define constant $access-do-introspection = #f;

define constant $oracle-db-name = "phantom";
define constant $oracle-user-name = "j2";
define constant $oracle-user-password = "j2";
define constant $oracle-detect-null-column = #t;
define constant $oracle-do-introspection = #t;

define constant $oracle8-db-name = "ORA8";
define constant $oracle8-user-name = "scott";
define constant $oracle8-user-password = "tiger";
define constant $oracle8-detect-null-column = #t;
define constant $oracle8-do-introspection = #t;

define constant $ms-sql-server-db-name = "viral";
define constant $ms-sql-server-user-name = "viral";
define constant $ms-sql-server-user-password = "viral";
define constant $ms-sql-server-detect-null-column = #t;
define constant $ms-sql-server-do-introspection = #t;
define constant $dbms-table 
  = vector(vector(<odbc-dbms>, "ODBC", <odbc-user>, <odbc-database>, 
		    <odbc-sql-statement>));


define variable *datasource-name* :: <string> = "";
define variable *user-name* :: <string> = "";
define variable *user-password* :: <string> = "";
define variable *the-dbms* = #f;
define variable *detect-null-column* = #t;
define variable *do-introspection* = #t;

define variable *dbms-class* = <dbms>;
define variable *dbms-class-name* :: <string> = "";
define variable *dbms-user-class* = <user>;
define variable *dbms-database-class* = <database>;
define variable *dbms-sql-statement-class* = <sql-statement>;


define function pristine-database(dbms :: <dbms>)
  with-dbms(dbms)
    with-database(make(<database>, datasource-name: *datasource-name*),
                  make(<user>, user-name: *user-name*, 
                       password: *user-password*))
      block ()
        execute("drop table dwsql");
        execute("drop table trans_table");
      exception (condition :: <condition>)
      end block;
    end with-database;
  end with-dbms;
end function;

define method make-datasource-table(arguments :: <sequence>) 
 => (table :: <sequence>)
  let table = make(<deque>);

  if (member?("-oracle", arguments, test: \=))
    add!(table, vector($oracle-db-name, $oracle-user-name, 
 		       $oracle-user-password, $oracle-detect-null-column,
                       $oracle-do-introspection));
  end if;

  if (member?("-oracle8", arguments, test: \=))
    add!(table, vector($oracle8-db-name, $oracle8-user-name,
                       $oracle8-user-password, $oracle8-detect-null-column,
                       $oracle8-do-introspection));
  end if;

  if (member?("-access", arguments, test: \=))
    add!(table, vector($access-db-name, $access-user-name, 
		       $access-user-password, $access-detect-null-column,
                       $access-do-introspection));
  end if;

  if (member?("-ms-sql-server", arguments, test: \=))
    add!(table, vector($ms-sql-server-db-name, $ms-sql-server-user-name,
                       $ms-sql-server-user-password, 
                       $ms-sql-server-detect-null-column,
                       $ms-sql-server-do-introspection));
  end if;

  when (empty?(table))
    format-out("No arguments supplied: pass -oracle and/or -access\n");
    exit-application(-1);
  end;
  table
end method make-datasource-table;

define method main 
    (application-name :: <string>, arguments :: <sequence>)
 => (exit-code :: <integer>)
  ignore(application-name);
  block (exit)
    *odbc-print-condition* := #f;
    *odbc-report-success-with-info* := #f;
    *debug?* := #t; //#"crashed";

    *trace-odbc-functions* := #f;

    for (dbms in $dbms-table)
      *dbms-class* := dbms[0];
      *dbms-class-name* := dbms[1];
      *dbms-user-class* := dbms[2];
      *dbms-database-class* := dbms[3];
      *dbms-sql-statement-class* := dbms[4];

      for (datasource in make-datasource-table(arguments))
	*datasource-name* := datasource[0];
	*user-name* := datasource[1];
	*user-password* := datasource[2];
	*detect-null-column* := datasource[3];
        *do-introspection* := datasource[4];

	format-out("*** Testing database %s\n", *datasource-name*);
	perform-suite(creation-test-suite);
	*the-dbms* := make(*dbms-class*);

	pristine-database(*the-dbms*);

	perform-suite(connection-test-suite);
	perform-suite(ddl-test-suite);
	perform-suite(dml-test-suite);
	perform-suite(query-test-suite);
	perform-suite(collection-test-suite);
	perform-suite(transaction-test-suite);
	perform-suite(datatype-test-suite);
	perform-suite(big-integer-test-suite);
        if (*do-introspection*)
	  perform-suite(introspection-test-suite);
        end if;
      end for;
    end for;
  end block;
  0;
end method main;


block (exit)
  let handler <sql-warning> = 
    method(diag, next-handler)
      let next-diag = next-dbms-diagnostic(diag);
      unless (next-diag = #f)
        format-out("*** Handling sql-warning - next diagnostic: %=\n", next-diag);
        signal(next-diag);
        exit();
      end unless;
    end method;
  main(application-name(), application-arguments());
end block;
