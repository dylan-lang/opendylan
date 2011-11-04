Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test dbms-make-test()
  let dbms = make(*dbms-class*);
  check-true(concatenate(*dbms-class-name*, " <dbms> make test"), 
	     instance?(dbms, *dbms-class*));
  check-condition("invalid <dbms> make test", <condition>, make(<dbms>));
end test;


define test user-make-test()
  let dbms = make(*dbms-class*);

  check-true(concatenate(*dbms-class-name*, " <user> make test"), 
	     instance?(make(*dbms-user-class*), *dbms-user-class*));

  check-true(concatenate(*dbms-class-name*, " <user> short-cut make test"),
	       with-dbms(dbms)
		 instance?(make(<user>), *dbms-user-class*)
 	       end with-dbms);

  check-condition("<user> invalid short-cut make test",
		  <dbms-not-specified>,
		  make(<user>));
end test;


define test database-make-test()
  let dbms = make(*dbms-class*);
  check-true(concatenate(*dbms-class-name*, " <database> make test"),
	     instance?(make(*dbms-database-class*,
			    datasource-name: *datasource-name*),
		       *dbms-database-class*));

  check-true(concatenate(*dbms-class-name*, " <database> short-cut make test"),
	     with-dbms(dbms)
	       instance?(make(<database>,
			      datasource-name: *datasource-name*),
			 *dbms-database-class*)
	     end with-dbms);

  check-condition("<database> invalid short-cut make test",
		  <dbms-not-specified>,
		  make(<database>));
end test;


define suite creation-test-suite()
  test dbms-make-test;
  test user-make-test;
  test database-make-test;
end suite;

