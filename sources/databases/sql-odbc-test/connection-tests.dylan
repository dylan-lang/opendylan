Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//$HopeName: D-databases-sql-odbc-test!connection-tests.dylan(trunk.3) $


define test single-connection-test()
  with-dbms(*the-dbms*)
    check-condition("Connection test: bad datasource-name",
		    <connection-exception>,
		    block ()
		      let connection = 
			connect(make(<database>, 
				     datasource-name: "bad-db-name"),
				make(<user>,
				     user-name: *user-name*,
				     password: *user-password*));
		      // In the event a condition is not thrown, disconnect
		      // from the database.
		      disconnect(connection);
		    end block);

    check("Connection test: good datasource-name", 
	  method ()
	    let connection = connect(make(<database>, 
					  datasource-name: *datasource-name*),
				     make(<user>,
					  user-name: *user-name*,
					  password: *user-password*));
	    disconnect(connection);
	    #t
	  end method);

    check("Connection test: short-cut",
	  method ()
	    let db = make(<database>, datasource-name: *datasource-name*);
	    let user = make(<user>,
			    user-name: *user-name*,
			    password: *user-password*);
	    let connection = connect(db, user);
	    disconnect(connection);
	    #t;
	  end method);
  end with-dbms;

  check-condition("Connection test: dbms not specified in connect call",
		  <dbms-not-specified>,
		  connect(make(<database>, 
			       datasource-name: *datasource-name*),
			  make(<user>,
			       user-name: *user-name*,
			       password: *user-password*)));
end test;

define test multiple-connection-test()
  with-dbms(*the-dbms*)
    check("Multiple connections test",
	  method ()
	    let db = make(<odbc-database>,
			  datasource-name: *datasource-name*);
	    let user = make(<odbc-user>,
			    user-name: *user-name*,
			    password: *user-password*);
	    let a-connection = connect(db, user);
	    let b-connection = connect(db, user);
	    disconnect(a-connection);
	    disconnect(b-connection);
	    #t
	  end method);
  end with-dbms;
end test;

define test misc-connection-functions-test()
  with-dbms(*the-dbms*)
    let con = connect(make(<odbc-database>,
			   datasource-name: *datasource-name*),
		      make(<odbc-user>, user-name: *user-name*, 
			   password: *user-password*),
		      dbms: *the-dbms*);
  check-equal("There is one connection in the list of connections.",
	      size(connections()), 1);
  check-equal("And that connection is the one we created.",
	      connections()[0], con);
  disconnect-all(dbms: make(<odbc-dbms>));
  check-equal("It's still here.",size(connections()), 1);
  disconnect-all();
  check-equal("Now there are none---we disconnected everything.",
	      size(connections()), 0);
  end with-dbms;
end test;

define suite connection-test-suite()
  test single-connection-test;
  test multiple-connection-test;
  test misc-connection-functions-test;
end suite;
