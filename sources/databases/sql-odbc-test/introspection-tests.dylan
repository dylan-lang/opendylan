Module: sql-odbc-test
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test resource-stress-test()
  let catalog-list = catalogs(connection: *introspection-connection*);
  for (catalog :: <odbc-catalog> in catalog-list)
    for (schema :: <schema> in catalog)
      for (a-table :: <sql-table> in schema)
        for (column :: <column> in a-table)
        end for;

        for (index :: <index> in indexes(a-table))
        end for;
      end for;
    end for;
  end for; 
end test;

define test simple-introspection-test()
  let table-found? = #f;
  let catalog-list = catalogs(connection: *introspection-connection*);
  for (catalog :: <odbc-catalog> in catalog-list)
    for (schema :: <schema> in catalog)
      for (a-table :: <sql-table> in schema)
        if (a-table.database-object-name.as-lowercase = "dwsql")
          table-found? := #t;
          examine-columns(a-table);
          examine-indices(a-table);
        end if;
      end for;
    end for;
  end for; 
  check-true("dwsql table found", table-found?);
end test;

define method examine-columns(a-table :: <sql-table>)
 => ()
  check-true("Table name is dwsql", 
             as-lowercase(a-table.database-object-name) = "dwsql");
  check-true("Table dwsql has two columns", a-table.size = 2);
  for (column keyed-by column-index :: <column> in a-table)
    check-true("Column is col_1 or col_2",
               as-lowercase(column.database-object-name) = "col_1"
	       | as-lowercase(column.database-object-name) = "col_2");

    if (*detect-null-column* = #t)
      select (as-lowercase(column.database-object-name) by \=)
        "col_1" => check-true("Column is nullable", column.nullable?);
        "col_2" => check-false("Column is not nullable", column.nullable?);
      end select;
    end if;

    // What about testing the column's domain?
  end for;
end method;

define method examine-indices(table :: <sql-table>)
  let count :: <integer> = 0;
  for (index in indexes(table))
    count := count + 1;
    let index-name = as-lowercase(index.database-object-name);
    check-true("Index has a name we expected", 
               index-name = "index1" | index-name = "index2" | index-name = "indexboth");
    select (index-name by \=)
      "index1" =>
        check-true("Index1 - column count = 1", index.fields.size = 1);
        check-true("Index1 - Indexed field is col_1",
                   as-lowercase(index.fields[0].database-object-name) = "col_1");
      "index2" => 
        check-true("Index1 - column count = 1", index.fields.size = 1);
        check-true("Index1 - Indexed field is col_2",
                   as-lowercase(index.fields[0].database-object-name) = "col_2");
      "indexboth" =>
        check-true("Indexboth - column count = 2", index.fields.size = 2);
        check-true("Indexboth - Indexed fields are col_1 and col_2",
                   begin
                     let field-0 = as-lowercase(index.fields[0].database-object-name);
                     let field-1 = as-lowercase(index.fields[1].database-object-name);
                     (field-0 = "col_1" & field-1 = "col_2") | 
                       (field-0 = "col_2" & field-1 = "col_1")
                   end);
    end select
  end for;
  check-equal("Three indices total", count, 3);
end method;

define method examine-constraints(table :: <sql-table>)
//  check-true("We need to know what constraints are all about!",#f);
end method;

define variable *introspection-connection* = #f;

define method create-introspection-test-table()
  with-connection(*introspection-connection*)
    execute("create table dwsql (col_1 char(50), col_2 number not null)");

    execute("create index index1 on dwsql (col_1)");
    execute("create index index2 on dwsql (col_2)");
    execute("create index indexboth on dwsql (col_1, col_2)");
  end with-connection;
end method;


define method introspection-test-setup()
  with-dbms(*the-dbms*)
    let database = make(<database>, datasource-name: *datasource-name*);
    let user = make(<user>, user-name: *user-name*, password: *user-password*);
    *introspection-connection* := connect(database, user);
    create-introspection-test-table();
  end with-dbms;
end method;


define method introspection-test-cleanup()
  with-connection(*introspection-connection*)
    execute("drop table dwsql");
  end with-connection;
  disconnect(*introspection-connection*);
  *introspection-connection* := #f;
end method;

define suite introspection-test-suite
  (setup-function: introspection-test-setup,
   cleanup-function: introspection-test-cleanup)
  test simple-introspection-test;
  // Until the GC is fixed, it is not possible to run this unless you
  // bump the VM up to something like 300MB and wait a few hours for
  // it to complete.
  //test resource-stress-test;
end suite;













