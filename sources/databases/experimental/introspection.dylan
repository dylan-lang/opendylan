Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $introspection-rowset-size :: <integer> = 10;

define class <odbc-catalog> (<filtered-collection>,
                             <odbc-forward-only-result-set>,
                             <catalog>)
end class;

define method catalog-generator(result-set :: <odbc-result-set>)
  let stmt :: <odbc-sql-statement> = result-set.%sql-statement;
  let return-code = nice-SQLTables(stmt.%statement-handle,
			           $sql-all-catalogs,  "", "", "");
  assert-odbc-goodness(return-code,
                       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);			           
end method;

/* Note:  these liaison functions call each other until they finally
bottom out; the idea here is to get the data for a catalog, say,
lazily, rather than going and getting all of the catalog data, schema
data, table data, etc..  So, when a catalog record is returned, the
liaison function makes an <ODBC-catalog> object.  This in itself has a
liaison function, because it is a result set, and its liaison function
(the one that will be applied to the records that are of type schema)
has to know about this instance, so we create the function with curry,
so that this instance will be passed into that liaison function,
should anyone ever attempt to index into the catalog collection.
Similarly, the schema has many tables in it has its elements, and so
its liaison function must be created to pass in the particular
instance of the schema to the table creation function (table-liaison).
It's turtles all the way down. */

define method catalog-liaison(record :: <odbc-record>) 
 => (catalog :: <odbc-catalog>)
  let catalog = make(<odbc-catalog>,
                     filtered-source: #"self",
                     database-object-name: record[0],
                     generator: schema-generator,
                     rowset-size: record.statement.connection.dbms.%dbms-rowset-size,
// rowsets aren't working. $introspection-rowset-size,
                     filter: rcurry(schema-filter, pair(#f, #())),
                     connection: record.statement.connection);
  catalog.liaison := rcurry(schema-liaison, catalog);
  catalog;
end method;

define method catalog-from-name(a-connection :: <odbc-connection>,
                                catalog-name :: <string>)
 => (catalog :: <odbc-catalog>)
  let stmt =  make(<odbc-sql-statement>, connection: a-connection, text: "");
  let return-code = SQLSetStmtOption(stmt.%statement-handle,
                                    $SQL-ROWSET-SIZE, 
                                    a-connection.dbms.%dbms-rowset-size);
  // rowsets aren't working  $introspection-rowset-size);

  assert-odbc-goodness(return-code,
		       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  let return-code = nice-SQLTables(stmt.%statement-handle, catalog-name,  "", "", "");
  assert-odbc-goodness(return-code,
                       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);	

  // The column count returned by SQLNumResultCols is ODBC's way
  // of checking if the executed sql statement returns a result-set.

  let (return-code, column-count) = SQLNumResultCols(stmt.%statement-handle);
  assert-odbc-goodness(return-code,
	               stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  if (column-count = 0)
    signal(make(<catalog-not-found>, catalog-name: catalog-name));		       
  else    
    let catalog = make(<odbc-catalog>,
                       filtered-source: #"self",
                       connection: a-connection,
                       database-object-name: catalog-name,
                       generator: schema-generator,
                       rowset-size: a-connection.dbms.%dbms-rowset-size,
    // rowsets aren't working  $introspection-rowset-size,
                       filter: rcurry(schema-filter, pair(#f, #())));
    catalog.liaison := rcurry(schema-liaison, catalog);
    catalog;
  end if;
end method;

define method catalogs-assist(the-connection :: <odbc-connection>)
 => (result-set :: <odbc-result-set>)
  make(<odbc-result-set>,
       generator: catalog-generator,
       connection: the-connection,
       rowset-size: the-connection.dbms.%dbms-rowset-size,
  // rowsets aren't working  $introspection-rowset-size,
       result-set-policy: make(<result-set-policy>, 
                               rowset-size: the-connection.dbms.%dbms-rowset-size),
       liaison: catalog-liaison);
end method;

define class <odbc-schema> (<odbc-forward-only-result-set>, <schema>)
// This slot isn't being used by anyone.
//  constant slot schema-catalog :: false-or(<odbc-catalog>) = #f,
//    init-keyword: schema-catalog:;
end class;

define method schema-generator(catalog :: <odbc-catalog>)
 => ()      
  let stmt = catalog.%sql-statement;
  /* MS SQL Server doesn't support search qualifier for tables unless a null
     pointer is passed. Of course, the ODBC documentation says a wild card and
     null pointer are the same thing. */
  let dbms-name = catalog.connection.dbms.%dbms-name;
  let table-qualifier = if (as-lowercase(dbms-name) = "microsoft sql server")
                          #f
                        else
                          ""
                        end if;
  let return-code = nice-SQLTables(stmt.%statement-handle,
			           catalog.database-object-name,  $sql-all-schemas, 
                                   table-qualifier, #f);
  assert-odbc-goodness(return-code,
                       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);	
end method;


/*  ---  schema-filter ---
Filter functions return true if they intend to skip the object, and
false if they intend to return the object.

If the name changes, then we are returning false.  The purpose of the
schema-tracking pair is for initialization: if we have not yet seen
any elements, then we have to return false (returning object), but we
need to store the first element that we have seen, so that we can
compare the name with the previous name.

Does this actually work?  It seems like if there were actually three
schemas, then we would be returning more than one element for the
second and third schemas.  It seems like schema-tracking.tail needs to
be changed whenever this function returns false (that is,
schema.database-object-name ~= schema-tracking.tail.database-object-name).
*/
define method schema-filter(result-set :: <odbc-result-set>,
                            schema :: <odbc-schema>,
                            schema-tracking :: <pair>)
 => (filter? :: <boolean>)
  if (schema-tracking.head = #f)
    schema-tracking.tail := schema;
    schema-tracking.head := #t;
    #f
  else
    schema.database-object-name = schema-tracking.tail.database-object-name;
  end if;
end method;

define method schema-liaison(record :: <odbc-record>, 
                             catalog :: <odbc-catalog>) 
 => (schema :: <odbc-schema>)
  let schema = make(<odbc-schema>,
                    database-object-name: record[1],
                    generator: rcurry(table-generator, catalog),
                    rowset-size: record.statement.connection.dbms.%dbms-rowset-size,
  // rowsets aren't working  $introspection-rowset-size,
                    connection: record.statement.connection);
  schema.liaison := rcurry(table-liaison, catalog, schema);
  schema;
end method;

define method schema-from-name(a-connection :: <odbc-connection>,
                               catalog-name :: <string>,
                               schema-name :: <string>)
 => (schema :: <odbc-schema>)
  let stmt = make(<odbc-sql-statement>, 
                  connection: a-connection, text: "");
  let return-code = SQLSetStmtOption(stmt.%statement-handle,
                                     $SQL-ROWSET-SIZE, 
                                     a-connection.dbms.%dbms-rowset-size);
  // rowsets aren't working  $introspection-rowset-size);
  assert-odbc-goodness(return-code,
		       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  let return-code = nice-SQLTables(stmt.%statement-handle, catalog-name,
			           schema-name, #f, #f);
  assert-odbc-goodness(return-code,
                       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);	
  let (return-code, column-count) = SQLNumResultCols(stmt.%statement-handle);
  assert-odbc-goodness(return-code,
	               stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  if (column-count = 0)
    signal(make(<schema-not-found>, schema-name: schema-name));		       
  else
    let catalog = catalog-from-name(a-connection, catalog-name);
    let schema = make(<odbc-schema>,
                      database-object-name: schema-name,
                      connection: a-connection,
                      rowset-size: a-connection.dbms.%dbms-rowset-size,
    // rowsets aren't working  $introspection-rowset-size,
                      generator: rcurry(table-generator, catalog));
    schema.liaison := rcurry(table-liaison, catalog, schema);
    schema;
  end if;
end method;

define class <odbc-table> (<odbc-forward-only-result-set>, <sql-table>)
end class;

define method table-generator(schema :: <odbc-schema>, catalog :: <odbc-catalog>)
 => ()
  let stmt = schema.%sql-statement;
  let return-code = nice-SQLTables(stmt.%statement-handle,
			           catalog.database-object-name,  
                                   schema.database-object-name, 
                                   $sql-all-tables, #f);
  assert-odbc-goodness(return-code,
                       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);			           
end method;

define method table-liaison(record :: <odbc-record>, 
                            catalog :: <odbc-catalog>,
                            schema :: <odbc-schema>) 
 => (table :: <odbc-table>)
  make(<odbc-table>,
       database-object-name: record[2],
       generator: rcurry(column-generator, catalog, schema),
       connection: record.statement.connection,
       rowset-size: record.statement.connection.dbms.%dbms-rowset-size,
  // rowsets aren't working  $introspection-rowset-size,
       liaison: column-liaison);
end method;

define method table-from-name(a-connection :: <odbc-connection>,
                              catalog-name :: <string>, 
                              schema-name :: <string>,
                              table-name :: <string>)
 => (table :: <odbc-table>)
  let stmt = make(<odbc-sql-statement>, connection: a-connection, text: "");
  let return-code = SQLSetStmtOption(stmt.%statement-handle,
                                     $SQL-ROWSET-SIZE, 
                                     a-connection.dbms.%dbms-rowset-size);
  // rowsets aren't working  $introspection-rowset-size);
  assert-odbc-goodness(return-code,
		       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);
  let return-code = nice-SQLTables(stmt.%statement-handle, catalog-name,  
                                   schema-name, table-name, #f);
  assert-odbc-goodness(return-code,
		       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  let (return-code, column-count) = SQLNumResultCols(stmt.%statement-handle);
  assert-odbc-goodness(return-code,
	               stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);

  if (column-count = 0)
    signal(make(<table-not-found>, table-name: table-name));		       
  else
    let catalog = catalog-from-name(a-connection, catalog-name);
    let schema = schema-from-name(a-connection, catalog-name, schema-name);
    make(<odbc-table>,
         database-object-name: table-name,
         generator: rcurry(column-generator, catalog, schema),
         connection: a-connection,
         rowset-size: a-connection.dbms.%dbms-rowset-size,
    // rowsets aren't working  $introspection-rowset-size,
         liaison: column-liaison);
  end if;
end method;


define class <odbc-column> (<column>)
end class;

define method column-generator(table :: <odbc-table>, 
                               catalog :: <odbc-catalog>,
                               schema :: <odbc-schema>)
 => ()
  let stmt = table.%sql-statement;
  let return-code = nice-SQLColumns(stmt.%statement-handle, 
                                    catalog.database-object-name, 
                                    schema.database-object-name, 
                                    table.database-object-name, #f);
  assert-odbc-goodness(return-code,
                       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);			           
end method;

define method column-liaison(record :: <odbc-record>) 
 => (column :: <odbc-column>)
  let column-name = record[3];
  let data-type = record[4];
  let nullable = record[10];

  make(<odbc-column>,
       database-object-name: column-name, 
       domain: element($sql-datatype-table, data-type, default: unfound),
       nullable?: if (nullable > 0) #t else #f end if,
       default-value: #f);
end method;


define class <odbc-index>(<index>)
end class;
  
define function new-index(collection :: <odbc-indexes>, 
                          index-record :: <odbc-record>)
 => (result :: <object>)
  let catalog-name = index-record[0];
  let schema-name = index-record[1];
  let table-name = index-record[2];
  let non-unique = index-record[3];
  let index-name = index-record[5];
  let column-name = index-record[8];
  let table = table-from-name(index-record.statement.connection, 
                              catalog-name, schema-name, table-name);
  let index  = make(<odbc-index>,
                    database-object-name: index-name,
                    unique-index?: ~non-unique,
                    indexed-table: table);
  index-grouper(collection, index, index-record);
  index;
end function;

define function index-finished?(collection :: <odbc-indexes>,
                                index :: <odbc-index>,
                                index-record :: <odbc-record>)
 => (result :: <boolean>)
  let index-name = index-record[5];
  let done? = index-name ~= index.database-object-name;
  done?;
end function;

define function index-grouper(collection :: <odbc-indexes>,
                              index :: <odbc-index>,
                              index-record :: <odbc-record>)
 => ()
  let name = index-record[8];
  let column = make(<odbc-column>,
                    database-object-name: name);
                   // table: current-index.table);
  push-last(index.fields, column);
end function;

define class <odbc-indexes> (<grouped-collection>,
                             <filtered-collection>,
                             <odbc-forward-only-result-set>)
  inherited slot filter, 
    init-value: method(collection, index-record)
                  let index-type = index-record[6];
                  index-type = $SQL-TABLE-STAT;
                end method;
end class;

define method indexes(table :: <odbc-table>)
 => (result-set :: <odbc-result-set>)
  generate-data(table);
  make(<odbc-indexes>,
       filtered-source: #"self",
       grouped-source: #"self",
       new-group: new-index,
       group-finished?: index-finished?,
       grouper: index-grouper,
       generator: rcurry(index-generator, table),
       rowset-size: table.%sql-statement.connection.dbms.%dbms-rowset-size,
  // rowsets aren't working  $introspection-rowset-size,
       connection: table.%sql-statement.connection);
end method;

define method index-generator(result-set :: <odbc-result-set>, table :: <odbc-table>)
  let stmt = result-set.%sql-statement;
  let return-code = nice-SQLStatistics(stmt.%statement-handle, #f, #f, 
                                       table.database-object-name,
		                       $SQL-INDEX-ALL, $SQL-QUICK);
  assert-odbc-goodness(return-code,
		       stmt.connection.dbms.%environment-handle,
		       stmt.connection.%connection-handle,
		       stmt.%statement-handle);
end method;



define method default-value(col :: <odbc-column>)
  => result :: <object>;
  signal(make(<feature-not-supported>));
end method;

define method constraints (table :: <odbc-table>)
 => result :: <result-set>; 
// only really I think it ought not to be a result set, I think it ought to
// be a boring old vector, like indexes.

  signal(make(<feature-not-supported>));

//  Call sqlprimarykeys to collect up a result set of primary keys.
//  Create an object which has a slot which contains this result set.
//  This object will be of type <primary-key-constraint>.

//  Next call sqlforeignkeys to collect up a result set of foreign keys.
//  Etc, ... of type <referential-constraint>.

//  Call sqlcolumns, or more likely just iterate over the table (whups, 
//  don't those forward only result sets suck?) and fetch out the columns
//  whose .unique? is true.  This is a <unique-constraint>.

//  We can't do checks or assertions.
end method;
