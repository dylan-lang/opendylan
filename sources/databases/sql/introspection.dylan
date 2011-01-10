Module:    sql-implementation
Author:    eec, yduJ
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//  Introspection classes and generics
define abstract class <database-object> (<object>)
  slot database-object-name :: <string>,
    required-init-keyword: name:;
end class <database-object>;

define abstract class <constraint> (<database-object>)
end class;

define abstract class <unique-constraint> (<constraint>)
end class;

define abstract class <check-constraint> (<constraint>)
end class; 

define abstract class <referential-constraint> (<constraint>)
end class;

define abstract class <assertion-constraint> (<constraint>)
end class;

define open generic constraints (db-object :: <database-object>)
 => result :: <result-set>;

//+++ What does connection and connection-setter do?
define open generic connection (o :: <object>)
 => result :: <connection>;

define open generic connection-setter (c :: <connection>, o :: <object>)
 => result :: <connection>;


define open abstract class <catalog> (<database-object>, <result-set>)
  slot connection :: <connection> = default-connection(),
    init-keyword: connection:;
end class;


define open generic catalogs (#key connection :: <connection>)
 => (result-set :: <result-set>);

define open generic catalogs-assist(connection :: <connection>)
 => (result-set :: <result-set>);

define method catalogs(#key connection :: <connection> = default-connection())
 => (result-set :: <result-set>)
  catalogs-assist(connection);
end method;  

define open abstract class <schema> (<database-object>, <result-set>)
end class;

define open abstract class <sql-table> (<database-object>, <result-set>)
end class;

define open generic indexes(table :: <sql-table>) 
 => index-collection :: <result-set>;

define open abstract class <column> (<database-object>)
  constant slot domain :: <object>, // <sql-type>
    init-keyword: domain:;
  constant slot nullable? :: <boolean>,
    init-keyword: nullable?:;
  open constant slot default-value :: <object>,
    init-keyword: default-value:;    
end class <column>;

define open generic default-value (column :: <column>)
 => default :: <object>;

define open abstract class <index> (<database-object>) 
  slot indexed-table :: <sql-table>,
    init-keyword: indexed-table:;
  slot fields :: <deque> = make(<deque>);
  constant slot unique-index? :: <boolean>,
    init-keyword: unique-index?:;
end class <index>;

define open generic catalog-from-name(connection :: <connection>,
			              name :: <string>)
 => (catalog :: <catalog>);

define open generic schema-from-name(connection :: <connection>,
                                     catalog-name :: <string>,
                                     schema-name :: <string>)
 => (schema :: <schema>);

define open generic table-from-name(connection :: <connection>,
                                    catalog-name :: <string>,
                                    schema-name :: <string>,
                                    table-name :: <string>)
 => (table :: <sql-table>);

define abstract class <database-object-not-found> (<diagnostic>)
end class;

define class <catalog-not-found> (<database-object-not-found>)
  constant slot catalog-name :: <string>,
    required-init-keyword: catalog-name:;
end class;

define class <schema-not-found> (<database-object-not-found>)
  constant slot schema-name :: <string>,
    required-init-keyword: schema-name:;
end class;

define class <table-not-found> (<database-object-not-found>)
  constant slot table-name :: <string>,
    required-init-keyword: table-name:;
end class;
