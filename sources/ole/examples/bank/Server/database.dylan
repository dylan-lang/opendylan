Module: bank-server
Synopsis: SQL database support
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant $dbms :: <dbms> = make(<odbc-dbms>);

define constant $default-datasource :: <string> = "bankDB";
define constant $default-user-name :: <string> = "";
define constant $default-password :: <string>  = "";

define method connect-to-database (#key datasource = $default-datasource,
                                        user-name = $default-user-name,
                                        password = $default-password)
 => (conn :: <connection>)
  with-dbms ($dbms)
    let database = make(<database>, datasource-name: datasource);
    let user =  make(<user>, user-name: user-name, password: password);
    let connection = connect(database, user);
    connection
  end;
end connect-to-database;

define method database-execute (conn :: <connection>,
                                text :: <string>,
                                parameters :: <sequence>)
  => (result-set)
  with-dbms (conn.dbms)
    let query = make(<sql-statement>, text: text, connection: conn);
    execute(query, parameters: parameters);
  end;
end database-execute;


