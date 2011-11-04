Module: sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define generic default-dbms() 
 => (dbms :: <dbms>);

define thread variable *default-dbms* :: false-or(<dbms>) = #f;

define method default-dbms() => (dbms :: <dbms>)
  if (~*default-dbms*)
    error(make(<dbms-not-specified>));
  else
    *default-dbms*;
  end if;
end method;


define macro with-dbms
  { with-dbms(?dbms:expression) ?body:* end }
    => { dynamic-bind (*default-dbms* = ?dbms)
           ?body
         end; }
end macro with-dbms;

define constant <connection-deque> = limited(<deque>, of: <connection>);
define constant *all-connections* = make(<connection-deque>);
define constant *all-connections-lock* = make(<lock>);

define thread variable *default-connection* = #f;

define generic default-connection() => (connection :: <connection>);

define method default-connection()
 => (connection :: <connection>)
  if (~*default-connection*)
    error(make(<connection-not-specified>));
  else
    *default-connection*;
  end if;
end method;


define macro with-connection
  { with-connection(?connection:expression) ?body:* end }
    => { // Sanity check
	 if (instance?(?connection, <connection>) = #f)
	   error(make(<invalid-argument>));
	 end if;
	 dynamic-bind (*default-connection* = ?connection)
	   with-dbms(*default-connection*.dbms)
	     ?body
           end with-dbms;
         end dynamic-bind; }
end macro;


define macro with-database
  { with-database(?database:expression, ?user:expression) ?body:* end }
    => { let connection = #f;
	 block ()
	   connection := connect(?database, ?user);
  	   with-connection(connection)
	     ?body
           end with-connection;
         cleanup 
	   if (connection ~= #f)
 	     disconnect(connection);
	   end if;
       end block; }
end macro;


define macro sql
  { sql ?expr:* end }
    => { ?expr }
end macro;

