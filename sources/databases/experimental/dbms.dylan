Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: D-databases-sql-odbc!dbms.dylan(trunk.7) $


//------------------------------------------------------------
// As of version 1, the gc has a limitation in that objects
// referenced only by a week table are not collected if the
// week table is referenced during a gc cycle. What this means
// for the ODBC-SQL library is that finalize methods are not 
// called and ODBC resources are not deallocated resulting in
// an application that runs progressively slower and eventually
// crashing. Sigh.
//
// The set-finalize-notification and notify-of-finalization 
// functions are debugging tools. The function passed to
// set-finalize-notification will be called whenever a finalize
// method is invoked. And, if this function is never called then
// one can assume that the gc limitation is whacking you on the
// head and that explicit calls to finalize is needed in the 
// client application. In general, a client application only 
// needs to call finalize on instances of <odbc-sql-statement>.
// Resources associated with instances of <odbc-connection> are
// released when the connection is disconnected.
//
// Once the gc limitation on weak tables is fixed, these functions
// will no longer be needed.

define variable *finalize-notification* :: false-or(<function>) = #f;
define constant $finalize-notification-lock = make(<lock>);

define function set-finalize-notification(fn :: false-or(<function>))
 => ()
  with-lock($finalize-notification-lock)
    *finalize-notification* := fn;
  end with-lock;
end function;

define function notify-of-finalization(finalized-object :: <object>)
  with-lock($finalize-notification-lock)
    if (*finalize-notification* ~= #f)
      *finalize-notification*(finalized-object);
    end if;
  end with-lock;
end function;

//------------------------------------------------------------

define concrete class <odbc-dbms> (<dbms>)
   slot %environment-handle = $null-environment-handle,
    init-keyword: environment-handle:;

   constant slot %allocated-connections = make(<table>, weak: #"key");

   slot %dbms-name :: false-or(<string>) = #f;
   slot %dbms-version :: false-or(<string>) = #f;

   // rowsets are not being done appropriately. The various binding objects need to
   // allocate arrays whose length matches the rowset-size. In general, though, the
   // rowset-size hasn't been an issue with the exception of SQL Server and Access.
   // SQL Server needs a rowset-size > 1 to create server-side cursor which allows
   // multiple statements to be executed on a single cursor (can you say hack!). One
   // thing I don't understand is that SQL Server and Oracle are not retrieving rowsets.
   // Each row is being retrieved and stored in the bindings. If rowsets were being 
   // retrieve, I would expect that the records being retrieved (equal to rowset-size)
   // is stored in the bindings but since there is more than one row, the extras would
   // also be written to memory overwriting whatever was there. This doesn't seem to be
   // happening. 

  // nope, that isn't happening.  what's happening instead is that the
  // set of rows is being kept in the statement handle, and info is being
  // pulled out of the handle row by row.  following was 1:
   slot %dbms-rowset-size :: <integer> = 10;
end class;

define method initialize(dbms :: <odbc-dbms>, #key) 
 => ()
  next-method();

  automatic-finalization-enabled?-setter(#t);

  let (return-code, environment-handle) =
    SQLAllocHandle($sql-handle-env,null-pointer(<SQLHANDLE>));
  assert-odbc-goodness(return-code, 
		      environment-handle, 
		      $null-connection-handle,
		      $null-statement-handle);
  dbms.%environment-handle := environment-handle;
  let (return-code, environment-handle) =
   SQLSetEnvAttr(environment-handle, $sql-attr-odbc-version, $sql-ov-odbc3, 0);
  assert-odbc-goodness(return-code, 
		      environment-handle, 
		      $null-connection-handle,
		      $null-statement-handle);

  finalize-when-unreachable(dbms);
end method;

define constant $dbms-finalization-lock :: <lock> = make(<lock>);
// See comment in sql-statement.dylan on $sql-statement-finalization-lock.
define method finalize(dbms :: <odbc-dbms>) 
 => ()
  do(finalize, dbms.%allocated-connections.key-sequence);

  with-lock($dbms-finalization-lock)
    if (dbms.%environment-handle ~= $null-environment-handle)
      let return-code = SQLFreeHandle($sql-handle-env, dbms.%environment-handle);
      // if FreeEnv failed, the environment-handle must be passed to
      // assert-odbc-goodness in order to get the error from SQLError.
     assert-odbc-goodness(return-code, 
		          dbms.%environment-handle,
		          $null-connection-handle,
		          $null-statement-handle);
      dbms.%environment-handle := $null-environment-handle;
    end if;
  end with-lock;

  notify-of-finalization(dbms);
  next-method();
end method;

define method dbms-name(dbms :: <odbc-dbms>, 
                        #key connection :: <odbc-connection> = default-connection())
 => (name :: <string>)
  let (return-code, name) = nice-SQLGetInfo(connection.%connection-handle,
                                            $sql-dbms-name);
  assert-odbc-goodness(return-code,
                       dbms.%environment-handle,
                       connection.%connection-handle,
                       $null-statement-handle);
  dbms.%dbms-name := name;
end method;

define method dbms-version(dbms :: <odbc-dbms>,
                           #key connection :: <odbc-connection> = default-connection())
 => (version :: <string>)
  let (return-code, version) = nice-SQLGetInfo(connection.%connection-handle,
                                               $sql-dbms-ver);
  assert-odbc-goodness(return-code,
                       dbms.%environment-handle,
                       connection.%connection-handle,
                       $null-statement-handle);
  dbms.%dbms-version := version;
end method;

define method multiple-connections?(dbms :: <odbc-dbms>)
  => (result :: <boolean>)
  #t; //is this true?  How would I find out?
end method;

