Module: sql-odbc-nice-layer
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant <return-code> = <integer>;
define constant <environment-handle> = <SQLHENV>;
define constant <connection-handle> = <SQLHDBC>;
define constant <statement-handle> = <SQLHSTMT>;
define constant <fetch-orientation> = <SQLSMALLINT>;
define constant <fetch-offset> = <SQLINTEGER>;

define constant $null-statement-handle   = null-pointer(<lpsqlhstmt>);
define constant $null-environment-handle = null-pointer(<lpsqlhenv>);
define constant $null-connection-handle  = null-pointer(<lpsqlhdbc>);


define method null-odbc-field?(indicator :: <c-int*>)
 => (null :: <boolean>)
  pointer-value(indicator) == $SQL-NULL-DATA;
end method;


define method  nice-SQLFreeEnv(environment-handle :: <environment-handle>)
 => (return-code :: <return-code>);
  let return-code = SQLFreeHandle($sql-handle-env, environment-handle);

  return-code;
end method;


define method nice-SQLError(environment-handle :: <environment-handle>,
			    connection-handle :: <connection-handle>,
			    statement-handle :: <statement-handle>)
 => (return-code :: <integer>, 
     sql-state :: <string>, 
     native-error :: <integer>, 
     error-message :: <string>)
  with-stack-structure (sql-state :: <C-string>, element-count: 6)
    with-stack-structure (error-message :: <C-string>,
			  element-count: $sql-max-message-length)
      let (return-code, native-error, error-message-length) =
	SQLError(environment-handle, 
		 connection-handle,
		 statement-handle,
		 sql-state,
		 error-message,
		 size(error-message));
      let return-state = as(<byte-string>, sql-state);
      let return-message = as(<byte-string>, error-message);
      values(return-code, return-state, native-error, return-message);
    end with-stack-structure;
  end with-stack-structure;
end method;


define method if-null(str) 
// treating an empty string as a null string was causing SQLTable to
// execute incorrectly. 
//  if (str = "" | str == #f) 
  if (str == #f)
    values(null-pointer(<C-string>), 0)
  else
    values(str, size(str))
  end if
end method;


define method nice-SQLConnect(connection-handle :: <connection-handle>, 
			      datasource-name :: <string>, 
			      user-identifier :: <string>, 
			      password :: <string>) 
 => (return-code :: <return-code>)
  let (ds-name, ds-name-size) = if-null(datasource-name);
  let (user-id, user-id-size) = if-null(user-identifier);
  let (password, password-size) = if-null(password);
  let return-code = SQLConnect(connection-handle,
			       ds-name, ds-name-size,
			       user-id, user-id-size,
			       password, password-size);
  
  return-code;
end method;


define method nice-SQLDriverConnect(connection-handle :: <connection-handle>, 
				    connection-string :: <string>)
 => (return-code :: <return-code>)
  let return-code = SQLDriverConnect(connection-handle,
				     null-pointer(<SQLHWND>),
				     connection-string,
				     $SQL-NTS,
				     null-pointer(<C-string>), 0,
				     $SQL-DRIVER-COMPLETE);

  return-code;                    
end method;


define method nice-SQLGetConnectAttr(connection-handle :: <connection-handle>,
				     attribute :: <integer>)
  with-stack-structure (option-value-pointer :: <LPSQLUINTEGER>)
    let return-code = SQLGetConnectAttr(connection-handle, attribute,
					option-value-pointer, 4);
    values(return-code, pointer-value(option-value-pointer));
  end with-stack-structure;
end method;

define method nice-SQLSetConnectAttr(connection-handle :: <connection-handle>,
				     attribute :: <integer>, value :: <integer>)
  //need to pass in stupid fourth arg --  pointer to size of value, which 
  //is ignored when we are passing in integer values (as we always are).
  SQLSetConnectAttr(connection-handle, attribute, value, 0); 
end method;

//deprecated
define method nice-SQLGetConnectOption(connection-handle :: 
					 <connection-handle>,
				       option-id :: <integer>)
 => (return-code :: <return-code>, 
     option-value :: <object>)
  with-stack-structure (option-value-pointer :: <LPSQLUINTEGER>)
    let return-code = SQLGetConnectOption(connection-handle,
					  option-id,
					  option-value-pointer);
    let option-value = pointer-value(option-value-pointer);
    values(return-code, option-value);
  end with-stack-structure;
end method;


define method nice-SQLPrepare(statement-handle :: <statement-handle>,
			      sql-text :: <string>)
  => (return-code :: <return-code>)
  let return-code = SQLPrepare(statement-handle,
			       sql-text,
			       size(sql-text));

  return-code;
end method;

define method nice-SQLSetPos(statement-handle :: <statement-handle>,
			     row :: <integer>,
			     option :: <integer>,
			     flock :: <integer>)
 => (return-code :: <return-code>)
  let return-code = SQLSetPos(statement-handle, row, option, flock);
  return-code;
end method;

define method nice-SQLExecute(statement-handle :: <statement-handle>)
  => (return-code :: <return-code>)
  let return-code = SQLExecute(statement-handle);

  return-code;
end method;


define method nice-SQLExecDirect(statement-handle :: <statement-handle>,
				 sql-statement :: <string>)
 => (return-code :: <return-code>)
  let return-code = SQLExecDirect(statement-handle,
				  sql-statement,
				  size(sql-statement));

  return-code;
end method;


define not-inline method nice-SQLFetch(statement-handle :: <statement-handle>)
  => (return-code :: <return-code>)
  let return-code = SQLFetch(statement-handle);
  format-out("sqlfetch: %=\n",statement-handle);
  return-code;
end method;

define method nice-SQLFetchScroll(statement-handle :: <statement-handle>,
				  fetch-orientation :: <fetch-orientation>,
				  fetch-offset :: <fetch-offset>)
 => (return-code :: <return-code>)
  let return-code = SQLFetchScroll(statement-handle, fetch-orientation, fetch-offset);

  return-code;
end method;




define method nice-SQLBindCol(statement-handle :: <statement-handle>,
			      column-number :: <integer>,
			      data-type :: <integer>,
			      storage :: <object>,
			      storage-size :: <integer>,
			      storage-byte-count :: <object>)
  => (return-code :: <return-code>)
  let return-code = SQLBindCol(statement-handle,
			       column-number,
			       data-type,
			       storage,
			       storage-size,
			       storage-byte-count);
  return-code;
end method;


define method nice-SQLAllocStmt(connection-handle :: <connection-handle>)
  => (return-code :: <return-code>, statment-handle :: <statement-handle>);
  let (return-code, statement-handle) =
    SQLAllocHandle($sql-handle-stmt, connection-handle);

  values(return-code, statement-handle);
end method;

define method nice-SQLFreeStmt(statement-handle :: <statement-handle>,
			       option :: <object>)
  => (return-code :: <return-code>);
  let return-code = 
    if (option == $sql-drop)
      nice-SQLFreeHandle($sql-handle-stmt, statement-handle);
    else
      SQLFreeStmt(statement-handle, option);
    end if;
  return-code;
end method;


//+++++ E fucking gads.
//Lose this ASAP.
define function collect-garbage ()
  block ()
    primitive-mps-collect();
    values(raw-as-integer(primitive-mps-committed()), 0)
  afterwards
    primitive-mps-release();
  end;
end function;

define variable *number-of-statement-handles* = 0;
define constant $max-statement-handles = 140; //empirically determined
define variable *statement-handles-gc-threshold* = 70;

define method nice-SQLAllocHandle(type :: <integer>,
				  parent-handle :: <SQLHANDLE>)
 => (return-code :: <return-code>, handle :: <SQLHANDLE>);
  if (type == $sql-handle-stmt)
    *number-of-statement-handles* := *number-of-statement-handles* + 1;
    if (*number-of-statement-handles* > *statement-handles-gc-threshold*)
      let orig-handles = *number-of-statement-handles*;
      collect-garbage();
      *statement-handles-gc-threshold* :=
	max(*statement-handles-gc-threshold*,
	    *number-of-statement-handles* + 10);
    end if;
  end if;
  let (return-code, handle) =
    SQLAllocHandle(type, parent-handle);

  values(return-code, handle);
end method;

define method nice-SQLFreeHandle (type :: <integer>, handle :: <SQLHANDLE>)
 => (return-code :: <return-code>);
  if (type == $sql-handle-stmt)
    *number-of-statement-handles* := *number-of-statement-handles* - 1;
  end if;
  SQLFreeHandle(type, handle);
end method;


define method nice-SQLNumResultCols(statement-handle :: <statement-handle>)
  => (return-code :: <object>, column-count :: <integer>);
  let (return-code, column-count) = SQLNumResultCols(statement-handle);
  values(return-code, column-count);
end method;


define method nice-SQLDescribeCol(statement-handle :: <statement-handle>,
				  column-number :: <integer>)
 => (return-code :: <object>,
     name :: <object>,
     data-type :: <object>,
     precision :: <object>,
     scale :: <object>,
     nullable :: <object>);
  let name-length = 128;  //+++ should call SQLGetInfo to get name size
  with-stack-structure (raw-column-name :: <C-string>, element-count: name-length)
    let (return-code, actual-name-length, data-type, 
	 precision, scale, nullable) =
      SQLDescribeCol(statement-handle,
		     column-number,
		     raw-column-name,
		     name-length);
 
      values(return-code, as(<byte-string>, raw-column-name), data-type,
	     precision, scale, nullable);
  end with-stack-structure;
end method;

define method nice-SQLTables(statement-handle :: <object>,
			     qual :: false-or(<string>),
			     owner :: false-or(<string>),
			     tablename :: false-or(<string>),
			     tabletype :: false-or(<string>))
  let (qual-name, qual-size) = if-null(qual);
  let (owner-name, owner-size) = if-null(owner);
  let (tablename-name, tablename-size) = if-null(tablename);
  let (tabletype-name, tabletype-size) = if-null(tabletype);
  SQLTables(statement-handle, qual-name, qual-size, owner-name,
	    owner-size, tablename-name, tablename-size,
	    tabletype-name, tabletype-size)
end method;

define method nice-SQLColumns(statement-handle :: <object>,
			      qual :: false-or(<string>),
			      owner :: false-or(<string>),
			      tablename :: false-or(<string>),
			      colname :: false-or(<string>))
  let (qual-name, qual-size) = if-null(qual);
  let (owner-name, owner-size) = if-null(owner);
  let (tablename-name, tablename-size) = if-null(tablename);
  let (colname-name, colname-size) = if-null(colname);
  SQLColumns(statement-handle, qual-name, qual-size, owner-name,
	    owner-size, tablename-name, tablename-size,
	    colname-name, colname-size)
end method;

define method nice-SQLStatistics(statement-handle :: <object>,
				 qual :: false-or(<string>),
				 owner :: false-or(<string>),
				 tablename :: false-or(<string>), 
				 unique :: <integer>,
				 accuracy :: <integer>)
  let (qual-name, qual-size) = if-null(qual);
  let (owner-name, owner-size) = if-null(owner);
  let (tablename-name, tablename-size) = if-null(tablename);
  SQLStatistics(statement-handle, qual-name, qual-size, owner-name,
	    owner-size, tablename-name, tablename-size, unique, accuracy)
end method;

define method nice-SQLGetDiagRec(handle-type :: <object>,
				 handle :: <object>,
				 rec-number :: <integer>)
  => (return-code :: <object>,
      sql-state :: <object>,
      native-error :: <object>,
      message :: <object>)
  let message-length = 512;
  with-stack-structure (sql-state :: <C-string>, element-count: 6)
    with-stack-structure (message :: <C-string>, element-count: message-length)
      let (return-code, native-error, text-length) = 
        SQLGetDiagRec(handle-type, handle, rec-number, 
		      sql-state, message, message-length);
      values(return-code, as(<byte-string>, sql-state),
	     native-error, as(<byte-string>,message));
    end with-stack-structure;
  end with-stack-structure;
end method;



define method nice-SQLGetDiagField(handle-type :: <object>,
				   handle :: <object>,
				   rec-number :: <integer>,
				   diag-identifier :: <object>)
 => (return-code :: <object>, diag-info :: <object>)
  let diag-info-max-size = 512;
  let diag-info-type = select (diag-identifier)
			 $sql-diag-cursor-row-count,
			 $sql-diag-dynamic-function-code,
			 $sql-diag-number,
			 $sql-diag-row-count,
			 $sql-diag-column-number,
			 $sql-diag-native,
			 $sql-diag-row-number,
			 $sql-diag-returncode
			   => #"integer";
			  
			 $sql-diag-dynamic-function,
			 $sql-diag-class-origin,
			 $sql-diag-connection-name,
			 $sql-diag-message-text,
			 $sql-diag-server-name,
			 $sql-diag-sqlstate,
			 $sql-diag-subclass-origin
			   => #"string";
			  
			 otherwise => 
			   error("Unknown diagnostic identifier\n");
		      end select;
  if (diag-info-type = #"integer")
    with-stack-structure (diag-info-ptr :: <LPSQLUINTEGER>)
      let (return-code, diag-info-actual-length) =
        SQLGetDiagField(handle-type, handle, rec-number, diag-identifier,
	  	        diag-info-ptr, diag-info-max-size);
      values (return-code, pointer-value(diag-info-ptr));
    end with-stack-structure;
  else
    with-stack-structure (diag-info-ptr :: <C-string>,
			  element-count: diag-info-max-size)
      let (return-code, diag-info-actual-length) =
        SQLGetDiagField(handle-type, handle, rec-number, diag-identifier,
	  	        diag-info-ptr, diag-info-max-size);
      values (return-code, as(<byte-string>, diag-info-ptr));
    end with-stack-structure;
  end if;
end method;    



define constant <sqlgetinfo-string-info-type>
  = one-of($sql-accessible-procedures,
           $sql-accessible-tables,
           $sql-catalog-name,
           $sql-catalog-name-separator,
           $sql-catalog-term,
           $sql-collation-seq,
           $sql-column-alias,
           $sql-data-source-name,
           $sql-data-source-read-only,
           $sql-database-name,
           $sql-dbms-name,
           $sql-dbms-ver,
           $sql-describe-parameter,
           $sql-dm-ver,
           $sql-driver-name,
           $sql-driver-odbc-ver,
           $sql-driver-ver,
           $sql-expressions-in-orderby,
           $sql-identifier-quote-char,
           $sql-integrity,
           $sql-keywords,
           $sql-like-escape-clause,
           $sql-max-row-size-includes-long,
           $sql-mult-result-sets,
           $sql-multiple-active-txn,
           $sql-need-long-data-len,
           $sql-odbc-ver,
           $sql-order-by-columns-in-select,
           $sql-procedure-term,
           $sql-procedures,
           $sql-row-updates,
           $sql-schema-term,
           $sql-search-pattern-escape,
           $sql-server-name,
           $sql-special-characters,
           $sql-table-term,
           $sql-user-name,
           $sql-xopen-cli-year);

define constant <sqlgetinfo-smallint-info-type>
  = one-of($sql-active-environments,
           $sql-catalog-location,
           $sql-concat-null-behavior,
           $sql-correlation-name,
           $sql-cursor-commit-behavior,
           $sql-cursor-rollback-behavior,
           $sql-file-usage,
           $sql-group-by,
           $sql-identifier-case,
           $sql-max-catalog-name-len,
           $sql-max-column-name-len,
           $sql-max-columns-in-group-by,
           $sql-max-columns-in-index,
           $sql-max-columns-in-order-by,
           $sql-max-columns-in-select,
           $sql-max-columns-in-table,
           $sql-max-concurrent-activities,
           $sql-max-cursor-name-len,
           $sql-max-driver-connections,
           $sql-max-identifier-len,
           $sql-max-procedure-name-len,
           $sql-max-schema-name-len,
           $sql-max-table-name-len,
           $sql-max-tables-in-select,
           $sql-max-user-name-len, 
           $sql-non-nullable-columns,
           $sql-null-collation,
           $sql-quoted-identifier-case,
           $sql-txn-capable);
                    
define constant <sqlgetinfo-uinteger-info-type>
  = one-of(//$sql-aggregate-functions,
           $sql-alter-domain,
           $sql-alter-table,
           $sql-async-mode,
           $sql-batch-row-count,
           $sql-batch-support,
           $sql-bookmark-persistence,
           $sql-catalog-usage,
           $sql-convert-bigint,
           $sql-convert-binary,
           $sql-convert-bit,
           $sql-convert-char,
           $sql-convert-date,
           $sql-convert-decimal,
           $sql-convert-double,
           $sql-convert-float,
           $sql-convert-integer,
           $sql-convert-interval-year-month,
           $sql-convert-interval-day-time,
           $sql-convert-longvarbinary,
           $sql-convert-longvarchar,
           $sql-convert-numeric,
           $sql-convert-real,
           $sql-convert-smallint,
           $sql-convert-time,
           $sql-convert-timestamp,
           $sql-convert-tinyint,
           $sql-convert-varbinary,
           $sql-convert-varchar,
           $sql-convert-functions,
           $sql-create-assertion,
           $sql-create-character-set,
           $sql-create-collation,
           $sql-create-domain,
           $sql-create-schema,
           $sql-create-table,
           $sql-create-translation,
           $sql-create-view,
           $sql-cursor-sensitivity,
           $sql-datetime-literals,
           $sql-ddl-index,
           $sql-default-txn-isolation,
           $sql-driver-hdbc,
           $sql-driver-henv,
           $sql-driver-hdesc,
           $sql-driver-hlib,
           $sql-driver-hstmt,
           $sql-drop-assertion,
           $sql-drop-character-set,
           $sql-drop-collation,
           $sql-drop-domain,
           $sql-drop-schema,
           $sql-drop-table,
           $sql-drop-translation,
           $sql-drop-view,
           $sql-dynamic-cursor-attributes1,
           $sql-dynamic-cursor-attributes2,
           $sql-forward-only-cursor-attributes1,
           $sql-forward-only-cursor-attributes2,
           $sql-getdata-extensions,
           $sql-index-keywords,
           $sql-info-schema-views,
           $sql-insert-statement,
           $sql-keyset-cursor-attributes1,
           $sql-keyset-cursor-attributes2,
           $sql-max-async-concurrent-statements,
           $sql-max-binary-literal-len,
           $sql-max-char-literal-len,
           $sql-max-index-size,
           $sql-max-row-size,
           $sql-max-statement-len,
           $sql-numeric-functions,
           $sql-odbc-interface-conformance,
           $sql-oj-capabilities,
           $sql-param-array-row-counts,
           $sql-param-array-selects,
           $sql-schema-usage,
           $sql-scroll-options,
           //$sql-sql-conformance,
           $sql-sql92-datetime-functions,
           $sql-sql92-foreign-key-delete-rule,
           $sql-sql92-foreign-key-update-rule,
           $sql-sql92-grant,
           $sql-sql92-numeric-value-functions,
           $sql-sql92-predicates,
           $sql-sql92-relational-join-operators,
           $sql-sql92-revoke,
           $sql-sql92-row-value-constructor,
           $sql-sql92-string-functions,
           $sql-sql92-value-expressions,
           $sql-standard-cli-conformance,
           $sql-static-cursor-attributes1,
           $sql-static-cursor-attributes2,
           $sql-string-functions,
           $sql-subqueries,
           $sql-system-functions,
           $sql-timedate-add-intervals,
           $sql-timedate-diff-intervals,
           $sql-timedate-functions,
           $sql-txn-isolation-option,
           $sql-union);


define generic nice-SQLGetInfo(connection-handle :: <object>,
                               info-type :: <object>)
 => (return-code :: <object>, info-value :: <object>);


define method nice-SQLGetInfo(connection-handle :: <object>,
                              info-type :: <sqlgetinfo-string-info-type>)
 => (return-code :: <object>, info-value :: <string>)
  let $max-string-size = 512; //+++ This isn't good.
  with-stack-structure(info-value-ptr :: <C-string>, 
                       element-count: $max-string-size)
    let (return-code, string-length) = SQLGetInfo(connection-handle,
                                                  info-type,
                                                  info-value-ptr,
                                                  $max-string-size);
    values(return-code, as(<byte-string>, info-value-ptr));
  end with-stack-structure;
end method;


define method nice-SQLGetInfo(connection-handle :: <object>,
                              info-type :: <sqlgetinfo-smallint-info-type>)
 => (return-code :: <object>, info-value :: <integer>)
  with-stack-structure(info-value-ptr :: <lpsqlusmallint>)
    let (return-code, string-length) = SQLGetInfo(connection-handle,
                                                  info-type,
                                                  info-value-ptr,
                                                  size-of(<sqlusmallint>));
    values(return-code, pointer-value(info-value-ptr));
  end with-stack-structure;
end method;


define method nice-SQLGetInfo(connection-handle :: <object>,
                              info-type :: <sqlgetinfo-uinteger-info-type>)
 => (return-code :: <object>, info-value :: <integer>)
  with-stack-structure(info-value-ptr :: <lpsqluinteger>)
    let (return-code, string-length) = SQLGetInfo(connection-handle,
                                                  info-type,
                                                  info-value-ptr,
                                                  size-of(<sqluinteger>));
    values(return-code, pointer-value(info-value-ptr));
  end with-stack-structure;
end method;  

define generic nice-SQLSetStmtAttr(statement-handle :: <object>,
                                   attribute :: <integer>,
                                   attribute-value :: type-union(<integer>, <string>))
 => (return-code :: <object>);

define method nice-SQLSetStmtAttr(statement-handle :: <object>,
                                  attribute :: <integer>,
                                  attribute-value :: <integer>)
 => (return-code :: <object>)
  let attribute-value-pointer = make(<lpsqluinteger>, address: attribute-value);
  let return-code = SQLSetStmtAttr(statement-handle, attribute, 
                                   attribute-value-pointer, $sql-is-integer);
  return-code;
end method;

define method nice-SQlSetStmtAttr(statement-handle :: <object>,
                                  attribute :: <integer>,
                                  attribute-value :: <string>)
 => (return-code :: <object>)
  SQLSetStmtAttr(statement-handle, attribute, attribute-value, attribute-value.size);
end method;


