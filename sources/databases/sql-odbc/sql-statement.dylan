Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define concrete class <odbc-sql-statement> (<sql-statement>)
  slot connection :: <odbc-connection> = default-connection(),
    init-keyword: connection:;

  slot bindings :: <vector> = make(<vector>);

  slot %statement-handle :: <statement-handle> = $null-statement-handle;

  slot %prepared :: <boolean> = #f;
end class;


define method make-dbms-specific
    (type == <sql-statement>, dbms :: <odbc-dbms>, #rest more-args)
  => (sql-statement :: <odbc-sql-statement>)
  apply(make, <odbc-sql-statement>, more-args);
end method;


define method initialize(stmt :: <odbc-sql-statement>, #key)
 => ()
  next-method();

  let connection-handle = stmt.connection.%connection-handle;
  let environment-handle = stmt.connection.dbms.%environment-handle;
  let (return-code, statement-handle) =
    nice-SQLAllocHandle($sql-handle-stmt, connection-handle);
  assert-odbc-goodness(return-code, environment-handle, connection-handle,
		       statement-handle);

  stmt.%statement-handle := statement-handle;

  // Setup for finalization.
  finalize-when-unreachable(stmt);
  stmt.connection.%allocated-sql-statements[stmt] := #t;
end method;

define method statement-column-names
    (statement :: <odbc-sql-statement>)
 => (column-names :: <simple-object-vector>)
  let (return-code, column-count) = SQLNumResultCols(statement.%statement-handle);
  let column-names = make(<simple-object-vector>, size: column-count);
  let connection-handle = statement.connection.%connection-handle;
  let environment-handle = statement.connection.dbms.%environment-handle;
  let statement-handle = statement.%statement-handle;
  for (column :: <integer> from 1 to column-count)
    let (return-code, name, data-type, precision, scale, nullable)
      = nice-SQLDescribeCol(statement-handle, column);

    assert-odbc-goodness(return-code, environment-handle,
			 connection-handle, statement-handle);
    column-names[column - 1] := name
  end;
  column-names
end method statement-column-names;

define constant $sql-statement-finalization-lock :: <lock> = make(<lock>);
// This seems like a heavy handed thing to do: halting all finalization on
// sql-statements on all other threads just to finalize a single instance.
// The alternative, each instance of <odbc-sql-statement> could have it's
// own lock object but I don't like the idea of creating many locks. So,
// I don't expect this synchronization issue to occur much so doing the
// heavy-handed thing shouldn't be a big deal (I think).
define method finalize
    (statement :: <odbc-sql-statement>)
 => ()
  // Bindings should finalize themselves. Finalization unordering
  // doesn't matter with bindings since ODBC doesn't maintain a reference
  // to them.

  with-lock($sql-statement-finalization-lock)
    let handle = statement.%statement-handle;
    if (handle ~= $null-statement-handle)
      let connection = statement.connection;
      close-statement(statement);
      let return-code = nice-SQLFreeHandle($sql-handle-stmt, handle);
      assert-odbc-goodness(return-code,
			   connection.dbms.%environment-handle,
			   connection.%connection-handle,
			   handle);
      statement.%statement-handle := $null-statement-handle;
    end
  end with-lock;

  notify-of-finalization(statement);
  next-method();
end method;


define method bind-parameters(statement :: <odbc-sql-statement>,
			      parameters :: <sequence>)
 => ()
  local method null?(parameter-number) => (result :: <boolean>)
	  if (statement.input-indicator == $no-indicator)
	    #f
	  else
	    let indicator =
	      if (instance?(statement.input-indicator, <sequence>))
		   statement.input-indicator[parameter-number]
	      else 
		statement.input-indicator
	      end if;

	    parameters[parameter-number] == indicator
	  end if;
	end method;
  local method do-binding(parameter-number) => (binding :: <binding>)
	  let initial-value = if (null?(parameter-number))
				$null-value 
			      else
				parameters[parameter-number]
			      end if;
	  let (sql-type, the-precision, the-scale) 
	    = sql-binding-info(initial-value);

	  let binding = make(<binding>, 
			     sql-data-type: sql-type,
			     precision: the-precision,
			     scale: the-scale,
			     initial-value: initial-value);
          block (exit)
  	    let (return-code, parameter-length) = 
	      SQLBindParameter(statement.%statement-handle,
			       parameter-number + 1,
			       $sql-param-input,
			       binding.c-data-type,
			       binding.sql-data-type,
			       binding.precision,
			       binding.scale,
			       binding.storage,
			       binding.storage-size,
			       binding.data-length);

	    assert-odbc-goodness(return-code,
			         statement.connection.dbms.%environment-handle,
			         statement.connection.%connection-handle,
			         statement.%statement-handle);
          exception (condition :: <odbc-optional-feature-not-implemented>)
            let c-name = element($c-datatype-display-names,
                                 binding.c-data-type, 
                                 default: 
                                   integer-to-string(binding.c-data-type));
            let sql-name = element($sql-datatype-display-names,
                                   binding.sql-data-type,
                                   default:
                                     integer-to-string(binding.sql-data-type));
            let msg = 
              format-to-string("Unable to bind parameter %d "
                               "from %s to %s with "
                               "precision %= and scale %=",
                               parameter-number,
                               c-name,
                               sql-name,
                               binding.precision,
                               binding.scale);
                                      
            push-last(condition.possible-explanation, msg);
            signal(condition);
          end block;

	  binding;
	end method;

  for (parameter-number :: <integer> from 0 below parameters.size)
    let binding :: <binding> = do-binding(parameter-number);
    statement.bindings := add!(statement.bindings, binding);
  end for;
end method;

define method execute
    (statement :: <odbc-sql-statement>,
     #key result-set-policy :: false-or(<result-set-policy>) = $default-result-set-policy,
          parameters :: <sequence> = #(),
          liaison :: false-or(<function>))
 => (result-set :: false-or(<result-set>))
  if (statement.%prepared)
    close-statement(statement)
  end;

  if (parameters.size > 0)
    bind-parameters(statement, parameters);
  end if;

  let return-code = SQLSetStmtOption(statement.%statement-handle,
                                     $SQL-ROWSET-SIZE, 
                                     statement.connection.dbms.%dbms-rowset-size);
// rowsets aren't working but we need a rowset-size > 1 in order to get SQL Server
// to execute multiple statements per connection (ie, server-side cursors).
//                                     result-set-policy.rowset-size);
  assert-odbc-goodness(return-code,
		       statement.connection.dbms.%environment-handle,
		       statement.connection.%connection-handle,
		       statement.%statement-handle);

  let return-code = nice-SQLExecDirect(statement.%statement-handle,
				       statement.text);
  statement.%prepared := #t;
  assert-odbc-goodness(return-code,
		       statement.connection.dbms.%environment-handle,
		       statement.connection.%connection-handle,
		       statement.%statement-handle);


  if (result-set-policy)
    // The column count returned by SQLNumResultCols is ODBC's way
    // of checking if the executed sql statement returns a result-set.

    let (return-code, column-count) = SQLNumResultCols(statement.%statement-handle);
    assert-odbc-goodness(return-code,
			 statement.connection.dbms.%environment-handle,
			 statement.connection.%connection-handle,
			 statement.%statement-handle);

    if (column-count > 0)
      make(<odbc-result-set>,
	   result-set-policy: result-set-policy,
	   statement: statement,
	   liaison: liaison);
    else
      make(<empty-result-set>);
    end if;
  else
    close-statement(statement)
  end
end method;

define method close-statement
    (statement :: <odbc-sql-statement>) => ()
  if (statement.%prepared)
    let handle = statement.%statement-handle;
    if (handle ~= $null-statement-handle)
      let connection = statement.connection;
      let return-code = nice-SQLFreeStmt(handle, $sql-close);
      assert-odbc-goodness(return-code,
			   connection.dbms.%environment-handle,
			   connection.%connection-handle,
			   handle)
    end;
    statement.%prepared := #f
  end;
end method close-statement;
