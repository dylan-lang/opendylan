Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// $HopeName: D-databases-sql-odbc!record.dylan(trunk.5) $


define concrete sealed class <odbc-record> (<record>)
  constant slot statement :: <odbc-sql-statement>,
    required-init-keyword: statement:;

  slot bindings :: <vector> = make(<vector>);

  required keyword rowset-size, type: <integer>;

  slot rowset-index :: <integer> = -1;
end class;


define concrete sealed class <odbc-coercion-record> (<odbc-record>, 
						     <coercion-record>)
end class; 

define method initialize(record :: <odbc-record>, #key rowset-size :: <integer>)
  => ()
  next-method();

  // this is so first rowset will get fetched at start
  record.rowset-index := rowset-size;
  format-out("initializing new rowset\n");

  let statement = record.statement;
  let environment-handle = statement.connection.dbms.%environment-handle;
  let connection-handle = statement.connection.%connection-handle;
  let statement-handle = statement.%statement-handle;

  local method odbc-class-id(sql-type, column-number) => (id :: <integer>)
	  if (column-number > statement.datatype-hints.size)
	    sql-type
	  else
	    let hint = statement.datatype-hints[column-number - 1];
	    let $not-found = make(<pair>);  
	    let class-id = element($odbc-class-id, hint, default: $not-found);
	    if (class-id == $not-found)
	      signal(make(<invalid-datatype-hint>, datatype-hint: hint));
	      sql-type
	    else
	      class-id
	    end if;
	  end if;
	end method;
  local method do-binding(column-number :: <integer>) 
          let (return-code, name, data-type, precision, scale, nullable)
	    = nice-SQLDescribeCol(statement-handle, column-number);

	  assert-odbc-goodness(return-code, environment-handle,
		 	       connection-handle, statement-handle);

	  format-out("making binding with rowset size %=\n",rowset-size);
	  let binding = make(<binding>,
                             rowset-size: rowset-size,
			     name: name,
			     sql-data-type: odbc-class-id(data-type, column-number),
			     precision: precision,
			     scale: scale,
			     nullable: if (nullable = 0) #f else #t end if);

          if (null-pointer?(binding.storage) = #f & 
              null-pointer?(binding.data-length) = #f)
	    let return-code = nice-SQLBindCol(statement-handle,
					      column-number,
					      binding.c-data-type,
					      binding.storage,
					      binding.storage-size,
					      binding.data-length);
	    assert-odbc-goodness(return-code, environment-handle,
			         connection-handle, statement-handle);
          end if;
	    
	  binding;
	end method;

  let (return-code, column-count) = SQLNumResultCols(statement-handle);
  assert-odbc-goodness(return-code, environment-handle, connection-handle,
		       statement-handle);

  for (column-number :: <integer> from 1 to column-count)
    let binding = do-binding(column-number);
    record.bindings := add!(record.bindings, binding);
  end for;
end method;


define method acquire-binding(record :: <odbc-record>, key :: <integer>)
 => (field :: <object>, null? :: <boolean>)
  let binding = element(record.bindings, key);
  values(binding, null-odbc-field?(binding.data-length));
end method;

define method next-record-state(record :: <odbc-record>, state :: <integer>)
 => (next-state :: <integer>)
  record.rowset-index := record.rowset-index; //+++++++++++ huh?
  state + 1;
end method;

define method no-more-record-elements?(record :: <odbc-record>, 
                                       state :: <integer>, limit :: <integer>)
 => (finished :: <boolean>)
  state = limit;
end method;

define method record-element-key-by-state(record :: <odbc-record>,
                                          state :: <integer>)
 => (key :: <integer>)
  state;
end method;

define method record-element-by-state(record :: <odbc-record>,
                                      state :: <integer>)
 => (record-element :: <object>)
  let (binding, null?) = acquire-binding(record, state);
  if (null? = #t)
    acquire-null-value(record.indicator-policy, state);
  else 
    pointer-value-address(binding.storage, index: record.rowset-index);
  end if;
end method;

define method rowset-lookup(storage :: <object> , size :: <integer>, i :: <integer>)
 => (<object>)
  pointer-value-address(storage, index: i);
end method;

define method rowset-lookup(storage :: <c-char*>, size :: <integer>, i :: <integer> )
 => (<object>)
//  format-out("contents:");
//  for (i from 0 below 7000) format-out("%=", 
//				     as (<character>, pointer-value(storage, index: i))); end for;
//  
//format-out("\n");
  format-out("size is %=\n",size);
  pointer-value-address(storage, index: i * size);
end method;

define method record-element-by-state(record :: <odbc-coercion-record>,
                                      state :: <integer>)
 => (record-element :: <object>)
  let (binding, null?) = acquire-binding(record, state);
  if (null? = #t)
    acquire-null-value(record.indicator-policy, state);
  else 
    convert-value(record.record-coercion-policy, 
                  rowset-lookup(binding.storage, binding.storage-size, record.rowset-index),
		  state);
  end if;
end method;

define method record-element-by-state-setter(new-element :: <object>,
                                             record :: <odbc-record>,
                                             state :: <integer>)
 => (new-element :: <object>)
  //not supported!
end method;

define method copy-record-state(record :: <odbc-record>, 
                                state :: <integer>)
 => (new-state :: <integer>)
  state;
end method;
                                             

define method forward-iteration-protocol(record :: <odbc-record>)
  => (initial-state :: <integer>,
      limit :: <integer>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>)
  let initial-state :: <integer> = 0;
  let limit :: <integer> = record.bindings.size;
  values(initial-state,
	 limit,
	 next-record-state,
	 no-more-record-elements?,
	 record-element-key-by-state,
	 record-element-by-state,
	 record-element-by-state-setter,
	 copy-record-state);
end method;



define method element(record :: <odbc-record>, 
		      key :: <integer>, #key default = unsupplied())
  => (record-element :: <object>)
  block ()
    record-element-by-state(record, key);
  exception (condition :: <data-not-available>)  //++ correct condition?
    if (unsupplied?(default))
      error("ELEMENT outside of range: %=", format-arguments: list(key));
    else
      default
    end if;
  end block;
end method;
