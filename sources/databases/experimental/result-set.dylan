Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <result-set-state> (<object>)
  slot result-set-key :: <integer> = 0,
    init-keyword: result-set-key:;

  slot done? :: <boolean> = #f,
    init-keyword: done?:;
end class;

define function advance-state(current-state :: <result-set-state>)
 => (new-state :: <result-set-state>)
  current-state.result-set-key := current-state.result-set-key + 1;
  current-state;
end function;



define abstract class <odbc-result-set> (<result-set>)
  slot %record :: false-or(<odbc-record>) = #f,
    init-keyword: record:;

  slot %sql-statement :: false-or(<odbc-sql-statement>) = #f,
    init-keyword: sql-statement:;

  constant slot %connection :: false-or(<odbc-connection>) = #f,
    init-keyword: connection:;

  constant slot %rowset-size :: false-or(<integer>) = #f, // was 1
    init-keyword: rowset-size:;

  constant slot generator :: false-or(<function>) = #f,
    init-keyword: generator:;

  slot data-generated? :: <boolean> = #f;

  slot %current-record-number :: <integer> = -1;  // Starts at 0
  slot %result-set-at-end? :: <boolean> = #f;
end class;


define generic next-record(result-set :: <odbc-result-set>) => ();

define method next-record(result-set :: <odbc-result-set>)
 => ()
  // lots of icky duplicated code here to be cleaned up
  if ((result-set.%record.rowset-index + 1) < result-set.%rowset-size)
    result-set.%record.rowset-index := result-set.%record.rowset-index + 1;
    result-set.%current-record-number := result-set.%current-record-number + 1;
    format-out("next-record: rowset-index: %=, -size: %=, current-record-number: %=\n",
	       result-set.%record.rowset-index, result-set.%rowset-size,
	       result-set.%current-record-number);
  else
    format-out("*** fetching new rowset\n");
    result-set.%record.rowset-index := 0;
    block ()
      let stmt = result-set.%sql-statement;
      let return-code = nice-SQLFetch(stmt.%statement-handle);
      assert-odbc-goodness(return-code,
		           stmt.connection.dbms.%environment-handle,
		           stmt.connection.%connection-handle,
		           stmt.%statement-handle);

      result-set.%current-record-number := result-set.%current-record-number + 1;
      format-out("next-record: rowset-index: %=, -size: %=, current-record-number: %=\n",
		 result-set.%record.rowset-index, result-set.%rowset-size,
		 result-set.%current-record-number);
//      for (ndx from 0 below result-set.%rowset-size) // index 0-n
//	format-out("    Value: %=\n", pointer-value(result-set.%record.bindings[0].storage, index: ndx));
//      end for;
    exception (condition :: <data-not-available>) 
      result-set.%result-set-at-end? := #t;
      signal(condition);
    end block;
  end if;
end method;

define method no-more-elements?(result-set :: <odbc-result-set>,
                                state :: <result-set-state>,
                                limit :: <object>)
 => (finished :: <boolean>)
   result-set.%result-set-at-end?
end method;


define method make(type == <odbc-result-set>,
		   #key result-set-policy :: <result-set-policy>,
		   statement :: false-or(<odbc-sql-statement>),
		   liaison :: false-or(<function>),
                   generator :: false-or(<function>),
                   connection: the-connection :: false-or(<odbc-connection>))
  => (result-set :: <result-set>)
  let result-set-class = if (result-set-policy.scrollable? = #t)
			   <odbc-scrollable-result-set>
			 else
			   <odbc-forward-only-result-set>
			 end if;

  let the-liaison = if (liaison ~= #f)
                      liaison
                    else
                      if (statement ~= #f & statement.coercion-policy ~= $no-coercion)
                        copy-sequence
                      else
                        identity
                      end if
                    end if;

 make(result-set-class,	sql-statement: statement, 
      liaison: the-liaison, generator: generator, 
      rowset-size: result-set-policy.rowset-size,
      connection: if (statement ~= #f) statement.connection else the-connection end if)
end method;

define class <odbc-forward-only-result-set> (<odbc-result-set>, 
                                             <forward-only-result-set>)
end class;

define class <odbc-scrollable-result-set> (<odbc-result-set>, 
                                           <scrollable-result-set>)
  constant slot %record-cache :: <deque> = make(<deque>);
end class;

define function generate-data(result-set :: <odbc-result-set>)
 => ()
  if (result-set.%sql-statement = #f)
    result-set.%sql-statement := make(<odbc-sql-statement>, 
                                      connection: result-set.%connection,
                                      text: "");
    let return-code = SQLSetStmtOption(result-set.%sql-statement.%statement-handle,
                                       $SQL-ROWSET-SIZE, result-set.%rowset-size);

    assert-odbc-goodness(return-code,
		         result-set.%sql-statement.connection.dbms.%environment-handle,
		         result-set.%sql-statement.connection.%connection-handle,
		         result-set.%sql-statement.%statement-handle);
   end if;

  if (result-set.generator ~= #f & result-set.data-generated? = #f)
    result-set.generator(result-set);
    result-set.data-generated? := #t;
  end if;
  let stmt = result-set.%sql-statement;
  result-set.%record := if (stmt.coercion-policy ~= $no-coercion)
	                  make(<odbc-coercion-record>, 
                               rowset-size: result-set.%rowset-size,
		               statement: stmt,
		               record-coercion-policy: stmt.coercion-policy,
		               indicator-policy: stmt.output-indicator);
                        else
	                  make(<odbc-record>, 
                               rowset-size: result-set.%rowset-size,
		               statement: stmt,
		               indicator-policy: stmt.output-indicator)
                        end if;
end function;

define sealed generic iteration-completed?(result-set :: <odbc-result-set>,
                                           state :: <result-set-state>)
 => (completed? :: <boolean>);

define method iteration-completed?(result-set :: <odbc-result-set>,
                                   state :: <result-set-state>)
 => (completed? :: <boolean>)
  result-set.%result-set-at-end?
end method;

define method iteration-completed?(result-set :: <odbc-scrollable-result-set>,
                                   state :: <result-set-state>)
 => (completed? :: <boolean>)
  next-method() & result-set.%record-cache.size <= state.result-set-key;
end method;

define method next-record(result-set :: <odbc-scrollable-result-set>)
 => ()
  format-out("next-record on scrollable result set:\n");
  debug-assert(result-set.%result-set-at-end? = #f,
               "next-record called on a result-set already at end.");
  next-method();

  let coerced-record = result-set.liaison(result-set.%record);
  push-last(result-set.%record-cache, coerced-record);
end method;

define method result-set-element(result-set :: <odbc-result-set>,
				 key :: <object>)
 => (result-set-element :: <object>)
  while (key > result-set.%current-record-number)
    next-record(result-set);
  end while;

  let result-set-element = if (key = result-set.%current-record-number)
                             result-set.liaison(result-set.%record);
                           else  // key < result-set.%current-record-number
                             error(make(<data-not-available>));
                           end if;
  result-set-element;
end method;

define method result-set-element(result-set :: <odbc-scrollable-result-set>,
				 key :: <integer>)
 => (result-set-element :: <object>)
  let cache-size = result-set.%record-cache.size;
  while (key >= cache-size)
    let coerced-record = next-method(result-set, cache-size);
    cache-size := cache-size + 1;
  end while;

  let result-set-element = element(result-set.%record-cache, key);
  result-set-element;
end method;

define method no-more-elements?(result-set :: <odbc-scrollable-result-set>, 
				state :: <result-set-state>, 
				limit :: <object>)
 => (finished :: <boolean>)
  iteration-completed?(result-set, state);
end method;

define method initial-result-set-state(result-set :: <odbc-result-set>)
 => (state :: <result-set-state>)
  let initial-state = make(<result-set-state>);
  if (result-set.%result-set-at-end? = #f)
    block ()
      // If next-record executes without throwing a condition, then
      // iteration is not complete since there is at least one record
      // that hasn't been visited.
      next-record(result-set);
    exception (condition :: <data-not-available>)
      // handled
    end block;
  end if;

  initial-state.done? := iteration-completed?(result-set, initial-state);
  initial-state;
end method;

define function result-set-limit(result-set :: <odbc-result-set>)
 => (limit :: <object>)
  make(<result-set-state>);
end function;

define method next-state(result-set :: <odbc-result-set>, state :: <result-set-state>)
 => (next-state :: <result-set-state>)
  if (result-set.%result-set-at-end? = #f)
    block ()
      next-record(result-set);
    exception (condition :: <data-not-available>)
      // Condition handled.
    end block;
  end if;

  state.done? := iteration-completed?(result-set, state);
  if (state.done? = #f)
    advance-state(state);
  end if;

  state;
end method;

define function element-key-by-state(result-set :: <odbc-result-set>, 
				     state :: <result-set-state>)
 => (key :: <object>)
  state.result-set-key;
end function;


define method element-by-state(result-set :: <odbc-result-set>, 
			       state :: <result-set-state>)
 => (result-set-element :: <object>)
  result-set-element(result-set, state.result-set-key);
end method;

define function element-by-state-setter()
end function;


define method copy-result-set-state(result-set :: <odbc-result-set>, 
			            state :: <result-set-state>)
 => (new-state :: <result-set-state>)
  make(<result-set-state>,
       result-set-key: state.result-set-key,
       done?: state.done?);
end method;


define method forward-iteration-protocol(result-set :: <odbc-result-set>)
  => (initial-state :: <result-set-state>,
      limit :: <object>,
      next-state :: <function>,
      finished-state? :: <function>,
      current-key :: <function>,
      current-element :: <function>,
      current-element-setter :: <function>,
      copy-state :: <function>)
  generate-data(result-set);
  let initial-state = initial-result-set-state(result-set);
  let result-set-limit = result-set-limit(result-set);
  values(initial-state,
	 result-set-limit,
	 next-state,
	 no-more-elements?,
	 element-key-by-state,
	 element-by-state,
	 element-by-state-setter,
	 copy-result-set-state);
end method;



define method element(result-set :: <odbc-result-set>, 
		      key :: <integer>, #key default = unsupplied())
  => (result-set-element :: <object>)
  generate-data(result-set);
  let result-set-element = block ()
                             result-set-element(result-set, key);
                           exception (condition :: <data-not-available>)
                             if (unsupplied?(default))
                               error("ELEMENT outside of range: %=", 
                                     format-arguments: list(key));
                             else
                               default
                             end if;
                           end block;
  result-set-element;
end method;

define method find-key(result-set :: <odbc-result-set>, fn :: <function>, 
		       #key skip :: <integer> = 0, failure = #f) 
 => (key :: <object>)
  block (exit)
    for (e keyed-by k in result-set,
	 index = 0 then index + 1)
      if (fn(e) & ((skip := skip - 1) < 0))
	exit(index)
      end if;
    end for;

    failure
  end block;
end method;

