Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: !conditions.dylan(D-kan.3) $

// When set to #t, a description of the condition will be printed to (??)
// at the time  condition is detected.
define variable *odbc-print-condition* :: <boolean> = #f;

// When set to #t, the return code $sql-success-with-info will be treated
// as an error and the appropriate condition will be signaled.
define variable *odbc-report-success-with-info* :: <boolean> = #f;

// When set to #t, all <data-not-available> conditions will be reported.
define variable *odbc-report-<data-not-available>* :: <boolean> = #f;


define sealed generic report-condition
    (info :: <object>) => ();

define method report-condition (object :: <object>) => ();
  format-out("Report condition - object: %=\n", object);
end method;


define open abstract class <odbc-error> (<database-error>)
end class <odbc-error>;

define sealed class <odbc-low-level-error> (<odbc-error>)
  constant slot low-level-error-sqlstate :: <string>,
    required-init-keyword: sqlstate:;
end class;


define method assert-odbc-goodness
    (return-code :: <object>, 
     environment-handle :: <object>, //<environment-handle>, 
     connection-handle :: <object>, //<connection-handle>, 
     statement-handle :: <object>)  //<statement-handle>)
 => ()
  local method assert-odbc-diagnostic-goodness(return-code) => ()
          if (return-code ~= $sql-success)
            if (return-code = $sql-invalid-handle)
              error("Assert-odbc-goodness: invalid ODBC handle.");
            else
              error("Assert-odbc-goodness: error in acquiring sqlstate: %=", return-code);
            end if;
          end if;
        end method;
  local method look-deeper()
	  let (handle-type, handle) = 
	    if (statement-handle ~= $null-statement-handle)
	      values($sql-handle-stmt, statement-handle);
	    elseif (connection-handle ~= $null-connection-handle)
	      values($sql-handle-dbc, connection-handle);
	    else
	      values($sql-handle-env, environment-handle)
	    end if;


	  let (return-code, sqlstate) = 
	    nice-SQLGetDiagField(handle-type,
				 handle,
				 1, 
				 $sql-diag-sqlstate);
	  assert-odbc-diagnostic-goodness(return-code);

	  let condition-type = find-diagnostic($diagnostic-table,
                                               $odbc-diagnostics-key,
                                               sqlstate);

	  if (unfound?(condition-type))
	    condition-type := make(<odbc-low-level-error>,
				   sqlstate: sqlstate);
          else
	    make(condition-type,
		 handle-type: handle-type,
		 handle: handle);
	  end if;
	end method;

  if ((return-code ~= $sql-success & return-code ~= $sql-success-with-info) |
	(return-code = $sql-success-with-info & 
         *odbc-report-success-with-info* = #t))

    if (return-code = $sql-no-data-found)
      let condition = make(<data-not-available>);
      if (*odbc-print-condition* & *odbc-report-<data-not-available>*)
        report-condition(condition);
      end if;

      signal(condition);
    else
      let condition = look-deeper();
      if (*odbc-print-condition*)
        report-condition(condition);
      end if;
                      
      signal(condition);
    end if;
  end if;
end method;

