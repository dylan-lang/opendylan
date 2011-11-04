Module: sql-odbc-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//---------- Errors in error handling classes and methods

define sealed abstract class <odbc-diagnostic-problem> (<object>)
  constant slot problem-diagnostic :: <object>,
    required-init-keyword: problem-diagnostic:;

  constant slot diagnostic-return-code :: <object>,
    required-init-keyword: diagnostic-return-code:;
end class;


define sealed class <odbc-diagnostic-error> (<odbc-diagnostic-problem>,
                                             <error>)
end class;

define function return-code-name(return-code :: <object>) 
 => (name :: <string>)
  select (return-code)
    $sql-success-with-info => "sql-success-with-info";
    $sql-error => "sql-error";
    $sql-invalid-handle => "sql-invalid-handle";
    $sql-no-data => "sql-no-data";

    otherwise => "unknown";
  end select;
end function;

define method condition-to-string(diag-error :: <odbc-diagnostic-error>)
 => (string :: false-or(<string>))
  format-to-string("ODBC Diagnostic Error - Return-code: (%=) %=\n",
                   diag-error.diagnostic-return-code,
                   return-code-name(diag-error.diagnostic-return-code));
end method;


define sealed class <odbc-diagnostic-warning> (<odbc-diagnostic-problem>,
                                               <warning>)
end class;

define method condition-to-string(diag-warning :: <odbc-diagnostic-warning>)
 => (string :: false-or(<string>))
  format-to-string("ODBC Diagnostic Warning - \n"
                   "  Return-code: (%=) %=\n",
                   diag-warning.diagnostic-return-code,
                   return-code-name(diag-warning.diagnostic-return-code));
end method;


define sealed concrete class <odbc-unexpected-return-code> (<error>)
  constant slot unexpected-return-code :: <object>, 
    required-init-keyword: unexpected-return-code:;
end class;

define method condition-to-string(cond :: <odbc-unexpected-return-code>)
 => (string :: false-or(<string>))
  format-to-string("ODBC Unexpected Return Code - \n"
                   "  Return-code: (%=) %=\n",
                   cond.unexpected-return-code,
                   return-code-name(cond.unexpected-return-code));
end method;


define sealed generic assert-diagnostic-goodness(diag :: <object>,
                                                 return-code :: <object>)
  => ();


define method assert-diagnostic-goodness(diagnostic :: <object>,
	                                 return-code :: <object>)
 => ()
  if (return-code ~= $sql-success)
    select (return-code)
      $sql-success-with-info 
        => signal(make(<odbc-diagnostic-warning>,
                       problem-diagnostic: diagnostic,
                       diagnostic-return-code: return-code));

      $sql-error, $sql-invalid-handle, $sql-no-data
        => error(make(<odbc-diagnostic-error>,
                      problem-diagnostic: diagnostic,
                      diagnostic-return-code: return-code));

      otherwise
        => error(make(<odbc-unexpected-return-code>,
                      unexpected-return-code: return-code));
    end select;
  end if;
end method;



//----------  Diagnostic  ----------


define sealed concrete class <odbc-diagnostic> (<diagnostic>)
  constant slot handle-type :: <object>,
    init-keyword: handle-type:;

  constant slot handle :: <object>,
    init-keyword: handle:;
end class;


define method command-function(diag :: <odbc-diagnostic>)
 => (command-function :: <string>);
  ""
end method;


define method dynamic-function(diag :: <odbc-diagnostic>)
 => (dynamic-function :: <string>)
  block ()
    let (return-code, dynamic-function) = 
      nice-SQLGetDiagField(diag.handle-type,
	  		   diag.handle,
			   0,
			   $sql-diag-dynamic-function);
    assert-diagnostic-goodness(diag, return-code);

    dynamic-function;
  exception (condition :: <odbc-diagnostic-error>)
    ""
  end block;
end method;


define method conditions-not-recorded?(diag :: <odbc-diagnostic>)
 => (not-recorded-status :: <boolean>)
  #f
end method;


define method row-count(diag :: <odbc-diagnostic>)
 => (row-count :: <integer>);
  block ()
    let (return-code, row-count) = 
      nice-SQLGetDiagField(diag.handle-type,
			   diag.handle,
			   0,
			   $sql-diag-row-count);
    assert-diagnostic-goodness(diag, return-code);
    row-count;
  exception (condition :: <odbc-diagnostic-error>)
    0;
  end block;
end method;


define method diagnostic-count(diag :: <odbc-diagnostic>)
 => (diagnostic-count :: <integer>);
  let (return-code, diagnostic-count) =
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 0,
			 $sql-diag-number);
  assert-diagnostic-goodness(diag, return-code);

  diagnostic-count;
end method;


define method returned-sqlstate(diag :: <odbc-diagnostic>)
 => (returned-sqlstate :: <string>);
  let (return-code, returned-sqlstate) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-sqlstate);
  assert-diagnostic-goodness(diag, return-code);

  returned-sqlstate
end method;


define method class-origin(diag :: <odbc-diagnostic>)
 => (class-origin :: <string>);
  let (return-code, class-origin) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-class-origin);
  assert-diagnostic-goodness(diag, return-code);

  class-origin
end method;


define method subclass-origin(diag :: <odbc-diagnostic>)
 => (subclass-origin :: <string>);
  let (return-code, subclass-origin) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-subclass-origin);
  assert-diagnostic-goodness(diag, return-code);

  subclass-origin
end method;


define method connection-name(diag :: <odbc-diagnostic>)
 => (connection-name :: <string>);
  let (return-code, connection-name) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-connection-name);
  assert-diagnostic-goodness(diag, return-code);

  connection-name
end method;


define method message-text(diag :: <odbc-diagnostic>)
 => (message-text :: <string>);
  let (return-code, message-text) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-message-text);
  assert-diagnostic-goodness(diag, return-code);

  message-text
end method;

define method next-dbms-diagnostic(diag :: <odbc-diagnostic>)
 => (next-condition :: false-or(<diagnostic>))
  let diag-count = diagnostic-count(diag);
  if (diag.condition-number >= diag-count)
    #f
  else
    let (return-code, sqlstate) = 
      nice-SQLGetDiagField(diag.handle-type,
	                   diag.handle,
	                   diag.condition-number + 1, 
			   $sql-diag-sqlstate);
    assert-diagnostic-goodness(diag, return-code);

    let condition-type = find-diagnostic($diagnostic-table,
                                         $odbc-diagnostics-key,
                                         sqlstate);

    if (unfound?(condition-type))
      make(<unknown-sqlstate>, sqlstate: sqlstate);
    else
      make(condition-type,
           condition-number: diag.condition-number + 1,
	   handle-type: diag.handle-type,
           handle: diag.handle);
    end if;
  end if;
end method;

define method diagnostic-to-string
    (diag :: <odbc-diagnostic>)
 => (string :: <string>)
  format-to-string("Diagnostic - \n"
                   "  Conditions not recorded: %=\n"
                   "  Command function: %=\n"
                   "  Dynamic function: %=\n"
                   "  Row count: %=\n"
                   "  Condition/Diagnostic number: %=\n"
                   "  Returned SQLState: %=\n"
                   "  Class origin: %=\n"
                   "  Subclass origin: %=\n"
                   "  Connection name: %=\n"
                   "  Message Text: %=\n"
                   "  Native error code: %=\n",
                   diag.conditions-not-recorded?,
                   diag.command-function,
                   diag.dynamic-function,
                   diag.row-count,
                   diag.condition-number,
                   diag.returned-sqlstate,
                   diag.class-origin,
                   diag.subclass-origin,
                   diag.connection-name,
                   diag.message-text,
                   diag.native-error-code);
end method;


//----------  Extensions to the diagnostic-detail protocol  ----------

define method native-error-code(diag :: <odbc-diagnostic>)
  => (native-error-code :: <integer>);
  let (return-code, native-error-code) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-native);
  assert-diagnostic-goodness(diag, return-code);

  native-error-code
end method;


define method column-number(diag :: <odbc-diagnostic>)
  => (column-number :: <integer>);
  let (return-code, column-number) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-column-number);
  assert-diagnostic-goodness(diag, return-code);

  column-number;
end method;


define method row-number(diag :: <odbc-diagnostic>)
  => (row-number :: <integer>);
  let (return-code, row-number) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-row-number);
  assert-diagnostic-goodness(diag, return-code);

  row-number;
end method;


define method server-name(diag :: <odbc-diagnostic>)
  => (server-name :: <string>);
  let (return-code, server-name) = 
    nice-SQLGetDiagField(diag.handle-type,
			 diag.handle,
			 diag.condition-number, 
			 $sql-diag-server-name);
  assert-diagnostic-goodness(diag,  return-code);

  server-name;
end method;


//----------  Specific Diagnostic Detail Implementation  ----------


define open class <odbc-sql-warning>
    (<odbc-diagnostic>, <sql-warning>)
end class;

define sealed class <odbc-dynamic-sql-error>
    (<odbc-diagnostic>, <dynamic-sql-error>)
end class;


define sealed class <odbc-connection-exception> 
    (<odbc-diagnostic>, <connection-exception>)
  constant slot connection-exception-dbms :: <object>,
    init-keyword: dbms:;

  constant slot connection-exception-database :: <object>,
    init-keyword: database:;

  constant slot connection-exception-user :: <object>,
    init-keyword: user:;
end class;

define sealed class <odbc-ambiguous-cursor-name>
    (<odbc-diagnostic>, <ambiguous-cursor-name>)
end class;


define sealed class <odbc-cardinality-violation>
    (<odbc-diagnostic>, <cardinality-violation>)
end class;


define sealed class <odbc-connection-does-not-exist> 
    (<odbc-diagnostic>, <connection-does-not-exist>) 
end class;

define sealed class <odbc-connection-failure> 
    (<odbc-diagnostic>, <connection-failure>) 
end class;

define sealed class <odbc-connection-name-in-use> 
    (<odbc-connection-exception>, <connection-name-in-use>)
end class;

define sealed class <odbc-sql-client-unable-to-establish-connection> 
    (<odbc-connection-exception>, <sql-client-unable-to-establish-connection>)
end class;

define sealed class <odbc-sql-server-rejected-establishment-of-connection> 
    (<odbc-connection-exception>,
     <sql-server-rejected-establishment-of-connection>)
end class;

define sealed class <odbc-transaction-resolution-unknown> 
    (<odbc-connection-exception>, <transaction-resolution-unknown>)
end class;

define sealed class <odbc-cursor-operation-conflict>
   (<odbc-diagnostic>, <cursor-operation-conflict>)
end class;

define sealed class <odbc-data-exception> 
    (<odbc-diagnostic>, <data-exception>)
end class;

define sealed class <odbc-character-not-in-repertoire> 
    (<odbc-data-exception>, <character-not-in-repertoire>)
end class;

define sealed class <odbc-datetime-field-overflow> 
    (<odbc-data-exception>, <datetime-field-overflow>)
end class;

define sealed class <odbc-division-by-zero> 
    (<odbc-data-exception>, <division-by-zero>)
end class;

define sealed class <odbc-error-in-assignment> 
    (<odbc-data-exception>, <error-in-assignment>)
end class;

define sealed class <odbc-indicator-overflow> 
    (<odbc-data-exception>, <indicator-overflow>)
end class;

define sealed class <odbc-interval-field-overflow> 
    (<odbc-data-exception>, <interval-field-overflow>)
end class;

define sealed class <odbc-invalid-character-value-for-cast> 
    (<odbc-data-exception>, <invalid-character-value-for-cast>)
end class;

define sealed class <odbc-invalid-datetime-format> 
    (<odbc-data-exception>, <invalid-datetime-format>)
end class;

define sealed class <odbc-invalid-escape-character> 
    (<odbc-data-exception>, <invalid-escape-character>)
end class;

define sealed class <odbc-invalid-escape-sequence> 
    (<odbc-data-exception>, <invalid-escape-sequence>)
end class;

define sealed class <odbc-invalid-fetch-sequence> 
    (<odbc-data-exception>, <invalid-fetch-sequence>)
end class;

define sealed class <odbc-invalid-parameter-value> 
    (<odbc-data-exception>, <invalid-parameter-value>)
end class;

define sealed class <odbc-invalid-time-zone-displacement-value> 
    (<odbc-data-exception>, <invalid-time-zone-displacement-value>)
end class;

define sealed class <odbc-null-value-no-indicator-parameter> 
    (<odbc-data-exception>, <null-value-no-indicator-parameter>)
end class;

define sealed class <odbc-numeric-value-out-of-range> 
    (<odbc-data-exception>, <numeric-value-out-of-range>)
end class;

define sealed class <odbc-string-data-length-mismatch> 
    (<odbc-data-exception>, <string-data-length-mismatch>)
end class;

define sealed class <odbc-string-data-right-truncation> 
    (<odbc-data-exception>, <string-data-right-truncation>)
end class;

define sealed class <odbc-substring-error> 
    (<odbc-data-exception>, <substring-error>)
end class;

define sealed class <odbc-trim-error> 
    (<odbc-data-exception>, <trim-error>)
end class;

define sealed class <odbc-unterminated-C-string> 
    (<odbc-data-exception>, <unterminated-C-string>)
end class;

define sealed class <odbc-dependent-privilege-descriptors-still-exist> 
    (<odbc-diagnostic>, <dependent-privilege-descriptors-still-exist>)
end class;

define sealed class <odbc-cursor-specification-cannot-be-executed> 
    (<odbc-dynamic-sql-error>, <cursor-specification-cannot-be-executed>)
end class;

define sealed class <odbc-invalid-descriptor-count> 
    (<odbc-dynamic-sql-error>, <invalid-descriptor-count>)
end class;

define sealed class <odbc-invalid-descriptor-index> 
    (<odbc-dynamic-sql-error>, <invalid-descriptor-index>)
end class;

define sealed class <odbc-prepared-statement-not-a-cursor-specification> 
    (<odbc-dynamic-sql-error>, <prepared-statement-not-a-cursor-specification>)
end class;

define sealed class <odbc-restricted-data-type-attribute-violation> 
    (<odbc-dynamic-sql-error>, <restricted-data-type-attribute-violation>)
end class;

define sealed class 
  <odbc-using-clause-does-not-match-dynamic-parameter-specification>
     (<odbc-dynamic-sql-error>,
      <using-clause-does-not-match-dynamic-parameter-specification>)
end class;

define sealed class <odbc-using-clause-does-not-match-target-specification> 
    (<odbc-dynamic-sql-error>, 
     <using-clause-does-not-match-target-specification>)
end class;

define sealed class <odbc-using-clause-required-for-dynamic-parameters> 
    (<odbc-dynamic-sql-error>, <using-clause-required-for-dynamic-parameters>)
end class;

define sealed class <odbc-using-clause-required-for-result-fields> 
    (<odbc-dynamic-sql-error>, <using-clause-required-for-result-fields>)
end class;

define sealed class <odbc-feature-not-supported> 
    (<odbc-diagnostic>, <feature-not-supported>)
end class;

define sealed class <odbc-multiple-server-transaction> 
    (<odbc-feature-not-supported>, <multiple-server-transaction>)
end class;

define sealed class <odbc-integrity-constraint-violation> 
    (<odbc-diagnostic>, <integrity-constraint-violation>)
end class;

define sealed class <odbc-invalid-authorization-specification> 
    (<odbc-diagnostic>, <invalid-authorization-specification>)
end class;

define sealed class <odbc-invalid-catalog-name> 
    (<odbc-diagnostic>, <invalid-catalog-name>)
end class;

define sealed class <odbc-invalid-character-set-name> 
    (<odbc-diagnostic>, <invalid-character-set-name>)
end class;

define sealed class <odbc-invalid-condition-number> 
    (<odbc-diagnostic>, <invalid-condition-number>)
end class;

define sealed class <odbc-invalid-cursor-name> 
    (<odbc-diagnostic>, <invalid-cursor-name>)
end class;

define sealed class <odbc-invalid-schema-name> 
    (<odbc-diagnostic>, <invalid-schema-name>)
end class;

define sealed class <odbc-invalid-sql-descriptor-name> 
    (<odbc-diagnostic>, <invalid-sql-descriptor-name>)
end class;

define sealed class <odbc-invalid-sql-statement-name> 
    (<odbc-diagnostic>, <invalid-sql-statement-name>)
end class;

define sealed class <odbc-invalid-transaction-state> 
    (<odbc-diagnostic>, <invalid-transaction-state>)
end class;

define sealed class <odbc-invalid-transaction-termination> 
    (<odbc-diagnostic>, <invalid-transaction-termination>)
end class;

define sealed class <odbc-no-data> 
    (<odbc-diagnostic>, <no-data>)
end class;

define sealed class <odbc-remote-database-access>
    (<odbc-diagnostic>, <remote-database-access>)
end class;

define sealed class <odbc-successful-completion> 
    (<odbc-diagnostic>, <successful-completion>)
end class;

define sealed class <odbc-syntax-error-or-access-rule-violation> 
    (<odbc-diagnostic>, <syntax-error-or-access-rule-violation>)
end class;

define sealed class
     <odbc-syntax-error-or-access-rule-violation-in-direct-sql-statement> 
     (<odbc-diagnostic>,
      <syntax-error-or-access-rule-violation-in-direct-sql-statement>)
end class;

define sealed class
    <odbc-syntax-error-or-access-rule-violation-in-dynamic-sql-statement> 
    (<odbc-diagnostic>,
     <syntax-error-or-access-rule-violation-in-dynamic-sql-statement>)
end class;

define sealed class <odbc-transaction-rollback>
    (<odbc-diagnostic>, <transaction-rollback>)
end class;

define sealed class 
  <odbc-transaction-rollback-due-to-integrity-constraint-violation>
    (<odbc-transaction-rollback>, 
     <transaction-rollback-due-to-integrity-constraint-violation>)
end class;

define sealed class <odbc-transaction-rollback-due-to-serialization-failure>
    (<odbc-transaction-rollback>,
     <transaction-rollback-due-to-serialization-failure>)
end class;

define sealed class <odbc-statement-completion-unknown> 
    (<odbc-transaction-rollback>, <statement-completion-unknown>)
end class;

define sealed class <odbc-triggered-data-change-violation> 
    (<odbc-diagnostic>, <triggered-data-change-violation>)
end class;

define sealed class <odbc-warning-cursor-operation-conflict> 
    (<odbc-sql-warning>, <warning-cursor-operation-conflict>)
end class;

define sealed class <odbc-disconnect-error>
    (<odbc-sql-warning>, <disconnect-error>)
end class;

define sealed class <odbc-implicit-zero-bit-padding> 
    (<odbc-sql-warning>, <implicit-zero-bit-padding>)
end class;

define sealed class <odbc-insufficient-item-descriptor-areas> 
    (<odbc-sql-warning>, <insufficient-item-descriptor-areas>)
end class;

define sealed class <odbc-null-value-eliminated-in-set-function> 
    (<odbc-sql-warning>, <null-value-eliminated-in-set-function>)
end class;

define sealed class <odbc-privilege-not-granted> 
    (<odbc-sql-warning>, <privilege-not-granted>)
end class;

define sealed class <odbc-privilege-not-revoked> 
    (<odbc-sql-warning>, <privilege-not-revoked>)
end class;

define sealed class <odbc-query-expression-too-long-for-information-schema> 
    (<odbc-sql-warning>, 
     <query-expression-too-long-for-information-schema>)
end class;

define sealed class <odbc-search-condition-too-long-for-information-schema> 
    (<odbc-sql-warning>,
     <search-condition-too-long-for-information-schema>)
end class;

define sealed class <odbc-warning-string-data-right-truncation> 
    (<odbc-sql-warning>, <warning-string-data-right-truncation>)
end class;

define sealed class <odbc-with-check-option-violation> 
    (<odbc-diagnostic>, <with-check-option-violation>)
end class;


//---------- ODBC Unique Diagnostic Details

define sealed class <odbc-invalid-connection-string-attribute>
    (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S00";
end class;

define sealed class <odbc-error-in-row> (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S01";
end class;

define sealed class <odbc-option-value-changed> (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S02";
end class;

define sealed class 
    <odbc-attempt-to-fetch-before-result-set-returned-the-first-rowset>
    (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S06";
end class;

define sealed class <odbc-fractional-truncation> (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S07";
end class;

define sealed class <odbc-error-saving-file-dsn> (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S08";
end class;

define sealed class <odbc-invalid-keyword> (<odbc-sql-warning>)
  keyword class-code: = "01";
  keyword subclass-code: = "S09";
end class;

define sealed class <odbc-invalid-use-of-default-parameter>
    (<odbc-dynamic-sql-error>)
  keyword class-code: = "07";
  keyword subclass-code: = "S01";
end class;

define sealed class <odbc-communication-link-failure> 
    (<odbc-connection-exception>)
  keyword class-code: = "08";
  keyword subclass-code: = "S01";
end class;

define sealed class <odbc-insert-value-list-does-not-match-column-list>
    (<odbc-cardinality-violation>)
  keyword class-code: = "21";
  keyword subclass-code: = "S01";
end class;

define sealed class <odbc-invalid-cursor-state> (<odbc-diagnostic>)
  keyword class-code: = "24";
  keyword subclass-code: = "000";
end class;

define sealed class <odbc-transaction-state>
    (<odbc-invalid-transaction-state>)
  keyword class-code: = "25";
  keyword subclass-code: = "S01";
end class;

define sealed class <odbc-transaction-still-active>
    (<odbc-invalid-transaction-state>)
  keyword class-code: = "25";
  keyword subclass-code: = "S02";
end class;

define sealed class <odbc-transaction-is-rolledback>
    (<odbc-invalid-transaction-state>)
  keyword class-code: = "25";
  keyword subclass-code: = "S03";
end class;

define sealed class <odbc-base-table-or-view-already-exists>
    (<odbc-syntax-error-or-access-rule-violation>)
  keyword class-code: = "42";
  keyword subclass-code: = "S01";
end class;

define sealed class <odbc-base-table-or-view-not-found>
    (<odbc-syntax-error-or-access-rule-violation>)
  keyword class-code: = "42";
  keyword subclass-code: = "S02";
end class;

define sealed class <odbc-index-already-exists>
    (<odbc-syntax-error-or-access-rule-violation>)
  keyword class-code: = "42";
  keyword subclass-code: = "S11";
end class;

define sealed class <odbc-index-not-found>
    (<odbc-syntax-error-or-access-rule-violation>)
  keyword class-code: = "42";
  keyword subclass-code: = "S12";
end class;

define sealed class <odbc-column-already-exists>
    (<odbc-syntax-error-or-access-rule-violation>)
  keyword class-code: = "42";
  keyword subclass-code: = "S21";
end class;

define sealed class <odbc-column-not-found>
    (<odbc-syntax-error-or-access-rule-violation>)
  keyword class-code: = "42";
  keyword subclass-code: = "S22";
end class;

define sealed class <odbc-general-error> (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "000";
end class;

define sealed class <odbc-memory-allocation-error> (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "001";
end class;

define sealed class <odbc-invalid-application-buffer-type>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "003";
end class;

define sealed class <odbc-invalid-sql-data-type>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "004";
end class;

define sealed class <odbc-associated-statement-is-not-prepared>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "007";
end class;

define sealed class <odbc-operation-canceled> (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "008";
end class;

define sealed class <odbc-invalid-use-of-null-pointer>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "009";
end class;

define sealed class <odbc-function-sequence-error> (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "010";
end class;

define sealed class <odbc-attribute-cannot-be-set-now>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "011";
end class;

define sealed class <odbc-invalid-transaction-operation-code>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "012";
end class;

define sealed class <odbc-memory-management-error> (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "013";
end class;

define sealed class <odbc-limit-on-the-number-of-handles-exceeded>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "014";
end class;

define sealed class <odbc-no-cursor-name-available> (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "015";
end class;

define sealed class <odbc-cannot-modify-an-implementation-row-descriptor>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "016";
end class;

define sealed class 
    <odbc-invalid-use-of-an-atumatically-allocated-descriptor-handle>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "017";
end class;

define sealed class <odbc-server-declined-cancel-request>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "018";
end class;

define sealed class <odbc-non-character-and-non-binary-data-sent-in-pieces>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "019";
end class;

define sealed class <odbc-attempt-to-concatenate-a-null-value>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "020";
end class;

define sealed class <odbc-inconsistent-descriptor-information>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "021";
end class;

define sealed class <odbc-invalid-attribute-value>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "024";
end class;

define sealed class <odbc-invalid-string-or-buffer-length>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "090";
end class;

define sealed class <odbc-invalid-descriptor-field-identifier>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "091";
end class;

define sealed class <odbc-invalid-attribute-option-identifier>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "092";
end class;

define sealed class <odbc-invalid-parameter-number>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "093";
end class;

define sealed class <odbc-function-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "095";
end class;

define sealed class <odbc-invalid-information-type>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "096";
end class;

define sealed class <odbc-column-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "097";
end class;

define sealed class <odbc-scope-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "098";
end class;

define sealed class <odbc-nullable-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "099";
end class;

define sealed class <odbc-uniqueness-option-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "100";
end class;

define sealed class <odbc-accuracy-option-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "101";
end class;

define sealed class <odbc-invalid-retrieval-code>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "103";
end class;

define sealed class <odbc-invalid-precision-or-scale-value>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "104";
end class;

define sealed class <odbc-invalid-parameter-type>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "105";
end class;

define sealed class <odbc-fetch-type-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "106";
end class;

define sealed class <odbc-row-value-out-of-range>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "107";
end class;

define sealed class <odbc-invalid-cursor-position>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "109";
end class;

define sealed class <odbc-invalid-driver-completion>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "110";
end class;

define sealed class <odbc-invalid-bookmark-value>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "111";
end class;

define sealed class <odbc-optional-feature-not-implemented>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "C00";
end class;

define sealed class <odbc-timeout-expired>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "T00";
end class;

define sealed class <odbc-connection-timeout-expired>
    (<odbc-diagnostic>)
  keyword class-code: = "HY";
  keyword subclass-code: = "T01";
end class;

define sealed class <odbc-driver-does-not-support-this-function>
    (<odbc-diagnostic>)
  keyword class-code: = "IM";
  keyword subclass-code: = "001";
end class;

define sealed class <odbc-data-source-name-not-found>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "002";
end class;

define sealed class <odbc-specified-driver-could-not-be-loaded>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "003";
end class;

define sealed class <odbc-driver-SQLAllocHandle-on-SQL-HANDLE-ENV-failed>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "004";
end class;

define sealed class <odbc-driver-SQLAllocHandle-on-SQL-HANDLE-DBC-failed>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "005";
end class;

define sealed class <odbc-driver-SQLSetConnectAttr-failed>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "006";
end class;

define sealed class <odbc-no-data-source-or-driver-specified>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "007";
end class;

define sealed class <odbc-dialog-failed>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "008";
end class;

define sealed class <odbc-unable-to-load-translation-dll>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "009";
end class;

define sealed class <odbc-data-source-name-too-long>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "010";
end class;

define sealed class <odbc-driver-name-too-long>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "011";
end class;

define sealed class <odbc-DRIVER-keyword-syntax-error>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "012";
end class;

define sealed class <odbc-invalid-name-of-file-DSN>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "014";
end class;

define sealed class <odbc-corrupt-file-data-source>
    (<odbc-connection-exception>)
  keyword class-code: = "IM";
  keyword subclass-code: = "015";
end class;

define sealed class <odbc-trace-file-error> (<odbc-diagnostic>)
  keyword class-code: = "IM";
  keyword subclass-code: = "013";
end class;



//--------------------  Diagnostic Installation  --------------------


define constant $odbc-diagnostics-key = #"odbc-dbms";


define function install-odbc-diagnostics(table :: <diagnostic-table>) 
 => ()
  local method install-diag(class :: <class>) => ()
	  install-diagnostic(table, class, key: $odbc-diagnostics-key)
        end method;

  install-diagnostic-key($odbc-diagnostics-key);

  //---------- Non-unique diagnostics  -----------
  install-diag(<odbc-ambiguous-cursor-name>);
  install-diag(<odbc-connection-does-not-exist>);
  install-diag(<odbc-connection-failure>);
  install-diag(<odbc-connection-name-in-use>);
  install-diag(<odbc-sql-client-unable-to-establish-connection>);
  install-diag(<odbc-sql-server-rejected-establishment-of-connection>);
  install-diag(<odbc-transaction-resolution-unknown>);
  install-diag(<odbc-cursor-operation-conflict>);
  install-diag(<odbc-data-exception>);
  install-diag(<odbc-character-not-in-repertoire>);
  install-diag(<odbc-datetime-field-overflow>);
  install-diag(<odbc-division-by-zero>);
  install-diag(<odbc-error-in-assignment>);
  install-diag(<odbc-indicator-overflow>);
  install-diag(<odbc-interval-field-overflow>);
  install-diag(<odbc-invalid-character-value-for-cast>);
  install-diag(<odbc-invalid-datetime-format>);
  install-diag(<odbc-invalid-escape-character>);
  install-diag(<odbc-invalid-escape-sequence>);
  install-diag(<odbc-invalid-fetch-sequence>);
  install-diag(<odbc-invalid-parameter-value>);
  install-diag(<odbc-invalid-time-zone-displacement-value>);
  install-diag(<odbc-null-value-no-indicator-parameter>);
  install-diag(<odbc-numeric-value-out-of-range>);
  install-diag(<odbc-string-data-length-mismatch>);
  install-diag(<odbc-string-data-right-truncation>);
  install-diag(<odbc-substring-error>);
  install-diag(<odbc-trim-error>);
  install-diag(<odbc-unterminated-C-string>);
  install-diag(<odbc-dependent-privilege-descriptors-still-exist>);
  install-diag(<odbc-cursor-specification-cannot-be-executed>);
  install-diag(<odbc-invalid-descriptor-count>);
  install-diag(<odbc-invalid-descriptor-index>);
  install-diag(<odbc-prepared-statement-not-a-cursor-specification>);
  install-diag(<odbc-restricted-data-type-attribute-violation>);
  install-diag(
    <odbc-using-clause-does-not-match-dynamic-parameter-specification>);
  install-diag(<odbc-using-clause-does-not-match-target-specification>);
  install-diag(<odbc-using-clause-required-for-dynamic-parameters>);
  install-diag(<odbc-using-clause-required-for-result-fields>);
  install-diag(<odbc-feature-not-supported>);
  install-diag(<odbc-multiple-server-transaction>);
  install-diag(<odbc-integrity-constraint-violation>);
  install-diag(<odbc-invalid-authorization-specification>);
  install-diag(<odbc-invalid-catalog-name>);
  install-diag(<odbc-invalid-character-set-name>);
  install-diag(<odbc-invalid-condition-number>);
  install-diag(<odbc-invalid-cursor-name>);
  install-diag(<odbc-invalid-schema-name>);
  install-diag(<odbc-invalid-sql-descriptor-name>);
  install-diag(<odbc-invalid-sql-statement-name>);
  install-diag(<odbc-invalid-transaction-state>);
  install-diag(<odbc-invalid-transaction-termination>);
  install-diag(<odbc-no-data>);
  install-diag(<odbc-remote-database-access>);
  install-diag(<odbc-successful-completion>);
  install-diag(<odbc-syntax-error-or-access-rule-violation>);
  install-diag(
    <odbc-syntax-error-or-access-rule-violation-in-direct-sql-statement>);
  install-diag(
    <odbc-syntax-error-or-access-rule-violation-in-dynamic-sql-statement>);
  install-diag(<odbc-transaction-rollback>);
  install-diag(
    <odbc-transaction-rollback-due-to-integrity-constraint-violation>);
  install-diag(<odbc-transaction-rollback-due-to-serialization-failure>);
  install-diag(<odbc-statement-completion-unknown>);
  install-diag(<odbc-triggered-data-change-violation>);
  install-diag(<odbc-warning-cursor-operation-conflict>);
  install-diag(<odbc-disconnect-error>);
  install-diag(<odbc-implicit-zero-bit-padding>);
  install-diag(<odbc-insufficient-item-descriptor-areas>);
  install-diag(<odbc-null-value-eliminated-in-set-function>);
  install-diag(<odbc-privilege-not-granted>);
  install-diag(<odbc-privilege-not-revoked>);
  install-diag(<odbc-query-expression-too-long-for-information-schema>);
  install-diag(<odbc-search-condition-too-long-for-information-schema>);
  install-diag(<odbc-warning-string-data-right-truncation>);
  install-diag(<odbc-with-check-option-violation>);
  install-diag(<odbc-sql-warning>);


  //----------  Unique ODBC Diagnostics  ----------

  install-diag(<odbc-syntax-error-or-access-rule-violation>);
  install-diag(<odbc-invalid-connection-string-attribute>);
  install-diag(<odbc-error-in-row>);
  install-diag(<odbc-option-value-changed>);
  install-diag(
    <odbc-attempt-to-fetch-before-result-set-returned-the-first-rowset>);
  install-diag(<odbc-fractional-truncation>);
  install-diag(<odbc-error-saving-file-dsn>);
  install-diag(<odbc-invalid-keyword>);
  install-diag(<odbc-invalid-use-of-default-parameter>);
  install-diag(<odbc-communication-link-failure>);
  install-diag(<odbc-insert-value-list-does-not-match-column-list>);
  install-diag(<odbc-invalid-cursor-state>);
  install-diag(<odbc-transaction-state>);
  install-diag(<odbc-transaction-still-active>);
  install-diag(<odbc-transaction-is-rolledback>);
  install-diag(<odbc-base-table-or-view-already-exists>);
  install-diag(<odbc-base-table-or-view-not-found>);
  install-diag(<odbc-index-already-exists>);
  install-diag(<odbc-index-not-found>);
  install-diag(<odbc-column-already-exists>);
  install-diag(<odbc-column-not-found>);
  install-diag(<odbc-general-error>);
  install-diag(<odbc-memory-allocation-error>);
  install-diag(<odbc-invalid-application-buffer-type>);
  install-diag(<odbc-invalid-sql-data-type>);
  install-diag(<odbc-associated-statement-is-not-prepared>);
  install-diag(<odbc-operation-canceled>);
  install-diag(<odbc-invalid-use-of-null-pointer>);
  install-diag(<odbc-function-sequence-error>);
  install-diag(<odbc-attribute-cannot-be-set-now>);
  install-diag(<odbc-invalid-transaction-operation-code>);
  install-diag(<odbc-memory-management-error>);
  install-diag(<odbc-limit-on-the-number-of-handles-exceeded>);
  install-diag(<odbc-no-cursor-name-available>);
  install-diag(<odbc-cannot-modify-an-implementation-row-descriptor>);
  install-diag(
    <odbc-invalid-use-of-an-atumatically-allocated-descriptor-handle>);
  install-diag(<odbc-server-declined-cancel-request>);
  install-diag(<odbc-non-character-and-non-binary-data-sent-in-pieces>);
  install-diag(<odbc-attempt-to-concatenate-a-null-value>);
  install-diag(<odbc-inconsistent-descriptor-information>);
  install-diag(<odbc-invalid-attribute-value>);
  install-diag(<odbc-invalid-string-or-buffer-length>);
  install-diag(<odbc-invalid-descriptor-field-identifier>);
  install-diag(<odbc-invalid-attribute-option-identifier>);
  install-diag(<odbc-invalid-parameter-number>);
  install-diag(<odbc-function-type-out-of-range>);
  install-diag(<odbc-invalid-information-type>);
  install-diag(<odbc-column-type-out-of-range>);
  install-diag(<odbc-scope-type-out-of-range>);
  install-diag(<odbc-nullable-type-out-of-range>);
  install-diag(<odbc-uniqueness-option-type-out-of-range>);
  install-diag(<odbc-accuracy-option-type-out-of-range>);
  install-diag(<odbc-invalid-retrieval-code>);
  install-diag(<odbc-invalid-precision-or-scale-value>);
  install-diag(<odbc-invalid-parameter-type>);
  install-diag(<odbc-fetch-type-out-of-range>);
  install-diag(<odbc-row-value-out-of-range>);
  install-diag(<odbc-invalid-cursor-position>);
  install-diag(<odbc-invalid-driver-completion>);
  install-diag(<odbc-invalid-bookmark-value>);
  install-diag(<odbc-optional-feature-not-implemented>);
  install-diag(<odbc-timeout-expired>);
  install-diag(<odbc-connection-timeout-expired>);
  install-diag(<odbc-driver-does-not-support-this-function>);
  install-diag(<odbc-data-source-name-not-found>);
  install-diag(<odbc-specified-driver-could-not-be-loaded>);
  install-diag(<odbc-driver-SQLAllocHandle-on-SQL-HANDLE-ENV-failed>);
  install-diag(<odbc-driver-SQLAllocHandle-on-SQL-HANDLE-DBC-failed>);
  install-diag(<odbc-driver-SQLSetConnectAttr-failed>);
  install-diag(<odbc-no-data-source-or-driver-specified>);
  install-diag(<odbc-dialog-failed>);
  install-diag(<odbc-unable-to-load-translation-dll>);
  install-diag(<odbc-data-source-name-too-long>);
  install-diag(<odbc-driver-name-too-long>);
  install-diag(<odbc-DRIVER-keyword-syntax-error>);
  install-diag(<odbc-invalid-name-of-file-DSN>);
  install-diag(<odbc-corrupt-file-data-source>);
  install-diag(<odbc-trace-file-error>);
end function;

register-diagnostic-installer(install-odbc-diagnostics);
