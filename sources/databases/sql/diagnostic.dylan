Module: sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// $HopeName: !diagnostic.dylan(D-kan.3) $


define function detail-info-not-available(detail-info :: <string>)
 => (str :: <string>)
  let msg = "information not available";

  signal(msg);
  msg;
end function;


//----------  Diagnostic Detail  ----------

define open abstract class <diagnostic> (<condition>)
  constant class slot class-code :: <string>,
    required-init-keyword: class-code:;

  constant each-subclass slot subclass-code :: <string> = "000",
    init-keyword: subclass-code:;

  constant slot condition-number :: <integer> = 1,
    init-keyword: condition-number:;

  constant slot possible-explanation :: <deque> = make(<deque>);
end class;

define method default-handler(diagnostic :: <diagnostic>)
  // Since <diagnostic> is a subclass of <condition>, a signalled diagnostic
  // will be dropped on the floor (similar to a warning) and the code will
  // continue to execute. An unhandled-diagnostic is an error so the debugger
  // will take notice of it.
  error(make(<unhandled-diagnostic>,
             diagnostic: diagnostic));
end method default-handler;


define open class <unknown-sqlstate> (<diagnostic>)
  keyword class-code: = "";
  keyword subclass-code: = "";

  constant slot sqlstate :: <string> = "",
    init-keyword: sqlstate:;
end class;

//---*** andrewa: not used, for some reason
ignore(sqlstate);

define open generic conditions-not-recorded?(diag :: <diagnostic>)
 => (not-recorded-status :: <boolean>);

define method conditions-not-recorded?(diag :: <diagnostic>)
 => (not-recorded-status :: <boolean>)
 #f;
end method;


define open generic dynamic-function(diag :: <diagnostic>)
 => (dynamic-function :: <string>);

define method dynamic-function(diag :: <diagnostic>)
 => (dynamic-function :: <string>)
  detail-info-not-available("dynamic-function");
end method;


define open generic row-count(diag :: <diagnostic>)
 => (count :: <integer>);

define method row-count(diag :: <diagnostic>)
 => (count :: <integer>)
  0;
end method;


define open generic command-function(diag :: <diagnostic>)
 => (command-function :: <string>);

define method command-function(diag :: <diagnostic>)
 => (command-function :: <string>)
  detail-info-not-available("command-function");
end method;


define open generic returned-sqlstate(diag :: <diagnostic>)
 => (sqlstate :: <string>);

define method returned-sqlstate(diag :: <diagnostic>)
 => (sqlstate :: <string>)
  detail-info-not-available("returned-sqlstate");
end method;


define open generic class-origin(diag :: <diagnostic>)
 => (class-origin :: <string>);

define method class-origin(diag :: <diagnostic>)
 => (class-origin :: <string>)
  detail-info-not-available("class-origin");
end method;


define open generic subclass-origin(diag :: <diagnostic>)
 => (subclass-origin :: <string>);

define method subclass-origin(diag :: <diagnostic>)
 => (subclass-origin :: <string>);
  detail-info-not-available("subclass-origin");
end method;


define open generic constraint-catalog(diag :: <diagnostic>)
 => (constraint-catalog :: <string>);

define method constraint-catalog(diag :: <diagnostic>)
 => (constraint-catalog :: <string>);
  detail-info-not-available("constraint-catalog");
end method;


define open generic constraint-schema(diag :: <diagnostic>)
 => (constraint-schema :: <string>);

define method constraint-schema(diag :: <diagnostic>)
 => (constraint-schema :: <string>);
  detail-info-not-available("constraint-schema");
end method;


define open generic constraint-name(diag :: <diagnostic>)
 => (constraint-name :: <string>);

define method constraint-name(diag :: <diagnostic>)
 => (constraint-name :: <string>);
  detail-info-not-available("constraint-name");
end method;


define open generic connection-name(diag :: <diagnostic>)
 => (connection-name :: <string>);

define method connection-name(diag :: <diagnostic>)
 => (connection-name :: <string>);
  detail-info-not-available("connection-name");
end method;


define open generic environment-name(diag :: <diagnostic>)
 => (env-name :: <string>);

define method environment-name(diag :: <diagnostic>)
 => (env-name :: <string>);
  detail-info-not-available("environment-name");
end method;


define open generic catalog-name(diag :: <diagnostic>)
 => (catalog-name :: <string>);

define method catalog-name(diag :: <diagnostic>)
 => (catalog-name :: <string>);
  detail-info-not-available("catalog-name");
end method;


define open generic schema-name(diag :: <diagnostic>)
 => (schema-name :: <string>);

define method schema-name(diag :: <diagnostic>)
 => (schema-name :: <string>);
  detail-info-not-available("schema-name");
end method;


define open generic table-name(diag :: <diagnostic>)
 => (table-name :: <string>);

define method table-name(diag :: <diagnostic>)
 => (table-name :: <string>);
  detail-info-not-available("table-name");
end method;


define open generic column-name(diag :: <diagnostic>)
 => (column-name :: <string>);

define method column-name(diag :: <diagnostic>)
 => (column-name :: <string>);
  detail-info-not-available("column-name");
end method;


define open generic cursor-name(diag :: <diagnostic>)
 => (cursor-name :: <string>);

define method cursor-name(diag :: <diagnostic>)
 => (cursor-name :: <string>);
  detail-info-not-available("cursor-name");
end method;


define open generic message-text(diag :: <diagnostic>)
 => (message-text :: <string>); 

define method message-text(diag :: <diagnostic>)
 => (message-text :: <string>); 
  detail-info-not-available("message-text");
end method;


define open generic next-dbms-diagnostic(diag :: <diagnostic>)
 => (next-diagnostic :: false-or(<diagnostic>));

define method next-dbms-diagnostic(diag :: <diagnostic>)
 => (next-diagnostic :: false-or(<diagnostic>))
  #f;
end method;


define open generic diagnostic-to-string(diag :: <diagnostic>)
 => (string :: <string>);

define method diagnostic-to-string(diag :: <diagnostic>)
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
                   "  Constraint catalog: %=\n"
                   "  Constraint schema: %=\n"
                   "  Constraint name: %=\n"
                   "  Connection name: %=\n"
                   "  Environment name: %=\n"
                   "  Catalog name: %=\n"
                   "  Schema name: %=\n"
                   "  Table name: %=\n"
                   "  Column name: %=\n"
                   "  Message Text: %=\n",
                   diag.conditions-not-recorded?,
                   diag.command-function,
                   diag.dynamic-function,
                   diag.row-count,
                   diag.condition-number,
                   diag.returned-sqlstate,
                   diag.class-origin,
                   diag.subclass-origin,
                   diag.constraint-catalog,
                   diag.constraint-schema,
                   diag.constraint-name,
                   diag.connection-name,
                   diag.environment-name,
                   diag.catalog-name,
                   diag.schema-name,
                   diag.table-name,
                   diag.column-name,
                   diag.message-text)
end method;


define method condition-to-string
    (diag :: <diagnostic>)
 => (string :: false-or(<string>))
  let diag-string :: <string> = make(<string>);
  let test-diag = diag;
  while (test-diag ~= #f)
    diag-string := concatenate(diag-string, diagnostic-to-string(test-diag));
    test-diag := next-dbms-diagnostic(test-diag);
  end while;
  diag-string
end method condition-to-string;


//----------  Specific Diagnostic Detail  ----------

define open class <ambiguous-cursor-name> (<diagnostic>)
  keyword class-code: = "3C";
end class;

define open class <cardinality-violation> (<diagnostic>)
  keyword class-code: = "21";
end class;

define open class <connection-exception> (<diagnostic>)
  keyword class-code: = "08";
end class;

define open class <connection-does-not-exist> (<connection-exception>) 
  keyword subclass-code: = "003";
end class;

define open class <connection-failure> (<connection-exception>) 
  keyword subclass-code: = "006";
end class;

define open class <connection-name-in-use> (<connection-exception>)
  keyword subclass-code: = "002";
end class;

define open class <sql-client-unable-to-establish-connection> 
    (<connection-exception>)
  keyword subclass-code: = "001";
end class;

define open class <sql-server-rejected-establishment-of-connection> 
    (<connection-exception>)
  keyword subclass-code: = "004";
end class;

define open class <transaction-resolution-unknown> (<connection-exception>)
  keyword subclass-code: = "007";
end class;

define open class <cursor-operation-conflict> (<diagnostic>)
  keyword class-code: = "09";
end class;

define open class <data-exception> (<diagnostic>)
  keyword class-code: = "22";
end class;

define open class <character-not-in-repertoire> (<data-exception>)
  keyword subclass-code: = "021";
end class;

define open class <datetime-field-overflow> (<data-exception>)
  keyword subclass-code: = "008";
end class;

define open class <division-by-zero> (<data-exception>)
  keyword subclass-code: = "012";
end class;

define open class <error-in-assignment> (<data-exception>)
  keyword subclass-code: = "005";
end class;

define open class <indicator-overflow> (<data-exception>)
  keyword subclass-code: = "022";
end class;

define open class <interval-field-overflow> (<data-exception>)
  keyword subclass-code: = "015";
end class;

define open class <invalid-character-value-for-cast> (<data-exception>)
  keyword subclass-code: = "018";
end class;

define open class <invalid-datetime-format> (<data-exception>)
  keyword subclass-code: = "007";
end class;

define open class <invalid-escape-character> (<data-exception>)
  keyword subclass-code: = "019";
end class;

define open class <invalid-escape-sequence> (<data-exception>)
  keyword subclass-code: = "025";
end class;

define open class <invalid-fetch-sequence> (<data-exception>)
  keyword subclass-code: = "006";
end class;

define open class <invalid-parameter-value> (<data-exception>)
  keyword subclass-code: = "023";
end class;

define open class <invalid-time-zone-displacement-value> (<data-exception>)
  keyword subclass-code: = "009";
end class;

define open class <null-value-no-indicator-parameter> (<data-exception>)
  keyword subclass-code: = "002";
end class;

define open class <numeric-value-out-of-range> (<data-exception>)
  keyword subclass-code: = "003";
end class;

define open class <string-data-length-mismatch> (<data-exception>)
  keyword subclass-code: = "026";
end class;

define open class <string-data-right-truncation> (<data-exception>)
  keyword subclass-code: = "001";
end class;

define open class <substring-error> (<data-exception>)
  keyword subclass-code: = "011";
end class;

define open class <trim-error> (<data-exception>)
  keyword subclass-code: = "027";
end class;

define open class <unterminated-C-string> (<data-exception>)
  keyword subclass-code: = "024";
end class;

define open class <dependent-privilege-descriptors-still-exist> 
    (<diagnostic>)
  keyword class-code: = "2B";
end class;

define open  class <dynamic-sql-error> (<diagnostic>)
  keyword class-code: = "07";
end class;

define open class <cursor-specification-cannot-be-executed> 
    (<dynamic-sql-error>)
  keyword subclass-code: = "003";
end class;

define open class <invalid-descriptor-count> (<dynamic-sql-error>)
  keyword subclass-code: = "008";
end class;

define open class <invalid-descriptor-index> (<dynamic-sql-error>)
  keyword subclass-code: = "009";
end class;

define open class <prepared-statement-not-a-cursor-specification> 
    (<dynamic-sql-error>)
  keyword subclass-code: = "005";
end class;

define open class <restricted-data-type-attribute-violation> 
    (<dynamic-sql-error>)
  keyword subclass-code: = "006";
end class;

define open class <using-clause-does-not-match-dynamic-parameter-specification>
     (<dynamic-sql-error>)
  keyword subclass-code: = "001";
end class;

define open class <using-clause-does-not-match-target-specification> 
    (<dynamic-sql-error>)
  keyword subclass-code: = "002";
end class;

define open class <using-clause-required-for-dynamic-parameters> 
    (<dynamic-sql-error>)
  keyword subclass-code: = "004";
end class;

define open class <using-clause-required-for-result-fields> 
    (<dynamic-sql-error>)
  keyword subclass-code: = "007";
end class;

define open class <feature-not-supported> (<diagnostic>)
  keyword class-code: = "0A";
end class;

define open class <multiple-server-transaction> (<feature-not-supported>)
  keyword subclass-code: = "001";
end class;

define open class <integrity-constraint-violation> (<diagnostic>)
  keyword class-code: = "23";
end class;

define open class <invalid-authorization-specification> (<diagnostic>)
  keyword class-code: = "28";
end class;

define open class <invalid-catalog-name> (<diagnostic>)
  keyword class-code: = "3D";
end class;

define open class <invalid-character-set-name> (<diagnostic>)
  keyword class-code: = "2C";
end class;

define open class <invalid-condition-number> (<diagnostic>)
  keyword class-code: = "35";
end class;

define open class <invalid-cursor-name> (<diagnostic>)
  keyword class-code: = "34";
end class;

define open class <invalid-schema-name> (<diagnostic>)
  keyword class-code: = "3F";
end class;

define open class <invalid-sql-descriptor-name> (<diagnostic>)
  keyword class-code: = "33";
end class;

define open class <invalid-sql-statement-name> (<diagnostic>)
  keyword class-code: = "26";
end class;

define open class <invalid-transaction-state> (<diagnostic>)
  keyword class-code: = "25";
end class;

define open class <invalid-transaction-termination> (<diagnostic>)
  keyword class-code: = "2D";
end class;

define open class <no-data> (<diagnostic>)
  keyword class-code: = "02";
end class;

define open class <remote-database-access> (<diagnostic>)
  keyword class-code: = "HZ";
end class;

define open class <successful-completion> (<diagnostic>)
  keyword class-code: = "00";
end class;

define open class <syntax-error-or-access-rule-violation> (<diagnostic>)
  keyword class-code: = "42";
end class;

define open class
     <syntax-error-or-access-rule-violation-in-direct-sql-statement> 
     (<diagnostic>)
  keyword class-code: = "2A";
end class;

define open class
    <syntax-error-or-access-rule-violation-in-dynamic-sql-statement> 
    (<diagnostic>)
  keyword class-code: = "37";
end class;

define open class <transaction-rollback> (<diagnostic>)
  keyword class-code: = "40";
end class;

define open class <transaction-rollback-due-to-integrity-constraint-violation>
    (<transaction-rollback>)
  keyword subclass-code: = "002";
end class;

define open class <transaction-rollback-due-to-serialization-failure>
    (<transaction-rollback>)
  keyword subclass-code: = "001";
end class;

define open class <statement-completion-unknown> (<transaction-rollback>)
  keyword subclass-code: = "003";
end class;

define open class <triggered-data-change-violation> (<diagnostic>)
  keyword class-code: = "27";
end class;

define open class <sql-warning> (<diagnostic>)
  keyword class-code: = "01";
end class;

define open class <warning-cursor-operation-conflict> (<sql-warning>)
  keyword subclass-code: = "001";
end class;

define open class <disconnect-error> (<sql-warning>)
  keyword subclass-code: = "002";
end class;

define open class <implicit-zero-bit-padding> (<sql-warning>)
  keyword subclass-code: = "008";
end class;

define open class <insufficient-item-descriptor-areas> (<sql-warning>)
  keyword subclass-code: = "005";
end class;

define open class <null-value-eliminated-in-set-function> (<sql-warning>)
  keyword subclass-code: = "003";
end class;

define open class <privilege-not-granted> (<sql-warning>)
  keyword subclass-code: = "007";
end class;

define open class <privilege-not-revoked> (<sql-warning>)
  keyword subclass-code: = "006";
end class;

define open class <query-expression-too-long-for-information-schema> 
    (<sql-warning>)
  keyword subclass-code: = "00A";
end class;

define open class <search-condition-too-long-for-information-schema> 
    (<sql-warning>)
  keyword subclass-code: = "009";
end class;

define open class <warning-string-data-right-truncation> (<sql-warning>)
  keyword subclass-code: = "004";
end class;

define open class <with-check-option-violation> (<diagnostic>)
  keyword class-code: = "44";
end class;


//--------------------  Diagnostic Table  --------------------


define class <diagnostic-table> (<object>)
  constant slot diagnostics :: <object-table> = make(<object-table>);
  constant slot general-key :: <symbol>,
    required-init-keyword: general-key:;

  slot diagnostics-installed? :: <boolean> = #f;

  constant slot installation-functions :: <deque> = make(<deque>);
end class <diagnostic-table>;


define constant $general-dbms = #"general-dbms";

define constant $diagnostic-table :: <diagnostic-table> 
    = make(<diagnostic-table>, general-key: $general-dbms);



define function register-diagnostic-installer
    (function :: <function>) => ()
  push-last($diagnostic-table.installation-functions, function)
end function register-diagnostic-installer;

define function install-diagnostics
    (table :: <diagnostic-table>) => ()
  install-general-diagnostics(table);
  for (fn in table.installation-functions)
    fn(table);
  end for;  
end function;

define function install-diagnostic-key
    (key :: <symbol>) => ()
  $diagnostic-table.diagnostics[key] := make(<string-table>);
end function install-diagnostic-key;

define function install-diagnostic
    (table :: <diagnostic-table>, class :: subclass(<diagnostic>),
     #key key :: <symbol> = table.general-key)
 => ()
  let diagnostic = make(class);
  let sqlstate = concatenate(diagnostic.class-code, diagnostic.subclass-code);
  table.diagnostics[key][sqlstate] := class;
end function install-diagnostic;

define function install-general-diagnostics(table :: <diagnostic-table>) => ()
  debug-assert(found?(element(table.diagnostics, table.general-key, default: $unfound)),
               "There is no subset of diagnostic table for a general dbms.");

  install-diagnostic(table, <ambiguous-cursor-name>);
  install-diagnostic(table, <cardinality-violation>);
  install-diagnostic(table, <connection-exception>);
  install-diagnostic(table, <connection-does-not-exist>);
  install-diagnostic(table, <connection-failure>);
  install-diagnostic(table, <connection-name-in-use>);
  install-diagnostic(table, <sql-client-unable-to-establish-connection>);
  install-diagnostic(table, <sql-server-rejected-establishment-of-connection>);
  install-diagnostic(table, <transaction-resolution-unknown>);
  install-diagnostic(table, <cursor-operation-conflict>);
  install-diagnostic(table, <character-not-in-repertoire>);
  install-diagnostic(table, <datetime-field-overflow>);
  install-diagnostic(table, <division-by-zero>);
  install-diagnostic(table, <error-in-assignment>);
  install-diagnostic(table, <indicator-overflow>);
  install-diagnostic(table, <interval-field-overflow>);
  install-diagnostic(table, <invalid-character-value-for-cast>);
  install-diagnostic(table, <invalid-datetime-format>);
  install-diagnostic(table, <invalid-escape-character>);
  install-diagnostic(table, <invalid-escape-sequence>);
  install-diagnostic(table, <invalid-fetch-sequence>);
  install-diagnostic(table, <invalid-parameter-value>);
  install-diagnostic(table, <invalid-time-zone-displacement-value>);
  install-diagnostic(table, <null-value-no-indicator-parameter>);
  install-diagnostic(table, <numeric-value-out-of-range>);
  install-diagnostic(table, <string-data-length-mismatch>);
  install-diagnostic(table, <string-data-right-truncation>);
  install-diagnostic(table, <substring-error>);
  install-diagnostic(table, <trim-error>);
  install-diagnostic(table, <unterminated-C-string>);
  install-diagnostic(table, <dependent-privilege-descriptors-still-exist>);
  install-diagnostic(table, <dynamic-sql-error>);
  install-diagnostic(table, <cursor-specification-cannot-be-executed>);
  install-diagnostic(table, <invalid-descriptor-count>);
  install-diagnostic(table, <invalid-descriptor-index>);
  install-diagnostic(table, <prepared-statement-not-a-cursor-specification>);
  install-diagnostic(table, <restricted-data-type-attribute-violation>);
  install-diagnostic(table, <using-clause-does-not-match-dynamic-parameter-specification>);
  install-diagnostic(table, <using-clause-does-not-match-target-specification>);
  install-diagnostic(table, <using-clause-required-for-dynamic-parameters>);
  install-diagnostic(table, <using-clause-required-for-result-fields>);
  install-diagnostic(table, <feature-not-supported>);
  install-diagnostic(table, <multiple-server-transaction>);
  install-diagnostic(table, <integrity-constraint-violation>);
  install-diagnostic(table, <invalid-authorization-specification>);
  install-diagnostic(table, <invalid-catalog-name>);
  install-diagnostic(table, <invalid-character-set-name>);
  install-diagnostic(table, <invalid-condition-number>);
  install-diagnostic(table, <invalid-cursor-name>);
  install-diagnostic(table, <invalid-schema-name>);
  install-diagnostic(table, <invalid-sql-descriptor-name>);
  install-diagnostic(table, <invalid-sql-statement-name>);
  install-diagnostic(table, <invalid-transaction-state>);
  install-diagnostic(table, <invalid-transaction-termination>);
  install-diagnostic(table, <no-data>);
  install-diagnostic(table, <remote-database-access>);
  install-diagnostic(table, <successful-completion>);
  install-diagnostic(table, <syntax-error-or-access-rule-violation>);
  install-diagnostic(table, 
    <syntax-error-or-access-rule-violation-in-direct-sql-statement>);
  install-diagnostic(table,  
    <syntax-error-or-access-rule-violation-in-dynamic-sql-statement>);
  install-diagnostic(table, <transaction-rollback>);
  install-diagnostic(table, <transaction-rollback-due-to-integrity-constraint-violation>);
  install-diagnostic(table, <transaction-rollback-due-to-serialization-failure>);
  install-diagnostic(table, <statement-completion-unknown>);
  install-diagnostic(table, <triggered-data-change-violation>);
  install-diagnostic(table, <sql-warning>);
  install-diagnostic(table, <warning-cursor-operation-conflict>);
  install-diagnostic(table, <disconnect-error>);
  install-diagnostic(table, <implicit-zero-bit-padding>);
  install-diagnostic(table, <insufficient-item-descriptor-areas>);
  install-diagnostic(table, <null-value-eliminated-in-set-function>);
  install-diagnostic(table, <privilege-not-granted>);
  install-diagnostic(table, <privilege-not-revoked>);
  install-diagnostic(table, <query-expression-too-long-for-information-schema>);
  install-diagnostic(table, <search-condition-too-long-for-information-schema>);
  install-diagnostic(table, <warning-string-data-right-truncation>);
  install-diagnostic(table, <with-check-option-violation>);
end function;


//--------------------  find-diagnostic not  --------------------
// Right now, the ODBC installs ODBC specific versions of the general
// diagnostic details and the ODBC library does not add anything 
// specific. Any overlap probably should be removed and this function
// should be modified to search for the diagnostic in the general 
// table in the event it isn't found in the specific table.

define function find-diagnostic
    (table :: <diagnostic-table>,
     diagnostic-set-key :: <object>,
     sqlstate :: <string>)
 => (diagnostic-detail-class :: <object>)
  if (table.diagnostics-installed? = #f)
    install-diagnostic-key(table.general-key);
    install-diagnostics(table);
    table.diagnostics-installed? := #t;
  end if;

  let subtable = element(table.diagnostics, diagnostic-set-key, default: $unfound);
  debug-assert(found?(subtable), 
               "Diagnostic table for key % not found.", diagnostic-set-key);

  let diag-class = element(subtable, sqlstate, default: $unfound);

  if (found?(diag-class))
    diag-class
  else
    let general-table = element(table.diagnostics, table.general-key, default: $unfound);
    assert(found?(general-table),
                 "The general diagnostic table was not found.");

    let diag-class = element(general-table, sqlstate, default: $unfound);

    if (found?(diag-class)) 
      diag-class;
    else 
      $unfound;
    end if;
  end if;
end function;






 
