Module: dylan-user
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sql
  use dylan;
  use common-dylan;
  use generic-arithmetic;
  use c-ffi;
  use io;
	
  export sql;
end library;

define module result-set
  create 
    <database-collection>,
        // Open abstract class

    <result-set>,
        // Open abstract class
        // Parents: <database-collection>

    <empty-result-set>,
        // Open concrete class
        // Parents: <result-set>

    <forward-only-result-set>,
        // Open abstract class
        // Parents: <result-set>

    <scrollable-result-set>,
        // Open abstract class
        // Parents: <result-set>

    liaison,
        // GF: liaison result-set => function
        //       result-set  Instance of <result-set>.
        //       function    Instance of <function>.
        // Returns the liaison function associated with result-set.
        // The liaison function is invoked by SQL on each record
        // retrieved from the database. The results of the liaision 
        // function make up the content of the result-set.
        // Liaison signature: fn(record :: <record>) => (obj :: <object>)

    liaison-setter;
	
  create 
    <result-set-policy>,
        // Open concrete class
        // Parents: <object>

    rowset-size,
        // GF: rowset-size policy => size
        //       policy  Instance of <result-set-policy>.
        //       size    Instance of type-union(<integer>, singleton(#"all")).
        // Returns the rowset-size of policy.

    scrollable?,
        // GF: scrollable? policy => scrollable?
        //       policy      Instance of <result-set-policy>.
        //       scrollable? Instance of <boolean>.
        // Returns the scrollable indicator of policy.

    scroll-window,
        // GF: scroll-window policy => window-size
        //       policy      Instance of <result-set-policy>.
        //       window-size Instance of <integer>.
        // Returns the scroll-window size of policy.

    asynchronous,
        // GF: asynchronous policy => async?
        //       policy  Instance of <result-set-policy>.
        //       async?  Instance of <boolean>.
        // Returns the asynchronous indicator of policy.

    $default-result-set-policy,
        // Constant Module Variable: <result-set-policy>

    $scrollable-result-set-policy,
        // Constant Module Variable: <result-set-policy>

    record-available?;
        // GF: record-available? result-set index => available?
        //       result-set  Instance of <result-set>.
        //       index       Instance of <integer>.
        //       available?  Instance of <boolean>.
        // Indicates if record with key index is available without blocking.
        // Useful if result-set is retrieving records asynchronously.

  create 
    <record>,
        // Open abstract class
        // Parents: <database-collection>

    <coercion-record>,
        // Open abstract class
        // Parents: <record>

    is-null?,
        // GF: is-null? record column-key => null-state
        //       record      Instance of <record>.
        //       column-key  Instance of <integer>.
        //       null-state  Instance of <boolean>.
        // Returns a null-value indication for the supplied column key.

    indicator-policy,
        // GF: indicator-policy record => policy
        //       record  Instance of <record>.
        //       policy  Instance of <indicator-policy>.
        // Returns the indicator policy of record. (This value is from the
        // SQL statement object supplied to the execute method.)

    record-coercion-policy;
        // GF: record-coercion-policy record => policy
        //       record  Instance of <record>.
        //       policy  Instance of <coercion-policy>.
        // Returns the coercion policy of record. (This value is from the
        // SQL statement object supplied to the execute method.)

  create
    <coercion-policy>,
        // Constant module variable
        // Type: type-union(singleton($default-conercion), <object>)

    default-conversion,
        // GF: default-conversion value => converted-value
        //       value            Instance of <object>.
        //       converted-value  Instance of <object>.
        // Performs the default conversion of value.
        // The SQL library provides a default conversion for C-FFI
        // objects to a Dylan object.

    $default-coercion,
        // Constant Module Variable: #"default-coercion"

    $no-coercion,
        // Constant Module Variable: #"no-coercion"
    
    convert-value,
        // GF: convert-value coercion-policy value key => converted-value
        //       coercion-policy  Instance of <coercion-policy>.
        //       value            Instance of <object>.
        //       key              Instance of <integer>.
        //       converted-value  Instance of <object>.
        // Converts value as dictated by coercion-policy indexed by key.

    acquire-null-value;
        // GF: acquire-null-value indicator index => null-value
        //       indicator   Instance of <object>.
        //       index       Instance of <integer>.
        //       null-value  Instance of <object>.
        // Returns the apporiate null-value from indicator as indicated
        // by the index.

  create
    $no-indicator,
        // Constant module variable: #"no-indicator"

    <indicator-policy>;
        // Constant module variable
        // type: type-union(singleton($no-indicator), <object>)
end module;	

define module sql
  use result-set, import: all, export: all;
  create
    <dbms>,
        // Open abstract class
        // Parents: <object>

    dbms-name,

    dbms-version,
 
    \with-dbms,
        // Statement macro: with-dbms(dbms) body end with-dbms;
        // Establishes dbms as the default dbms for the execution of body.
        // If, within the execution of body, the make method is invoked
        // on <user>, <database>, or <sql-statement>, dbms-specific instances
        // of these classes will be created.

    connections,
        // GF: connections #key dbms => seq
        //       dbms  An instance of false-or(<dbms>).
        //       seq   Instance of <sequence>.
        // Returns a sequence of all active connections against the supplied
        // dbms or a sequence of all active connections if dbms is #f.

    multiple-connections?,
        // GF: multiple-connections? dbms => status
        //       dbms    An instance of <dbms>.
        //       status  An instance of <boolean>.
        // Returns #t if multiple connections are supported on dbms otherwise,
        // #f is returned.

    disconnect-all;
        // GF: disconnect-all #key dbms => ()
        //       dbms  An instance of false-or(<dbms>).
        // Terminates all connections served by dbms. If dbms is #f, all
        // active connections are terminated.

  create
    <user>,
        // Open abstract class
        // Parents: <object>

    <database>,
        // Open abstract class
        // Parents: <object>

    connect,
        // GF: connect database user => connection
        //       database    Instance of <database>.
        //       user        Instance of <user>.
        //       connection  Instance of <connection>.
        // Establishes a connection to the given database for the supplied
        // user.

    connect-with-prompt,
        // GF: connect-with-prompt dbms #key database user => connection
        //       dbms        Instance of <dbms>.
        //       database    Instance of <database>.
        //       user        Instance of <user>.
        //       connection  Instance of <connection>.
        // Prompts the user for the information necessary to connect to a 
        // given database and establishes a connection to this database.
        // Use connect-with-prompt? to determine if this feature is supported.

    connect-with-prompt?;
        // GF: connect-with-prompt? dbms => promptable?
        //       dbms         Instance of <dbms>.
        //       promptable?  Instance of <boolean>.
        // Returns #t if dbms supports the connect-with-prompt feature.

  create
    <connection>,
        // Open abstract class
        // Parents: <object>

    dbms,
        // GF: dbms connection => dbms
        //       connection  Instance of <connection>.
        //       dbms        Instance of <dbms>.
        // Returns the dbms object associated with connection.

    database,
        // GF: database connection => database
        //       connection  Instance of <connection>.
        //       database    Instance of <database>.
        // Returns the database object associated with connection.

    user,
        // GF: user connection => user
        //       connection   Instance of <connection>.
        //       user         Instance of <dbms>.
        // Returns the user object associated with connection.

    disconnect;
        // GF: disconnect connection #key terminate-statements => ()
        //       connection            Instance of <connection>.
        //       terminate-statements  Instance of <boolean>.
        // Terminates the given connection. If any SQL statements are executing
        // asynchronously or a SQL-select statement has results pending, a 
        // condition will be signaled unless terminate-statements is #t.


  create
    default-dbms,
        // GF: default-dbms => dbms
        //       dbms  Instance of <dbms>.
        // Returns the dbms established by the with-dbms macro. Signals 
        // <dbms-not-specified> if no default dbms has be established.

    default-connection,
        // GF: default-connection => connection
        //       connection  Instance of <connection>.
        // Returns the connection established by the with-database or 
        // with-connection macro. Signals <connection-not-specified> if no
        // default connection has not been established.

    *all-connections*, 
        // For use in implementing DBMS-specific implementation of sql.
        // Clients of sql or any dbms-specific implementation of 
        // sql should not call this method.

    *all-connections-lock*,
        // For use in implementing DBMS-specific implementation of sql.

    \with-connection,
        // Statement Macro: with-connection(connection) body end
        // Within the dynamic scope of body, establishes connection as the
        // default connection. The result of the macro is the last expression
        // executed within body.

    \with-database,
        // Statement Macro: with-database(database, user) body end
        // This macro applies the connect function to its parameters and
        // establishes the resulting connection as the default connection
        // within the dynamic scope of body. The connection is terminated
        // when execution leaves the scope of body. The result of this macro
        // is the last expression in body executed.
 
    \sql,
        // Statement macro: NOT IMPLEMENTED!!

    make-dbms-specific;
        // For use in implementing DBMS-specific implementation of sql.
        // Clients of sql or any dbms-specific implementation of 
        // sql should not call this method.


  create
    <null-value>,
        // Open concrete class
        // Parents: <object>

    $null-value;
        // Constant Module Variable: <null-value>
        // The canonical null-value.

  create
    <database-statement>,
        // Open abstract class
        // Parents: <object>

    execute,
        // GF: execute statement #key all-keys => result-set
        //       statement   Instance of <database-statement>.
        //       result-set  Instance of false-or(<result-set>).
        // Sends the statement to the DBMS server for execution.


    <sql-statement>,
        // Open abstract class
        // Parents: <database-statement>

    statement-column-names,

    text,
        // GF: text statement => text
        //       statement   Instance of <sql-statement>.
        //       text        Instance of <string>.
        // Returns a string containing the text of statement

    text-setter,
        // GF: text-setter text statement => text
        //       text        Instance of <string>.
        //       statement   Instance of <sql-statement>.
        // Changes the text of statement

    output-indicator,
        // GF: output-indicator statement => indicators
        //       statement   Instance of <sql-statement>.
        //       indicators  Instance of <object>.
        // Returns the output-indicator for statement.

    output-indicator-setter,
        // GF: output-indicator-setter indicators statement => indicators
        //       indicators   Instance of <object>.
        //       statement    Instance of <sql-statement>.
        // Sets the output-indicator for statement to indicators.

    input-indicator,
        // GF: input-indicator statement => indicators
        //       statement   Instance of <sql-statement>.
        //       indicators  Instance of <object>.
        // Returns input-indicator for statement.

    input-indicator-setter,
        // GF: input-indicator-setter indicators statement => indicators
        //       indicators  Instance of <object>.
        //       statement   Instance of <sql-statement>.
        // Changes the input-indicator of statement to indicators.

    coercion-policy,
        // GF: coercion-policy statement => policy
        //       statement   Instance of <sql-statement>.
        //       policy      Instance of <coercion-policy>.
        // Returns the coercion-policy of statement.

    coercion-policy-setter,
        // GF: coercion-policy-setter policy statement => policy
        //       policy     Instance of <coercion-policy>.
        //       statement  Instance of <sql-statement>.
        // Changes the coercion-policy of statement to policy.

    datatype-hints,
        // GF: datatype-hints statement => hints
        //       statement   Instance of <sql-statement>.
        //       hints       Instance of <sequence>
        // Returns the datatype-hints of statement.

    datatype-hints-setter;
        // GF: datatype-hints-setter hints statement => hints
        //       hints      Instance of <sequence>.
        //       statement  Instance of <sql-statement>.
        // Changes the datatype-hints of statement.


  create
    // SQL specific conditions.
    <dbms-not-specified>,
        // Open concrete class
        // Parents: <error>

    <connection-not-specified>,
        // Open concrete class
        // Parents: <error>

    <data-not-available>,
        // Open concrete class
        // Parents: <error>

    <invalid-argument>,
        // Open concrete class
        // Parents: <error>

    <result-set-mutation-error>,
        // Open concrete class
        // Parents: <error>

    <invalid-datatype-hint>;
        // Open concrete class
        // Parents: <error>

  create
    <sql-datatype>,
    <sql-unknown-type>,
    <sql-unsupported-type>,
    <sql-integer>,
    <sql-smallint>,
    <sql-numeric>,
    <sql-decimal>,
    <sql-real>,
    <sql-double-precision>,
    <sql-float>,
    <sql-character>,
    <sql-character-varying>,
    <sql-national-character>,
    <sql-national-character-varying>,
    <sql-bit>,
    <sql-bit-varying>,
    <sql-date>,
    <sql-time>,
    <sql-timestamp>,
    <sql-time-with-time-zone>,
    <sql-timestamp-with-time-zone>,
    <sql-year-month-interval>,
    <sql-day-time-interval>,
    <sql-bigint>,
    <sql-binary>,
    <sql-double>,
    <sql-longvarbinary>,
    <sql-longvarchar>,
    <sql-tinyint>,
    <sql-varbinary>,
    <sql-type-timestamp>;

  create
    <database-object>,
    <constraint>,
    <unique-constraint>,
    <check-constraint>,
    <referential-constraint>,
    <assertion-constraint>,
    constraints,
    <catalog>,
    <column>,
    <index>,
    <schema>,
    <sql-table>,
    catalogs,
    catalogs-assist, // I don't like having to export this!
    column-name,
    default-value,
    domain,
    indexes,
    nullable?,
    unique-index?, 

    indexed-table, 
    indexed-table-setter, 
    fields, 
    fields-setter,

    connection, 
    connection-setter,
    database-object-name, 
    database-object-name-setter;

  create
    catalog-from-name,
    schema-from-name,
    table-from-name,
    <database-object-not-found>,
    <catalog-not-found>,
    <schema-not-found>,
    <table-not-found>;

  create
    rollback-transaction,
    commit-transaction,
    default-diagnostics-size,

    default-transaction-mode,
    default-isolation-level,
    default-diagnostics-size,

    \with-transaction;

  create
    <diagnostic>,
    <database-error>,
    <sql-error>,
    <unhandled-diagnostic>,
    class-code,
    subclass-code,
    <unknown-sqlstate>,
    conditions-not-recorded?,
    row-count,
    command-function,
    dynamic-function,
    condition-number,
    returned-sqlstate,
    class-origin,
    subclass-origin,
    constraint-catalog,
    constraint-schema,
    constraint-name,
    connection-name,
    environment-name,
    catalog-name,
    schema-name,
    table-name,
    column-name,
    cursor-name,
    message-text,
    possible-explanation,
    next-dbms-diagnostic,
    diagnostic-to-string;

  create
    <diagnostic-table>,
    install-diagnostic,
    install-diagnostic-key,
    installation-functions,
    register-diagnostic-installer,
    find-diagnostic,
    $diagnostic-table;

  create
    <ambiguous-cursor-name>,
    <cardinality-violation>,
    <connection-exception>,
    <connection-does-not-exist>,
    <connection-failure>,
    <connection-name-in-use>,
    <sql-client-unable-to-establish-connection> ,
    <sql-server-rejected-establishment-of-connection>,
    <transaction-resolution-unknown>,
    <cursor-operation-conflict>,
    <data-exception>,
    <character-not-in-repertoire>,
    <datetime-field-overflow>,
    <division-by-zero>,
    <error-in-assignment>,
    <indicator-overflow>,
    <interval-field-overflow>,
    <invalid-character-value-for-cast>,
    <invalid-datetime-format>,
    <invalid-escape-character>,
    <invalid-escape-sequence>,
    <invalid-fetch-sequence>,
    <invalid-parameter-value>,
    <invalid-time-zone-displacement-value>,
    <null-value-no-indicator-parameter>,
    <numeric-value-out-of-range>,
    <string-data-length-mismatch>,
    <string-data-right-truncation>,
    <substring-error>,
    <trim-error>,
    <unterminated-C-string>,
    <dependent-privilege-descriptors-still-exist>,
    <dynamic-sql-error>,
    <cursor-specification-cannot-be-executed>,
    <invalid-descriptor-count>,
    <invalid-descriptor-index>,
    <prepared-statement-not-a-cursor-specification>,
    <restricted-data-type-attribute-violation>,
    <using-clause-does-not-match-dynamic-parameter-specification>,
    <using-clause-does-not-match-target-specification>,
    <using-clause-required-for-dynamic-parameters>,
    <using-clause-required-for-result-fields>,
    <feature-not-supported>,
    <multiple-server-transaction>,
    <integrity-constraint-violation>,
    <invalid-authorization-specification>,
    <invalid-catalog-name>,
    <invalid-character-set-name>,
    <invalid-condition-number>,
    <invalid-cursor-name>,
    <invalid-schema-name>,
    <invalid-sql-descriptor-name>,
    <invalid-sql-statement-name>,
    <invalid-transaction-state>,
    <invalid-transaction-termination>,
    <no-data>,
    <remote-database-access>,
    <successful-completion>,
    <syntax-error-or-access-rule-violation>,
    <syntax-error-or-access-rule-violation-in-direct-sql-statement>,
    <syntax-error-or-access-rule-violation-in-dynamic-sql-statement>,
    <transaction-rollback>,
    <transaction-rollback-due-to-integrity-constraint-violation>,
    <transaction-rollback-due-to-serialization-failure>,
    <statement-completion-unknown>,
    <triggered-data-change-violation>,
    <sql-warning>,
    <warning-cursor-operation-conflict>,
    <disconnect-error>,
    <implicit-zero-bit-padding>,
    <insufficient-item-descriptor-areas>,
    <null-value-eliminated-in-set-function>,
    <privilege-not-granted>,
    <privilege-not-revoked>,
    <query-expression-too-long-for-information-schema>,
    <search-condition-too-long-for-information-schema>,
    <warning-string-data-right-truncation>,
    <with-check-option-violation>;

  create 
    <transaction>,
    transaction-mode,
    transaction-mode-setter,
    isolation-level,
    isolation-level-setter,
    diagnostics-size,
    diagnostics-size-setter;

  //++ Do not export to client applications.
  create
    start-transaction,
    end-transaction;

  create
    <transaction-mode>,
    $read-only,
    $read-write;

  create
    <isolation-level>,
    $read-uncommitted,
    $read-committed,
    $repeatable-read,
    $serializable;
end module;

define module result-set-implementation
  use dylan;
  use common-dylan;
  use generic-arithmetic, prefix: "big/";
  use dylan-extensions;
  use machine-words;
  use machine-word-lowlevel;

  use c-ffi;
  use format-out;
  use finalization;

  use sql;
  use result-set;
end module; 

define module sql-implementation
  use dylan;
  use common-dylan, exclude: { format-to-string };
  use threads;
  use finalization;
  use sql;
  use result-set;
  use format-out;
  use format;
end module sql-implementation;
