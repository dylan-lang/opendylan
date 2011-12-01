Module: dylan-user
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sql-odbc
  use dylan;
  use common-dylan;
  use io;
  use system;
  use generic-arithmetic;
  use sql, 
    import: {sql};
  use odbc-ffi, 
    import: {odbc-ffi};
  use c-ffi, export: {$sql-accessible-procedures,
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
		      $sql-xopen-cli-year,
		      $sql-active-environments,
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
		      $sql-txn-capable,
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
		      $sql-union};
		      	
  export sql-odbc;
end library;


define module sql-odbc
  use dylan;
  use threads;
  use sql, import: all, export: all;
  use odbc-ffi, 
    import: { *trace-odbc-functions* },
    export: { *trace-odbc-functions* };

  // Debugging functions used to determine if finalization is occuring.
  // Once the gc limitation on weak tables is fixed, these functions
  // will no longer be needed.
  create  
    set-finalize-notification,
    notify-of-finalization;

  create
    assert-odbc-goodness,
    report-condition,
    *odbc-print-condition*,
    *odbc-report-success-with-info*,
    *odbc-report-<data-not-available>*;

  create
    <odbc-low-level-error>,
    low-level-error-sqlstate;

  create
    <odbc-dbms>,
    %environment-handle,
    <odbc-user>,
    user-name, user-name-setter,
    password, password-setter,
    <odbc-database>,
    datasource-name, datasource-name-setter,
    <odbc-connection>,
    %connection-handle;  //++ Why is this exported?

  create
    <odbc-sql-statement>;

  create
    <odbc-result-set>,
    <odbc-forward-only-result-set>,
    <odbc-scrollable-result-set>,
    <odbc-record>;

  create     
    <odbc-catalog>, 
    <odbc-schema>, 
    <odbc-table>, 
    <odbc-column>,
    <odbc-index>;

  create
    <odbc-diagnostic-problem>,
    problem-diagnostic,
    diagnostic-return-code,

    <odbc-diagnostic-error>,
    <odbc-diagnostic-warning>,

    <odbc-diagnostic>,
    server-name,
    row-number,
    column-number,

    <odbc-connection-exception>,
    connection-exception-user,
    connection-exception-database,
    connection-exception-dbms,

    <odbc-invalid-connection-string-attribute>,
    <odbc-error-in-row>,
    <odbc-option-value-changed>,
    <odbc-attempt-to-fetch-before-result-set-returned-the-first-rowset>,
    <odbc-fractional-truncation>,
    <odbc-error-saving-file-dsn>,
    <odbc-invalid-keyword>,
    <odbc-invalid-use-of-default-parameter>,
    <odbc-communication-link-failure>,
    <odbc-insert-value-list-does-not-match-column-list>,
    <odbc-invalid-cursor-state>,
    <odbc-transaction-state>,
    <odbc-transaction-still-active>,
    <odbc-transaction-is-rolledback>,
    <odbc-base-table-or-view-already-exists>,
    <odbc-base-table-or-view-not-found>,
    <odbc-index-already-exists>,
    <odbc-index-not-found>,
    <odbc-column-already-exists>,
    <odbc-column-not-found>,
    <odbc-general-error>,
    <odbc-memory-allocation-error>,
    <odbc-invalid-application-buffer-type>,
    <odbc-invalid-sql-data-type>,
    <odbc-associated-statement-is-not-prepared>,
    <odbc-operation-canceled>,
    <odbc-invalid-use-of-null-pointer>,
    <odbc-function-sequence-error>,
    <odbc-attribute-cannot-be-set-now>,
    <odbc-invalid-transaction-operation-code>,
    <odbc-memory-management-error>,
    <odbc-limit-on-the-number-of-handles-exceeded>,
    <odbc-no-cursor-name-available>,
    <odbc-cannot-modify-an-implementation-row-descriptor>,
    <odbc-invalid-use-of-an-atumatically-allocated-descriptor-handle>,
    <odbc-server-declined-cancel-request>,
    <odbc-non-character-and-non-binary-data-sent-in-pieces>,
    <odbc-attempt-to-concatenate-a-null-value>,
    <odbc-inconsistent-descriptor-information>,
    <odbc-invalid-attribute-value>,
    <odbc-invalid-string-or-buffer-length>,
    <odbc-invalid-descriptor-field-identifier>,
    <odbc-invalid-attribute-option-identifier>,
    <odbc-invalid-parameter-number>,
    <odbc-function-type-out-of-range>,
    <odbc-invalid-information-type>,
    <odbc-column-type-out-of-range>,
    <odbc-scope-type-out-of-range>,
    <odbc-nullable-type-out-of-range>,
    <odbc-uniqueness-option-type-out-of-range>,
    <odbc-accuracy-option-type-out-of-range>,
    <odbc-invalid-retrieval-code>,
    <odbc-invalid-precision-or-scale-value>,
    <odbc-invalid-parameter-type>,
    <odbc-fetch-type-out-of-range>,
    <odbc-row-value-out-of-range>,
    <odbc-invalid-cursor-position>,
    <odbc-invalid-driver-completion>,
    <odbc-invalid-bookmark-value>,
    <odbc-optional-feature-not-implemented>,
    <odbc-timeout-expired>,
    <odbc-connection-timeout-expired>,
    <odbc-driver-does-not-support-this-function>,
    <odbc-data-source-name-not-found>,
    <odbc-specified-driver-could-not-be-loaded>,
    <odbc-driver-SQLAllocHandle-on-SQL-HANDLE-ENV-failed>,
    <odbc-driver-SQLAllocHandle-on-SQL-HANDLE-DBC-failed>,
    <odbc-driver-SQLSetConnectAttr-failed>,
    <odbc-no-data-source-or-driver-specified>,
    <odbc-dialog-failed>,
    <odbc-unable-to-load-translation-dll>,
    <odbc-data-source-name-too-long>,
    <odbc-driver-name-too-long>,
    <odbc-DRIVER-keyword-syntax-error>,
    <odbc-invalid-name-of-file-DSN>,
    <odbc-corrupt-file-data-source>,
    <odbc-trace-file-error>;
end module;


define module sql-odbc-nice-layer
  //+++ Once the FFI is stable, I hope to do away with this module
  use dylan;
  use c-ffi;
  use sql;
  use odbc-ffi;
  use format-out;
  use dylan-direct-c-ffi; //++++ for hokey garbage collection lossage. Lose this.

  export
    <return-code>,
    <environment-handle>,
    <connection-handle>,
    <statement-handle>,
    <fetch-orientation>,
    <fetch-offset>;

  export
    $null-statement-handle,
    $null-environment-handle,
    $null-connection-handle;

  export
    null-odbc-field?,
    nice-SQLFreeEnv,
    nice-SQLError,
    nice-SQLConnect,
    nice-SQLDriverConnect,
    nice-SQLPrepare,
    nice-SQLExecute,
    nice-SQLExecDirect,
    nice-SQLFetch,
    nice-SQLFetchScroll,
    nice-SQLBindCol,
    nice-SQLAllocStmt,
    nice-SQLFreeStmt,
    nice-SQLAllocHandle,
    nice-SQLFreeHandle,
    nice-SQLNumResultCols,
    nice-SQLDescribeCol,
    nice-SQLTables,
    nice-SQLColumns,
    nice-SQLStatistics,
    nice-SQLGetConnectOption,
    nice-SQLGetConnectAttr,
    nice-SQLSetConnectAttr,
    nice-SQLGetDiagField,
    nice-SQLGetDiagRec,
    nice-SQLGetInfo,
    nice-SQLSetStmtAttr,
    nice-SQLSetPos;
end module;


define module sql-odbc-conditions
  use dylan;
  use format-out;
  use sql;
  use odbc-ffi;
  use sql-odbc-nice-layer;
  use sql-odbc;
end module;

define module sql-odbc-implementation
  use dylan;
  use generic-arithmetic, prefix: "big/";
  use dylan-extensions;
  use machine-words;
  use machine-word-lowlevel;

  use threads;
  use finalization;
  use c-ffi;
  use format-out;
  use format;
  use date;

  use dylan-primitives;

  use sql-odbc-nice-layer;
  use odbc-ffi;

  use sql;
  use sql-odbc;
end module;

