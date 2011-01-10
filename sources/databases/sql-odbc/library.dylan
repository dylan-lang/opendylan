Module:    dylan-user
Author:    eec
Version:   $(HopeName)
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library sql-odbc
  use functional-dylan;
  use generic-arithmetic;
  use sql, 
    import: {sql};
  use odbc-ffi, 
    import: {odbc-ffi};
  use io;
  use system;
  use c-ffi;

  export sql-odbc;
end library sql-odbc;

define module sql-odbc
  use sql, import: all, export: all;
  use odbc-ffi,
    import: { *trace-odbc-functions* },
    export: { *trace-odbc-functions* };

  // Debugging functions used to determine if finalization is occurring.
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
    <odbc-error>,
    <odbc-sql-warning>,
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
    <odbc-sql-statement>,
    %statement-handle;

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
  use functional-dylan;
  use c-ffi;
  use sql;
  use odbc-ffi;
  use format-out;
  use dylan-direct-c-ffi; //++++ for hokey garbage collection lossage. Lose this.

  export
    <return-code>,
    <environment-handle>,
    <connection-handle>,
    <statement-handle>;

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
    nice-SQLGetInfo;
end module;


define module sql-odbc-conditions
  use functional-dylan;
  use format-out;
  use sql;
  use odbc-ffi;
  use sql-odbc-nice-layer;
  use sql-odbc;
end module;

define module sql-odbc-implementation
  use functional-dylan;
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

  export <binding>,
         binding-name,
         sql-binding-info;
end module;

