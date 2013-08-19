***************
The SQL library
***************

.. current-library:: sql
.. current-module:: sql

The SQL module
--------------

.. constant:: $default-coercion

.. constant:: $default-result-set-policy

.. constant:: $diagnostic-table

.. constant:: $no-coercion

.. constant:: $no-indicator

.. constant:: $null-value

.. constant:: $read-committed

.. constant:: $read-only

.. constant:: $read-uncommitted

.. constant:: $read-write

.. constant:: $repeatable-read

.. constant:: $scrollable-result-set-policy

.. constant:: $serializable

.. constant:: *all-connections*

.. constant:: *all-connections-lock*

.. class:: <ambiguous-cursor-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <assertion-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. class:: <cardinality-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <catalog-not-found>

   :superclasses: :class:`<database-object-not-found>`

   :keyword catalog-name:

.. class:: <catalog>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`, :class:`<result-set>`

   :keyword connection:

.. class:: <character-not-in-repertoire>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <check-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. constant:: <coercion-policy>

.. class:: <coercion-record>
   :open:
   :abstract:

   :superclasses: :class:`<record>`

   :keyword record-coercion-policy:

.. class:: <column>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`

   :keyword default-value:
   :keyword domain:
   :keyword nullable?:

.. class:: <connection-does-not-exist>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <connection-exception>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <connection-failure>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <connection-name-in-use>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <connection-not-specified>
   :open:

   :superclasses: ``<error>``


.. class:: <connection>
   :open:
   :abstract:

   :superclasses: ``<object>``

   :keyword dbms:

.. class:: <constraint>
   :abstract:

   :superclasses: :class:`<database-object>`


.. class:: <cursor-operation-conflict>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <cursor-specification-cannot-be-executed>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <data-exception>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <data-not-available>
   :open:

   :superclasses: ``<error>``


.. class:: <database-collection>
   :open:
   :abstract:

   :superclasses: ``<sequence>``


.. class:: <database-error>
   :open:
   :abstract:

   :superclasses: ``<error>``


.. class:: <database-object-not-found>
   :abstract:

   :superclasses: :class:`<diagnostic>`


.. class:: <database-object>
   :abstract:

   :superclasses: ``<object>``

   :keyword name:

.. class:: <database-statement>
   :open:
   :abstract:

   :superclasses: ``<object>``


.. class:: <database>
   :open:
   :abstract:

   :superclasses: ``<object>``


.. class:: <datetime-field-overflow>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <dbms-not-specified>
   :open:

   :superclasses: ``<error>``


.. class:: <dbms>
   :open:
   :abstract:

   :superclasses: ``<object>``


.. class:: <dependent-privilege-descriptors-still-exist>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <diagnostic-table>

   :superclasses: ``<object>``

   :keyword general-key:

.. class:: <diagnostic>
   :open:
   :abstract:

   :superclasses: ``<condition>``

   :keyword class-code:
   :keyword condition-number:
   :keyword subclass-code:

.. class:: <disconnect-error>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <division-by-zero>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <dynamic-sql-error>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <empty-result-set>
   :open:

   :superclasses: :class:`<result-set>`

   :keyword liaison:

.. class:: <error-in-assignment>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <feature-not-supported>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <forward-only-result-set>
   :open:
   :abstract:

   :superclasses: :class:`<result-set>`


.. class:: <implicit-zero-bit-padding>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <index>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`

   :keyword indexed-table:
   :keyword unique-index?:

.. class:: <indicator-overflow>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. constant:: <indicator-policy>

.. class:: <insufficient-item-descriptor-areas>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <integrity-constraint-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <interval-field-overflow>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-argument>
   :open:

   :superclasses: ``<error>``


.. class:: <invalid-authorization-specification>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-catalog-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-character-set-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-character-value-for-cast>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-condition-number>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-cursor-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-datatype-hint>
   :open:

   :superclasses: ``<warning>``

   :keyword datatype-hint:

.. class:: <invalid-datetime-format>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-descriptor-count>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <invalid-descriptor-index>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <invalid-escape-character>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-escape-sequence>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-fetch-sequence>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-parameter-value>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-schema-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-sql-descriptor-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-sql-statement-name>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-time-zone-displacement-value>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <invalid-transaction-state>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <invalid-transaction-termination>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. constant:: <isolation-level>

.. class:: <multiple-server-transaction>
   :open:

   :superclasses: :class:`<feature-not-supported>`

   :keyword subclass-code:

.. class:: <no-data>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <null-value-eliminated-in-set-function>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <null-value-no-indicator-parameter>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <null-value>
   :open:

   :superclasses: ``<object>``


.. class:: <numeric-value-out-of-range>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <prepared-statement-not-a-cursor-specification>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <privilege-not-granted>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <privilege-not-revoked>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <query-expression-too-long-for-information-schema>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <record>
   :open:
   :abstract:

   :superclasses: :class:`<database-collection>`

   :keyword indicator-policy:

.. class:: <referential-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. class:: <remote-database-access>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <restricted-data-type-attribute-violation>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <result-set-mutation-error>
   :open:

   :superclasses: ``<error>``


.. class:: <result-set-policy>
   :open:

   :superclasses: ``<object>``

   :keyword asynchronous:
   :keyword rowset-size:
   :keyword scroll-window:
   :keyword scrollable:

.. class:: <result-set>
   :open:
   :abstract:

   :superclasses: :class:`<database-collection>`

   :keyword liaison:

.. class:: <schema-not-found>

   :superclasses: :class:`<database-object-not-found>`

   :keyword schema-name:

.. class:: <schema>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`, :class:`<result-set>`


.. class:: <scrollable-result-set>
   :open:
   :abstract:

   :superclasses: :class:`<result-set>`


.. class:: <search-condition-too-long-for-information-schema>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <sql-bigint>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-binary>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-bit-varying>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-bit>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-character-varying>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-character>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-client-unable-to-establish-connection>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <sql-datatype>
   :open:
   :abstract:

   :superclasses: ``<object>``


.. class:: <sql-date>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-day-time-interval>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-decimal>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-double-precision>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-double>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-error>
   :open:
   :abstract:

   :superclasses: :class:`<database-error>`


.. class:: <sql-float>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-integer>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-longvarbinary>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-longvarchar>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-national-character-varying>
   :open:

   :superclasses: :class:`<sql-character-varying>`


.. class:: <sql-national-character>
   :open:

   :superclasses: :class:`<sql-character>`


.. class:: <sql-numeric>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-real>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-server-rejected-establishment-of-connection>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <sql-smallint>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-statement>
   :open:
   :abstract:

   :superclasses: :class:`<database-statement>`

   :keyword coercion-policy:
   :keyword datatype-hints:
   :keyword input-indicator:
   :keyword output-indicator:
   :keyword text:

.. class:: <sql-table>
   :open:
   :abstract:

   :superclasses: :class:`<database-object>`, :class:`<result-set>`


.. class:: <sql-time-with-time-zone>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-time>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-timestamp-with-time-zone>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-timestamp>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-tinyint>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-type-timestamp>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-unknown-type>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-unsupported-type>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-varbinary>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <sql-warning>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <sql-year-month-interval>
   :open:

   :superclasses: :class:`<sql-datatype>`


.. class:: <statement-completion-unknown>
   :open:

   :superclasses: :class:`<transaction-rollback>`

   :keyword subclass-code:

.. class:: <string-data-length-mismatch>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <string-data-right-truncation>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <substring-error>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <successful-completion>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <syntax-error-or-access-rule-violation-in-direct-sql-statement>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <syntax-error-or-access-rule-violation-in-dynamic-sql-statement>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <syntax-error-or-access-rule-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <table-not-found>

   :superclasses: :class:`<database-object-not-found>`

   :keyword table-name:

.. constant:: <transaction-mode>

.. class:: <transaction-resolution-unknown>
   :open:

   :superclasses: :class:`<connection-exception>`

   :keyword subclass-code:

.. class:: <transaction-rollback-due-to-integrity-constraint-violation>
   :open:

   :superclasses: :class:`<transaction-rollback>`

   :keyword subclass-code:

.. class:: <transaction-rollback-due-to-serialization-failure>
   :open:

   :superclasses: :class:`<transaction-rollback>`

   :keyword subclass-code:

.. class:: <transaction-rollback>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <transaction>
   :open:

   :superclasses: ``<object>``

   :keyword diagnostics-size:
   :keyword isolation-level:
   :keyword transaction-mode:

.. class:: <triggered-data-change-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. class:: <trim-error>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <unhandled-diagnostic>
   :open:

   :superclasses: :class:`<sql-error>`

   :keyword diagnostic:

.. class:: <unique-constraint>
   :abstract:

   :superclasses: :class:`<constraint>`


.. class:: <unknown-sqlstate>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:
   :keyword sqlstate:
   :keyword subclass-code:

.. class:: <unterminated-c-string>
   :open:

   :superclasses: :class:`<data-exception>`

   :keyword subclass-code:

.. class:: <user>
   :open:
   :abstract:

   :superclasses: ``<object>``


.. class:: <using-clause-does-not-match-dynamic-parameter-specification>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <using-clause-does-not-match-target-specification>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <using-clause-required-for-dynamic-parameters>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <using-clause-required-for-result-fields>
   :open:

   :superclasses: :class:`<dynamic-sql-error>`

   :keyword subclass-code:

.. class:: <warning-cursor-operation-conflict>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <warning-string-data-right-truncation>
   :open:

   :superclasses: :class:`<sql-warning>`

   :keyword subclass-code:

.. class:: <with-check-option-violation>
   :open:

   :superclasses: :class:`<diagnostic>`

   :keyword class-code:

.. generic-function:: acquire-null-value

   :signature: acquire-null-value (indicator index) => (null-value)

   :parameter indicator: An instance of ``<object>``.
   :parameter index: An instance of ``<integer>``.
   :value null-value: An instance of ``<object>``.

.. generic-function:: asynchronous

   :signature: asynchronous (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: catalog-from-name
   :open:

   :signature: catalog-from-name (connection name) => (catalog)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter name: An instance of ``<string>``.
   :value catalog: An instance of ``<catalog>``.

.. generic-function:: catalog-name
   :open:

   :signature: catalog-name (diag) => (catalog-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value catalog-name: An instance of ``<string>``.

.. generic-function:: catalogs
   :open:

   :signature: catalogs (#key connection) => (result-set)

   :parameter #key connection: An instance of :class:`<connection>`.
   :value result-set: An instance of :class:`<result-set>`.

.. generic-function:: catalogs-assist
   :open:

   :signature: catalogs-assist (connection) => (result-set)

   :parameter connection: An instance of :class:`<connection>`.
   :value result-set: An instance of :class:`<result-set>`.

.. generic-function:: class-code

   :signature: class-code (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: class-origin
   :open:

   :signature: class-origin (diag) => (class-origin)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value class-origin: An instance of ``<string>``.

.. generic-function:: coercion-policy

   :signature: coercion-policy (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: coercion-policy-setter

   :signature: coercion-policy-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: column-name
   :open:

   :signature: column-name (diag) => (column-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value column-name: An instance of ``<string>``.

.. generic-function:: command-function
   :open:

   :signature: command-function (diag) => (command-function)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value command-function: An instance of ``<string>``.

.. generic-function:: commit-transaction
   :open:

   :signature: commit-transaction (transaction) => ()

   :parameter transaction: An instance of :class:`<transaction>`.

.. generic-function:: condition-number

   :signature: condition-number (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: conditions-not-recorded?
   :open:

   :signature: conditions-not-recorded? (diag) => (not-recorded-status)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value not-recorded-status: An instance of ``<boolean>``.

.. generic-function:: connect
   :open:

   :signature: connect (database user) => (connection)

   :parameter database: An instance of :class:`<database>`.
   :parameter user: An instance of :class:`<user>`.
   :value connection: An instance of :class:`<connection>`.

.. generic-function:: connect-with-prompt
   :open:

   :signature: connect-with-prompt (dbms #key database user) => (connection)

   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #key database: An instance of ``false-or(<database>)``.
   :parameter #key user: An instance of ``false-or(<user>)``.
   :value connection: An instance of :class:`<connection>`.

.. generic-function:: connect-with-prompt?
   :open:

   :signature: connect-with-prompt? (dbms) => (connect-with-prompt-status)

   :parameter dbms: An instance of :class:`<dbms>`.
   :value connect-with-prompt-status: An instance of ``<boolean>``.

.. generic-function:: connection
   :open:

   :signature: connection (o) => (result)

   :parameter o: An instance of ``<object>``.
   :value result: An instance of :class:`<connection>`.

.. generic-function:: connection-name
   :open:

   :signature: connection-name (diag) => (connection-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value connection-name: An instance of ``<string>``.

.. generic-function:: connection-setter
   :open:

   :signature: connection-setter (c o) => (result)

   :parameter c: An instance of :class:`<connection>`.
   :parameter o: An instance of ``<object>``.
   :value result: An instance of :class:`<connection>`.

.. generic-function:: connections
   :open:

   :signature: connections (#key dbms) => (connection-sequence)

   :parameter #key dbms: An instance of ``false-or(<dbms>)``.
   :value connection-sequence: An instance of ``<sequence>``.

.. generic-function:: constraint-catalog
   :open:

   :signature: constraint-catalog (diag) => (constraint-catalog)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value constraint-catalog: An instance of ``<string>``.

.. generic-function:: constraint-name
   :open:

   :signature: constraint-name (diag) => (constraint-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value constraint-name: An instance of ``<string>``.

.. generic-function:: constraint-schema
   :open:

   :signature: constraint-schema (diag) => (constraint-schema)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value constraint-schema: An instance of ``<string>``.

.. generic-function:: constraints
   :open:

   :signature: constraints (db-object) => (result)

   :parameter db-object: An instance of ``<database-object>``.
   :value result: An instance of :class:`<result-set>`.

.. generic-function:: convert-value

   :signature: convert-value (coercion-policy value key) => (converted-value)

   :parameter coercion-policy: An instance of :class:`<coercion-policy>`.
   :parameter value: An instance of ``<object>``.
   :parameter key: An instance of ``<integer>``.
   :value converted-value: An instance of ``<object>``.

.. generic-function:: cursor-name
   :open:

   :signature: cursor-name (diag) => (cursor-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value cursor-name: An instance of ``<string>``.

.. generic-function:: database
   :open:

   :signature: database (connection) => (database)

   :parameter connection: An instance of :class:`<connection>`.
   :value database: An instance of :class:`<database>`.

.. generic-function:: database-object-name

   :signature: database-object-name (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: database-object-name-setter

   :signature: database-object-name-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: datatype-hints

   :signature: datatype-hints (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: datatype-hints-setter

   :signature: datatype-hints-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: dbms
   :open:

   :signature: dbms (connection) => (dbms)

   :parameter connection: An instance of :class:`<connection>`.
   :value dbms: An instance of :class:`<dbms>`.

.. generic-function:: dbms-name
   :open:

   :signature: dbms-name (dbms #key connection) => (dbms-name)

   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #key connection: An instance of :class:`<connection>`.
   :value dbms-name: An instance of ``<string>``.

.. generic-function:: dbms-version
   :open:

   :signature: dbms-version (dbms #key connection) => (dbms-version)

   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #key connection: An instance of :class:`<connection>`.
   :value dbms-version: An instance of ``<string>``.

.. generic-function:: default-connection

   :signature: default-connection () => (connection)

   :value connection: An instance of :class:`<connection>`.

.. generic-function:: default-conversion
   :open:

   :signature: default-conversion (value) => (converted-value)

   :parameter value: An instance of ``<object>``.
   :value converted-value: An instance of ``<object>``.

.. generic-function:: default-dbms

   :signature: default-dbms () => (dbms)

   :value dbms: An instance of :class:`<dbms>`.

.. generic-function:: default-diagnostics-size
   :open:

   :signature: default-diagnostics-size (connection) => (diagnostics-size)

   :parameter connection: An instance of :class:`<connection>`.
   :value diagnostics-size: An instance of ``<integer>``.

.. generic-function:: default-isolation-level
   :open:

   :signature: default-isolation-level (connection) => (level)

   :parameter connection: An instance of :class:`<connection>`.
   :value level: An instance of :class:`<isolation-level>`.

.. generic-function:: default-transaction-mode
   :open:

   :signature: default-transaction-mode (connection) => (mode)

   :parameter connection: An instance of :class:`<connection>`.
   :value mode: An instance of :class:`<transaction-mode>`.

.. generic-function:: default-value
   :open:

   :signature: default-value (column) => (default)

   :parameter column: An instance of :class:`<column>`.
   :value default: An instance of ``<object>``.

.. generic-function:: diagnostic-to-string
   :open:

   :signature: diagnostic-to-string (diag) => (string)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value string: An instance of ``<string>``.

.. generic-function:: diagnostics-size

   :signature: diagnostics-size (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: diagnostics-size-setter

   :signature: diagnostics-size-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: disconnect
   :open:

   :signature: disconnect (connection #key terminate-statements) => ()

   :parameter connection: An instance of :class:`<connection>`.
   :parameter #key terminate-statements: An instance of ``<boolean>``.

.. generic-function:: disconnect-all
   :open:

   :signature: disconnect-all (#key dbms) => ()

   :parameter #key dbms: An instance of ``false-or(<dbms>)``.

.. generic-function:: domain

   :signature: domain (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: dynamic-function
   :open:

   :signature: dynamic-function (diag) => (dynamic-function)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value dynamic-function: An instance of ``<string>``.

.. generic-function:: end-transaction
   :open:

   :signature: end-transaction (transaction) => ()

   :parameter transaction: An instance of :class:`<transaction>`.

.. generic-function:: environment-name
   :open:

   :signature: environment-name (diag) => (env-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value env-name: An instance of ``<string>``.

.. generic-function:: execute
   :open:

   :signature: execute (database-statement #key #all-keys) => (result-set)

   :parameter database-statement: An instance of ``type-union(<database-statement>, <string>)``.
   :value result-set: An instance of ``false-or(<result-set>)``.

.. generic-function:: fields

   :signature: fields (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: fields-setter

   :signature: fields-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. function:: find-diagnostic

   :signature: find-diagnostic (table diagnostic-set-key sqlstate) => (diagnostic-detail-class)

   :parameter table: An instance of :class:`<diagnostic-table>`.
   :parameter diagnostic-set-key: An instance of ``<object>``.
   :parameter sqlstate: An instance of ``<string>``.
   :value diagnostic-detail-class: An instance of ``<object>``.

.. generic-function:: indexed-table

   :signature: indexed-table (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: indexed-table-setter

   :signature: indexed-table-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: indexes
   :open:

   :signature: indexes (table) => (index-collection)

   :parameter table: An instance of :class:`<sql-table>`.
   :value index-collection: An instance of :class:`<result-set>`.

.. generic-function:: indicator-policy

   :signature: indicator-policy (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: input-indicator
   :open:

   :signature: input-indicator (sql-statement) => (input-indicator)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value input-indicator: An instance of :class:`<indicator-policy>`.

.. generic-function:: input-indicator-setter
   :open:

   :signature: input-indicator-setter (new-input-indicator sql-statement) => (new-input-indicator)

   :parameter new-input-indicator: An instance of :class:`<indicator-policy>`.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-input-indicator: An instance of :class:`<indicator-policy>`.

.. function:: install-diagnostic

   :signature: install-diagnostic (table class #key key) => ()

   :parameter table: An instance of :class:`<diagnostic-table>`.
   :parameter class: An instance of ``subclass(<diagnostic>)``.
   :parameter #key key: An instance of ``<symbol>``.

.. function:: install-diagnostic-key

   :signature: install-diagnostic-key (key) => ()

   :parameter key: An instance of ``<symbol>``.

.. generic-function:: installation-functions

   :signature: installation-functions (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: is-null?

   :signature: is-null? (record key) => (null-state)

   :parameter record: An instance of :class:`<record>`.
   :parameter key: An instance of ``<integer>``.
   :value null-state: An instance of ``<boolean>``.

.. generic-function:: isolation-level

   :signature: isolation-level (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: isolation-level-setter

   :signature: isolation-level-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: liaison

   :signature: liaison (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: liaison-setter

   :signature: liaison-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: make-dbms-specific
   :open:

   :signature: make-dbms-specific (type dbms #rest more-args) => (instance)

   :parameter type: An instance of ``<class>``.
   :parameter dbms: An instance of :class:`<dbms>`.
   :parameter #rest more-args: An instance of ``<object>``.
   :value instance: An instance of ``<object>``.

.. generic-function:: message-text
   :open:

   :signature: message-text (diag) => (message-text)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value message-text: An instance of ``<string>``.

.. generic-function:: multiple-connections?
   :open:

   :signature: multiple-connections? (dbms) => (multiple-connections-status)

   :parameter dbms: An instance of :class:`<dbms>`.
   :value multiple-connections-status: An instance of ``<boolean>``.

.. generic-function:: next-dbms-diagnostic
   :open:

   :signature: next-dbms-diagnostic (diag) => (next-diagnostic)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value next-diagnostic: An instance of ``false-or(<diagnostic>)``.

.. generic-function:: nullable?

   :signature: nullable? (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: output-indicator
   :open:

   :signature: output-indicator (sql-statement) => (output-indicator)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value output-indicator: An instance of :class:`<indicator-policy>`.

.. generic-function:: output-indicator-setter
   :open:

   :signature: output-indicator-setter (new-output-indicator sql-statement) => (new-output-indicator)

   :parameter new-output-indicator: An instance of :class:`<indicator-policy>`.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-output-indicator: An instance of :class:`<indicator-policy>`.

.. generic-function:: possible-explanation

   :signature: possible-explanation (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: record-available?
   :open:

   :signature: record-available? (result-set key) => (availability)

   :parameter result-set: An instance of :class:`<result-set>`.
   :parameter key: An instance of ``<integer>``.
   :value availability: An instance of ``<boolean>``.

.. generic-function:: record-coercion-policy

   :signature: record-coercion-policy (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. function:: register-diagnostic-installer

   :signature: register-diagnostic-installer (function) => ()

   :parameter function: An instance of ``<function>``.

.. generic-function:: returned-sqlstate
   :open:

   :signature: returned-sqlstate (diag) => (sqlstate)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value sqlstate: An instance of ``<string>``.

.. generic-function:: rollback-transaction
   :open:

   :signature: rollback-transaction (transaction) => ()

   :parameter transaction: An instance of :class:`<transaction>`.

.. generic-function:: row-count
   :open:

   :signature: row-count (diag) => (count)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value count: An instance of ``<integer>``.

.. generic-function:: rowset-size

   :signature: rowset-size (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: schema-from-name
   :open:

   :signature: schema-from-name (connection catalog-name schema-name) => (schema)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter catalog-name: An instance of ``<string>``.
   :parameter schema-name: An instance of ``<string>``.
   :value schema: An instance of :class:`<schema>`.

.. generic-function:: schema-name
   :open:

   :signature: schema-name (diag) => (schema-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value schema-name: An instance of ``<string>``.

.. generic-function:: scroll-window

   :signature: scroll-window (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: scrollable?

   :signature: scrollable? (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. macro:: sql

.. generic-function:: start-transaction
   :open:

   :signature: start-transaction (connection transaction-mode isolation-level diagnostics-size) => (transaction)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter transaction-mode: An instance of :class:`<transaction-mode>`.
   :parameter isolation-level: An instance of :class:`<isolation-level>`.
   :parameter diagnostics-size: An instance of ``<integer>``.
   :value transaction: An instance of :class:`<transaction>`.

.. generic-function:: statement-column-names
   :open:

   :signature: statement-column-names (statement) => (column-names)

   :parameter statement: An instance of :class:`<sql-statement>`.
   :value column-names: An instance of ``<sequence>``.

.. generic-function:: subclass-code

   :signature: subclass-code (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: subclass-origin
   :open:

   :signature: subclass-origin (diag) => (subclass-origin)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value subclass-origin: An instance of ``<string>``.

.. generic-function:: table-from-name
   :open:

   :signature: table-from-name (connection catalog-name schema-name table-name) => (table)

   :parameter connection: An instance of :class:`<connection>`.
   :parameter catalog-name: An instance of ``<string>``.
   :parameter schema-name: An instance of ``<string>``.
   :parameter table-name: An instance of ``<string>``.
   :value table: An instance of :class:`<sql-table>`.

.. generic-function:: table-name
   :open:

   :signature: table-name (diag) => (table-name)

   :parameter diag: An instance of :class:`<diagnostic>`.
   :value table-name: An instance of ``<string>``.

.. generic-function:: text
   :open:

   :signature: text (sql-statement) => (sql-statement-text)

   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value sql-statement-text: An instance of ``<string>``.

.. generic-function:: text-setter
   :open:

   :signature: text-setter (new-text sql-statement) => (new-text)

   :parameter new-text: An instance of ``<string>``.
   :parameter sql-statement: An instance of :class:`<sql-statement>`.
   :value new-text: An instance of ``<string>``.

.. generic-function:: transaction-mode

   :signature: transaction-mode (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: transaction-mode-setter

   :signature: transaction-mode-setter (value object) => (#rest results)

   :parameter value: An instance of ``<object>``.
   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: unique-index?

   :signature: unique-index? (object) => (#rest results)

   :parameter object: An instance of ``<object>``.
   :value #rest results: An instance of ``<object>``.

.. generic-function:: user
   :open:

   :signature: user (connection) => (user)

   :parameter connection: An instance of :class:`<connection>`.
   :value user: An instance of :class:`<user>`.

.. macro:: with-connection

.. macro:: with-database

.. macro:: with-dbms

.. macro:: with-transaction
