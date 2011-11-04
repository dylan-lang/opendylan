Module: sql-implementation
Author: eec
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define open abstract class <dbms> (<object>)
end class;

define open generic dbms-name(dbms :: <dbms>, #key connection :: <connection>)
 => (dbms-name :: <string>);

define open generic dbms-version(dbms :: <dbms>, #key connection :: <connection>)
 => (dbms-version :: <string>);


define open generic connections(#key dbms :: false-or(<dbms>))
 => (connection-sequence :: <sequence>);

define method connections(#key dbms: the-dbms :: false-or(<dbms>))
 => (connection-sequence :: <sequence>)
  with-lock(*all-connections-lock*)
    if (~the-dbms)
      *all-connections*;
    else
      choose(compose(curry(\=, the-dbms), dbms), *all-connections*);
    end if;
  end with-lock;
end method;

define open generic multiple-connections?(dbms :: <dbms>)
 => (multiple-connections-status :: <boolean>);


define open generic disconnect-all(#key dbms :: false-or(<dbms>))
 => ();

define method disconnect-all(#key dbms: the-dbms :: false-or(<dbms>))
 => ()
  for (a-connection in *all-connections*)
    if (~the-dbms | a-connection.dbms = the-dbms)
      disconnect(a-connection);
    end if;
  end for;
end method;
      

define open abstract class <user> (<object>)
end class;


define open abstract class <database> (<object>)
end class;


define open generic connect(database :: <database>, user :: <user>, #key)
 => (connection :: <connection>);


define open generic connect-with-prompt
    (dbms :: <dbms>, #key database :: false-or(<database>), 
     user :: false-or(<user>))
 => (connection :: <connection>);


define open generic connect-with-prompt?(dbms :: <dbms>)
 => (connect-with-prompt-status :: <boolean>);


define constant $read-only = #"read-only";
define constant $read-write = #"read-write";

define constant <transaction-mode> = one-of($read-only, $read-write);

define constant $read-uncommitted = #"read-uncommitted";
define constant $read-committed = #"read-committed";
define constant $repeatable-read = #"repeatable-read";
define constant $serializable = #"serializable";

define constant <isolation-level> = one-of($read-uncommitted, $read-committed,
					   $repeatable-read, $serializable);


define open concrete class <transaction> (<object>)
  slot transaction-mode :: <transaction-mode>,
    required-init-keyword: transaction-mode:;

  slot isolation-level :: <isolation-level>,
    required-init-keyword: isolation-level:;

  slot diagnostics-size :: <integer>,
    required-init-keyword: diagnostics-size:;
end class;


define open abstract class <connection> (<object>)
  constant slot dbms :: <dbms>,
    required-init-keyword: dbms:;
end class;


define open generic dbms(connection :: <connection>)
 => (dbms :: <dbms>);


define open generic database(connection :: <connection>)
 => (database :: <database>);


define open generic user(connection :: <connection>)
 => (user :: <user>);
    

define open generic disconnect
    (connection :: <connection>, #key terminate-statements :: <boolean>)
 => ();


define open concrete class <null-value> (<object>)
end class;

define constant $null-value :: <null-value> = make(<null-value>);

define open abstract class <database-statement> (<object>)
end class;


define open generic execute
    (database-statement :: type-union(<database-statement>, <string>), 
     #key, #all-keys)
 => (result-set :: false-or(<result-set>));

define method execute(sql-statement :: <string>, #rest more-args,
		      #key parameters :: <sequence> = #(),
		      coercion-policy :: <coercion-policy> = $default-coercion,
		      datatype-hints :: <sequence> = #())
 => (result-set :: false-or(<result-set>))
  let statement = make(<sql-statement>,
		       text: sql-statement,
		       coercion-policy: coercion-policy,
		       datatype-hints: datatype-hints);
  apply(execute, statement, more-args);
end method;


define open abstract class <sql-statement> (<database-statement>)
  slot text :: <string>,
    required-init-keyword: text:;

  slot input-indicator :: <object> = $null-value,
    init-keyword: input-indicator:;


  slot output-indicator :: <object> = $null-value,
    init-keyword: output-indicator:;

  slot coercion-policy :: <coercion-policy> = $default-coercion,
    init-keyword: coercion-policy:;

  slot datatype-hints :: <sequence> = #(),
    init-keyword: datatype-hints:;
end class;

define open generic statement-column-names
    (statement :: <sql-statement>)
 => (column-names :: <sequence>);

define open generic text(sql-statement :: <sql-statement>)
 => (sql-statement-text :: <string>);


define open generic text-setter
    (new-text :: <string>, sql-statement :: <sql-statement>)
 => (new-text :: <string>);


define open generic output-indicator(sql-statement :: <sql-statement>)
 => (output-indicator :: <indicator-policy>);


define open generic output-indicator-setter
    (new-output-indicator :: <indicator-policy>, 
     sql-statement :: <sql-statement>)
 => (new-output-indicator :: <indicator-policy>);


define open generic input-indicator(sql-statement :: <sql-statement>)
 => (input-indicator :: <indicator-policy>);


define open generic input-indicator-setter
    (new-input-indicator :: <indicator-policy>,
     sql-statement :: <sql-statement>)
 => (new-input-indicator :: <indicator-policy>);

