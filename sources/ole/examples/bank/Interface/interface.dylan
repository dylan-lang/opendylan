module: bank-interface
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open dual-interface <IBankAccount-provider> (<simple-dispatch>)
  client-class <IBankAccount>;
  name "IBankAccount";
  uuid "{836215C2-CCC6-11D2-A51C-00600808472F}";
  function IBankAccount/name () => (name :: <string>),
    name: "name";
  function IBankAccount/balance () => (balance :: <integer>),
    name: "balance";
  function IBankAccount/credit (amount :: <integer>) => (),
    name: "credit";
  function IBankAccount/debit (amount :: <integer>) => (fail-reason :: <string>),
    name: "debit";
end;

define open dual-interface <IBankCheckingAccount-provider> (<IBankAccount-provider>)
  client-class <IBankCheckingAccount> (<IBankAccount>);
  name "IBankCheckingAccount";
  uuid "{836215C1-CCC6-11D2-A51C-00600808472F}";
  function IBankCheckingAccount/limit () => (limit :: <integer>),
    name: "limit";
end;

define open dual-interface <IBank-provider> (<simple-dispatch>)
  client-class <IBank>;
  name "IBank";
  uuid "{836215C0-CCC6-11D2-A51C-00600808472F}";
  function IBank/name () => (name :: <string>),
    name: "name";
  function IBank/open-account (name :: <string>) => (account :: <Interface>),
    name: "openAccount";
  function IBank/open-checking-account (name :: <string>, limit :: <integer>)
                                             => (checking-acct :: <Interface>),
    name: "openCheckingAccount";
  function IBank/retrieve-account (name :: <string>)
                                             => (acct :: <Interface>),
    name: "retrieveAccount";
  function IBank/close-account (account :: <mapped-interface>) => (),
    name: "closeAccount";
end;

// Error codes
define constant $IBANK-E-REFUSE = MAKE-SCODE(1, $FACILITY-ITF, #x200);
define constant $IBANK-E-DUPLICATE-ACCOUNT = MAKE-SCODE(1, $FACILITY-ITF, #x201);
define constant $IBANK-E-NON-EXISTENT-ACCOUNT = MAKE-SCODE(1, $FACILITY-ITF, #x202);
