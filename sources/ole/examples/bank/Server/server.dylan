Module: bank-server
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define coclass $bank-component
  name "Bank";
  documentation "Dylan Bank Demo";
  uuid "{01B68F80-CCC9-11D2-A51C-00600808472F}";
  component-class <bank>;
  interface <bank>;
  // Tell a little white lie here -- these two interfaces aren't actually
  // supported by bank objects, they are supported by separate objects,
  // but this is the simplest way to get them into the type library...
  interface <account>;
  interface <checking-account>;
end;
// Dunno why this isn't in the coclass.
define constant $Bank-prog-ID = "HQN.SimpleBankDemo.1";


define COM-interface <bank> (<IBank-provider>)
   constant slot name :: <string> = "Dylan Bank";
   slot database-connection :: false-or(<connection>) = connect-to-database();
   constant slot server-frame  :: false-or(<server-frame>) = #f, init-keyword: server-frame:;
end <bank>;

define method terminate (bank :: <bank>) => ()
  // TODO: should free up all retrieved account objects?
  when (bank.database-connection)
    disconnect(bank.database-connection);
    bank.database-connection := #f;
  end;
  next-method();
end terminate;


define COM-interface <account> (<IBankAccount-provider>)
  constant slot bank :: <bank>, required-init-keyword: bank:;
  constant slot name :: <string>, required-init-keyword: name:;
end <account>;

define constant <account-or-null>
  = type-union(<account>, singleton($null-interface));

define COM-interface <checking-account> (<account>,
					 <IBankCheckingAccount-provider>)
end <checking-account>;

define constant <checking-account-or-null>
  = type-union(<checking-account>, singleton($null-interface));

define method bank-log (bank :: <bank>, #rest format-arguments)
  let message = apply(format-to-string, format-arguments);
  debug-message("Log: %s", message);
  when (bank.server-frame) log-message(bank.server-frame, message) end;
end bank-log;

define method bank-update (bank :: <bank>,
			   text :: <string>, #rest parameters)
 => ()
  database-execute(bank.database-connection, text, parameters);
  when (bank.server-frame) refresh(bank.server-frame) end;
end bank-update;

define method bank-value (bank :: <bank>,
			  text :: <string>, #rest parameters)
 => (value)
  let results = database-execute(bank.database-connection, text, parameters);
  if (empty?(results))
    #f
  else
    results[0][0]
  end;
end bank-value;

define method account-exists? (bank :: <bank>, name :: <string>)
 => (well? :: <boolean>)
  bank-value(bank, "select Name from Accounts where Name = ?", name) & #t
end method;


///////////////// IBankAccount methods

define method IBankAccount/name (account :: <account>)
  => (status :: <HRESULT>, name :: <string>)
  bank-log(account.bank, "Name enquiry on %s.", account.name);
  values($S-OK, account.name);
end IBankAccount/name;

define method IBankAccount/balance (account :: <account>) 
 => (status :: <HRESULT>, balance :: <integer>)
  bank-log(account.bank, "Balance enquiry on %s.", account.name);
  let value = bank-value(account.bank,
			 "select Balance from Accounts where Name = ?",
			 account.name);
  if (value)
    values($S-OK, value)
  else
    values($IBANK-E-NON-EXISTENT-ACCOUNT, 0);
  end;
end IBankAccount/balance;

define method IBankAccount/credit (account :: <account>, amount :: <integer>)
 => (status :: <HRESULT>)
  bank-log(account.bank, "Credit on %s by %d.", account.name, amount);
  if (amount >= 0)
    bank-update(account.bank,
                "update Accounts set Balance = Balance + ? where Name = ?",
                amount, account.name);
    $S-OK
  else
    $E-INVALIDARG
  end;
end IBankAccount/credit;

define method IBankAccount/debit (account :: <account>, amount :: <integer>)
 => (status :: <HRESULT>, fail-reason :: <string>)
  bank-log(account.bank, "Debit on %s by %d.", account.name, amount);
  if (amount >= 0)
    bank-update(account.bank,
	        "update Accounts set Balance = Balance - ?"
		  " where Name = ? and Balance >= ?",
	        amount, account.name, amount);
    values($S-OK, $null-string);
  else
    values($E-INVALIDARG, $null-string)
  end;
end IBankAccount/debit;

///////////////// IBankCheckingAccount methods

define method IBankCheckingAccount/limit (account :: <checking-account>)
 => (status :: <HRESULT>, limit :: <integer>)
  bank-log(account.bank, "Limit enquiry on %s.", account.name);
  let value = bank-value(account.bank,
			 "select Limit from Accounts where Name = ?",
			 account.name);
  if (value)
    values($S-OK, value)
  else
    values($IBANK-E-NON-EXISTENT-ACCOUNT, 0);
  end;
end IBankCheckingAccount/limit;

// Override the inherited method to check against Limit
define method IBankAccount/debit (account :: <checking-account>, amount :: <integer>)
 => (status :: <HRESULT>, fail-reason :: <string>)
  bank-log(account.bank, "Debit on %s by %d.", account.name, amount);
  if (amount >= 0)
    bank-update(account.bank,
	        "update Accounts set Balance = Balance - ? "
		  "where Name = ? and (Balance + Limit) >= ?",
	        amount, account.name, amount);
    values($S-OK, $null-string);
  else
    values($E-INVALIDARG, $null-string)
  end;
end IBankAccount/debit;

///////////////// IBank methods on <bank>

define method IBank/name (bank :: <bank>)
 => (status :: <HRESULT>, name :: <string>)
  values($S-OK, bank.name);
end method IBank/name;
 
define method IBank/open-account (bank :: <bank>,
                                  account-name :: <string>)
 => (status :: <HRESULT>, account :: <account-or-null>)
  bank-log(bank, "Open account on %s for %s.", bank.name, account-name);
  if (account-exists?(bank, account-name))
    values($IBANK-E-DUPLICATE-ACCOUNT, $null-interface)
  else
    // Arguments are dynamic extent, so have to copy.
    // Should make it be <unicode-string> once that's fully supported.
    let account-name = as(<byte-string>, account-name);
    bank-update(bank,
		"insert into Accounts(Name, Balance, Limit) values(?, ?, ?)",
		account-name, 0, $null-value);
    let account = make(<account>, bank: bank, name: account-name);
    AddRef(account);
    values($S-OK, account)
  end if;
end method IBank/open-account;

define method IBank/open-checking-account (bank :: <bank>,
                                           account-name :: <string>,
                                           limit :: <integer>)
 => (status :: <HRESULT>, account :: <checking-account-or-null>)
  // Arguments are dynamic extent, so have to copy.
  // Should make it be <unicode-string> once that's fully supported.
  let account-name = as(<byte-string>, account-name);
  bank-log(bank, "Open checking account on %s for %s with limit %d.",
	   bank.name, account-name, limit);
  if (account-exists?(bank, account-name))
    values($IBANK-E-DUPLICATE-ACCOUNT, $null-interface);
  elseif (limit < 0)
    values($E-INVALIDARG, $null-interface);
  else
    bank-update(bank,
		"insert into Accounts(Name, Balance, Limit) values(?, ?, ?)",
		account-name, 0, limit);
    let account = make(<checking-account>, bank: bank, name: account-name);
    AddRef(account);
    values($S-OK, account);
  end if;
end method IBank/open-checking-account;

define method IBank/retrieve-account (bank :: <bank>,
                                      account-name :: <string>)
 => (status :: <HRESULT>, account :: <account-or-null>)
  // Arguments are dynamic extent, so have to copy.
  // Should make it be <unicode-string> once that's fully supported.
  let account-name = as(<byte-string>, account-name);
  bank-log(bank, "Retrieve account on %s for %s.", bank.name, account-name);
  let value = bank-value(bank,
			 "select Limit from Accounts where Name = ?",
			 account-name);
  if (~value)
    values($IBANK-E-NON-EXISTENT-ACCOUNT, $null-interface);
  elseif (~instance?(value, <null-value>))
    let account = make(<checking-account>, bank: bank, name: account-name);
    AddRef(account);
    values($S-OK, account)
  else
    let account = make(<account>, bank: bank, name: account-name);
    AddRef(account);
    values($S-OK, account)
  end;
end method IBank/retrieve-account;

define method IBank/close-account (bank :: <bank>, account :: <account>)
 => (status :: <HRESULT>)
  bank-log(bank,"Close account on %s for %s.", bank.name, account.name);
  bank-update(bank,"delete from Accounts where Name = ?", account.name);
  $S-OK
end method IBank/close-account;

