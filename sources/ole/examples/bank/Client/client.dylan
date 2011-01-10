Module:    bank-client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// banks

define function com-check (status, context, instance)
  if (FAILED?(status))
    ole-error(status, context, instance)
  end
end;

define function ole-error-and-release(status, context, interface, #rest args)
  block ()
    apply(ole-error, status, context, interface, args);
  cleanup
    Release(interface);
  end;
end;

define method account-name (account :: <IBankAccount>) => (name :: <string>)
  let (status :: <HRESULT>, name :: <string>) = IBankAccount/name(account);
  com-check(status, "IBankAccount/name", account);
  // should be <unicode-string> when that is supported
  as(<byte-string>, name)
end method;

define method account-balance (account :: <IBankAccount>) => (balance :: <string>)
  let (status :: <HRESULT>, balance :: <integer>) = IBankAccount/balance(account);
  com-check(status, "IBankAccount/balance", account);
  integer-to-string(balance);
end method;

define method account-limit (account :: <IBankAccount>) => (limit :: <string>)
  "N/A"
end method;

define method account-limit (account :: <IBankCheckingAccount>) => (limit :: <string>)
  let (status :: <HRESULT>, limit :: <integer>) = IBankCheckingAccount/limit(account);
  com-check(status, "IBankCheckingAccount/limit", account);
  integer-to-string(limit);
end method;

define method credit-account (account :: <IBankAccount>, amount :: <integer>)
  let status :: <HRESULT> = IBankAccount/credit(account, amount);
  com-check(status, "IBankAccount/credit", account);
end;

define class <transaction-refused> (<error>)
  constant slot transaction-refused-reason, required-init-keyword: reason:;
end;

define method debit-account (account :: <IBankAccount>, amount :: <integer>)
  let (status, fail-reason) = IBankAccount/debit(account, amount);
  unless (SUCCEEDED?(status))
    if (status = $IBANK-E-REFUSE)
      error(make(<transaction-refused>, reason: fail-reason));
    else
      com-check(status, "IBankAccount/debit", account);
    end;
  end;
end method debit-account;

define class <duplicate-account> (<error>) end;

define method open-account (bank :: <IBank>, name :: <string>)
 => (account :: <IBankAccount>)
  let (status, unknown) = IBank/open-account(bank, name);
  if (SUCCEEDED?(status))
    let (status, account)
      = QueryInterface(unknown, $IID-<IBankAccount-provider>);
    if (SUCCEEDED?(status))
      Release(unknown);
      pointer-cast(<IBankAccount>, account)
    else
      ole-error-and-release(status, "QueryInterface", unknown);
    end;
  elseif (status = $IBANK-E-DUPLICATE-ACCOUNT)
    error(make(<duplicate-account>));
  else
    ole-error(status, "IBank/open-account", bank);
  end;
end open-account;

define method open-checking-account (bank :: <IBank>, name :: <string>, limit :: <integer>)
  => (account :: <IBankCheckingAccount>)
  let (status, unknown) = IBank/open-checking-account(bank, name, limit);
  if (SUCCEEDED?(status))
    let (status, account)
      = QueryInterface(unknown, $IID-<IBankCheckingAccount-provider>);
    if (SUCCEEDED?(status))
      Release(unknown);
      pointer-cast(<IBankCheckingAccount>, account)
    else
      ole-error-and-release(status, "QueryInterface", unknown);
    end;
  elseif (status = $IBANK-E-DUPLICATE-ACCOUNT)
    error(make(<duplicate-account>));
  else
    ole-error(status, "IBank/open-checking-account", bank);
  end;
end method open-checking-account;

define class <non-existent-account> (<error>) end;

define method retrieve-account (bank :: <IBank>, name :: <string>)
  => (account :: <IBankAccount>)
  let (status, unknown) = IBank/retrieve-account(bank, name);
  if (SUCCEEDED?(status))
    let (status, account)
      = QueryInterface(unknown, $IID-<IBankAccount-provider>);
    if (SUCCEEDED?(status))
      Release(unknown);
      let (status, checking-account)
	= QueryInterface(account, $IID-<IBankCheckingAccount-provider>);
      if (SUCCEEDED?(status))
	Release(account);
	pointer-cast(<IBankCheckingAccount>, checking-account)
      elseif (status == $E-NOINTERFACE)
	pointer-cast(<IBankAccount>, account)
      else
	ole-error-and-release(status, "QueryInterface", account);
      end;
    else
      ole-error-and-release(status, "QueryInterface", unknown);
    end;
  elseif (status = $IBANK-E-NON-EXISTENT-ACCOUNT)
    error(make(<non-existent-account>))
  else
    ole-error(status, "IBank/retrieve-account", bank);
  end;
end retrieve-account;

define method close-account (bank :: <IBank>, account :: <IBankAccount>) => ()
  let status = IBank/close-account(bank, account);
  com-check(status, "IBank/close-account", account);
end close-account;
  
