Module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library bank-interface
  use dylan;
  use ole-automation;

  export bank-interface;
end library;


define module bank-interface
  use dylan;
  use ole-automation;

  export <IBankAccount-provider>, <IBankAccount>,
    $IID-<IBankAccount-provider>,
    IBankAccount/name,
    IBankAccount/balance,
    IBankAccount/credit,
    IBankAccount/debit;

  export <IBankCheckingAccount-provider>, <IBankCheckingAccount>,
    $IID-<IBankCheckingAccount-provider>,
    IBankCheckingAccount/limit;

  export <IBank-provider>, <IBank>,
    $IID-<IBank-provider>,
    IBank/name,
    IBank/open-account,
    IBank/open-checking-account,
    IBank/retrieve-account,
    IBank/close-account;

  export
    $IBANK-E-REFUSE,
    $IBANK-E-DUPLICATE-ACCOUNT,
    $IBANK-E-NON-EXISTENT-ACCOUNT;

end module bank-interface;
