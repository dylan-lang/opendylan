Module:    bank-server
Synopsis:  The CORBA server of the bank example.
Author:    Claudio Russo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define class <bank-implementation> (BankingDemo/<bank-servant>)
   slot connection :: <connection>, required-init-keyword: connection:;
   constant slot poa :: PortableServer/<POA>, required-init-keyword: poa:;
   constant slot name :: CORBA/<string>, required-init-keyword: name:;
   constant slot server-frame  :: <server-frame>, required-init-keyword: server-frame:;
end class <bank-implementation>;

define  class <account-implementation> (BankingDemo/<account-servant>)
   constant slot bank :: <bank-implementation>, required-init-keyword: bank:;                
   constant slot name :: CORBA/<string>, required-init-keyword: name:;
end class <account-implementation>;

define class <checkingAccount-implementation> (<account-implementation>, BankingDemo/<checkingAccount-servant>)
end class <checkingAccount-implementation>;

define method existsAccount?
   (bank :: <bank-implementation>, name :: <string>)
     => (boolean :: <boolean>);
    with-connection(bank.connection)
      let query = make(<sql-statement>,
                       text: "select Name from Accounts "
                             "where Name = ?");
      let result-set = execute(query, parameters: vector(name));
      ~ (empty?(result-set));
    end with-connection;
end method existsAccount?;

define method BankingDemo/account/name (account :: <account-implementation>) 
 => (name :: CORBA/<string>)
    log-message(account.bank.server-frame, 
                format-to-string("Name enquiry on %s.", account.name));
     account.name;
end method BankingDemo/account/name;

define method BankingDemo/account/balance (account :: <account-implementation>) 
       => (balance :: CORBA/<long>)
    log-message(account.bank.server-frame, 
                format-to-string("Balance enquiry on %s.", account.name));
    with-connection(account.bank.connection)
      let query = make(<sql-statement>,
                       text: "select Balance  from Accounts "
                             "where Name = ?");
      let result-set = execute(query, parameters: vector(account.name));
      as(CORBA/<long>, result-set[0][0]);
    end with-connection;
end method BankingDemo/account/balance;



define method BankingDemo/account/credit (account :: <account-implementation>, amount :: CORBA/<unsigned-long>)
       => () 
    log-message(account.bank.server-frame, 
                format-to-string("Credit on %s by %d.", account.name, amount));
    with-connection(account.bank.connection)
      let amount = abs(amount);
      let query = make(<sql-statement>,
                       text: "update Accounts "
                             "set Balance = Balance + ? "
                             "where Name = ?");
      execute(query, parameters: vector(as(<integer>, amount), account.name));
    end with-connection;
        refresh(account.bank.server-frame);
end method BankingDemo/account/credit;

define method BankingDemo/account/debit (account :: <account-implementation>, amount :: CORBA/<long>)
       => ()
    log-message(account.bank.server-frame, 
                format-to-string("Debit on %s by %d.", account.name, amount));
    with-connection(account.bank.connection)
      let amount = abs(amount);
      let query = make(<sql-statement>,
                       text: "update Accounts "
                             "set Balance = Balance - ? "
                             "where Name = ? and Balance >= ?");
      execute(query, parameters: vector(as(<integer>, amount), account.name, as(<integer>, amount)));
    end with-connection;
    refresh(account.bank.server-frame);
end method BankingDemo/account/debit;

define method BankingDemo/checkingAccount/limit (account :: <checkingAccount-implementation>)
       => (limit :: CORBA/<long>)
    log-message(account.bank.server-frame, 
                format-to-string("Limit enquiry on %s.", account.name));
    with-connection(account.bank.connection)
      let query = make(<sql-statement>,
                       text: "select Limit from Accounts "
                             "where Name = ?");
      let result-set = execute(query, parameters: vector(account.name));
      as(CORBA/<long>, result-set[0][0]);
    end with-connection;
end method BankingDemo/checkingAccount/limit;

define method BankingDemo/account/debit (account :: <checkingAccount-implementation>, amount :: CORBA/<long>)
       => ()
    log-message(account.bank.server-frame, 
                format-to-string("Debit on %s by %d.", account.name, amount));
    with-connection(account.bank.connection)
      let amount = abs(amount);
      let query = make(<sql-statement>,
                       text: "update Accounts "
                             "set Balance = Balance - ? "
                             "where Name = ? and (Balance + Limit) >= ?");
      execute(query, parameters: vector(as(<integer>, amount), account.name, as(<integer>, amount)));
    end with-connection;
    refresh(account.bank.server-frame);
end method BankingDemo/account/debit;

define method BankingDemo/bank/name (bank :: <bank-implementation>) 
 => (name :: CORBA/<string>)
     bank.name;
end method BankingDemo/bank/name;
 
define method BankingDemo/bank/openAccount (bank :: <bank-implementation>, name :: CORBA/<string>)
       => (account :: BankingDemo/<account>)
       log-message(bank.server-frame, 
                format-to-string("Open account on %s for %s.", bank.BankingDemo/Bank/name, name));
       if (existsAccount?(bank, name))
           error (make(BankingDemo/bank/<duplicateAccount>));
       else    
           begin 
             with-connection(bank.connection)
	       let query = make(<sql-statement>,
                                text: "insert into Accounts(Name, Balance, Limit) "
			              "values(?, ?, ?)",
				input-indicator: #f);
	       execute(query, parameters: vector(name, 
 					         as(<integer>, 0),
                                                 #f));
             end with-connection;
             refresh(bank.server-frame);
             let new-account = make(<account-implementation>, bank: bank, name: name);
             as(BankingDemo/<account>, 
                PortableServer/POA/servant-to-reference(bank.poa, new-account));
           end;
       end if;
end method BankingDemo/bank/openAccount;

define method BankingDemo/bank/openCheckingAccount (bank :: <bank-implementation>, name :: CORBA/<string>, limit :: CORBA/<long>)
       => (checkingAccount :: BankingDemo/<checkingAccount>)
       log-message(bank.server-frame, 
                format-to-string("Open checking account on %s for %s with limit %d.", bank.BankingDemo/Bank/name, name, limit));
       if (existsAccount?(bank, name))
           error (make(BankingDemo/bank/<duplicateAccount>));
       else    
           begin 
             with-connection(bank.connection)
               let limit = abs(limit);
  	       let query = make(<sql-statement>,
		                text: "insert into Accounts(Name, Balance, Limit) "
  			              "values(?, ?, ?)",
		                input-indicator: #f);
	       execute(query, parameters: vector(name, 
		 	                         as(<integer>, 0),
                                                 as(<integer>, limit)));
	     end with-connection;
             refresh(bank.server-frame);
             let new-account = make(<checkingAccount-implementation>, bank: bank, name: name);
             as(BankingDemo/<checkingAccount>,
		PortableServer/POA/servant-to-reference(bank.poa, new-account));
           end;
       end if;
end method BankingDemo/bank/openCheckingAccount;

define method BankingDemo/bank/retrieveAccount (bank :: <bank-implementation>, name :: CORBA/<string>)
       => (account :: BankingDemo/<account>)
    log-message(bank.server-frame, 
                format-to-string("Retrieve account on %s for %s.", bank.BankingDemo/Bank/name, name));
    with-connection(bank.connection)
      let query = make(<sql-statement>,
                       text: "select Limit from Accounts "
                             "where Name = ?",
  		       output-indicator: #f);
      let result-set = execute(query, 
                               parameters: vector(name), 
                               result-set-policy: $scrollable-result-set-policy);
      if (empty? (result-set))
           error (make(BankingDemo/bank/<nonExistentAccount>));
      else if (result-set[0][0])
               as(BankingDemo/<checkingAccount>,
		  PortableServer/POA/servant-to-reference(bank.poa,
                                                           make(<checkingAccount-implementation>,
                                                                bank: bank,
                                                                name: name)));
	   else as(BankingDemo/<account>,
		   PortableServer/POA/servant-to-reference(bank.poa,
                                                           make(<account-implementation>,
                                                                bank: bank,
                                                                name: name)));
	   end if;
       end if;
    end with-connection;
end method BankingDemo/bank/retrieveAccount;

define method BankingDemo/bank/closeAccount (bank :: <bank-implementation>, account-reference :: BankingDemo/<account>)
       => ()
  let account = Portableserver/POA/reference-to-servant(bank.poa, account-reference);
  log-message(bank.server-frame, 
              format-to-string("Close account on %s for %s.", bank.BankingDemo/Bank/name, account.name));
  with-connection(bank.connection)
    let query = make(<sql-statement>,
                     text: "delete from Accounts "
                           "where Name = ?");
    execute(query, parameters: vector(account.name));
  end with-connection;
  refresh(bank.server-frame);
end method BankingDemo/bank/closeAccount;



