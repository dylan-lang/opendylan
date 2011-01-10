Module: bank-server-4
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ACCOUNT IMPLEMENTATION

define class <property-object> (<object>)
  slot object-properties :: <sequence> = #();
end class;

define method initialize (object :: <property-object>, #rest args, #key)
  next-method();
  object-properties(object) := args;
end method;

define constant <demo-account> = <property-object>;

define method account/name (account :: <demo-account>)
 => (name :: corba/<string>)
  get-property(object-properties(account), name:);
end method;
  
define method tie/account/balance (account :: <demo-account>)
 => (balance :: corba/<float>)
  get-property(object-properties(account), balance:, default: 0.0);
end method;

define method tie/account/balance-setter (new-balance :: corba/<float>, account :: <demo-account>)
 => (balance :: corba/<float>)
  put-property!(object-properties(account), balance:, new-balance);
  new-balance
end method;

define method initialize-account (object :: <demo-account>)
  let bank = get-property(object-properties(object), bank:);
  let name = account/name(object);
  let id = as(<symbol>, name);
  let accounts = bank/accounts(bank);
  let existing-account = element(accounts, id, default: #f);
  if (existing-account)
    error(make(bank/<reject>, reason: "cannot create new account with same name"));
  end if;
  element(accounts, id) := object;
  portableserver/poa/activate-object(*bank-poa*, make(<account-tie>, object: object));
end method;

define method tie/account/makeLodgement (object :: <demo-account>, lodgement :: corba/<float>)
 => ()
  tie/account/balance(object) := tie/account/balance(object) + lodgement;
end method;

define method tie/account/makeWithdrawal (object :: <demo-account>, withdrawl :: corba/<float>)
 => ()
  if (currentaccount?(object) & ((tie/account/balance(object) + tie/currentaccount/overdraftlimit(object)) < withdrawl))
    error("ALERT: Account \"%s\" over agreed overdraft limit.", account/name(object));
  end if;
  if (tie/account/balance(object) < withdrawl)
    error("Warning: Account \"%s\" overdrawn.", account/name(object));
  end if;
  tie/account/balance(object) := tie/account/balance(object) - withdrawl;
end method;

/// CURRENTACCOUNT IMPLEMENTATION

define constant <demo-currentaccount> = <property-object>;

define method tie/currentaccount/overdraftlimit (currentaccount :: <demo-currentaccount>)
 => (limit :: corba/<float>)
  get-property(object-properties(currentaccount), limit:, default: 0.0);
end method;

define method currentaccount? (object :: <demo-currentaccount>)
    get-property(object-properties(object), limit:, default: #f);
end method;

/// BANK IMPLEMENTATION

define constant <demo-bank> = <property-object>;

define method bank/accounts (bank :: <demo-bank>)
 => (account :: <table>)
  let accounts = get-property(object-properties(bank), accounts:, default: #f);
  accounts
    | begin
	let accounts = make(<table>);
	put-property!(object-properties(bank), accounts:, accounts);
	accounts;
      end;
end method;

define method tie/bank/newAccount (object :: <demo-bank>, name :: corba/<string>)
 => (account :: <account>)
  let account = make(<demo-account>, bank: object, name: name);
  initialize-account(account);
  as(<account>,
     portableserver/poa/servant-to-reference(*bank-poa*, make(<account-tie>, object: account)));
end method;

define method tie/bank/newCurrentAccount (object :: <demo-bank>, name :: corba/<string>, limit :: corba/<float>)
 => (account :: <currentaccount>)
  let currentaccount = make(<demo-currentaccount>, bank: object, name: name, limit: limit);
  initialize-account(currentaccount);
  as(<currentaccount>,
     portableserver/poa/servant-to-reference(*bank-poa*,
					     make(<currentaccount-tie>, object: currentaccount)));
end method;

define method tie/bank/deleteAccount (object :: <demo-bank>, reference :: <account>)
 => ()
  let servant = portableserver/poa/reference-to-servant(*bank-poa*, reference);
  let account = tied-object(servant);
  let id = as(<symbol>, account/name(account));
  remove-key!(bank/accounts(object), id);
  portableserver/poa/deactivate-object(*bank-poa*,
				       portableserver/poa/reference-to-id(*bank-poa*,
									  reference));
end method;

/// MAINLINE

define variable *bank-ior-file* :: <string> = "c:\\temp\\bank-4.ior";

define variable *bank-poa* = #f;

define method start-bank-server ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // next create a new POA and request receiver thread (but it is created
  // in a holding state)
  *bank-poa* := portableserver/poa/create-poa(root-poa, "Account POA 3", #f, lifespan-policy: #"transient");

  // actually make a bank object (put it in a global for testing)
  let bank = make(<bank-tie>, object: make(<demo-bank>));

  // put it in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(*bank-poa*, bank);

  // create an ior string to pass to clients (via special variable
  // for the purposes of testing)
  corba/orb/object-to-file(orb, *bank-ior-file*, portableserver/poa/servant-to-reference(*bank-poa*, bank));

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let poa-manager = portableserver/poa/the-poamanager(*bank-poa*);
  portableserver/poamanager/activate(poa-manager);

  // block until all ORB processing completed
  // corba/orb/run(orb);
end method;

register-server(start-bank-server);
