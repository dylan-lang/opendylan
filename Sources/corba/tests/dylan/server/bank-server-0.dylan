Module: bank-server-0
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ACCOUNT IMPLEMENTATION

define class <demo-account> (<account-servant>)
  constant slot account/name :: corba/<string>, required-init-keyword: name:;
  slot account/balance :: corba/<float> = 0.0, init-keyword: balance:;
end class;

define method initialize (object :: <demo-account>, #key bank)
  next-method();
  let name = account/name(object);
  let id = as(<symbol>, name);
  let accounts = bank/accounts(bank);
  let existing-account = element(accounts, id, default: #f);
  if (existing-account)
    error(make(bank/<reject>, reason: "cannot create new account with same name"));
  end if;
  element(accounts, id) := object;
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  portableserver/poa/activate-object(root-poa, object);
end method;

define method account/makeLodgement (object :: <demo-account>, lodgement :: corba/<float>)
 => ()
  account/balance(object) := account/balance(object) + lodgement;
end method;

define method account/makeWithdrawal (object :: <demo-account>, withdrawl :: corba/<float>)
 => ()
  if (account/balance(object) < withdrawl)
    error("Warning: Account \"%s\" overdrawn.", account/name(object));
  end if;
  account/balance(object) := account/balance(object) - withdrawl;
end method;

/// CURRENTACCOUNT IMPLEMENTATION

define class <demo-currentaccount> (<currentaccount-servant>, <demo-account>)
  constant slot currentaccount/overdraftlimit :: corba/<float> = 0.0, init-keyword: limit:;
end class;

define method account/makeWithdrawal (object :: <demo-currentaccount>, withdrawl :: corba/<float>)
 => ()
  if ((account/balance(object) + currentaccount/overdraftlimit(object)) < withdrawl)
    error("ALERT: Account \"%s\" over agreed overdraft limit.", account/name(object));
  end if;
  next-method();
end method;

/// BANK IMPLEMENTATION

define class <demo-bank> (<bank-servant>)
  constant slot bank/accounts :: <table> = make(<table>);
end class;

define method bank/newAccount (object :: <demo-bank>, name :: corba/<string>)
 => (account :: <account>)
  make(<demo-account>, bank: object, name: name);
end method;

define method bank/newCurrentAccount (object :: <demo-bank>, name :: corba/<string>, limit :: corba/<float>)
 => (account :: <currentaccount>)
  make(<demo-currentaccount>, bank: object, name: name, limit: limit);
end method;

define method bank/deleteAccount (object :: <demo-bank>, reference :: <account>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let account = portableserver/poa/reference-to-servant(root-poa, reference);
  let id = as(<symbol>, account/name(account));
  remove-key!(bank/accounts(object), id);
  portableserver/poa/deactivate-object(root-poa,
				       portableserver/poa/reference-to-id(root-poa,
									  reference));
end method;

/// MAINLINE

define variable *bank-ior-file* :: <string> = "c:\\temp\\bank-0.ior";

define method start-bank-server ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a bank object (put it in a global for testing)
  let bank = make(<demo-bank>);

  // put it in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, bank);

  // create an ior string to pass to clients (via special variable
  // for the purposes of testing)
  corba/orb/object-to-file(orb, *bank-ior-file*, portableserver/poa/servant-to-reference(root-poa, bank));

  // flick the switch on the poa-manager flow control so its
  // receiver thread starts
  let manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(manager);

  // block until all ORB processing completed
  // corba/orb/run(orb);
end method;

register-server(start-bank-server);
