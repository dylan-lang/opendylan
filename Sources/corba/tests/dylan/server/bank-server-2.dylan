Module: bank-server-2
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

define method initialize (account :: <demo-account>, #key bank, reincarnate?)
  next-method();
  let name = account/name(account);
  let id = as(<symbol>, name);
  let accounts = bank/accounts(bank);
  let existing-account = element(accounts, id, default: #f);
  if (existing-account & ~reincarnate?)
    error(make(bank/<reject>, reason: "cannot create new account with same name"));
  end if;
  element(accounts, id) := pair(account-class(account), describe-account(account));
end method;

define method account-class (account :: <demo-account>)
  <demo-account>
end method;

define method describe-account (account :: <demo-account>)
 => (description :: <list>)
  list(name: account/name(account),
       balance: account/balance(account));
end method;

define method account/makeLodgement (account :: <demo-account>, lodgement :: corba/<float>)
 => ()
  account/balance(account) := account/balance(account) + lodgement;
end method;

define method account/makeWithdrawal (account :: <demo-account>, withdrawl :: corba/<float>)
 => ()
  if (account/balance(account) < withdrawl)
    error("Warning: Account \"%s\" overdrawn.", account/name(account));
  end if;
  account/balance(account) := account/balance(account) - withdrawl;
end method;

define class <object-bank-mixin> (<object>)
  constant slot object-bank :: <bank>, required-init-keyword: bank:;
end class;

define class <account-activator> (PortableServer/<ServantActivator>, <object-bank-mixin>)
end class;

define class <account-poa-activator> (PortableServer/<AdapterActivator>, <object-bank-mixin>)
end class;

define method PortableServer/ServantActivator/incarnate (activator :: <account-activator>,
							 objectID :: corba/<string>,
							 adapter :: portableserver/<poa>)
 => (servant :: PortableServer/<servant>)
  let bank = object-bank(activator);
  let desc = element(bank/accounts(bank), as(<symbol>, objectid));
  let (class, initargs) = values(head(desc), tail(desc));
  apply(make, class, reincarnate?: #t, bank: bank, initargs);
end method;

define method PortableServer/ServantActivator/etherealize (activator :: <account-activator>,
							   objectID :: corba/<string>,
							   adapter :: portableserver/<poa>,
							   servant :: PortableServer/<servant>,
							   cleanup-in-progress,
							   remaining-activations)
 => ();
  let bank = object-bank(activator);
  let accounts = bank/accounts(bank);
  element(accounts, as(<symbol>, objectID)) := describe-account(servant);
end method;


/// CURRENTACCOUNT IMPLEMENTATION

define class <demo-currentaccount> (<currentaccount-servant>, <demo-account>)
  constant slot currentaccount/overdraftlimit :: corba/<float> = 0.0, init-keyword: limit:;
end class;

define method account-class (account :: <demo-currentaccount>)
  <demo-currentaccount>
end method;

define method describe-account (account :: <demo-currentaccount>)
 => (description :: <list>)
  concatenate(list(limit: currentaccount/overdraftlimit(account)), next-method());
end method;

define method account/makeWithdrawal (account :: <demo-currentaccount>, withdrawl :: corba/<float>)
 => ()
  if ((account/balance(account) + currentaccount/overdraftlimit(account)) < withdrawl)
    error("ALERT: Account \"%s\" over agreed overdraft limit.", account/name(account));
  end if;
  next-method();
end method;

/// BANK IMPLEMENTATION

define class <demo-bank> (<bank-servant>)
  constant class slot bank/ior-file :: <string> = "c:\\temp\\bank-2.ior";
  constant class slot bank/poa-name :: <string> = "Bank POA 2 (incarnate/etherealize)";
  constant slot bank/accounts :: <table> = make(<table>);
end class;

define method bank/newAccount (bank :: <demo-bank>, name :: corba/<string>)
 => (account :: <account>)
  create-account(bank, <account>, <demo-account>, name: name, bank: bank);
end method;

define method bank/newCurrentAccount (bank :: <demo-bank>, name :: corba/<string>, limit :: corba/<float>)
 => (account :: <currentaccount>)
  create-account(bank, <currentaccount>, <demo-currentaccount>, name: name, limit: limit, bank: bank);
end method;

define method create-account
    (bank :: <demo-bank>, class :: subclass(<account>), servant-class :: subclass(<demo-account>),
     #rest initargs)
 => (account :: <account>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let account-poa = portableserver/poa/find-poa(root-poa, bank/poa-name(bank), #t);
  let account-servant = apply(make, servant-class, initargs);
  let name = account/name(account-servant);
  let repository-id = portableserver/servant/primary-interface(account-servant, name, account-poa);
  let account-reference
    = as(class, portableserver/poa/create-reference-with-id(account-poa, name, repository-id));
  portableserver/poa/destroy(account-poa, #t, #f); // destroy POA after creating account to force etherealize/incarnate test
  account-reference
end method;

define method bank/deleteAccount (bank :: <demo-bank>, reference :: <account>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let account-poa = portableserver/poa/find-poa(root-poa, bank/poa-name(bank), #t);
  let account = portableserver/poa/reference-to-servant(account-poa, reference);
  let id = as(<symbol>, account/name(account));
  remove-key!(bank/accounts(bank), id);
  portableserver/poa/deactivate-object(account-poa, account/name(account));
end method;

/// MAINLINE

define method portableserver/adapteractivator/unknown-adapter (activator :: <account-poa-activator>,
							       parent :: PortableServer/<poa>,
							       name :: corba/<string>)
    => (activated? :: <boolean>)
  // create a new POA and request receiver thread (but it is created
  // in a holding state)
  let account-poa = portableserver/poa/create-poa(parent, name, #f,
						  lifespan-policy: #"persistent", // NB because we are destroying POA in between
						  id-assignment-policy: #"user-id",
						  request-processing-policy: #"use-servant-manager");

  // register a servant-manager for accounts with the custom poa
  let locator = make(<account-activator>, bank: object-bank(activator));
  portableserver/poa/set-servant-manager(account-poa, locator);

  // flick the switch on the poa-manager flow control so the
  // receiver threads start
  let poa-manager = portableserver/poa/the-poamanager(account-poa);
  portableserver/poamanager/activate(poa-manager);

  #t // inform caller that POA is created
end method;

define method start-bank-server ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a bank object (put it in a global for testing)
  let bank = make(<demo-bank>);

  // set up callback object for creating subPOA
  portableserver/poa/the-activator(root-poa) := make(<account-poa-activator>, bank: bank);

  // put bank in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, bank);

  // create an ior string to pass to clients (via special variable
  // for the purposes of testing)
  corba/orb/object-to-file(orb, bank/ior-file(bank), portableserver/poa/servant-to-reference(root-poa, bank));

  let root-poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(root-poa-manager);

  // block until all ORB processing completed
  // corba/orb/run(orb);
end method;

register-server(start-bank-server);
