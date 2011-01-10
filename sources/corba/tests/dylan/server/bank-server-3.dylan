Module: bank-server-3
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// ACCOUNT IMPLEMENTATION

define class <demo-account> (<account-servant>)
  constant slot account/name :: corba/<string>, required-init-keyword: name:;
  slot account/balance :: corba/<float> = 0.0, init-keyword: balance:;
end class;

define method initialize (account :: <demo-account>, #key bank)
  next-method();
  let name = account/name(account);
  let id = as(<symbol>, name);
  let accounts = bank/accounts(bank);
  let existing-account = element(accounts, id, default: #f);
  if (existing-account)
    error(make(bank/<reject>, reason: "cannot create new account with same name"));
  end if;
  element(accounts, id) := account;
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

define class <account-locator> (PortableServer/<ServantLocator>)
  constant slot object-bank :: <bank>, required-init-keyword: bank:;
end class;

define method PortableServer/ServantLocator/PreInvoke (locator :: <account-locator>,
							 objectID :: corba/<string>,
							 adapter :: portableserver/<poa>,
						       operation :: corba/<identifier>)
 => (servant :: PortableServer/<servant>, cookie)
  let servant = element(bank/accounts(object-bank(locator)), as(<symbol>, objectid));
  values(servant, servant);
end method;

define method PortableServer/ServantLocator/postinvoke (locator :: <account-locator>,
							   objectID :: corba/<string>,
							   adapter :: portableserver/<poa>,
							operation :: corba/<identifier>,
							cookie,
							   servant :: PortableServer/<servant>)
 => ();
  unless (cookie = servant)
    error("cookie not passed back to postinvoke")
  end unless;
end method;


/// CURRENTACCOUNT IMPLEMENTATION

define class <demo-currentaccount> (<currentaccount-servant>, <demo-account>)
  constant slot currentaccount/overdraftlimit :: corba/<float> = 0.0, init-keyword: limit:;
end class;

define method account/makeWithdrawal (account :: <demo-currentaccount>, withdrawl :: corba/<float>)
 => ()
  if ((account/balance(account) + currentaccount/overdraftlimit(account)) < withdrawl)
    error("ALERT: Account \"%s\" over agreed overdraft limit.", account/name(account));
  end if;
  next-method();
end method;

/// BANK IMPLEMENTATION

define class <demo-bank> (<bank-servant>)
  constant class slot bank/poa-name :: <string> = "Bank POA 3 (pre/postinvoke)";
  constant class slot bank/ior-file :: <string> = "c:\\temp\\bank-3.ior";
  constant slot bank/accounts :: <table> = make(<table>);
end class;

define method bank/newAccount (bank :: <demo-bank>, name :: corba/<string>)
 => (account :: <account>)
  create-account(bank, <account>, <demo-account>, name: name, bank: bank);
end method;

define method bank/newCurrentAccount (bank :: <demo-bank>, name :: corba/<string>, limit :: corba/<float>)
 => (account :: <currentaccount>)
  create-account(bank, <currentaccount>, <demo-currentaccount>,
		 name: name, limit: limit, bank: bank);
end method;

define method create-account
    (bank :: <demo-bank>, class :: subclass(<account>), servant-class :: subclass(<demo-account>),
     #rest initargs)
 => (object :: <account>)
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let account-poa = portableserver/poa/find-poa(root-poa, bank/poa-name(bank), #t);
  let account = apply(make, servant-class, initargs);
  let name = account/name(account);
  let repository-id = portableserver/servant/primary-interface(account, name, account-poa);
  as(class,
     portableserver/poa/create-reference-with-id(account-poa, name, repository-id));
end method;  

define method bank/deleteAccount (bank :: <demo-bank>, reference :: <account>)
 => ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");
  let account-poa = portableserver/poa/find-poa(root-poa, bank/poa-name(bank), #t);
  let objectid = portableserver/poa/reference-to-id(account-poa, reference);
  let locator = portableserver/poa/get-servant-manager(account-poa);
  let (account, cookie) = PortableServer/ServantLocator/PreInvoke(locator, objectid, account-poa, "dummy");
  let id = as(<symbol>, account/name(account));
  remove-key!(bank/accounts(bank), id);
end method;

/// MAINLINE

define method start-bank-server ()
  // get reference to ORB
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");

  // get reference to root POA (there will already be a listener, dispatcher,
  // and default receiver threads running)
  let root-poa = corba/orb/resolve-initial-references(orb, "RootPOA");

  // actually make a bank object
  let bank = make(<demo-bank>);

  // create a new POA and request receiver thread (but it is created
  // in a holding state)
  let account-poa = portableserver/poa/create-poa(root-poa, bank/poa-name(bank), #f,
						  lifespan-policy: #"transient",
						  id-assignment-policy: #"user-id",
						  servant-retention-policy: #"non-retain",
						  request-processing-policy: #"use-servant-manager");

  // register a servant-manager for accounts with the custom poa
  let locator = make(<account-locator>, bank: bank);
  portableserver/poa/set-servant-manager(account-poa, locator);

  // flick the switch on the poa-manager flow control so the
  // receiver threads start
  let poa-manager = portableserver/poa/the-poamanager(account-poa);
  portableserver/poamanager/activate(poa-manager);

  // put bank in the active-object table of the POA
  let objectid = portableserver/poa/activate-object(root-poa, bank);

  // create an ior string to pass to clients (via special variable
  // for the purposes of testing)
  corba/orb/object-to-file(orb, bank/ior-file(bank), portableserver/poa/servant-to-reference(root-poa, bank));

  // flick the switch on the poa-manager flow control so the
  // receiver threads start
  let root-poa-manager = portableserver/poa/the-poamanager(root-poa);
  portableserver/poamanager/activate(root-poa-manager);

  // block until all ORB processing completed
  // corba/orb/run(orb);
end method;

register-server(start-bank-server);
