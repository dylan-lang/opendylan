Module: bank-client
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define suite bank-test-suite ()
  test bank-0-request-tests;
  test bank-1-request-tests;
  test bank-2-request-tests;
  test bank-3-request-tests;
  test bank-4-request-tests;
end suite;

define constant zero/$bank-ior-file :: <string> = "c:\\temp\\bank-0.ior";
define constant one/$bank-ior-file :: <string> = "c:\\temp\\bank.ior";
define constant two/$bank-ior-file :: <string> = "c:\\temp\\bank-2.ior";
define constant three/$bank-ior-file :: <string> = "c:\\temp\\bank-3.ior";
define constant four/$bank-ior-file :: <string> = "c:\\temp\\bank-4.ior";

define test bank-0-request-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let bankref = as(<bank>, corba/orb/file-to-object(orb, zero/$bank-ior-file));
  do-bank-tests(orb, bankref);
end test;
  
define test bank-1-request-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let bankref = as(<bank>, corba/orb/file-to-object(orb, one/$bank-ior-file));
  do-bank-tests(orb, bankref);
end test;
  
define test bank-2-request-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let bankref = as(<bank>, corba/orb/file-to-object(orb, two/$bank-ior-file));
  do-bank-tests(orb, bankref);
end test;
  
define test bank-3-request-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let bankref = as(<bank>, corba/orb/file-to-object(orb, three/$bank-ior-file));
  do-bank-tests(orb, bankref);
end test;
  
define test bank-4-request-tests ()
  let orb = corba/orb-init(make(corba/<arg-list>), "Functional Developer ORB");
  let bankref = as(<bank>, corba/orb/file-to-object(orb, four/$bank-ior-file));
  do-bank-tests(orb, bankref);
end test;
  
define method do-bank-tests (orb :: corba/<orb>, bankref :: <bank>)
  let lodgement1 = 31.00;
  let lodgement2 = 53.17;
  let withdrawal1 = 17.00;
  let withdrawal2 = 7.13;

  // ACCOUNT
  let accref = #f;
  check("account made on bank server", instance?, (accref := bank/newAccount(bankref, "fred")), <account>);
  check-false("lodgement1 reaches account", account/makelodgement(accref, lodgement1));
  check-false("lodgement2 reaches account", account/makelodgement(accref, lodgement2));
  check-false("withdrawal1 reaches account", account/makewithdrawal(accref, withdrawal1));
  check-false("withdrawal2 reaches account", account/makewithdrawal(accref, withdrawal2));
  check("balance comes back from account", \=, account/balance(accref), lodgement1 + lodgement2 - withdrawal1 - withdrawal2);
  check-condition("check reject signalled on account", bank/<reject>, bank/newAccount(bankref, "fred"));
  check-false("account deleted on server", Bank/deleteAccount(bankref, accref));

  let overdraft1 = 20.00;
  let overdraft2 = 10.00;
  let withdrawal3 = 100.0;

  // CURRENTACCOUNT
  let caccref = #f;
  check("currentaccount made on bank server", instance?, (caccref := bank/newCurrentAccount(bankref, "joe", overdraft1)), <currentaccount>);
  check-false("lodgement1 reaches currentaccount", account/makelodgement(caccref, lodgement1));
  check-false("lodgement2 reaches currentaccount", account/makelodgement(caccref, lodgement2));
  check-false("withdrawal1 reaches currentaccount", account/makewithdrawal(caccref, withdrawal1));
  check-false("withdrawal2 reaches currentaccount", account/makewithdrawal(caccref, withdrawal2));
  check("balance comes back from currentaccount", \=, account/balance(caccref), lodgement1 + lodgement2 - withdrawal1 - withdrawal2);
  check-condition("check reject signalled on currentaccount", bank/<reject>, bank/newCurrentAccount(bankref, "joe", overdraft2));
  check("overdraftlimit comes back from currentaccount", \=, currentaccount/overdraftlimit(caccref), overdraft1);
  check-condition("user error translated to corba error", corba/<bad-operation>, account/makewithdrawal(caccref, withdrawal3));
  check-false("currentaccount deleted on server", Bank/deleteAccount(bankref, caccref));
end method;
