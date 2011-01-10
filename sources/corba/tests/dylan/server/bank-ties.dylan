Module: bank-server-4
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
Issues:

---*** put <servant-tie> class into ORB runtime

*/

/// RUNTIME SUPPORT

define open abstract class <servant-tie> (<object>)
  constant slot tied-object, required-init-keyword: object:;
end class;

/// TIED GENERICS
///
/// NB User must implement these. We need new generics because the normal
/// generics are defined on IDL interface classes and the user doesn't
/// want to subclass from these for delegation. That's the whole point:
/// the user doesn't want to have to insert the servant superclass
/// at the top of their class hierarchy. C++ gets around this because
/// member functions are scoped by classes.

define open generic tie/account/balance (object :: <object>)
 => (result :: CORBA/<float>);

define open generic tie/account/makeLodgement (object :: <object>, f :: CORBA/<float>)
 => ();

define open generic tie/account/makeWithdrawal (object :: <object>, f :: CORBA/<float>)
 => ();

define open generic tie/currentAccount/overdraftLimit (object :: <object>)
 => (result :: CORBA/<float>);

define open generic tie/bank/newAccount (object :: <object>, name :: CORBA/<string>)
 => (result :: <account>);

define open generic tie/bank/newCurrentAccount (object :: <object>, name :: CORBA/<string>, limit :: CORBA/<float>)
 => (result :: <currentAccount>);

define open generic tie/bank/deleteAccount (object :: <object>, a :: <account>)
 => ();


/// TIED CLASSES AND TRAMPOLINES

define class <account-tie> (<account-servant>, <servant-tie>)
end class;

define method account/balance (object :: <account-tie>)
 => (result :: CORBA/<float>)
  tie/account/balance(tied-object(object));
end method;

define method account/makeLodgement (object :: <account-tie>, f :: CORBA/<float>)
 => ()
  tie/account/makelodgement(tied-object(object), f);
end method;

define method account/makeWithdrawal (object :: <account-tie>, f :: CORBA/<float>)
 => ()
  tie/account/makeWithdrawal(tied-object(object), f);
end method;

define class <currentAccount-tie> (<currentaccount-servant>, <account-tie>)
end class;

define method currentAccount/overdraftLimit (object :: <currentAccount-tie>)
 => (result :: CORBA/<float>)
  tie/currentaccount/overdraftlimit(tied-object(object))
end method;

define class <bank-tie> (<bank-servant>, <servant-tie>)
end class;

define method bank/newAccount (object :: <bank-tie>, name :: CORBA/<string>)
 => (result :: <account>)
  tie/bank/newaccount(tied-object(object), name);
end method;

define method bank/newCurrentAccount (object :: <bank-tie>, name :: CORBA/<string>, limit :: CORBA/<float>)
 => (result :: <currentAccount>)
  tie/bank/newCurrentAccount(tied-object(object), name, limit);
end method;

define method bank/deleteAccount (object :: <bank-tie>, a :: <account>)
 => ()
  tie/bank/deleteAccount(tied-object(object), a);
end method;

/// IMPLEMENTATION

/* eof */
