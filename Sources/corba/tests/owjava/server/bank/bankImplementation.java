package server.bank;

import java.util.Vector;
import IE.Iona.OrbixWeb._CORBA;
import org.omg.CORBA.SystemException;

import shared._bankOperations;
import shared.bankPackage.*;
import shared.account;
import shared.currentAccount;
import shared._tie_account;
import shared._tie_currentAccount;

/**
 * The bank implementation class.
 * The bank can create accounts or current accounts and maintains a 
 * Vector of all accounts created.
 */
class bankImplementation implements _bankOperations { 

    public bankImplementation () throws SystemException{
      super();
    }

    /** 
     * Internal record() operation used to add new accounts to the Vector
     */
    void record(String name, accountImplementation p) {
      AccList.addElement(p);
      nameList.addElement(name);
    }

    /** 
     * newAccount() creates a new account and adds it to the account Vector.
     */
    public account newAccount(String name) throws reject {

      System.out.println("Creating Account for " + name);

      accountImplementation accImpl = null;

      if(nameList.contains(name)) 
	throw new reject("Duplicate name "+name); 

      try {
           accImpl = new accountImplementation(0,name);
          }
           catch (SystemException se)
		 {
                  System.out.println("Exception : " + se.toString());
                 }
      account acc = new _tie_account(accImpl); 
	  record(name , accImpl);
	  return acc;
	}

	/** 
	 * newCurrentAccount() creates a new current account and adds it to the 
	 * account Vector.
	 */
    public currentAccount newCurrentAccount(String name, float limit) throws reject {
 
      System.out.println("Creating current account for "+name); 
      currentAccountImpl currImpl = null;

      if(nameList.contains(name)) 
	throw new reject("Duplicate name "+name);       

      try {
           currImpl = new currentAccountImpl(0,name,limit);
          }
           catch (SystemException se)
		 {
         	  System.out.println("Exception : " + se.toString());
      		 }
      currentAccount currAcc = new _tie_currentAccount(currImpl);
      record(name, currImpl);
      return currAcc;
    }

    public void deleteAccount(account a) {
    try {
      //     nameList.removeElement(((currentAccountImpl)(a._deref())).get_name());
      //AccList.removeElement((currentAccountImpl)(a._deref())); 
     nameList.removeElement(((accountImplementation)(a._deref())).get_name());
     AccList.removeElement((accountImplementation)(a._deref())); 
     _CORBA.Orbix.dispose(a);
    } catch (Exception ex) { System.out.println("****" + ex); };

    }

  Vector AccList = new Vector();         // Account Object list.
  Vector nameList = new Vector();	//name list
}
