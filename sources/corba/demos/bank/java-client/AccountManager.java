// Name: AccountManager
// Author: Jason Trenouth, Clive Tong
// Copyright: (c) 1999 Functional Objects, Inc. All rights reserved.
// Version: $HopeName: D-corba-demos-bank-java-client!AccountManager.java(trunk.1) $

// ---*** Use frame.setIconImage for frame icon
// ---*** frame.getToolkit().getImage("filename")

package BankingDemo;

import org.omg.CORBA.ORB;
import org.omg.CORBA.Any;
import org.omg.CosNaming.*;
import org.omg.CosNaming.NamingContextPackage.*;
import BankingDemo.accountPackage.*; 
import BankingDemo.bankPackage.*; 

import java.awt.*;
import java.awt.event.*;

public class AccountManager extends Frame {
  AccountDialog dialog = null;
  String dialog_value = null;
  bank chosen_bank = null;
  account chosen_account = null;
  String account_name = null;
  int chosen_amount = 0;
  List accounts_list = null;
  TextField balance_field = null;
  TextField limit_field = null;

  public static void main(String args[]) { 

    Frame f = new AccountManager(args);
    f.show();

  }

  public void set_dialog_value (String new_value) {
    dialog_value = new_value;
  };  

  private void init_client (String args[]) {
    org.omg.CORBA.ORB orb = ORB.init(args,null);

    org.omg.CORBA.Object name_service = null;
    try {
      name_service = orb.resolve_initial_references ("NameService");
    }
    catch 
      (org.omg.CORBA.ORBPackage.InvalidName x)
      {
	System.exit (1);
      };
    NamingContext name_context = NamingContextHelper.narrow(name_service);

    NameComponent[] lookup = new NameComponent [1];
    lookup [0] = new NameComponent("Current Bank", "Bank");
    org.omg.CORBA.Object unnarrowed_bank = null;
    try {
      unnarrowed_bank = name_context.resolve (lookup);
    }
    catch (org.omg.CosNaming.NamingContextPackage.CannotProceed x)
      {
	System.exit (1);
      }
    catch (org.omg.CosNaming.NamingContextPackage.NotFound x)
      {
	System.exit (1);
      }
    catch (org.omg.CosNaming.NamingContextPackage.InvalidName x)
      {
	System.exit (1);
      };
    chosen_bank = bankHelper.narrow(unnarrowed_bank);
  };

  public AccountManager(String args[]) {
    
    super("Account Manager");

    init_client(args);
    
    MenuBar app_menubar = new MenuBar();
    setMenuBar(app_menubar);
    
    Menu bank_menu = new Menu("Bank");
    app_menubar.add(bank_menu);
    
    MenuItem new_account_item = new MenuItem("Open Account...");
    bank_menu.add(new_account_item);
    
    MenuItem new_checking_account_item = new MenuItem("Open Checking Account...");
    bank_menu.add(new_checking_account_item);
    
    MenuItem retrieve_account_item = new MenuItem("Retrieve Account...");
    bank_menu.add(retrieve_account_item);
    
    MenuItem close_account_item = new MenuItem("Close Account");
    bank_menu.add(close_account_item);
    
    MenuItem exit_item = new MenuItem("Exit");
    bank_menu.add(exit_item);
    
    Menu account_menu = new Menu("Account");
    app_menubar.add(account_menu);
    
    MenuItem credit_account_item = new MenuItem ("Credit Account...");
    account_menu.add(credit_account_item);
    
    MenuItem debit_account_item = new MenuItem ("Debit Account...");
    account_menu.add(debit_account_item);
      
    GridBagLayout gridBag = new GridBagLayout();
    setLayout(gridBag);
    
    GridBagConstraints lc = new GridBagConstraints();
    
    lc.gridx = 0;
    lc.gridy = 0;
    lc.anchor = GridBagConstraints.WEST;
    lc.insets = new Insets(6,6,6,6);
    Label accounts_label = new Label("Accounts:");
    gridBag.setConstraints(accounts_label, lc);
    add(accounts_label);
    
    lc.gridx = 0;
    lc.gridy = 1;
    lc.gridheight = GridBagConstraints.REMAINDER;
    accounts_list = new List(10, false);
    gridBag.setConstraints(accounts_list, lc);
    add(accounts_list);
    
    lc.gridx = 1;
    lc.gridy = 1;
    lc.gridheight = 1;
    Label balance_label = new Label("Balance:");
    gridBag.setConstraints(balance_label, lc);
    add(balance_label);
    
    lc.gridx = 2;
    lc.gridy = 1;
    balance_field = new TextField(20);
    gridBag.setConstraints(balance_field, lc);
    add(balance_field);
    
    lc.gridx = 1;
    lc.gridy = 2;
    lc.anchor = GridBagConstraints.NORTH;
    Label limit_label = new Label("Limit:");
    gridBag.setConstraints(limit_label, lc);
    add(limit_label);
    
    lc.gridx = 2;
    lc.gridy = 2;
    limit_field = new TextField (20);
    gridBag.setConstraints(limit_field, lc);
    add(limit_field);
    
    lc.gridx = 2;
    lc.gridy = 3;
    lc.fill = GridBagConstraints.BOTH;
    lc.anchor = GridBagConstraints.SOUTH;
    AccountLogo logo = new AccountLogo();
    gridBag.setConstraints(logo, lc);
    add(logo);

    setBackground(Color.lightGray);
    pack();
    setResizable(false);
    
  }
  
  private account getSelectedAccount () {
      chosen_account = null;
      account_name = accounts_list.getSelectedItem();
      try {
	chosen_account = chosen_bank.retrieveAccount(account_name);
      }
      catch (nonExistentAccount x)
      {
	return null;
      };
      return chosen_account;
  };

  private void updateAccountDetails (account acc) {
      String new_value = "" + acc.balance() + "";
      balance_field.setText(new_value);
      if (acc._is_a("IDL:BankingDemo/checkingAccount:1.0")) {
	checkingAccount cacc = checkingAccountHelper.narrow(acc);
	new_value = "" + cacc.limit() + ""; 
	limit_field.setText(new_value);
      } else {
	limit_field.setText("N/A");
      };
  };

  private void updateAccountManager () {
      chosen_account = getSelectedAccount();
      if (chosen_account != null) {
	updateAccountDetails(chosen_account);
      };
  };

  public boolean handleEvent(Event event) {
    switch(event.id) {
      
    case Event.LIST_SELECT:
      updateAccountManager();
      return true;
      
    case Event.ACTION_EVENT:
      String command = (String)event.arg;

      if (command == "Exit") {
	dispose();
	System.exit(0);

      } else if (command == "Open Account...") {
	dialog = new AccountDialog(this, "Open Account", "Enter Account Name:");
	dialog.show();
	if (dialog_value != null) {
	  account_name = dialog_value;
	  try {
	    chosen_bank.openAccount(account_name);
	  }
	  catch (duplicateAccount x) {
	    return true; // ---*** display warning
	  };
	  accounts_list.addItem(account_name, 0);
	  accounts_list.select(0);
	  updateAccountManager();
	}

      } else if (command == "Open Checking Account...") {
	dialog = new AccountDialog(this, "Open Checking Account", "Enter Account Name:");
	dialog.show();
	if (dialog_value != null) {
	  account_name = dialog_value;
	  dialog = new AccountDialog(this, "Open Checking Account", "Enter Credit Limit:");
	  dialog.show();
	  if (dialog_value != null) {
	    int limit = 0;
	    try {
	      limit = Integer.parseInt(dialog_value);
	    }
	    catch (NumberFormatException x) {
	      return true; // ---*** warning
	    };
	    try {
	      chosen_bank.openCheckingAccount(account_name, limit);
	    }
	    catch (duplicateAccount x) {
	      return true; // ---*** display warning
	    };
	    accounts_list.addItem(account_name, 0);
	    accounts_list.select(0);
	    updateAccountManager();
	  };
	};

      } else if (command == "Retrieve Account...") {
	dialog = new AccountDialog(this, "Retieve Account", "Enter Account Name:");
	dialog.show();
	if (dialog_value != null) {
	  account_name = dialog_value;
	  accounts_list.addItem(account_name, 0);
	  accounts_list.select(0);
	  updateAccountManager();
	};
	
      } else if (command == "Credit Account...") {
	dialog = new AccountDialog(this, "Credit Account", "Enter Amount:");
	dialog.show();
	if (dialog_value != null) {
	  try {
	    chosen_amount = Integer.parseInt(dialog_value);
	  }
	  catch (NumberFormatException x) {
	    return true; // ---*** warning
	  };
	  chosen_account = getSelectedAccount();
	  if (chosen_account != null) {
	    chosen_account.credit(chosen_amount);
	    updateAccountDetails(chosen_account);
	  };
	};

      } else if (command == "Debit Account...") {
	dialog = new AccountDialog(this, "Debit Account", "Enter Amount:");
	dialog.show();
	if (dialog_value != null) {
	  try {
	    chosen_amount = Integer.parseInt(dialog_value);
	  }
	  catch (NumberFormatException x) {
	    return true; // ---*** warning
	  };
	  chosen_account = getSelectedAccount();
	  if (chosen_account != null) {
	    try {
	      chosen_account.debit(chosen_amount);
	    }
	    catch (refusal x) {
	      return true; // ---*** display warning
	    };
	    updateAccountDetails(chosen_account);
	  };
	};

      } else if (command == "Close Account") {
	chosen_account = getSelectedAccount();
	if (chosen_account != null) {
	  chosen_bank.closeAccount(chosen_account);
	  accounts_list.delItem(accounts_list.getSelectedIndex());
	  updateAccountManager();
	};
      };
      return true;
      
    }

    return super.handleEvent(event);
  }
  
}
