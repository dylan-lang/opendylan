Module:    bank-client
Synopsis:  The CORBA client of the bank example.
Author:    Claudio Russo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// banks

define constant $default-client-frame-width  = 300;
define constant $default-client-frame-height = 350;

define frame <bank-frame> (<simple-frame>)
  constant slot bank :: BankingDemo/<bank>, init-keyword: bank:;

  constant slot bank-accounts :: limited(<deque>, of: bankingdemo/<account>)
    = make(limited(<deque>, of: bankingdemo/<account>));

  pane bank-menu (frame)
    make(<menu>,
         label: "&Bank",
         children:
           vector(make(<menu-button>,
                       label: "Open New Account...",
                       documentation: "Creates a new ordinary account (with no overdraft allowed).",
		       activate-callback: openaccount-callback),
                  make(<menu-button>,
                       label: "Open New Checking Account...",
                       documentation: "Creates a new checking account (with overdraft allowed).",
                       activate-callback: opencheckingaccount-callback),
                  make(<menu-button>,
                       label: "Retieve Account...",
                       documentation: "Finds an account given its name.",
                       activate-callback: retrieveaccount-callback),
                  make(<menu-button>,
                       label: "Close Account...",
                       documentation: "Closes the selected account.",
                       activate-callback: closeaccount-callback),
                  make(<menu-button>,
                       label: "Exit",
                       documentation: "Exits the application.",
                       activate-callback: exit-callback)
                  )); 
   pane account-menu (frame)
     make(<menu>,
          label: "&Account",
          children:
            vector(make(<menu-button>,
                        label: "Credit Account...",
                        documentation: "Credits the selected account given an amount.",
                        activate-callback: credit-callback),
                   make(<menu-button>,
                        label: "Debit Account...",
                        documentation: "Debits the selected account given an amount.",
                        activate-callback: debit-callback)));
                                         
    
   pane accounts-pane (frame)
     make(<table-control>, headings: list("Name", "Balance", "Limit"),
           generators: list(account-name,
                            account-balance,
                            account-limit),
           items: bank-accounts(frame));

  menu-bar (frame)
    make(<menu-bar>,
         children: vector(bank-menu(frame), account-menu(frame)));

// activation of frame elements
  status-bar (frame) make(<status-bar>, label: "Ready.");

  layout (frame)
     accounts-pane(frame);

// frame title
  keyword title: =  "Bank Client";
  keyword width:  = $default-client-frame-width;
  keyword height: = $default-client-frame-height;
end frame <bank-frame>;

define method account-name (account :: bankingdemo/<account>)
  bankingdemo/account/name(account)
end method;

define method account-balance (account :: bankingdemo/<account>)
  integer-to-string(bankingdemo/account/balance(account))
end method;

define method account-limit (account :: bankingdemo/<account>)
  "N/A"
end method;

define method account-limit (account :: bankingdemo/<checkingaccount>)
  integer-to-string(bankingdemo/checkingaccount/limit(account))
end method;

define method Exit-callback(gadget :: <gadget>) => ()
  let frame = sheet-frame(gadget);
  exit-frame(frame);
end method;

define method credit-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let account = gadget-value(accounts-pane(bank-frame));
  let amount = prompt-for-amount(owner: bank-frame, title: "Credit ...");
  if (amount) 
      BankingDemo/account/credit(account, abs(amount));
      update-gadget(accounts-pane(bank-frame));
  end if;
end method credit-callback;

define method debit-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let account = gadget-value(accounts-pane(bank-frame));
  let amount = prompt-for-amount(owner: bank-frame, title: "Debit ...");
  if (amount) 
     block ()
        BankingDemo/account/debit(account, amount); 
        update-gadget(accounts-pane(bank-frame));
     exception(refusal :: BankingDemo/account/<refusal>)
        notify-user(concatenate("Debit refused for the following reason: ", 
                                 refusal.BankingDemo/account/refusal/reason), 
                    owner: bank-frame);
     end block;
  end if; 
end method debit-callback;

define method openAccount-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let bank = bank-frame.bank;
  let name = prompt-for-name(owner: bank-frame, title: "Open Account ...");
  if (name)
     block ()
        let account = BankingDemo/bank/openAccount(bank, name);
        push-last(bank-accounts(bank-frame), account);
        update-gadget(accounts-pane(bank-frame));
     exception(duplicateAccount :: BankingDemo/bank/<duplicateAccount>)
        notify-user(concatenate("Cannot create another account for ", name, "!"), 
                    owner: bank-frame);
     end block;
  end if;
end method openAccount-callback;

define method openCheckingAccount-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let bank = bank-frame.bank;
  let name = prompt-for-name(owner: bank-frame, title: "Open Checking Account ...");
  if (name)
     let limit = prompt-for-amount(owner: bank-frame, title: "Limit ...");
     if (limit)
        block ()
          let checkingAccount = BankingDemo/bank/openCheckingAccount(bank, name, limit);
          push-last(bank-accounts(bank-frame), checkingAccount);
          update-gadget(accounts-pane(bank-frame));
	exception(duplicateAccount :: BankingDemo/bank/<duplicateAccount>)
	  notify-user(concatenate("Cannot create another account for ", name, "!"), 
		      owner: bank-frame);
	end block;
     end if;
  end if;
end method openCheckingAccount-callback;

define method retrieveAccount-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let bank = bank-frame.bank;
  let name = prompt-for-name(owner: bank-frame, title: "Retrieve Account ...");
  if (name)
   begin 
    if (any? (method (account :: bankingdemo/<account>)
                 account.bankingdemo/account/name = name;
              end method,
              bank-frame.bank-accounts))
         notify-user("Account already retrieved!", 
                     owner: bank-frame);       
   else
     block ()
       let account = BankingDemo/bank/retrieveAccount(bank, name);
       push-last(bank-accounts(bank-frame), account);
       update-gadget(accounts-pane(bank-frame));
     exception(nonExistentAccount :: BankingDemo/bank/<nonExistentAccount>)
         notify-user(concatenate("No existing account for ", name, "!"), 
                     owner: bank-frame);
     end block;
    end if;
   end;
  end if;
end method retrieveAccount-callback;

define method closeAccount-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let account = gadget-value(accounts-pane(bank-frame));
  let okay = notify-user(concatenate("Close the account: ", bankingdemo/account/name(account), "?"),
                         style: #"warning",
                         exit-style: #"ok-cancel",
                         owner: bank-frame);
  if (okay)
    let bank = bank-frame.bank;
    BankingDemo/bank/closeAccount(bank, account);
    remove!(bank-accounts(bank-frame), account);
    update-gadget(accounts-pane(bank-frame))
  end if;
end method closeAccount-callback;

// utility functions

define method prompt-for-name 
   (#key title = "", owner)
 => (name :: false-or(<string>))
  let text-field = make(<text-field>, 
                       activate-callback: exit-dialog);
  let dialog = make(<dialog-frame>, 
                    title: title,
                    owner: owner,
                    layout: vertically ()
       	              labelling ("Name:") text-field end;
                    end,
                    input-focus: text-field);
  if (start-dialog(dialog))
    gadget-value(text-field);
  end
end method prompt-for-name;

define method prompt-for-amount 
   (#key title = "", owner)
 => (amount :: false-or(CORBA/<long>))
  let text-field = make(<text-field>, activate-callback: exit-dialog);
  let dialog = make(<dialog-frame>, 
                    title: title,
                    owner: owner,
                    layout: vertically ()
                              labelling ("Amount:") text-field end;
                            end,
                    input-focus: text-field);
  if (start-dialog(dialog)) 
       string-to-integer(gadget-value(text-field))
     | (notify-user("Invalid amount (operation cancelled)!",  owner: owner) & #f);       
  end if;
end method prompt-for-amount;

