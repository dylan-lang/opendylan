Module:    bank-client
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant <account-deque> = limited(<deque>, of: <IBankAccount>);

define constant $default-client-frame-width  = 300;
define constant $default-client-frame-height = 350;

define frame <bank-frame> (<simple-frame>)
  constant slot bank :: <IBank>, required-init-keyword: bank:;

  constant slot bank-accounts :: <account-deque> = make(<account-deque>);

  pane bank-menu (frame)
    make(<menu>,
         label: "&Bank",
         children:
           vector(make(<menu-button>,
                       label: "Open New Account...",
                       documentation: "Creates a new ordinary account (with no overdraft allowed).",
		       activate-callback: open-account-callback),
                  make(<menu-button>,
                       label: "Open New Checking Account...",
                       documentation: "Creates a new checking account (with overdraft allowed).",
                       activate-callback: open-checking-account-callback),
                  make(<menu-button>,
                       label: "Retieve Account...",
                       documentation: "Finds an account given its name.",
                       activate-callback: retrieve-account-callback),
                  make(<menu-button>,
                       label: "Close Account...",
                       documentation: "Closes the selected account.",
                       activate-callback: close-account-callback),
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
           generators: list(account-name, account-balance, account-limit),
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

define method exit-callback (gadget :: <gadget>) => ()
  let frame = gadget.sheet-frame;
  exit-frame(frame);
end method;

define method credit-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let account = gadget-value(accounts-pane(bank-frame));
  if (account)
    let amount = prompt-for-amount(owner: bank-frame, title: "Credit ...");
    when (amount) 
      credit-account(account, amount);
      update-gadget(accounts-pane(bank-frame));
    end;
  else
    notify-user("No account selected");
  end;
end method credit-callback;

define method debit-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let account = gadget-value(accounts-pane(bank-frame));
  if (account)
    let amount = prompt-for-amount(owner: bank-frame, title: "Debit ...");
    when (amount) 
      block ()
	debit-account(account, amount);
      exception (c :: <transaction-refused>)
	notify-user(concatenate("Debit refused for the following reason: ", 
				c.transaction-refused-reason),
		    owner: bank-frame);
      end;
      update-gadget(accounts-pane(bank-frame));
    end;
  else
    notify-user("No account selected");
  end;
end method debit-callback;

define method open-account-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let name = prompt-for-name(owner: bank-frame, title: "Open Account ...");
  if (name)
    let bank = bank-frame.bank;
    block ()
      let account = open-account(bank, name);
      push-last(bank-accounts(bank-frame), account);
    exception (c :: <duplicate-account>)
      notify-user(concatenate("Cannot create another account for ", name, "!"),
		  owner: bank-frame);
    end;
    update-gadget(accounts-pane(bank-frame));
  end if;
end method open-account-callback;


define method open-checking-account-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let name = prompt-for-name(owner: bank-frame, title: "Open Checking Account ...");
  if (name)
     let limit = prompt-for-amount(owner: bank-frame, title: "Limit ...");
     if (limit)
       let bank = bank-frame.bank;
       block ()
	 let account = open-checking-account(bank, name, limit);
	 push-last(bank-accounts(bank-frame), account);
       exception (c :: <duplicate-account>)
	 notify-user(concatenate("Cannot create another account for ", name, "!"), 
		     owner: bank-frame);
       end;
       update-gadget(accounts-pane(bank-frame));
     end if;
  end if;
end method open-checking-account-callback;

define method retrieve-account-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let bank = bank-frame.bank;
  let name = prompt-for-name(owner: bank-frame, title: "Retrieve Account ...");
  when (name)
    if (any?(method (account :: <IBankAccount>)
	       account-name(account) = name
	     end,
	     bank-frame.bank-accounts))
      notify-user("Account already retrieved!", owner: bank-frame);       
    else
      block ()
	let account = retrieve-account(bank, name);
	push-last(bank-accounts(bank-frame), account);
      exception (c :: <non-existent-account>)
	notify-user(concatenate("No existing account for ", name, "!"), 
		    owner: bank-frame);
      end;
      update-gadget(accounts-pane(bank-frame));
    end if;
  end;
end method retrieve-account-callback;

define method close-account-callback (gadget :: <gadget>) => ()
  let bank-frame = gadget.sheet-frame;
  let account = gadget-value(accounts-pane(bank-frame));
  if (account)
    let okay = notify-user(concatenate("Close the account: ", account-name(account), "?"),
			   style: #"warning",
			   exit-style: #"ok-cancel",
			   owner: bank-frame);
    when (okay)
      let bank = bank-frame.bank;
      close-account(bank, account);
      remove!(bank-accounts(bank-frame), account);
      update-gadget(accounts-pane(bank-frame))
    end;
  else
    notify-user("No account selected");
  end;
end method close-account-callback;

// utility functions

define method prompt-for-name 
   (#key title = "", owner)
 => (name :: false-or(<string>))
  let text-field = make(<text-field>, activate-callback: exit-dialog);
  let dialog = make(<dialog-frame>, 
                    title: title,
                    owner: owner,
                    layout: vertically ()
		              labelling ("Name:")
                                text-field
		              end;
                            end,
                    input-focus: text-field);
  if (start-dialog(dialog))
    gadget-value(text-field);
  end
end method prompt-for-name;

define method prompt-for-amount 
   (#key title = "", owner) => (amount :: false-or(<integer>))
  let text-field = make(<text-field>, activate-callback: exit-dialog);
  let dialog = make(<dialog-frame>, 
		    title: title,
		    owner: owner,
                    layout: vertically ()
                              labelling ("Amount:")
                                text-field
		              end;
                            end,
                    input-focus: text-field);
  iterate prompt ()
    if (start-dialog(dialog)) 
      let text = gadget-value(text-field);
      let (amount, int-end) = string-to-integer(text, default: -1);
      if (amount >= 0 & int-end = text.size)
	amount
      else
	notify-user("Invalid amount!",  owner: owner);
	prompt();
      end;
    end if;
  end;
end method prompt-for-amount;
