Module:    bank-server
Synopsis:  The GUI for the CORBA server of the bank example.
Author:    Claudio Russo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define constant $default-server-frame-width  = 300;
define constant $default-server-frame-height = 350;

define constant $log-size = 10;

define method record-name (record) => (name)
  record[0];
end method;

define method record-balance (record) => (balance)
  record[1];
end method;

define method record-limit (record) => (limit)
  record[2];
end method;

define frame <server-frame> (<simple-frame>)
  slot connection :: <connection>, init-keyword: connection:;
  pane file-menu (frame)
    make(<menu>,
         label: "&Database",
         children:
           vector(make(<menu-button>,
                       label: "Refresh",
                       documentation: "Forces refresh of accounts details from database.",
		       activate-callback: Refresh-callback),
                  make(<menu-button>,
                       label: "Delete All",
                       documentation: "Deletes all account details from database.",
                       activate-callback: DeleteAll-callback),
                  make(<menu-button>,
                       label: "&Exit",
                       documentation: "Exits the application.",
                       activate-callback: Exit-callback)
                  )); 
                                         
  menu-bar (frame)
    make(<menu-bar>,
         children: vector(file-menu(frame)));

  pane refresh-check-button (frame)
       make(<check-button>, label: "Active Refresh");

  pane log-check-button (frame)
       make(<check-button>, label: "Active Log");

  pane accounts-pane (frame)
     make(<table-control>, headings: list("Name", "Balance", "Limit"),
           generators: list(record-name, record-balance, record-limit), items: #[]);

  constant slot log :: <deque>, init-value: make(<deque>, size: $log-size, fill: "");

  pane log-pane (frame)
     make(<list-box>, 
          selection-mode: #"none", 
          borders: #"output",
          items: frame.log);

  layout (frame)
    vertically (spacing: 2)
      refresh-check-button(frame);
      accounts-pane(frame);
      log-check-button(frame);
      log-pane(frame);
    end;

  status-bar (frame) make(<status-bar>, label: "Ready.");
  keyword title:  = "Bank Server";
  keyword width:  = $default-server-frame-width;
  keyword height: = $default-server-frame-height;
end frame;

define method Exit-callback(button :: <menu-button>) => ()
  let frame = sheet-frame(button);
  exit-frame(frame);
end method;

define method DeleteAll-callback(button :: <menu-button>) => ()
  let frame = sheet-frame(button);
  with-connection(frame.connection)
      let query = make(<sql-statement>,
                       text: "delete from Accounts");
      let result-set = as(<sequence>, execute(query));
      refresh(frame, force?: #t);
  end with-connection;
end method;

define method Refresh-callback(button :: <menu-button>) => ()
  let frame = sheet-frame(button);
  refresh(frame, force?: #t);
end method;

define method refresh(frame :: <server-frame>, #key force? = #f) => ()
  if (force? | frame.refresh-check-button.gadget-value) 
  with-connection(frame.connection)
      let query = make(<sql-statement>,
                       text: "select Name, Balance, Limit from Accounts ");
      let result-set = as(<sequence>, execute(query));
      frame.accounts-pane.gadget-items := result-set;
  end with-connection;
  end if;
end method;

define method log-message(frame :: <server-frame>, message :: <string>, #key force? = #f) => ()
  if (force? | frame.log-check-button.gadget-value)
    pop(frame.log);
    push-last(frame.log, message);     
    update-gadget(frame.log-pane);
  end if;
end method;











