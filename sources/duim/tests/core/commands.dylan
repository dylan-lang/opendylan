Module:       duim-test-suite
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Command class tests

define sideways method make-test-instance
    (class :: subclass(<command-decorator>)) => (instance :: <command-decorator>)
  make(class, object: "Dummy Object", type: <object>)
end method make-test-instance;

define sideways method make-test-instance
    (class :: subclass(<functional-command>)) => (instance :: <functional-command>)
  make(class, server: "Dummy Server", function: always(#f))
end method make-test-instance;

define duim-commands class-test <command> ()
  //---*** Fill this in...
  //---*** Maybe get this from the commands test suite?
end class-test <command>;

define duim-commands class-test <functional-command> ()
  //---*** Fill this in...
end class-test <functional-command>;

define duim-commands class-test <command-decorator> ()
  //---*** Fill this in...
end class-test <command-decorator>;

define duim-commands class-test <command-table> ()
  //---*** Fill this in...
end class-test <command-table>;


/// Command tests

define test simple-commands-test ()
  let frame = make-test-frame(<test-frame>);
  let command = make(<functional-command>, function: test-callback, server: frame);
  prepare-for-test-callback();
  execute-command(command);
  check-true("Execute command", test-callback-invoked?());
end test simple-commands-test;


/// Command table tests

define test command-tables-test ()
  let command-table = make(<command-table>, name: #"Test Command Table");
  let command = test-callback;
  add-command(command-table, command);
  check-true("Add function to command-table", 
             command-present?(command-table, command));
  remove-command(command-table, command);
  check-false("Remove function from command-table", 
              command-present?(command-table, command));
  let command = <functional-command>;
  add-command(command-table, command);
  check-true("Add command class to command-table", 
             command-present?(command-table, command));
  remove-command(command-table, command);
  check-false("Remove command class from command-table", 
              command-present?(command-table, command));
  let command = make(<functional-command>, function: test-callback, server: #f);
  add-command(command-table, command);
  check-true("Add command to command-table", 
             command-present?(command-table, command));
  remove-command(command-table, command);
  check-false("Remove command from command-table", 
              command-present?(command-table, command));
end test command-tables-test;


/// Command table menu handling

define variable *test-command-menus*
  = vector(vector("Identity", identity),
           vector("First", first));

define test command-table-menus-test ()
  let command-table = make(<command-table>, name: #"Test Command Table");
  for (command-data in *test-command-menus*)
    add-command-table-menu-item
      (command-table, command-data[0], <command>, command-data[1])
  end;
  let framem = find-test-frame-manager();
  let menus 
    = make-menus-from-command-table(command-table, #f, framem, label: "Test");
  let menu = size(menus) = 1 & menus[0];
  check-true("Command table menu created", menu & instance?(menu, <menu>));
  check-equal("Command table menu label", gadget-label(menu), "Test");
  let menu-items = sheet-children(menu);
  check-equal("Command table menu correct size",
              size(menu-items), size(*test-command-menus*));
  for (item in sheet-children(menu),
       command-data in *test-command-menus*,
       count from 0)
    check-equal(format-to-string("Command table menu item %d label", count),
                command-data[0], gadget-label(item));
    check-true(format-to-string("Command table menu item %d callback", count),
               gadget-activate-callback(item) ~= #f);
  end;
  menus
end test command-table-menus-test;


/// Define the commands test suite

define suite duim-commands-suite ()
  test simple-commands-test;
  test command-tables-test;
  test command-table-menus-test;
end suite duim-commands-suite;
