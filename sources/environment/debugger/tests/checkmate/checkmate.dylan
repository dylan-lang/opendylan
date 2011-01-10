Module: checkmate
Synopsis: Test application for debugger
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <CHECKMATE> (internal)

define sealed frame <checkmate> (<simple-frame>)
  sealed constant slot checkmate-data :: <stretchy-vector> = make(<stretchy-vector>);
  pane checkmate-error-types-pane (checkmate)
    make(<radio-box>,
	 orientation: #"vertical",
	 items: vector(#"simple-error",
		       #"access-violation",
		       #"array-bounds-exception",
		       #"datatype-misalignment",
		       #"illegal-instruction-exception",
		       #"privileged-instruction-exception",
		       #"denormal-exception",
		       #"float-divide-by-zero-exception",
		       #"inexact-result-exception",
		       #"invalid-float-operation",
		       #"float-overflow-exception",
		       #"float-underflow-exception",
		       #"float-stack-check-exception",
		       #"integer-divide-by-zero-exception",
		       #"noncontinuable-exception"),
	 label-key: curry(as, <string>));
  layout (checkmate)
      grouping ("Error Type")
        checkmate.checkmate-error-types-pane
      end;
  command-table (checkmate) *checkmate-command-table*;
  status-bar (checkmate) make(<status-bar>);
end frame;


/// $CHECKMATES (internal)

define variable $checkmates = make(<stretchy-vector>);


/// MAKE (dylan)

define sealed domain make (<checkmate>);


/// INITIALIZE (dylan)

define sealed method initialize (checkmate :: <checkmate>, #key)
 => ()
  next-method();
  add!($checkmates, checkmate);
  checkmate.frame-title := format-to-string("Checkmate %d", $checkmates.size);
  add!(checkmate.checkmate-data, false-or(<integer>)); // a type
  add!(checkmate.checkmate-data, <class>); // a class
  add!(checkmate.checkmate-data, singleton(1)); // a singleton
  add!(checkmate.checkmate-data, 'a');
  add!(checkmate.checkmate-data, #"symbol");
  add!(checkmate.checkmate-data, #t);
  add!(checkmate.checkmate-data, 1.0);
  add!(checkmate.checkmate-data, 1.0 / 6.0);
  add!(checkmate.checkmate-data, 1);
  let table = make(<table>);
  table[#"foo"] := 1;
  table[#"bar"] := 2;
  table[#"baz"] := make(<table>);
  table[#"quux"] := 3;
  add!(checkmate.checkmate-data, table);
  let array = make(<array>, dimensions: #[2, 2]);
  aref(array, 0, 0) := 1;
  aref(array, 0, 1) := 2;
  aref(array, 1, 0) := make(<array>, dimensions: #[2, 2]);
  aref(array, 1, 1) := 3;
  add!(checkmate.checkmate-data, array);
  add!(checkmate.checkmate-data, list(1,2,list(),3));
  add!(checkmate.checkmate-data, pair(1,2));
  let deque = make(<deque>);
  add!(deque, 1);
  add!(deque, 2);
  add!(deque, make(<deque>));
  add!(deque, 3);
  add!(checkmate.checkmate-data, deque);
  add!(checkmate.checkmate-data, "1 2 \"3\"");
  add!(checkmate.checkmate-data, vector(1,2,vector(),3));
  let stretchy-vector = make(<stretchy-vector>);
  add!(stretchy-vector, 1);
  add!(stretchy-vector, 2);
  add!(stretchy-vector, make(<stretchy-vector>));
  add!(stretchy-vector, 3);
  add!(checkmate.checkmate-data, stretchy-vector);
  add!(checkmate.checkmate-data, make(<range>, from: 0, to: 4, by: 1));
  add!(checkmate.checkmate-data, identity);
  add!(checkmate.checkmate-data, method () end method);
  add!(checkmate.checkmate-data, make);
  add!(checkmate.checkmate-data, concatenate);
  add!(checkmate.checkmate-data, range);
  add!(checkmate.checkmate-data, \=);
  add!(checkmate.checkmate-data, compose);
  add!(checkmate.checkmate-data, make(<simple-warning>,
				      format-string: "1 2 %= 3",
				      format-arguments: vector(make(<simple-warning>))));
  add!(checkmate.checkmate-data, make(<simple-restart>,
				      format-string: "1 2 %= 3",
				      format-arguments: vector(make(<simple-restart>))));
  add!(checkmate.checkmate-data, make(<abort>));
  add!(checkmate.checkmate-data, make(<simple-error>,
				      format-string: "1 2 %= 3",
				      format-arguments: vector(make(<simple-error>))));
  add!(checkmate.checkmate-data, make(<type-error>,
				      value: make(<type-error>),
				      type: <type-error>));
end method;


/// $LOCK (internal)

define constant $lock = make(<lock>);


/// $NOTIFICATION (internal)

define constant $notification = make(<notification>, lock: $lock);


/// *CHECKMATE-MULTI-THREADED?* (internal)

define variable *checkmate-multi-threaded?* = #t;


/// BOOT-CHECKMATE (internal)

define function boot-checkmate ()
  let checkmate = make(<checkmate>);
  if (*checkmate-multi-threaded?*)
    boot-multi-threaded-checkmate(checkmate);
  else
    boot-single-threaded-checkmate(checkmate);
  end if;
end function;


/// BOOT-MULTI-THREADED-CHECKMATE (internal)

define function boot-multi-threaded-checkmate (checkmate :: <checkmate>)
  start-checkmate(checkmate);
  with-lock ($lock)
    wait-for($notification);
  end;
end function;


/// BOOT-SINGLE-THREADED-CHECKMATE (internal)

define function boot-single-threaded-checkmate (checkmate :: <checkmate>)
  command-enabled?(new-checkmate, checkmate) := #f;
  command-enabled?(close-checkmate, checkmate) := #f;
  start-frame(checkmate);
end function;


/// NEW-CHECKMATE (internal)

define function new-checkmate (checkmate :: <checkmate>)
  start-checkmate(make(<checkmate>));
end function;


/// START-CHECKMATE (internal)

define function start-checkmate (checkmate :: <checkmate>)
  make(<thread>,
       function: method () start-frame(checkmate) end method,
       title: format-to-string("Thread for %s", checkmate.frame-title));
end function;


/// CLOSE-CHECKMATE (internal)

define function close-checkmate (checkmate :: <checkmate>)
  exit-frame(checkmate, destroy?: #t);
  $checkmates := remove!($checkmates, checkmate);
  if ($checkmates.size == 0)
    exit-checkmate(checkmate);
  end if;
end function;


/// EXIT-CHECKMATE (internal)

define function exit-checkmate (checkmate :: <checkmate>)
  local method exit (frame)
	  exit-frame(frame, destroy: #t);
	end method;
  do(exit, $checkmates);
  with-lock ($lock)
    release($notification);
  end;
end function;


/// CHECKMATE-DEBUG-MESSAGE (internal)

define function checkmate-debug-message (checkmate :: <checkmate>)
  debug-message("debug-message from %s", checkmate.frame-title);
end function;


/// CHECKMATE-WARNING (internal)

define function checkmate-warning (checkmate :: <checkmate>)
  signal(make(<simple-warning>,
	      format-string: "warning from %s",
	      format-arguments: checkmate.frame-title));
end function;


/// CHECKMATE-BREAK (internal)

define function checkmate-break (checkmate :: <checkmate>)
  break("break from %s", checkmate.frame-title);
end function;


/// CHECKMATE-ERROR (internal)

define function checkmate-error (checkmate :: <checkmate>)
  select (checkmate.checkmate-error-types-pane.gadget-value)
    #f, #"simple-error"			=> error("error from %s", checkmate.frame-title);
    #"access-violation"			=> RaiseException($EXCEPTION-ACCESS-VIOLATION, 0, 0, 0);
    #"array-bounds-exception"		=> RaiseException($EXCEPTION-ARRAY-BOUNDS-EXCEEDED, 0, 0, 0);
    #"datatype-misalignment"		=> RaiseException($EXCEPTION-DATATYPE-MISALIGNMENT, 0, 0, 0);
    #"illegal-instruction-exception"	=> RaiseException($EXCEPTION-ILLEGAL-INSTRUCTION, 0, 0, 0);
    #"privileged-instruction-exception" => RaiseException($EXCEPTION-PRIVILEGED-INSTRUCTION, 0, 0, 0);
    #"denormal-exception"		=> RaiseException($EXCEPTION-FLOAT-DENORMAL-OPERAND, 0, 0, 0);
    #"float-divide-by-zero-exception"	=> RaiseException($EXCEPTION-FLOAT-DIVIDE-BY-ZERO, 0, 0, 0);
    #"inexact-result-exception"		=> RaiseException($EXCEPTION-FLOAT-INEXACT-RESULT, 0, 0, 0);
    #"invalid-float-operation"		=> RaiseException($EXCEPTION-FLOAT-INVALID-OPERATION, 0, 0, 0);
    #"float-overflow-exception"		=> RaiseException($EXCEPTION-FLOAT-OVERFLOW, 0, 0, 0);
    #"float-underflow-exception"	=> RaiseException($EXCEPTION-FLOAT-UNDERFLOW, 0, 0, 0);
    #"float-stack-check-exception"	=> RaiseException($EXCEPTION-FLOAT-STACK-CHECK, 0, 0, 0);
    #"integer-divide-by-zero-exception"	=> RaiseException($EXCEPTION-INTEGER-DIVIDE-BY-ZERO, 0, 0, 0);
    #"noncontinuable-exception"		=> RaiseException($EXCEPTION-NONCONTINUABLE-EXCEPTION, 0, 0, 0);
  end select;
end function;


/// CHECKMATE-CONTINUABLE-ERROR (internal)

define function checkmate-continuable-error (checkmate :: <checkmate>)
  cerror(format-to-string("continue running %s", checkmate.frame-title),
	 "continuable error from %s",
	 checkmate.frame-title);
end function;

/// *FILE-COMMAND-TABLE* (internal)

define command-table *file-command-table* (*global-command-table*)

  menu-item "New" of <function> = new-checkmate,
                                 documentation: "Creates a new clone of the checkmate window";
  menu-item "Close" of <function> = close-checkmate,
                                   documentation: "Destroys current checkmate window";
  menu-item "Exit" of <function> = exit-checkmate,
                                  documentation: "Quits checkmate and destroys all checkmate windows";
end command-table;


/// *SIGNAL-COMMAND-TABLE* (internal)

define command-table *signal-command-table* (*global-command-table*)
  menu-item "Debug Message" of <function> = checkmate-debug-message,
                                           documentation: "Outputs Dylan debug message";
  menu-item "Warning" of <function> = checkmate-warning,
                                     documentation: "Signals Dylan warning";
  menu-item "Break" of <function> = checkmate-break,
                                   documentation: "Signals Dylan break";
  menu-item "Error" of <function> = checkmate-error,
                                   documentation: "Signals simple Dylan error or low level error (choose type)";
  menu-item "Continuable Error" of <function> = checkmate-continuable-error,
                                               documentation: "Establishes simple-restart then signals simple Dylan error";
end command-table;


///  *CHECKMATE-COMMAND-TABLE* (internal)

define command-table *checkmate-command-table* (*global-command-table*)
  menu-item "File" of <command-table> = *file-command-table*;
  menu-item "Signal" of <command-table> = *signal-command-table*;
end command-table;


