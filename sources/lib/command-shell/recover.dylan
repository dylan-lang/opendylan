Module:    command-shell
Synopsis:  A simple listener restart mechanism
Author:    Nosa Omorogbe
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define function condition-handler(condition :: <condition>, next-handler)
  display-condition(condition);
  block()
    restart-handler();
  exception(<abort>,
	      init-arguments:
	      vector(format-string:
		       format-to-string("Return to level %s", 
					  *top-level-loop*.level)))
  end block;
  
  signal(make(<command-loop-continuation>));
end function condition-handler;

define function display-condition
    (condition :: <condition>, #key prefix = "Internal error") => ()
  let error-message :: <string>
    = block ()
	format-to-string("%s", condition)
      exception (error :: <error>)
	block ()
	  format-to-string("*** Crashed printing condition of class %=: %s",
			   condition.object-class, error)
	exception (<error>)
	  "*** Crashed printing error, and then printing crash condition"
	end
      end;
  user-message(*top-level-loop*, "\n%s: %s\n", prefix, error-message);
end function display-condition;

define function restart-handler()
  print-restart-options();
  signal-error();
end function;


define function restart-decliner(#rest args)
  user-message(*top-level-loop*, "\n Restart Handler Declined; Please retry or choose another option");
  restart-handler();
end function;


define function unpick-format-string(#rest args,
				     #key format-string = "[<class> <simple-restart>]")
  format-string;
end function;

define function print-restart-options()
  user-message(*top-level-loop*, "\n");
  user-message(*top-level-loop*, "  0: (debug) Invoke low-level debugger\n");
  let count = 0;
  let abort-count = 0;
  let simple-restart-count = 0;
  do-handlers
    (method(type, test, function, init-arguments)
	 select(type by subtype?)
	   <simple-restart> =>
	     count := count + 1; simple-restart-count := simple-restart-count + 1;
	     let prefix = if (simple-restart-count = 1) "(continue) " else "" end if;
	     user-message(*top-level-loop*, "  %s: %s\n", count,
			concatenate(prefix, apply(unpick-format-string, init-arguments)));
	   <abort> =>
	     count := count + 1; abort-count := abort-count + 1;
	     let prefix = if (abort-count = 1) "(abort) " else "" end if;
	     user-message(*top-level-loop*, "  %s: %s\n", count,
			concatenate(prefix, apply(unpick-format-string, init-arguments)));
	   <restart> =>
	     count := count + 1;
	     user-message(*top-level-loop*, "  %s: %s\n", count, type);
	   otherwise => #f;
	 end select;
     end method);
  user-message(*top-level-loop*, "\nType c followed by a number to proceed or type help for other options\n");
end function;


define function signal-error()
  let prompt
    = format-to-string("%s:%s => ", 
		       *prompt-prefix*,
		       *top-level-loop*.level + 1);
  dynamic-bind (*top-level-loop* = make(<command-loop>,
					command-table: *top-level-loop*.command-table,
					prompt: prompt,
					source: *standard-input*,
					sink: *standard-output*,
					banner-function: dylan-banner,
					level: *top-level-loop*.level + 1))
      run-command-loop-internal(*top-level-loop*);
  end;
end function;

define method invoke-restart (option-string :: <string>)
  let option = string-to-integer(option-string, default: 1);
  invoke-restart(option)
end method invoke-restart;

define method invoke-restart (option :: <integer>)
  if (option = 0)
    user-message(*top-level-loop*, "\nEntering debugger...");
    break();
    print-restart-options();
  else
    let count = 0;
    do-handlers(method(type, test, function, init-arguments)
		    if (subtype?(type, <restart>))
		      count := count + 1;
		      if (option = count)
			let restart-obj = apply(make, type, init-arguments);
			restart-query(restart-obj);
			function(restart-obj, restart-decliner)
		      end if;
		    end if;
		end method);
    user-message(*top-level-loop*, "\nThere is no such restart; Please choose another option\n");
  end if;
end method invoke-restart;
