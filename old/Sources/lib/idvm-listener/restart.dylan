module: idvm-listener
Synopsis:  Design a restart mechanism for the In-Dylan Virtual Machine (IDVM)
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method unpick-format-string(#rest args,
				   #key format-string = "[<class> <simple-restart>]")
  format-string;
end method unpick-format-string;

define method print-restart-options(#rest args)
  send-string("\n\n");
  send-string("  0: (debug) Invoke low-level debugger\n");
  let count = 0;
  let abort-count = 0;
  let simple-restart-count = 0;
  do-handlers(method(type, test, function, init-arguments)
		  select(type by subtype?)
		    <simple-restart> =>
		      count := count + 1; simple-restart-count := simple-restart-count + 1;
		      let prefix = if (simple-restart-count = 1) "(continue) " else "" end if;
		      send-string(format-to-string("  %s: %s\n", count,
						   concatenate(prefix, apply(unpick-format-string, init-arguments))));
		    <abort> =>
		      count := count + 1; abort-count := abort-count + 1;
		      let prefix = if (abort-count = 1) "(abort) " else "" end if;
		      send-string(format-to-string("  %s: %s\n", count,
						   concatenate(prefix, apply(unpick-format-string, init-arguments))));
		    <restart> =>
		      count := count + 1;
		      send-string(format-to-string("  %s: %s\n", count, type));
		  end select;
	      end method);
  send-string("\nType c: followed by a number to proceed or type h: for other options\n");
end method print-restart-options;

define method idvm-restart-handler()
  print-restart-options();
  idvm-signal-error("debugger", #());
end method idvm-restart-handler;

define method idvm-restart-decliner(#rest args)
  send-string("\n Restart Handler Declined; Please retry or choose another option");
  idvm-restart-handler();
end method idvm-restart-decliner;


/// RESTART QUERIES


define class <query-restart>(<simple-restart>)
 slot value-to-use;
end class <query-restart>;

define method restart-query(condition :: <query-restart>)

  send-char(restart-query-signal);

  read-command();

  let (#rest returned-values)
    =  block(return-values)
	 read-and-process-forms(make(<doss-loader>, stream: idvm-read-stream()),
				return-values);
       end block;
  condition.value-to-use := (~ returned-values.empty? & returned-values.first);

  buffer-handshake-processor();

  send-char(end-of-stream-signal);
  
end method restart-query;


define method query-error(cond-or-string, #rest arguments)
      block ()
	apply(cerror, "Return Dummy Value(#f)", cond-or-string, arguments);
        #f;
      exception (condition :: <query-restart>, init-arguments: vector(format-string: "Return a value to use"))
	condition.value-to-use;
      end block;
end method query-error;

define constant idvm-restarter-commands =
  list(list(#"c",
	    "Execute specified option from restart menu",
	    method(#rest args)
		let cont = if (args.empty?) 1 else args.first end if;
		if (cont = 0) invoke-low-level-debugger(); end if;
		let count = 0;
		do-handlers(method(type, test, function, init-arguments)
				if (subtype?(type, <restart>))
				  count := count + 1;
				  if (cont = count)
				    let restart-obj = apply(make, type, init-arguments);
				    restart-query(restart-obj);
				    function(restart-obj, idvm-restart-decliner)
				  end if;
				end if;
			    end method);
	    end method),
       list(#"ERROR",
	    "list restart options",
	    print-restart-options),
       list(#"a",
	    "Quit debugger",
	    method(#rest args) abort(); end method));
