module: idvm-listener
Synopsis:  Design a native listener interface to the In-Dylan Virtual Machine (IDVM)
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant end-of-stream-signal = as(<character>, 127);
define constant ready-to-receive-data-signal = as(<character>, 128);
define constant error-signal = as(<character>, 129);
define constant abort-remote-listener-signal = as(<character>, 130);
define constant abort-remote-evaluator-signal = as(<character>, 131);
define constant restart-query-signal = as(<character>, 132);

define class <idvm-listener>(<object>)
 slot commands-table :: <list>, init-value: #(), init-keyword: commands:;
 slot listener-level :: <integer>, init-value: -1, init-keyword: level:;
end class <idvm-listener>;

define variable *idvm-listener* = make(<idvm-listener>);

define method wrapped-load-code-from-tcp-stream () => ();

  let handler (<serious-condition>) = condition-handler;
  block()
    idvm-listener(*idvm-commands*);
  exception(<end-of-stream-error>)
  end block;

end method wrapped-load-code-from-tcp-stream;

define method idvm-listener(commands :: <list>)
  let s = idvm-read-stream();
  fluid-bind(*idvm-listener* =
	       make(<idvm-listener>,
		    commands: add-idvm-listener-commands(commands),
		    level: *idvm-listener*.listener-level + 1))
    while (#t)
      idvm-listener-command-reader(s);
    end while;
  end fluid-bind;
end method idvm-listener;

define method idvm-listener-command-reader(s :: <tcp-stream>)
  block(exit)
    idvm-listener-command-evaluator(make(<doss-loader>, stream: s),
				    idvm-find-command(as(<symbol>,
							 as(<byte-string>, read-command()))),
				    exit);
  end block;
end method idvm-listener-command-reader;

define method idvm-listener-command-evaluator(loader :: <doss-loader>, command, exit :: <function>)
  if (command)
    loader.stream.input-buffer.buffer-end := 0;
    let (#rest returned-values)
      =  block(return-values)
	   read-and-process-forms(loader, return-values);
         exception(<abort>,
		   init-arguments:
                     vector(format-string:
                       format-to-string("Return to level %s", 
                                        *idvm-listener*.listener-level)))
	   flush-socket();
	   loader.stream.input-buffer.buffer-position := loader.stream.input-buffer.buffer-end;
	   send-char(abort-remote-evaluator-signal);
	   exit();
         end block;
  
    apply(command, returned-values);

    // the last buffer-handshake must be done here
    buffer-handshake-processor();

    send-char(end-of-stream-signal);
  
  else
    format-out("\nCOMMAND NOT RECOGNIZED BY IDVM");
    read-command();
    flush-socket();
    send-char(abort-remote-evaluator-signal);
  end if;
  
end method idvm-listener-command-evaluator;

define method read-and-process-forms(loader :: <doss-loader>, return-values :: <function>)
    let remembered-values = #[];
    let handler (<end-of-doss-stream>) 
       = method (cond :: <end-of-doss-stream>, next-handler)
           apply(return-values, remembered-values);
         end method;
    let (#rest the-values) = loader.fetch-object;
    remembered-values := the-values;
    while (#t)
      let (#rest the-values) = loader.fetch-next-object;
      remembered-values := the-values;
    end while;
end method;

define method idvm-signal-error(prompt :: <string>, commands :: <list>)
 let commands = concatenate(idvm-restarter-commands, commands);
  send-char(error-signal);
  format-out(":%s:%s", prompt, *idvm-listener*.listener-level + 1);
  send-char(end-of-stream-signal);
  block()
    idvm-listener(commands);
  cleanup
    send-char(abort-remote-listener-signal);
  end block;
end method idvm-signal-error;
      
define method idvm-read-stream()
  make(<tcp-stream>, element-type: <byte>, direction: #"input",
       input-buffer-handshake: buffer-handshake-processor);
end method idvm-read-stream;

define constant condition-handler =
  method (condition :: <serious-condition>, next-handler)

    send-string(format-to-string("\nError: %s\n", condition.object-class));

    select(condition by instance?)
      <simple-condition> =>
	send-string(apply(format-to-string, condition.condition-format-string, condition.condition-format-arguments));
      otherwise => #f;
    end select;

    idvm-restart-handler();

    #f
  end method;

define method idvm-debugger-listener(commands :: <list>)
  block()
    idvm-signal-error("idvm-debugger", commands);
  exception(<continue-idvm>,
	    init-arguments: vector(format-string:
				  format-to-string("Continue execution from idvm-debugger:%s",
						     idvm-listener-level() + 1)))
  end block;
end method idvm-debugger-listener;

define constant buffer-handshake-processor =
  method(#key arg)
      send-char(ready-to-receive-data-signal);
  end method;

define constant kill-idvm-engine =
  method(#rest args)
      format-out("\nEXITING REMOTE IDVM ENGINE");
      buffer-handshake-processor();
      send-char(end-of-stream-signal);
      signal(make(<end-of-stream-error>, stream: #f));
  end method;

define constant invoke-low-level-debugger =
  method(#rest args)
      format-out("\nENTERING REMOTE WINDOWS DEBUGGER...");
      primitive-break();
  end method;

define variable *idvm-values* = make(<vector>, size: 3, fill: #[]);

define constant values-fetcher =
 method(n :: <integer>)
     if (n < 0 | n > 2)
       format-out("\nlast-values: number out of range");
     else
      *idvm-values*[n];
     end if;
 end method;

last-values := values-fetcher;

define method idvm-find-command(command :: <symbol>)
  block(return)
    for (command-record in *idvm-listener*.commands-table)
      if (command-record.first = command)
	return(command-record.third)
      end if;
    end for;
    return(#f);
  end block;
end method idvm-find-command;

define constant idvm-commands-table =
 method()
     *idvm-listener*.commands-table
 end method;

define constant idvm-listener-level =
 method()
     *idvm-listener*.listener-level
 end method;

define constant add-idvm-listener-commands =
  method(idvm-commands :: <list>)
      reduce(add,
	     *idvm-listener*.commands-table,
	     idvm-commands);
  end method;

define constant idvm-commands =
  list(list(#"h",
	    "Display help info for idvm engine",
	    method(#rest args)
		let fixed-str = make(<string>, size: 25, fill:' ');

		local method print-help-line(first :: <symbol>, second :: <string>)
			let first = as(<string>, first);
			format-out("\n\t%s%s",
				   replace-subsequence!(fixed-str, first, end: first.size),
				   second);
			fill!(fixed-str, ' ');
		      end method print-help-line;

		let already-seen = #();
		for (command in idvm-commands-table())
		  unless(command.second.empty?)
		    if (~ member?(command.first, already-seen))
		      already-seen := add(already-seen, command.first);
		      print-help-line(command.first, command.second);
		    end if;
		  end unless;
		end for;
	    end method),

       list(#"REPORT",
	    "Turn on compiler and runtime reporting",
	    method(#rest args)
		*idvm-report?* := #t;
		*namespace-debug-print* := #t;
	    end method),
       
       list(#"UNREPORT",
	    "Turn off compiler and runtime reporting",
	    method(#rest args)
		*idvm-report?* := #f;
		*namespace-debug-print* := #f;
	    end method),

       list(#"KILL",
	    "Kill running idvm engine",
	    kill-idvm-engine),
       
       list(#"VALUES",
	    "Evaluate a dylan form(DEFAULT COMMAND)",
	    method(#rest returned-values)
		*idvm-values*[2] := *idvm-values*[1];
		*idvm-values*[1] := *idvm-values*[0];
		*idvm-values*[0] := returned-values;
		$$$ := ~ *idvm-values*[2].empty? & *idvm-values*[2].first;
		$$ := ~ *idvm-values*[1].empty? & *idvm-values*[1].first;
		$ := ~ *idvm-values*[0].empty? & *idvm-values*[0].first;
		for (v in returned-values)
		  print('\n');
		  print(v);
		end for;
		print('\n');
	    end method),

       list(#"d",
	    "Invoke low-level debugger",
	    invoke-low-level-debugger),

       list(#"LOAD-LIB",
            "",
            method(#rest args)
		unless(member?(args.first, *idvm-library-namespace*, test: \=))
		  *idvm-library-namespace* := add(*idvm-library-namespace*, args.first);
		end unless;
	    end method),

       list(#"LOADED",
            "Display libraries loaded into the idvm",
            method(#rest args)
		format-out("\nLoaded Libraries: ");
		for(library in *idvm-library-namespace*)
		  format-out("%s  ", library);
		end for;
		print('\n');
	    end method),

       list(#"LOADED?",
            "Determine if a library is loaded into idvm",
            method(#rest args)
		unless(args.empty?)
		  buffer-handshake-processor();
		  if (member?(args.first, *idvm-library-namespace*, test: \=))
		    format-out("#t");
		  else
		    format-out("#f");
		  end if;
		end unless;
            end method));

*idvm-commands* := concatenate(idvm-commands, *idvm-commands*);
 
*idvm-library-namespace* := idvm-namespace.key-sequence;

