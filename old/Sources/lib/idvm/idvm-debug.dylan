Module:    IDVM
Language:  infix-dylan
Synopsis:  IDVM Instruction Set 3 (23/8/94)
Author:    Eliot Miranda, Tony Mann, Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// for the idvm-listener

define generic idvm-debugger-listener(commands :: <list>);

define variable *idvm-commands* = #();

define variable debugged-code = make(<table>);

define variable *idvm-report?* = #f;

// compiler Bug with duplicated "00000" in source

define variable *idvm-byte* = make(<string>, size: 5, fill: '0');

define class <continue-idvm>(<simple-restart>)
end class <continue-idvm>;

define idvm-method idvm-debugger ()
  let breaking? = idvm-break?(vm-code[0], ip);
  let tracing? = idvm-trace?(vm-code[0], ip);
    if (breaking? | tracing?)
      let print-args =
	method ()
	  let next = ip;
	  while (next < vm-code.size
		   & ~ (instance?(vm-code[next], <method>)
			  & (element(debug-to-normal-map, vm-code[next], default: #f)
			       | element(normal-to-debug-map, vm-code[next], default: #f))))
	    next := next + 1
	  end;
	  format-out("%s: %s %s\nresult: %s\nvars: %s\n",
		     ip - 1,
		     vm-code[ip - 1].debug-name,
		     copy-sequence(vm-code, start: ip, end: next),
		     result,
		     vars)
	end;

      if (ip == 2)
        let info = vm-code[0];

        format-out("calling %s %s with %s\n",
		   if (info.is-a-closure) "closure in method" else "method" end,
		   info.method-name,
		   copy-sequence(vars,start: 1, end: info.arg-count + 1));
      end;

      if (tracing?)
        print-args();
      end;

      if (breaking?)
	idvm-debugger-listener(add(idvm-debugger-commands,
				   list(#"v",
					"Print local-variable info of idvm-engine",
					method(#rest args) print-args(); end method)));
        if (~ tracing?)
          print-args();
        end if;
      end if; // breaking?
    end if;
end;

// HACK -- This piece of code is dependent on environment slot offset in <method>s

define constant idvm-code-vector =
  method(func :: <method>)
    let env = slot-element(func, 4);
    let cv =
    any?(method(obj)
           instance?(obj, <vector>)
           & ~ obj.empty?
           & instance?(obj[0], <vm-method-info>)
           & obj;
         end method,
         env);
    cv;
  end method;

define constant idvm-debugging-method? =
  method(info :: <vm-method-info>)
      info.vm-trace? | info.vm-trace-ins? | info.vm-break? | info.vm-step?;
  end method;

define constant idvm-trace? =
  method(info :: <vm-method-info>, ip :: <integer>)
      info.vm-trace-ins? | (info.vm-trace? & (ip = 2));
  end method;

define constant idvm-break? =
  method(info :: <vm-method-info>, ip :: <integer>)
      info.vm-step? | (info.vm-break? & (ip = 2));
  end method;

define constant idvm-decode-byte =
  method(byte :: <string>)
        values(element(byte, 0) = '1',
	       element(byte, 1) = '1',
	       element(byte, 2) = '1',
	       element(byte, 3) = '1',
	       element(byte, 4) = '1');
  end method;

define constant idvm-set-byte =
  method(byte :: <string>)
        element(*idvm-byte*, 0) := '1';
        for (i from 1 to 4)
          if (element(byte, i) = '1')
            element(*idvm-byte*, i) := '1'
          end if;
        end for;
  end method;

define constant idvm-unset-byte =
  method(byte :: <string>)
        for (i from 1 to 4)
          if (element(byte, i) = '1')
            element(*idvm-byte*, i) := '0'
          end if;
        end for;
        if (*idvm-byte* = "10000")
          element(*idvm-byte*, 0) := '0';
        end if;
  end method;

define constant idvm-print-debugged-methods =
  method(byte :: <string>)
      let (turn-on-debug?, break?, step?, trace?, trace-ins?) = idvm-decode-byte(byte);

      local method print-debugged-methods(getter :: <function>, s :: <string>)
	      format-out("\n----------IDVM METHODS WITH '%s' ENABLED-------------", s);
	      for(vm-code in debugged-code)
		if (vm-code[0].getter)
		  format-out("\n%s%s", vm-code[0].method-name, vm-code[0].parameter-type);
		end if;
	      end for;
	    end method print-debugged-methods;

      if (debugged-code.empty?)
	format-out("\nNOTHING is currently being debugged in the IDVM");
      else
	trace? & print-debugged-methods(vm-trace?, "TRACE");
	trace-ins? & print-debugged-methods(vm-trace-ins?, "TRACE-INSTRUCTIONS");
	break? & print-debugged-methods(vm-break?, "BREAK");
	step? & print-debugged-methods(vm-step?, "STEP");
      end if;
  end method;

define constant idvm-undebug-idvm-engine =
  method(byte :: <string>)
      
      idvm-unset-byte(byte);

      if (debugged-code.empty?)
	format-out("\nNOTHING is currently being debugged in the IDVM");
      else
	for(func in debugged-code.key-sequence)
	  debug-method(byte, func);
	end for;
      end if;
      
  end method;

define constant idvm-debug-idvm-engine =
  method(byte :: <string>)
      
      idvm-set-byte(byte);

  end method;


define method flip-idvm-inst-set (info :: <vm-method-info>, map :: <table>, status-symbol :: <symbol>,
				  #key start = 1, fin)
  let vm-code = info.vm-code-vec;

  for (i from start below fin | vm-code.size)
    let alter-ego = element(map,vm-code[i],default: #f);

    if (alter-ego)
        vm-code[i] := alter-ego;
        if (  alter-ego == idvm-make-closure
            | alter-ego == idvm-make-closure-copying
            | alter-ego == idvmd-make-closure
            | alter-ego == idvmd-make-closure-copying)
	  flip-idvm-inst-set(vm-code[i + 1], map, status-symbol, start: start, fin: fin)
        end
    end
  end;

  info.status := status-symbol;
end;

define constant print-debug-info =
  method(info :: <vm-method-info>)
      format-out("\n\t[BREAK: %s  STEP: %s  TRACE: %s  TRACE-INS: %s]\n",
		 info.vm-break?, info.vm-step?, info.vm-trace?, info.vm-trace-ins?);
  end method;

define constant debug-method =
  method(byte :: <string>,
	 func :: <function>,
	 #rest the-specializers)
      if (byte ~= "00000")
        let (turn-on-debug?, break?, step?, trace?, trace-ins?) = idvm-decode-byte(byte);

	     if (the-specializers.empty?
		   | (every?(\=, func.specializers, the-specializers)))

	       let vm-code = func.idvm-code-vector;
	       
	       if (vm-code
		     & ((break? & (vm-code[0].vm-break? ~= turn-on-debug?))
			  | (step? & (vm-code[0].vm-step? ~= turn-on-debug?))
			  | (trace? & (vm-code[0].vm-trace? ~= turn-on-debug?))
			  | (trace-ins? & (vm-code[0].vm-trace-ins? ~= turn-on-debug?))))
		 break? & (vm-code[0].vm-break? := turn-on-debug?);
		 step? & (vm-code[0].vm-step? := turn-on-debug?);
		 trace? & (vm-code[0].vm-trace? := turn-on-debug?);
		 trace-ins? & (vm-code[0].vm-trace-ins? := turn-on-debug?);
		 format-out("\n%s IDVM FUNCTION: %s %s", 
			    if (turn-on-debug?) "DEBUGGING" else "UNDEBUGGING" end if,
			    vm-code[0].method-name, func.specializers);
		 print-debug-info(vm-code[0]);

		 if (trace? | break?)
		   if (turn-on-debug? | ~ vm-code[0].idvm-debugging-method?)
		     flip-idvm-inst-set(vm-code[0],
					if (turn-on-debug?) normal-to-debug-map else debug-to-normal-map end if,
					if (turn-on-debug?) #"debug" else #"normal" end if,
					fin: 2);
		   end if;
		 end if;
		 if (trace-ins? | step?)
		     if (turn-on-debug? | (~ vm-code[0].vm-trace-ins? & ~ vm-code[0].vm-step?))
		       let start =
			 if (vm-code[0].vm-break? | vm-code[0].vm-trace?) 2 else 1 end if;
		       flip-idvm-inst-set(vm-code[0],
					  if (turn-on-debug?) normal-to-debug-map else debug-to-normal-map end if,
					  if (turn-on-debug?) #"debug" else #"normal" end if,
					  start: start);
		     end if;
		 end if;
		 if (turn-on-debug?)
		   element(debugged-code, func, default: #f) | (debugged-code[func] := vm-code);
		 else
		   if (~ vm-code[0].idvm-debugging-method?)
		     remove-key!(debugged-code, func);
		   end if;
		 end if;
	       end if;
	     end if;
      end if;
  end method;

define constant debug-function =
  method(byte :: <string>,
	 func :: <function>,
	 #rest the-specializers)
      do(method(func :: <function>)
	     apply(debug-method, byte, func, the-specializers);
	 end method,
	 select(func by instance?)
	   <generic-function> => func.generic-function-methods;
	   otherwise => vector(func);
	 end select);
  end method;

define constant idvm-print-code =
  method(func :: <function>, #rest the-specializers)
      do(method(func :: <function>)
	     if (the-specializers.empty?
		   | (every?(\=, func.specializers, the-specializers)))

	       let vm-code = func.idvm-code-vector;
	       if (vm-code)
		 format-out("\nIDVM CODE FOR %s %s\n", vm-code[0].method-name, func.specializers);
		 let ip = 2;
		 let fixed-str = make(<string>, size: 30, fill:' ');

		 while (ip <= vm-code.size)
		   let next = ip;
		   while (next < vm-code.size
			    & ~ (instance?(vm-code[next], <method>)
				   & (element(debug-to-normal-map, vm-code[next], default: #f)
					| element(normal-to-debug-map, vm-code[next], default: #f))))
		     next := next + 1
		   end while;
		   let mangled-opcode = copy-sequence(vm-code[ip - 1].debug-name, start: 10);
		   fill!(fixed-str, ' ');
		   let opcode = replace-subsequence!(fixed-str, mangled-opcode, end: mangled-opcode.size);
		   format-out("%s: \t%s \t\t\t%s\n",
			      ip - 1,
			      opcode,
			      copy-sequence(vm-code, start: ip, end: next));
		   ip := next + 1;
		 end while;
	       end if;
	     end if;
	 end method,
	 select(func by instance?)
	   <generic-function> => func.generic-function-methods;
	   otherwise => vector(func);
	 end select);
  end method;

define constant idvm-debug = 
  method (#rest args)
    if (args.empty?)
      idvm-print-debugged-methods("11111");
    else
      apply(debug-function, "11111", args);
    end if;
  end method;

define constant idvm-undebug =
  method (#rest args)
    if (args.empty?)
      idvm-undebug-idvm-engine("01111");
    else
      apply(debug-function, "01111", args);
    end if;
  end method;

define constant idvm-break =
  method (#rest args)
    if (args.empty?)
      idvm-print-debugged-methods("11000");
    else
      apply(debug-function, "11000", args);
    end if;
  end method;

define constant idvm-unbreak =
  method (#rest args)
    if (args.empty?)
      idvm-undebug-idvm-engine("01000");
    else
      apply(debug-function, "01000", args);
    end if;
  end method;

define constant idvm-step =
  method (#rest args)
    if (args.empty?)
      idvm-print-debugged-methods("10100");
    else
      apply(debug-function, "10100", args);
    end if;
  end method;

define constant idvm-unstep =
  method (#rest args)
    if (args.empty?)
      idvm-undebug-idvm-engine("00100");
    else
      apply(debug-function, "00100", args);
    end if;
  end method;

define constant idvm-trace =
  method (#rest args)
    if (args.empty?)
      idvm-print-debugged-methods("10010");
    else
      apply(debug-function, "10010", args);
    end if;
  end method;

define constant idvm-untrace =
  method (#rest args)
    if (args.empty?)
      idvm-undebug-idvm-engine("00010");
    else
      apply(debug-function, "00010", args);
    end if;
  end method;

define constant idvm-trace-ins =
  method (#rest args)
    if (args.empty?)
      idvm-print-debugged-methods("10001");
    else
      apply(debug-function, "10001", args);
    end if;
  end method;

define constant idvm-untrace-ins =
  method (#rest args)
    if (args.empty?)
      idvm-undebug-idvm-engine("00001");
    else
      apply(debug-function, "00001", args);
    end if;
  end method;

define constant idvm-debug-idvm = method(#rest args) idvm-debug-idvm-engine("11111"); end method;

define constant idvm-trace-idvm = method(#rest args) idvm-debug-idvm-engine("10010"); end method;

define constant idvm-trace-ins-idvm = method(#rest args) idvm-debug-idvm-engine("10001"); end method;

define constant idvm-break-idvm = method(#rest args) idvm-debug-idvm-engine("11000"); end method;

define constant idvm-step-idvm = method(#rest args) idvm-debug-idvm-engine("10100"); end method;

define constant idvm-commands =
  list(list(#"DEBUG",
	    "Without args:\tdisplay idvm methods with any debug enabled\n\t\t\t\t With args:\tenable all debug in specified idvm method",
	    idvm-debug),
       list(#"UNDEBUG",
	    "Disable all debug in idvm methods",
	    idvm-undebug),
       list(#"BREAK",
	    "Without args:\tdisplay idvm methods with break enabled\n\t\t\t\t With args:\tenable break in specified idvm method",
	    idvm-break),
       list(#"UNBREAK",
	    "Disable break in idvm methods",
	    idvm-unbreak),
       list(#"STEP",
	    "Without args:\tdisplay idvm methods with step enabled\n\t\t\t\t With args:\tenable step in specified idvm method",
	    idvm-step),
       list(#"UNSTEP",
	    "Disable step in idvm methods",
	    idvm-unstep),
       list(#"TRACE",
	    "Without args:\tdisplay idvm methods with trace enabled\n\t\t\t\t With args:\tenable trace in specified idvm method",
	    idvm-trace),
       list(#"UNTRACE",
	    "Disable trace in idvm methods",
	    idvm-untrace),
       list(#"TRACE-INS",
	    "Without args:\tdisplay idvm methods with instructions-trace enabled\n\t\t\t\t With args:\tenable instructions-trace in specified idvm method",
	    idvm-trace-ins),
       list(#"UNTRACE-INS",
	    "Disable instructions-trace in idvm methods",
	    idvm-untrace-ins),

       list(#"CODE",
	    "Print code for idvm methods",
	    idvm-print-code),

       list(#"DEBUG-IDVM",
	    "Enable all debug in idvm engine",
	    idvm-debug-idvm),
       list(#"TRACE-IDVM",
	    "Enable trace in idvm engine",
	    idvm-trace-idvm),
       list(#"TRACE-INS-IDVM",
	    "Enable instructions-trace in idvm engine",
	    idvm-trace-ins-idvm),
       list(#"BREAK-IDVM",
	    "Enable break in idvm engine",
	    idvm-break-idvm),
       list(#"STEP-IDVM",
	    "Enable step in idvm engine",
	    idvm-step-idvm));

define constant idvm-debugger-commands =
  list(list(#"g",
	    "Continue execution from idvm-debugger",
	    method(#rest args) signal(make(<continue-idvm>)); end method),
       list(#"q",
	    "Quit idvm-debugger",
	    method(#rest args) abort(); end method));

define constant idvm-report? =
 method(stream, format-string :: <string>, #rest args)
     *idvm-report?* & apply(format, stream, format-string, args);
 end method;

*idvm-commands* := concatenate(idvm-commands, *idvm-commands*);


