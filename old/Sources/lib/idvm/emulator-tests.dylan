module:    idvm
Synopsis:  Some tests that do useful things in the emulator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



//// A useful macro for defining a test via IDVM assembler

define macro idvm-assembler-definer
  { define idvm-assembler ?name:name (?params:*) (?size:expression)
      ?assembler:*
    end }
    => { define constant ?name 
           = method (#rest r) 
               apply(?name ## "-inner", r)
             end;
         define constant ?name ## "-inner" 
           = build-vm(vector(?params),
                      vector(#f, ?assembler),
                      ?size,
                      method-name: ?"name") }
end macro;



/// Some patches to make the debugging stuff work in the emulator


import-cl-functions
 ( sys(constants-vector)        (as: constants-vector),
   sys(symbol-constants$symbol) (as: symbol-constants),
   cl(format)                   (as: cl-format));

define method specializers (x) x end;




define method emulator-debug-name (meth :: <function>)
  let full-name 
    = cl-format(#(), "~s", element(debug-to-normal-map, meth, default: meth));
  copy-sequence(full-name, start: 59, end: full-name.size - 2);
end method;

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
	  format-out("%s: %s %s\n  result: %s\n  vars: %s\n",
		     ip - 1,
		     vm-code[ip - 1].emulator-debug-name,
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
    let env = symbol-constants(func);
    if (instance?(env, <pair>)) env := env.tail end;
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


////// Start of real tests


define method show (x)
  format-out("%= ", x);
  x
end;




define idvm-assembler nfib (<integer>) (3)
    IDVM-lit-br-gt, 1, 1, 5,          // n > 1 ?
    IDVM-return-lit, 1,               // no - return 1
  // lab:
    IDVM-lit-sub, 1, 1,               // res := n - 1 for nfib(n-1)
    IDVM-call-res, nfib,              // call nfib(n-1)
    IDVM-loc-gets-res, 2,             // loc(2) := nfib(n-1)
    IDVM-lit-sub, 1, 2,               // res := n - 2 for nfib(n-2)
    IDVM-call-res, nfib,              // call nfib(n-2)
    IDVM-res-loc-add, 2,              // res := nfib(n-2) + nfib(n-1)
    IDVM-res-lit-add, 1,              // res := res + 1
    IDVM-return
end idvm-assembler;




define idvm-assembler loop (<integer>) (3)
    IDVM-lit-br-gt, 1, 1, 5,          // n > 1 ?
    IDVM-return-lit, 1,               // no - return 1
  // lab:
    IDVM-lit-sub, 1, 1,               // res := n - 1 for show(n-1)
    IDVM-call-res, show,              // call show(n-1)
    IDVM-loc-gets-res, 1,             // loc(1) := show(n-1)
    IDVM-jump, -14                    // Back to the beginning 
end idvm-assembler;

// loop(1000) will stack overflow on LW (with stack size 48000), if run as a threaded
// code VM




define idvm-assembler detect (<method>, <collection>) (8)
  IDVM-bind-exit-returning,      3,   //  1 exit procedure in local 3
  IDVM-make-closure-copying,          //  3
  make(<vm-method-info>,
       parameters: vector(<object>),
       stack-size: 2,
       closed-var-indices: #[ 1, 3 ], /* m & return */
       vm-code-vec: vector(#f,
                           IDVM-res-gets-ev, 0,             //  1 res := m (the method)
                           IDVM-rescall-loc,  1,            //  3 m(o)
                           IDVM-jump-true,   2,             //  5
                           IDVM-return,                     //  7
                           IDVM-res-gets-ev, 1,             //  8 res := exit procedure
                           IDVM-rescall-loc-returning, 1)), // 10 call exit procedure with o
  IDVM-loc-gets-res, 4,               //  5 set up closure in call to do
  IDVM-call-loc-loc, do, hilo(4,2),   //  7 call do(m,c)
  IDVM-return-lit, #"not-found"
end idvm-assembler;



define idvm-assembler test-multiple-values (<collection>) (16)
  IDVM-mv-bind, 5, hilo(2,8),
  IDVM-call-loc-returning, forward-iteration-protocol, 1,
  IDVM-call-n-returning, vector, hilo(7,2)
end;
