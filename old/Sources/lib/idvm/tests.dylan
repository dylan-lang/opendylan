Module: idvm
Language: infix-dylan
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define method time-fib-func(f :: <function>, #key start = 0);
    let i = start;
    let timer = make(<execution-timer>);
    let value = #f;

    while (not(user-time(timer)) | user-time(timer) <= 30.0)
        set-start(timer);
        value := f(i);
        set-end(timer);
        format-out("nfib(%d) = %d, nfibs: %d, time: %s\n",
                         i,     value,      block ()
		                               round(value / user-time(timer))
					    exception (<error>)
					       -1
					    end,
                                                         round(user-time(timer) * 1000.0) / 1000.0);
        i := i + 1;
    end while;
end method;

import-cl-functions (system(invoke-snm)(as: lisp/snfib),
                     system(typed-nfib)(as: lisp/tnfib));

// fibonacci:
define method native-nfib(n :: <integer>) => <integer>;
    if (n <= 1) 1 else native-nfib(n - 1) + native-nfib(n - 2) + 1 end
end;

define method singleton-native-nfib(n == 0) => <integer>; 1 end;
define method singleton-native-nfib(n == 1) => <integer>; 1 end;

define method singleton-native-nfib(n :: <integer>) => <integer>;
    singleton-native-nfib(n - 1) + singleton-native-nfib(n - 2) + 1
end;

/* emulated (evaluate this:)
define method emulated-nfib(n :: <integer>) => <integer>;
    if (n <= 1) 1 else emulated-nfib(n - 1) + emulated-nfib(n - 2) + 1 end
end;

define method es-nfib(n == 0) => <integer>; 1 end;
define method es-nfib(n == 1) => <integer>; 1 end;

define method es-nfib(n :: <integer>) => <integer>;
    es-nfib(n - 1) + es-nfib(n - 2) + 1
end;
*/

define method native-sum(col :: <collection>)
    let sum = 0;
    map(method (i :: <integer>) sum := sum + i end, col);
    sum
end;
/*
define method emulated-sum(col :: <collection>)
    let sum = 0;
    map(method (i :: <integer>) sum := sum + i end, col);
    sum
end;
*/

define method native-detect(m :: <method>, col :: <collection>)
   block (return)
       do(method(o) if (m(o)) return(o) end end, col);
       #"not-found"
   end
end;


define generic nfib(n :: <integer>);

define generic sum (c :: <collection>);

define generic detect (m :: <method>, c :: <collection>);

define generic worrisit?(n :: <number>) => <string>;

define generic test-rest(#rest all) => <s-o-v>;

define generic test-keys(#key dummy, #all-keys) => <s-o-v>;

define generic hbt(m :: <integer>,n :: <integer>);

define generic test-multiple-values(c :: <collection>) => <vector>;

define method make-tests ()

/* singleton specialisers */
add-method(worrisit?,
           build-vm(vector(singleton(0)),
                    vector(#f, IDVM-return-lit,#"zero"),
                    0,
                    method-name: #"worrisit?"));
add-method(worrisit?,
           build-vm(vector(singleton(1)),
                    vector(#f, IDVM-return-lit,#"one"),
                    0,
                    method-name: #"worrisit?"));
add-method(worrisit?,
           build-vm(vector(<integer>),
                    vector(#f, IDVM-lit-br-ge, 1, 0, 5,
                 IDVM-return-lit,#"negative",
                 IDVM-return-lit,#"a lot"),
                 1,
                 method-name: #"worrisit?"));

/* nfib */
add-method(nfib,
           build-vm(vector(<integer>),
                    vector(#f,
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
                           IDVM-return),
                    3,
                    method-name: #"nfib"));

/* minimal rest test */
add-method(test-rest,build-vm(vector(),vector(#f, IDVM-return-loc,1),2,uses-rest: #t));

/* minimal key test */
add-method(test-keys,build-vm(vector(),vector(#f, IDVM-call-loc-loc-returning, vector, hilo(2,3)),
                               3,key-value-pairs: vector(foo:,"foo",bar:,"bar")));

/* closures */

  /*    define method detect(m :: <method>, col :: <collection>)
          block (return)
            do(method(o) if (m(o)) return(o) end end, col);
            #"not-found"
          end
        end;
   */

add-method(detect,
 build-vm(vector(<method>,<collection>),
          vector(#f, 
                 IDVM-bind-exit-returning,      3,   //  1 exit procedure in local 3
                 IDVM-make-closure-copying,          //  3
                     make(<vm-method-info>,
                          parameters: vector(<object>),
                          stack-size: 2,
                          closed-var-indices: #[ 1, 3 ], /* m & return */
                          vm-code-vec: vector(
                              IDVM-res-gets-ev, 0,             //  1 res := m (the method)
                              IDVM-rescall-loc,  1,            //  3 m(o)
                              IDVM-jump-true,   2,             //  5
                              IDVM-return,                     //  7
                              IDVM-res-gets-ev, 1,             //  8 res := exit procedure
                              IDVM-rescall-loc-returning, 1)), // 10 call exit procedure with o
                 IDVM-loc-gets-res, 4,               //  5 set up closure in call to do
                 IDVM-call-loc-loc, do, hilo(4,2),   //  7 call do(m,c)
                 IDVM-return-lit, #"not-found"),     // 10 return not-found (returns to bind-exit inst)
           8,
           method-name: #"detect"));

/* handler-bind */
add-method(hbt,
  build-vm(vector(<integer>,<integer>),
           vector(#f,
                  IDVM-handler-bind-lit, <error>, hilo(3,5),
                  IDVM-call-loc-loc-returning, \/, hilo(1, 2),
                  IDVM-return-lit, "what a f***-*p!",
                  IDVM-call-res-returning, vector),
           2,
           method-name: #"hbt"));

/* multiple values */
/* define method native-test-multiple-values(c :: <collection>)
       let (l-initial-state,
            l-limit,
            l-next-state,
            l-finished-state?,
            l-current-key,
            l-current-element,
            l-current-element-setter,
            l-copy-state) = c.forward-iteration-protocol;

     vector(l-initial-state,
            l-limit,
            l-next-state,
            l-finished-state?,
            l-current-key,
            l-current-element,
            l-current-element-setter,
            l-copy-state)
   end method;
 */
add-method(test-multiple-values,
 build-vm(vector(<collection>),
          vector(#f,
                 IDVM-mv-bind, 5, hilo(2,8),
                 IDVM-call-loc-returning, forward-iteration-protocol, 1,
                 IDVM-call-n-returning, vector, hilo(8,2)),
           16,
           method-name: #"test-multiple-values"));

end;

define method run-tests()

    format-out("map(worrisit?,range(from: -3, up-to: 4)) => %s\n\n",
           block ()
               map(worrisit?,range(from: -3, up-to: 4));
           exception (<error>)
               #"failed"
           end);

    format-out("nfib(10) => %s\n\n",
           block ()
               nfib(10);
           exception (<error>)
               #"failed"
           end);

    format-out("test-rest(1,2,3) => %s\n\n",
           block ()
               test-rest(1,2,3);
           exception (<error>)
               #"failed (probably because of parameter list congruency rules)"
           end);

    format-out("test-keys(foo: 1, bar: 2, foo: 3) => %s\n\n",
           block ()
               test-keys(foo: 1, bar: 2, foo: 3);
           exception (<error>)
               #"failed"
           end);
 
    format-out("detect(odd?,range(up-to: 100, by: 2)) => %s\n\n",
           block ()
               detect(odd?,range(up-to: 100, by: 2));
           exception (<error>)
               #"failed"
           end);

    format-out("detect(odd?,range(up-to: 100, by: 1)) => %s\n\n",
           block ()
               detect(odd?,range(up-to: 100, by: 1));
           exception (<error>)
               #"failed"
           end);

    format-out("hbt(10,2) => %s\n\n",
           block ()
               hbt(10,2);
           exception (<error>)
               #"failed"
           end);

    format-out("hbt(1,0) => %s\n\n",
           block ()
               hbt(1,0);
           exception (<error>)
               #"failed"
           end);

    format-out("test-multiple-values(range(up-to: 1)) => %s\n\n",
           block ()
               test-multiple-values(range(up-to: 1));
           exception (<error>)
               #"failed"
           end);
end method;
