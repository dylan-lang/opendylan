Module: idvm-tests
Synopsis:  Define tests for the In-Dylan Virtual Machine (IDVM)
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define constant hilo =
  method(hi, lo)
      lo + ash(hi, 16);
  end method;

define constant dummy-test = method() format-out("\nNOTHING TO TEST\n"); end method;

/// GENERICS

define generic vm-nfib(n :: <integer>);

define generic detect (m :: <function>, c :: <collection>);

/// METHODS

define variable *test-up* = dummy-test;


/// CLASSES

define class <a>(<object>)
 slot slot1 :: <string>, required-init-keyword: slot1:;
 slot slot2 :: <string>, required-init-keyword: slot2:;
end class <a>;

define class <b>(<a>)
 slot slot3 :: <string>, required-init-keyword: slot3:;
 slot slot4 :: <string>, required-init-keyword: slot4:;
end class <b>;

define class <c>(<b>)
 slot slot5 :: <string>, required-init-keyword: slot5:;
 slot slot6 :: <string>, required-init-keyword: slot6:;
end class <c>;


define method make-tests ()

/// hard-wired compiler-generated idvm-code using the idvm back-end


/* nfib

define method vm-nfib(n :: <integer>)
 if (n < 2)
  1;
 else
  1 + vm-nfib(n - 1) + vm-nfib(n - 2);
 end if;
end method;
*/

  add-method(vm-nfib,
	     build-vm(vector(<integer>),
		      vector(#f,
			     idvm-call-loc-lit,             \<,      1,     2,
			     idvm-loc-gets-res,             0,
			     idvm-res-gets-lit,             #f,
			     idvm-res-loc-br-idne,          0,       27,
			     idvm-call-loc-lit,             \-,      1,     1,
			     idvm-call-res,                 vm-nfib,
			     idvm-loc-gets-res,             0,
			     idvm-call-lit-loc,             \+,      1,     0,
			     idvm-loc-gets-res,             0,
			     idvm-call-loc-lit,             \-,	     1,	    2,
			     idvm-call-res,                 vm-nfib,
			     idvm-loc-gets-res,             2,
			     idvm-call-loc-loc-returning,   \+,	     hilo(0, 2),
			     idvm-loc-gets-lit,             0,       1,
			     idvm-return-loc,               0),
		      32,
		      method-name: #"vm-nfib"));

/* closures

define method detect(m :: <method>, col :: <collection>)
  block (return)
    do(method(o) if (m(o)) return(o) end end, col);
    #"not-found"
  end
end;
*/

  add-method(detect,
	     build-vm(vector(<function>,<collection>),
		      vector(#f,
			     idvm-bind-exit-returning,         0,
			     idvm-make-closure-copying, 	
			     make(<vm-method-info>,
				  parameters: vector(<object>),
				  stack-size: 32,
				  closed-var-indices: #[ 0, 1 ],
				  vm-code-vec:
				    vector(#f,
					   idvm-res-gets-ev,              	1,
					   idvm-rescall-loc,              	1,
					   idvm-loc-gets-res,             	2,
					   idvm-res-gets-lit,             	#f,
					   idvm-res-loc-br-idne,          	2,	5,
					   idvm-res-gets-lit,             	#f,
					   idvm-return,                   
					   idvm-res-gets-ev,              	0,
					   idvm-rescall-loc,              	1,
					   idvm-jump,                     	-6)),
			     idvm-loc-gets-res,             	0,
			     idvm-call-loc-loc,             	do,	  hilo(0, 2),
			     idvm-loc-gets-lit,             	0,	  #"not-found",
			     idvm-return-loc,               	0),
		      32,
		      method-name: #"detect"));

/* Bind Exit, Unwind-protect

(print 
 (bind-exit (return)
  (unwind-protect
      (+ (return 10) 2)
    (print 9)
    (return 20)
    (print 0)
    30)))
*/

  *test-up* :=
    build-vm(vector(),
	     vector(#f,
		    idvm-bind-exit,                	hilo(23,0),
		    idvm-unwind-protect,           	hilo(11,21),
		    idvm-res-gets-loc,             	0,
		    idvm-rescall-lit,              	10,
		    idvm-loc-gets-res,             	1,
		    idvm-call-loc-lit-returning,   	\+,	1,	2,
		    idvm-call-lit,                 	print,	9,
		    idvm-res-gets-loc,             	0,
		    idvm-rescall-lit,              	20,
		    idvm-call-lit-returning,       	print,	0,
		    idvm-call-res-returning,       	print),
	     32,
	     method-name: top-level-form:);


/* Class hierarchy, next-method, generic-function dispatch

define method print(object :: <a>, #rest keys)
 format-out("\nOBJECT %s:\n", object.object-class);
 format-out("\n SLOT slot1: %s \n", object.slot1);
 format-out("\n SLOT slot2: %s \n", object.slot2);
 object;
end method print;

define method print(object :: <b>, #rest keys)
 format-out("\nOBJECT %s:\n", object.object-class);
 format-out("\n SLOT slot3: %s \n", object.slot3);
 format-out("\n SLOT slot4: %s \n", object.slot4);
 next-method();
 object;
end method print;

define method print(object :: <c>, #rest keys)
 format-out("\nOBJECT %s:\n", object.object-class);
 format-out("\n SLOT slot5: %s \n", object.slot5);
 format-out("\n SLOT slot6: %s \n", object.slot6);
 next-method();
 object;
end method print;
*/

  add-method(print,
	     build-vm(vector(<a>),
		      vector(#f,
			     idvm-call-loc,                 	object-class,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\nOBJECT %s:\n",	0,
			     idvm-call-loc,                 	slot1,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\n SLOT slot1: %s \n",	0,
			     idvm-call-loc,                 	slot2,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc-returning,   	format-out,	"\n SLOT slot2: %s \n",	0),
		      32,
		      uses-rest: #t,
		      method-name: #"print"));

  add-method(print,
	     build-vm(vector(<b>),
		      vector(#f,
			     idvm-loc-gets-res,             	3,
			     idvm-call-loc,                 	object-class,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\nOBJECT %s:\n",	0,
			     idvm-call-loc,                 	slot3,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\n SLOT slot3: %s \n",	0,
			     idvm-call-loc,                 	slot4,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\n SLOT slot4: %s \n",	0,
			     idvm-res-gets-loc,             	3,
			     idvm-rescall-returning),
		      32,
		      uses-next-method: #t,
		      uses-rest: #t,
		      method-name: #"print"));

  add-method(print,
	     build-vm(vector(<c>),
		      vector(#f,
			     idvm-loc-gets-res,             	3,
			     idvm-call-loc,                 	object-class,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\nOBJECT %s:\n",	0,
			     idvm-call-loc,                 	slot5,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\n SLOT slot5: %s \n",	0,
			     idvm-call-loc,                 	slot6,	1,
			     idvm-loc-gets-res,             	0,
			     idvm-call-lit-loc,             	format-out,	"\n SLOT slot6: %s \n",	0,
			     idvm-res-gets-loc,             	3,
			     idvm-rescall-returning),
		      32,
		      uses-next-method: #t,
		      uses-rest: #t,
		      method-name: #"print"));






end method make-tests;

define method run-tests()

/* nfib */

  format-out("\n*****TESTING NFIB\n");
  do(method(i)
	 format(*standard-output*, "nfib ~S => ~S~%", i, vm-nfib(i));
     end method,
     range(to: 20));


/* closures */

  format-out("\n*****TESTING CLOSURES\n");
  format-out("detect(odd?,range(to: 100, by: 2)) => ~a~%~%",
	     block ()
	       detect(odd?,range(to: 100, by: 2));
	     exception (<error>)
	       #"failed"
	     end);

  format-out("detect(odd?,range(to: 100, by: 1)) => ~a~%~%",
	     block ()
	       detect(odd?,range(to: 100, by: 1));
	     exception (<error>)
	       #"failed"
	     end);


/* Bind Exit, Unwind-protect */

  format-out("\n*****TESTING BIND-EXIT, UNWIND-PROTECT\n");
  *test-up*();


/* Class hierarchy, next-method, generic-function dispatch */


  format-out("\n*****TESTING CLASS HIERARCHY, GENERIC-FUNCTION DISPATCH\n");
  format-out("\n TEST 1: print <a>\n");
  print(make(<a>, slot1: "first slot", slot2: "second slot"));
  
  format-out("\n TEST 2: print <b>\n");
  print(make(<b>, slot1: "first slot", slot2: "second slot",
	     slot3: "third slot", slot4: "fourth slot"));

  format-out("\n TEST 3: print <c>\n");
  print(make(<c>, slot1: "first slot", slot2: "second slot",
	     slot3: "third slot", slot4: "fourth slot",
	     slot5: "fifth slot", slot6: "sixth slot"));



end method run-tests;

make-tests();

run-tests();

/* RESULTS:


*****TESTING NFIB

nfib 0 => 1

nfib 1 => 1

nfib 2 => 3

nfib 3 => 5

nfib 4 => 9

nfib 5 => 15

nfib 6 => 25

nfib 7 => 41

nfib 8 => 67

nfib 9 => 109

nfib 10 => 177

nfib 11 => 287

nfib 12 => 465

nfib 13 => 753

nfib 14 => 1219

nfib 15 => 1973

nfib 16 => 3193

nfib 17 => 5167

nfib 18 => 8361

nfib 19 => 13529

nfib 20 => 21891



*****TESTING CLOSURES

detect(odd?,range(to: 100, by: 2)) => not-found



detect(odd?,range(to: 100, by: 1)) => 1





*****TESTING BIND-EXIT, UNWIND-PROTECT

920

*****TESTING CLASS HIERARCHY, GENERIC-FUNCTION DISPATCH



 TEST 1: print <a>



OBJECT [<class> idvm_testsXidvm_testsX_L_a_G_@]:



 SLOT slot1: first slot 



 SLOT slot2: second slot 



 TEST 2: print <b>



OBJECT [<class> idvm_testsXidvm_testsX_L_b_G_@]:



 SLOT slot3: third slot 



 SLOT slot4: fourth slot 



OBJECT [<class> idvm_testsXidvm_testsX_L_b_G_@]:



 SLOT slot1: first slot 



 SLOT slot2: second slot 



 TEST 3: print <c>



OBJECT [<class> idvm_testsXidvm_testsX_L_c_G_@]:



 SLOT slot5: fifth slot 



 SLOT slot6: sixth slot 



OBJECT [<class> idvm_testsXidvm_testsX_L_c_G_@]:



 SLOT slot3: third slot 



 SLOT slot4: fourth slot 



OBJECT [<class> idvm_testsXidvm_testsX_L_c_G_@]:



 SLOT slot1: first slot 



 SLOT slot2: second slot 


*/