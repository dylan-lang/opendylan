Module:    parameter-type-tests
Synopsis:  Test the "define custom-interface" macro.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define macro interface-test-definer
  { 
    define interface-test ?iface-type:name ?:name (?supers:*)
      ?tests
    end
  } => {
    define interface-test-interface ?iface-type ?name (?supers)
      ?tests
    end;
    distribute-over-semicolons(\define-interface-test-method, 
			       ("<S-" ## ?name ## ">"), (?tests));
    define function "test-" ## ?name (iface :: "<C-" ## ?name ## ">")
      distribute-over-semicolons(\interface-test-tester, 
				 (iface), (?tests));
    end;
  }

  tests:
    { } => { }
    { parameter ?:name \:: ("<" ## ?ctype:name ## ">") = (?values:*); ... }
    => { parameter ?name :: ("<" ## ?ctype ## ">", "<" ## ?ctype ## "*>")
		 = (?values); ... }
    { ?rest:*; ... } => { ?rest; ... }
end;


define macro interface-test-interface-definer
  {
    define interface-test-interface ?iface-type:name ?:name (?supers:*)
      ?members
    end
  } => {
    define ?iface-type "<S-" ## ?name ## ">" (?supers)
      client-class "<C-" ## ?name ## ">";
      ?members
    end;
  }

  members:
    { } => { }
    { 
      parameter ?:name \:: (?ctype:*, ?ptype:*) = (?vals:*); 
      ... 
    } => { 
      member-function "test-iparm-" ## ?name (p :: ?ctype) => (); 
      member-function "test-oparm-" ## ?name (p :: out-ref(?ctype)) => ();
      // Because of Bug ####, hack:
      // member-function "test-ioparm-" ## ?name (p :: inout-ref(?ctype)) => ();
      member-function "test-ioparm-" ## ?name (p :: ?ptype) => ();
      ... 
    }
    { test ?rest:*; ... } => { ... }
    { ?other:*; ...  } => { ?other; ... }
end;


define function oeq (a, b) => (r :: <HRESULT>)
  if (a = b) $S-OK else $S-FALSE end if
end;


define macro coe
  { coe(?:body) } => { block() ?body exception (<error>) $CO-E-ERRORINAPP end }
end;


define macro define-interface-test-method
  {
    define-interface-test-method((?class:name), 
      (parameter ?:name \:: (?ctype:*, ?ptype:*) = 
	(?v1:expression, ?v2:expression)))
  } => {
    define method "test-iparm-" ## ?name (this :: ?class, p) => (r :: <HRESULT>)
      coe(oeq(p, ?v1))
    end;
    define method "test-oparm-" ## ?name (this :: ?class, p) => (r :: <HRESULT>)
      coe(p.pointer-value := ?v2; $S-OK)
    end;
    define method "test-ioparm-" ## ?name (this :: ?class, p) =>(r :: <HRESULT>)
      coe(
	let r = oeq(pointer-value(p), ?v2);
	p.pointer-value := ?v1; r
      )
    end;
  }

  { define-interface-test-method((?:name), (?other:*)) } => { }
end;


define macro distribute-over-semicolons
  { distribute-over-semicolons(?macro:name, (?distributee:*), ()) } => { }

  { 
    distribute-over-semicolons(?macro:name, (?distributee:*), 
			       (?item:*; ?rest:*))
  } => {
    ?macro ## ""((?distributee), (?item));
    distribute-over-semicolons(?macro, (?distributee), (?rest))
  }
end macro distribute-over-semicolons;


define function check-OK (name :: <string>, r :: <HRESULT>) => ()
  check-equal(name, r, $S-OK);
end;


define function parameter-test 
	(name :: <string>, iface,
	 ifunc :: <function>, ofunc :: <function>, iofunc :: <function>, 
	 ctype :: <type>, ptype :: <type>, v1, v2) => ()
  check-OK(format-to-string("Input parameter %s; C type: %=, value: %=",
			    name, ctype, v1),
	   ifunc(iface, v1));
  with-stack-structure (out-ptr :: ptype)
    check-OK(format-to-string("Output parameter %s; C type: %=", name, ctype),
	     ofunc(iface, out-ptr));
    check-equal(format-to-string("Output parameter %s; C type: %=", 
				 name, ctype),
		out-ptr.pointer-value, v2);
  end;
  with-stack-structure (inout-ptr :: ptype)
    check-no-errors(format-to-string("Inout parameter %s: C type: %=, "
			       "setting in value: %=", name, ctype, v2),
		    inout-ptr.pointer-value := v2);
    check-OK(format-to-string("In-out parameter %s; C type: %=, value: %=", 
			      name, ctype, inout-ptr.pointer-value),
	     iofunc(iface, inout-ptr));
    check-equal(format-to-string("In-out parameter %s; C type: %=", 
				 name, ctype),
		inout-ptr.pointer-value, v1);
  end;
end;


define macro interface-test-tester
  {
    interface-test-tester((?iface:expression),
      (parameter ?:name \:: (?ctype:*, ?ptype:*) = 
	(?v1:expression, ?v2:expression)))
  } => {
    parameter-test(?"name", ?iface, "test-iparm-" ## ?name,
		   "test-oparm-" ## ?name, "test-ioparm-" ## ?name,
		   ?ctype, ?ptype, ?v1, ?v2);
  }

  { 
    interface-test-tester((?iface:expression), (test (?:name) (?:body))) 
  } => { 
    begin
      let ?name = ?iface;
      ?body;
    end;
  }

  { interface-test-tester((?iface:expression), (?other:*)) } => { }
end;
