Module:    parameter-type-tests
Synopsis:  Tests of parameter types for COM interfaces.
Author:    Seth LaForge
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define c-pointer-type <C-boolean*> => <C-boolean>;
define c-pointer-type <lplpstr> => <lpstr>;
define c-pointer-type <lplpwstr> => <lpwstr>;

define interface-test vtable-interface IVtableParmTest (<IUnknown>)
  uuid "{6c83043e-6558-11d2-a3cc-0060b0572a7f}";

  parameter char :: (<C-character>) = ('a', 'Z');
  parameter short :: (<C-signed-short>) = (666, -999);
  //parameter long :: (<C-signed-long>) = (666666, -999999);

  // Bug 4225:
  // parameter float :: (<C-float>) = (3.141592, 2.7182818);
  // parameter double :: (<C-double>) = (3.141592d0, 2.7182818d0);

  parameter bstr :: (<BSTR>, <LPBSTR>) = 
  	  (as(<BSTR>, "paradise"), as(<BSTR>, "where you are right now"));
  //BUG 4237: parameter hresult :: (<C-HRESULT>) = ($S-FALSE, $CLASS-E-NOAGGREGATION);
  // parameter variant-bool :: (<variant-bool>) = (#f, #t);
  parameter bool :: (<C-boolean>) = (#f, #t);
  parameter signed-char :: (<C-signed-char>) = (69, -96);
  //BUG 3915: parameter unsigned-char :: (<C-unsigned-char>) = (12, 123);
  parameter unsigned-short :: (<C-unsigned-short>) = (12345, 45678);
  //parameter unsigned-long :: (<C-unsigned-long>) = (1234567890, 3456789012);
  // This doesn't work, and should be bug-reported:
  //parameter unsigned-long :: (<C-unsigned-long>) = (123456789, 345678901);
  //parameter int :: (<C-int>) = (123456789, -123456789);
  
  // These crashes big-time, and should be bug-reported:
  // parameter lpstr :: (<lpstr>, <lplpstr>) = 
  //	(as(<lpstr>, "bright"), as(<lpstr>, "red"));
  // parameter lpwstr :: (<lpwstr>, <lplpwstr>) = 
  // 	(as(<lpwstr>, "phone"), as(<lpwstr>, "booth"));
  

  member-function itest-interface (i :: <C-IVtableParmTestParm>) => ();
  test (this) (
    format-out("iface test begins\n");
    let iface = #f;
    check-no-errors("Input parameter interface: creating interface",
		    iface := make(<S-IVtableParmTestParm>));
    check-no-errors("Input parameter interface: clientizing",
		    iface := make(<C-IVtableParmTestParm>,
				  address: iface.pointer-address));
    AddRef(iface);
    check-OK("Input parameter interface: intest",
	     itest-interface(this, iface));
    check-equal("Input parameter interface: release",
		Release(iface), 0);
    format-out("iface test ends\n");
  );

  /* This crashes big-time, and should be bug-reported:

  member-function otest-interface (i :: out-ref(<C-IVtableParmTestParm>)) => ();
  test (this) (
    format-out("oface test begins\n");
    let iface = make(<C-IVtableParmTestParm*>);
    format-out("2\n");
    check-OK("Output parameter interface: otest",
	     otest-interface(this, iface));
    format-out("3\n");
    with-stack-structure (out-ptr :: <C-int*>)
      check-OK("Output parameter interface: call",
	       parm-val(iface.pointer-value, out-ptr));
      check-equal("Output parameter interface: result",
		  out-ptr.pointer-value, 666);
    end;
    check-equal("Output parameter interface: release",
		Release(iface), 0);
    format-out("oface test ends\n");
  );
  */
end;

define method itest-interface (this :: <S-IVtableParmTest>, 
				i :: <C-IVtableParmTestParm>) 
			    => (r :: <HRESULT>)
  coe(
    with-stack-structure (out-ptr :: <C-int*>)
      let r = parm-val(i, out-ptr);
      if (r = $S-OK) oeq(out-ptr.pointer-value, 666) else r end
    end with-stack-structure
  )
end;

define method otest-interface (this :: <S-IVtableParmTest>, 
				i :: <C-IVtableParmTestParm*>) 
			    => (r :: <HRESULT>)
  coe(
    let iface = make(<S-IVtableParmTestParm>);
    i.pointer-value := make(<C-IVtableParmTestParm>, 
			    address: iface.pointer-address);
  )
end;

define constant $VtableParmTest-uuid = "{9179f78a-6863-11d2-a3cc-0060b0572a7f}";

define vtable-interface <S-IVtableParmTestParm> (<IUnknown>)
  client-class <C-IVtableParmTestParm>;
  uuid "{595ac166-69e1-11d2-a3cc-0060b0572a7f}";
  member-function parm-val (val :: out-ref(<C-int>)) => ();
end;

define C-pointer-type <C-IVtableParmTestParm*> => <C-IVtableParmTestParm>;

define method parm-val (this :: <S-IVtableParmTestParm>, val :: <C-int*>)
		    => (r :: <HRESULT>)
  val.pointer-value := 666; $S-OK
end;
