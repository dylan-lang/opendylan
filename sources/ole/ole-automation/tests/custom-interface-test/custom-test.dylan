Module:    custom-interface-test
Synopsis:  Test the "define custom-interface" macro.
Author:    David N. Gray
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// macro for a check-not-crash that traps errors when the occur

define macro check-not-crash
  { check-not-crash
     (?check-name:expression, ?check-expression:expression) }
    => { check-true (?check-name, begin
				    ?check-expression;
				    #t
				  end) }
end macro check-not-crash;



define constant $IID-IFoo :: <REFIID> =
  as(<REFIID>, "{08448351-265D-11D2-9A67-006097C90313}");

define custom-interface <IFoo> ( <IUnknown> )
  client-class <C-IFoo>;
  uuid $IID-IFoo;
  member-function IFoo/Bar ( n :: <C-void*> ) => ();
  member-function IFoo/Num ( n :: <integer> ) => ();
  vtable-member IFoo/Special ( )
	 => ( x :: <C-int>, y :: <integer>, z :: <C-short> );
  property size :: <integer>;
  constant property foo-const :: <single-float>;
  member-function IFoo/Square ( n :: <C-int> ) => (nn :: <C-int>);
end;  


define COM-interface <my-foo> ( <IFoo> )
  slot size :: <integer> = 0, init-keyword: size:;
end <my-foo>;

define method IFoo/Bar (This :: <my-foo>, n :: <C-void*>)
 => (status :: <HRESULT>);
  if ( null-pointer?(n) )
    $S-FALSE
  else
    $S-OK
  end if
end method;

define method IFoo/Num (This :: <my-foo>, n :: <integer>)
 => (status :: <HRESULT>);
  if ( zero?(n) )
    $S-FALSE
  else
    $S-OK
  end if
end method;

define method IFoo/Special (This :: <my-foo>)
 => ( x :: <integer>, y :: <integer>, z :: <integer> );
  values( -301, -302, -303)
end;

define method IFoo/Square (This :: <my-foo>, n :: <integer>)
 => (status :: <HRESULT>, square :: <integer>);
  values( $S-OK, n * n )
end method;

define constant $foo-const = 1.234;

define method foo-const (This :: <my-foo>) => (value :: <single-float>)
  $foo-const
end;

define constant $NULL-PTR = null-pointer(<C-void*>);
define constant $OK-PTR = make(<C-void*>, address: #xff00);

define constant $default-size = 70;

define variable *ci1* :: <interface> = $null-interface;
define variable *ci2* :: <interface> = $null-interface;
define variable *ci3* :: <interface> = $null-interface;

define test custom-interface-test 
		    (name: "custom-interface-test",
		     description: "test COM custom interfaces")

  check-true("make(<my-foo>)",
	     instance?(*ci1* := make(<my-foo>, size: $default-size),
		       <my-foo>));

  check-equal("QueryInterface(*ci1*, $IID-IFoo)",
	      begin
		let ( status, interface2 ) = QueryInterface(*ci1*, $IID-IFoo);
		*ci2* := interface2;
		status
	      end,
	      $S-OK);

  check-equal("*ci1* = *ci2*", *ci1*, *ci2*);

  check-equal("IFoo/Bar({<my-foo>},$NULL-PTR)",
	      IFoo/Bar(*ci2*, $NULL-PTR),
	      $S-FALSE);

  check-equal("IFoo/Bar({<my-foo>},$OK-PTR)",
	      IFoo/Bar(*ci2*, $OK-PTR),
	      $S-OK);

  check-equal("IFoo/Special({<my-foo>}),",
	      begin
		let ( #rest values ) = IFoo/Special(*ci2*);
		values
	      end,
	      #[-301, -302, -303]);

  check-true("<C-IFoo>", subtype?(<C-IFoo>, <interface>));

  check-equal("set *ci3*",
	      begin
		*ci3* := make(<C-IFoo>, address: pointer-address(*ci2*));
		pointer-address(*ci3*)
	      end,
	      pointer-address(*ci2*));

  check-equal("IFoo/Bar({<C-IFoo>},$NULL-PTR)",
	      IFoo/Bar(*ci3*, $NULL-PTR),
	      $S-FALSE);

  check-equal("IFoo/Bar({<C-IFoo>},$OK-PTR)",
	      IFoo/Bar(*ci3*, $OK-PTR),
	      $S-OK);

  check-equal("IFoo/Num({<C-IFoo>}, 0)",
	      IFoo/Num(*ci3*, 0),
	      $S-FALSE);

  check-equal("IFoo/Num({<C-IFoo>}, 7)",
	      IFoo/Num(*ci3*, 7),
	      $S-OK);

  check-equal("IFoo/Special({<C-IFoo>})",
	      begin
		let ( #rest values ) = IFoo/Special(*ci3*);
		values
	      end,
	      #[-301, -302, -303]);

  check-equal("dylan-interface(*ci3*)",
	      dylan-interface(*ci3*),
	      *ci1*);

  check-equal("size(*ci3*) default",
	      size(*ci3*),
	      $default-size);

  check-equal("set size(*ci3*)",
	      begin
		size(*ci3*) := 987;
		size(*ci3*)
	      end,
	      987);

  check-equal("IFoo/Square",
	      begin
		let ( status, value ) = IFoo/Square(*ci3*, 13);
		value
	      end,
	      13 * 13);

  check-equal("foo-const", foo-const(*ci3*), $foo-const);

  check-type(*ci1*, <my-foo>);
  check-not-crash("Release(*ci2*)",
		  Release(*ci2*));

end test;

