Module:    win32-util-test
Synopsis:  Tests data types defined manually in the Win32-common library.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define test win32-types-test 
  (name: "win32-types-test",
   description: 
     "tests data types in the Win32-common library")

  // <LARGE-INTEGER>
  check-large-integer(456);
  check-large-integer(-789);
  check-large-integer(#x87654321);

  // <ULARGE-INTEGER>
  check-unsigned-large-integer(0);
  check-unsigned-large-integer(#x12345);
  check-unsigned-large-integer(#x87654321);

  check-equal("init large integer",
	      pointer-value(make(<PULARGE-INTEGER>, value: 384710)),
	      384710);

  // <WCHAR>
  test-wchar('w');
  test-wchar(as(<character>, #xFEFF)); // unicode byte order mark

  // <BOOLEAN-test> 
  test-bool-byte(#f);
  test-bool-byte(#t);

end test;

define C-union <test-wchar>
  slot get-short :: <C-unsigned-short>;
  slot get-wchar :: <WCHAR>;
  pointer-type-name: <test-wchar*>;
end;

define function test-wchar( char :: <character> ) => ();
  let u = make(<test-wchar*>);
  u.get-short := #xEEEE;
  check-equal("set <WCHAR> value", u.get-wchar := char, char);
  check-equal("<WCHAR> value", u.get-wchar, char);
  check-equal("raw <WCHAR>", u.get-short, as(<integer>, char));
  destroy(u);
end;

define C-union <test-bool-byte>
  slot get-byte :: <C-unsigned-char>;
  slot get-bool-byte :: <BOOLEAN-BYTE>;
  pointer-type-name: <test-bool-byte*>;
end;

define function test-bool-byte( value :: <boolean> ) => ();
  let u = make(<test-bool-byte*>);
  check-equal("set <BOOLEAN-BYTE> value", u.get-bool-byte := value, value);
  check-equal("<BOOLEAN-BYTE> value", u.get-bool-byte, value);
  check-equal("raw <BOOLEAN-BYTE>", u.get-byte,
	      if (value) 1 else 0 end if);
  destroy(u);
end;

define method check-large-integer ( value )
  with-stack-structure( pli :: <PLARGE-INTEGER> )
    pointer-value(pli) := value;
    check-equal("<PLARGE-INTEGER>", pointer-value(pli), value);
  end;
end check-large-integer;

define method check-unsigned-large-integer ( value )
  with-stack-structure( pli :: <PULARGE-INTEGER> )
    pointer-value(pli) := value;
    check-equal("<PULARGE-INTEGER>", pointer-value(pli), value);
  end;
end check-unsigned-large-integer;
