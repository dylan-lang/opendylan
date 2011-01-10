Module:    win32-util-test
Synopsis:  Tests utility functions in the Win32-common library.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define test win32-util-test 
  (name: "win32-util-test",
   description: 
     "tests utility functions in the Win32-common library")

  check-equal("LOWORD(#x60003)", LOWORD(#x60003), 3);
  check-equal("HIWORD(#x60003)", HIWORD(#x60003), 6);
  check-equal("LOWORD($FFFFFFFF)", LOWORD($FFFFFFFF), #xFFFF);
  check-equal("HIWORD($FFFFFFFF)", HIWORD($FFFFFFFF), #xFFFF);
  check-equal("LOWORD(-1)", LOWORD(-1), #xFFFF);
  check-equal("HIWORD(-1)", HIWORD(-1), #xFFFF);
  let w :: <machine-word> = as(<machine-word>, #x8765F321);
  check-equal("HIWORD(w)", HIWORD(w), #x8765);
  check-equal("LOWORD(w)", LOWORD(w), #xF321);
  let di :: <double-integer> = as(<double-integer>, #x8765F321);
  check-equal("HIWORD(di)", HIWORD(di), #x8765);
  check-equal("LOWORD(di)", LOWORD(di), #xF321);

  check-equal("MAKELONG", MAKELONG(7,8), as(<machine-word>, #x80007));
  check-makelong(#xabcd,45);
  check-makelong(78,#xf00d);
  check-makelong(#x8888,#x9999);
  check-equal("MAKELONG(#t", MAKELONG(#t, 7), #x70001);
  check-equal("MAKELONG(#f", MAKELONG(#f, 8), #x80000);

  check-LPARAM-TO-XY(45, -56);
  check-LPARAM-TO-XY(-89, 1234);

  check-lang-id(12,34);

  check-lcid($LANG-CHINESE, $SORT-CHINESE-UNICODE);


  check-equal("LOBYTE(#x123456)", LOBYTE(#x123456), #x56);
  check-equal("HIBYTE(#x123456)", HIBYTE(#x123456), #x34);
  check-equal("LOBYTE(w)", LOBYTE(w), #x21);
  check-equal("HIBYTE(w)", HIBYTE(w), #xF3);
  check-equal("MAKEWORD", MAKEWORD(#x123,#x789), #x8923);

end test;

define method check-makelong ( low, high )
  let w = MAKELONG(low,high);
  check-equal("HIWORD(MAKELONG(...))", HIWORD(w), high);
  check-equal("LOWORD(MAKELONG(...))", LOWORD(w), low);
end check-makelong;

define method check-LPARAM-TO-XY ( low, high )
  let w = MAKELONG(low,high);
  let ( x, y ) = LPARAM-TO-XY(w);
  check-equal("x in LPARAM-TO-XY", x, low);
  check-equal("y in LPARAM-TO-XY", y, high);
end check-LPARAM-TO-XY;

define method check-lang-id ( primary, sub )
  let lid = MAKELANGID(primary, sub);
  check-equal("PRIMARYLANGID", PRIMARYLANGID(lid), primary);
  check-equal("SUBLANGID", SUBLANGID(lid), sub);
end check-lang-id;

define method check-lcid ( language, sort )
  let lcid = MAKELCID(language, sort);
  check-equal("LANGIDFROMLCID", LANGIDFROMLCID(lcid), language);
  check-equal("SORTIDFROMLCID", SORTIDFROMLCID(lcid), sort);
end check-lcid;

