Module:    ole-test
Synopsis:  Test <ole-array>.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *array* = #f;

define test ole-array-test 
  (name: "ole-array-test",
   description: "tests the <ole-vector> and <ole-array> data types")

  /* // SafeArrayCreate doesn't seem to allow 0 dimensions
  check-equal("empty array",
	      begin
		let ea = #f;
		block()
		  ea := make(<ole-array>, dimensions: #[]);
		  dimensions(ea)
		cleanup
		  if ( ea ) destroy(ea); end if;
		  ea := #f;
		end
	      end,
	      #[]);
  */

  check-not-crash("create 2 x 3 int array",
		  *array* := make(<ole-array>, dimensions: #[2, 3],
				  fill: 0, vartype: $VT-I4));
  check-equal("rank of 2x3", rank(*array*), 2);
  check-equal("dimensions of 2x3", dimensions(*array*), #[2, 3]);
  check-equal("size of 2x3", size(*array*), 2 * 3);

  check-equal("row-major-index(a,0,0)", row-major-index(*array*,0,0), 0);
  check-equal("row-major-index(a,1,0)", row-major-index(*array*,1,0), 1);
  check-equal("row-major-index(a,0,1)", row-major-index(*array*,0,1), 2);
  check-equal("row-major-index(a,1,1)", row-major-index(*array*,1,1), 3);

  check-not-crash("set elements of 2x3 array",
		  begin
		    *array*[0,0] := 1000;
		    *array*[0,1] := 1001;
		    *array*[0,2] := 1002;
		    *array*[1,0] := 1100;
		    *array*[1,1] := 1101;
		    *array*[1,2] := 1102;
		  end);
  check-equal("a[0,0]", *array*[0,0], 1000);
  check-equal("a[0,2]", *array*[0,2], 1002);
  check-equal("a[1,0]", *array*[1,0], 1100);
  check-equal("a[1,2]", *array*[1,2], 1102);

  check-not-crash("destroy int array", 
		  begin
		    let a = *array*;
		    *array* := #f;
		    destroy(a);
		  end);

end test ole-array-test;
