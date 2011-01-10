Module:   tests
Author:   Jonathan Bachrach, Keith Playford, Paul Haahr
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define compiler-test reference
  i=(1, 1) & p=(#f, #f) & p=(#t, #t)
  /* & p=(\+, \+) & p=(#[1], #[1]) */ 
end;

define constant x = 1;

define compiler-test definition
  i=(x, 1);
end;

define variable y = 1;

define compiler-test global-assignment
  i=((y := 2), 2) &
  i=(y, 2)
end;

define compiler-test \begin
  p=(begin end, #f) &
  i=(begin 1 end, 1) &
  i=(begin 1; 2 end, 2) &
  i=(begin 1; 2; 3 end, 3) &
  i=(begin 1; begin 2 end end, 2)
end;

define constant z = i+(1, 3);

define compiler-test primitive
  i=(z, 4) &
  i=(i+(1, 2), 3) &
  i>(2, 1) &
  i<(1, 2)
  /* &
  primitive-true?(%raw-machine-word-equals?
         (%raw-machine-word-add(integer-as-raw(1), integer-as-raw(2)), 
      integer-as-raw(3))) &
  primitive-false?(%raw-machine-word-greater-than?(integer-as-raw(1), integer-as-raw(2))) &
  primitive-true?(%raw-machine-word-less-than?(integer-as-raw(1), integer-as-raw(2))) &
  i=(primitive-vector-element(#[1], integer-as-raw(0)), 1)
  */
end;

define compiler-test \if
  i=(if (#t) 1 else 2 end, 1) &
  i=(if (#f) 1 else 2 end, 2) &
  i=(if (#t) begin 1; 2 end else 3 end, 2) &
  i=(if (i=(1, 2)) 3 else 4 end, 4)
end;

define compiler-test scope
  i=(begin let a = 1; let b = i+(a, 1); b end, 2) &
  i=(begin let a = 1; let a = i-(a, 1); a end, 0)
end;

define compiler-test call
  p=((method () end)(), #f) &
  i=((method (x) 1 end)(2), 1) &
  i=((method (x) x end)(1), 1) &
  i=((method (x, y) x end)(1, 2), 1) &
  i=((method (x) (method (y) y end)(x) end)(1), 1) &
  i=((method (p?, x, y) if (p?) x else y end end)(#t, 1, 0), 1) &
  i=((method (p?, x, y) if (p?) x else y end end)(#f, 1, 0), 0)
end;

define compiler-test rest-call
  i=(primitive-vector-size((method (#rest x) x end)()), 0) &
  i=(primitive-vector-element((method (#rest x) x end)(1), 0), 1) &
  i=(primitive-vector-element((method (x, #rest y) y end)(1, 2), 0), 2) &
  i=((method (x, #rest y) x end)(1, 2), 1) &
  i=(primitive-vector-size((method (x, y, #rest z) z end)(1, 2, 3)), 1)
end;

define compiler-test key-call
  p=((method (#key x) x end)(), #f) &
  i=((method (#key x = 0) x end)(), 0) &
  i=((method (#key x) x end)(x: 1), 1) &
  i=((method (#key x, y) x end)(x: 1), 1) &
  i=((method (#key x, y) y end)(y: 1), 1) &
  i=((method (#key x = 0) x end)(x: 1), 1)  /* &
  i=((method (#key x = 0, y = x) y end)(x: 1), 1) */
end;

define compiler-test key-rest-call
  i=(primitive-vector-size((method (#rest r, #key x) r end)()), 0) &
  i=(primitive-vector-size((method (#rest r, #key x) r end)(x: 1)), 2) &
  p=(primitive-vector-element
       ((method (#rest r, #key x) r end)(x: 1), 0), 
       #"x") &
  i=(primitive-vector-element((method (#rest r, #key x) r end)(x: 1), 1), 1) &
  i=((method (#rest r, #key x) x end)(x: 1), 1)
end;

/*
define compiler-test apply
  i=(primitive-apply(method (x) x end, 1, #[1]), 1) &
  i=(primitive-apply(method (x, y) x end, 2, 1, #[2]), 1) &
  i=(primitive-apply(method (x, y) y end, 2, 1, #[2]), 2) &
  i=(primitive-apply(method (x, y) x end, 1, #[1, 2]), 1) &
  i=(primitive-apply(method (x, y) y end, 1, #[1, 2]), 2) &
  i=(primitive-vector-element
      (primitive-apply(method (#rest x) x end, 1, #[1]), 0), 1) &
  i=(primitive-vector-element
      (primitive-apply(method (#rest x) x end, 2, 1, #[2]), 0), 1) &
  i=(primitive-vector-element
      (primitive-apply(method (#rest x) x end, 2, 1, #[2]), 1), 2) &
  i=(primitive-vector-size
      (primitive-apply(method (#rest x) x end, 2, 1, #[2])), 2) &
  i=(primitive-apply(method (x, #rest y) x end, 1, #[1]), 1) &
  i=(primitive-vector-element
      (primitive-apply(method (x, #rest y) y end, 2, 1, #[2]), 0), 
      2) &
  i=(primitive-apply(method (x, #rest y) x end, 1, #[1, 2]), 1) &
  i=(primitive-vector-element
      (primitive-apply(method (x, #rest y) y end, 1, #[1, 2]), 0), 2)
end;
*/

define constant f
  = method (n) if (i<(n, 1)) 1 else i*(n, f(i-(n, 1))) end end;

define compiler-test recursion
  i=(f(1), 1) &
  i=(f(2), 2) &
  i=(f(3), 6) &
  i=(f(4), 24)
end;

define compiler-test closure
  i=((method (x) (method () x end)() end)(1), 1) & 
  i=(((method (x) (method () x end) end)(1))(), 1) &
  i=((method (x) (method () x := i+(x, 1) end)() end)(0), 1) & 
  i=(((method (x) (method () x := i+(x, 1) end) end)(0))(), 1) 
end;

define compiler-test labels
  i=(begin 
       local 
	 method f () g() end method, 
	 method g () 1 end method; 
       f() 
     end, 1) &
  i=(begin 
       local 
	 method f (x) g(x) end method, 
	 method g (y) y end method; 
       f(1)
     end, 1) &
  i=(begin
       local 
	 method f (n) if (i<(n, 1)) 1 else i*(n, f(i-(n, 1))) end end; 
       f(4) 
     end, 
     24) &
  i=(begin 
       local method f (n, r)
	 if (i<(n, 1)) r else f(i-(n, 1), i*(n, r)) end
       end method;
       f(4, 1)
     end, 
     24) &
  i=(begin 
       local 
	 method f (x) if (i<(x, 10)) g(i+(x, 2)) else x end end method,
	 method g (y) f(i-(y, 1)) end method;
       f(1)
     end,
     10) &
  begin 
    local method f (n, x, y)
      if (i>(n, 0)) f(i-(n, 1), y, x) else x end 
    end method;
    i=(f(0, 1, 2), 1) & i=(f(1, 1, 2), 2) & i=(f(2, 1, 2), 1)
  end &
  i=(begin
       let f = method (self, x, y)
		 if (i=(x, 0))
		   y
		 else
		   self(self, i-(x, 1), i+(y, 1))
		 end if
	       end method;
       f(f, 4, 0)
     end,
     4)
  end;

define compiler-test bind-exit
  i=(block (return) return(1) end, 1) &
  i=(block (return) return(1); 2 end, 1) &
  i=(block (return) i+(return(1), 2) end, 1) &
  i=(block (return) (method (r) r(1) end)(return) end, 1)
end;

define compiler-test unwind-protect
  i=(block () 1 cleanup 2 end, 1) &
  i=(block (return) return(1) cleanup 2 end, 1) /* & 
  i=(block (return) 
       return(1) 
     cleanup
       block ()
	 return(1)
       cleanup
	 return(3)
       end
     end,
     3) &
  i=(begin
       let a = 1;
       block (return)
	 return(1)
       cleanup
	 a := 2
       end;
       a
     end,
     2) &
  i=(block (a)
       block (b)
	 block (c)
	   c(0)
	 cleanup
	   a(13)
	 end
       cleanup
	 b(42)
       end
     end, 
     42) */
end;

define compiler-test multiple-values
  p=(primitive-values(0), #f) &
  i=(primitive-values(1, 1), 1) &
  begin let (x, y) = primitive-values(2, 3, 2); i=(x, 3) & i=(y, 2) end &
  begin
    let (x, y, z, none) = primitive-values(3, 1, 2, 3);
    i=(x, 1) & i=(y, 2) & i=(z, 3) & p=(none, #f)
  end &
  begin let (x, y, z, none)
	  = block () primitive-values(3, 1, 2, 3) 
            cleanup primitive-values(2, 4, 5)
            end;
	i=(x, 1) & i=(y, 2) & i=(z, 3) & p=(none, #f)
  end &
  begin let (x, y, z, none)
	  = block () primitive-values(3, 1, 2, 3) 
            afterwards primitive-values(2, 4, 5)
            end;
	i=(x, 1) & i=(y, 2) & i=(z, 3) & p=(none, #f)
  end
end;

