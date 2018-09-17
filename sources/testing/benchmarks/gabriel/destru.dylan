Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: DESTRU - Converted from Common Lisp
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// DESTRU -- Destructive operation benchmark

define function destructive (n :: <integer>, m :: <integer>)
  let l = for (i from 10 to 1 by -1,  // "vonce rrrockets are up..."
	       it = #() then pair(#(), it))  // pair instead of push
	  finally it
	  end for;
  for (i from n to 1 by -1)
    if (null?(first(l)))
      for (l = l then tail(l), until: null?(l))
	if (null?(first(l)))
	  rplaca(l, pair(#(), #()));
	end if;
	nconc(first(l), for (j from m to 1 by -1,
			     a = #() then pair(#(), a))  // pair instead of push
			finally a
			end for)
      end for;
    else
      for (l1 = l then tail(l1),
	   l2 = tail(l) then tail(l2), until: null?(l2))
	rplacd(for (j from floor/(size(first(l2)), 2) to 1 by -1,
		    a = first(l2) then tail(a))
		 rplaca(a, i);
	       finally a
	       end for,
	       begin
		 let n = floor/(size(first(l1)), 2);
		 if (n == 0)
		   rplaca(l1, #());
		   first(l1)
		 else
		   for(j from n above 1 by -1,
		       a = first(l1) then tail(a))
		     rplaca(a, i);
		   finally
		     let result = tail(a);
		     rplacd(a, #());
		     result
		   end for
		 end if
	       end); // begin
      end for;
    end if;
  end for;
  l
end function destructive;

define function testdestru ()
  destructive(600, 50);
end function testdestru;

define benchmark destru = testdestru;

