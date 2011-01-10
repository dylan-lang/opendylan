Module:   dfmc-common
Synopsis: topological sort, from _The Art of the Metaobject Protocol_
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Elements is a list of the things to sort, constraints is a list
// of two element sequences where the first element must precede the
// second, and tie-breaker is a function which takes a list of
// elements and the current ordering.

define method topological-sort
    (elements :: <list>, constraints :: <list>, tie-breaker :: <function>)
  local method sort (remaining-constraints, remaining-elements, result)
	  local method next-minimal-elements (remaining-elements :: <list>)
		  choose(method (class)
			   ~member?(class,
				    remaining-constraints,
				    test: method (a, b)
					    a == b.second
					  end method)
			 end method,
			 remaining-elements)
		end method next-minimal-elements;
	  let minimal-elements = remaining-elements.next-minimal-elements;
	  if (minimal-elements.empty?)
	    if (remaining-elements.empty?)
	      result
	    else
	      error("Inconsistent precedence graph ~S.", remaining-elements)
	    end if
	  else
	    let choice
	      = if (empty?(minimal-elements.tail))
		  minimal-elements.head
		else
		  tie-breaker(minimal-elements, result)
		end if;
	    sort(remove(remaining-constraints, choice,
			test: method (a, b) member?(b, a) end),
		 remove(remaining-elements, choice),
		 concatenate(result, list(choice)))
	  end if
	end method sort;
  sort(constraints, elements, #())
end method topological-sort;
