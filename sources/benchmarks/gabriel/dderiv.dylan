Module: gabriel-benchmarks
Author: Carl Gay
Synopsis: DDERIV -- Symbolic derivative benchmark written by Vaughn Pratt.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// This benchmark is a variant of the simple symbolic derivative program 
/// (DERIV). The main change is that it is `table-driven.'  Instead of using a
/// large COND that branches on the CAR of the expression, this program finds
/// the code that will take the derivative on the property list of the atom in
/// the CAR position. So, when the expression is (+ . <rest>), the code
/// stored under the atom '+ with indicator DERIV will take <rest> and
/// return the derivative for '+. The way that MacLisp does this is with the
/// special form: (DEFUN (FOO BAR) ...). This is exactly like DEFUN with an
/// atomic name in that it expects an argument list and the compiler compiles
/// code, but the name of the function with that code is stored on the
/// property list of FOO under the indicator BAR, in this case. You may have
/// to do something like:

/// A note on the Dylan translation:
///   car => head
///   cdr => tail
///   cadr => second
///   mapcar => map
///   cons => pair
///   Use a hash table ($table) rather than symbol plists.
///   Add two type specifiers, one in dderiv/ and one in dderiv.

define function dderiv-aux (a)
  list(#"/", dderiv(a), a)
end;

define function dderiv+ (a)
  pair(#"+", map(dderiv, a))
end;

define function dderiv- (a)
  pair(#"-", map(dderiv, a))
end;

define function dderiv* (a)
  list(#"*", pair(#"*", a), pair(#"+", map(dderiv-aux, a)))
end;

define function dderiv/ (a :: <list> /* Added for Dylan version */)
  list(#"-",
       list(#"/",
	    dderiv(head(a)),
	    second(a)),
       list(#"/",
	    head(a),
	    list(#"*",
                 second(a),
                 second(a),
                 dderiv(second(a)))))
end function dderiv/;

// This table added for the Dylan translation.
define table $table = { #"+" => dderiv+,
			#"-" => dderiv-,
			#"*" => dderiv*,
			#"/" => dderiv/ };
			 
// This function added for the Dylan translation.
/*  Also defined in boyer.dylan
define function atom? (x)
  ~instance?(x, <pair>)
end;
*/

define function dderiv (a)
  if (atom?(a))
    if (a == #"x")
      1
    else
      0
    end
  else
    let b :: <list> = a;  // Added for Dylan version
    let dderiv = $table[head(b)];
    if (dderiv)
      dderiv(tail(b));
    else
      error("you lose");
    end if
  end if
end function dderiv;

define function dderiv-run ()
  for (i from 0 below 1000)
    //(declare (type fixnum i))
    dderiv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"), #(#"*", #"b", #"x"), 5));
    dderiv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"), #(#"*", #"b", #"x"), 5));
    dderiv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"), #(#"*", #"b", #"x"), 5));
    dderiv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"), #(#"*", #"b", #"x"), 5));
    dderiv(#(#"+", #(#"*", 3, #"x", #"x"), #(#"*", #"a", #"x", #"x"), #(#"*", #"b", #"x"), 5));
  end for;
end function dderiv-run;

define benchmark dderiv = dderiv-run;
