module: SDE-vector
author: Nick Kramer (nkramer@cs.cmu.edu)

//======================================================================
//
// Copyright (c) 1995, 1996, 1997  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
// Bug reports should be sent to <gd-bugs@gwydiondylan.org>; questions,
// comments and suggestions are welcome at <gd-hackers@gwydiondylan.org>.
// Also, see http://www.gwydiondylan.org/ for updates and documentation. 
//
//======================================================================

// Stretchy double ended vector

// (A collection that has keys from -n to n, where n is a non-negative
// integer) It's not technically a vector because the keys don't start
// at 0, and so doesn't inherit a lot of sequence methods, but
// otherwise it behaves like a vector.

define class <SDE-vector> (<mutable-collection>, <stretchy-collection>)
  slot contents :: <stretchy-vector>;
end class <SDE-vector>;

define method initialize (vec :: <sde-vector>, #next next-method,
			  #key size: sz = 0, fill: fill = #f)
  next-method();
  vec.contents := make(<stretchy-vector>, size: sz, fill: fill);
end method initialize;

define method size (vec :: <sde-vector>) => sz :: <integer>;
  vec.contents.size;
end method size;

define method empty? (vec :: <sde-vector>) => answer :: <boolean>;
  vec.contents.empty?;
end method empty?;

define method get-index (key :: <integer>) => non-negative-index :: <integer>;
  2 * abs(key) - (if (key < 0) 1 else 0 end);
end method get-index;

define constant no-default = pair(#f, #f);

define method element (vec :: <sde-vector>, key :: <integer>, 
		       #key default = no-default)
 => elt :: <object>;
  if (default == no-default)
    vec.contents[get-index(key)];
  else
    element(vec.contents, get-index(key), default: default);
  end if;
end method element;

define method element-setter (val :: <object>, vec :: <sde-vector>, 
			      key :: <integer>) => val :: <object>;
  vec.contents[get-index(key)] := val;
end method element-setter;

define method forward-iteration-protocol (vec :: <sde-vector>)
 => (initial-state :: <object>, limit :: <object>, next-state :: <function>,
     finished-state? :: <function>, current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  let init = - truncate/(vec.contents.size, 2);
  values(init,
	 init + vec.contents.size,  // limit

	 method (v :: <sde-vector>, state :: <integer>)
	  => next-state :: <integer>;
	   state + 1;
	 end method,

	 method (v :: <sde-vector>, state :: <integer>, limit :: <integer>)
	  => finished? :: <boolean>;
	   state = limit;
	 end method,

	 method (v :: <sde-vector>, state :: <integer>)
	  => current-key :: <integer>;
	   state;
	 end method,

	 method (v :: <sde-vector>, state :: <integer>)
	  => current-elt :: <object>;
	   v.contents[get-index(state)];
	 end method,

	 method (val :: <object>, v :: <sde-vector>, state :: <integer>)
	  => val :: <object>;
	   v.contents[get-index(state)] := val;
	 end method,

	 method (v :: <sde-vector>, state :: <integer>)
	  => copied-state :: <integer>;
	   state;
	 end method);
end method forward-iteration-protocol;

