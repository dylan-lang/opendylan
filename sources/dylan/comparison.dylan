Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/*
define inline function pointer-id? (x, y)
  primitive-id?(x, y)
end function;

define inline function value-object? (x)
  indirect-object?(x) 
    & pointer-id?(indirect-object-class(indirect-object-class(x)), <value-class>)
end function;
*/

define inline-only function both-indirect-objects? (x, y) => (value :: <boolean>)
  primitive-machine-word-equal?
    (primitive-machine-word-logand
       (primitive-machine-word-logior(primitive-cast-pointer-as-raw(x), 
				      primitive-cast-pointer-as-raw(y)),
	integer-as-raw(3)),
     integer-as-raw(0))
end function;


// TODO: SYNC UP WITH REST

define constant $value-class-mask = 1;

define constant $raw-value-class-mask = ash($value-class-mask, $integer-tag-width);

/*
  // This version is correct and tagging-scheme independent
  // but it is poorly optimized by the current version of the compiler
define inline-only function value-wrapper? (w) => (value :: <boolean>)
  logand(mm-wrapper-subtype-mask(w), $value-class-mask) == 1
end function;
*/

// This version of value-wrapper? is more efficient
// but potentially more fragile too.
//
define inline-only function value-wrapper? (w) => (value :: <boolean>)
  primitive-machine-word-not-equal?
    (primitive-machine-word-logand
       (primitive-cast-pointer-as-raw(mm-wrapper-subtype-mask(w)),
	integer-as-raw($raw-value-class-mask)),
     integer-as-raw(0))
end function;


define function \== (x, y) => (id? :: <boolean>)
  pointer-id?(x, y) | (both-indirect-objects?(x, y)
			 & begin
			     let x-w = indirect-object-mm-wrapper(x);
			     let y-w = indirect-object-mm-wrapper(y);
			     pointer-id?(x-w, y-w)
			       & value-wrapper?(x-w)
			       & (begin
				    let c = iclass-class(mm-wrapper-implementation-class(x-w));
				    let e = value-class-comparitor(c) | init-value-class-comparitor(c);
				    (method (#rest v) %dynamic-extent(v); %invoke-engine-node(e, \=, v) end)(x, y)
				  end)
			   end)
end function;


define function init-value-class-comparitor (c :: <class>)
  let e :: <partial-dispatch-cache-header-engine-node>
    = system-allocate-repeated-instance(<partial-dispatch-cache-header-engine-node>, 
					<object>, #f, 2, <object>);
  properties(e) := logior(ash(engine-node$k-cache-header, properties$v-entry-type),
			  ash(3, pdisp$v-typemask));
  partial-dispatch-type(e, 0) := c;
  partial-dispatch-type(e, 1) := c;
  cache-header-engine-node-parent(e) := \=;
  cache-header-engine-node-next(e) := $absent-engine-node;
  primitive-initialize-engine-node(e);
  value-class-comparitor(c) | (value-class-comparitor(c) := e)
end function;


// define function \== (x, y) => (id? :: <boolean>)
//   pointer-id?(x, y) | (value-object?(x)
// 			 & indirect-object?(y)
// 			 & pointer-id?(indirect-object-class(x), indirect-object-class(y))
// 			 & x = y)
// end function;


define macro with-factored-equality 
  { with-factored-equality (?item:expression, ?testfn:expression, ?testname:name) ?body:body end }
    => { begin
	   let item = ?item;
	   let testfn = ?testfn;
	   if (testfn ~== \==)
	     local method ?testname (frob) testfn(frob, item) end method;
	     ?body
	   elseif (value-object?(item))
	     let ic :: <implementation-class> = indirect-object-implementation-class(item);
	     local method ?testname (frob)
		     pointer-id?(frob, item) | (indirect-object?(frob)
						  & indirect-object-implementation-class(frob) == ic
						  & frob = item)
		   end method;
	     ?body
	   else
	     local method ?testname (frob) pointer-id?(frob, item) end method;
	     ?body
	   end if
	 end }
end macro;


define inline function \~ (x) => (not :: <boolean>)
  pointer-id?(x, #f)
end function;

define open generic \= (x, y) => (well? :: <boolean>);

define open generic \< (x, y) => (well? :: <boolean>);

define inline function \~= (x, y) => (well? :: <boolean>)
  ~(x = y)
end function;

define inline function \~== (x, y) => (well? :: <boolean>)
  ~(x == y)
end function;

define inline function \> (x, y) => (well? :: <boolean>)
  y < x
end function;

define inline function \<= (x, y) => (well? :: <boolean>)
  ~(y < x)
end function;

define inline function \>= (x, y) => (well? :: <boolean>)
  ~(x < y)
end function;

define inline function binary-min (x, y)
  if (x < y)
    x
  else
    y
  end if
end function;

define function min (object, #rest objects) => (min)
  reduce(binary-min, object, objects)
end function min;

define inline function binary-max (x, y)
  if (x > y)
    x
  else
    y
  end if
end function;

define function max (object, #rest objects) => (max)
  reduce(binary-max, object, objects)
end function max;
