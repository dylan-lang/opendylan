language: infix-dylan
module: dispatch-engine-internal
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define inline constant slot-method-requiring-class-discrimination?
  = method (m :: <method>, argnum :: <integer>)
      if (case
	    argnum == 0 => instance?(m, <getter-accessor-method>);
	    argnum == 1 => instance?(m, <setter-accessor-method>);
	    otherwise => #f;
	  end case)
	let m :: <accessor-method> = m;
	let sd :: <slot-descriptor> = method-slot-descriptor(m);  // who is missing type info?
	let c :: <class> = slot-owner(sd);
	~class-primary?(c)
      else
	#f
      end if
    end method;


define constant slotdiscrim$v-offset 
  = engine-node$v-data-start;

// define inline constant builtin-slot-engine-node-offset = method
//     (e :: <slot-access-engine-node>) => (offset :: <integer>);
//   engine-node-raw-integer(e)
// end method;


define inline constant set-slot-engine-node-offset = method
    (e :: <slot-access-engine-node>, offset :: <integer>) => (offset :: <integer>);
  let mask :: <integer> = ash(1, slotdiscrim$v-offset) - 1;
  let props :: <integer> = properties(e);
  properties(e) := logior(ash(offset, slotdiscrim$v-offset), logand(props, mask));
  let callbacks :: <simple-object-vector> = *engine-node-callbacks*;
  if (~(vector-element(callbacks, logand(ash(props, - properties$v-entry-type),
					 ash(1, properties$s-entry-type) - 1))))
    engine-node-raw-integer(e) := offset
  end if;
  offset
end method;

define inline constant callback-slot-engine-node-offset = method
    (e :: <slot-access-engine-node>) => (offset :: <integer>);
  ash(properties(e), - slotdiscrim$v-offset)
end method;


//define inline constant slot-engine-node-size-offset = method
//    (e :: <repeated-slot-access-engine-node>) => (offset :: <integer>);
//  engine-node-data-1(e)
//end method;

//define inline constant slot-engine-node-size-offset-setter = method
//    (offset :: <integer>, e :: <repeated-slot-access-engine-node>) => (offset :: <integer>);
//  engine-node-data-1(e) := offset
//end method;

// define engine-node-slot slot-engine-node-size-offset 
//    <repeated-slot-access-engine-node> <integer> engine-node-data-1;



// Basic instance slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant instance-slot-getter-engine-node = method
//    (e :: <boxed-instance-slot-getter-engine-node>,
//     gf :: <generic-function>, 
//     args :: <simple-object-vector>)
//  gf;
//  let inst = vector-element(args, 0);
//  let value = slot-element(inst, builtin-slot-engine-node-offset(e));
//  if (unbound?(value))
//    unbound-instance-slot(inst, builtin-slot-engine-node-offset(e))
//  else 
//    value
//  end if
//end method;


// Basic instance slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant instance-slot-setter-engine-node = method
//    (e :: <boxed-instance-slot-setter-engine-node>,
//     gf :: <generic-function>, 
//     args :: <simple-object-vector>)
//  gf;
//  let inst = vector-element(args, 1);
//  let value = vector-element(args, 0);
//  slot-element(inst, builtin-slot-engine-node-offset(e)) := value
//end method;


// Basic instance slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant repeated-instance-slot-getter-engine-node = method
//    (e :: <boxed-repeated-instance-slot-getter-engine-node>,
//     gf :: <generic-function>,
//     args :: <simple-object-vector>)
//  let inst = vector-element(args, 0);
//  let idx :: <integer> = vector-element(args, 1);
//  let size-offset :: <integer> = slot-engine-node-size-offset(e);
//  let siz :: <integer> = slot-element(inst, size-offset);
//  if (idx < 0 | idx >= siz)
//    repeated-slot-getter-index-out-of-range-trap(inst, idx);
//  else
//    let offset :: <integer> = builtin-slot-engine-node-offset(e);
//    let value = /* repeated-slot-element(inst, offset, idx) */
//      slot-element(inst, idx + offset);
//    if (unbound?(value))
//      unbound-repeated-slot(inst, idx)
//    else
//      value
//    end if
//  end if
//end method;


// Basic instance slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant repeated-instance-slot-setter-engine-node = method
//    (e :: <boxed-repeated-instance-slot-setter-engine-node>,
//     gf :: <generic-function>,
//     args :: <simple-object-vector>)
//  let value = vector-element(args, 0);
//  let inst = vector-element(args, 1);
//  let idx :: <integer> = vector-element(args, 2);
//  let size-offset :: <integer> = slot-engine-node-size-offset(e);
//  let siz :: <integer> = slot-element(inst, size-offset);
//  if (idx < 0 | idx >= siz)
//    repeated-slot-setter-index-out-of-range-trap(value, inst, idx)
//  else
//    let offset :: <integer> = builtin-slot-engine-node-offset(e);
//    // repeated-slot-element(inst, offset, idx) := value
//    slot-element(inst, idx + offset) := value
//  end if
//end method;

// Basic byte slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant byte-slot-getter-engine-node = method
//    (e :: <byte-slot-getter-engine-node>,
//     gf :: <generic-function>,
//     args :: <simple-object-vector>)
//  let inst = vector-element(args, 0);
//  let offset :: <integer> = builtin-slot-engine-node-offset(e);
//  byte-slot-element(inst, offset, 0)
//end method;


// Basic byte slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant byte-slot-setter-engine-node = method
//    (e :: <byte-slot-setter-engine-node>,
//     gf :: <generic-function>,
//     args :: <simple-object-vector>)
//  let value = vector-element(args, 0);
//  let inst = vector-element(args, 1);
//  let offset :: <integer> = builtin-slot-engine-node-offset(e);
//  byte-slot-element(inst, offset, 0) := value
//end method;


// Basic byte slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant repeated-byte-slot-getter-engine-node = method
//    (e :: <repeated-byte-slot-getter-engine-node>,
//     gf :: <generic-function>,
//     args :: <simple-object-vector>)
//  let inst = vector-element(args, 0);
//  let idx :: <integer> = vector-element(args, 1);
//  let size-offset :: <integer> = slot-engine-node-size-offset(e);
//  let siz :: <integer> = slot-element(inst, size-offset);
//  if (idx < 0 | idx >= siz)
//    repeated-slot-getter-index-out-of-range-trap(inst, idx)
//  else
//    let offset :: <integer> = builtin-slot-engine-node-offset(e);
//    byte-slot-element(inst, offset, idx)
//  end if
//end method;


// Basic byte slots have built-in engine nodes, and don't need a callback.
// Here's the emulated engine node with obsolete argument order.
//define constant repeated-byte-slot-setter-engine-node = method
//    (e :: <repeated-byte-slot-setter-engine-node>,
//     gf :: <generic-function>,
//     args :: <simple-object-vector>)
//  let value = vector-element(args, 0);
//  let inst = vector-element(args, 1);
//  let idx :: <integer> = vector-element(args, 2);
//  let size-offset :: <integer> = slot-engine-node-size-offset(e);
//  let siz :: <integer> = slot-element(inst, size-offset);
//  if (idx < 0 | idx >= siz)
//    repeated-slot-setter-index-out-of-range-trap(value, inst, idx)
//  else
//    let offset :: <integer> = builtin-slot-engine-node-offset(e);
//    byte-slot-element(inst, offset, idx) := value
//  end if
//end method;


// Class slots are currently implemented by callback.
define constant %gf-dispatch-boxed-class-slot-getter
  = method (inst, e :: <boxed-class-slot-getter-engine-node>, parent :: <dispatch-starter>)
      let offset :: <integer> = callback-slot-engine-node-offset(e);
      let cls :: <class> = object-class(inst);
      let storage :: <simple-object-vector> = class-slot-storage(cls);
      let e :: <pair> = vector-element(storage, offset);
      let val = head(e);
      if (unbound?(val))
	unbound-class-slot(inst, offset)
      else
	val
      end if
    end method;


// Class slots are currently implemented by callback.
define constant %gf-dispatch-boxed-class-slot-setter
  = method (val, inst, e :: <boxed-class-slot-setter-engine-node>, parent :: <dispatch-starter>)
      let offset :: <integer> = callback-slot-engine-node-offset(e);
      let cls :: <class> = object-class(inst);
      let storage :: <simple-object-vector> = class-slot-storage(cls);
      let e :: <pair> = vector-element(storage, offset);
      head(e) := val
    end method;
      



define function slot-location (sd :: <slot-descriptor>, 
			       icls :: <implementation-class>,
			       ds :: <dispatch-state>)
 => (index :: <integer>, location :: <integer>, success? :: <boolean>)
  let off :: <integer> = slot-offset-i(sd, icls);
  if (off)
    values(off, off, #t)
  else
    // A little more sophistication might find this as being an obsolete instance
    // error, or somesuch.
    dispresult(make(<simple-error>,
		    format-string: "Can't find slot location for %= in %= - perhaps an "
		      "instance of a redefined class has been encountered.",
		    format-arguments: vector(sd, iclass-class(icls))),
	       ds);
    values(0, 0, #f)
  end if
end function;


define function make-slot-access-engine-node (meth :: <accessor-method>, ds :: <dispatch-state>)
 => (e :: <engine-node>)
  let setter? = instance?(meth, <setter-accessor-method>);
  let sd :: <slot-descriptor> = method-slot-descriptor(meth);
  let thisargiclass :: <implementation-class> 
    = object-implementation-class(vector-element(%ds-args(ds), if (setter?) 1 else 0 end if));
  let (index :: <integer>, location :: <integer>, success? :: <boolean>)
    = slot-location(sd, thisargiclass, ds);
  if (~success?)
    $absent-engine-node
  elseif (instance?(sd, <repeated-slot-descriptor>))
    let sd :: <repeated-slot-descriptor> = sd;
    let sizesd :: <slot-descriptor> = size-slot-descriptor(sd);
    let (size-index :: <integer>, size-location :: <integer>) 
      = slot-location(sizesd, thisargiclass, ds);
    get-repeated-slot-access-engine-node(select (sd by instance?)
					   <any-instance-slot-descriptor> =>
					     // @@@@ This is sick.
					     if (sd.slot-type == <byte-character>)
					       engine-node$k-raw-byte-repeated-instance-slot-getter
					     else
					       engine-node$k-boxed-repeated-instance-slot-getter
					     end if;
					   <any-class-slot-descriptor> =>
					     error("You must be joking");
					 end select,
					 setter?, index, location, size-index, size-location)
  else
    get-slot-access-engine-node(select (sd by instance?)
				  <any-instance-slot-descriptor> => 
				    engine-node$k-boxed-instance-slot-getter;
				  <any-class-slot-descriptor> =>
				    engine-node$k-boxed-class-slot-getter;
				end select,
				setter?, index, location)
  end if
end function;


 
define primary class <slot-access-engine-repository> (<object>)
  // slot engine-node-code :: false-or(<integer>), required-init-keyword: code:;
  slot engine-node-table :: <simple-object-vector>, init-value: #[];
end class;


define inline sealed method make (c == <slot-access-engine-repository>, #key code) 
 => (t :: <slot-access-engine-repository>);
  c; make-slot-access-engine-repository(code)
end method;

define constant make-slot-access-engine-repository = method (code)
  let t :: <slot-access-engine-repository>
    = system-allocate-simple-instance(<slot-access-engine-repository>);
  // if (code)
  //   let code :: <integer> = code;
  //   engine-node-code(t) := code 
  // end;
  engine-node-table(t) := #[];
  t
end method;


define variable *slot-access-engine-repositories* :: <simple-object-vector>
     = begin
	 let v :: <simple-object-vector>
	   = make(<simple-object-vector>, size: engine-node$k-slot-engine-node-count);
	 local method loop (i :: <integer>)
		 if (i < engine-node$k-slot-engine-node-count)
		   v[i] := make-slot-access-engine-repository(i + engine-node$k-first-slot-engine-node);
		   loop(i + 1)
		 end if
	       end method;
	 loop(0);
	 v
       end;

define constant $slot-access-engine-repository-lock :: <simple-lock>
    = make-simple-lock();


define macro with-slot-access-engine-repository-locked

  { with-slot-access-engine-repository-locked (?object:expression) 
      ?body:body 
    end 
  }
  => 
  { with-lock ($slot-access-engine-repository-lock)
      ?object ; 
      ?body
    end with-lock
  }
end macro;


define constant get-slot-access-engine-node = method
    (code :: <integer>, setter?, 
     index :: <integer>, integer-data :: <integer>)
  let code :: <integer> = if (setter?) code + 1 else code end;
  let rep-offset :: <integer> = code - engine-node$k-first-slot-engine-node;
  let reps :: <simple-object-vector> = *slot-access-engine-repositories*;
  let repository :: <slot-access-engine-repository> = vector-element(reps, rep-offset);
  get-from-repository(repository, index, 
		      method () 
			let e :: <slot-access-engine-node>
			  = bootstrap-allocate-engine-node(code, 0);
			set-slot-engine-node-offset(e, integer-data);
			primitive-initialize-engine-node(e);
			e
		      end)
end method;

define constant get-repeated-slot-access-engine-node = method
    (code :: <integer>, setter?, 
     index :: <integer>, integer-data :: <integer>,
     size-index :: <integer>, size-offset :: <integer>)
  let code :: <integer> = if (setter?) code + 1 else code end;
  let rep-offset :: <integer> = code - engine-node$k-first-slot-engine-node;
  let reps :: <simple-object-vector> = *slot-access-engine-repositories*;
  let repository :: <slot-access-engine-repository> = vector-element(reps, rep-offset);
  let diff-index :: <integer> = index - size-index - 1;
  // if (diff-index < 0) error("wtf?") end;
  assert(diff-index >= 0, 
	 "get-repeated-slot-access-engine-node: negative diff-index %= code %= index %= integer-data %= size-index %= size-offset %=",
	 diff-index, code, index, integer-data, size-index, size-offset);
  let r2 :: <slot-access-engine-repository>
    = get-from-repository(repository, index, method () make-slot-access-engine-repository(#f) end);
  get-from-repository(r2, diff-index,
		      method ()
			let e :: <repeated-slot-access-engine-node> 
			  = bootstrap-allocate-engine-node(code, 0);
			set-slot-engine-node-offset(e, integer-data);
			slot-engine-node-size-offset(e) := size-offset;
			primitive-initialize-engine-node(e);
			e
		      end method)
end method;


define constant get-from-repository = method
    (repository :: <slot-access-engine-repository>, index :: <integer>, create-new-one :: <function>)
  let table :: <simple-object-vector> = engine-node-table(repository);
  let len :: <integer> = size(table);
  ( (index < len & vector-element(table, index))
     |
     (with-slot-access-engine-repository-locked (repository)
	let table :: <simple-object-vector>
	= begin
	    let table :: <simple-object-vector> = engine-node-table(repository);
	    let len :: <integer> = size(table);
	    if (index < len)
	      table
	    else
	      let newtable :: <simple-object-vector>
		= %make-simple-vector(logand(index + 15 + 1, -16), #f);
	      local method fill (i :: <integer>)
		      if (i == 0)
			engine-node-table(repository) := newtable
		      else
			let i :: <integer> = i - 1;
			vector-element(newtable, i) := vector-element(table, i);
			fill(i)
		      end if
		    end method;
	      fill(len)
	    end if
	  end;
      (vector-element(table, index)
	 |
	 (vector-element(table, index) := create-new-one()))
     end)
     )
end method;



define function make-slot-accessing-next-method-chain (ds :: <dispatch-state>, m :: <accessor-method>)
  let engine :: <engine-node> = make-slot-access-engine-node(m, ds);
  engine ~== $absent-engine-node & pair(engine, %ds-gf(ds))
end function;
