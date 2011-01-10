module:    c-ffi-implementation
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// top level exported <C-string>
/// notice that import-map is not needed since the default import for
/// any pointer is itself
define open abstract simple-c-mapped-subtype <c-string>
      (<c-raw-unsigned-char*>, <string>) 
  export-map type-union(<byte-string>, <c-string>),
    export-function: export-c-string;
  pointer-type <c-string*>;
end;

/// the instantiation of <c-string>
define sealed concrete c-subtype <instantiation-of-c-string> (<C-string>)
end;


/// make a <C-string>.  Notice size and fill keywords accepted as
/// any collection.

// TODO: CORRECTNESS: What are the real restrictions?

define inline function check-c-string-size-options 
    (class :: <class>, size, element-count) 
 => (size :: false-or(<integer>), element-count :: <integer>)
  case
    size
      => if (element-count & size + 1 ~== element-count)
           error("The element-count: %=, size: %= options to make on "
                 "%= are inconsistent.", element-count, size, class);
         end;
         if (size < 0)
           error("The size: %= option to make on %= is less than the "
                 "minimum size 0.", size, class);
         end;
         values(size, size + 1);
    element-count
      => values(#f, element-count);
    otherwise
      => values(0, 1);
  end;
end function;

define method make 
    (class == <instantiation-of-c-string>, #rest other-keys,
       #key size = #f, element-count = #f,
            fill = $not-given, address = #f)
 => (o :: <instantiation-of-c-string>);
  if (address)
    apply(next-method, class, other-keys);
  else
    let (size, element-count)
      = check-c-string-size-options(class, size, element-count);
    // Extra-bytes and that stuff is handled in the default method, we just
    // arrange to default element-count to 1 + size to account for the null
    // terminator.
    let result = apply(next-method, class,  element-count: element-count,
                       other-keys);
    if (size)
      let size :: <integer> = size;
      let raw-pointer :: <raw-c-pointer> = primitive-unwrap-c-pointer(result);
      if (fill)
        // check type and fill in default.
        let fill :: <byte-character>
          = if (given?(fill)) fill else ' ' end;
        let raw-fill :: <raw-integer> 
          = integer-as-raw(as(<integer>, fill));
        for (i :: <integer> from 0 below size)
          primitive-c-unsigned-char-at-setter
            (raw-fill, raw-pointer, integer-as-raw(i), integer-as-raw(0));
        end;
      end;
      // Null terminate.
      primitive-c-unsigned-char-at-setter
        (integer-as-raw(0), raw-pointer, 
           integer-as-raw(size), integer-as-raw(0));
    end;
    result
  end;
end;

define method make
    (class == <C-string>, #rest other-keys,
      #key size = #f, element-count = #f,
           fill = $not-given, address = #f)
 => (o :: <instantiation-of-c-string>)
  apply(make, <instantiation-of-c-string>, other-keys);
end method;

/// A constructor for static C strings.

// TODO: CORRECTNESS: Make a macro? 

define function C-string-constant
    (string :: <byte-string>) => (value :: <C-string>)
  make(<C-string>,
       address: primitive-wrap-machine-word
                  (primitive-cast-pointer-as-raw
                    (primitive-string-as-raw(string))))
end function;

/*
// For some reason this doesn't get called with the right stuff.
/// This initialize method that accepts a fill: keyword can be inherited. 
define method initialize (result :: <C-string>,
			  #key fill = $not-given,
			       address,
			       element-count,
			  #all-keys)
 => ();
  if(~address & fill)
    if (~given?(fill))
      // defaults to space if not given
      fill := ' ';
    end;
    let raw-fill :: <raw-integer> = integer-as-raw(as(<integer>, fill));
    let raw-pointer :: <raw-c-pointer> = primitive-unwrap-c-pointer(result);
    for(i from 0 below element-count)
      primitive-c-unsigned-char-at-setter(raw-fill, raw-pointer,
					  integer-as-raw(i), integer-as-raw(0));
    end;
    primitive-c-unsigned-char-at-setter(integer-as-raw(0), raw-pointer,
					integer-as-raw(element-count), integer-as-raw(0));
  end;
  values();
end;
*/

    
/// pointer-value in a C-string returns a character.
define method pointer-value (c-str :: <c-string>, #key index :: <integer> = 0)
 => (c :: <byte-character>);
  as(<byte-character>,
     raw-as-integer
       (primitive-c-unsigned-char-at(primitive-unwrap-c-pointer(c-str),
			             integer-as-raw(index),
			             integer-as-raw(0))));
end;


define method pointer-value-setter (c :: <byte-character>,
				    c-str :: <c-string>, #key index :: <integer> = 0)
 => (c :: <byte-character>);
  primitive-c-unsigned-char-at-setter(integer-as-raw(as(<integer>, c)),
				      primitive-unwrap-c-pointer(c-str),
				      integer-as-raw(index),
				      integer-as-raw(0));
  c
end;

/// 
define method export-c-string (obj :: <c-pointer>) => (obj :: <C-pointer>);
  obj
end;

define method export-c-string (obj :: <byte-string>)
 => (p :: <c-raw-unsigned-char*>);
  make-c-pointer(<c-raw-unsigned-char*>,
		 primitive-cast-pointer-as-raw(primitive-string-as-raw(obj)),
		 #[])
end;




define function cstr-next-state (collection :: <C-string>, state :: <integer>)
  => (next :: <integer>);
  state + 1
end;

define function cstr-finished-state (collection :: <C-string>,
				     state :: <integer>,
				     limit :: <boolean>)
 => (b :: <boolean>)
  null-pointer?(collection)
  | 0 = raw-as-integer
          (primitive-c-unsigned-char-at(primitive-unwrap-c-pointer(collection),
					integer-as-raw(0),
					integer-as-raw(state)))
end;

define function cstr-current-key (c :: <C-string>,
				  i :: <integer>)
 => (i :: <integer>);
  i
end;

define function cstr-current-element (c :: <C-string>,
				      i :: <integer>)
 => (i :: <character>);
  pointer-value(c, index: i)
end;

define function cstr-current-element-setter (new :: <character>,
					     c :: <C-string>,
					     i :: <integer>)
 => (i :: <character>);
  pointer-value-setter(new, c, index: i)
end;

define function cstr-copy-state (c :: <C-string>,
				 s :: <integer>)
 => (s :: <integer>);
  s
end;


define method forward-iteration-protocol (cstr :: <C-string>)
 => (initial-state :: <object>, limit :: <object>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values(0,
	 #f,
	 cstr-next-state,
	 cstr-finished-state,
	 cstr-current-key,
	 cstr-current-element,
	 cstr-current-element-setter,
	 cstr-copy-state)
end;
	 
define constant $not-given = list(#"not-given");

define function given? (obj :: <object>) => (b :: <boolean>);
  obj ~== $not-given
end;


define method element (cstr :: <C-string>, key :: <integer>,
		       #key default = $not-given)
 => (c :: <character>);
  if (key < 0)
    if (given?(default))
      default
    else
      error("ELEMENT outside of range: %= %=", cstr, key)
    end;
  else
    pointer-value(cstr, index: key);
  end;
end;
  
define method element-setter (c :: <character>,
			      cstr :: <C-string>,
			      key :: <integer>)
 => (c :: <character>);
  if (key < 0)
      error("ELEMENT outside of range: %= %=", cstr, key)
  end;
  pointer-value-setter(c, cstr, index: key);
end;

define method size (c :: <C-string>) => (s :: <integer>);
  if (null-pointer?(c))
    0
  else
    raw-as-integer(primitive-strlen(primitive-unwrap-c-pointer(c)))
  end;
end;


define method empty? (obj :: <C-string>) => (b :: <boolean>);
  null-pointer?(obj)
    | 0 = raw-as-integer
            (primitive-c-unsigned-char-at(primitive-unwrap-c-pointer(obj),
					  integer-as-raw(0),
					  integer-as-raw(0)))
end;

define method \= (string-1 :: <C-string>, string-2 :: <C-string>)
 => (result :: <boolean>)
  if ((string-1 == string-2) | (empty?(string-1) & empty?(string-2)))
    #t;
  elseif (empty?(string-1) | empty?(string-2))
    #f;
  else
    for (c1 :: <character> in string-1, 
         c2 :: <character> in string-2,
         eq = #t then c1 = c2,
         while: eq)
    finally 
      eq
    end for;
  end if;
end method;


// <C-unicode-string>
// ....

/// top level exported <c-unicode-string>
/// notice that import-map is not needed since the default import for
/// any pointer is itself
define open /* abstract */ simple-c-mapped-subtype <c-unicode-string>
      (<c-unsigned-short*>, <string>) 
  export-map type-union(<unicode-string>, <c-unicode-string>),
    export-function: export-c-string;
  pointer-type <c-unicode-string*>;
end;

/// the instantiation of <c-unicode-string>
define sealed concrete c-subtype <instantiation-of-c-unicode-string> (<c-unicode-string>)
end;


/// make a <c-unicode-string>.  Notice size and fill keywords accepted as
/// any collection.
define method make 
    (class == <c-unicode-string>, #rest other-keys,
       #key size = #f, element-count = #f,
            fill = $not-given, address = #f)
 => (o :: <instantiation-of-c-unicode-string>);
  if (address)
    apply(make, <instantiation-of-c-unicode-string>, other-keys);
  else
    let (size, element-count)
      = check-c-string-size-options(class, size, element-count);
    // Extra-bytes and that stuff is handled in the default method, we just
    // arrange to default element-count to 1 + size to account for the null
    // terminator.
    let result = apply(make, <instantiation-of-c-unicode-string>,
		       element-count: element-count, other-keys);
    if (size)
      let size :: <integer> = size;
      let raw-pointer :: <raw-c-pointer> = primitive-unwrap-c-pointer(result);
      if (fill)
        // Check type and fill in default.
        let fill :: <unicode-character>
	  = as(<unicode-character>, if (given?(fill)) fill else ' ' end);
        let raw-fill :: <raw-integer> 
          = integer-as-raw(as(<integer>, fill));
        for (i :: <integer> from 0 below size)
          primitive-c-unsigned-short-at-setter
            (raw-fill, raw-pointer, integer-as-raw(i), integer-as-raw(0));
        end;
      end;
      // Null terminate.
      primitive-c-unsigned-short-at-setter
	(integer-as-raw(0), raw-pointer, integer-as-raw(size), integer-as-raw(0));
    end;
    result
  end;
end;
  
define method C-unicode-string-constant
    (string :: <byte-string>) => (value :: <C-unicode-string>)
  as(<C-unicode-string>, string);
end method;

define method C-unicode-string-constant
    (string :: <unicode-string>) => (value :: <C-unicode-string>)
  make(<C-unicode-string>,
       address: primitive-wrap-machine-word
                  (primitive-cast-pointer-as-raw
                    (primitive-string-as-raw(string))))
end method;

/*
// For some reason this doesn't get called with the right stuff.
/// This initialize method that accepts a fill: keyword can be inherited. 
define method initialize (result :: <c-unicode-string>,
			  #key fill = $not-given,
			       address,
			       element-count,
			  #all-keys)
 => ();
  if(~address & fill)
    if (~given?(fill))
      // defaults to space if not given
      fill := ' ';
    end;
    let raw-fill :: <raw-integer> = integer-as-raw(as(<integer>, fill));
    let raw-pointer :: <raw-c-pointer> = primitive-unwrap-c-pointer(result);
    for(i from 0 below element-count)
      primitive-c-unsigned-short-at-setter(raw-fill, raw-pointer,
					   integer-as-raw(i), integer-as-raw(0));
    end;
    primitive-c-unsigned-short-at-setter(integer-as-raw(0), raw-pointer,
					 integer-as-raw(element-count), integer-as-raw(0));
  end;
  values();
end;
*/


/// pointer-value in a C-unicode-string returns a character.

define inline function pointer-integer-value 
    (c-str :: <c-unicode-string>, #key index :: <integer> = 0) => (val :: <integer>)
  raw-as-integer
    (primitive-c-unsigned-short-at
      (primitive-unwrap-c-pointer(c-str),
       integer-as-raw(index),
       integer-as-raw(0)));
end function;

define method pointer-value
    (c-str :: <c-unicode-string>, #key index :: <integer> = 0)
 => (c :: <character>);
  as(<unicode-character>, pointer-integer-value(c-str, index: index))
end;

define method pointer-value-setter
    (c :: <unicode-character>, c-str :: <c-unicode-string>, #key index :: <integer> = 0)
 => (c :: <unicode-character>);
  primitive-c-unsigned-short-at-setter
    (integer-as-raw(as(<integer>, c)),
     primitive-unwrap-c-pointer(c-str),
     integer-as-raw(index),
     integer-as-raw(0));
  c
end;

define method pointer-value-setter
    (c :: <byte-character>, c-str :: <c-unicode-string>, #key index :: <integer> = 0)
 => (c :: <byte-character>);
  primitive-c-unsigned-short-at-setter
    (integer-as-raw(as(<integer>, as(<unicode-character>, c))),
     primitive-unwrap-c-pointer(c-str),
     integer-as-raw(index),
     integer-as-raw(0));
  c
end;

/// 

define method export-c-string (obj :: <unicode-string>)
 => (p :: <c-raw-signed-char*>);
  make-c-pointer(<c-raw-unsigned-short*>,
		 primitive-cast-pointer-as-raw(primitive-string-as-raw(obj)),
		 #[])
end;

define inline function custr-next-state 
    (collection :: <c-unicode-string>, state :: <integer>)
 => (next :: <integer>);
  state + 1
end;

define inline function custr-finished-state 
    (collection :: <c-unicode-string>, state :: <integer>, limit :: <boolean>)
 => (b :: <boolean>)
  null-pointer?(collection)
    | 0 = pointer-integer-value(collection, index: state)
end;

define inline function custr-current-key 
    (c :: <c-unicode-string>, i :: <integer>)
 => (i :: <integer>);
  i
end;

define inline function custr-current-element 
    (c :: <c-unicode-string>, i :: <integer>)
 => (i :: <character>);
  pointer-value(c, index: i)
end;

define inline function custr-current-element-setter 
    (new :: <character>, c :: <c-unicode-string>, i :: <integer>)
 => (i :: <character>);
  pointer-value-setter(new, c, index: i)
end;

define inline function custr-copy-state 
    (c :: <c-unicode-string>, s :: <integer>)
 => (s :: <integer>);
  s
end;

define method forward-iteration-protocol
    (custr :: <c-unicode-string>)
 => (initial-state :: <object>, limit :: <object>,
     next-state :: <function>, finished-state? :: <function>,
     current-key :: <function>,
     current-element :: <function>, current-element-setter :: <function>,
     copy-state :: <function>);
  values(0,
	 #f,
	 custr-next-state,
	 custr-finished-state,
	 custr-current-key,
	 custr-current-element,
	 custr-current-element-setter,
	 custr-copy-state)
end;
	 
define method element
    (custr :: <c-unicode-string>, key :: <integer>,
       #key default = $not-given)
 => (c :: <character>);
  if (key < 0)
    if (given?(default))
      default
    else
      error("ELEMENT outside of range: %= %=", custr, key)
    end;
  else
    pointer-value(custr, index: key);
  end;
end;
  
define method element-setter
    (c :: <character>, custr :: <c-unicode-string>, key :: <integer>)
 => (c :: <character>);
  if (key < 0)
    error("ELEMENT outside of range: %= %=", custr, key)
  end;
  pointer-value-setter(c, custr, index: key);
end;

define method size (c :: <c-unicode-string>) => (s :: <integer>);
  if (null-pointer?(c))
    0
  else
    for (i from 0, until: pointer-integer-value(c, index: i) = 0) 
    finally i
    end;
  end;
end;

define method empty? (obj :: <c-unicode-string>) => (b :: <boolean>);
  null-pointer?(obj)
    | 0 = pointer-integer-value(obj, index: 0)
end;

define method \=
    (string-1 :: <C-unicode-string>, string-2 :: <C-unicode-string>)
 => (result :: <boolean>)
  if ((string-1 == string-2) | (empty?(string-1) & empty?(string-2)))
    #t;
  elseif (empty?(string-1) | empty?(string-2))
    #f;
  else
    for (c1 :: <character> in string-1, 
         c2 :: <character> in string-2,
         eq = #t then c1 = c2,
         while: eq)
    finally 
      eq
    end for;
  end if;
end method;


// ------


/// <c-character>
define open simple-c-mapped-subtype <C-character>
  (<C-raw-unsigned-char>)
  export-map <character>,
    export-function: method (x :: <character>)
		      => (m :: <machine-word>);
		       as(<machine-word>, as(<integer>, x));
		     end;
  import-map <character>,
    import-function:
      method (x :: <machine-word>) => (i :: <character>);
	as(<character>, as-unsigned(<integer>, x));
      end;
  pointer-type <C-character*>;
end;

/*

/// <C-dylan-object>
define open simple-c-mapped-subtype <C-dylan-object>
  (<C-raw-unsigned-long>)
  export-map type-union(<integer>, <object>),
    export-function: method (x :: <object>)
		      => (m :: <machine-word>);
		       as(<machine-word>,
			  maybe-register-c-object(x));
		     end;
  import-map <object>,
    import-function:
      method (x :: <machine-word>) => (i :: <object>);
	lookup-c-object(as-unsigned(<integer>, x));
      end;
  pointer-type <C-dylan-object*>;
end;


// maybe both these tables should be weak with respect to object
define constant $c-object-integer-table = make(<table>);
define constant $c-integer-object-table = make(<stretchy-vector>);
define constant $c-no-object = pair(#"no", #"object");
define variable *c-next-object-index* = 0;

define method maybe-register-c-object (obj :: <object>)
 => (i :: <integer>);
  let maybe-answer = element($c-object-integer-table, obj, default: #f);
  if (maybe-answer)
    maybe-answer
  else
    // !@#$ this should really be interlocked against other threads
    let answer = *c-next-object-index*;
    *c-next-object-index* := *c-next-object-index* + 1;
    $c-object-integer-table[obj] := answer;
    $c-integer-object-table[answer] := obj;
    answer
  end if
end method;
    
define method lookup-c-object (i :: <integer>)
 => (obj :: <object>);
  let obj = element($c-integer-object-table, i, default: $c-no-object);
  if (obj = $c-no-object)
    // !@#$ raise an error here
    #f
  else
    obj
  end if;
end method;

*/

/*
// not used yet, but clearly useful
define method unregister-c-object (obj :: <object>) => ();
  let index = element($c-object-integer-table, obj, default: #f);
  if (index)
    remove-key!($c-object-integer-table, obj);
    $c-integer-object-table[index] := $c-no-object;
  end if;
  values();
end;
*/  


// ----
/// <C-boolean>

define method import-c-boolean (i :: <machine-word>) => (b :: <boolean>);
  if (as(<integer>, i) = 0) #f else #t end
end;

define method export-c-boolean (b :: <boolean>) => (m :: <machine-word>);
  if (b) as(<machine-word>, 1) else as(<machine-word>, 0) end
end;


define simple-c-mapped-subtype <C-boolean> (<C-raw-signed-int>)
  map <boolean>,
    import-function: method (i :: <machine-word>) => (b :: <boolean>);
		       if (i = as(<machine-word>, 0)) #f else #t end end,
    export-function: method (b :: <boolean>) => (m :: <machine-word>);
		       if (b)
			 as(<machine-word>, 1)
		       else
			 as(<machine-word>, 0)
		       end
		     end;
  pointer-type <C-boolean*>;
end;


///---*** NOTE: I think this code needs work w.r.t <machine-word>s 
///---*** being larger than <integer>s! (-Palter)

/// <C-dylan-object>
define open simple-c-mapped-subtype <C-dylan-object> (<C-void*>)
  pointer-type <c-dylan-object*>;
end;


/// implemented as a table/handle

define class <c-dylan-object-reference> (<object>)
  constant slot object-handle :: <c-dylan-object>, required-init-keyword: handle:;
  slot ref-count :: <integer> = 0;
end;

define constant $c-dylan-object-table = make(<table>);
define constant $c-dylan-handle-table = make(<stretchy-vector>);
define variable *c-dylan-object-current-handle* = 0;

define method generate-c-dylan-object-handle (obj :: <object>)
 => (i :: <integer>)
  // locking may be necessary here.
  block (return)
    for (i from 1 to *c-dylan-object-current-handle*)
      if ($c-dylan-handle-table[i] == #f)
	$c-dylan-handle-table[i] := obj; // set it to something to reserve slot
	return(i)
      end if;
    end for;
    *c-dylan-object-current-handle* := *c-dylan-object-current-handle* + 1;
    return(*c-dylan-object-current-handle*); 
  end block;
end;

define method as (c == <integer>, handle :: <c-dylan-object>)
 => (int :: <integer>)
  as(<integer>, pointer-address(handle));
end;

define method initialize (ref :: <c-dylan-object-reference>,
			  #key handle, object)
  element($c-dylan-handle-table, as(<integer>, handle)) := object;
  element($c-dylan-object-table, object) := ref;
end;


/// exported function
define method register-c-dylan-object (obj :: <object>) => ()
  let existing-reference = element($c-dylan-object-table, obj, default: #f);
  let reference
    = existing-reference
      | make(<c-dylan-object-reference>,
	     handle: make(<c-dylan-object>,
			  address: generate-c-dylan-object-handle(obj)),
	     object: obj);
  reference.ref-count := reference.ref-count + 1;
  values();
end;

/// exported function
define method unregister-c-dylan-object (obj :: <object>) => ();
  let reference = element($c-dylan-object-table, obj, default: #f);
  if (reference)
    reference.ref-count := reference.ref-count - 1;
    if (reference.ref-count == 0)
      remove-key!($c-dylan-object-table, obj);
      $c-dylan-handle-table[as(<integer>, reference.object-handle)]
	:= #f;
    end;
  end;
  values();
end;

/// exported function
define method export-c-dylan-object (obj :: <object>)
 => (handle :: <c-dylan-object>)
  let existing-reference = element($c-dylan-object-table, obj, default: #f);
  unless (existing-reference)
    // error out here
  end;
  existing-reference.object-handle;
end;

/// exported function
define method import-c-dylan-object (handle :: <c-dylan-object>)
 => (obj :: <object>)
  let existing-object = element($c-dylan-handle-table,
				as(<integer>, handle),
				default: #f);
  unless (existing-object)
    // error out here
  end;
  existing-object
end;
