Module:    c-ffi-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

//
// ELEMENT
//
define sideways inline method element 
    (ptr :: <C-statically-typed-pointer>, index :: <integer>, #key default)
 => (o :: <object>);
  pointer-value(ptr, index: index);
end method element;


//
// ELEMENT-SETTER
//
define sideways inline method element-setter 
    (new :: <object>, ptr :: <C-statically-typed-pointer>, index :: <integer>)
 => (o :: <object>);
  pointer-value-setter(new, ptr, index: index);
  new;
end method element-setter;


//
// POINTER-ADDRESS
//
define inline function pointer-address
    (ptr :: <C-pointer>) => (a :: <machine-word>);
  primitive-wrap-machine-word
    (primitive-cast-pointer-as-raw(primitive-unwrap-c-pointer(ptr)))
end function pointer-address;


//
// POINTER-VALUE & POINTER-VALUE-SETTER
//
define macro pointer-value-method-definer
  { define pointer-value-method ?pointer-class:name ?accessor:name
                                ?result-class:name 
  }
  =>
  { define sideways method pointer-value
	(ptr :: ?pointer-class, #key index :: <integer> = 0)
     => (result :: ?result-class);
      ?accessor(ptr, scaled-index: index)
    end method pointer-value;
    define sideways method pointer-value-setter
	(new-value :: ?result-class, ptr :: ?pointer-class,
	 #key index :: <integer> = 0)
     => (new-value :: ?result-class);
      ?accessor(ptr, scaled-index: index) := new-value
    end method pointer-value-setter;
  }
end macro pointer-value-method-definer;

define pointer-value-method <C-raw-signed-int*> C-signed-int-at <machine-word>;
define pointer-value-method <C-raw-unsigned-int*> C-unsigned-int-at <machine-word>;
define pointer-value-method <C-raw-unsigned-char*> C-unsigned-char-at <machine-word>;
define pointer-value-method <C-raw-signed-char*> C-signed-char-at <machine-word>;
// define pointer-value-method <C-raw-char*> C-char-at <machine-word>;
define pointer-value-method <C-raw-unsigned-short*> C-unsigned-short-at <machine-word>;
define pointer-value-method <C-raw-signed-short*> C-signed-short-at <machine-word>;
define pointer-value-method <C-raw-unsigned-long*> C-unsigned-long-at <machine-word>;
define pointer-value-method <C-raw-signed-long*> C-signed-long-at <machine-word>;

define pointer-value-method <C-float*> C-float-at <single-float>;
define pointer-value-method <C-double*> C-double-at <double-float>;
/*
define pointer-value-method <C-long-double*> C-long-double-at <extended-float>;
*/

// TODO: CORRECTNESS: Pointers to pointers don't seem to have 
// <C-pointer-to-pointer> as a superclass uniformly. As a hack,
// we can detect pointers to pointers by omission - i.e. if
// it ain't caught by one of the above methods, then it may be
// a pointer type...

define sideways method pointer-value
    (ptr :: <C-statically-typed-pointer>, /* <C-pointer-to-pointer> */
     #key index :: <integer> = 0)
 => (p1 :: <C-pointer>);
  let ref-type = referenced-type(object-class(ptr));
  if (subtype?(ref-type, <C-pointer>))
    make-c-pointer(ref-type,
		   primitive-cast-pointer-as-raw
		     (primitive-c-pointer-at
			(primitive-unwrap-c-pointer(ptr),
			 integer-as-raw(index),
			 integer-as-raw(0))),
		   #[]);
  else
    next-method();
  end if;
end method pointer-value;


define sideways method pointer-value-setter 
    (new :: <C-pointer>,
     ptr :: <C-statically-typed-pointer>, /* <C-pointer-to-pointer> */
     #key index :: <integer> = 0)
 => (new :: <C-pointer>);
  let ref-type = referenced-type(object-class(ptr));
  if (subtype?(ref-type, <C-pointer>))
    primitive-c-pointer-at-setter(primitive-unwrap-c-pointer(new),
				  primitive-unwrap-c-pointer(ptr),
				  integer-as-raw(index),
				  integer-as-raw(0));
    new;
  else
    next-method();
  end if;
end method pointer-value-setter;


//
// POINTER-VALUE-ADDRESS
//
define inline sideways method pointer-value-address
    (ptr :: <C-statically-typed-pointer>, #key index :: <integer> = 0)
 => (value :: <C-statically-typed-pointer>)
  let clss = object-class(ptr);
  let object-size :: <integer> = size-of(referenced-type(clss));
  make-c-pointer(clss,
		 primitive-machine-word-add
		   (primitive-cast-pointer-as-raw
		      (primitive-unwrap-c-pointer(ptr)),
		    integer-as-raw(index * object-size)),
		 #[]);
end method pointer-value-address;
