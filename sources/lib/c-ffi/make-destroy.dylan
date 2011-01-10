module:    c-ffi-implementation
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


define sideways inline method make (class :: subclass(<C-pointer>),
			     #rest keys,
			     #key address = #f,
			          allocator :: <function> = default-allocator,
			          element-count :: <integer> = 1,
			          extra-bytes :: <integer> = 0)
 => (ptr :: <C-pointer>);

  let instantiable-class = concrete-class(class) | class;
  if (address)
    make-c-pointer-internal
      (instantiable-class, as(<machine-word>, address), keys);
  else
    let ref-type = referenced-type(class);
    if (ref-type)
      make-c-pointer-internal
	(instantiable-class,
	 allocator((size-of(ref-type) * element-count) + extra-bytes),
	 keys);
    else
      // TODO: Error here. Can't allocate anything
      make-c-pointer-internal
	(instantiable-class, as(<machine-word>, 0), keys);
    end if;
  end if;
end method make;

define concrete C-subtype <C-heap-pointer> (<C-void*>) end;


define open generic destroy (ptr :: <C-pointer>, #key) => ();


define inline method destroy (ptr :: <C-statically-typed-pointer>,
			      #key deallocator :: <function> = default-deallocator)
 => ();
  deallocator(pointer-address(ptr));
end method destroy;


define inline method null-pointer (class :: subclass(<C-pointer>))
 => (obj :: <C-pointer>);
  // could cache this for the class.
  make(class, address: 0);
end method null-pointer;

define inline method null-pointer? (ptr :: <C-pointer>)
 => (b :: <boolean>);
  primitive-machine-word-equal?
    (primitive-cast-pointer-as-raw(primitive-unwrap-c-pointer(ptr)),
     integer-as-raw(0))
end method null-pointer?;

/*
[gts, 11/97, wait until harp backend ready]
once this is done with a converter, following macro should be commented out.
*/

define macro with-stack-structure
  { with-stack-structure (?:name :: ?type:expression, ?keys:*)
     ?:body
  end }
    =>
    // !@#$ really needs to use the compiler primitive that I don't
    // know about yet 
    { begin
	let with-stack-struct-temp = #f;
	block ()
	  with-stack-struct-temp := make(?type, ?keys);
	  let ?name :: ?type = with-stack-struct-temp;
	  ?body;
	  afterwards		// this should be cleanup,
				// but Gray complained of slowness
	    if(with-stack-struct-temp)
	      destroy(with-stack-struct-temp)
	    end if;
	end block;
      end }
    keys:
    { } => { }
    { ?expr:*, ...} => { ?expr, ... }
end macro;

/*
define macro with-pinned-objects
  { with-pinned-objects (?objects:*) ?:body end }
    => { ?objects };

  objects:
    { } => { ?body };
    { ?object:name, ... }
      => { begin
             let ?object = primitive-pin-object(?object);
             with-pinned-objects(...)
               ?body
             end;
             primitive-unpin-object(?object);
           end };

end macro;

define macro with-c-string
  { with-c-string (?var:name = ?string:expression)
     ?:body
     end }
    => { begin
	   let string = ?string;
           with-pinned-objects(string)
	     block ()
               let raw-address = primitive-string-as-raw(pinned-string);
               let str-address = primitive-wrap-machine-word(raw-address);
               let ?var = make(<c-string>, address: str-address);
	       ?body
	     cleanup
	     primitive-unpin-object(pinned-string);
	     #f
	   end;
	 end }
end macro;
*/

define macro with-c-string
  { with-c-string (?var:name = ?string:expression)
     ?:body
     end }
    => { begin
	   let string = ?string;
           let pinned-string = primitive-pin-object(string);
	   block ()
             let raw-address
	       = primitive-cast-pointer-as-raw(primitive-string-as-raw(pinned-string));
             let str-address = primitive-wrap-machine-word(raw-address);
             let ?var = make(<c-string>, address: str-address);
	     ?body
	   cleanup
	     primitive-unpin-object(pinned-string);
	     #f
	   end;
	 end }
end macro;

/*


define method \- (pointer1 :: <C-pointer>, pointer2 :: <C-pointer>)
 => (m :: <machine-word>);
  %-(pointer-address(pointer1), pointer-address(pointer2))
end;


define method \+ (pointer1 :: <C-pointer>, pointer2 :: <C-pointer>)
 => (m :: <machine-word>);
  %+(pointer-address(pointer1), pointer-address(pointer2))
end;

*/


define sideways inline method \=
    (pointer1 :: <C-pointer>, pointer2 :: <C-pointer>)
 => (b :: <boolean>);
  primitive-machine-word-equal?
    (primitive-unwrap-c-pointer(pointer1),
     primitive-unwrap-c-pointer(pointer2))
end;

define sideways inline method \<
    (pointer1 :: <C-pointer>, pointer2 :: <C-pointer>)
 => (b :: <boolean>);
  primitive-machine-word-less-than?
    (primitive-unwrap-c-pointer(pointer1),
     primitive-unwrap-c-pointer(pointer2))
end;


// -----


define inline function pointer-cast
    (class :: subclass(<C-pointer>), ptr :: <C-pointer>)
 => (new :: <C-pointer>);
  make(class, address: pointer-address(ptr))
end;

