module:    base-harp
Synopsis:  Conditional slot-packing
Author:    Nosa Omo
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/// Conditional slot-packing

// This should really live in a shared utility library
// since it has universal applicability

/*

These slot-packing macros are used as follows:


* The abstract class whose slots are to be conditionally packed

define abstract class <slot-owner> (...)

  ...

  slot slot-owner-packed-slot :: 
    type-union(<integer>, <slot-owner-unpacked-slots>) = 0,
    init-keyword: packed-slot-keyword:;

  ...

end class;


* The concrete subclass whose instance stores the packed slots

define class <slot-owner-concrete-class> (<slot-owner>)
  ...
end class;


* The unpacked-slots class used when attempted packing fails the combined range checks

define class <slot-owner-unpacked-slots> (<object>)

  slot packed-slot-1-internal :: <packed-slot-1-type>, init-keyword: slot-1-keyword:;

  ...

  slot packed-slot-n-internal :: <packed-slot-n-type>, init-keyword: slot-n-keyword:;

end class;


* The conditional packed-slots definition

define packed-slots?(<slot-owner>)
  packed-slot-1     :: <packed-slot-1-type>, init-keyword: slot-1-keyword:, width: slot-1-width;
  ...
  packed-slot-n     :: <packed-slot-n-type>, init-keyword: slot-n-keyword:, width: slot-n-width;
end;


* The make method that either makes the packed slot if combined range checks succeed,
  or the unpacked-slots object if checks fail


define method make
    (class == <slot-owner>,
     #rest keys,
     #key slot-1-keyword-arg,
          ...
          slot-n-keyword-arg,
     #all-keys) 
    => (r :: <slot-owner-concrete-class>)
  let slots =
    make-compiled-lambda-packed-slot?(slot-1-keyword-arg,
				      ...
				      slot-n-keyword-arg)
  | (make(<slot-owner-unpacked-slots>,
	  slot-1-keyword: slot-1-keyword-arg,
          ...
	  slot-n-keyword: slot-n-keyword-arg));

  apply(make, <slot-owner-concrete-class>,
	packed-slot-keyword: slots,
	keys);

end method;


* You can pack an object in its entirety by also adding the following dummy 
  definitions, renaming previously defined <slot-owner> as <slot-owner-unpacked-slots>,
  and recoding make method above to just return slots


define constant <slot-owner> = type-union(<integer>, <slot-owner-unpacked-slots>);
define constant slot-owner-packed-slot = identity;

define constant dummy-setter = method(value, instance) value end;
define constant function-packed-slot-1-internal-setter = dummy-setter;
...
define constant function-packed-slot-n-internal-setter = dummy-setter;
define constant slot-owner-packed-slot-setter = dummy-setter;




*/


define macro packed-slots?-definer
  { define packed-slots? ("<" ## ?slot-owner:name ## ">")
      ?slots:*
    end
  }
    =>
  {
   define packed-slots?-aux "<" ## ?slot-owner ## ">"
     (?slots) (?slots) (?slots) (?slots)
   end
  }

end macro;

define macro packed-slots?-aux-definer
  { define packed-slots?-aux "<" ## ?slot-owner:name ## ">"
      (?slots:*) (?slot-params) (?slot-names) (?slot-args)
    end
  }
    => { 
	 define packed-slots?-accessors "<" ## ?slot-owner ## ">" (0)
           ?slots
         end;

         define method "make-" ## ?slot-owner ## "-packed-slot?"(?slot-params)
	  => (packed-slot? :: false-or(<integer>))
	     pack-slots? (?slot-names) () end
	 end method;

         define inline method "make-" ## ?slot-owner ## "-unpacked-slots"(packed-slot :: <integer>)
	  => (unpacked-slots :: "<" ## ?slot-owner ## "-unpacked-slots>")
	     make("<" ## ?slot-owner ## "-unpacked-slots>",
		  ?slot-args)
	 end method;


       }

slot-params:
  { } => { }

  { constant ?slot-params-spec:*;  ... } => { ?slot-params-spec, ... }

  { ?slot-params-spec:*;  ... } => { ?slot-params-spec, ... }

slot-params-spec:

  { ?:name :: ?slot-type:name, ?keys:* } => { ?name :: ?slot-type }

slot-names:
  { } => { }

  { constant ?slot-names-spec:*;  ... } => { ?slot-names-spec, ... }

  { ?slot-names-spec:*;  ... } => { ?slot-names-spec, ... }

slot-names-spec:

  { ?:name :: ?slot-type:name, ?keys:* } => { ?name }


slot-args:
  { } => { }

  { constant ?slot-args-spec:*;  ... } => { ?slot-args-spec, ... }

  { ?slot-args-spec:*;  ... } => { ?slot-args-spec, ... }

slot-args-spec:

  { ?:name :: ?slot-type:name, init-keyword: ?:symbol, ?keys:* }
    => { ?symbol ?name ## "-internal"(packed-slot) }

end macro;

define macro pack-slots?

  { pack-slots? () (?packed-slots:*) end }
    =>
  { logior(?packed-slots) }


  {  pack-slots? (?:name, ?slots:*) () end }
    =>
  {  let packed-slot = "pack-" ## ?name (?name);

     if (packed-slot)
       pack-slots? (?slots) (packed-slot) end
     end }


  {  pack-slots? (?:name, ?slots:*) (?packed-slots:*) end }
    =>
  {  let packed-slot = "pack-" ## ?name (?name);

     if (packed-slot)
       pack-slots? (?slots) (?packed-slots, packed-slot) end
     end }

end macro;

define macro packed-slots?-accessors-definer
  { define packed-slots?-accessors ?slot-owner:name (?count:expression)
    end
  } 
    =>  { }

  { define packed-slots?-accessors  "<" ## ?slot-owner:name ## ">" (?count:expression)
      ?:name \:: ?slot-type:name, 
      #key ?width:expression = 1,
           ?init-keyword:symbol;
      ?more-slots:*
    end
  }
    =>
    {
	 define method ?name ## "-setter" (value :: ?slot-type, instance :: "<" ## ?slot-owner ## ">")
	  => (result :: ?slot-type)
	   let packed-slot = ?slot-owner ## "-packed-slot"(instance);
	   if (instance?(packed-slot, <integer>))
	     let packed-value :: false-or(<integer>) = "pack-" ## ?name(value);
	     ?slot-owner ## "-packed-slot"(instance) := 
	       if (packed-value)
		 let mask :: <integer> = lognot(ash(lognot(ash(-1, ?width)), ?count));
		 logior(logand(packed-slot, mask), packed-value)
	       else
		 let unpacked-slots :: "<" ## ?slot-owner ## "-unpacked-slots>" =
		   "make-" ## ?slot-owner ## "-unpacked-slots"(packed-slot);
		 ?name ## "-internal"(unpacked-slots) := value;
		 unpacked-slots
	       end if;
	   else
	     ?name ## "-internal-setter"(value, packed-slot)
	   end if;
	   value
	 end method;

         define packed-slots?-accessors-aux "<" ## ?slot-owner ## ">" (?count)
           ?name :: ?slot-type ?width
         end;

         define packed-slots?-accessors "<" ## ?slot-owner ## ">" (?count + ?width)
           ?more-slots
         end
    }

  { define packed-slots?-accessors  ?slot-owner:name (?count:expression)
      constant ?:name \:: ?slot-type:name, 
      #key ?width:expression = 1,
           ?init-keyword:symbol;
      ?more-slots:*
    end
  }
    =>
    {
         define packed-slots?-accessors-aux ?slot-owner (?count)
           ?name :: ?slot-type ?width
         end;

         define packed-slots?-accessors ?slot-owner (?count + ?width)
           ?more-slots
         end
    }
end macro; 

define macro packed-slots?-accessors-aux-definer

  { define packed-slots?-accessors-aux  "<" ## ?slot-owner:name ## ">" (?count:expression)
      ?:name \:: "<" ## ?slot-type:name ## ">"  ?width:expression
    end
  }
    =>
    {
	 define inline method ?name (instance :: "<" ## ?slot-owner ## ">") => (result :: "<" ## ?slot-type ## ">")
	   ?name ## "-internal"(?slot-owner ## "-packed-slot"(instance))
	 end method;

         define method ?name ## "-internal"(packed-slot :: <integer>) => (result :: "<" ## ?slot-type ## ">")
	   "unpack-" ## ?slot-type ## "?"(packed-slot, ?width, ?count)
	 end method;

         define inline method "pack-" ## ?name
	     (slot-value :: "<" ## ?slot-type ## ">")
	  => (packed-slot :: false-or(<integer>))
	   "pack-" ## ?slot-type ## "?"(slot-value, ?width, ?count)
	 end method;
    }
end macro; 


/// Some common conditional packing functions


// Only pack integers if they are in an unsigned range

define inline method pack-integer?
  (value :: <integer>, size :: <integer>, pos :: <integer>)
  => (packed-slot :: false-or(<integer>))
  let bit-pattern :: <integer> = ash(1, size);
  if (value < bit-pattern)
    ash(value, pos);
  end if;
end method;

define inline method unpack-integer?
  (packed-slot :: <integer>, size :: <integer>, pos :: <integer>)
  => (value :: <integer>)
  let bit-pattern :: <integer> = ash(1, size) - 1;
  let packed-slot :: <integer> = ash(packed-slot, - pos);
  logand(packed-slot, bit-pattern);
end method;


// Always pack booleans

define inline method pack-boolean?
  (value :: <boolean>, size :: <integer>, pos :: <integer>)
  => (packed-slot :: <integer>)
  if (value)
    ash(1, pos)
  else
    0
  end if;
end method;

define inline method unpack-boolean?
  (packed-slot :: <integer>, size :: <integer>, pos :: <integer>)
  => (value :: <boolean>)
  logbit?(pos, packed-slot)
end method;


// Don't pack arbitrary objects unless they are 1-bit encodeable (e.g. booleans)

define inline method pack-object?
  (value, size :: <integer>, pos :: <integer>)
  => (packed-slot :: false-or(<integer>))
  select(value by \==)
    #f => 0;
    #t => ash(1, pos);
    otherwise => #f;
  end select
end method;

define inline method unpack-object?
  (packed-slot :: <integer>, size :: <integer>, pos :: <integer>)
  => (value)
  logbit?(pos, packed-slot)
end method;
