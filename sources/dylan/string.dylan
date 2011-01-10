Module:    internal
Author:    Jonathan Bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


// BOOTED: define ... abstract class <string> ... end;


/////////////////
// IMPLEMENTATION
/////////////////


// STRING

//
// AS
//

define sealed inline method as (class == <string>, string :: <string>) 
    => (s :: <string>)
  string
end method as;

define method as (class == <string>, collection :: <collection>)
     => (bs :: <byte-string>)
  as(<byte-string>, collection)
end method as;


//
// MAKE
//

define sealed inline method make (class == <string>, #key size = 0, fill = ' ')
 => (result :: <byte-string>)
  make(<byte-string>, size: size, fill: fill)
end method make;

// 
// SHARED STRING
// 

define macro shared-string-definer
  { define shared-string ?:name (#key ?fill:expression) }
    => { define method make
	      (class == "<" ## ?name ## "-string>", 
	       #key fill :: "<" ## ?name ## "-character>" = ?fill, size :: <integer> = 0)
	   => (res :: "<" ## ?name ## "-string>")
	   if (size = 0)
	     empty(class)
	   else
	     system-allocate-repeated-instance
	       ("<" ## ?name ## "-string>", "<" ## ?name ## "-character>", unbound(), size, fill);
	   end if
	 end method; 

         define sealed inline method concrete-limited-string-class
             (of == "<" ## ?name ## "-character>")
          => (type :: singleton("<" ## ?name ## "-string>"))
           "<" ## ?name ## "-string>"
         end method;

         define inline sealed method element
	     (string :: "<" ## ?name ## "-string>", index :: <integer>,
	      #key default = unsupplied())
	  => (character :: "<" ## ?name ## "-character>")
	   if (element-range-check(index, size(string)))
	     string-element(string, index)
	   else
	     if (unsupplied?(default))
	       element-range-error(string, index)
	     else 
	       check-type(default, element-type(string));
	       default
	     end if
	   end if
	 end method element;

         define inline sealed method element-no-bounds-check
	     (string :: "<" ## ?name ## "-string>", index :: <integer>, #key default)
		  => (character :: "<" ## ?name ## "-character>")
	   string-element(string, index)
	 end method element-no-bounds-check;

	 define inline sealed method element-setter 
	     (new-value :: "<" ## ?name ## "-character>", 
	      string :: "<" ## ?name ## "-string>", index :: <integer>)
		 => (character :: "<" ## ?name ## "-character>")
	   if (element-range-check(index, size(string)))
	     string-element(string, index) := new-value
	   else
	     element-range-error(string, index)
	   end if
	 end method element-setter;

         define inline sealed method element-setter
             (new-value :: <character>,
              string :: "<" ## ?name ## "-string>", index :: <integer>)
          => (character :: "<" ## ?name ## "-character>")
           string[index] := as("<" ## ?name ## "-character>", new-value);
         end method element-setter;

	 define inline sealed method element-no-bounds-check-setter 
	     (new-value :: "<" ## ?name ## "-character>", 
	      string :: "<" ## ?name ## "-string>", index :: <integer>)
		 => (character :: "<" ## ?name ## "-character>")
	   string-element(string, index) := new-value
	 end method element-no-bounds-check-setter;

         define inline sealed method element-no-bounds-check-setter
             (new-value :: <character>,
              string :: "<" ## ?name ## "-string>", index :: <integer>)
          => (character :: "<" ## ?name ## "-character>")
           string-element(string, index)
             := as("<" ## ?name ## "-character>", new-value);
         end method element-no-bounds-check-setter;

	 define sealed inline method type-for-copy
	     (object :: "<" ## ?name ## "-string>") => (c :: <class>)
	   "<" ## ?name ## "-string>"
	 end method type-for-copy;

         define sealed inline method element-type 
	     (t :: "<" ## ?name ## "-string>") => (type :: <type>)
	   "<" ## ?name ## "-character>"
	 end method;

	 define sealed inline method as
	     (class == "<" ## ?name ## "-string>", string :: "<" ## ?name ## "-string>")
	  => (s :: "<" ## ?name ## "-string>")
	   string
	 end method as;

	 define method as
	     (class == "<" ## ?name ## "-string>", collection :: <collection>)
	  => (s :: "<" ## ?name ## "-string>")
	   let new-string :: "<" ## ?name ## "-string>"
	     = make("<" ## ?name ## "-string>", size: collection.size);
	   replace-subsequence!(new-string, collection);
	   new-string
	 end method as;

	 define inline function ?name ## "-string-current-element"
	     (string :: "<" ## ?name ## "-string>", state :: <integer>)
	   string-element(string, state)
	 end function;

	 define inline function ?name ## "-string-current-element-setter"
	     (new-value :: <character>, string :: "<" ## ?name ## "-string>", 
	      state :: <integer>)
	   string-element(string, state) := as("<" ## ?name ## "-character>", new-value);
	 end function;

	 define sealed inline method forward-iteration-protocol 
	   (sequence :: "<" ## ?name ## "-string>")
	     => (initial-state :: <integer>, limit :: <integer>,
		 next-state :: <function>, finished-state? :: <function>,
		 current-key :: <function>,
		 current-element :: <function>, current-element-setter :: <function>,
		 copy-state :: <function>)
	   values(0,
		  sequence.size,
		  sequence-next-state,
		  sequence-finished-state?,
		  sequence-current-key,
		  ?name ## "-string-current-element",
		  ?name ## "-string-current-element-setter",
		  identity-copy-state)
	 end method forward-iteration-protocol;

	 define sealed inline method backward-iteration-protocol 
	   (sequence :: "<" ## ?name ## "-string>")
	     => (final-state :: <integer>,
		 limit :: <integer>,
		 previous-state :: <function>,
		 finished-state? :: <function>,
		 current-key :: <function>,
		 current-element :: <function>,
		 current-element-setter :: <function>,
		 copy-state :: <function>)
	   values(sequence.size - 1,
		  -1,
		  sequence-previous-state,
		  sequence-finished-state?,
		  sequence-current-key,
		  ?name ## "-string-current-element",
		  ?name ## "-string-current-element-setter",
		  identity-copy-state)
	 end method backward-iteration-protocol;

	 define sealed domain size ("<" ## ?name ## "-string>");
	 define sealed domain make (singleton("<" ## ?name ## "-string>"));
	 define sealed domain initialize ("<" ## ?name ## "-string>");

	 define inline sealed method empty?
	     (string :: "<" ## ?name ## "-string>") => (result :: <boolean>)
	   string.size = 0
	 end method empty?;

	 define sealed method \<
	     (string-1 :: "<" ## ?name ## "-string>", string-2 :: "<" ## ?name ## "-string>")
	  => (well? :: <boolean>)
	   let min-size :: <integer> = min(string-1.size, string-2.size);
	   iterate grovel (index :: <integer> = 0)
	     if (index >= min-size)
	       string-1.size < string-2.size
	     else
	       let character-1 = string-element(string-1, index);
	       let character-2 = string-element(string-2, index);
	       if (character-1 = character-2)
		 grovel(index + 1)
	       else
		 character-1 < character-2
	       end if
	     end if
	   end iterate
	 end method \<;

	 define sealed method \=
	     (string-1 :: "<" ## ?name ## "-string>", string-2 :: "<" ## ?name ## "-string>")
	  => (eq :: <boolean>)
	   unless (string-1.size ~= string-2.size)
	     for (c1 :: "<" ## ?name ## "-character>" in string-1, 
		  c2 :: "<" ## ?name ## "-character>" in string-2,
		  eq = #t then c1 == c2,
		  while: eq)
	     finally 
	       eq
	     end
	   end
	 end;

	 define sealed method case-insensitive-equal
	     (string-1 :: "<" ## ?name ## "-string>", string-2 :: "<" ## ?name ## "-string>")
	  => (eq :: <boolean>)
	   unless (string-1.size ~= string-2.size)
	     for (c1 :: "<" ## ?name ## "-character>" in string-1, 
		  c2 :: "<" ## ?name ## "-character>" in string-2,
		  eq = #t then c1 == c2 | as-lowercase(c1) == as-lowercase(c2),
		  while: eq)
	     finally 
	       eq
	     end
	   end
	 end;

	 define sealed method as-lowercase (string :: "<" ## ?name ## "-string>") 
	  => (new-string :: "<" ## ?name ## "-string>")
	   let new-string :: "<" ## ?name ## "-string>" 
	     = make("<" ## ?name ## "-string>", size: string.size);
	   for (i :: <integer> from 0 below string.size) 
	     string-element(new-string, i)
	       := as-lowercase(string-element(string, i))
	   end for;   
	   new-string
	 end method as-lowercase;

	 define sealed method as-lowercase! (string :: "<" ## ?name ## "-string>") 
 	  => (string :: "<" ## ?name ## "-string>")
	   for (i :: <integer> from 0 below string.size) 
	     string-element(string, i)
	       := as-lowercase(string-element(string, i))
	   end for;   
	   string
	 end method as-lowercase!;

	 define sealed method as-uppercase (string :: "<" ## ?name ## "-string>") 
	  => (new-string :: "<" ## ?name ## "-string>")
	   let new-string :: "<" ## ?name ## "-string>" 
	     = make("<" ## ?name ## "-string>", size: string.size);
	   for (i :: <integer> from 0 below string.size) 
	     string-element(new-string, i)
	       := as-uppercase(string-element(string, i))
	   end for;   
	   new-string
	 end method as-uppercase;

	 define sealed method as-uppercase! (string :: "<" ## ?name ## "-string>") 
	     => (string :: "<" ## ?name ## "-string>")
	   for (i :: <integer> from 0 below string.size) 
	     string-element(string, i)
	       := as-uppercase(string-element(string, i))
	   end for;   
	   string
	 end method as-uppercase!; 
         }
end macro;

define macro string-definer
  { define string ?:name (#key ?fill:expression) }
    => { define shared-string ?name (fill: ?fill);
         define sealed concrete primary class "<" ## ?name ## "-string>" (<string>, <vector>)
	   repeated sealed inline slot string-element :: "<" ## ?name ## "-character>",
	     init-value: ?fill,
	     init-keyword: fill:,
	     size-getter: size,
	     size-init-keyword: size:,
	     size-init-value: 0;
	 end class;

         define constant "$empty-<" ## ?name ## "-string>"
           = system-allocate-repeated-instance
               ("<" ## ?name ## "-string>", "<" ## ?name ## "-character>", unbound(), 0, ?fill);

	 define sealed method empty
	     (class == "<" ## ?name ## "-string>") => (res :: "<" ## ?name ## "-string>")
          "$empty-<" ## ?name ## "-string>"
	 end method; }
end macro;

define constant <string-type>
  = type-union(subclass(<string>), <limited-string-type>);

define method limited-string
    (of :: <type>, size :: false-or(<integer>)) => (type :: <string-type>)
  let concrete-class
    = concrete-limited-string-class(of);
  if (size)
    make(<limited-string-type>,
	 class:          <string>,
	 element-type:   of,
	 concrete-class: concrete-class,
	 size:           size)
  else 
    concrete-class
  end if;
end method;

//
// BYTE-STRING
//

// BOOTED: define ... class <byte-string> ... end;


define shared-string byte (fill: ' ');

define sealed method empty
    (class == <byte-string>) => (res :: <byte-string>)
 ""
end method; 

define inline method system-allocate-repeated-instance
    (class == <byte-string>, type == <byte-character>, fill, 
     repeated-size :: <integer>, repeated-fill :: <byte-character>)
 => (instance :: <byte-string>)
  system-allocate-repeated-byte-instance-terminated
    (<byte-string>, repeated-size, repeated-fill);
end method;

