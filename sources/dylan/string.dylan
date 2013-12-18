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
  { define shared-string ?:name (#key ?fill:expression, ?class-name:name) }
    => { define inline sealed method element
             (string :: "<" ## ?class-name ## "-string>", index :: <integer>,
              #key default = unsupplied())
          => (character)
           if (element-range-check(index, size(string)))
             string-element(string, index)
           else
             if (unsupplied?(default))
               element-range-error(string, index)
             else
               default
             end if
           end if
         end method element;

         define inline sealed method element-no-bounds-check
             (string :: "<" ## ?class-name ## "-string>",
              index :: <integer>, #key default)
          => (character :: "<" ## ?name ## "-character>")
           string-element(string, index)
         end method element-no-bounds-check;

         define inline sealed method element-setter
             (new-value :: "<" ## ?name ## "-character>",
              string :: "<" ## ?class-name ## "-string>", index :: <integer>)
          => (character :: "<" ## ?name ## "-character>")
           if (element-range-check(index, size(string)))
             string-element(string, index) := new-value
           else
             element-range-error(string, index)
           end if
         end method element-setter;

         define inline sealed method element-setter
             (new-value :: <character>,
              string :: "<" ## ?class-name ## "-string>", index :: <integer>)
          => (character :: "<" ## ?name ## "-character>")
           string[index] := as("<" ## ?name ## "-character>", new-value);
         end method element-setter;

         define inline sealed method element-no-bounds-check-setter
             (new-value :: "<" ## ?name ## "-character>",
              string :: "<" ## ?class-name ## "-string>", index :: <integer>)
          => (character :: "<" ## ?name ## "-character>")
           string-element(string, index) := new-value
         end method element-no-bounds-check-setter;

         define inline sealed method element-no-bounds-check-setter
             (new-value :: <character>,
              string :: "<" ## ?class-name ## "-string>", index :: <integer>)
          => (character :: "<" ## ?name ## "-character>")
           string-element(string, index)
             := as("<" ## ?name ## "-character>", new-value);
         end method element-no-bounds-check-setter;

         define sealed inline method element-type
             (t :: "<" ## ?class-name ## "-string>") => (type :: <type>)
           "<" ## ?name ## "-character>"
         end method;
         
         define sealed inline method as
             (class == "<" ## ?class-name ## "-string>",
              string :: "<" ## ?class-name ## "-string>")
          => (s :: "<" ## ?class-name ## "-string>")
           string
         end method as;

         define method as
             (class == "<" ## ?class-name ## "-string>", coll :: <collection>)
          => (s :: "<" ## ?class-name ## "-string>")
           let new-string :: "<" ## ?class-name ## "-string>"
             = make("<" ## ?class-name ## "-string>", size: coll.size);
           replace-subsequence!(new-string, coll);
           new-string
         end method as;

         define inline function ?class-name ## "-string-current-element"
             (string :: "<" ## ?class-name ## "-string>", state :: <integer>)
           string-element(string, state)
         end function;

         define inline function ?class-name ## "-string-current-element-setter"
             (new-value :: <character>, string :: "<" ## ?class-name ## "-string>",
              state :: <integer>)
           string-element(string, state) := as("<" ## ?name ## "-character>", new-value);
         end function;

         define sealed inline method forward-iteration-protocol
             (sequence :: "<" ## ?class-name ## "-string>")
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
                  ?class-name ## "-string-current-element",
                  ?class-name ## "-string-current-element-setter",
                  identity-copy-state)
         end method forward-iteration-protocol;

         define sealed inline method backward-iteration-protocol
             (sequence :: "<" ## ?class-name ## "-string>")
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
                  ?class-name ## "-string-current-element",
                  ?class-name ## "-string-current-element-setter",
                  identity-copy-state)
         end method backward-iteration-protocol;

         define sealed domain size ("<" ## ?class-name ## "-string>");
         define sealed domain make (singleton("<" ## ?class-name ## "-string>"));
         define sealed domain initialize ("<" ## ?class-name ## "-string>");

         define inline sealed method empty?
             (string :: "<" ## ?class-name ## "-string>") => (result :: <boolean>)
           string.size = 0
         end method empty?;

         define sealed method \<
             (string-1 :: "<" ## ?class-name ## "-string>", 
              string-2 :: "<" ## ?class-name ## "-string>")
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
             (string-1 :: "<" ## ?class-name ## "-string>", 
              string-2 :: "<" ## ?class-name ## "-string>")
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
             (string-1 :: "<" ## ?class-name ## "-string>",
              string-2 :: "<" ## ?class-name ## "-string>")
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

         define sealed method as-lowercase
             (string :: "<" ## ?class-name ## "-string>")
          => (new-string :: "<" ## ?class-name ## "-string>")
           let new-string :: "<" ## ?class-name ## "-string>"
             = make("<" ## ?class-name ## "-string>", size: string.size);
           for (i :: <integer> from 0 below string.size)
             string-element(new-string, i)
               := as-lowercase(string-element(string, i))
           end for;
           new-string
         end method as-lowercase;

         define sealed method as-lowercase!
              (string :: "<" ## ?class-name ## "-string>")
           => (string :: "<" ## ?class-name ## "-string>")
           for (i :: <integer> from 0 below string.size)
             string-element(string, i)
               := as-lowercase(string-element(string, i))
           end for;
           string
         end method as-lowercase!;

         define sealed method as-uppercase
             (string :: "<" ## ?class-name ## "-string>")
          => (new-string :: "<" ## ?class-name ## "-string>")
           let new-string :: "<" ## ?class-name ## "-string>"
             = make("<" ## ?class-name ## "-string>", size: string.size);
           for (i :: <integer> from 0 below string.size)
             string-element(new-string, i)
               := as-uppercase(string-element(string, i))
           end for;
           new-string
         end method as-uppercase;

         define sealed method as-uppercase!
             (string :: "<" ## ?class-name ## "-string>")
          => (string :: "<" ## ?class-name ## "-string>")
           for (i :: <integer> from 0 below string.size)
             string-element(string, i)
               := as-uppercase(string-element(string, i))
           end for;
           string
         end method as-uppercase!;
         }
end macro;

//
// LIMITED AND NOT LIMITED STRINGS
//

define constant <string-type>
  = type-union(subclass(<string>), <limited-string-type>);

// Defines class and methods for a <limited-X-class>.
define macro limited-string-definer
  { define limited-string ?:name (#key ?fill:expression) }
    => { define shared-string ?name (fill: ?fill, class-name: "limited-" ## ?name);
    
         define sealed concrete primary class "<limited-" ## ?name ## "-string>"
             (<limited-fillable-collection>, <string>, <vector>)
           repeated sealed inline slot string-element :: "<" ## ?name ## "-character>",
             init-value: ?fill,
             init-keyword: fill:,
             size-getter: size,
             size-init-keyword: size:,
             size-init-value: 0;
         end class;

         define method make
             (class == "<limited-" ## ?name ## "-string>",
              #key fill :: "<" ## ?name ## "-character>" = ?fill, size :: <integer> = 0,
                   element-type-fill: default-fill = ?fill)
          => (res :: "<limited-" ## ?name ## "-string>")
           let instance = system-allocate-repeated-instance
             ("<limited-" ## ?name ## "-string>", "<" ## ?name ## "-character>", unbound(), size, fill);
           instance.element-type-fill := default-fill;
           instance
         end method;
       
         define sealed inline method type-for-copy
             (object :: "<limited-" ## ?name ## "-string>") => (c :: <type>)
           limited-string(element-type(object), element-type-fill(object), #f)
         end method type-for-copy;

         define sealed inline method concrete-limited-string-class
             (of == "<" ## ?name ## "-character>", default-fill)
          => (type :: singleton("<limited-" ## ?name ## "-string>"), fully-specified?)
           values("<limited-" ## ?name ## "-string>", default-fill = ?fill)
         end method;

         define sealed inline method limited-string-default-fill
             (of == "<" ## ?name ## "-character>") => (fill :: "<" ## ?name ## "-character>")
           ?fill
         end method;
       }
end macro;

// Defines class and methods for an <X-string>. The <byte-string> class was
// defined from boot, so use string-without-class alone for it.
define macro string-definer
  { define string ?:name (#key ?fill:expression) }
  => { define string-without-class ?name (fill: ?fill);

       define sealed concrete primary class "<" ## ?name ## "-string>" (<string>, <vector>)
         repeated sealed inline slot string-element :: "<" ## ?name ## "-character>",
           init-value: ?fill,
           init-keyword: fill:,
           size-getter: size,
           size-init-keyword: size:,
           size-init-value: 0;
       end class;
       
       define method make
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

       define constant "$empty-<" ## ?name ## "-string>"
         = system-allocate-repeated-instance
             ("<" ## ?name ## "-string>", "<" ## ?name ## "-character>", unbound(), 0, ?fill);

       define sealed method empty
           (class == "<" ## ?name ## "-string>") => (res :: "<" ## ?name ## "-string>")
         "$empty-<" ## ?name ## "-string>"
       end method;
     }
end macro;

// Defines methods for an <X-string> class.
define macro string-without-class-definer
  { define string-without-class ?:name (#key ?fill:expression) }
    => { define shared-string ?name (fill: ?fill, class-name: ?name);
      
         define sealed inline method type-for-copy
             (object :: "<" ## ?name ## "-string>") => (c :: <class>)
           "<" ## ?name ## "-string>"
         end method type-for-copy;
      
         define sealed inline method element-type-fill
             (t :: "<" ## ?name ## "-string>") => (fill :: "<" ## ?name ## "-character>")
           ?fill
         end method;
  }
end macro;

//
// LIMITED STRINGS
//

define limited-string byte (fill: ' ');

define method limited-string
    (of :: <type>, default-fill :: <character>, size :: false-or(<integer>))
 => (type :: <string-type>)
  let (concrete-class, fully-specified?)
    = concrete-limited-string-class(of, default-fill);
  if (size | ~fully-specified?)
    make(<limited-string-type>,
         class:          <string>,
         element-type:   of,
         default-fill:   default-fill,
         concrete-class: concrete-class,
         size:           size)
  else
    concrete-class
  end if;
end method;

define sealed inline method concrete-limited-string-class
    (of == <character>, default-fill)
 => (type :: subclass(<string>), fully-specified?)
  values(<limited-byte-string>, default-fill = limited-string-default-fill(of))
end method;

define sealed inline method limited-string-default-fill
    (of :: <type>) => (fill == ' ')
  ' '
end method;

//
// BYTE-STRING
//

// BOOTED: define ... class <byte-string> ... end;

define string-without-class byte (fill: ' ', class-name: byte);

define method make
    (class == <byte-string>, #key fill :: <byte-character> = ' ', size :: <integer> = 0)
 => (res :: <byte-string>)
  if (size = 0)
    empty(class)
  else
    system-allocate-repeated-instance
      (<byte-string>, <byte-character>, unbound(), size, fill);
  end if
end method;

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

