Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Hack up generic addition and subtraction
define method \+ (a, b) sealed+(a, b) end;
define method \- (a, b) sealed-(a, b) end;

// Hack up generic array reference
define method aref (array, #rest indices)
  apply(sealed-aref, array, indices)
end method;

define method aref-setter (val, array, #rest indices)
  apply(sealed-aref-setter, val, array, indices)
end method;

define constant <address> = <integer>;
define constant $null-address = 0;
define constant pointer-at = unsigned-long-at;
define constant pointer-at-setter = unsigned-long-at-setter;

define method character-at (addr :: <address>)
  as(<character>, signed-byte-at(addr))
end method;

define method character-at-setter (c :: <character>, addr :: <address>)
  signed-byte-at(addr) := as(<integer>, c)
end method;

define method string-at (addr :: <address>)
  let end-addr =
    for (walk from addr, 
         until: signed-byte-at(walk) == 0)
    finally walk
    end;
  let string = make(<string>, size: end-addr - addr);
  for (walk from addr below end-addr,
       i from 0)
    string[i] := character-at(walk);
  end;
  string
end method;

define method string-at-setter (string :: <string>, addr :: <address>)
  for (char in string, walk from addr)
    character-at(walk) := char;
  end;
  string
end method;

define constant C-malloc = malloc;

define macro subclass-slot-definer
  { define subclass-slot ?getter:name = ?default:expression 
      of ?class:name 
    end }
    => { define constant $subclass-slot-table-for- ## ?getter = make(<table>);

         define method ?getter (class :: subclass(?class))
           let val = 
             element($subclass-slot-table-for- ## ?getter, class,
                     default: not-found());
           if (found?(val)) 
             val
           else
             ?getter(class) := ?default
           end
         end method;

         define method ?getter ## \-setter (value, class :: subclass(?class))
           element($subclass-slot-table-for- ## ?getter, class) := value
         end method;
       }
  { define subclass-slot ?getter:name of ?class:name end }
    => { define subclass-slot ?getter = #f of ?class end }
end macro;

// Hackery

define macro lw-type-definer
  { define lw-type ?class:expression = ?type end }
    => { define method lw-type (c :: subclass(?class))
           ?type
         end method; }
end macro;

// eof
