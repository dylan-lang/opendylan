Module: c-ffi
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Abstract class of all C types.

define abstract class <C-value> (<object>) end class;

define subclass-slot stored-pointer-type of <C-value> end;
define subclass-slot size-of             of <C-value> end;
define subclass-slot alignment-of        of <C-value> end;   

// Size-of actually corresponds to C's size-of in the sense that it
// returns the size of the type taking alignment constraints into
// account. Like C, it works in terms of char-sized units.

define generic size-of (value :: <C-value>) 
  => (size :: <integer>);

define generic superclass-for-pointer-type (type)
  => (pointer-type);

define method superclass-for-pointer-type (type :: subclass(<C-value>))
  <C-typed-pointer>
end method;

define abstract class <C-structure> (<C-value>) end class;

// Use "wrapper" terminology?

define macro wrapper-definer
  { define ?mods wrapper ?name ?keys end }
    => { define abstract class ?name (<C-value>) end;

         define method canonical-type (class :: subclass(?name))
           ?name
         end method;

         begin
           let _class = ?name;
           ?keys
         end }
keys:
  { #key ?size, ?alignment }
    => { size-of(_class)      := ?size;
         alignment-of(_class) := ?alignment; }
end macro;

// The fundamental C type wrappers.

define wrapper <C-signed-char>
  size: 1, alignment: 1
end wrapper;

define wrapper <C-unsigned-char> 
  size: 1, alignment: 1
end wrapper;

define constant <C-char> = <C-signed-char>; // Platform dependant!!!

define wrapper <C-signed-short> 
  size: 2, alignment: 2
end wrapper;

define wrapper <C-unsigned-short> 
  size: 2, alignment: 2
end wrapper;

define constant <C-short> = <C-signed-short>; // Platform dependant!!!

define wrapper <C-signed-int> 
  size: 4, alignment: 4
end wrapper;

define wrapper <C-unsigned-int> 
  size: 4, alignment: 4
end wrapper;

define constant <C-int> = <C-signed-int>; // Platform dependant!!!

define wrapper <C-signed-long> 
  size: 4, alignment: 4
end wrapper;

define wrapper <C-unsigned-long> 
  size: 4, alignment: 4
end wrapper;

define constant <C-long> = <C-signed-long>; // Platform dependant!!!

define wrapper <C-float> 
  size: 4, alignment: 4
end wrapper;

define wrapper <C-double> 
  size: 8, alignment: 8
end wrapper;

define wrapper <C-long-double> 
  size: 8, alignment: 8
end wrapper;

define wrapper <C-void> 
  size: 0, alignment: 0
end wrapper;

define method superclass-for-pointer-type (type :: subclass(<C-void>))
  <C-untyped-pointer>
end method;

// Hacky, hacky.

define macro low-level-methods-definer
  { define low-level-methods ?class:expression using ?getter end }
    => { define method low-level-value-at 
             (cls :: subclass(?class), addr)
           ?getter(addr)
         end method;
         define method low-level-value-at-setter
             (val, cls :: subclass(?class), addr)
           ?getter(addr) := val
         end method; }
end macro;

define low-level-methods <C-signed-char>    using signed-byte-at end;
define low-level-methods <C-unsigned-char>  using unsigned-byte-at end;
define low-level-methods <C-signed-short>   using signed-short-at end;
define low-level-methods <C-unsigned-short> using unsigned-short-at end;
define low-level-methods <C-signed-int>     using signed-long-at end;
define low-level-methods <C-unsigned-int>   using unsigned-long-at end;
define low-level-methods <C-signed-long>    using signed-long-at end;
define low-level-methods <C-unsigned-long>  using unsigned-long-at end;
define low-level-methods <C-float>          using float-at end;
define low-level-methods <C-double>         using double-at end;

//// Abstract supertypes?

define method abstract-supertype (c :: subclass(<C-value>))
  <C-value>
end method;

define method copy-wrapper-properties (to, from :: subclass(<C-value>))
  size-of(to)      := size-of(from);
  alignment-of(to) := alignment-of(from);
end method;

// Hacky LW methods

define lw-type <C-signed-char>    = byte: end;
define lw-type <C-unsigned-char>  = unsigned-byte: end;
define lw-type <C-signed-short>   = short: end;
define lw-type <C-unsigned-short> = unsigned-short: end;
define lw-type <C-signed-int>     = long: end;
define lw-type <C-unsigned-int>   = unsigned-long: end;
define lw-type <C-signed-long>    = long: end;
define lw-type <C-unsigned-long>  = unsigned-long: end;
define lw-type <C-float>          = single-float: end;
define lw-type <C-double>         = double-float: end;

define method lw-export (v :: <number>)
  v
end method;

define method lw-import (class :: subclass(<C-value>), v :: <number>)
  v
end method;

// eof
