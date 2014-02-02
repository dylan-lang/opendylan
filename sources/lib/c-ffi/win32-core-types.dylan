module:    c-ffi-implementation
Author:    Gail Zacharias
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define open C-subtype <HANDLE> (<C-void*>) end;

// convenience functions

define method as (class :: subclass(<HANDLE>), n :: <integer>)
 => value :: <HANDLE>;
  make(class, address: n)
end;

define method as (class :: subclass(<HANDLE>), n :: <machine-word>)
 => value :: <HANDLE>;
  make(class, address: n)
end;

define method as (class == <integer>, h :: <HANDLE>)
 => value :: <integer>;
  as(<integer>, pointer-address(h))
end;

define inline method as (class == <machine-word>, h :: <HANDLE>)
 => value :: <machine-word>;
  pointer-address(h)
end;

define sealed method null-handle (class :: subclass(<HANDLE>))
 => value :: <HANDLE>;
  null-pointer(class)
end;

define inline sealed method null-handle? (object :: <HANDLE>)
 => value :: <boolean>;
  null-pointer?(object)
end;

// Handles are represented as pointers, but they never have space
// allocated by `make', so it would not be meaningful to call `destroy' on one.
define method destroy (h :: <HANDLE>, #key) => ();
  error("destroy not valid on handle %=", h);
end;

// Seal for efficiency.
define sealed domain make (subclass(<HANDLE>));
define sealed domain initialize (<HANDLE>);


define C-subtype <HDC> (<HANDLE>) end;
define C-subtype <HWND> (<HANDLE>) end;
define C-subtype <HMENU> (<HANDLE>) end;
define C-subtype <HACCEL> (<HANDLE>) end;
define C-subtype <HHOOK> (<HANDLE>) end;
define C-subtype <HEVENT> (<HANDLE>) end;
define C-subtype <HCOLORSPACE> (<HANDLE>) end;
define C-subtype <HGLRC> (<HANDLE>) end;
define C-subtype <HDESK> (<HANDLE>) end;
define C-subtype <HENHMETAFILE> (<HANDLE>) end;
define C-subtype <HICON> (<HANDLE>) end;
define C-subtype <HMETAFILE> (<HANDLE>) end;
define C-subtype <HINSTANCE> (<HANDLE>) end;
define inline constant <HMODULE> = <HINSTANCE>;
define C-subtype <HRSRC> (<HANDLE>) end;
define C-subtype <HSTR> (<HANDLE>) end;
define C-subtype <HTASK> (<HANDLE>) end;
define C-subtype <HWINSTA> (<HANDLE>) end;
define C-subtype <HKL> (<HANDLE>) end;
define inline constant <HCURSOR> = <HICON>;

define C-subtype <HGDIOBJ> (<HANDLE>) end;
define C-subtype <HBITMAP> (<HGDIOBJ>) end;
define C-subtype <HBRUSH> (<HGDIOBJ>) end;
define C-subtype <HFONT> (<HGDIOBJ>) end;
define C-subtype <HPALETTE> (<HGDIOBJ>) end;
define C-subtype <HPEN> (<HGDIOBJ>) end;
define C-subtype <HRGN> (<HGDIOBJ>) end;
