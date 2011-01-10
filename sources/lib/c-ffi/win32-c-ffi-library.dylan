Module:    dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library c-ffi
  use functional-dylan;
  export c-ffi;
  export win32-core;
end;


define module c-ffi-interface
  create
    destroy,
    C-char-at,
    C-char-at-setter,
    C-signed-char-at,
    C-signed-char-at-setter,
    C-unsigned-char-at,
    C-unsigned-char-at-setter,
    C-short-at,
    C-short-at-setter,
    C-signed-short-at,
    C-signed-short-at-setter,
    C-unsigned-short-at,
    C-unsigned-short-at-setter,
    C-long-at,
    C-long-at-setter,
    C-signed-long-at,
    C-signed-long-at-setter,
    C-unsigned-long-at,
    C-unsigned-long-at-setter,
    C-int-at,
    C-int-at-setter,
    C-signed-int-at,
    C-signed-int-at-setter,
    C-unsigned-int-at,
    C-unsigned-int-at-setter,

    C-float-at,
    C-float-at-setter,
    C-double-at,
    C-double-at-setter,

    <C-unsigned-char>,
    <C-unsafe-unsigned-char>,
    <C-both-unsigned-char>,
    <C-unsigned-char*>,
    <C-unsafe-unsigned-char*>,
    <C-both-unsigned-char*>,

    <C-signed-char>,
    <C-unsafe-signed-char>,
    <C-both-signed-char>,
    <C-signed-char*>,
    <C-unsafe-signed-char*>,
    <C-both-signed-char*>,

    <C-char>,
    <C-unsafe-char>,
    <C-both-char>,
    <C-char*>,
    <C-unsafe-char*>,
    <C-both-char*>,

    <C-unsigned-long>,
    <C-unsafe-unsigned-long>,
    <C-both-unsigned-long>,
    <C-unsigned-long*>,
    <C-unsafe-unsigned-long*>,
    <C-both-unsigned-long*>,

    <C-signed-long>,
    <C-unsafe-signed-long>,
    <C-both-signed-long>,
    <C-signed-long*>,
    <C-unsafe-signed-long*>,
    <C-both-signed-long*>,
    
    <C-long>,
    <C-unsafe-long>,
    <C-both-long>,
    <C-long*>,
    <C-unsafe-long*>,
    <C-both-long*>,

    <C-unsigned-short>,
    <C-unsafe-unsigned-short>,
    <C-both-unsigned-short>,
    <C-unsigned-short*>,
    <C-unsafe-unsigned-short*>,
    <C-both-unsigned-short*>,

    <C-signed-short>,
    <C-unsafe-signed-short>,
    <C-both-signed-short>,
    <C-signed-short*>,
    <C-unsafe-signed-short*>,
    <C-both-signed-short*>,

    <C-short>,
    <C-unsafe-short>,
    <C-both-short>,
    <C-short*>,
    <C-unsafe-short*>,
    <C-both-short*>,

    <C-unsigned-int>,
    <C-unsafe-unsigned-int>,
    <C-both-unsigned-int>,
    <C-unsigned-int*>,
    <C-unsafe-unsigned-int*>,
    <C-both-unsigned-int*>,

    <C-signed-int>,
    <C-unsafe-signed-int>,
    <C-both-signed-int>,
    <C-signed-int*>,
    <C-unsafe-signed-int*>,
    <C-both-signed-int*>,

    <C-int>,
    <C-unsafe-int>,
    <C-both-int>,
    <C-int*>,
    <C-unsafe-int*>,
    <C-both-int*>,

    <C-raw-long>,
    <C-raw-short>,
    <C-raw-long*>,
    <C-raw-short*>,

    <C-character>,
    <C-character*>,
    <C-dylan-object>,
    <C-dylan-object*>,

    C-pointer-at,
    C-pointer-at-setter,
    pointer-address,
    null-pointer,
    null-pointer?,
    <C-string>, C-string-constant,
    <C-string*>,
    <C-unicode-string>, C-unicode-string-constant,
    <C-unicode-string*>,
    <C-boolean>,
    <C-dylan-object>,
    register-c-dylan-object,
    unregister-c-dylan-object,
    export-c-dylan-object,
    import-c-dylan-object,

/* [gts, 11/97, wait until harp backend ready]
   once handled by conversion, the following line should be commented out */
    \with-stack-structure,
    \with-c-string,
    pointer-cast,
    c-type-cast,

    copy-into!,
    copy-bytes!,
    equal-memory?,
    clear-memory!,

    $trace-ffi-calls,
    log-entry,
    log-exit,

    <ffi-integer>
    ;
end module;

// These don't belong here but need to be shared between libraries
// which use the std win32 header interfaces and DUIM (which doesn't).
define module win32-core
  create
    <HANDLE>,
    <HBITMAP>,
    <HDC>,
    <HWND>,
    <HMENU>,
    <HACCEL>;
  create
    null-handle?,
    null-handle;
  create
    <HHOOK>,
    <HEVENT>,
    <HGDIOBJ>,
    <HBRUSH>,
    <HCOLORSPACE>,
    <HGLRC>,
    <HDESK>,
    <HENHMETAFILE>,
    <HFONT>,
    <HICON>,
    <HMETAFILE>,
    <HINSTANCE>,
    <HMODULE>,
    <HPALETTE>,
    <HPEN>,
    <HRGN>,
    <HRSRC>,
    <HSTR>,
    <HTASK>,
    <HWINSTA>,
    <HKL>,
    <HCURSOR>;
end;


define module c-ffi-kludges
  use machine-words, export: all;
  create
    check-import-range,
    check-export-range,
    export-to-machine-word,
    export-c-string,
    export-c-boolean,
    import-c-boolean;
end;

define module c-ffi
  use c-ffi-interface, export: all;
  use dylan-c-ffi, export: all;
  use dylan-extensions, export: { <abstract-integer>, <big-integer> };
  use c-ffi-kludges, export: all;
end;

define module c-ffi-implementation
  use c-ffi;
  use win32-core;
  use functional-dylan;
  use dylan-extensions;
  use dylan-primitives;
  use dylan-c-ffi;
  use machine-words;
  use simple-format;
end;
