module:    pentium-win32-rtg
Synopsis:  Support for primitive generation for the Dylan runtime
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// Support for the Windows-specific STDCALL convention
///
/// Calls to Windows API DLLs use the STDCALL convention, (callee pops
/// stack). This involves an unusual naming convention too.

define method int-to-string (num :: <integer>) => (<str :: <byte-string>)
  let (quot, rem) = truncate/(num, 10);
  let this-char = as(<character>, rem + as(<integer>, '0'));
  let top = if (quot > 0) int-to-string(quot) else "" end if;
  let top-size = top.size;
  let res = make(<byte-string>, size: (1 + top-size), fill: this-char);
  replace-subsequence!(res, top, end: top-size);
end method;

define method op--stdcall-c 
    (be :: <pentium-windows-back-end>, name :: <byte-string>,
     #rest args)
  let arg-bytes = int-to-string(args.size * 4);
  let name-ref = ins--constant-ref(be, stdcall-mangle(be, name, arg-bytes));
  apply(op--stdcall-c, be, name-ref, args);
end method;


define method op--stdcall-c 
    (be :: <pentium-windows-back-end>, name-ref :: <constant-reference>,
     #rest args)
  // Push the args in reverse order
  for (arg in reverse(args))
    ins--push(be, arg);
  end for;
  // Do the call
  ins--call-alien(be, name-ref, 0);
  // But don't bother to pop the args
end method;
 