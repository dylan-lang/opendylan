module:    pentium-win32-rtg
Synopsis:  Debugger Primitives for the Dylan Win32 Pentium runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define win-fun runtime-external win-OutputDebugString = "OutputDebugStringA",
  data:  "4";


define sideways method op--output-debug-string
    (be :: <pentium-windows-back-end>, string :: <register>) => ()
  op--stdcall-c(be, win-OutputDebugString, string);
end method;
