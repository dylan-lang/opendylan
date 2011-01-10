module:    pentium-win32-rtg
Synopsis:  Non-local exit primitives for the Dylan Win32 Pentium runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



/// OP--POP-ANY-SEH-HANDLERS
/// This is responsible for the minimal amount of interaction between the 
/// Dylan NLX implementation, and the Windows SEH mechanism. Just before
/// popping the stack, we look to see if it will make any of the registered
/// SEH handlers stale. If so, then they get popped first. In practice,
/// there will only be a need for this if an NLX is made past a callback.

define sideways method op--pop-any-SEH-handlers
    (be :: <pentium-windows-back-end>, new-stack-ptr :: <register>)
  with-harp (be)
    nreg seh-frame;
    tag ok, loop;

    ins--tag(be, loop);
    ins--get-seh(be, seh-frame);
    ins--bhi(be, ok, seh-frame, new-stack-ptr);
    // If the SEH frame is below the new stack pointer, pop SEH frame
    ins--ld(be, seh-frame, seh-frame, 0); // next frame
    ins--set-seh(be, seh-frame);
    ins--bra(be, loop);

    ins--tag(be, ok);
  end with-harp;
end method;
