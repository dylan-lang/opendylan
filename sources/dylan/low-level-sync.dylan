module:    threads-internal
Synopsis:  The implementation of the low-level synchronization functions
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



define inline function synchronize-side-effects () => ()
  primitive-synchronize-side-effects();
end function;

define inline function sequence-point () => ()
  primitive-sequence-point();
end function;

