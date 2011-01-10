module:    native-rtg
Synopsis:  OS specific Primitives for the Dylan runtime generator
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


//// OS-specific primitives


// First, the constants


// Now, the code


define method as-direct-ref 
    (be :: <harp-back-end>, ref :: <indirect-constant-reference>)
    => (nref :: <address-constant-reference>)
  ins--constant-ref(be, ref.cr-refers-to);
end method;



define generic-runtime-primitive start-timer;

define generic-runtime-primitive stop-timer;



define used-by-client generic-runtime-primitive exit-application;


define generic-runtime-primitive run-application;

