Module:    internal
Synopsis:  Invocation of any thread-specific initializations in the internal module
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND



// Any initializations for threads other than the main thread should 
// happen here


// Guard variable that may be tested to see if the bootstrap is over
//
define variable *dylan-library-initialized?* = #f;

define inline  function internal-initialize-thread ()
  initialize-default-hash-state();
end function;


// They think it's all over. It is now!
//
*dylan-library-initialized?* := #t;
