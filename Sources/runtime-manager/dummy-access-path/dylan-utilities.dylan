module:          access-path-implementation
synopsis:        Special access-path functions that take advantage of low-level
                 knowledge of the dylan implementation.
author:          Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// DYLAN-CURRENT-UNWIND-PROTECT-FRAME
//    Gets the thread-local pointer to the current unwind-protect frame in
//    the dynamic environment.

define method dylan-current-unwind-protect-frame
    (ap :: <access-path>, thread :: <remote-thread>)
       => (uwp-if-found :: false-or(<remote-value>))
  if (thread.nub-descriptor.current-unwind-protect == 0)
    #f
  else
    thread.nub-descriptor.current-unwind-protect
  end if
end method;


///// DYLAN-UNWIND-PROTECT-FRAME-CONTENTS
//    Reads in the contents of an unwind-protect frame, including the
//    pointer to the previous unwind-protect frame.

define method dylan-unwind-protect-frame-contents
    (ap :: <access-path>, up-frame :: <remote-value>)
       => (parent-fp :: <remote-value>,
           previous-unwind-protect :: <remote-value>,
           cleanup-code :: <remote-value>)
  values(read-value(ap, up-frame + 0),
         read-value(ap, up-frame + 1),
         read-value(ap, up-frame + 2));
end method;


///// DYLAN-BIND-EXIT-FRAME-CONTENTS
//    Reads in the contents of a bind-exit frame.

define method dylan-bind-exit-frame-contents
    (ap :: <access-path>, bx-frame :: <remote-value>)
       => (parent-fp :: <remote-value>,
           continuation :: <remote-value>,
           stored-unwind-protect :: <remote-value>,
           mv-vector :: <remote-value>)
  values(read-value(ap, bx-frame + 0),
         read-value(ap, bx-frame + 1),
         read-value(ap, bx-frame + 2),
         read-value(ap, bx-frame + 3));
end method;


///// DYLAN-ARGUMENT-REGISTERS
//    Returns a sequence of <unassigned-remote-register> objects representing
//    the registers that are used to pass arguments under the dylan calling
//    convention. Element n of this sequence (if the element exists) will
//    describe the register holding the (n+1)th argument to the function.

define method dylan-argument-registers
    (ap :: <access-path>) => (registers :: <sequence>)
  #[]
end method;


///// DYLAN-CURRENT-FUNCTION
//    Returns the value of the function register. This cannot determine
//    liveness of the function register. That check should already have been
//    made before calling this method.

define method dylan-current-function
    (ap :: <access-path>, thread :: <remote-thread>)
       => (remote-lambda :: <remote-value>)
  thread.nub-descriptor.ebx
end method;


///// DYLAN-INSTALLED-HANDLERS
//    Returns the value of the current set of handlers.

define method dylan-installed-handlers
    (ap :: <access-path>, thread :: <remote-thread>)
       => (remote-list :: <remote-value>)
  let s = find-symbol(ap, "KPempty_listYinternalVdylan");
  if (s)
    s.remote-symbol-address
  else
    as-remote-value(0)
  end if
end method;


///// DYLAN-RESOLVE-KEYWORD
//    Given the name of an interned keyword symbol, this function attempts
//    to find the address of the keyword.

define method dylan-resolve-keyword
    (ap :: <access-path>, keyword-name :: <string>)
       => (keyword-address :: false-or(<remote-value>))

  let vector-sym = find-symbol(ap, "Poblist");
  let cursor-sym = find-symbol(ap, "PoblistUcursor");
  #f;
end method;
