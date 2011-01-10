module:     devel-dbg-ui
synopsis:   Functionality for allowing EVAL to work with C functions.
author:     Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <C-INTERACTION-RETURNED>
//    A class of breakpoint used to capture the return of an
//    interactive call to C code.

define class <c-interaction-returned> (<breakpoint>)

  constant slot used-thread :: <remote-thread>,
    required-init-keyword: thread:;

  constant slot used-frame :: <remote-value>,
    required-init-keyword: frame:;

  constant slot calling-frame :: false-or(<remote-value>),
    init-value: #f,
    init-keyword: calling-frame:;

  constant slot stored-context :: <object>,
    required-init-keyword: context:;

  slot c-function-result :: <remote-value>;

end class;


///// HANDLE-DEBUG-POINT-EVENT (Debugger Manager Method)
//    Handles the return of an interactively-invoked C function.

define method handle-debug-point-event
    (application :: <debug-target>, bp :: <c-interaction-returned>,
     thread :: <remote-thread>)
        => (interested? :: <boolean>)
  if (thread == bp.used-thread)
    let top-frame-now =
      initialize-stack-trace(application.debug-target-access-path, thread);
    let calling-frame-now =
      previous-frame(application.debug-target-access-path, top-frame-now);
    let top-frame-pointer =
      frame-pointer(application.debug-target-access-path, top-frame-now);
    let calling-frame-pointer =
      if (calling-frame-now)
        frame-pointer(application.debug-target-access-path, calling-frame-now)
      else
        #f
      end if;
    if ((top-frame-pointer = bp.used-frame) |
        ((bp.calling-frame) & (calling-frame-pointer) &
         (bp.calling-frame = calling-frame-pointer)))
      bp.c-function-result := 
        remote-call-result(application.debug-target-access-path, thread);
      remote-restore-context(application.debug-target-access-path, thread,
                             bp.stored-context);
      deregister-debug-point(application, bp);
      next-method();
    else
      #f
    end if
  else
    #f
  end if;
end method;


///// INVOKE-C
//    Basically a reflection of the DM's INVOKE-DYLAN.

define method invoke-c
    (application :: <debug-target>, thread :: <remote-thread>,
     c-function :: <remote-value>, return-callback :: <function>,
     #rest argument-list)
  => (succeeded? :: <boolean>)
      let top-frame = 
      initialize-stack-trace(application.debug-target-access-path, thread);
  let top-frame-pointer =
    frame-pointer(application.debug-target-access-path, top-frame);
  let calling-frame-now =
    previous-frame(application.debug-target-access-path, top-frame);
  let calling-frame-pointer =
    if (calling-frame-now)
      frame-pointer(application.debug-target-access-path, calling-frame-now)
    else
      #f
    end if;
  let (return-address, context) =
      call-debugger-function(
            application, 
            apply, 
            remote-call,
            application.debug-target-access-path,
            thread,
            c-function,
            argument-list);
  let debug-point = 
    make(<c-interaction-returned>,
         address: return-address,
         callback: return-callback,
         context: context,
         thread: thread,
         frame: top-frame-pointer,
         calling-frame: calling-frame-pointer);
  register-debug-point(application, debug-point);
  #t;
end method;
