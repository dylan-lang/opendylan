module:        dm-internals
synopsis:      Modelling the stack trace of a thread.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// FIRST-STACK-FRAME
//    Initializes the stack trace for a thread, and returns the newest frame
//    on the stack, regardless of its type.

define method first-stack-frame
   (application :: <debug-target>, thread :: <remote-thread>)
      => (top-frame :: <application-stack-frame>)
  update-thread-stack-trace(application, thread);
  sort-thread-stack-trace(application, thread);
end method;


///// GET-TOP-CALL-FRAME
//    Similar to FIRST-STACK-FRAME, but is guaranteed to return an instance
//    of <call-frame>, rather than the more general <application-stack-frame>

define method get-top-call-frame
    (application :: <debug-target>, thread :: <remote-thread>)
        => (top-call :: <call-frame>)
  update-thread-stack-trace(application, thread);
  sort-thread-stack-trace(application, thread);
  find-thread(application, thread).call-frame-vector[0];
end method;


///// GET-PREVIOUS-CALL-FRAME
//    Similar to PREVIOUS-STACK-FRAME, but if any frame is returned, it will
//    be a <call-frame>.

define method get-previous-call-frame
    (application :: <debug-target>, f :: <call-frame>)
        => (prev-call :: false-or(<call-frame>))
  f.call-frame-linked-to
end method;


///// PREVIOUS-STACK-FRAME
//    Given a stack frame, try to return the adjacent (older) stack frame

define method previous-stack-frame
    (application :: <debug-target>, f :: <application-stack-frame>)
       => (maybe-frame :: false-or(<application-stack-frame>))
  f.frame-generic-link-older;
end method;


///// NEXT-STACK-FRAME
//    Given a stack frame, try to return the adjacent (newer) stack frame

define method next-stack-frame
    (application :: <debug-target>, f :: <application-stack-frame>)
       => (maybe-frame :: false-or(<application-stack-frame>))
  f.frame-generic-link-newer;
end method;


///// STACK-FRAME-THREAD
//    Returns the thread for a stack frame.

define method stack-frame-thread (f :: <application-stack-frame>)
    => (thread :: <remote-thread>)
  f.frame-associated-thread
end method;


///// CALL-FRAME-DESCRIPTION
//    Returns the <function-frame> object that the access path holds for
//    this call frame.

define method call-frame-description
    (application :: <debug-target>, frame :: <call-frame>)
       => (ap-frame :: <function-frame>)
  frame.ap-frame-description
end method;


///// CALL-FRAME-FRAME-POINTER
//    Returns the frame pointer address of a call frame.

define method call-frame-frame-pointer
    (application :: <debug-target>, f :: <call-frame>)
       => (fp :: <remote-value>)
  f.call-frame-frame-pointer-cache
end method;


///// CALL-FRAME-RETURN-ADDRESS
//    Returns the return address of a call frame.

define method call-frame-return-address 
    (application :: <debug-target>, f :: <call-frame>)
       => (fp :: <remote-value>)
  f.call-frame-return-address-cache
end method;


///// CALL-FRAME-INSTRUCTION-POINTER
//    Returns the program counter (code location) of a call frame.

define method call-frame-instruction-pointer
    (application :: <debug-target>, f :: <call-frame>)
  f.call-frame-instruction-pointer-cache
end method;


///// LOCATE-CALL-FRAME-FUNCTION
//    Tries to find a symbolic entry point for the call frame's code
//    location, and works out the offset in bytes from the start of the
//    function. Also determines whether the frame is a dylan stack
//    frame, and tries to find a model object from the compiler.

define method locate-call-frame-function
    (application :: <debug-target>, f :: <call-frame>) => ()
  unless (f.attempted-to-locate-called-function?)
    let path = application.debug-target-access-path;
    let table = application.debug-target-symbol-table;
    let (entry-point, offset) =
      symbol-table-symbol-relative-address
         (table, f.call-frame-instruction-pointer-cache);
    if (entry-point)
      f.called-function-symbol := entry-point;
      f.called-function-offset := offset;
      f.call-frame-calling-dylan? := 
         (entry-point.remote-symbol-language == $symbol-language-Dylan);
      if (f.call-frame-calling-dylan?)
        // TODO: Get the model object from the compiler.
      end if;
    end if;
    f.attempted-to-locate-called-function? := #t;
  end unless
end method;


///// CALL-FRAME-FUNCTION
//    Returns a description of the function being called in a call frame,
//    if a description can be found.
/*
define method call-frame-function
    (application :: <debug-target>, f :: <call-frame>)
      => (symbol :: false-or(<remote-symbol>),
          model-object :: false-or(<object>))
  locate-call-frame-function(application, f);
  values(f.called-function-symbol,
         f.call-frame-function-model-object-cache)
end method;
*/
define method call-frame-function 
    (application :: <debug-target>, frame :: <call-frame>)
       => (func-sym :: false-or (<remote-symbol>),
           func-obj :: false-or(<remote-value>),
           gf-obj :: false-or(<remote-value>))
  locate-call-frame-function(application, frame);
  let func-sym = frame.called-function-symbol;
  let func-obj = #f;
  let gf-obj = #f;
  if (func-sym & mangled-name-is-iep?(func-sym.remote-symbol-name))
    let func-name = mangle-map-iep-to-method(func-sym.remote-symbol-name);
    let dll = func-sym.remote-symbol-library;
    let sym = 
      symbol-table-find-symbol(application.debug-target-symbol-table,
                               func-name, library: dll);
    if (sym)
      func-obj := sym.remote-symbol-address
    end if;
    if (func-obj & mangled-name-is-method?(func-name))
      let gf-name = mangle-map-method-to-generic(func-name);
      let dll = mangled-name-to-remote-library(application, gf-name);
      let sym = symbol-table-find-symbol
                   (application.debug-target-symbol-table,
                    gf-name, library: dll);
      if (sym)
        gf-obj := sym.remote-symbol-address
      end if
    end if
  end if;
  values(func-sym, func-obj, gf-obj);
end method;


///// DYLAN-CALL-FRAME?
//    Decides whether a function frame is running Dylan code. 
//    TODO: Decide the precise technique for doing this.

define method dylan-call-frame?
    (application :: <debug-target>, f :: <call-frame>)
       => (answer :: <boolean>)
  locate-call-frame-function(application, f);
  f.call-frame-calling-dylan?
end method;


///// CALL-FRAME-CODE-OFFSET
//    Returns the offset in bytes from the start of the function being called
//    in the stack frame, to the frame's actual code location.

define method call-frame-code-offset
    (application :: <debug-target>, f :: <call-frame>)
       => (byte-offset :: <integer>)
  locate-call-frame-function(application, f);
  f.called-function-offset;
end method;



///// CALL-FRAME-ALIGNED-AT-SOURCE-LOCATOR?
//    If there is a source location that corresponds exactly to the
//    code location for this call frame, the source location will be
//    returned. If there is no such source location, #f is returned,
//    and align-thread-to-source-location can be used to attempt
//    alignment.

define method call-frame-aligned-at-source-locator?
    (application :: <debug-target>, call-frame :: <call-frame>)
      => (maybe-locator :: false-or(<source-locator>))
  locate-call-frame-function(application, call-frame);
  let func-sym = call-frame.called-function-symbol;
  if (~func-sym)
    #f
  else
    let slm = function-source-location-map
               (application.debug-target-access-path, func-sym);
    if (~slm)
      #f
    else
      let (exact, ahead, behind)
        = nearest-source-locations(application.debug-target-access-path,
                                   slm,
                                   call-frame-instruction-pointer
                                      (application, call-frame));
      if (exact)
        let (linenumber, address)
          = source-location-description(slm, exact);
        loc(slm.source-filename, slm.base-linenumber + linenumber); // TODO
      else
        #f
      end if
    end if
  end if
end method;


///// CALL-FRAME-NEAREST-SOURCE-LOCATOR
//    Regardless of whether the call frame is positioned exacly on a
//    source location, this function attempts to find the nearest
//    source locator.

define method call-frame-nearest-source-locator
    (application :: <debug-target>, call-frame :: <call-frame>)
      => (maybe-locator :: false-or(<source-locator>))
  locate-call-frame-function(application, call-frame);
  let func-sym = call-frame.called-function-symbol;
  if (~func-sym)
    #f
  else
    let slm = function-source-location-map
               (application.debug-target-access-path, func-sym);
    if (~slm)
      #f
    else
      let (exact, ahead, behind)
        = nearest-source-locations(application.debug-target-access-path,
                                   slm,
                                   call-frame-instruction-pointer
                                      (application, call-frame));
      if (exact)
        let (linenumber, address)
          = source-location-description(slm, exact);
        loc(slm.source-filename, slm.base-linenumber + linenumber); // TODO
      elseif (behind)
        let (linenumber, address)
          = source-location-description(slm, behind);
        loc(slm.source-filename, slm.base-linenumber + linenumber); // TODO
      elseif (ahead)
        let (linenumber, address)
          = source-location-description(slm, ahead);
        loc(slm.source-filename, slm.base-linenumber + linenumber); // TODO
      else
        #f
      end if
    end if
  end if
end method;

