module:       dm-internals
synopsis:     Looking at the stack and the dynamic environment
author:       Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


/////
///// <APPLICATION-STACK-FRAME>
/////

define abstract sealed class <application-stack-frame>
                                  (<object>)

       slot
        application-frame-address :: <remote-value>,
        required-init-keyword: address:;

       slot stack-frame-thread :: <application-thread>;

       slot
        next-application-frame :: false-or (<application-stack-frame>),
        init-value: #f;

       slot
        previous-application-frame :: false-or (<application-stack-frame>),
        init-value: #f;

       slot
        maybe-more-application-frames? :: <boolean>,
        init-value: #t;

end class;


///// <DYLAN-STACK-FRAME-MIXIN>

define abstract class <dylan-stack-frame-mixin> (<object>)
end class;


///// <CALL-FRAME>

define /* abstract */ class <call-frame> (<application-stack-frame>)

       slot ap-frame-description :: <function-frame>,
            required-init-keyword: access-path-frame:;

       slot frame-function :: false-or (<remote-symbol>),
            init-value: #f,
            init-keyword: called-symbol:;

       slot frame-function-compiler-model :: <object>,
            init-value: #f,
            init-keyword: called-model:;

       slot next-call-frame :: false-or (<call-frame>),
            init-value: #f;

       slot previous-call-frame :: false-or (<call-frame>),
            init-value: #f;

       slot maybe-more-calls? :: <boolean>,
            init-value: #t;

end class;


///// <DYLAN-CALL-FRAME>

define class <dylan-call-frame> (<call-frame>,
                                 <dylan-stack-frame-mixin>)
end class;


///// <IMPLEMENTATION-STACK-FRAME>

define abstract class <implementation-stack-frame>
                        (<application-stack-frame>,
                         <dylan-stack-frame-mixin>)
end class;


///// <BIND-EXIT-FRAME>
//    We may not be able to model these at all. They aren't linked, and
//    we might not be able to find them anyway!

define class <bind-exit-frame> (<implementation-stack-frame>)
end class;


///// <UNWIND-PROTECT-FRAME>

define class <unwind-protect-frame> (<implementation-stack-frame>)

       slot 
        next-unwind-protect-frame :: false-or(<unwind-protect-frame>),
        init-value: #f;

       slot 
        previous-unwind-protect-frame :: false-or(<unwind-protect-frame>),
        init-value: #f;

       slot
        maybe-more-unwind-protects? :: <boolean>,
        init-value: #t;

       slot uwp-cleanup :: <remote-value>,
            required-init-keyword: cleanup-code:;

       slot uwp-frame :: <remote-value>,
            required-init-keyword: uwp-frame:;

       slot uwp-parent-call-frame :: <remote-value>,
            required-init-keyword: uwp-parent-call:;

       slot uwp-link :: false-or(<remote-value>),
            required-init-keyword: uwp-link:;

end class;

    
///// <DYNAMIC-BIND-FRAME>

define class <dynamic-bind-frame> (<unwind-protect-frame>)
end class;

/*

IMPLEMENTATION: Modelling stack frames.

Ultimately, in a stack backtrace, the two important frame types are
<call-frame> and <unwind-protect-frame>. Each of these frame types has
its own dynamic chain. The lazy stack model for a thread has three
parts to it:

1. The chain of call frames (called call-frame-linkage)

2. The chain of unwind-protect frames (call uwp-frame-linkage)

3. The "intermingled" sequence of stack frames of both kinds,
   reflecting their ordering in the runtime (called stack-model).
   This is what is meant by "the stack model".

All three sequences are maintained lazily. Linking a new frame to
stack-model requires examining (and possibly extending) the current
state of the two separate sequences, and selecting the frame to
link.

Note that finding "previous" frames is the only operation that
potentially requires extension of these sequences, since that is the
direction in which we walk the stack. Apart from the topmost frame,
it is impossible for a frame to exist in the model without its "next"
frame already existing.

Once a frame has been linked to the model, its type is recorded, and
a slot is updated to reflect this change. If the added frame is a
call frame, then the slot "oldest-modelled-call-frame" is updated
to point to it. If the added frame is an unwind-protect, then the
slot "oldest-modelled-uwp-frame" is updated. This is because it is
quite possible for sequences (1) and (2) to contain frames that have
not yet been linked onto the model. For example, the oldest call
frame on sequence (1) is not necessarily the oldest call frame on
sequence (3). (The process of selecting a frame to link causes this
to be true).

*/

///// UPDATE-OLDEST-FRAMES
//    I can't think of a decent name for this function!
//    Given a thread descriptor, and a stack frame that has just
//    been linked to the stack model for that thread, this function
//    makes sure that the oldest-modelled-call-frame and
//    oldest-modelled-uwp-frame pointers are kept current.

define method update-oldest-frames
    (application :: <debug-target>, thread :: <application-thread>,
     frame :: <application-stack-frame>)
end method;

define method update-oldest-frames
    (application :: <debug-target>, thread :: <application-thread>,
     frame :: <unwind-protect-frame>)
  thread.oldest-modelled-uwp-frame := frame;
end method;

define method update-oldest-frames
    (application :: <debug-target>, thread :: <application-thread>,
     frame :: <call-frame>)
  thread.oldest-modelled-call-frame := frame;
end method;


///// GET-PREVIOUS-UNWIND-PROTECT-FRAME
//    Follows the UWP frame linkage to get the previous frame

define method get-previous-unwind-protect-frame
    (application :: <debug-target>, uwp :: <unwind-protect-frame>)
       => (maybe-uwp :: false-or (<unwind-protect-frame>))

  let thread = uwp.stack-frame-thread;
  let path = application.debug-target-access-path;

  if (uwp.previous-unwind-protect-frame)
    uwp.previous-unwind-protect-frame
  elseif (~uwp.maybe-more-unwind-protects?)
    #f
  elseif (~uwp.uwp-link)
    uwp.maybe-more-unwind-protects? := #f;
    #f;
  else
    // Lazily construct a new link in the chain of UWPs
    let new-uwp = uwp.uwp-link;
    let (new-uwp-parent, new-uwp-link, new-uwp-cleanup) = 
      dylan-unwind-protect-frame-contents(path, new-uwp);
    let frame = make (<unwind-protect-frame>,
                      uwp-frame: new-uwp,
                      address: new-uwp,
                      uwp-parent-call: new-uwp-parent,
                      uwp-link: new-uwp-link,
                      cleanup-code: new-uwp-cleanup);
    frame.next-unwind-protect-frame := uwp;
    frame.stack-frame-thread := thread;
    uwp.previous-unwind-protect-frame := frame;
    frame;
  end if
end method;


///// GET-NEXT-UNWIND-PROTECT-FRAME
//    Returns the unwind-protect frame that linked to this one.

define method get-next-unwind-protect-frame
    (application :: <debug-target>, uwp :: <unwind-protect-frame>)
       => (maybe-uwp :: false-or (<unwind-protect-frame>))
  uwp.next-unwind-protect-frame
end method;


///// GET-PREVIOUS-CALL-FRAME
//    Returns the call frame that the given call frame links to

define method get-previous-call-frame
    (application :: <debug-target>, call :: <call-frame>)
       => (maybe-call :: false-or (<call-frame>))

  let thread = call.stack-frame-thread;
  let frame = #f;

  if (call.previous-call-frame)
    call.previous-call-frame
  elseif (~call.maybe-more-calls?)
    #f;
  else
    let ap-frame = call.ap-frame-description;
    let ap-prev-frame = 
      previous-frame (application.debug-target-access-path,
                      ap-frame);
    if (ap-prev-frame)
      let (func-sym, func-model) =
        try-to-locate-frame-function(application, ap-prev-frame);

      if (func-sym)
        if (func-sym.remote-symbol-language == $symbol-language-Dylan)
          frame := make (<dylan-call-frame>,
                            called-symbol: func-sym,
                            called-model: func-model,
                            address:
                              frame-pointer
                                (application.debug-target-access-path,
                                 ap-prev-frame),
                            access-path-frame: ap-prev-frame);
        else
          frame := make (<call-frame>,
                            called-symbol: func-sym,
                            called-model: func-model,
                            address:
                              frame-pointer
                                (application.debug-target-access-path,
                                 ap-prev-frame),
                            access-path-frame: ap-prev-frame);
        end if
      else
        frame := make (<call-frame>,
                          called-symbol: #f,
                          called-model: #f,
                          address:
                            frame-pointer
                              (application.debug-target-access-path,
                               ap-prev-frame),
                          access-path-frame: ap-prev-frame);
      end if;
      call.previous-call-frame := frame;
      frame.next-call-frame := call;
      frame.stack-frame-thread := thread;
      frame;                     
    else
      call.maybe-more-calls? := #f;
      #f
    end if
  end if
end method;


///// GET-NEXT-CALL-FRAME
//    Returns the call frame that linked to this one.

define method get-next-call-frame 
    (application :: <debug-target>, frame :: <call-frame>) 
       => (maybe-call :: false-or (<call-frame>))
  frame.next-call-frame;
end method;


///// TRY-TO-LOCATE-FRAME-FUNCTION
//    For a call frame, does a symbol lookup to try and find the name of the
//    function being called in this frame.

define method try-to-locate-frame-function 
    (application :: <debug-target>, function-frame :: <function-frame>) 
       => (function-symbol :: false-or(<remote-symbol>),
           function-model :: false-or(<compiler-model>))  // TODO: Name!!
  let ip = frame-instruction-address
             (application.debug-target-access-path, function-frame);
  let (closest, offset)
    = symbol-table-symbol-relative-address
         (application.debug-target-symbol-table, ip);
  let model = #f;
  values(closest, model);
end method;


///// CALL-FRAME-FUNCTION
//    Returns the <remote-symbol> that represents the function being called
//    in this call frame, given that it exists. Otherwise, returns #f

define method call-frame-function 
    (application :: <debug-target>, frame :: <call-frame>)
       => (func-sym :: false-or (<remote-symbol>),
           func-obj :: false-or(<remote-value>),
           gf-obj :: false-or(<remote-value>))
  values(frame.frame-function, #f, #f)
end method;

// For a dylan call frame, we should be able to do more...

define method call-frame-function 
    (application :: <debug-target>, frame :: <dylan-call-frame>)
       => (func-sym :: false-or (<remote-symbol>),
           func-obj :: false-or(<remote-value>),
           gf-obj :: false-or(<remote-value>))
  let func-sym = frame.frame-function;
  let func-obj = #f;
  let gf-obj = #f;
  if (func-sym & mangled-name-is-iep?(func-sym.remote-symbol-name))
    let func-name = mangle-map-iep-to-method(func-sym.remote-symbol-name);
    let dll = mangled-name-to-remote-library(application, func-name);
    let sym = find-symbol(application.debug-target-access-path,
                          func-name, library: dll);
    if (sym)
      func-obj := sym.remote-symbol-address
    end if;
    if (func-obj & mangled-name-is-method?(func-name))
      let gf-name = mangle-map-method-to-generic(func-name);
      let dll = mangled-name-to-remote-library(application, gf-name);
      let sym = find-symbol(application.debug-target-access-path,
                            gf-name, library: dll);
      if (sym)
        gf-obj := sym.remote-symbol-address
      end if
    end if
  end if;
  values(func-sym, func-obj, gf-obj);
end method;


///// CALL-FRAME-DESCRIPTION
//    Returns the <function-frame> object that the access path holds for
//    this call frame.

define method call-frame-description
    (application :: <debug-target>, frame :: <call-frame>)
       => (ap-frame :: <function-frame>)
  frame.ap-frame-description
end method;


///// CALL-FRAME-RETURN-ADDRESS

define method call-frame-return-address
  (application :: <debug-target>, frame :: <call-frame>)
     => (addr :: <remote-value>)
  frame-return-address
    (application.debug-target-access-path,
     frame.ap-frame-description);
end method;


///// CALL-FRAME-FRAME-POINTER

define method call-frame-frame-pointer
  (application :: <debug-target>, frame :: <call-frame>)
    => (fp :: <remote-value>)
  frame-pointer
    (application.debug-target-access-path,
     frame.ap-frame-description);
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
  let func-sym = call-frame.frame-function;
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
  let func-sym = call-frame.frame-function;
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


///// CALL-FRAME-INSTRUCTION-POINTER

define method call-frame-instruction-pointer
  (application :: <debug-target>, frame :: <call-frame>)
    => (ip :: <remote-value>)
  frame-instruction-address
    (application.debug-target-access-path,
     frame.ap-frame-description);
end method;


///// GET-TOP-CALL-FRAME
//    Obtains the call frame at the top of the call stack. This function
//    will definitely not return #f - there _must_ be a stack of some
//    description.

define method get-top-call-frame
    (application :: <debug-target>, rthread :: <remote-thread>) 
       => (top-call :: <call-frame>)
  let thread = find-thread (application, rthread);
  let top-frame = #f;

  // It's conceivable that the topmost call frame has been
  // modelled for this thread already. If so, just return
  // it. Otherwise, call the access path to initialize a
  // stack trace, build a model of the topmost call frame,
  // and return that.

  if (thread.call-frame-linkage)
    thread.call-frame-linkage;
  else
    let ap-top-frame =
      initialize-stack-trace (application.debug-target-access-path,
                              rthread);
    let (func-sym, func-model)
      = try-to-locate-frame-function(application, ap-top-frame);

    if (func-sym)
      if (func-sym.remote-symbol-language == $symbol-language-Dylan)
        top-frame := 
          make (<dylan-call-frame>,
                called-symbol: func-sym,
                called-model: func-model,
                address:
                   frame-pointer
                     (application.debug-target-access-path,
                      ap-top-frame),
                access-path-frame: ap-top-frame);
      else
        top-frame :=
          make (<call-frame>,
                called-symbol: func-sym,
                called-model: func-model,
                address:
                  frame-pointer
                    (application.debug-target-access-path,
                     ap-top-frame),
                access-path-frame: ap-top-frame);
      end if
    else
      top-frame :=
        make(<call-frame>,
             called-symbol: #f,
             called-mode: #f,
             address:
               frame-pointer
                 (application.debug-target-access-path,
                  ap-top-frame),
             access-path-frame: ap-top-frame);
    end if;
    top-frame.stack-frame-thread := thread;
    top-frame.stack-frame-thread := thread;   // Link city!
    top-frame;
  end if
end method;


///// GET-TOP-UNWIND-PROTECT-FRAME
//    Obtains the unwind protect frame at the top of the dynamic environment.
//    This function might return #f.

//    Argh! 'Ang on a minute...
//    %current-unwind-protect-frame is a thread-local variable. This
//    function pays no heed to this. In fact, we just don't know how
//    thread-local variables are going to work!

define method get-top-unwind-protect-frame
    (application :: <debug-target>, rthread :: <remote-thread>)
       => (maybe-uwp :: false-or (<unwind-protect-frame>))

  let path = application.debug-target-access-path;
  let thread = find-thread(application, rthread);

  // As with get-top-call-frame, we may already have this
  // modelled. We may also have established that there are
  // no unwind-protect frames. Deal with these cases first,
  // else go ahead and plunge into the (frozen) dynamic
  // environment.

  if (thread.uwp-frame-linkage)
    thread.uwp-frame-linkage
  elseif (thread.definitely-no-uwp?)
    #f;
  else
    let top-uwp =
      dylan-current-unwind-protect-frame(path, rthread);
    if (~top-uwp)
      thread.definitely-no-uwp? := #t;
      #f
    else
      let (top-uwp-parent, top-uwp-link, top-uwp-cleanup) = 
        dylan-unwind-protect-frame-contents(path, top-uwp);
      let top-uwp-frame =
        make (<unwind-protect-frame>,
              address: top-uwp,
              uwp-frame: top-uwp,
              uwp-parent-call: top-uwp-parent,
              uwp-link: top-uwp-link,
              cleanup-code: top-uwp-cleanup);
      top-uwp-frame.stack-frame-thread := thread;
      top-uwp-frame
    end if
  end if
end method;


///// NEWEST-STACK-FRAME
//    Given two stack frames of any type, returns the newest of them.
//    Note - this relies on \< being provided on instances of
//    <remote-value>.

define method newest-stack-frame
    (f1 :: <application-stack-frame>, f2 :: <application-stack-frame>)
       => (app-frame :: <application-stack-frame>)
  if (application-frame-address (f1) < application-frame-address (f2))
    f1
  else
    f2
  end if;
end method;


///// SCAN-TO-NEXT-UNWIND-PROTECT
//    Given a call frame, climb upwards to find an unwind protect frame. Return
//    #f if there isn't one!

define method scan-to-next-unwind-protect
    (frame :: <call-frame>)
       => (maybe-uwp :: false-or(<unwind-protect-frame>))
  let thread = frame.stack-frame-thread;
  if (thread.oldest-modelled-uwp-frame)
    thread.oldest-modelled-uwp-frame
  else
    thread.uwp-frame-linkage
  end if
end method;


///// SCAN-TO-NEXT-CALL
//    The opposite way around to scan-to-next-unwind-protect

define method scan-to-next-call
    (frame :: <unwind-protect-frame>)
       => (maybe-call :: false-or(<call-frame>))
  let thread = frame.stack-frame-thread;
  if (thread.oldest-modelled-call-frame)
    thread.oldest-modelled-call-frame
  else
    thread.call-frame-linkage
  end if
end method;


///// NEXT-STACK-FRAME
//    Given a stack frame of any type, returns the stack frame that is more
//    recent. Since models of new frames are only generated while traveling in
//    the other direction, the "next frame" (assuming there is one) must
//    already be modelled, so we can just painlessly return it.

define method next-stack-frame 
  (application :: <debug-target>, frame :: <application-stack-frame>)
     => (maybe-frame :: false-or (<application-stack-frame>))
  frame.next-application-frame;
end method;


///// PREVIOUS-STACK-FRAME
//    Given a stack frame, returns the previous frame. This may involve lazily
//    generating new frames. This function behaves differently depending on
//    whether the frame "in hand" is a call frame or an unwind-protect frame.

define method previous-stack-frame 
  (application :: <debug-target>, frame :: <call-frame>)
     => (maybe-frame :: false-or (<application-stack-frame>))
  let thread = frame.stack-frame-thread;

  if (frame.previous-application-frame)
    frame.previous-application-frame
  elseif (~frame.maybe-more-application-frames?)
    #f
  else
    let newest-uwp = scan-to-next-unwind-protect (frame);
    if (~newest-uwp)
      let prev-frame = get-previous-call-frame (application, frame);
      if (~prev-frame)
        frame.maybe-more-application-frames? := #f;
        #f;
      else
        prev-frame.next-application-frame := frame;
        frame.previous-application-frame := prev-frame;
        update-oldest-frames (application, thread, prev-frame);
        prev-frame;
      end if
    else
      let prev-call = get-previous-call-frame (application, frame);
      let prev-uwp = 
        get-previous-unwind-protect-frame (application, newest-uwp);
      if (prev-call & prev-uwp)
        let prev-frame = newest-stack-frame (prev-call, prev-uwp);
        prev-frame.next-application-frame := frame;
        frame.previous-application-frame := prev-frame;
        update-oldest-frames (application, thread, prev-frame);
        prev-frame;
      elseif (prev-call)
        let prev-frame = newest-stack-frame (prev-call, newest-uwp);
        prev-frame.next-application-frame := frame;
        frame.previous-application-frame := prev-frame;
        update-oldest-frames (application, thread, prev-frame);
        prev-frame;
      elseif (prev-uwp)
        prev-uwp.next-application-frame := frame;
        frame.previous-application-frame := prev-uwp;
        update-oldest-frames (application, thread, prev-uwp);
        prev-uwp;
      else
        frame.maybe-more-application-frames? := #f;
        #f;
      end if
    end if
  end if
end method;

define method previous-stack-frame 
  (application :: <debug-target>, frame :: <unwind-protect-frame>)
     => (maybe-frame :: false-or (<application-stack-frame>))
 
  let thread = frame.stack-frame-thread;

  if (frame.previous-application-frame)
    frame.previous-application-frame
  elseif (~frame.maybe-more-application-frames?)
    #f
  else
    let newest-call = scan-to-next-call (frame);
    if (~newest-call)
      let prev-frame = 
        get-previous-unwind-protect-frame (application, frame);
      if (~prev-frame)
        frame.maybe-more-application-frames? := #f;
        #f;
      else
        prev-frame.next-application-frame := frame;
        frame.previous-application-frame := prev-frame;
        prev-frame;
      end if
    else
      let prev-call = get-previous-call-frame (application, newest-call);
      let prev-uwp = 
        get-previous-unwind-protect-frame (application, frame);
      if (prev-call & prev-uwp)
        let prev-frame = newest-stack-frame (prev-call, prev-uwp);
        prev-frame.next-application-frame := frame;
        frame.previous-application-frame := prev-frame;
        update-oldest-frames (application, thread, prev-frame);
        prev-frame;
      elseif (prev-call)
        prev-call.next-application-frame := frame;
        frame.previous-application-frame := prev-call;
        update-oldest-frames (application, thread, prev-call);
        prev-call;
      elseif (prev-uwp)
        let prev-frame = newest-stack-frame (newest-call, prev-uwp);
        prev-frame.next-application-frame := frame;
        frame.previous-application-frame := prev-frame;
        update-oldest-frames (application, thread, prev-frame);
        prev-frame;
      else
        frame.maybe-more-application-frames? := #f;
        #f;
      end if
    end if
  end if
end method;

// A method to catch all cases

define method previous-stack-frame 
  (application :: <debug-target>, frame :: <implementation-stack-frame>)
     => (nothing :: false-or (<application-stack-frame>))
  #f
end method;


///// FIRST-STACK-FRAME
//    Returns whatever frame is at the top of the stack. This can be called
//    whether or not we have already began modelling the stack during this
//    debugger-transaction. The first test is whether the top frame is
//    already modelled.

define method first-stack-frame 
  (application :: <debug-target>, rthread :: <remote-thread>)
     => (top-frame :: <application-stack-frame>)

  let thread = find-thread (application, rthread);

  if (thread.stack-model)
    thread.stack-model
  else
    // We are kicking off our stack model. Obtain the first
    // frame in each of the call chain and the UWP chain, and
    // make the newest of them our top frame.

    thread.call-frame-linkage := 
      get-top-call-frame (application, rthread);
    thread.uwp-frame-linkage :=
      get-top-unwind-protect-frame (application, rthread);
    if (thread.uwp-frame-linkage)
      let top-frame = newest-stack-frame (thread.call-frame-linkage,
                                          thread.uwp-frame-linkage);
      update-oldest-frames (application, thread, top-frame);
      top-frame;
    else
      thread.oldest-modelled-call-frame := thread.call-frame-linkage;
      thread.call-frame-linkage;
    end if
  end if
end method;

