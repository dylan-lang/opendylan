module:      dm-internals
synopsis:    Descriptions of stack frames in the application.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// <APPLICATION-STACK-FRAME>
//    The superclass of any stack frame on any thread in the runtime.

define abstract class <application-stack-frame> (<object>)

  // The generic frame pointer is used to compare two stack frames of
  // any type, in order to determine which is the more recent.

  constant slot frame-generic-frame-pointer :: <remote-value>,
    required-init-keyword: generic-fp:;

  // The thread, of whose stack trace this frame is a part.

  constant slot frame-associated-thread :: <remote-thread>,
    required-init-keyword: thread:;

  // The "generic links" of stack frames ensure a total ordering by
  // age, regardless of type. This describes a chain where call frames
  // and unwind-protect frames (for example) can be intermixed.
  // These links are generated lazily.

  slot frame-generic-link-newer :: false-or(<application-stack-frame>),
    init-value: #f,
    init-keyword: newer:;

  slot frame-generic-link-older :: false-or(<application-stack-frame>),
    init-value: #f,
    init-keyword: older:;

end class;


///// <DYLAN-STACK-FRAME-MIXIN>
//    Mixes into any stack frame class that has something to do with DYLAN.

define abstract class <dylan-stack-frame-mixin> (<object>)
end class;


///// <IMPLEMENTATION-STACK-FRAME>
//    The superclass of any stack frame that is specific to the dylan
//    implementation that the DM knows about.

define abstract class <implementation-stack-frame> 
     (<application-stack-frame>, <dylan-stack-frame-mixin>)
end class;


///// <ACCESS-PATH-STACK-FRAME>
//    The superclass of any stack frame that has been constructed from a
//    more primitive descriptor obtained via the access path.

define abstract class <access-path-stack-frame> (<application-stack-frame>)

  slot ap-frame-description :: <function-frame>,
    required-init-keyword: ap-frame:;

end class;


///// <CALL-FRAME>
//    The class of stack frames that correspond to function calls.

define class <call-frame> (<access-path-stack-frame>)

  slot attempted-to-locate-called-function? :: <boolean>,
    init-value: #f;

  slot called-function-symbol :: false-or(<remote-symbol>),
    init-keyword: function-symbol:,
    init-value: #f;

  slot called-function-offset :: <integer> = 0;

  slot call-frame-calling-dylan? :: <boolean> = #f;
/*
  slot call-frame-linked-from :: false-or(<call-frame>),
    init-value: #f,
    init-keyword: linked-from:;
*/
   slot call-frame-linked-to :: false-or(<call-frame>),
    init-value: #f,
    init-keyword: linked-to:;

  constant slot call-frame-instruction-pointer-cache :: <remote-value>,
    required-init-keyword: ip:;

  constant slot call-frame-frame-pointer-cache :: <remote-value>,
    required-init-keyword: fp:;

  constant slot call-frame-return-address-cache :: <remote-value>,
    required-init-keyword: ret:;

end class;


///// <DYLAN-CALL-FRAME>
//    The class of call frames that correspond to dylan function calls.

define class <dylan-call-frame>
    (<call-frame>, <dylan-stack-frame-mixin>)
end class;


///// BUILD-ACCESS-PATH-FRAME-VECTOR
//    Returns a vector (ordered newest to oldest) of <function-frame> objects
//    representing the stack trace of a <remote-thread>.

define method build-access-path-frame-vector
    (application :: <debug-target>, thread :: <remote-thread>)
       => (frames :: <vector>)
  let path = application.debug-target-access-path;
  let count = number-of-frames-on-stack(path, thread);
  let frames = make(<vector>, size: count);
  let i = 0;
  let frame = initialize-stack-trace(path, thread);
  while (frame)
    frames[i] := frame;
    frame := previous-frame(path, frame);
    i := i + 1;
  end while;
  frames
end method;


///// MERGE-NEW-CALL-FRAME-VECTOR
//    Given a new access-path level stack trace, and the previous snapshot
//    of call frames at DM-level, create a new DM-level trace of call
//    frames, re-using as many of the existing frames as possible.

define method merge-new-call-frame-vector
    (application :: <debug-target>, thread :: <remote-thread>,
     existing-vector :: <vector>, new-access-path-vector :: <vector>)
      => (new-call-frame-vector :: <vector>)

  let path = application.debug-target-access-path;
  let table = application.debug-target-symbol-table;

  // MAKE-OR-RECYCLE-CALL-FRAME
  
  local method make-or-recycle-call-frame
          (call-frame :: <call-frame>, ap-frame :: <function-frame>)
             => (frame-to-use :: <call-frame>, fresh? :: <boolean>)

    // We consider two stack frames to be equivalent if both their
    // frame pointers and their instruction pointers are identical.

    let dm-fp = call-frame-frame-pointer(application, call-frame);
    let dm-ip = call-frame-instruction-pointer(application, call-frame);
    let ap-fp = frame-pointer(path, ap-frame);
    let ap-ip = frame-instruction-address(path, ap-frame);
    if ((dm-fp = ap-fp) & (dm-ip = ap-ip))
      call-frame.ap-frame-description := ap-frame;
      call-frame.frame-generic-link-newer := #f;
      call-frame.frame-generic-link-older := #f;
      values(call-frame, #f)
    else
      // TODO: Decide between <call-frame> and <dylan-call-frame>.
      //       Is there ANY fast way to do this without symbol lookup??
      //       (Ask Tony).
      values(make(<call-frame>, thread: thread,
                  // This line looks bloody strange...
                  generic-fp: ap-fp, ap-frame: ap-frame, fp: ap-fp, ip: ap-ip,
                  ret: frame-return-address(path, ap-frame)),
             #t)
    end if;
  end method;

  // No matter how much (or little) frame re-use we can do, we know for
  // sure that the new vector of call frames will be the same size as
  // the access-path's list. Further, there will be a one-to-one relationship
  // between each <call-frame> in the new (merged) vector, and each
  // <function-frame> in the access-path's vector.

  let new-call-frame-vector 
    = make(<vector>, size: size(new-access-path-vector));

  // Vectors are ordered newest to oldest!

  let oldest-ap-frame = size(new-access-path-vector) - 1;
  let oldest-dm-frame = size(existing-vector) - 1;
  let ap-frame-i = oldest-ap-frame;
  let dm-frame-i = oldest-dm-frame;
  let can-recycle? = (dm-frame-i >= 0);

  // Go through each frame in the access-path's vector of <function-frame>s.
  // For each one, either re-use a <call-frame> from the existing stack
  // trace, or build a new one. Note that, as soon as we build the first
  // freshly allocated frame, it is assumed that no more recycling is possible.

  while (ap-frame-i >= 0)
    let this-ap-frame = new-access-path-vector[ap-frame-i];
    if (can-recycle?)
      let (dm-frame-to-use, new?) = 
        make-or-recycle-call-frame(existing-vector[dm-frame-i],
                                   this-ap-frame);
      new-call-frame-vector[ap-frame-i] := dm-frame-to-use;
      dm-frame-i := dm-frame-i - 1;
      if (new? | (dm-frame-i < 0))
        can-recycle? := #f
      end if;
    else
      // TODO: Again, decide between <call-frame> and <dylan-call-frame>,
      //       but how?
      new-call-frame-vector[ap-frame-i] :=
         make(<call-frame>, thread: thread,
              generic-fp: frame-pointer(path, this-ap-frame),
              ap-frame: this-ap-frame,
              fp: frame-pointer(path, this-ap-frame),
              ip: frame-instruction-address(path, this-ap-frame),
              ret: frame-return-address(path, this-ap-frame))
    end if;
    ap-frame-i := ap-frame-i - 1;
  end while;

  // Update the linkage.
  // Go through each frame in the blended stack trace, and make sure they
  // chain to each other properly.
  // (Note: Two-way traversal is not currently needed. Some code is
  // commented out here to squelch compiler warnings).

//new-call-frame-vector[0].call-frame-linked-from := #f;
  unless (oldest-ap-frame == 0)
    new-call-frame-vector[0].call-frame-linked-to :=
      new-call-frame-vector[1];
//  new-call-frame-vector[oldest-ap-frame].call-frame-linked-from :=
//    new-call-frame-vector[oldest-ap-frame - 1];
  end unless;
  new-call-frame-vector[oldest-ap-frame].call-frame-linked-to := #f;

  for (i from 1 below oldest-ap-frame)
//  new-call-frame-vector[i].call-frame-linked-from :=
//     new-call-frame-vector[i - 1];
    new-call-frame-vector[i].call-frame-linked-to :=
       new-call-frame-vector[i + 1];
  end for;

  // And return the result.
  new-call-frame-vector;
end method;


///// UPDATE-THREAD-STACK-TRACE
//    When the application stops, this function ensures that the stack trace
//    is correct for the given thread.

define method update-thread-stack-trace
    (application :: <debug-target>, thread :: <remote-thread>) => ()
  let dm-thread = find-thread(application, thread);
  unless (dm-thread.thread-stack-trace-valid?)
    let dynamic-env = 
       build-dynamic-environment
         (application, 
          thread,
          thread-dynamic-environment(application, thread));
    let ap-calls =
       build-access-path-frame-vector
         (application, thread);
    let dm-calls =
       merge-new-call-frame-vector
         (application, thread, dm-thread.call-frame-vector, ap-calls);
    dm-thread.call-frame-vector := dm-calls;
    dm-thread.dynamic-environment-vector := dynamic-env;
    dm-thread.thread-stack-trace-valid? := #t;
    dm-thread.thread-stack-trace-ordered? := #f;
  end unless;
  values();
end method;


///// SORT-THREAD-STACK-TRACE
//    Go through the vectors of frames, and create the complete sorted
//    chain, by linking frames of different types together.
//    Also, for convenience, returns the newest frame that exists.

define method sort-thread-stack-trace
    (application :: <debug-target>, thread :: <remote-thread>)
        => (newest-frame :: <application-stack-frame>)
  let dm-thread = find-thread(application, thread);
  let path = application.debug-target-access-path;

  local method newer-of
          (f1 :: <application-stack-frame>, f2 :: <application-stack-frame>)
             => (newer :: <application-stack-frame>)
    let fp1 = f1.frame-generic-frame-pointer;
    let fp2 = f2.frame-generic-frame-pointer;
    if (older-stack-frame?(path, fp1, fp2))
      f2
    else
      f1
    end if
  end method;

  unless (dm-thread.thread-stack-trace-ordered?)
    let sorted-frames = #f;

    if (dm-thread.dynamic-environment-vector = #[])

      // In the simple case where there are no unwind protect frames,
      // the sorted set of frames is identical to the sorted sequence of
      // call frames.
      // Note: There must be at least one call frame in a stack trace.

      sorted-frames := dm-thread.call-frame-vector;

    else

      // The final vector of frames must be as large as the combined
      // size of the call frame and unwind frame vectors.

      sorted-frames := 
        make(<vector>, 
             size: size(dm-thread.dynamic-environment-vector) +
                   size(dm-thread.call-frame-vector));

      // Point to the newest
      let call-i = 0;
      let unwind-i = 0;
      let call-last = size(dm-thread.call-frame-vector) - 1;
      let unwind-last = size(dm-thread.dynamic-environment-vector) - 1;

      // Select stack frames in the correct order...

      for (sorted-i from 0 to (call-last + unwind-last + 1))
        let call-frame =
          element(dm-thread.call-frame-vector, call-i, default: #f);
        let unwind-frame =
          element(dm-thread.dynamic-environment-vector, unwind-i, default: #f);
        if (call-frame == #f)
          sorted-frames[sorted-i] := unwind-frame;
          unwind-i := unwind-i + 1;
        elseif (unwind-frame == #f)
          sorted-frames[sorted-i] := call-frame;
          call-i := call-i + 1;
        else
          let selected-frame = newer-of(call-frame, unwind-frame);
          if (selected-frame == call-frame)
            call-i := call-i + 1
          else
            unwind-i := unwind-i + 1
          end if;
          sorted-frames[sorted-i] := selected-frame;
        end if
      end for;
    end if;

    // 'sorted-frames' is now a vector of frames ordered newest to oldest,
    // but the generic links are still not yet in place.
    // However, it's now very easy to go through the vector and insert those
    // links.

    let sorted-last = size(sorted-frames) - 1;
    sorted-frames[0].frame-generic-link-newer := #f;
    unless (sorted-last == 0)
      sorted-frames[0].frame-generic-link-older := sorted-frames[1];
      sorted-frames[sorted-last].frame-generic-link-newer :=
            sorted-frames[sorted-last - 1];
    end unless;
    sorted-frames[sorted-last].frame-generic-link-older := #f;
    for (i from 1 below sorted-last)
      sorted-frames[i].frame-generic-link-newer := sorted-frames[i - 1];
      sorted-frames[i].frame-generic-link-older := sorted-frames[i + 1];
    end for;

    // And flag that our stack trace is now sorted.
    dm-thread.thread-stack-trace-ordered? := #t;
    dm-thread.thread-generic-top-frame := sorted-frames[0];
  end unless;
  dm-thread.thread-generic-top-frame;
end method;
