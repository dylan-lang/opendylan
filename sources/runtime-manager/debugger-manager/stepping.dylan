module:      dm-internals
synopsis:    The DM's implementation of source-level stepping in Dylan code.
author:      Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND


///// ALIGN-THREAD-TO-SOURCE-LOCATION
//    Modifies the state of a thread so that, upon continuation, it will
//    advance to the nearest (in terms of control flow, not instruction
//    pointer) recorded code location. The caller can additionally require
//    that the destination be an interactive location.
//    This function does not accept a <call-frame> argument - it assumes
//    that the thread must perform its alignment from wherever it happens
//    to be at a moment in time.

define method align-thread-to-source-location
    (application :: <debug-target>, thread :: <remote-thread>,
     #key interactive? = #f)
  => (success? :: <boolean>)

  debugger-message("align-thread-to-source-location %=", thread);
  let success? = #f;
  let call-frame = get-top-call-frame(application, thread);
  let obtained-context? = #f;
  let context-sets = make(<stretchy-vector>, size: 0);
  let path = application.debug-target-access-path;
  let context-count = 0;
  let context-limit = 3;
  let dm-thread = find-thread(application, thread);

  while (call-frame & (context-count < context-limit))
    let (fp-context, parent-fp-context, return-address, bp-addresses)
      = call-frame-recorded-locations(application, call-frame,
                                      interactive?: interactive?);
    if (bp-addresses.size > 0)
      obtained-context? := #t;
      context-count := context-count + 1;
      add!(context-sets,
           pair(call-frame.ap-frame-description, bp-addresses));
    end if;
    call-frame := get-previous-call-frame(application, call-frame)
  end while;

  if (obtained-context?)
    block ()
      for (frame-address-list-pair in context-sets)
        apply-thread-stepping-control
          (path, 
           thread, 
           tail(frame-address-list-pair), 
           $step-operation-step-over,
           stack-frame: head(frame-address-list-pair));
      end for;
      success? := #t;
      dm-thread.stepping-mode := $thread-aligning-to-recorded-location;
    exception (<debug-point-error>)
      success? := #f
    end block
  end if;

  success?
end method;


///// INSTRUCT-THREAD-TO-STEP-OVER
//    Modifies the state of a thread so that, upon continuation, it will
//    perform a "step over" operation, returning control to the debugger
//    when the destination of the step is reached.

define method instruct-thread-to-step-over
    (application :: <debug-target>, thread :: <remote-thread>,
     #key call-frame = #f)
  => (success? :: <boolean>)

  let success? = #f;
  let path = application.debug-target-access-path;
  let dm-thread = find-thread(application, thread);

  // If the call-frame is not specified as context for the step operation,
  // then assume that we want to step within the topmost call frame on the
  // thread's stack.

  unless(call-frame)
    call-frame := get-top-call-frame(application, thread)
  end unless;

  // Find the parent of the call frame, if there is one. If there is
  // a calling frame, we will additionally set up a "step out", since
  // it's quite possible that "step over" will result in a return.
  // If there is no calling frame, we cannot do this.

  let calling-frame = get-previous-call-frame(application, call-frame);

  block (exit)
    // Acquire the contextual frame pointers, and the breakpoint addresses.
    let (fp-context, parent-fp-context, return-address, bp-addresses)
      = call-frame-recorded-locations(application, call-frame);
    let ap-frame = call-frame.ap-frame-description;
    apply-thread-stepping-control(path, 
                                  thread, 
                                  bp-addresses,
                                  $step-operation-step-over,
                                  stack-frame: ap-frame);
    if (calling-frame)
      let ap-frame = calling-frame.ap-frame-description;
      apply-thread-stepping-control(path, 
                                    thread, 
                                    vector(return-address),
                                    $step-operation-step-out,
                                    stack-frame: ap-frame);
    end if;
    success? := #t;
    dm-thread.stepping-mode := $thread-stepping-over;
  exception (<debug-point-error>)
    success? := #f
  end block;
  success?
end method;


///// INSTRUCT-THREAD-TO-STEP-OUT
//    Modifies the state of a thread so that, upon continuation, it will
//    perform a "step out" operation, returning control to the debugger
//    when the destination of the step is reached.

define method instruct-thread-to-step-out
    (application :: <debug-target>, thread :: <remote-thread>,
     #key call-frame = #f)
  => (success? :: <boolean>)

  let success? = #f;
  let path = application.debug-target-access-path;
  let dm-thread = find-thread(application, thread);

  // If the call-frame is not specified as context for the step operation,
  // then assume that we want to step within the topmost call frame on the
  // thread's stack.

  unless(call-frame)
    call-frame := get-top-call-frame(application, thread)
  end unless;

  let calling-frame = get-previous-call-frame(application, call-frame);

  if (calling-frame)
    // Our strategy is to make the step destination be a recorded location
    // if possible, rather than just blindly stepping to the return address
    // of the supplied frame. If there are recorded locations in the
    // calling frame, then we will step to one of those instead.
    block (exit)
      // Acquire the contextual frame pointers, and the breakpoint addresses.
      let (fp-context, parent-fp-context, return-address, bp-addresses)
        = call-frame-recorded-locations(application, calling-frame);
      if (bp-addresses.size > 0)
        let ap-frame = calling-frame.ap-frame-description;
        apply-thread-stepping-control(path, 
                                      thread, 
                                      bp-addresses,
                                      $step-operation-step-out,
                                      stack-frame: ap-frame);
        success? := #t;
        dm-thread.stepping-mode := $thread-stepping-out;
      else
        let (fp-context, parent-fp-context, return-address, bp-addresses)
          = call-frame-recorded-locations(application, call-frame);
        let ap-frame = call-frame.ap-frame-description;
        apply-thread-stepping-control(path,
                                      thread,
                                      vector(return-address),
                                      $step-operation-step-out,
                                      stack-frame: ap-frame);
        success? := #t;
        dm-thread.stepping-mode := $thread-stepping-out;
      end if
    exception (<debug-point-error>)
      success? := #f
    end block;
  end if;
  success?
end method;


///// INSTRUCT-THREAD-TO-STEP-INTO
//    Modifies the state of a thread so that, upon continuation, it will
//    perform a "step into" operation, returning control to the debugger
//    when the destination of the step is reached.
//    By far the most complex of the three stepping operations, this
//    implementation allows the client to pre-calculate the addresses
//    that might be reached by the step. The reason for this is that
//    only the front-end of the compiler maintains call-graph information:
//    there is no access to it in back-end debug info. Without
//    call graph information, it is almost impossible to make step-into
//    work reliably.

define method instruct-thread-to-step-into
    (application :: <debug-target>, thread :: <remote-thread>,
     #key call-frame = #f, precomputed-addresses = #f)
  => (success? :: <boolean>)
  
  let success? = #f;
  let path = application.debug-target-access-path;
  let dm-thread = find-thread(application, thread);

  local method method-breakpoint-address (m :: <remote-value>)
                  => (addr :: <remote-value>)
          let (sig, addr, keys) = remote-method-inspect(application, m);
          addr
        end method;
 
  local method function-object-breakpoint-addresses (f :: <remote-value>)
                  => (addrs :: false-or(<sequence>))
          let classification = classify-dylan-object(application, f);
          select(classification)

            $generic-function-type =>
               let (signature, meths) =
                 remote-generic-function-inspect(application, f);
               remove-duplicates(map(method-breakpoint-address, meths),
                                 test: \=);

            $method-type =>
               vector(method-breakpoint-address(f));

            otherwise => #f;

          end select;
        end method;

  // If the call-frame is not specified as context for the step operation,
  // then assume that we want to step within the topmost call frame on the
  // thread's stack.

  unless(call-frame)
    call-frame := get-top-call-frame(application, thread)
  end unless;

  block ()
    if (precomputed-addresses)
      let ap-frame = call-frame.ap-frame-description;
      apply-thread-stepping-control(path,
                                    thread,
                                    precomputed-addresses,
                                    $step-operation-step-into,
                                    stack-frame: ap-frame);
      success? := #t;
      dm-thread.stepping-mode := $thread-stepping-into;
    else
      let (destination, use-function-register?, worked?)
        = dylan-calculate-destination-for-step-into(path, thread);
      if (worked?)
        if (use-function-register?)
          let function-register = dylan-current-function(path, thread);
          let addrs = function-object-breakpoint-addresses(function-register);
          if (addrs)
            let ap-frame = call-frame.ap-frame-description;
            apply-thread-stepping-control(path,
                                          thread,
                                          addrs,
                                          $step-operation-step-into,
                                          stack-frame: ap-frame);
            success? := #t;
            dm-thread.stepping-mode := $thread-stepping-into;
          else
            debug-message("DM: Function register live but not a function!?");
            success? := #f;
          end if;
        else
          let ap-frame = call-frame.ap-frame-description;
          apply-thread-stepping-control(path,
                                        thread,
                                        vector(destination),
                                        $step-operation-step-into,
                                        stack-frame: ap-frame);
          success? := #t;
          dm-thread.stepping-mode := $thread-stepping-into;
        end if;
      else
        debug-message("DM: Destination for step-into could not be computed");
        success? := #f;
      end if
    end if;
  exception (<debug-point-error>)
    success? := #f;
  exception (<remote-access-violation-error>)
    success? := #f;
  end block;
  success?;
end method;

