module:        dm-internals
synopsis:      Modelling the dynamic environment of a thread.
author:        Paul Howard
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// The following constants are derived from the layout descriptions of
// bind-exit and unwind-protect frame given in:

//      D-harp-pentium-rtg!nlx-primitives.dylan


///// ------------------------ BIND-EXIT FRAME ----------------------------

//define constant $BXF-vectored-MV-offset          = 0;
//define constant $BXF-stack-vector-header-offset  = 1;
//define constant $BXF-stack-vector-size-offset    = 2; Not currently used
//define constant $BXF-stack-vector-first-offset   = 3; Not currently used
//define constant $BXF-dynamic-environment-offset  = 11;
//define constant $BXF-frame-pointer-offset        = 12;
//define constant $BXF-continuation-address-offset = 13;

define class <bind-exit-frame> (<implementation-stack-frame>)
/*
  slot bind-exit-mv-vector :: <remote-value>,
    required-init-keyword: mv-vector:;

  slot bind-exit-call-frame-pointer :: <remote-value>,
    required-init-keyword: call-frame-pointer:;

  slot bind-exit-stack-mv-vector :: <sequence>,
    init-value: #[],
    init-keyword: stack-mv-vector:;

  slot bind-exit-dynamic-environment-pointer :: <remote-value>,
    required-init-keyword: dynamic-environment-pointer:;

  slot bind-exit-continuation-pointer :: <remote-value>,
    required-init-keyword: continuation:;
*/
end class;


///// ---------------------- UNWIND-PROTECT FRAME -------------------------

define constant $UPF-previous-UPF-offset         = 0;
define constant $UPF-frame-pointer-offset        = 1;
define constant $UPF-cleanup-address-offset      = 2;
define constant $UPF-last                        = as-remote-value(0);

define class <unwind-protect-frame> (<implementation-stack-frame>)

  constant slot cleanup-address :: <remote-value>,
    required-init-keyword: cleanup-address:;

  constant slot unwind-protect-call-frame-pointer :: <remote-value>,
    required-init-keyword: call-frame-pointer:;

end class;


///// READ-UNWIND-PROTECT
//    Generates all values associated with an unwind protect frame. The
//    supplied pointer must be known to be a legal unwind protect frame.

define method read-unwind-protect
    (application :: <debug-target>, uwp :: <remote-value>)
       => (previous-uwp :: <remote-value>,
           call-frame-pointer :: <remote-value>,
           cleanup-addr :: <remote-value>)
  let path = debug-target-access-path(application);
  block ()
    values
      (read-value
         (path, indexed-remote-value(uwp, $UPF-previous-UPF-offset)),
       read-value
         (path, indexed-remote-value(uwp, $UPF-frame-pointer-offset)),
       read-value
         (path, indexed-remote-value(uwp, $UPF-cleanup-address-offset)));
  exception (<remote-access-violation-error>)
    values($UPF-last, as-remote-value(0), as-remote-value(0));
  end block;
end method;


///// COUNT-UNWIND-PROTECTS
//    Determines the number of unwind-protect frames that make up the
//    chain.

define method count-unwind-protects
    (application :: <debug-target>, upf :: <remote-value>)
    => (count :: <integer>)
  let path = application.debug-target-access-path;
  let this-upf :: <remote-value> = upf;
  let count = 0;
  block ()
    while (~(this-upf = $UPF-last))
      // NB: Efficiency hack!!!
      // The following read-value should, by rights, be:
      // (path, indexed-remote-value(this-upf, $UPF-previous-UPF-offset)).
      // However, while we know that the required offset is zero in the current
      // implementation, and we want to generate this count quickly, we
      // make use of the specialized knowledge.
      this-upf := read-value(path, this-upf);
      count := count + 1;
    end while;
  exception (<remote-access-violation-error>)
    count := 0
  end block;
  count;
end method;

/*
///// READ-BIND-EXIT
//    Generates all values associated with a bind-exit frame. The supplied
//    pointer must be known to be a legal unwind protect frame.

define method read-bind-exit
    (application :: <debug-target>, bxf :: <remote-value>)
       => (heap-mv-vector :: <remote-value>,
           stack-mv-vector :: <remote-value>,
           unwind-protect-chain :: <remote-value>,
           call-frame-pointer :: <remote-value>,
           continuation :: <remote-value>)
  let path = debug-target-access-path(application);
  values
    (read-value
       (path, indexed-remote-value(bxf, $BXF-vectored-mv-offset)),
     indexed-remote-value
       (path, $BXF-stack-vector-header-offset),
     read-value
       (path, indexed-remote-value(bxf, $BXF-dynamic-environment-offset)),
     read-value
       (path, indexed-remote-value(bxf, $BXF-frame-pointer-offset)),
     read-value
       (path, indexed-remote-value(bxf, $BXF-continuation-address-offset)))
end method;
*/

///// BUILD-DYNAMIC-ENVIRONMENT
//    Given a remote-value known to be a pointer to an unwind-protect
//    chain, return an ordered sequence of the chain of unwind-protect
//    frames.
//    Note that this takes a pointer to the first dynamic frame as an
//    argument. This is to allow the function to build a chain of
//    unwind protect frames from an arbitrary point - such as the
//    "dynamic environment" field of a bind-exit frame!!!

define method build-dynamic-environment
    (application :: <debug-target>, thread :: <remote-thread>,
     upf :: <remote-value>)
       => (newest-to-oldest :: <vector>)

  // Allocate an empty stretchy-vector to hold the sequence of frames.
  let seq = #[];

  // If there is no chain to build, then do nothing.
  let counter = count-unwind-protects(application, upf);

  unless (counter == 0)
    seq := make(<vector>, size: counter);
    let i = 0;
    let built-frame = #f;
    let this-upf :: <remote-value> = upf;
    let (previous :: <remote-value>, 
         call-fp :: <remote-value>, 
         cleanup-addr :: <remote-value>) = 
       read-unwind-protect(application, upf);
    while (~(this-upf = $UPF-last))
      let frame-in-hand = built-frame;
      built-frame := make(<unwind-protect-frame>,
                          generic-fp: this-upf,
                          thread: thread,
                          linked-from: frame-in-hand,
                          cleanup-address: cleanup-addr,
                          call-frame-pointer: call-fp);
      ignore(built-frame.cleanup-address);
      ignore(built-frame.unwind-protect-call-frame-pointer);
      seq[i] := built-frame;
      i := i + 1;
      let (new-upf :: <remote-value>, 
           new-call-fp :: <remote-value>, 
           new-cleanup-addr :: <remote-value>) =
         read-unwind-protect(application, this-upf);
      this-upf := new-upf;
      call-fp := new-call-fp;
      cleanup-addr := new-cleanup-addr;
    end while;
  end unless;

  // And return the sequence.
  seq;
end method;

/*
///// BUILD-BIND-EXIT
//    Given a remote-value known to be a pointer to a bind-exit frame,
//    build the actual <bind-exit-frame> object.

define method build-bind-exit
    (application :: <debug-target>, thread :: <remote-thread>, 
     bxf :: <remote-value>)
       => (bxf-object :: <bind-exit-frame>)

  let (heap-mv-vector, stack-mv-vector, up-chain, call-fp, continuation)
    = read-bind-exit(application, bxf);

  // Just construct the instance.
  make(<bind-exit-frame>,
       generic-frame-pointer: bxf,
       thread: thread,
       mv-vector: heap-mv-vector,
       call-frame-pointer: call-fp,
       dynamic-environment-pointer: up-chain,
       continuation: continuation)
end method;
*/

///// THREAD-DYNAMIC-ENVIRONMENT
//    Gets the pointer to the current unwind protect frame for a thread.

define method thread-dynamic-environment
    (application :: <debug-target>, thread :: <remote-thread>)
       => (upf-top :: <remote-value>)
  let path = application.debug-target-access-path;
  let teb-base = dylan-thread-environment-block-address(path, thread);
  let dynamic-env-address
         = indexed-remote-value(teb-base, $TEB-dynamic-environment-offset);
  block ()
    read-value(path, dynamic-env-address);
  exception (pants :: <remote-access-violation-error>)
    $UPF-last;
  end block;
end method;
