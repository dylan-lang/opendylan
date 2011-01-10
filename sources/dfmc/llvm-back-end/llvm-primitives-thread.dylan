Module: dfmc-llvm-back-end
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              Additional code is Copyright 2009-2010 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Thread environment

// %teb-tlv-index is the variable which contains the handle for the
// Windows TLV/pthreads key which holds the TEB for each thread.
//
define runtime-variable %teb-tlv-index :: <raw-machine-word>
  = make-raw-literal(as(<machine-word>, -1));

// %teb-chain is a variable holding the current TEB in a chain of live TEBs
//
define runtime-variable %teb-chain :: <raw-address>
  = make-raw-literal(0);


/// Thread primitives

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-thread
    (thread :: <object>, name :: <object>,
     priority :: <integer>, function :: <function>,
     synchronous? :: <raw-boolean>)
  => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-thread
    (thread :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-thread-join-single
    (thread :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-thread-join-multiple
    (thread-vector :: <simple-object-vector>) => (res :: <object>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-thread-yield
    () => ();

define side-effect-free stateless dynamic-extent &c-primitive-descriptor primitive-current-thread
    () => (thread :: <object>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-recursive-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-semaphore
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-notification
    (notif :: <object>, lock :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-simple-lock-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-recursive-lock-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-semaphore-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-wait-for-notification-timed
    (notif :: <object>, lock :: <object>, ms :: <integer>)
     => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-recursive-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-semaphore
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-notification
    (notif :: <object>, lock :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-release-all-notification
    (notif :: <object>, lock :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-recursive-lock
    (lock :: <object>, name :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-recursive-lock
    (obj) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-simple-lock
    (lock :: <object>, name :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effect-free stateful dynamic-extent &c-primitive-descriptor primitive-owned-simple-lock
    (lock :: <object>)  => (res :: <integer>);

define side-effect-free stateful dynamic-extent &c-primitive-descriptor primitive-owned-recursive-lock
    (lock :: <object>)  => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-semaphore
    (lock :: <object>, name :: <object>,
    init :: <integer>, max :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-semaphore
    (obj :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-make-notification
    (lock :: <object>, name :: <object>)
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-destroy-notification
    (obj :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-sleep
    (ms :: <integer>) => ();

/*
define side-effecting stateful dynamic-extent &primitive-descriptor primitive-assign-atomic-memory
    (location :: <raw-pointer>, newval :: <object>) => (newval)
end;

define side-effecting stateful dynamic-extent &primitive-descriptor primitive-conditional-update-memory
    (location :: <raw-pointer>, newval, oldval)
    => (res :: <integer>)
end;
*/

define side-effecting stateful indefinite-extent &c-primitive-descriptor primitive-allocate-thread-variable
    (initial-value :: <object>) => (handle :: <raw-pointer>);

define side-effect-free dynamic-extent stateless &unimplemented-primitive-descriptor primitive-read-thread-variable
    (handle :: <raw-pointer>) => (value :: <object>);
  //---*** Fill this in...
end;

define side-effecting stateless dynamic-extent &unimplemented-primitive-descriptor primitive-write-thread-variable
    (handle :: <raw-pointer>, newval :: <object>) => (newval);
  //---*** Fill this in...
end;

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-initialize-current-thread
    (thread :: <object>, synchronous? :: <raw-boolean>) => ();

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-initialize-special-thread
    (thread :: <object>) => ();

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-detach-thread
    (thread :: <object>) => ();

/*
define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-unlock-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &c-primitive-descriptor primitive-unlock-recursive-lock
    (lock :: <object>) => (res :: <integer>);
*/

define stateful side-effect-free dynamic-extent &unimplemented-primitive-descriptor primitive-sequence-point () => ();
  //---*** Fill this in...
end;

define side-effecting stateful dynamic-extent &unimplemented-primitive-descriptor primitive-synchronize-side-effects
    () => ();
  //---*** Fill this in...
end;
