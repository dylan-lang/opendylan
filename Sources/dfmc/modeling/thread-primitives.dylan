module: dfmc-modeling
author: jonathan bachrach
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Threads

define side-effecting stateful dynamic-extent &primitive primitive-make-thread
    (thread :: <object>, name :: <object>,
     priority :: <integer>, function :: <function>,
     synchronous? :: <raw-boolean>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-destroy-thread
    (thread :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-thread-join-single
    (thread :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-thread-join-multiple
    (thread-vector :: <simple-object-vector>) => (res :: <object>);

define side-effecting stateful dynamic-extent &primitive primitive-thread-yield 
    () => ();

define side-effect-free stateless dynamic-extent &primitive primitive-current-thread
    () => (thread :: <object>);

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-recursive-lock
    (lock :: <object>) => (res :: <integer>); 

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-semaphore
    (lock :: <object>) => (res :: <integer>); 

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-notification
    (notif :: <object>, lock :: <object>) 
    => (res :: <integer>); 

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-simple-lock-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-recursive-lock-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-semaphore-timed
    (lock :: <object>, ms :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-wait-for-notification-timed
    (notif :: <object>, lock :: <object>, ms :: <integer>) 
     => (res :: <integer>); 

define side-effecting stateful dynamic-extent &primitive primitive-release-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-release-recursive-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-release-semaphore
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-release-notification
    (notif :: <object>, lock :: <object>) 
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-release-all-notification
    (notif :: <object>, lock :: <object>) 
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-make-recursive-lock
    (lock :: <object>, name :: <object>) 
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-destroy-recursive-lock 
    (obj) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-make-simple-lock
    (lock :: <object>, name :: <object>) 
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-destroy-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effect-free stateful dynamic-extent &primitive primitive-owned-simple-lock 
    (lock :: <object>)  => (res :: <integer>);

define side-effect-free stateful dynamic-extent &primitive primitive-owned-recursive-lock 
    (lock :: <object>)  => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-make-semaphore
    (lock :: <object>, name :: <object>, 
    init :: <integer>, max :: <integer>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-destroy-semaphore 
    (obj :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-make-notification
    (lock :: <object>, name :: <object>) 
    => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-destroy-notification 
    (obj :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-sleep 
    (ms :: <integer>) => ();


/*
define side-effecting stateful dynamic-extent &primitive primitive-assign-atomic-memory
    (location :: <raw-pointer>, newval :: <object>) => (newval);

define side-effecting stateful dynamic-extent &primitive primitive-conditional-update-memory
    (location :: <raw-pointer>, newval, oldval) 
    => (res :: <integer>);
*/


define side-effecting stateful indefinite-extent &primitive primitive-allocate-thread-variable
    (initial-value :: <object>) => (handle :: <raw-pointer>);

define side-effect-free dynamic-extent stateless &primitive primitive-read-thread-variable
    (handle :: <raw-pointer>) => (value :: <object>);

define side-effecting stateless dynamic-extent &primitive primitive-write-thread-variable
    (handle :: <raw-pointer>, newval :: <object>) => (newval);

define side-effecting stateful dynamic-extent &primitive primitive-initialize-current-thread
    (thread :: <object>, synchronous? :: <raw-boolean>) => ();

define side-effecting stateful dynamic-extent &primitive primitive-initialize-special-thread
    (thread :: <object>) => ();

define side-effecting stateful dynamic-extent &primitive primitive-unlock-simple-lock
    (lock :: <object>) => (res :: <integer>);

define side-effecting stateful dynamic-extent &primitive primitive-unlock-recursive-lock
    (lock :: <object>) => (res :: <integer>);

define stateful side-effect-free dynamic-extent &primitive primitive-sequence-point () => ();

define side-effecting stateful dynamic-extent &primitive primitive-synchronize-side-effects
    () => ();

// eof
