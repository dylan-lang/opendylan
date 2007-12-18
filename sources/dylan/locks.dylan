module:    threads-internal
Synopsis:  The implementation of the <lock> classes
Author:    Tony Mann
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND




//// <lock>


define open abstract class <lock> (<synchronization>)
end class;


define sealed inline method make (class == <lock>, #rest keys, #key, #all-keys) 
    => (lock :: <simple-lock>)
  apply(make, <simple-lock>, keys);
end method;


define inline function lock-wait-result 
    (lock :: <lock>, prim-res :: <integer>) => (res :: <boolean>)
  select (prim-res)
    $success => #t;
    $timeout => #f;
    otherwise => lock-wait-result-error(lock, prim-res);
  end select;
end;


define method lock-wait-result-error
    (lock :: <lock>, prim-res :: <integer>) => (res :: <boolean>)
  select (prim-res)
    $success => #t;
    $timeout => #f;
    $pre-locked => error(make(<already-owned-error>, lock: lock));
    otherwise => error(make(<unexpected-synchronization-error>, 
                            synchronization: lock));
  end select;
end method;


define inline function lock-release-result 
    (lock :: <lock>, prim-res :: <integer>) => ()
  unless (prim-res == $success)
    lock-release-result-error(lock, prim-res);
  end unless;
end;


define method lock-release-result-error
    (lock :: <lock>, prim-res :: <integer>) => ()
  select (prim-res)
    $unlocked => error(make(<not-owned-error>, lock: lock));
    $count-exceeded => error(make(<count-exceeded-error>, lock: lock));
    otherwise => error(make(<unexpected-synchronization-error>, 
                            synchronization: lock));
  end select;
end method;


//// <semaphore>


define constant $semaphore-maximum-count-limit = 1000000;


define open abstract primary class <semaphore> 
  (<portable-container>, <lock>)

  constant slot initial-count :: <integer>,
    init-value: 0, init-keyword: initial-count:;

  constant slot maximum-count :: <integer>,
    init-value: $semaphore-maximum-count-limit, init-keyword: maximum-count:;

end class;


define sealed class <semaphore-i> (<semaphore>)
end class;

define sealed domain make (singleton(<semaphore-i>));
define sealed domain initialize (<semaphore-i>);


define sealed inline method make 
    (class == <semaphore>, #rest keys, #key, #all-keys)
 => (lock :: <semaphore-i>)
  apply(make, <semaphore-i>, keys);
end method;


define method initialize (lock :: <semaphore>, #key) => ()
  drain-finalization-queue();
  next-method();
  let res = primitive-make-semaphore(lock, 
                                     lock.synchronization-name, 
                                     lock.initial-count,
                                     lock.maximum-count);
  check-synchronization-creation(lock, res);
  finalize-when-unreachable(lock);
end method;


define inline sealed method release (lock :: <semaphore>, #key) => ()
  debug-out(#"lock", "Releasing lock %= in thread %=\n", 
            lock, current-thread-id());
  let res = primitive-release-semaphore(lock);
  lock-release-result(lock, res);
end method;


define inline sealed method wait-for (lock :: <semaphore>, #key timeout) => (success?)
  debug-out(#"lock", "Waiting for lock %= in thread %=\n", 
            lock, current-thread-id());
  let res = if (timeout)
               primitive-wait-for-semaphore-timed(lock, timeout.millisecs)
             else
               primitive-wait-for-semaphore(lock)
             end if;
  debug-out(#"lock", "Waiting for lock %= in thread %= returned %=\n", 
            lock, current-thread-id(), res);
  lock-wait-result(lock, res);
end method;


define sealed method finalize (lock :: <semaphore>) => ()
  let res = primitive-destroy-semaphore(lock);
  check-synchronization-finalization(lock, res);
end method;




//// <exclusive-lock>


define open abstract class <exclusive-lock> (<lock>)
end class;


define sealed inline method make 
    (class == <exclusive-lock>, #rest keys, #key, #all-keys)
 => (lock :: <simple-lock>)
  apply(make, <simple-lock>, keys);
end method;


define open generic owned? (lock :: <exclusive-lock>) => (owned? :: <boolean>);





//// <recursive-lock>


define open abstract primary class <recursive-lock> 
  (<portable-container>, <exclusive-lock>)
end class;


define sealed class <recursive-lock-i> (<recursive-lock>)
end class;

define sealed domain make (singleton(<recursive-lock-i>));
define sealed domain initialize (<recursive-lock-i>);


define sealed inline method make 
    (class == <recursive-lock>, #rest keys, #key, #all-keys)
 => (lock :: <recursive-lock-i>)
  apply(make, <recursive-lock-i>, keys);
end method;


define method initialize (lock :: <recursive-lock>, #key) => ()
  drain-finalization-queue();
  next-method();
  let res = primitive-make-recursive-lock(lock, lock.synchronization-name);
  check-synchronization-creation(lock, res);
  finalize-when-unreachable(lock);
end method;


define inline sealed method release (lock :: <recursive-lock>, #key) => ()
  debug-out(#"lock", "Releasing lock %= in thread %=\n", 
            lock, current-thread-id());
  let res = primitive-release-recursive-lock(lock);
  lock-release-result(lock, res);
end method;


define inline sealed method wait-for
      (lock :: <recursive-lock>, #key timeout) => (success?)
  debug-out(#"lock", "Waiting for lock %= in thread %=\n", 
            lock, current-thread-id());
  let res = if (timeout)
               primitive-wait-for-recursive-lock-timed(lock, timeout.millisecs)
             else
               primitive-wait-for-recursive-lock(lock)
             end if;
  debug-out(#"lock", "Waiting for lock %= in thread %= returned %=\n", 
            lock, current-thread-id(), res);
  lock-wait-result(lock, res);
end method;


define sealed method owned? (lock :: <recursive-lock>) => (owned? :: <boolean>)
  primitive-owned-recursive-lock(lock) == $true;
end method;


define sealed method finalize (lock :: <recursive-lock>) => ()
  let res = primitive-destroy-recursive-lock(lock);
  check-synchronization-finalization(lock, res);
end method;





//// <simple-lock>


define open abstract primary class <simple-lock> 
  (<portable-container>, <exclusive-lock>)
end class;


define sealed class <simple-lock-i> (<simple-lock>)
end class;

define sealed domain make (singleton(<simple-lock-i>));
define sealed domain initialize (<simple-lock-i>);


// make-simple-lock is a fast mechanism for allocating <simple-lock>s
// which may be used early in the bootstrap of the Dylan library.

define function make-simple-lock () => (result :: <simple-lock>)
  let instance :: <simple-lock-i> =
    system-allocate-simple-instance(<simple-lock-i>, fill: #f);
  initialize-simple-lock(instance, #f);
  instance
end;

define function initialize-simple-lock (lock :: <simple-lock>, name) => ()
  drain-finalization-queue();
  let res = primitive-make-simple-lock(lock, name);
  check-synchronization-creation(lock, res);
  finalize-when-unreachable(lock);
end;



define sealed inline method make 
    (class == <simple-lock>, #rest keys, #key, #all-keys)
 => (lock :: <simple-lock-i>)
  apply(make, <simple-lock-i>, keys);
end method;


define method initialize (lock :: <simple-lock>, #key) => ()
  next-method();
  initialize-simple-lock(lock, lock.synchronization-name);
end method;


define inline sealed method release (lock :: <simple-lock>, #key) => ()
  debug-out(#"lock", "Releasing lock %= in thread %=\n", 
            lock, current-thread-id());
  let res = primitive-release-simple-lock(lock);
  lock-release-result(lock, res);
end method;


define inline sealed method wait-for
      (lock :: <simple-lock>, #key timeout) => (success?)
  debug-out(#"lock", "Waiting for lock %= in thread %=\n", 
            lock, current-thread-id());
  let res = if (timeout)
               primitive-wait-for-simple-lock-timed(lock, timeout.millisecs)
             else
               primitive-wait-for-simple-lock(lock)
             end if;
  debug-out(#"lock", "Waiting for lock %= in thread %= returned %=\n", 
            lock, current-thread-id(), res);
  lock-wait-result(lock, res);
end method;


define sealed method owned? (lock :: <simple-lock>) => (owned? :: <boolean>)
  primitive-owned-simple-lock(lock) == $true;
end method;


define sealed method finalize (lock :: <simple-lock>) => ()
  let res = primitive-destroy-simple-lock(lock);
  check-synchronization-finalization(lock, res);
end method;



//// <read-write-lock>
//// The class of multiple-reader single-writer locks


// The internal state of a <read-write-lock> is either :
//   a <thread>, indicating the thread which owns the lock in write mode
// or
//   an <integer>, indicating the number of times the lock has been claimed
//   in read mode.
// When unlocked, the state will be 0
//
// The lock class is implemented as a monitored data structure. 
// A <notification> is released whenever there is a possibility
// of a state transition from read mode to write mode (i.e.
// whenever the lock moves into the unlocked state)

define constant <lock-state> = type-union(<thread>, <integer>);


define open abstract primary class <read-write-lock> (<exclusive-lock>)
  constant slot internal-monitor = make(<notification>, lock: make(<simple-lock>));
  slot rw-lock-state :: <lock-state> = 0;
end class;


define sealed class <read-write-lock-i> (<read-write-lock>)
end class;


define sealed domain make (singleton(<read-write-lock-i>));
define sealed domain initialize (<read-write-lock-i>);


define sealed inline method make 
    (class == <read-write-lock>, #rest keys, #key, #all-keys)
 => (lock :: <read-write-lock-i>)
  apply(make, <read-write-lock-i>, keys);
end method;


define sealed method release (lock :: <read-write-lock>, #key) => ()
  debug-out(#"lock", "Releasing lock %= in thread %=\n", 
            lock, current-thread-id());
  let monitor = lock.internal-monitor;
  let inner-lock = monitor.associated-lock;
  let res =
    with-lock (inner-lock)
      let state = lock.rw-lock-state;
  
      if (state.locked-for-writing?)
        lock.rw-lock-state := 0;
        release-all(monitor);
        #t
  
      elseif (state.locked-for-reading?)
        let new-state = state - 1;
        lock.rw-lock-state := new-state;
        if (new-state == 0)
          release-all(monitor);
        end if;
        #t;

      else #f;
      end if;
    end with-lock;

  unless (res)
    lock-release-result-error(lock, $unlocked);
  end unless;
end method;


define sealed method wait-for
      (lock :: <read-write-lock>, #key timeout, mode = #"read") => (success?)
  if (mode == #"read" | mode == #"write")
    debug-out(#"lock", "Waiting for lock %= in thread %=\n", 
              lock, current-thread-id());
    let monitor = lock.internal-monitor;
    let inner-lock = monitor.associated-lock;
    block (exit)
      with-lock (inner-lock)
    
        if (mode == #"write")
          until (lock.lock-is-free?)
            unless (wait-for(monitor, timeout: timeout))
              debug-out(#"lock", "Acquired lock %= in thread %=\n", 
                        lock, current-thread-id());
              exit(#f);
            end unless;
          end until;
          lock.rw-lock-state := current-thread();
    
        else // mode == #"read"
          until (lock.lock-is-free-for-reading?)
            unless (wait-for(monitor, timeout: timeout))
              debug-out(#"lock", "Acquired lock %= in thread %=\n", 
                        lock, current-thread-id());
              exit(#f);
            end unless;
          end until;
          lock.rw-lock-state := lock.rw-lock-state + 1;

        end if;
        #t
      end with-lock;
    end block;

  else
    error("Unknown mode for waiting for lock: %=", mode);
  end if;
end method;


define sealed method owned? (lock :: <read-write-lock>) => (owned? :: <boolean>)
  lock.rw-lock-state.locked-for-writing?;
end method;

define sealed method owned-for-reading? (lock :: <read-write-lock>) => (owned? :: <boolean>)
  lock.rw-lock-state.locked-for-reading?;
end method;



define inline method locked-for-writing? 
    (state :: <lock-state>) => (locked? :: <boolean>)
  state == current-thread();
end method;

define inline method locked-for-reading? 
    (state :: <lock-state>) => (locked? :: <boolean>)
  instance?(state, <integer>) & (state > 0);
end method;

define inline method lock-is-free? 
    (rw-lock :: <read-write-lock>) => (free? :: <boolean>)
  rw-lock.rw-lock-state == 0;
end method;

define inline method lock-is-free-for-reading? 
    (rw-lock :: <read-write-lock>) => (free? :: <boolean>)
  instance?(rw-lock.rw-lock-state, <integer>)
end method;


//Helper for debug output
define inline function current-thread-id () => (res)
  current-thread().thread-name | current-thread();
end;
