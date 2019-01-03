Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// API inspired by https://github.com/kiuma/thread-pool

define class <thread-pool> (<object>)
  constant slot %pool-size :: <integer>,
    init-value: 1, init-keyword: size:;
  constant slot %pool-lock :: <lock> = make(<lock>);
  slot %pool-notification :: <notification>;
  constant slot %pool-threads :: <stretchy-vector> = make(<stretchy-vector>);
  constant slot %pool-queue :: <object-deque> = make(<object-deque>);
  slot %pool-state :: one-of(#"stopped", #"running", #"stopping"),
    init-value: #"stopped";
end class;

define sealed method initialize
    (instance :: <thread-pool>, #next next-method, #key) => ();
  instance.%pool-notification := make(<notification>, lock: instance.%pool-lock);
end method;

define method thread-pool-start (pool :: <thread-pool>) => ();
  local
    method worker()
      let thunk
        = with-lock (pool.%pool-lock)
            while (pool.%pool-state == #"running" & empty?(pool.%pool-queue))
              wait-for(pool.%pool-notification);
            end while;
            if (pool.%pool-state == #"running")
              pop(pool.%pool-queue)
            end if
          end with-lock;
      if (thunk)
        thunk();
        worker();
      end;
    end;

  with-lock (pool.%pool-lock)
    assert(pool.%pool-state == #"stopped",
           "thread-pool-start requires a stopped thread pool");
    pool.%pool-state := #"running";
    for (i from 0 below pool.%pool-size)
      add!(pool.%pool-threads, make(<thread>, function: worker));
    end for;
  end with-lock;
end method;

define method thread-pool-stop (pool :: <thread-pool>) => ();
  with-lock (pool.%pool-lock)
    assert(pool.%pool-state == #"running",
           "thread-pool-stop requires a running thread pool");
    pool.%pool-state := #"stopping";
    release-all(pool.%pool-notification);
  end with-lock;
  apply(join-thread, pool.%pool-threads);
  with-lock (pool.%pool-lock)
    pool.%pool-threads.size := 0;
    pool.%pool-state := #"stopped";
  end with-lock;
end method;

define method thread-pool-add
    (pool :: <thread-pool>, thunk :: <function>) => ();
  with-lock (pool.%pool-lock)
    if (empty?(pool.%pool-queue))
      release(pool.%pool-notification);
    end if;
    push-last(pool.%pool-queue, thunk);
  end with-lock;
end method;
