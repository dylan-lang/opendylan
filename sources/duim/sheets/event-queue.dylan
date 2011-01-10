Module:       duim-sheets-internals
Synopsis:     DUIM sheets
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Event queues

// Event queues encapsulate both a queue and a notification that serves to
// synchronize multiple threads
define sealed class <event-queue> (<object>)
  sealed constant slot %deque :: <object-deque> = make(<object-deque>);
  sealed constant slot %non-empty :: <notification> = make(<notification>, lock: make(<lock>));
end class <event-queue>;

define sealed domain make (singleton(<event-queue>));
define sealed domain initialize (<event-queue>);


define sealed method event-queue-push
    (queue :: <event-queue>, event :: <event>) => ()
  with-lock (associated-lock(queue.%non-empty))
    when (empty?(queue.%deque))
      // If there are any threads waiting for something to go into
      // the event queue, wake them up now
      release-all(queue.%non-empty)
    end;
    push(queue.%deque, event)
  end
end method event-queue-push;

define sealed method event-queue-push-last
    (queue :: <event-queue>, event :: <event>) => ()
  with-lock (associated-lock(queue.%non-empty))
    when (empty?(queue.%deque))
      release-all(queue.%non-empty)
    end;
    push-last(queue.%deque, event)
  end
end method event-queue-push-last;

define sealed method event-queue-pop
    (queue :: <event-queue>) => (event :: <event>)
  with-lock (associated-lock(queue.%non-empty))
    // Block until there's something to pop
    while (empty?(queue.%deque))
      wait-for(queue.%non-empty);
    end;
    pop(queue.%deque)
  end
end method event-queue-pop;

define sealed method event-queue-top
    (queue :: <event-queue>) => (event :: <event>)
  with-lock (associated-lock(queue.%non-empty))
    while (empty?(queue.%deque))
      wait-for(queue.%non-empty);
    end;
    queue.%deque[0]
  end
end method event-queue-top;

define sealed method event-queue-wait
    (queue :: <event-queue>, #key timeout) => (timed-out? :: <boolean>)
  with-lock (associated-lock(queue.%non-empty))
    while (empty?(queue.%deque))
      wait-for(queue.%non-empty, timeout: timeout);
    end;
    // If the queue is empty, we must have timed out
    empty?(queue.%deque)
  end
end method event-queue-wait;

define sealed method event-queue-empty?
    (queue :: <event-queue>) => (true? :: <boolean>)
  // Don't bother locking
  empty?(queue.%deque)
end method event-queue-empty?;

define sealed method event-queue-clear (queue :: <event-queue>) => ()
  with-lock (associated-lock(queue.%non-empty))
    queue.%deque.size := 0
  end
end method event-queue-clear;

