Module: orb-utilities
Author: Jason Trenouth
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// A <DEQUE> where POP and POP-LAST block if there are no elements.
/// They wait for another thread to add some via PUSH or PUSH-LAST.

define class <mailbox> (<deque>)
  constant slot mailbox-representation :: <deque> = make(<deque>);
  constant slot mailbox-lock :: <lock> = make(<lock>);
  constant slot mailbox-semaphore :: <semaphore> = make(<semaphore>);
end class;

define sealed domain make (subclass(<mailbox>));
define sealed domain initialize (<mailbox>);

define method pop (mailbox :: <mailbox>)
 => (item :: <object>)
  wait-for(mailbox-semaphore(mailbox));
  with-lock (mailbox-lock(mailbox))
    pop(mailbox-representation(mailbox));
  end;
end method;

define method push (mailbox :: <mailbox>, item :: <object>)
 => (item :: <object>)
  with-lock (mailbox-lock(mailbox))
    push(mailbox-representation(mailbox), item);
  end;
  release(mailbox-semaphore(mailbox));
end method;

define method pop-last (mailbox :: <mailbox>)
 => (item :: <object>)
  wait-for(mailbox-semaphore(mailbox));
  with-lock (mailbox-lock(mailbox))
    pop-last(mailbox-representation(mailbox));
  end;
end method;

define method push-last (mailbox :: <mailbox>, item :: <object>)
 => (item :: <object>)
  with-lock (mailbox-lock(mailbox))
    push-last(mailbox-representation(mailbox), item);
  end;
  release(mailbox-semaphore(mailbox));
end method;

define method empty? (mailbox :: <mailbox>)
 => (empty? :: <boolean>)
  with-lock (mailbox-lock(mailbox))
    empty?(mailbox-representation(mailbox));
  end;
end method;
