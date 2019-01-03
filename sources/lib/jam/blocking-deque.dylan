Module:       jam-internals
Author:       Peter S. Housel
Copyright:    Original Code is Copyright 2004-2018 Gwydion Dylan Maintainers
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define class <blocking-deque> (<deque>)
  constant slot %deque-storage :: <object-deque> = make(<object-deque>);
  constant slot %deque-lock :: <lock> = make(<lock>);
  slot %deque-notification :: <notification>;
end class;

define sealed method initialize
    (instance :: <blocking-deque>, #next next-method, #key) => ();
  instance.%deque-notification := make(<notification>, lock: instance.%deque-lock);
end method;

define sealed method push-last
    (deque :: <blocking-deque>, new-element :: <object>)
 => (new-element :: <deque>);
  with-lock (deque.%deque-lock)
    push-last(deque.%deque-storage, new-element);
    release(deque.%deque-notification);
  end;
  deque
end method;

define method blocking-pop (deque :: <blocking-deque>) => (object)
  with-lock (deque.%deque-lock)
    while (empty?(deque.%deque-storage))
      wait-for(deque.%deque-notification);
    end while;
    pop(deque.%deque-storage)
  end with-lock
end method;
