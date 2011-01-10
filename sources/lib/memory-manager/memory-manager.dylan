module: memory-manager
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define variable *last-size* = #f;

define class <garbage-collection>(<condition>)
  constant slot garbage-collection-info, init-keyword: info:;
end class;

define method last-size ()
  *last-size* | (*last-size* := room())
end method;

define method maybe-collect-garbage (delta) 
  let new-size = room();
  if ((new-size - last-size()) > delta)
    collect-garbage();
    *last-size* := room();
  end if;
end method;

define constant $max-heap = 50 * 1000000;

define method collect-garbage? (max-heap) => (collect? :: <boolean>)
  let max-heap =
    if (max-heap == #"default")
      $max-heap
    else
      max-heap * 1000000;
    end if;
  let new-size = room();
  new-size > max-heap
end method;

define method collect-garbage! () 
  collect-garbage();
end method;
