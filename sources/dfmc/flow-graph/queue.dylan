module: dfmc-flow-graph
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// <QUEUEABLE-ITEM-MIXIN>
/// Queueable items have a next link that points to the rest of the queue.
/// They can only belong to one queue. The status field says whether an item
/// should be ignored when processing the queue (#"dead"), whether its
/// in a queue (#"absent" or #f). Items will only be added to a queue if
/// they are #"absent".
///
/// Maybe we should just add the slots to  <computation> directly?
///
/// A thought: if we use the queue itself to indicate that the item was in a
/// queue rather than #f then we wouldn't have to pass the queue around as an
/// argument everywhere.

define abstract class <queueable-item-mixin> (<object>)
  slot item-properties :: <integer> = 0;
end class <queueable-item-mixin>;

define constant $queueable-item-absent  = 0;
define constant $queueable-item-present = 1;
define constant $queueable-item-dead    = 2;

define packed-slots item-properties (<queueable-item-mixin>, <object>)
  field slot item-status = $queueable-item-absent, field-size: 2;
end packed-slots;

define method mark-as-dead (x :: <queueable-item-mixin>) => ()
  x.item-status := $queueable-item-dead;
end method mark-as-dead;

/// <QUEUE-MIXIN>
/// Basically holds the queue. Queue operations such as adding and popping
/// etc are defined on this class and <queueable-item-mixin>. The operations
/// maintain as far as possible the integrity of the queue.

define constant <queue> = <stretchy-object-vector>;

/// Protocol for <queue>s and <queueable-item-mixin>s

// adds an (#"absent") item to a queue.
define generic add-to-queue!
  (queue :: <queue>, item :: <queueable-item-mixin>) => ();

// returns the first non-dead item from a queue. Will pop dead items if found.
define generic queue-head
  (queue :: <queue>) => ( item :: false-or(<queueable-item-mixin>));

// pop items from the queue until it pops a non dead item, which it returns
define generic queue-pop
  (queue :: <queue>) => ( item :: false-or(<queueable-item-mixin>));

define generic reverse-queue! (queue :: <queue>) => ();

/// Default implementation of protocol for <queue>s and <queueable-item-mixin>s

define method print-queue-out (queue)
  do-queue(method (i)
             format-out("  ELT %= STATUS %=\n", i, i.item-status)
           end, queue);
end method;

define method queue-pop! (queue :: <queue>)
 => (item :: false-or(<queueable-item-mixin>))
  let new-size = size(queue) - 1;
  unless (new-size = -1)
    let elt = queue[new-size];
    elt.item-status := $queueable-item-absent;
    size(queue) := new-size;
    elt
  end unless
end method;

define inline method queue-push!
    (queue :: <queue>, item :: <queueable-item-mixin>) => ()
  add!(queue, item)
end method;

define method add-to-queue!
    (queue :: <queue>, item :: <queueable-item-mixin>) => ()
  if (item.item-status == $queueable-item-absent)
    item.item-status := $queueable-item-present;
    queue-push!(queue, item);
  end if;
end method add-to-queue!;

define method pop-dead! (queue :: <queue>)
       => (item :: false-or(<queueable-item-mixin>))
  iterate loop (i :: <integer> = size(queue) - 1)
    unless (i < 0)
      let item = queue[i];
      if (item.item-status == $queueable-item-dead)
	size(queue) := i;
	loop(i - 1);
      else
	item // return first non-dead item
      end if;
    end unless;
  end iterate;
end method;

define inline method queue-head (queue :: <queue>)
 => (item :: false-or(<queueable-item-mixin>))
  pop-dead!(queue);
end method;

define method queue-pop (queue :: <queue>)
       => (item :: false-or(<queueable-item-mixin>))
  let item = pop-dead!(queue);
  if (item)
    item.item-status := $queueable-item-absent;
    queue-pop!(queue)
  end if;
end method;

define method reverse-queue! (queue :: <queue>) => ()
  pop-dead!(queue);
  reverse!(queue);
  pop-dead!(queue);
end method reverse-queue!;

define method do-queue (function :: <function>, queue :: <queue>) => ()
  pop-dead!(queue);
  for (item in queue)
    unless (item.item-status == $queueable-item-dead)
      function(item);
    end unless;
  end for;
end method;
