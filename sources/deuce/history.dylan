Module:       deuce-internals
Synopsis:     The Deuce editor
Author:       Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Basic history support

define variable $default-history-length :: <integer> = 40;

define constant <yank-state> = one-of(#"clipboard", #"yank", #"yank-next");

define open abstract primary class <basic-history> (<object>)
  sealed constant slot %lock :: <recursive-lock> = make(<recursive-lock>);
  sealed slot %elements :: <list> = #();
  sealed slot %current-length :: <integer> = 0;
  sealed slot %maximum-length :: false-or(<integer>) = $default-history-length,
    init-keyword: maximum-length:;
  sealed slot %rotation :: <integer> = 0;	// state for 'yank-next'
  sealed slot %position :: <integer> = 0;	// 'yank' to 'yank-next' communication
  sealed slot %temporary-element = #f;		// can be bound to temporary front of list
  // Where should 'yank-next' get the next element from?
  sealed slot %yank-state :: <yank-state> = #"yank",
    init-keyword: yank-state:;
end class <basic-history>;

define sealed method initialize-yank-state 
    (history :: <basic-history>) => ()
  history.%yank-state := #"yank"
end method initialize-yank-state;

// Number of elements in the history, including the temporary element
define sealed method history-length
    (history :: <basic-history>) => (length :: <integer>)
  if (history.%temporary-element)
    history.%current-length + 1
  else
    history.%current-length
  end
end method history-length;

// Get the element from HISTORY indexed by INDEX
define sealed method history-element
    (history :: <basic-history>, index :: <integer>, #key fixup?)
 => (elt, index :: false-or(<integer>))
  block (return)
    let delta :: <integer> = 0;
    with-lock (history.%lock)
      when (history.%temporary-element)
	when (index = 0 | (fixup? & index < 0))
	  return(history.%temporary-element, 0)
	end;
	inc!(index);
	dec!(delta)
      end;
      let elements = history.%elements;
      case
	index < 0 =>
	  values(fixup? & ~empty?(elements) & head(elements), delta);
	index < history.%current-length =>
	  values(~empty?(elements) & elements[index], index + delta);
	fixup? =>
	  values(~empty?(elements) & last(elements), history.%current-length + delta - 1);
	otherwise =>
	  values(#f, #f);
      end
    end
  end
end method history-element;

// Walk over the elements in the history, starting at INDEX offset by OFFSET,
// continuing CUTOFF-LENGTH times.  FUNCTION gets called with two arguments,
// the element and its index.  If TEST is supplied, it should be a function
// of one argument, the element.
define sealed method do-history-elements
    (function :: <function>, history :: <basic-history>,
     #key index, offset :: <integer> = 0, cutoff-length, test)
  let index :: <integer>
    = case
	~index    => offset;
	index > 0 => offset + index - 1;
	otherwise => offset + index;
      end;
  let length = history-length(history);
  let n :: <integer> = if (cutoff-length) min(length, cutoff-length) else length end;
  for (i :: <integer> from 0 below n)
    unless (-1 < index & index < length)
      index := modulo(index, length)
    end;
    let elt = history-element(history, index);
    when (history-element-visible?(history, elt)
	  & (~test | test(elt)))
      function(elt, index)
    end;
    inc!(index)
  end
end method do-history-elements;

// This is the guts of c-Y/c-m-Y, or c-sh-Y/c-m-sh-Y.
// Returns an element, or #f if it can't find one (that matches).
// INDEX is the index at which to start looking for a matching element.
// If TEST is supplied, it should be a function of one argument, an element.
define method yank-from-history
    (history :: <basic-history>, #key index, test) => (elt)
  block (return)
    let idx :: <integer>
      = case
	  ~index    => history.%rotation;
	  index > 0 => index - 1;
	  index < 0 => index;
	  otherwise => error("Zero should have been handled at a higher level");
	end;
    let fixup? = #t;
    with-lock (history.%lock)
      history.%yank-state := #"yank-next";
      while (#t)
	let (elt, position) = history-element(history, idx, fixup?: fixup?);
	case
	  ~elt =>
	    return(#f);
	  (index | history-element-visible?(history, elt)) & (~test | test(elt)) =>
	    history.%position := position;
	    return(elt);
	end;
	inc!(idx);
	fixup? := #f
      end
    end
  end
end method yank-from-history;

// This is the guts of m-Y or m-sh-Y (having already figured out that the last
// command was a yanking command and which history it used).
// Returns #f if it fails to find anything different.
// INDEX and TEST are as for 'yank-from-history'.
define method yank-next-from-history
    (history :: <basic-history>, #key index :: <integer> = 1, test) => (elt)
  block (return)
    when (index = 0)
      error("Zero should have been handled at a higher level")
    end;
    when (history.%position)
      with-lock (history.%lock)
	history.%yank-state := #"yank-next";
	let old-elt = history-element(history, history.%position);
        local method do-element (elt, idx :: <integer>)
		unless (history-elements-equal?(history, elt, old-elt))
		  history.%rotation := idx;
		  history.%position := idx;
		  return(elt)
		end
	      end method;
	do-history-elements(do-element, history,
			    index: history.%position + 1, offset: index, test: test)
      end
    end
  end
end method yank-next-from-history;

// Push an element onto the front of the history and extend its length
define method history-push
    (history :: <basic-history>, elt) => ()
  with-lock (history.%lock)
    let top-elt = history-top(history);
    // It saves space and is a nicer user interface if we don't push the new
    // element if it is equal to the element currently on top
    unless (top-elt & history-elements-equal?(history, elt, top-elt))
      if (history.%current-length = history.%maximum-length)
	let last-pair = begin
			  let last-pair = history.%elements;
			  for (i :: <integer> from 0 below history.%current-length - 3)
			    last-pair := tail(last-pair)
			  end;
			  last-pair
			end;
	push!(history.%elements, elt);
	tail(last-pair) := list(head(tail(last-pair)))
      else
	push!(history.%elements, elt);
	inc!(history.%current-length)
      end
    end;
    history.%rotation := 0;
    history.%temporary-element := #f
  end
end method history-push;

// Pop the top element from the history, and reduce its length.
define method history-pop
    (history :: <basic-history>) => (elt)
  with-lock (history.%lock)
    when (history.%current-length > 0)
      dec!(history.%current-length);
      let elt = head(history.%elements);
      history.%elements := tail(history.%elements);
      elt
    end
  end 
end method history-pop;

define method history-top
    (history :: <basic-history>) => (elt)
  ~empty?(history.%elements) & head(history.%elements)
end method history-top;

// Replace the top element of the history with a new element.
define method history-top-setter
    (elt, history :: <basic-history>) => (elt)
  head(history.%elements) := elt
end method history-top-setter;

define method history-elements-equal?
    (history :: <basic-history>, elt1, elt2) => (equal? :: <boolean>)
  // Note that on strings, this does a case-sensitive comparison
  elt1 = elt2
end method history-elements-equal?;

// Is this element visible in the history?  The default answer is yes
define method history-element-visible?
    (history :: <basic-history>, elt) => (visible? :: <boolean>)
  ignore(elt);
  #t
end method history-element-visible?;

define method reset-history
    (history :: <basic-history>) => ()
  with-lock (history.%lock)
    history.%elements := #();
    history.%current-length := 0;
    history.%position := 0;
    history.%rotation := 0;
    history.%temporary-element := #f;
    initialize-yank-state(history)
  end
end method reset-history;


/// Kill ring history

define sealed class <kill-history> (<basic-history>)
  keyword yank-state: = #"clipboard";
end class <kill-history>;

define sealed domain make (singleton(<kill-history>));
define sealed domain initialize (<kill-history>);

define sealed method initialize-yank-state 
    (history :: <kill-history>) => ()
  history.%yank-state := #"clipboard"
end method initialize-yank-state;


/// Kill ring elements

define method history-element-size
    (elt :: <byte-string>) => (size :: <integer>)
  size(elt)
end method history-element-size;

define method history-element-size
    (elt :: <interval>) => (size :: <integer>)
  count-characters(elt)
end method history-element-size;


define method merge-history-elements
    (elt1 :: <byte-string>, elt2 :: <byte-string>)
 => (result :: <byte-string>)
  concatenate-as(<byte-string>, elt1, elt2)
end method merge-history-elements;

define method merge-history-elements
    (elt1 :: <byte-string>, elt2 :: <interval>)
 => (result :: <byte-string>)
  concatenate-as(<byte-string>, elt1, as(<byte-string>, elt2))
end method merge-history-elements;

define method merge-history-elements
    (elt1 :: <interval>, elt2 :: <byte-string>)
 => (result :: <byte-string>)
  concatenate-as(<byte-string>, as(<byte-string>, elt1), elt2)
end method merge-history-elements;

// Note that this doesn't copy the intervals!
define method merge-history-elements
    (elt1 :: <interval>, elt2 :: <interval>) => (result :: <interval>)
  let end1   :: <basic-bp> = interval-end-bp(elt1);
  let start2 :: <basic-bp> = interval-start-bp(elt2);
  if (bp-character(end1) = '\n')
    let last1  = bp-line(end1);
    let first2 = bp-line(start2);
    // Link up the two intervals
    line-next(last1) := first2;
    line-previous(first2) := last1;
    interval-end-bp(elt1) := interval-end-bp(elt2)
  else
    let bp = insert!(end1, elt2);
    move-bp!(end1, bp-line(bp), bp-index(bp))
  end;
  elt1
end method merge-history-elements;


/// Higher level kill ring functions

define method add-to-kill-ring
    (kill-ring :: <kill-history>, object :: type-union(<basic-interval>, <string>),
     #key merge? = #f, reverse? = #f) => (elt)
  let top-elt
    = merge? & history-top(kill-ring);
  let new-elt
    = if (instance?(object, <string>))
	object
      else
	let interval :: <basic-interval> = object;	// force tighter type...
	if (bp-line(interval-start-bp(interval)) == bp-line(interval-end-bp(interval)))
	  as(<byte-string>, interval)			// more efficient for simple things...
	else
	  copy-interval(interval)
	end
      end;
  let new-elt
    = if (top-elt)
	if (reverse?)
	  merge-history-elements(new-elt, top-elt)
	else
	  merge-history-elements(top-elt, new-elt)
	end
      else
	new-elt
      end;
  if (top-elt)
    history-top(kill-ring) := new-elt
  else
    history-push(kill-ring, new-elt)
  end;
  new-elt
end method add-to-kill-ring;

// This does the "next" kind of yank depending on the history's yank state
define method yank-from-kill-ring
    (kill-ring :: <kill-history>, window :: <basic-window>,
     #key index)
 => (elt :: false-or(type-union(<basic-interval>, <string>)))
  let policy     = editor-policy(frame-editor(window-frame(window)));
  let clipboard? = clipboard-policy(policy);
  let clipboard  = clipboard? & get-from-clipboard(window, <byte-string>);
  let state      = kill-ring.%yank-state;
  case
    clipboard? & state == #"clipboard" & ~index =>
      kill-ring.%yank-state := #"yank";
      clipboard;
    state == #"yank-next" =>
      yank-next-from-history(kill-ring, index: index | 1);
    otherwise =>
      kill-ring.%yank-state := #"yank-next";
      let elt = yank-from-history(kill-ring, index: index | 1);
      if (clipboard? & elt = clipboard)
	yank-next-from-history(kill-ring, index: index | 1)
      else
	elt
      end
  end
end method yank-from-kill-ring;
